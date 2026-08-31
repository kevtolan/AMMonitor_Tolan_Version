audio_annotator_ui <- function(id, viewer_mode) {
  ns <- NS(id)
  tagList(
    tags$script(HTML(paste0(
      'var map = {16: false, 65: false};',
      '$(document).keydown(function(e) {',
      'if (e.keyCode in map) {',
      'map[e.keyCode] = true;',
      'if (map[16] && map[65]) {', # shift+a
      "$('#",
      ns("addAnnotation"),
      "').click()",
      '}',
      '}',
      '}).keyup(function(e) {',
      'if (e.keyCode in map) {',
      'map[e.keyCode] = false;',
      '}',
      '});'
    ))),
    tags$script(HTML(paste0(
      'var map2 = {16: false, 67: false};',
      '$(document).keydown(function(e) {',
      'if (e.keyCode in map2) {',
      'map2[e.keyCode] = true;',
      'if (map2[16] && map2[67]) {', # shift+c
      "$('#",
      ns("copy_annotation"),
      "').click()",
      '}',
      '}',
      '}).keyup(function(e) {',
      'if (e.keyCode in map2) {',
      'map2[e.keyCode] = false;',
      '}',
      '});'
    ))),
    tags$script(HTML(paste0(
      'var map3 = {16: false, 79: false};',
      '$(document).keydown(function(e) {',
      'if (e.keyCode in map3) {',
      'map3[e.keyCode] = true;',
      'if (map3[16] && map3[79]) {', # shift+o
      "$('#",
      ns("openImage"),
      "').click()",
      '}',
      '}',
      '}).keyup(function(e) {',
      'if (e.keyCode in map3) {',
      'map3[e.keyCode] = false;',
      '}',
      '});'
    ))),
    tags$span(
      title = 'copy annotation',
      actionButton(
        ns('copy_annotation'),
        "",
        # ' Copy Previous',
        icon = icon('copy')
      )
    ),
    tags$span(
      title = 'add annotations',
      actionButton(
        ns('addAnnotation'),
        "",
        # 'Add Annotations',
        icon = icon('plus-circle')
      )
    ),
    tags$br(),
    tags$br(),

    textInput(
      ns('manual_detx'),
      'Manual Detections (defaults to model count; overwrite as needed):',
      value = "",
      width = '100%'
    ),
    actionButton(ns('save_manual_detx'), 'Save Detections'),
    tags$br(), tags$br(),
    textOutput(ns('manual_detx_save_status')),
    tags$br(),

    uiOutput(ns('taggerUI'))
  )
}

audio_annotator_server <- function(id, selectedUser = reactive(NA), audio_name, last_audio_name, the_bboxes, metadata_cache, active) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define reactive values
    last_audio <- reactiveVal('') # The last audio to have been annotated
    annoUpdate <- reactiveVal(1) # To trigger an update of the annotations DT

    # Live count of modeloutputs for this recording -- used as the displayed
    # default whenever media.ManualDetx hasn't been set. Mirrors the same
    # logic in audio_player.R: includes unverified and verified-valid
    # detections with value_num >= 14; excludes any explicitly marked invalid.
    current_model_output_count <- reactive({
      req(audio_name())
      these_ids <- metadata_cache()$cache$modeloutputs$pk_modeloutputid[
        metadata_cache()$cache$modeloutputs$fk_mediaid == audio_name() &
          !is.na(metadata_cache()$cache$modeloutputs$value_num) &
          metadata_cache()$cache$modeloutputs$value_num >= 14
      ]
      invalid_ids <- unique(metadata_cache()$cache$modelverifications$fk_modeloutputid[
        metadata_cache()$cache$modelverifications$is_valid == 0
      ])
      model_count <- sum(!(these_ids %in% invalid_ids))

      # Manual annotations (from the Tagger) for this recording, any taxon,
      # any annotator, excluding any pending-deletion in the cache. Summed
      # with the model count per user request -- no de-duplication against
      # model detections.
      annotation_count <- sum(
        metadata_cache()$cache$annotations$fk_mediaid == audio_name() &
          metadata_cache()$cache$annotations$is_delete == 0 &
          metadata_cache()$cache$annotations$fk_taxonid != 'no-species',
        na.rm = TRUE
      )

      model_count + annotation_count
    })

    # metadata_cache() here is a reactive *getter* passed in from
    # audio_player_server (not a reactiveValues object we can assign into
    # directly -- `metadata_cache()$cache$... <- x` has no `metadata_cache<-`
    # method and would error), so this session's own saves are tracked
    # locally instead. The shared cache picks up the change next time it
    # refreshes from the DB; this just keeps this box showing the right
    # value immediately after you save.
    just_saved <- reactiveVal(list(pk_mediaid = NA, value = NA))

    # Load the current recording's ManualDetx into the box, defaulting to the
    # live model-output count when no manual value has been saved
    observe({
      req(audio_name())
      stored_value <- metadata_cache()$cache$mediaMetaData$ManualDetx[
        metadata_cache()$cache$mediaMetaData$pk_mediaid == audio_name()
      ]
      display_value <- if (!is.na(just_saved()$pk_mediaid) && just_saved()$pk_mediaid == audio_name() && !is.na(just_saved()$value)) {
        just_saved()$value  # saved this session, and it's an actual override (not a revert-to-default)
      } else if (!is.na(just_saved()$pk_mediaid) && just_saved()$pk_mediaid == audio_name()) {
        current_model_output_count()  # reverted to default this session (saved blank)
      } else if (length(stored_value) && !is.na(stored_value[1])) {
        stored_value[1]
      } else {
        current_model_output_count()
      }
      updateTextInput(
        session = session,
        inputId = 'manual_detx',
        value = as.character(display_value)
      )
      output$manual_detx_save_status <- renderText("")
    })

    # Save the (possibly overwritten) detection count back to media.ManualDetx
    observeEvent(input$save_manual_detx, {
      req(audio_name())

      new_value <- suppressWarnings(as.integer(input$manual_detx))
      if (input$manual_detx != "" && is.na(new_value)) {
        output$manual_detx_save_status <- renderText("Enter a whole number, or leave blank to use the model count.")
        return()
      }

      rs <- DBI::dbSendQuery(
        con(),
        "UPDATE media SET ManualDetx = $1 WHERE pk_mediaid = $2;"
      )
      DBI::dbBind(rs, list(new_value, audio_name()))
      DBI::dbClearResult(rs)

      just_saved(list(pk_mediaid = audio_name(), value = new_value))

      output$manual_detx_save_status <- renderText(paste0('Saved ', format(Sys.time(), '%H:%M:%S')))
    })

    output$taggerUI <- renderUI({
      audio_name()
      tagList(
        wellPanel(
          shiny::tags$h3('Recording-level Tags'),
          lapply(non_taxa_label_options[which(non_taxa_label_options$recordings == 1 & non_taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_medialistid'], function(x) {
            # options list (named vector)
            listitems <- medialistitems[which(medialistitems$fk_medialistid == x),]
            choices <- listitems$pk_medialistitemid
            names(choices) <- listitems$item
            
            selectizeInput(
              inputId = ns(x),
              label = x,
              choices = c("", choices),
              multiple = FALSE
            )
          }),
          lapply(non_taxa_label_options[which(non_taxa_label_options$recordings == 1 & non_taxa_label_options$list_type == 'numeric_list'), 'pk_medialistid'],function(x) {
            items <- medialistitems[which(medialistitems$fk_medialistid == x),]
            if (nrow(items) > 0) {
              lapply(1:nrow(items), function(i) {
                numericInput(
                  inputId = ns(paste0(items$item[i], "_", items$pk_medialistitemid[i])),
                  label = items$item[i],
                  value = NULL
                )
              })
            }
          }),
          lapply(non_taxa_label_options[which(non_taxa_label_options$recordings == 1 & non_taxa_label_options$list_type == 'checkbox_list'), 'pk_medialistid'],function(x) {
            items <- medialistitems[which(medialistitems$fk_medialistid == x),]
            if (nrow(items) > 0) {
              lapply(1:nrow(items), function(i) {
                checkboxInput(
                  inputId = ns(paste0(items$item[i], "_", items$pk_medialistitemid[i])),
                  label = items$item[i],
                  value = FALSE
                )
              })
            }
          })
        ),
        
        wellPanel(
          shiny::tags$h3('Taxon Tags'),
          selectizeInput(
            ns('newTaxon'),
            'Select taxon',
            choices = c("", 'no-species', sort(setdiff(taxon_names$pk_taxonid, c('no-species'))
            )),
            multiple = FALSE,
            selected = NULL,
            options = list(
              placeholder = "Select a taxon",
              maxItems = 1
            )
          ),
          lapply(
            taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid'], 
            function(x) {
              # options list (named vector)
              listitems <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
              choices <- listitems$pk_librarylistitemid
              names(choices) <- listitems$item
              
              shinyjs::hidden(selectizeInput(
                inputId = ns(x),
                label = x,
                choices = c("", choices),
                multiple = FALSE
              ))
            }
          ),
          lapply(
            taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'], 
            function(x) {
              taxa_num_tags <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
              if (nrow(taxa_num_tags) > 0) {
                lapply(1:nrow(taxa_num_tags), function(i) {
                  shinyjs::hidden(numericInput(
                    inputId = ns(paste0(taxa_num_tags$item[i], "_", taxa_num_tags$pk_librarylistitemid[i])),
                    label = taxa_num_tags$item[i],
                    value = NULL
                  ))
                })
              }
            }
          ),
          lapply(
            taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'], 
            function(x) {
              taxa_num_tags <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
              if (nrow(taxa_num_tags) > 0) {
                lapply(1:nrow(taxa_num_tags), function(i) {
                  shinyjs::hidden(checkboxInput(
                    inputId = ns(paste0(taxa_num_tags$item[i], "_", taxa_num_tags$pk_librarylistitemid[i])),
                    label = taxa_num_tags$item[i],
                    value = FALSE
                  ))
                })
              }
            }
          )
        )
      )
    })
    
    # Filter out which tags should be displayed as options for the specified taxon label
    validTags <- reactive({
      taxonlist_table <- merge(
        taxa_label_options[which(taxa_label_options$recordings == 1),], 
        librarylistitems, 
        by.x = 'fk_child_librarylistid', 
        by.y = 'fk_librarylistid',
      )
      
      taxonListTagNames <- taxonlist_table[which(taxonlist_table$item == input$newTaxon), 'pk_librarylistid']
      
      taxonGroupTagNames <- unlist(lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1), 'pk_librarylistid'],
        function(x) {
          libtaxon_name <- taxa_label_options[which(taxa_label_options$pk_librarylistid == x), 'fk_taxonid']
          libTaxon <- taxon_names[which(taxon_names$pk_taxonid == libtaxon_name),]
          
          if (nrow(libTaxon) == 1) {
            
            newTaxonRankValue <- taxon_names[
              which(taxon_names$pk_taxonid == input$newTaxon), 
              paste0('rank_', tolower(libTaxon$taxon_rank))
            ]
            
            if (libTaxon[paste0('rank_', tolower(libTaxon$taxon_rank))] %in% newTaxonRankValue) {x}
          }
        }
      ))
      
      sort(c(taxonListTagNames, taxonGroupTagNames))
    })
    
    # A taxon is selected from the annotations selectInput
    observeEvent(input$newTaxon, {
      for (the_tag in taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid']) {
        if (the_tag %in% validTags()) {
          shinyjs::show(the_tag)
        } else {
          shinyjs::hide(the_tag)
        }
      }
      
      for (the_tag in taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type %in% c('numeric_list', 'checkbox_list')), 'pk_librarylistid']) {
        items <- librarylistitems[which(librarylistitems$fk_librarylistid == the_tag),]
        item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
        if (the_tag %in% validTags()) {
          for (id in item_ids) {
            shinyjs::show(id)
          }
        } else {
          for (id in item_ids) {
            shinyjs::hide(id)
          }
        }
      }
      
      
      # Reset taxon annoTags
      lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid'], 
        function(x) {
          updateSelectizeInput(
            session,
            x,
            selected = ""
          )
        }
      )
      
      lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'],
        function(x) {
          items <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
          if (nrow(items) > 0) {
            item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
            lapply(item_ids, function(x) {
              updateNumericInput(
                session,
                inputId = x,
                value = NA
              )
            })
          }
        }
      )
      
      lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'],
        function(x) {
          items <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
          if (nrow(items) > 0) {
            item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
            lapply(item_ids, function(x) {
              updateCheckboxInput(
                session,
                inputId = x,
                value = FALSE
              )
            })
          }
        }
      )
    })
    
    annotations_cache <- reactiveVal(NA)
    
    observeEvent(input$addAnnotation, priority = 10000, {if (active()) {
      if (!is.na(input$newTaxon)) {
        # Get new annotations/annotags/mediatags
        new_tags <- get_new_annotations(
          metadata_cache = metadata_cache(), 
          input = input, 
          selectedUser = selectedUser(), 
          fileID = audio_name(),
          bboxes = the_bboxes()
        )
      } else {
        new_tags <- NA
      }
      
      annotations_cache(new_tags)
      
      # Reset the taxon list
      updateSelectizeInput(
        session,
        'newTaxon',
        selected = ""
      )
      
      # Reset all taxa label options
      lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid'], 
        function(x) {
          updateSelectizeInput(
            session,
            x,
            selected = ""
          )
        }
      )
      
      lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'],
        function(x) {
          items <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
          if (nrow(items) > 0) {
            item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
            lapply(item_ids, function(x) {
              updateNumericInput(
                session,
                inputId = x,
                value = NA
              )
            })
          }
        }
      )
      
      lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'],
        function(x) {
          items <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
          if (nrow(items) > 0) {
            item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
            lapply(item_ids, function(x) {
              updateCheckboxInput(
                session,
                inputId = x,
                value = FALSE
              )
            })
          }
        }
      )
      
      # Reset mediatag options
      lapply(
        non_taxa_label_options[which(non_taxa_label_options$recordings == 1 & non_taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_medialistid'], 
        function(x) {
          updateSelectizeInput(
            session,
            x,
            selected = ""
          )
        }
      )
      
      lapply(
        non_taxa_label_options[which(non_taxa_label_options$recordings == 1 & non_taxa_label_options$list_type == 'numeric_list'), 'pk_medialistid'],
        function(x) {
          items <- medialistitems[which(medialistitems$fk_medialistid == x),]
          
          if (nrow(items) > 0) {
            item_ids <- paste0(items$item, "_", items$pk_medialistitemid)
            lapply(item_ids, function(x) {
              updateNumericInput(
                session,
                inputId = x,
                value = NA
              )
            })
          }
        }
      )
      
      lapply(
        non_taxa_label_options[which(non_taxa_label_options$recordings == 1 & non_taxa_label_options$list_type == 'checkbox_list'), 'pk_medialistid'],
        function(x) {
          items <- medialistitems[which(medialistitems$fk_medialistid == x),]
          
          if (nrow(items) > 0) {
            item_ids <- paste0(items$item, "_", items$pk_medialistitemid)
            lapply(item_ids, function(x) {
              updateCheckboxInput(
                session,
                inputId = x,
                value = FALSE
              )
            })
          }
        }
      )
      
      for (the_tag in taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid']) {
        shinyjs::hide(the_tag)
      }
      
      for (the_tag in taxa_label_options[which(taxa_label_options$recordings == 1 & taxa_label_options$list_type %in% c('numeric_list', 'checkbox_list')), 'pk_librarylistid']) {
        items <- librarylistitems[which(librarylistitems$fk_librarylistid == the_tag),]
        item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
        for (id in item_ids) {
          shinyjs::hide(id)
        }
      }
      annoUpdate(annoUpdate()+1)
    }})
    
    observeEvent(input$copy_annotation, {if (active()) {
      no_prev_audio <- is.na(last_audio_name())
      
      if (no_prev_audio) {
        showModal(modalDialog('No previous annotations exist.', easyClose = TRUE))
      } else {
        file_annotations_prev <- metadata_cache()$cache$annotations[
          metadata_cache()$cache$annotations$fk_mediaid == last_audio_name() &
            metadata_cache()$cache$annotations$fk_personid == selectedUser(),
        ]
        
        file_mediatags_prev <- metadata_cache()$cache$mediatags[
          metadata_cache()$cache$mediatags$fk_mediaid == last_audio_name() &
            metadata_cache()$cache$mediatags$fk_personid == selectedUser(),
        ]
        
        if (nrow(file_annotations_prev) == 0 && nrow(file_mediatags_prev) == 0) {
          showModal(modalDialog('No previous annotations exist.', easyClose = TRUE))
        } else {
          file_annotations <- metadata_cache()$cache$annotations[
            metadata_cache()$cache$annotations$fk_mediaid == audio_name() &
              metadata_cache()$cache$annotations$fk_personid == selectedUser(),
          ]
          
          file_mediatags <- metadata_cache()$cache$mediatags[
            metadata_cache()$cache$mediatags$fk_mediaid == audio_name() &
              metadata_cache()$cache$mediatags$fk_personid == selectedUser(),
          ]
          
          # Copy annotations for audio -----------
          
          # Get temporary annotation ID's to use for new, cached annotations
          cached_annoIDs <- metadata_cache()$cache$annotations$pk_annotationid[
            metadata_cache()$cache$annotations$pk_annotationid < 0
          ]
          annID_new <- ifelse(
            length(cached_annoIDs) == 0,
            -1,
            min(cached_annoIDs) - 1
          )
          
          # Get temporary annotags ID's to use for new, cached annotags
          cached_annotagIDs <- metadata_cache()$cache$annotags$pk_annotagid[
            metadata_cache()$cache$annotags$pk_annotagid < 0
          ]
          anntagID_new <- ifelse(
            length(cached_annotagIDs) == 0,
            -1,
            min(cached_annotagIDs) - 1
          )
          
          # Get temporary mediatags ID's to use for new, cached mediatags
          cached_mediatagIDs <- metadata_cache()$cache$mediatags$pk_mediatagid[
            metadata_cache()$cache$mediatags$pk_mediatagid < 0
          ]
          mediatagID_new <- ifelse(
            length(cached_mediatagIDs) == 0,
            -1,
            min(cached_mediatagIDs) - 1
          )
          
          new_annotations <- data.frame()
          new_annotags <- data.frame()
          new_mediatags <- data.frame()
          
          for (i in seq_len(nrow(file_annotations_prev))) {
            
            # See if a valid annotation exists
            annID_existing <- file_annotations$pk_annotationid[
              which(file_annotations$fk_taxonid == file_annotations_prev$fk_taxonid[i])
            ]
            
            if (length(annID_existing) == 0) {
              new_annotation <- data.frame(
                pk_annotationid = annID_new,
                fk_personid = selectedUser(),
                fk_mediaid = audio_name(),
                fk_searchlistid = NA,
                fk_taxonid = file_annotations_prev$fk_taxonid[i],
                x_max = NA, # Never copy bounding boxes
                x_min = NA,
                y_max = NA,
                y_min = NA,
                notes = NA,
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                is_add = 1,
                is_delete = 0
              )
              
              fk_annotationid <- new_annotation$pk_annotationid
              new_annotations <- rbind(new_annotations, new_annotation)
              annID_new <- annID_new - 1
            } else {
              fk_annotationid <- annID_existing
            }
            
            annoTags_taxa_prev <- metadata_cache()$cache$annotags[
              metadata_cache()$cache$annotags$fk_annotationid == file_annotations_prev$pk_annotationid[i],
            ]
            
            annoTags_taxa <- metadata_cache()$cache$annotags[
              metadata_cache()$cache$annotags$fk_annotationid == fk_annotationid,
            ]
            
            for (j in seq_len(nrow(annoTags_taxa_prev))) {
              
              # Get any matching existing annoTag ID's
              if (!any(annoTags_taxa$fk_librarylistitemid == annoTags_taxa_prev$fk_librarylistitemid[j])) {
                
                # Create the new annoTag
                new_annoTag <- data.frame(
                  pk_annotagid = anntagID_new,
                  fk_annotationid = fk_annotationid,
                  fk_librarylistitemid = annoTags_taxa_prev$fk_librarylistitemid[j],
                  value_num = annoTags_taxa_prev$value_num[j],
                  fk_librarylistid = librarylistitems$fk_librarylistid[
                    librarylistitems$pk_librarylistitemid == annoTags_taxa_prev$fk_librarylistitemid[j]
                  ],
                  item = librarylistitems$item[
                    librarylistitems$pk_librarylistitemid == annoTags_taxa_prev$fk_librarylistitemid[j]
                  ],
                  is_add = 1,
                  is_delete = 0
                )
                
                anntagID_new <- anntagID_new - 1
                new_annotags <- rbind(new_annotags, new_annoTag)
              }
            }
          }
          annotations_cache(
            list(
              annotations = new_annotations, 
              annotags = new_annotags, 
              mediatags = new_mediatags
            )
          )
        }
      }
      annoUpdate(annoUpdate()+1)
    }})
    
    return(reactiveValues(
      annoUpdate = reactive(annoUpdate()),
      annotations_cache = reactive(annotations_cache())
    ))
  })
}
