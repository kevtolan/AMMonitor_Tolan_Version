photo_annotator_ui <- function(id, viewer_mode) {
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
    # uiOutput('openImage'),
    tags$span(
      title = 'open image in new tab',
      actionButton(
        ns('openImage'),
        "", 
        icon = icon('image')
      )
    ),
    tags$br(),
    tags$br(),
    
    uiOutput(ns('taggerUI'))
  )
}

photo_annotator_server <- function(id, selectedUser = reactive(NA), photo_name, last_photo_name, the_bboxes, autosave_rate, metadata_cache, deleted_rows, active) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define reactive values
    last_photo <- reactiveVal('') # The last photo to have been annotated
    annoUpdate <- reactiveVal(1) # To trigger an update of the annotations DT
    # annotations_cache <- reactiveVal(NA) # Cache of annotations
    
    output$taggerUI <- renderUI({
      tagList(
        wellPanel(
          shiny::tags$h3('Photo-level Tags'),
          lapply(non_taxa_label_options[which(non_taxa_label_options$photos == 1 & non_taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_medialistid'], function(x) {
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
          lapply(non_taxa_label_options[which(non_taxa_label_options$photos == 1 & non_taxa_label_options$list_type == 'numeric_list'), 'pk_medialistid'],function(x) {
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
          lapply(non_taxa_label_options[which(non_taxa_label_options$photos == 1 & non_taxa_label_options$list_type == 'checkbox_list'), 'pk_medialistid'],function(x) {
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
            options = list(
              placeholder = "Select a taxon"
            )
          ),
          lapply(
            taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid'], 
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
            taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'], 
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
            taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'], 
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
        taxa_label_options[which(taxa_label_options$photos == 1),], 
        librarylistitems, 
        by.x = 'fk_child_librarylistid', 
        by.y = 'fk_librarylistid'
      )
      
      taxonListTagNames <- taxonlist_table[which(taxonlist_table$item == input$newTaxon), 'pk_librarylistid']
      
      taxonGroupTagNames <- unlist(lapply(
        taxa_label_options[which(taxa_label_options$photos == 1), 'pk_librarylistid'],
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
      for (the_tag in taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid']) {
        if (the_tag %in% validTags()) {
          shinyjs::show(the_tag)
        } else {
          shinyjs::hide(the_tag)
        }
      }
      
      for (the_tag in taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type %in% c('numeric_list', 'checkbox_list')), 'pk_librarylistid']) {
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
        taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid'], 
        function(x) {
          updateSelectizeInput(
            session,
            x,
            selected = ""
          )
        }
      )
      
      lapply(
        taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'],
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
        taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'],
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
    
    observeEvent(input$addAnnotation, {
      if (!is.na(input$newTaxon)) {
        # Get new annotations/annotags/mediatags
        new_tags <- get_new_annotations(
          metadata_cache = metadata_cache(), 
          input = input, 
          selectedUser = selectedUser(), 
          fileID = photo_name(),
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
      
      # Reset taxa label options
      lapply(
        taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid'],
        function(x) {
          updateSelectizeInput(
            session,
            x,
            selected = ""
          )
        }
      )
      
      lapply(
        taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'],
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
        taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'],
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
        non_taxa_label_options[which(non_taxa_label_options$photos == 1 & non_taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_medialistid'],
        function(x) {
          updateSelectizeInput(
            session,
            x,
            selected = ""
          )
        }
      )
      
      lapply(
        non_taxa_label_options[which(non_taxa_label_options$photos == 1 & non_taxa_label_options$list_type == 'numeric_list'), 'pk_medialistid'],
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
        non_taxa_label_options[which(non_taxa_label_options$photos == 1 & non_taxa_label_options$list_type == 'checkbox_list'), 'pk_medialistid'],
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
      
      for (the_tag in taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type %in% c('dropdown_list', 'bbox_list')), 'pk_librarylistid']) {
        shinyjs::hide(the_tag)
      }
      
      for (the_tag in taxa_label_options[which(taxa_label_options$photos == 1 & taxa_label_options$list_type %in% c('numeric_list', 'checkbox_list')), 'pk_librarylistid']) {
        items <- librarylistitems[which(librarylistitems$fk_librarylistid == the_tag),]
        item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
        for (id in item_ids) {
          shinyjs::hide(id)
        }
      }
      annoUpdate(annoUpdate()+1)
    })
    
    observeEvent(input$copy_annotation, {if (active()) {
      no_prev_photo <- is.na(last_photo_name())
      
      if (no_prev_photo) {
        showModal(modalDialog('No previous photo exist.', easyClose = TRUE))
      } else {
        file_annotations_prev <- metadata_cache()$cache$annotations[
          metadata_cache()$cache$annotations$fk_mediaid == last_photo_name() &
            metadata_cache()$cache$annotations$fk_personid == selectedUser(),
        ]
        
        file_mediatags_prev <- metadata_cache()$cache$mediatags[
          metadata_cache()$cache$mediatags$fk_mediaid == last_photo_name() &
            metadata_cache()$cache$mediatags$fk_personid == selectedUser(),
        ]
        
        if (nrow(file_annotations_prev) == 0 && nrow(file_mediatags_prev) == 0) {
          showModal(modalDialog('No previous annotations exist.', easyClose = TRUE))
        } else {
          
          file_annotations <- metadata_cache()$cache$annotations[
            metadata_cache()$cache$annotations$fk_mediaid == photo_name() &
              metadata_cache()$cache$annotations$fk_personid == selectedUser(),
          ]
          
          file_mediatags <- metadata_cache()$cache$mediatags[
            metadata_cache()$cache$mediatags$fk_mediaid == photo_name() &
              metadata_cache()$cache$mediatags$fk_personid == selectedUser(),
          ]
          
          # Copy annotations for photos -----------
          
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
                fk_mediaid = photo_name(),
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
          
          # Copy file level tags
          for (i in seq_len(nrow(file_mediatags_prev))) {
            
            # Get any matching existing mediatag IDs
            if (!any(file_mediatags$fk_medialistitemid == file_mediatags_prev$fk_medialistitemid[i])) {
              
              # Create the new mediatag
              new_mediatag <- data.frame(
                pk_mediatagid = mediatagID_new,
                fk_mediaid = photo_name(),
                fk_medialistitemid = file_mediatags_prev$fk_medialistitemid[i],
                fk_personid = selectedUser(),
                x_max = NA,
                x_min = NA,
                y_max = NA,
                y_min = NA,
                value_num = file_mediatags_prev$value_num[i],
                notes = NA,
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                fk_medialistid = medialistitems$fk_medialistid[
                  medialistitems$pk_medialistitemid == file_mediatags_prev$fk_medialistitemid[i]
                ],
                item = medialistitems$item[
                  medialistitems$pk_medialistitemid == file_mediatags_prev$fk_medialistitemid[i]
                ],
                is_add = 1,
                is_delete = 0
              )
              mediatagID_new <- mediatagID_new - 1
              new_mediatags <- rbind(new_mediatags, new_mediatag)
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
