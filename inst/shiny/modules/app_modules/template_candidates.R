#!! ModName = template_candidates
#!! ModDisplayName = Template Candidates
#!! ModDescription = Displays annotations marked as potential templates for selection
#!! ModCitation = Tang, Caroline.  (2023). template_candidates. [Source code].
#!! ModNotes = NA
#!! ModActive = 1
#!! FunctionReturn = selectedTaxon !! selected taxon !! character
#!! FunctionReturn = selectedAnnos !! rows of selected annotations !! data.frame
#!! Package = reactable !! 0.4.3 !! notes

# the ui function
template_candidates_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Select Annotations by Taxon"),
    uiOutput(ns('taxonLabels')),
    h3("These annotations will be converted to templates:"),
    reactableOutput(ns('annotations'))
  )
}


# the server function
template_candidates_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    audio_taxa_label_options <- taxa_label_options[which(taxa_label_options$recordings == 1),]
    
    output$taxonLabels <- renderUI({
      tagList(
        selectizeInput(
          ns('taxon'),
          label = "Select a taxon:",
          choices = c(
            "",
            dbGetQuery(
              con(),
              'SELECT DISTINCT annotations.fk_taxonid FROM annotags 
          LEFT JOIN annotations ON annotags.fk_annotationid = annotations.pk_annotationid 
          LEFT JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid 
          WHERE librarylistitems.item = \'template\';'
            )[,]
          ),
          multiple = FALSE,
          options = list(placeholder = "Taxon name")
        ),
        lapply(
          audio_taxa_label_options[which(audio_taxa_label_options$list_type %in% c('dropdown_list', 'bbox_required_list')), 'pk_librarylistid'], 
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
          audio_taxa_label_options[which(audio_taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'], 
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
          audio_taxa_label_options[which(audio_taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'], 
          function(x) {
            taxa_box_tags <- librarylistitems[which(librarylistitems$fk_librarylistid == x & librarylistitems$item != 'template'),]
            if (nrow(taxa_box_tags) > 0) {
              lapply(1:nrow(taxa_box_tags), function(i) {
                shinyjs::hidden(checkboxInput(
                  inputId = ns(paste0(taxa_box_tags$item[i], "_", taxa_box_tags$pk_librarylistitemid[i])),
                  label = taxa_box_tags$item[i],
                  value = FALSE
                ))
              })
            }
          }
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
      
      taxonListTagNames <- taxonlist_table[which(taxonlist_table$item == input$taxon), 'pk_librarylistid']
      
      taxonGroupTagNames <- unlist(lapply(
        taxa_label_options[which(taxa_label_options$recordings == 1), 'pk_librarylistid'],
        function(x) {
          libtaxon_name <- taxa_label_options[which(taxa_label_options$pk_librarylistid == x), 'fk_taxonid']
          libTaxon <- taxon_names[which(taxon_names$pk_taxonid == libtaxon_name),]
          
          if (nrow(libTaxon) == 1) {
            
            newTaxonRankValue <- taxon_names[
              which(taxon_names$pk_taxonid == input$taxon), 
              paste0('rank_', tolower(libTaxon$taxon_rank))
            ]
            
            if (libTaxon[paste0('rank_', tolower(libTaxon$taxon_rank))] %in% newTaxonRankValue) {x}
          }
        }
      ))
      
      sort(c(taxonListTagNames, taxonGroupTagNames))
    })
    
    # A taxon is selected from the annotations selectInput
    observeEvent(input$taxon, {
      for (the_tag in audio_taxa_label_options[which(taxa_label_options$list_type %in% c('dropdown_list', 'bbox_required_list')), 'pk_librarylistid']) {
        if (the_tag %in% validTags()) {
          shinyjs::show(the_tag)
        } else {
          shinyjs::hide(the_tag)
        }
      }
      
      for (the_tag in audio_taxa_label_options[which(audio_taxa_label_options$list_type %in% c('numeric_list', 'checkbox_list')), 'pk_librarylistid']) {
        items <- librarylistitems[which(librarylistitems$fk_librarylistid == the_tag & librarylistitems$item != 'template'),]
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
      
      # Reset taxon annotags
      lapply(
        audio_taxa_label_options[which(audio_taxa_label_options$list_type %in% c('dropdown_list', 'bbox_required_list')), 'pk_librarylistid'], 
        function(x) {
          updateSelectizeInput(
            session,
            x,
            selected = ""
          )
        }
      )
      
      lapply(
        audio_taxa_label_options[which(audio_taxa_label_options$list_type == 'numeric_list'), 'pk_librarylistid'],
        function(x) {
          items <- librarylistitems[which(librarylistitems$fk_librarylistid == x),]
          if (nrow(items) > 0) {
            item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
            lapply(item_ids, function(x) {
              updateNumericInput(
                session,
                inputId = x,
                value = NULL
              )
            })
          }
        }
      )
      
      lapply(
        audio_taxa_label_options[which(audio_taxa_label_options$list_type == 'checkbox_list'), 'pk_librarylistid'],
        function(x) {
          items <- librarylistitems[which(librarylistitems$fk_librarylistid == x & librarylistitems$item != "template"),]
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
    
    annotations <- reactive({
      req(input$taxon)
      stmnt <- 'SELECT pk_annotationid, fk_mediaid, media.filename, fk_taxonid, x_min, x_max, y_min, y_max 
      FROM annotations 
      LEFT JOIN media ON annotations.fk_mediaid = media.pk_mediaid 
      WHERE pk_annotationid IN 
      (SELECT fk_annotationid FROM annotags 
      LEFT JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid 
      WHERE librarylistitems.item = \'template\')'
      
      params <- list()
      param_counter <- 1
      
      if (!is.null(input$taxon) && input$taxon != "") {
        stmnt <- paste0(stmnt, ' AND fk_taxonid = $', param_counter)
        params[[param_counter]] <- input$taxon
        param_counter <- param_counter + 1
      }
      
      for (t_label in audio_taxa_label_options[which(audio_taxa_label_options$list_type %in% c('dropdown_list', 'bbox_required_list')), 'pk_librarylistid']) {
        if (!is.null(input[[t_label]]) && !is.na(input[[t_label]]) && input[[t_label]] != "") {
          stmnt <- paste0(
            stmnt, 
            ' AND pk_annotationid IN (SELECT fk_annotationid FROM annotags 
            WHERE fk_librarylistitemid = $', param_counter, ')'
          )
          params[[param_counter]] <- input[[t_label]]
          param_counter <- param_counter + 1
        }
      }
      
      for (t_label in audio_taxa_label_options[which(audio_taxa_label_options$list_type %in% c('numeric_list')), 'pk_librarylistid']) {
        items <- librarylistitems[which(librarylistitems$fk_librarylistid == t_label),]
        if (nrow(items) > 0) {
          items$item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
          for (i in seq_len(nrow(items))) {
            if (!is.null(input[[items$item_ids[i]]]) && !is.na(input[[items$item_ids[i]]])) {
              stmnt <- paste0(
                stmnt, 
                ' AND pk_annotationid IN (SELECT fk_annotationid FROM annotags WHERE fk_librarylistitemid = $', param_counter,
              )
              params[[param_counter]] <- items$pk_librarylistitemid[i]
              param_counter <- param_counter + 1
              
              stmnt <- paste0(
                stmnt,
                ' AND value_num = $', param_counter, ')'
              )
              params[[param_counter]] <- input[[items$item_ids[i]]]
              param_counter <- param_counter + 1
            }
          }
        }
      }
      
      for (t_label in audio_taxa_label_options[which(audio_taxa_label_options$list_type %in% c('checkbox_list')), 'pk_librarylistid']) {
        items <- librarylistitems[which(librarylistitems$fk_librarylistid == t_label & librarylistitems$item != 'template'),]
        if (nrow(items) > 0) {
          items$item_ids <- paste0(items$item, "_", items$pk_librarylistitemid)
          for (i in seq_len(nrow(items))) {
            if (!is.null(input[[items$item_ids[i]]]) && input[[items$item_ids[i]]]) {
              stmnt <- paste0(
                stmnt, 
                ' AND pk_annotationid IN (SELECT fk_annotationid FROM annotags WHERE fk_librarylistitemid = $', param_counter, ')'
              )
              params[[param_counter]] <- items$pk_librarylistitemid[i]
              param_counter <- param_counter + 1
            }
          }
        }
      }
      
      stmnt <- paste0(stmnt, ';')
      
      # Send parameterized query
      rs <- dbSendQuery(con(), stmnt)
      dbBind(rs, params)
      annos <- dbFetch(rs)
      dbClearResult(rs)
      
      annos
    })
    
    
    output$annotations <- renderReactable({
      
      reactable(
        annotations()[which(!(names(annotations()) %in% c('pk_annotationid')))], 
        searchable = FALSE,
        details = function(index) {
          stmnt <- paste0(
            'SELECT pk_annotagid, librarylistitems.fk_librarylistid, librarylistitems.item, annotags.value_num 
            FROM annotags 
            LEFT JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid 
            WHERE annotags.fk_annotationid = ', annotations()$pk_annotationid[index], 
            ' AND librarylistitems.item != \'template\';')
          
          behaviours <- dbGetQuery(con(), stmnt)
          
          reactable(behaviours)
        }
      )
      
    })
    
    return(
      reactiveValues(
        selectedTaxon = reactive(input$taxon),
        selectedAnnos = reactive(annotations())
      )
    )
  })
}
