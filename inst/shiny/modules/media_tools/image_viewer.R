css <- "body{ background-color: ivory; 
}
.boxNew {
        border: 3px solid #FFFF00;
        position: absolute;
}

.boxOld {
        border: 3px solid #FF0000;
        position: absolute;
}

.rectBOLD {
        border: 6px solid #FF0000;
        position: absolute;
}

}
"

image_viewer_ui <- function(id, viewer_mode) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    # Define "hot keys"
    tags$script(HTML(paste0(
      "$(function(){", 
      "$(document).keyup(function(e) {",
      "if (e.which == 37) {", # left-arrow
      "$('#",
      ns('prev_photo'),
      "').click()",
      "}",
      "});",
      "})"
    ))),
    tags$script(HTML(paste0(
      "$(function(){", 
      "$(document).keyup(function(e) {",
      "if (e.which == 39) {", # right-arrow
      "$('#",
      ns('next_photo'),
      "').click()",
      "}",
      "});",
      "})"
    ))),
    # JavaScript to reload page on custom message
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('reload', function(message) {
      location.reload();
      });"
    )),
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(css),
        tags$style(HTML(
          paste0(
            "#", ns(''), "filters_applied ",
            "{background-color: yellow; font-size: 20px; font-style: bold;}"
          )
        ))
      ),
      fluidRow(
        fluidRow(
          column(
            width = 11,
            switch(
              viewer_mode,
              "viewer" = tags$h2('Photo Viewer'),
              "tagger" = tags$h2('Photo Tagger'),
              "verifier" = tags$h2('Annotation Verifier'),
              "modelOutputs" = tags$h2('Model Verifier')
            )
          ),
          column(
            width = 1,
            actionButton(ns("reset_app"), "Return to login")
          )
        ),
        shinydashboard::box(
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          title = 'Image Filters & Settings',
          fluidRow(
            column(
              6,
              wellPanel(
                tags$h3('Visit Filters'),
                shiny::tags$br(),
                shiny::tags$i('Select a location to show visits'),
                reactable::reactableOutput(ns('filterVisitTable'))
              ),
              wellPanel(
                tags$h3('Jump to photo'),
                span(textOutput(ns('truncated_list')), style="color:red"),
                tags$br(),
                selectInput(
                  ns('goto_photo'),
                  'Select Photo:',
                  choices = NULL
                )
              ),
              textOutput(ns('filters_applied')),
              actionButton(ns('apply_filters'), 'Apply Filters', class = "btn-warning"),
            ),
            column(
              3,
              wellPanel(
                tags$h3('Image Filters'),
                selectInput(
                  ns('filterLocation'),
                  'Select a location',
                  choices = 'all'
                ),
                dateRangeInput(
                  ns('filterDateRange'),
                  'Select date range (default includes all dates)',
                  start = '1900-01-01',
                  end = '2100-01-01',
                  min = '1900-01-01',
                  max = '2100-01-01',
                  startview = 'year'
                ),
                selectInput(
                  ns('filterTaxa'),
                  'Select taxa',
                  choices = c('all')
                ),
                if (viewer_mode == "tagger") {
                  radioButtons(
                    ns('excludeAnnotated'),
                    label = 'Exclude annotated by',
                    choices = c('NA', 'Me', 'Anyone'),
                    selected = 'NA'
                  )
                } else {
                  shinyjs::hidden(radioButtons(
                    ns('excludeAnnotated'),
                    label = 'Exclude if annotated by',
                    choices = c('NA', 'Me', 'Anyone'),
                    selected = 'NA'
                  ))
                },
                if (viewer_mode %in% c("modelOutputs", "verifier")) {
                  radioButtons(
                    ns('excludeAnnoVerified'),
                    label = 'Exclude if verified by',
                    choices = c('NA', 'Me', 'Anyone'),
                    selected = 'NA'
                  )
                } else {
                  shinyjs::hidden(radioButtons(
                    ns('excludeAnnoVerified'),
                    label = 'Exclude verified from',
                    choices = c('NA', 'Me', 'Anyone'),
                    selected = 'NA'
                  ))
                }
              ),
              {if (viewer_mode == "modelOutputs") {
                wellPanel(
                  shiny::tags$h3('Model Filters'),
                  selectInput(
                    ns('modelID'),
                    'Select model',
                    choices =  'all',
                    selected = 'all'
                  ),
                  numericInput(
                    ns('modelConf'),
                    'Model Value:',
                    value = NA
                  ),
                  checkboxInput(
                    ns('modelLessThan'),
                    'Less than Value',
                    value = FALSE
                  )
                )
              }},
            ),
            column(
              3,
              wellPanel(
                shiny::tags$h3('Photo Settings'),
                textInput(
                  ns('imagePathURL'),
                  'Image directory path/URL',
                  value = isolate(
                    ifelse(
                      grepl("(\\/$)|(^$|)",IMG_PATH()),
                      IMG_PATH(),
                      paste(IMG_PATH(), '/', sep = "")
                    )
                  )
                ),
                selectInput(
                  ns('dim'),
                  'Image size',
                  choices = c(600,800,1000,1200), 
                  selected = '800'
                ),
                {if (viewer_mode == "viewer") {
                  checkboxInput(
                    inputId = ns('viewModelOutputs'),
                    label = 'Show Model Outputs',
                    value = FALSE
                  )
                }}
              ),
              wellPanel(
                shiny::tags$h3('Viewer Settings'),
                checkboxInput(
                  ns('random_order'),
                  'Randomize photo order'
                ),
                numericInput(
                  ns('cache_size'),
                  'Cache size (# of images)',
                  value = 50, 
                  min = 1,
                  step = 1
                ),
                switch(
                  viewer_mode,
                  'viewer' = character(0),
                  numericInput(
                    ns('autosave_rate'),
                    'Auto-save Rate (# of images)',
                    value = 1, 
                    min = 1,
                    step = 1
                  )
                )
              )
            )
          ),
          textOutput(ns('num_photos'))
        )
      ),
      column(
        10,
        textOutput(ns('image_meta')),
        uiOutput(ns('canvas'), inline = TRUE),
        fluidRow(
          # Navigate between photos
          column(
            8,
            tags$br(),
            switch(
              viewer_mode,
              modelOutputs = tagList(
                actionButton(inputId = ns("prev_photo"), label = "Previous photo"),
                actionButton(inputId = ns("next_photo"), label = "Next photo"),
                actionButton(ns('save_metadata'), 'Save Labels')
              ),
              tagger = tagList(
                actionButton(inputId = ns("prev_photo"), label = "Previous photo"),
                actionButton(inputId = ns("next_photo"), label = "Next photo"),
                shinyjs::disabled(actionButton(ns('save_metadata'), 'Save Labels'))
              ),
              verifier = tagList(
                actionButton(inputId = ns("prev_photo"), label = "Previous photo"),
                actionButton(inputId = ns("next_photo"), label = "Next photo"),
                actionButton(ns('save_metadata'), 'Save Labels')
              ),
              tagList(
                actionButton(inputId = ns("prev_photo"), label = "Previous photo"),
                actionButton(inputId = ns("next_photo"), label = "Next photo"),
                shinyjs::hidden(actionButton(ns('save_metadata'), 'Save Labels'))
              )
            )
          )
        )
      )
    )
  )
}

image_viewer_server <- function(id, selectedUser = reactive(NA), active = reactive(TRUE), viewer_mode = "viewer", updateTags = reactive(NA), annotations_cache = reactive(NA), selected_rows = reactive(NA), deleted_rows = reactive(NA), verifications_cache = reactive(NA)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Javascript supporing bounding boxes in the tagger ------------------
    theJS <- paste0("
  <script> 
    var element = null;  
    var oldCoords = null;
    var ct = 1;
    var myPlot = document.getElementById('", ns('theDiv'), "'); 
  </script>
        
  <script>
    myPlot.onclick = e =>  {
      var myCanv = document.getElementById('", ns('theDiv'), "'); 

      newCoords =  [e.pageX - $(myPlot).offset().left, e.pageY - $(myPlot).offset().top];
      if (oldCoords == null) {
        oldCoords = newCoords;
        Shiny.onInputChange('",ns('new_bbox_coords'), "', newCoords);
        Shiny.onInputChange('",ns('boxNum'),"', ct);
      } else if (e.detail === 2) {
        oldCoords = null;
        ct -= 1;
        document.getElementById('", ns(''), "newDiv' + String(ct)).remove();
        setTimeout(function() {Shiny.onInputChange('",ns('new_bbox_coords'),"', 1); }, 50);
      } else {
       

        document.getElementById('", ns(''), "newDiv' + String(ct-1)).remove()
        oldCoords = null;
        Shiny.onInputChange('",ns('new_bbox_coords'), "', newCoords);
        Shiny.onInputChange('",ns('boxNum'),"', ct);
      };
      ct += 1;
    }
  </script>
"
    )
    
    # Define reactive values
    i_photo <- reactiveVal(1) # Index of current photo displayed
    last_photo <- reactiveVal('') # Index of last photo displayed
    visitID <- reactiveVal() # Visit ID (if selected in filters)
    the_bboxes <- reactiveVal(data.frame(
      x_min = numeric(0),
      x_max = numeric(0),
      y_min = numeric(0),
      y_max = numeric(0)
    ))
    point_cache <- reactiveVal() # Keep track of points/clicks for bounding boxes
    
    date_ranges <- reactiveVal(AMMonitor::qryMediaDateRange(con(), "photo"))
    
    if (file.exists(paste(ammPath, 'settings', 'cache_size.txt', sep = '/'))) {
      # readLines (not read.csv) so a file without a trailing newline -- the
      # common case when it's been hand-edited -- doesn't print a spurious
      # "incomplete final line" warning on every app start.
      cache_size <- suppressWarnings(as.numeric(trimws(readLines(
        paste(ammPath, 'settings', 'cache_size.txt', sep = '/'),
        warn = FALSE
      )[1])))
      if (is.numeric(cache_size) && !is.na(cache_size)) {
        updateNumericInput(
          session,
          'cache_size',
          'Cache size (# of images)',
          value = cache_size, 
          min = 1,
          step = 1
        )
      }
    }
    
    if (file.exists(paste(ammPath, 'settings', 'autosave_rate.txt', sep = '/'))) {
      save_rate <- read.csv(
        paste(ammPath, 'settings', 'autosave_rate.txt', sep = '/'),
        header = F
      )[,]
      if (is.numeric(save_rate)) {
        updateNumericInput(
          session,
          'autosave_rate',
          'Auto-save Rate (# of images)',
          value = save_rate, 
          min = 1,
          step = 1
        )
      }
    }
    
    observe({
      req(con())
      modelIDquery <- dbGetQuery(con(), 'SELECT pk_modelid, model_name FROM models ORDER BY model_name;')
      modelIDs <- modelIDquery$pk_modelid
      names(modelIDs) <- modelIDquery$model_name
      
      updateSelectInput(
        session = session,
        'modelID',
        'Select model',
        choices = c(
          all = 'all',
          modelIDs
        ),
        selected = 'all'
      )
    })
    
    # Set media path for images
    observeEvent(input$imagePathURL, {
      IMG_PATH(input$imagePathURL)
    })
    
    observeEvent(IMG_PATH(), {
      updateTextInput(
        session = session,
        'imagePathURL',
        value = IMG_PATH()
      )
    })
    
    photos_on_startup <- reactiveVal(1) # For altering startup behavior of apply_filters

    # Filtered dataframe of available photos. ignoreInit = TRUE so this
    # doesn't run (and render a photo) until the user actually presses
    # Apply Filters -- previously it fired once on load with the blank/default
    # filters, which was slow and showed a photo nobody asked to see.
    photos_avail <- eventReactive(input$apply_filters, {
      # First, save metadata cache (if needed)
      if (
        photos_on_startup() != 1 && 
        nrow(metadata_cache$cache$mediaMetaData) != 0 && (
          any(metadata_cache$cache$annotations[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotags[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotationverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotagverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$modelverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$mediatags[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$mediatagverifications[,c('is_add', 'is_delete')] == 1)
        )
      ) {
        metadata_cache <- save_metadata_cache(metadata_cache, annotations_cache())
      } 
      
      # Next, apply the filters
      output$filters_applied <- renderText("")
      photos <- switch(
        viewer_mode,
        modelOutputs = AMMonitor::qryModelOutputsMedia(
          con(),
          locationID = ifelse(is.null(input$filterLocation), 'all', input$filterLocation),
          dateRange = ifelse(
            is.null(input$filterDateRange), 
            as.list(date_ranges()),
            list(input$filterDateRange)
          ),
          visitID = visitID(),
          taxonID = ifelse(is.null(input$filterTaxa), 'all', input$filterTaxa),
          excludeAnnoVerified = input$excludeAnnoVerified,
          selectedUser = selectedUser(),
          model = input$modelID,
          confValue = input$modelConf,
          lessThan = input$modelLessThan,
          newOnly = FALSE
        ),
        verifier = AMMonitor::qryMedia(
          con(),
          locationID = ifelse(is.null(input$filterLocation), 'all', input$filterLocation),
          dateRange = ifelse(
            is.null(input$filterDateRange), 
            as.list(date_ranges()),
            list(input$filterDateRange)
          ),
          visitID = visitID(),
          taxonID = ifelse(is.null(input$filterTaxa), 'all', input$filterTaxa),
          excludeAnnoVerified = input$excludeAnnoVerified,
          selectedUser = selectedUser(),
          verify = TRUE,
          mediaType = "photo"
        ),
        tagger = AMMonitor::qryMedia(
          con(),
          locationID = ifelse(is.null(input$filterLocation), 'all', input$filterLocation),
          dateRange = ifelse(
            is.null(input$filterDateRange), 
            as.list(date_ranges()),
            list(input$filterDateRange)
          ),
          visitID = visitID(),
          taxonID = ifelse(is.null(input$filterTaxa), 'all', input$filterTaxa),
          excludeAnnotated = input$excludeAnnotated,
          selectedUser = selectedUser(),
          verify = FALSE,
          mediaType = "photo"
        ),
        AMMonitor::qryMedia(
          con(),
          locationID = ifelse(is.null(input$filterLocation), 'all', input$filterLocation),
          dateRange = ifelse(
            is.null(input$filterDateRange), 
            as.list(date_ranges()),
            list(input$filterDateRange)
          ),
          visitID = visitID(),
          taxonID = ifelse(is.null(input$filterTaxa), 'all', input$filterTaxa),
          selectedUser = NA,
          verify = FALSE,
          mediaType = "photo"
        )
      )
      if (input$random_order) {
        photos <- photos[sample(1:nrow(photos)),]
      }
      i_photo(1)
      i_cache(1)
      photos
    }, ignoreInit = TRUE)
    
    i_cache <- reactiveVal(1) # Initialize cache counter
    
    metadata_cache <- reactiveValues(
      i_cache_start = NA,
      i_cache_end = NA,
      cache = list(
        mediaMetaData = NA,
        annotations = NA,
        annotags = NA,
        annotationverifications = NA,
        annotagverifications = NA,
        modeloutputs = NA,
        modelverifications = NA,
        mediatags = NA,
        mediatagverifications = NA
      )
    )
    
    # Update metadata cache based on annotations cache
    observe({
      req(annotations_cache())
      if (!nrow(annotations_cache()$annotations) == 0) {
        metadata_cache$cache$annotations <- rbind(
          metadata_cache$cache$annotations,
          annotations_cache()$annotations
        )
      }
      
      if (!nrow(annotations_cache()$annotags) == 0) {
        metadata_cache$cache$annotags <- rbind(
          metadata_cache$cache$annotags,
          annotations_cache()$annotags
        )
      }
      
      if (!nrow(annotations_cache()$mediatags) == 0) {
        metadata_cache$cache$mediatags <- rbind(
          metadata_cache$cache$mediatags,
          annotations_cache()$mediatags
        )
      }
    }) |> bindEvent(annotations_cache())
    
    # Trigger updates to database if cache size exceeded
    observe({
      req(!is.na(metadata_cache$cache$annotations))
      total_changes <- sum(
        metadata_cache$cache$annotations[,c('is_add', 'is_delete')],
        metadata_cache$cache$annotags[,c('is_add', 'is_delete')],
        metadata_cache$cache$mediatags[,c('is_add', 'is_delete')],
        metadata_cache$cache$annotationverifications[,c('is_add', 'is_delete')],
        metadata_cache$cache$annotagverifications[,c('is_add', 'is_delete')],
        metadata_cache$cache$mediatagverifications[,c('is_add', 'is_delete')],
        metadata_cache$cache$modelverifications[,c('is_add', 'is_delete')]
      )
      
      if (viewer_mode != "viewer" && total_changes >= input$autosave_rate) {
        save_metadata_now(TRUE)
      }
    }) |> bindEvent(annotations_cache(), verifications_cache(), deleted_rows())
    
    # Update metadata cache based on verifications
    observe({
      req(verifications_cache())
      
      # Add/remove annotation verifications
      for (i in seq_len(nrow(verifications_cache()$annotationverifications))) {
        the_annoverification <- verifications_cache()$annotationverifications[i,]
        
        # Check if cache says to delete
        if (the_annoverification$is_delete) {
          # If not in the db yet, just remove it from the cache
          if (the_annoverification$pk_annoverificationid < 0) {
            metadata_cache$cache$annotationverifications <- metadata_cache$cache$annotationverifications[
              metadata_cache$cache$annotationverifications$pk_annoverificationid != the_annoverification$pk_annoverificationid,
            ]
          } else {
            # If it's in the db, just mark it for deletion
            metadata_cache$cache$annotationverifications$is_delete[
              metadata_cache$cache$annotationverifications$pk_annoverificationid == the_annoverification$pk_annoverificationid
            ] <- 1
          }
          next
        }
        
        # If no verification already exists, add it
        if (! the_annoverification$pk_annoverificationid %in% metadata_cache$cache$annotationverifications$pk_annoverificationid) {
          metadata_cache$cache$annotationverifications <- rbind(
            metadata_cache$cache$annotationverifications,
            the_annoverification
          )
        } else {
          # If an verification exists in the cache, update it
          matching_annoverif_mask <- metadata_cache$cache$annotationverifications$pk_annoverificationid == verifications_cache()$annotationverifications$pk_annoverificationid
          
          metadata_cache$cache$annotationverifications$is_valid[matching_annoverif_mask] <- the_annoverification$is_valid
          metadata_cache$cache$annotationverifications$is_add[matching_annoverif_mask] <- 1
          metadata_cache$cache$annotationverifications$is_delete[matching_annoverif_mask] <- 0
        }
      }
      
      # Add/remove annotag verifications
      for (i in seq_len(nrow(verifications_cache()$annotagverifications))) {
        the_annotagverification <- verifications_cache()$annotagverifications[i,]
        
        # Check if cache says to delete
        if (the_annotagverification$is_delete) {
          # If not in the db yet, just remove it from the cache
          if (the_annotagverification$pk_tagverificationid < 0) {
            metadata_cache$cache$annotagverifications <- metadata_cache$cache$annotagverifications[
              metadata_cache$cache$annotagverifications$pk_tagverificationid != the_annotagverification$pk_tagverificationid,
            ]
          } else {
            # If it's in the db, just mark it for deletion
            metadata_cache$cache$annotagverifications$is_delete[
              metadata_cache$cache$annotagverifications$pk_tagverificationid == the_annotagverification$pk_tagverificationid
            ] <- 1
          }
          next
        }
        
        # If no verification already exists, add it
        if (! the_annotagverification$fk_annotagid %in% metadata_cache$cache$annotagverifications$fk_annotagid) {
          metadata_cache$cache$annotagverifications <- rbind(
            metadata_cache$cache$annotagverifications,
            the_annotagverification
          )
        } else {
          # If an verification exists in the cache, update it
          matching_annotagverif_mask <- metadata_cache$cache$annotagverifications$pk_tagverificationid == verifications_cache()$annotagverifications$pk_tagverificationid[i]
          
          metadata_cache$cache$annotagverifications$is_valid[matching_annotagverif_mask] <- the_annotagverification$is_valid
          metadata_cache$cache$annotagverifications$is_add[matching_annotagverif_mask] <- 1
          metadata_cache$cache$annotagverifications$is_delete[matching_annotagverif_mask] <- 0
        }
      }
      
      # Add/remove mediatag verifications
      for (i in seq_len(nrow(verifications_cache()$mediatagverifications))) {
        the_mediatagverification <- verifications_cache()$mediatagverifications[i,]
        
        # Check if cache says to delete
        if (the_mediatagverification$is_delete) {
          # If not in the db yet, just remove it from the cache
          if (the_mediatagverification$pk_mediatagverificationid < 0) {
            metadata_cache$cache$mediatagverifications <- metadata_cache$cache$mediatagverifications[
              metadata_cache$cache$mediatagverifications$pk_mediatagverificationid != the_mediatagverification$pk_mediatagverificationid,
            ]
          } else {
            # If it's in the db, just mark it for deletion
            metadata_cache$cache$mediatagverifications$is_delete[
              metadata_cache$cache$mediatagverifications$pk_mediatagverificationid == the_mediatagverification$pk_mediatagverificationid
            ] <- 1
          }
          next
        }
        
        # If no verification already exists, add it
        if (! the_mediatagverification$pk_mediatagverificationid %in% metadata_cache$cache$mediatagverifications$pk_mediatagverificationid) {
          metadata_cache$cache$mediatagverifications <- rbind(
            metadata_cache$cache$mediatagverifications,
            the_mediatagverification
          )
        } else {
          # If an verification exists in the cache, update it
          matching_mediatagverif_mask <- metadata_cache$cache$mediatagverifications$pk_mediatagverificationid == verifications_cache()$mediatagverifications$pk_mediatagverificationid[i]
          
          metadata_cache$cache$mediatagverifications$is_valid[matching_mediatagverif_mask] <- the_mediatagverification$is_valid
          metadata_cache$cache$mediatagverifications$is_add[matching_mediatagverif_mask] <- 1
          metadata_cache$cache$mediatagverifications$is_delete[matching_mediatagverif_mask] <- 0
        }
      }
      
      # Add/remove model verifications
      for (i in seq_len(nrow(verifications_cache()$modelverifications))) {
        the_modelverification  <- verifications_cache()$modelverifications[i,]
        
        # Check if cache says to delete
        if (the_modelverification$is_delete) {
          # If not in the db yet, just remove it from the cache
          if (the_modelverification$pk_modelverificationid < 0) {
            metadata_cache$cache$modelverifications <- metadata_cache$cache$modelverifications[
              metadata_cache$cache$modelverifications$pk_modelverificationid != the_modelverification$pk_modelverificationid,
            ]
          } else {
            # If it's in the db, just mark it for deletion
            metadata_cache$cache$modelverifications$is_delete[
              metadata_cache$cache$modelverifications$pk_modelverificationid == the_modelverification$pk_modelverificationid
            ] <- 1
          }
          next
        }
        
        # If no verification already exists, add it
        if (! the_modelverification$pk_modelverificationid %in% metadata_cache$cache$modelverifications$pk_modelverificationid) {
          metadata_cache$cache$modelverifications <- rbind(
            metadata_cache$cache$modelverifications,
            the_modelverification
          )
        } else {
          # If an verification exists in the cache, update it
          matching_modelverif_mask <- metadata_cache$cache$modelverifications$pk_modelverificationid == verifications_cache()$modelverifications$pk_modelverificationid[i]
          
          metadata_cache$cache$modelverifications$is_valid[matching_modelverif_mask] <- the_modelverification$is_valid
          metadata_cache$cache$modelverifications$is_add[matching_modelverif_mask] <- 1
          metadata_cache$cache$modelverifications$is_delete[matching_modelverif_mask] <- 0
        }
      }
    }) |> bindEvent(verifications_cache())
    
    save_metadata_now <- reactiveVal(FALSE)
    
    observe({
      if (save_metadata_now()) {
        metadata_cache <- save_metadata_cache(metadata_cache, annotations_cache())
      }
      save_metadata_now(FALSE)
    }) |> bindEvent(save_metadata_now(), i_cache())
    
    
    # Update the metadata cache
    observe(priority = 9999, {
      # First, save any unsaved tags (if needed)
      if (
        photos_on_startup() != 1 && 
        nrow(metadata_cache$cache$mediaMetaData) != 0 && (
          any(metadata_cache$cache$annotations[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotags[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotationverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotagverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$modelverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$mediatags[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$mediatagverifications[,c('is_add', 'is_delete')] == 1)
        )
      ) {
        metadata_cache <- save_metadata_cache(metadata_cache, annotations_cache())
      } 
      
      i_cache_start <- max(1, input$cache_size*(i_cache()-1))
      i_cache_end <- min(input$cache_size*i_cache(), nrow(photos_avail()))
      if (nrow(photos_avail()) == 0) {
        cache_media <- integer(0)
        cache_annotations <- DBI::dbGetQuery(con(), 'SELECT * FROM annotations WHERE fk_mediaID = -99;')
        cache_annotags <- dbGetQuery(con(), 'SELECT annotags.*, fk_librarylistid, item FROM annotags INNER JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid WHERE fk_annotationid = -99;')
        cache_annotags <- dbGetQuery(con(), 'SELECT annotags.*, fk_librarylistid, item FROM annotags INNER JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid WHERE fk_annotationid = -99;')
        cache_modeloutputs <- dbGetQuery(con(), 'SELECT model_name, pk_modeloutputid, fk_librarylistitemid, fk_mediaid, fk_medialistitemid, modeloutputs.fk_taxonid, value_num FROM modeloutputs INNER JOIN models ON modeloutputs.fk_modelid = models.pk_modelid WHERE fk_mediaid = -99;')
        cache_mediatags <- dbGetQuery(con(), 'SELECT pk_mediatagid, fk_mediaid, fk_personid, fk_medialistid, item, value_num FROM mediatags INNER JOIN medialistitems ON mediatags.fk_medialistitemid = medialistitems.pk_medialistitemid WHERE pk_mediatagid = -99;')
      } else {
        cache_media <- photos_avail()$pk_mediaid[i_cache_start:i_cache_end]
        cache_annotations <- DBI::dbGetQuery(
          con(),
          paste(
            'SELECT annotations.*, 0 AS is_add, 0 AS is_delete FROM annotations WHERE fk_mediaID IN (',
            ifelse(
              length(cache_media) != 0,
              paste(cache_media, collapse = ', '),
              "NULL"
            ),
            ')',
            ifelse(
              input$filterTaxa == "all",
              "",
              paste0(
                " AND annotations.fk_taxonid = '",
                gsub("'", "''", input$filterTaxa),
                "'"
              )
            ),
            ';'
          )
        )
        cache_annotags <- dbGetQuery(
          con(),
          paste(
            'SELECT annotags.*, fk_librarylistid, item, 0 AS is_add, 0 AS is_delete FROM annotags INNER JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid WHERE fk_annotationid IN (',
            ifelse(
              nrow(cache_annotations) != 0,
              paste(unique(cache_annotations$pk_annotationid), collapse = ', '),
              "NULL"
            ),
            ');'
          )
        )
        cache_modeloutputs <- dbGetQuery(
          con(),
          paste(
            'SELECT model_name, pk_modeloutputid, fk_librarylistitemid, fk_mediaid, fk_medialistitemid, x_min, x_max, y_min, y_max, modeloutputs.fk_taxonid, value_num, 0 AS is_add, 0 AS is_delete FROM modeloutputs INNER JOIN models ON modeloutputs.fk_modelid = models.pk_modelid WHERE fk_mediaid IN (',
            ifelse(
              length(cache_media) != 0,
              paste(cache_media, collapse = ', '),
              "NULL"
            ),
            ') ',
            ifelse(
              input$filterTaxa == "all",
              "",
              paste0(
                " AND modeloutputs.fk_taxonid = '",
                gsub("'", "''", input$filterTaxa),
                "'"
              )
            ),
            ifelse(
              input$modelID == "all",
              "",
              paste0(' AND modeloutputs.fk_modelid = ', input$modelID)
            ),
            ifelse(
              is.na(input$modelConf),
              "",
              paste0(
                ' AND modeloutputs.value_num',
                ifelse(
                  input$modelLessThan,
                  " <= ",
                  " >= "
                ),
                input$modelConf
              )
            ),
            ';'
          )
        )
        cache_mediatags <- dbGetQuery(
          con(),
          paste(
            'select mediatags.*, fk_medialistid, item, 0 AS is_add, 0 AS is_delete FROM mediatags INNER JOIN medialistitems ON mediatags.fk_medialistitemid = medialistitems.pk_medialistitemid WHERE fk_mediaid IN (',
            ifelse(
              length(cache_media) != 0,
              paste(cache_media, collapse = ', '),
              "NULL"
            ),
            ");"
          )
        )
      }
      
      metadata_cache$i_cache_start = i_cache_start
      metadata_cache$i_cache_end = i_cache_end
      metadata_cache$cache = list(
        mediaMetaData = dbGetQuery(
          con(),
          paste(
            'SELECT pk_mediaid, fk_locationid FROM media INNER JOIN visits ON media.fk_visitid = visits.pk_visitid WHERE pk_mediaid IN (',
            ifelse(
              length(cache_media) != 0,
              paste(cache_media, collapse = ', '),
              'NULL'
            ),
            ');'
          )
        ),
        annotations = cache_annotations,
        annotags = cache_annotags,
        annotationverifications = dbGetQuery(
          con(),
          paste(
            'SELECT annotationverifications.*, 0 AS is_add, 0 AS is_delete FROM annotationverifications WHERE fk_annotationid IN (',
            ifelse(
              nrow(cache_annotations) != 0,
              paste(unique(cache_annotations$pk_annotationid), collapse = ', '),
              "NULL"
            ),
            ');'
          )
        ),
        annotagverifications = dbGetQuery(
          con(),
          paste(
            'SELECT annotagverifications.*, 0 AS is_add, 0 AS is_delete FROM annotagverifications WHERE fk_annotagid IN (',
            ifelse(
              nrow(cache_annotags) != 0,
              paste(unique(cache_annotags$pk_annotagid), collapse = ', '),
              "NULL"
            ),
            ');'
          )
        ),
        modeloutputs = cache_modeloutputs,
        modelverifications = dbGetQuery(
          con(),
          paste(
            'SELECT modelverifications.*, 0 AS is_add, 0 AS is_delete FROM modelverifications WHERE fk_modeloutputid IN (',
            ifelse(
              nrow(cache_modeloutputs) != 0,
              paste(cache_modeloutputs$pk_modeloutputid, collapse = ', '),
              "NULL"
            ),
            ');'
          )
        ),
        mediatags = cache_mediatags,
        mediatagverifications = dbGetQuery(
          con(),
          paste(
            'SELECT mediatagverifications.*, 0 AS is_add, 0 AS is_delete FROM mediatagverifications WHERE fk_mediatagid IN (',
            ifelse(
              nrow(cache_mediatags) != 0,
              paste(cache_mediatags$pk_mediatagid, collapse = ', '),
              "NULL"
            ),
            ');'
          )
        )
      )
    }) |> bindEvent(photos_avail(), i_cache(), input$cache_size)
    
    # Update cache counter when needed
    observe(priority = 9999, {
      if (i_photo() < metadata_cache$i_cache_start || i_photo() > metadata_cache$i_cache_end) {
        i_cache(ceiling(i_photo() / input$cache_size))
        save_metadata_now(TRUE)
      }
    })
    
    # Update metadata upon annotation deletion
    observe({
      req(deleted_rows())
      if (length(deleted_rows()$annotags) > 0) {
        for (i in seq_len(nrow(deleted_rows()$annotags))) {
          i_delete <- which(metadata_cache$cache$annotations$pk_annotationid == deleted_rows()$annotags$pk_annotationid[i])
          
          # Remove the annotags and annotations
          if (!is.na(deleted_rows()$annotags$pk_annotagid[i])) {
            # browser()
            i_delete_annotags <- which(
              metadata_cache$cache$annotags$pk_annotagid == deleted_rows()$annotags$pk_annotagid[i]
            )
            
            # If deleting a new annotag, just remove from the metadata cache
            if (metadata_cache$cache$annotags$is_add[i_delete_annotags] == 1) {
              metadata_cache$cache$annotags <- metadata_cache$cache$annotags[-i_delete_annotags,]
            } else {
              metadata_cache$cache$annotags$is_delete[i_delete_annotags] <- 1
            }
          } else {
            # If deleting a new annotation, just remove from the metadata cache
            if (metadata_cache$cache$annotations$is_add[i_delete] == 1) {
              metadata_cache$cache$annotations <- metadata_cache$cache$annotations[-i_delete,]
            } else {
              metadata_cache$cache$annotations$is_delete[i_delete] <- 1
            }
          }
        }
      }
      
      # Remove the mediatags
      if (length(deleted_rows()$mediatags) != 0) {
        for (i in seq_len(length(deleted_rows()$mediatags))) {
          i_delete <- which(metadata_cache$cache$mediatags$pk_mediatagid == deleted_rows()$mediatags[i])
          
          # If deleting a new annotation, just remove from the metadata cache
          if (metadata_cache$cache$mediatags$is_delete[i_delete] == 1) {
            metadata_cache$cache$mediatags <- metadata_cache$cache$mediatags[-i_delete,]
          } else {
            metadata_cache$cache$mediatags$is_delete[i_delete] <- 1
          }
        }
      }
    }) |> bindEvent(deleted_rows())
    
    # Display image metadata (above the image)
    output$image_meta <- renderText({
      if (nrow(photos_avail()) >= i_photo()) {
        paste0(
          'Photo ',
          i_photo(),
          ' of ',
          nrow(photos_avail()),
          '; File Name: ',
          photos_avail()$filename[i_photo()],
          '; Location: ', 
          metadata_cache$cache$mediaMetaData$fk_locationid[
            metadata_cache$cache$mediaMetaData$pk_mediaid == photos_avail()$pk_mediaid[i_photo()]
          ],
          '; Date/Time: ', 
          paste(photos_avail()[i_photo(), c('start_date', 'start_time')], collapse = ' ')
        )
      } else {
        ""
      }
    })
    
    # Display a warning if there are un-applied filters selected
    observe({
      if (photos_on_startup() != 1) {
        output$filters_applied <- renderText({
          "Warning: Un-applied filters selected. Press \"apply filters\" to apply changes."
        })
      }
      photos_on_startup(0)
    }) |> bindEvent(
      input$filterLocation, 
      input$filterTaxa, 
      input$excludeAnnotated,
      input$excludeAnnoVerified,
      input$filterDateRange, 
      input$modelID,
      input$modelConf,
      input$modelLessThan,
      visitID(),
      input$random_order,
      ignoreInit = TRUE
    )
    
    # Display the image
    output$canvas <- renderUI({
      deleted_rows()
      updateTags()
      input$viewModelOutputs
      if (nrow(photos_avail())) {
        img_url <- ifelse(
          !is.na(photos_avail()$filepath[i_photo()]),
          photos_avail()$filepath[i_photo()],
          paste0(
            input$imagePathURL,
            ifelse(endsWith(input$imagePathURL, '/') || input$imagePathURL == "", "", "/"),
            photos_avail()$filename[i_photo()]
          )
        )
        
        # For local (non web-based) paths, add the resource path
        if (!grepl('http', img_url)) {
          if (!dirname(img_url) %in% resourcePaths()) {
            addResourcePath('photos', dirname(img_url))
          }
          img_url <- paste0('photos/', basename(img_url))
        }
        
        shinyjs::runjs(HTML(paste0('function getMeta(url){
          const img = new Image();
          img.addEventListener("load", function() {
              // alert( this.naturalWidth +\' \'+ this.naturalHeight );
              Shiny.onInputChange("', ns('img_size'), '", [this.naturalWidth, this.naturalHeight])
          });
          img.src = url;
          }; getMeta("', img_url, '");')))
        
        tagList(
          HTML(paste0(
            theJS,
            tags$div(
              id = ns('theDiv'), 
              style=paste0('width:', input$dim,'px;'),
              tags$img(
                id = ns('theImg'),
                src = img_url,
                width = input$dim
              )
            )
          )),
          # Attempt to pre-load the next image, if from the web (significant speed-up!)
          if (i_photo() < nrow(photos_avail())) {
            img_next_url <- ifelse(
              !is.na(photos_avail()$filepath[i_photo()+1]),
              photos_avail()$filepath[i_photo()+1],
              paste0(
                input$imagePathURL,
                ifelse(endsWith(input$imagePathURL, '/'), "", "/"),
                photos_avail()$filename[i_photo()+1]
              )
            )
            
            if (grepl('http', img_url)) {
              tags$img(
                src = img_next_url,
                width = 0
              )
            }
          }
        )
      } else {
        tags$img(
          src = 'NoImageAvailable.JPG',
          width = input$dim
        )
      }
    })
    
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # BOUNDING BOXES --------------
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    
    observeEvent(input$new_bbox_coords, {
      req(viewer_mode == 'tagger')
      if (length(point_cache()) == 0){
        insertUI(
          selector = paste0('#', ns('theImg')), 
          where = 'afterEnd', 
          ui = tags$div(
            id = ns(paste0('newDiv', input$boxNum)), 
            class = 'boxNew',  
            style=paste0(
              "left: ", input$new_bbox_coords[1] + 15,"px; ",
              "top: ", input$new_bbox_coords[2] + 15,"px; ",
              "width: 1px; height: 1px;"
            )
          )
        )
        
        point_cache(input$new_bbox_coords)
      } else if ((length(input$new_bbox_coords) == 1)) {
        point_cache(numeric(0))
      } else {
        tag_id <- ifelse(
          test = nrow(the_bboxes()) == 0,
          yes = input$boxNum,
          no = max(input$boxNum, max(the_bboxes()$boxNum)+2)
        )
        
        insertUI(
          selector = paste0('#', ns('theImg')), 
          where = 'afterEnd', 
          ui = tags$div(
            id = ns(paste0('newDiv', tag_id)), 
            class = 'boxNew',  
            style=paste0(
              "left: ", round(min(point_cache()[1], input$new_bbox_coords[1])) + 15,"px; ",
              "top: ", round(min(point_cache()[2], input$new_bbox_coords[2])) + 20,"px; ",
              "width: ", round(abs(point_cache()[1] - input$new_bbox_coords[1])),"px; ",
              "height: ", round(abs(point_cache()[2] - input$new_bbox_coords[2])),"px;"
            )
          )
        )
        
        heightRatio <- as.numeric(input$dim)*(input$img_size[2]/input$img_size[1])
        
        the_bboxes(rbind(
          the_bboxes(),
          data.frame(
            x_min = min(point_cache()[1], input$new_bbox_coords[1])/as.numeric(input$dim),
            x_max = max(point_cache()[1], input$new_bbox_coords[1])/as.numeric(input$dim),
            y_min = min(point_cache()[2], input$new_bbox_coords[2])/heightRatio,
            y_max = max(point_cache()[2], input$new_bbox_coords[2])/heightRatio,
            boxNum = tag_id
          )
        ))
        point_cache(numeric(0))
      }
    })
    
    # Remove new boxes when photo changes
    observe({
      req(nrow(photos_avail()) > 0)
      req(photos_on_startup() != 1)
      # Remove all pending bounding boxes
      for (i_box in seq_len(nrow(the_bboxes()))) {
        removeUI(
          selector = paste0('#newDiv', the_bboxes()$boxNum[i_box]), 
          multiple = TRUE
        )
      }
      the_bboxes(the_bboxes()[0,])
      point_cache(numeric(0))
      
      # Subset of annotations with matching photoID
      if (viewer_mode == 'modelOutputs') {
        if (!is.na(input$modelConf)) {
          if (input$modelLessThan == TRUE) {
            scores_conditions_mask <- metadata_cache$cache$modeloutputs$value_num <= input$modelConf
          } else {
            scores_conditions_mask <- metadata_cache$cache$modeloutputs$value_num >= input$modelConf
          }
        } else {
          scores_conditions_mask <- rep(TRUE, nrow(metadata_cache$cache$modeloutputs))
        }
        
        the_annotations <- metadata_cache$cache$modeloutputs[
          metadata_cache$cache$modeloutputs$fk_mediaid == photos_avail()$pk_mediaid[i_photo()] & scores_conditions_mask,
        ]
        
      } else {
        user_conditions_annotations_mask <- switch(
          viewer_mode,
          "viewer" = rep(TRUE, nrow(metadata_cache$cache$annotations)),
          "tagger" = metadata_cache$cache$annotations$fk_personid == selectedUser(),
          "verifier" = metadata_cache$cache$annotations$fk_personid != selectedUser(),
        )
        
        user_conditions_mediatags_mask <- switch(
          viewer_mode,
          "viewer" = rep(TRUE, nrow(metadata_cache$cache$mediatags)),
          "tagger" = metadata_cache$cache$mediatags$fk_personid == selectedUser(),
          "verifier" = metadata_cache$cache$mediatags$fk_personid != selectedUser(),
        )
        
        the_annotations <- metadata_cache$cache$annotations[
          metadata_cache$cache$annotations$fk_mediaid == photos_avail()$pk_mediaid[i_photo()] & user_conditions_annotations_mask & metadata_cache$cache$annotations$is_delete == 0,
        ]
        
        the_mediatags <- metadata_cache$cache$mediatags[
          metadata_cache$cache$mediatags$fk_mediaid == photos_avail()$pk_mediaid[i_photo()] & user_conditions_mediatags_mask & metadata_cache$cache$mediatags$is_delete == 0,
        ]
        
        if (nrow(the_mediatags) != 0) {
          the_mediatags[setdiff(names(the_annotations), names(the_mediatags))] <- NA
        }
        
        if (nrow(the_annotations) != 0) {
          the_annotations[setdiff(names(the_mediatags), names(the_annotations))] <- NA
        }
        
        the_annotations <- rbind(the_annotations, the_mediatags)
      }
      
      the_modelOutputs <- metadata_cache$cache$modeloutputs[
        metadata_cache$cache$modeloutputs$fk_mediaid == photos_avail()$pk_mediaid[i_photo()],
      ]
      
      if (viewer_mode == 'viewer') {
        if (input$viewModelOutputs) {
          
          if (nrow(the_modelOutputs) != 0) {
            if (length(setdiff(names(the_annotations), names(the_modelOutputs))) != 0) {
              the_modelOutputs[setdiff(names(the_annotations), names(the_modelOutputs))] <- NA
            }
            
            if (nrow(the_annotations) != 0) {
              the_annotations[setdiff(names(the_modelOutputs), names(the_annotations))] <- NA
            }
            
            the_annotations <- rbind(the_annotations, the_modelOutputs)
          }
        }
      }
      
      # Add any bounding boxes for the annotations
      if (!is.null(input$dim) && nrow(the_annotations) != 0) { # Wait until the image dimensions are pulled
        for (i_box in which(!is.na(the_annotations$x_min))) {
          
          modeloutput <- !is.null(the_annotations$pk_modeloutputid[i_box]) && !is.na(the_annotations$pk_modeloutputid[i_box])
          annotation <- !is.null(the_annotations$pk_annotationid[i_box]) && !is.na(the_annotations$pk_annotationid[i_box])
          mediatag <- !is.null(the_annotations$pk_mediatagid[i_box]) && !is.na(the_annotations$pk_mediatagid[i_box])
          
          if (modeloutput) {
            id <- paste0("_modeloutput_", the_annotations$pk_modeloutputid[i_box])
          } else if (annotation) {
            id <- paste0("_annotation_", the_annotations$pk_annotationid[i_box])
          } else if (mediatag) {
            id <- paste0("_mediatag_", the_annotations$pk_mediatagid[i_box])
          }
          
          insertUI(
            selector = paste0('#', ns('theImg')), 
            where = 'afterEnd', 
            immediate = FALSE,
            ui = tags$div(
              id = ns(paste0('oldDiv', id)), 
              class = 'boxOld', 
              style=paste0(
                "left: ", the_annotations$x_min[i_box]*as.numeric(input$dim) + 15, "px; ",
                "top: ", the_annotations$y_min[i_box]*(as.numeric(input$dim)/(input$img_size[1]/input$img_size[2])) + 20, "px; ",
                "width: ", (the_annotations$x_max[i_box]-the_annotations$x_min[i_box])*as.numeric(input$dim),"px; ",
                "height: ", (the_annotations$y_max[i_box]-the_annotations$y_min[i_box])*as.numeric(input$dim)/(input$img_size[1]/input$img_size[2]),"px;"
              )
            )
          )
        }
      }
    }) |> bindEvent(photos_avail()$pk_mediaid[i_photo()], deleted_rows(), updateTags(), input$viewModelOutputs)
    
    # Highlight a box when its row is selected
    observeEvent(selected_rows(), {
      req(selectedUser(), nrow(photos_avail()) > 0)
      req(photos_on_startup() != 1)
      
      # Subset of annotations with matching photoID
      if (viewer_mode == 'modelOutputs') {
        if (!is.na(input$modelConf)) {
          if (input$modelLessThan == TRUE) {
            scores_conditions_mask <- metadata_cache$cache$modeloutputs$value_num <= input$modelConf
          } else {
            scores_conditions_mask <- metadata_cache$cache$modeloutputs$value_num >= input$modelConf
          }
        } else {
          scores_conditions_mask <- rep(TRUE, nrow(metadata_cache$cache$modeloutputs))
        }
        
        the_annotations <- metadata_cache$cache$modeloutputs[
          metadata_cache$cache$modeloutputs$fk_mediaid == photos_avail()$pk_mediaid[i_photo()] & scores_conditions_mask,
        ]
      } else {
        
        user_conditions_annotations_mask <- switch(
          viewer_mode,
          "viewer" = rep(TRUE, nrow(metadata_cache$cache$annotations)),
          "tagger" = metadata_cache$cache$annotations$fk_personid == selectedUser(),
          "verifier" = metadata_cache$cache$annotations$fk_personid != selectedUser(),
        )
        
        user_conditions_mediatags_mask <- switch(
          viewer_mode,
          "viewer" = rep(TRUE, nrow(metadata_cache$cache$mediatags)),
          "tagger" = metadata_cache$cache$mediatags$fk_personid == selectedUser(),
          "verifier" = metadata_cache$cache$mediatags$fk_personid != selectedUser(),
        )
        
        the_annotations <- metadata_cache$cache$annotations[
          metadata_cache$cache$annotations$fk_mediaid == photos_avail()$pk_mediaid[i_photo()] & user_conditions_annotations_mask,
        ]
        
        the_mediatags <- metadata_cache$cache$mediatags[
          metadata_cache$cache$mediatags$fk_mediaid == photos_avail()$pk_mediaid[i_photo()] & user_conditions_mediatags_mask,
        ]
        
        the_modelOutputs <- metadata_cache$cache$modeloutputs[
          metadata_cache$cache$modeloutputs$fk_mediaid == photos_avail()$pk_mediaid[i_photo()],
        ]
      }
      
      if (nrow(selected_rows()) == 0) {
        annoIDs_selected <- integer(0)
      } else {
        annoIDs_selected <- unique(
          switch(
            viewer_mode,
            modelOutputs = selected_rows()$pk_modeloutputid,
            selected_rows()$pk_annotationid
          )
        )
      }
      
      tags_or_modeloutputs <- switch(
        viewer_mode,
        modelOutputs = unique(the_annotations$pk_modeloutputid),
        unique(the_annotations$pk_annotationid)
      )
      
      id_type <- switch(
        viewer_mode,
        modelOutputs = '_modeloutput_',
        '_annotation_'
      )
      
      for (i in tags_or_modeloutputs) {
        if (i %in% annoIDs_selected) {
          shinyjs::removeClass(
            selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
            class = "boxOld"
          )
          shinyjs::addClass(
            selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
            class = "rectBOLD"
          )
        } else {
          shinyjs::removeClass(
            selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
            class = "rectBOLD"
          )
          shinyjs::addClass(
            selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
            class = "boxOld"
          )
        }
      }
      
      if (viewer_mode != 'modelOutputs') {
        selected_mediatags <- ifelse(
          nrow(selected_rows()) == 0,
          integer(0),
          unique(selected_rows()$pk_mediatagid)
        )
        
        id_type <- "_mediatag_"
        
        for (i in the_mediatags$pk_mediatagid) {
          if (i %in% selected_mediatags) {
            shinyjs::removeClass(
              selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
              class = "boxOld"
            )
            shinyjs::addClass(
              selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
              class = "rectBOLD"
            )
          } else {
            shinyjs::removeClass(
              selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
              class = "rectBOLD"
            )
            shinyjs::addClass(
              selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
              class = "boxOld"
            )
          }
        }
        
        if (viewer_mode == "viewer" && input$viewModelOutputs) {
          selected_modeloutputs <- ifelse(
            nrow(selected_rows()) == 0,
            integer(0),
            unique(selected_rows()$pk_modeloutputid)
          )
          
          id_type <- "_modeloutput_"
          
          for (i in the_modelOutputs$pk_modeloutputid) {
            if (i %in% selected_modeloutputs) {
              shinyjs::removeClass(
                selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
                class = "boxOld"
              )
              shinyjs::addClass(
                selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
                class = "rectBOLD"
              )
            } else {
              shinyjs::removeClass(
                selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
                class = "rectBOLD"
              )
              shinyjs::addClass(
                selector = paste0('div:is(#', ns('oldDiv'), id_type, i, ')'),
                class = "boxOld"
              )
            }
          }
        }
      }
    })
    
    # Redraw the boxes if the image size changes
    observeEvent(input$dim, {
      
      for (i_box in seq_len(nrow(the_bboxes()))) {
        insertUI(
          selector = paste0('#', ns('theImg')), 
          where = 'afterEnd', 
          ui = tags$div(
            id = ns(paste0('newDiv', the_bboxes()$boxNum[i_box])), 
            class = 'boxNew',  
            style=paste0(
              "left: ", the_bboxes()$x_min[i_box]*as.numeric(input$dim) + 15,"px; ",
              "top: ", the_bboxes()$y_min[i_box]*(as.numeric(input$dim)/(input$img_size[1]/input$img_size[2]))+ 20,"px; ",
              "width: ", (the_bboxes()$x_max[i_box]-the_bboxes()$x_min[i_box])*as.numeric(input$dim),"px; ",
              "height: ", (the_bboxes()$y_max[i_box]-the_bboxes()$y_min[i_box])*as.numeric(input$dim)/(input$img_size[1]/input$img_size[2]),"px;"
            )
          )
        )
      }
    })
    
    
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # ALL THINGS FILTERS --------------
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    
    # Image filters --------------
    observe({
      
      temp_locs <- AMMonitor::qryMediaLocations(con(), "photo")
      
      updateSelectInput(
        session,
        'filterLocation',
        choices = c('all', temp_locs), 
        selected = 'all'
      )
    })
    
    # Visit table filter ----------------
    
    visitTable <- reactive({
      AMMonitor::qryVisitTable(con(), "photo", input$filterLocation, selectedUser(), disconnect = FALSE)
    })
    
    output$filterVisitTable <- reactable::renderReactable({
      reactable::reactable(
        visitTable()[,names(visitTable()) != 'pk_visitid'],
        selection = "single", 
        onClick = "select"
      )
    })
    
    observeEvent(input$filterVisitTable__reactable__selected, {
      visitID(visitTable()$pk_visitid[input$filterVisitTable__reactable__selected])
      
      # Detect time-lapse photos ------------------------
      if (nrow(photos_avail()) != 0) {
        times_u <- unique(substr(photos_avail()$start_time, 1, 5))
        
        time_counts <- data.frame(
          time = times_u,
          freq = tabulate(match(substr(photos_avail()$start_time, 1, 5), times_u))
        )
        
        tl_mask <- time_counts$freq >= 0.75*(
          as.POSIXlt.character(max(photos_avail()$start_date)) - as.POSIXlt.character(min(photos_avail()$start_date)))
        
        if (any(tl_mask)) {
          showModal(modalDialog(
            paste(
              'possible time-lapse detected: ',
              paste(time_counts$time[tl_mask], collapse = ", ")
            )
          ))
        }
      }
    })
    
    # Date range filter ------------------------
    if (isolate(nrow(photos_avail())) != 0) {
      updateDateRangeInput(
        session,
        'filterDateRange',
        'Select date range (default includes all dates)',
        start = date_ranges()$startdate, 
        end = date_ranges()$enddate
      )
    }
    
    # Update date ranges when you select a new location
    observeEvent(input$filterLocation, {
      if (!is.null(input$filterVisitTable__reactable__selected)) {
        visitID(NULL)
      }
      if (isolate(nrow(photos_avail())) != 0) {
        updateDateRangeInput(
          session,
          'filterDateRange',
          'Select date range (default includes all dates)',
          start = date_ranges()$startdate,
          end = date_ranges()$enddate
        )
      }
    })
    
    # Taxon Filters ---------------------------
    updateSelectInput(
      session,
      'filterTaxa',
      choices = c('all', sort(
        taxon_names$pk_taxonid,
      )),
      selected = 'all'
    )
    
    # Show the number of images found with the given filters
    output$num_photos <- renderText({paste(nrow(photos_avail()), 'photos found')})
    
    # Jump-to photo options and actions
    observeEvent(photos_avail(), priority = 9999, {
      updateSelectizeInput(
        session = session,
        inputId = 'goto_photo',
        label = 'Select Photo',
        choices = photos_avail()[['filename']], 
        options = list(
          placeholder = 'Select photo ID',
          onInitialize = I('function() { this.setValue(""); }'),
          maxOptions = 5000
        ),
        server = TRUE
      )
      
      if (nrow(photos_avail()) > 5000) {
        output$truncated_list <- renderText({
          paste(
            'Only the first 5000 photos of',
            nrow(photos_avail()),
            'selected photos are displayed in the search below.',
            'Use the available filters to reduce your options',
            'to choose remaining photos by name.'
          )
        })
      } else {
        output$truncated_list <- renderText({""})
      }
    }, ignoreInit = FALSE)
    
    observeEvent(input$goto_photo, {
      if (input$goto_photo != "") {
        i_photo(which(photos_avail()[['filename']] == input$goto_photo))
      }
    }, ignoreInit = TRUE)
    
    observe({
      req(metadata_cache)
      if (
        metadata_cache$i_cache_end != 0 && (
          any(metadata_cache$cache$annotations[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotags[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotationverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$annotagverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$modelverifications[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$mediatags[,c('is_add', 'is_delete')] == 1) ||
          any(metadata_cache$cache$mediatagverifications[,c('is_add', 'is_delete')] == 1)
        )
      ) {
        updateActionButton(
          session = session, 
          inputId = 'save_metadata', 
          label = 'Save Labels',
          icon = icon('warning')
        )
        shinyjs::enable('save_metadata')
      } else {
        updateActionButton(
          session = session, 
          inputId = 'save_metadata', 
          label = 'Save Labels',
          icon = character(0)
        )
        shinyjs::disable('save_metadata')
      }
    }) |> bindEvent(
      metadata_cache$cache,
      save_metadata_now()
    )
    
    observeEvent(input$save_metadata, {
      save_metadata_now(TRUE)
    })
    
    
    # Navigate between available photos
    observeEvent(input$prev_photo, {
      if (active() && i_photo() > 1) {
        i_photo(i_photo() - 1)
      }
    })
    
    observeEvent(input$next_photo, {
      if (active() && i_photo() < nrow(photos_avail())) {
        i_photo(i_photo() + 1)
      }
    })
    
    observeEvent(input$reset_app, {
      # Send a message to trigger the page reload
      session$sendCustomMessage("reload", list())
    })
    
    return(reactiveValues(
      photo_name = reactive(photos_avail()$pk_mediaid[i_photo()]),
      last_photo_name = reactive(ifelse(
        test = isolate(i_photo()) == 1,
        NA,
        photos_avail()$pk_mediaid[i_photo()-1]
      )),
      bboxes = reactive(the_bboxes()),
      modelConf = reactive(input$modelConf),
      modelLessThan = reactive(input$modelLessThan),
      viewModelOutputs = reactive(input$viewModelOutputs),
      metadata_cache = reactive(metadata_cache),
      autosave_rate = reactive(input$autosave_rate)
    ))
  })
}
