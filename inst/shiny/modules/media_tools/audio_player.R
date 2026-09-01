audio_player_ui <- function(id, viewer_mode) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # Define "hot keys"
    tags$script(HTML(paste0(
      "$(function(){",
      "$(document).keyup(function(e) {",
      "if (e.which == 37) {", # left-arrow
      "$('#",
      ns('prev_file'),
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
      ns('next_file'),
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
              "viewer" = tags$h2('Audio Player'),
              "tagger" = tags$h2('Audio Tagger'),
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
          title = 'Recording Filters & Settings',
          fluidRow(
            column(
              6,
              wellPanel(
                tags$h3('Audio Filters'),
                selectizeInput(
                  ns('filterLocation'),
                  'Select a Location:',
                  choices = c('all'),
                  selected = 'all'
                ),
                dateRangeInput(
                  ns('filterDateRange'),
                  'Select Date Range (Defaults to all):',
                  start = '1900-01-01',
                  end = '2100-01-01',
                  min = '1900-01-01',
                  max = '2100-01-01',
                  startview = 'year'
                ),
                selectizeInput(
                  ns('filterTaxa'),
                  'Select Taxa:',
                  choices = c('all')
                ),
                checkboxInput(
                  ns('random_order'),
                  'Randomize recording order'
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
                },
                radioButtons(
                  ns('manualDetxFilter'),
                  label = 'Manual Detections',
                  choices = c('All' = 'all', 'Not yet saved' = 'unsaved', 'Already saved' = 'saved'),
                  selected = 'all'
                )
              ),
              {if (viewer_mode == "modelOutputs") {
                wellPanel(
                  shiny::tags$h3('Model Filters'),
                  selectInput(
                    ns('modelID'),
                    'Select Model:',
                    choices = 'all',
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
              wellPanel(
                tags$h3('Visit Filters'),
                shiny::tags$br(),
                shiny::tags$i('Select a location to show visits.'),
                reactable::reactableOutput(ns('filterVisitTable'))
              ),
              wellPanel(
                tags$h3('Jump to Recording'),
                span(textOutput(ns('truncated_list')), style="color:red"),
                tags$br(),
                selectInput(
                  ns('goto_audio'),
                  'Select Recording:',
                  choices = NULL
                )
              )
            ),
            column(
              6,
              wellPanel(
                h3('Audio/Spectrogram Settings'),
                textInput(
                  ns('audioPathURL'),
                  'Audio Directory Path/URL:',
                  value = isolate(
                    ifelse(
                      grepl("(\\/$)|(^$|)",AUDIO_PATH()),
                      AUDIO_PATH(),
                      paste(AUDIO_PATH(), '/', sep = "")
                    )
                  )
                ),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      ns('cache_size'),
                      'Cache size (# of recordings, max 10000)',
                      value = 2500,
                      min = 1,
                      max = 10000,
                      step = 1
                    )
                  ),
                  column(
                    6,
                    switch(
                      viewer_mode,
                      'viewer' = character(0),
                      numericInput(
                        ns('autosave_rate'),
                        'Auto-save Rate (# of recordings)',
                        value = 1,
                        min = 1,
                        step = 1
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      inputId = ns("specLength"),
                      label = 'Spectrogram Length (s):',
                      min = 1,
                      value = 20
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      ns('spec_wl'),
                      'Window Length',
                      value = 512,
                      min = 0
                    )
                  )
                ),
                fluidRow(
                  column(
                    6,
                    selectInput(
                      ns('spec_wn'),
                      'Window Name',
                      choices = c("hamming","bartlett","blackman","flattop","hanning","rectangle"),
                      selected = 'hanning'
                    )
                  ),
                  column(
                    6,
                    selectInput(
                      inputId = ns('palette'),
                      label = 'Spectrogram Color Palette:',
                      choices = c('temp.colors', 'reverse.gray.colors.1', 'reverse.gray.colors.2', 'reverse.heat.colors', 'reverse.terrain.colors', 'reverse.topo.colors', 'reverse.cm.colors'),
                      selected = 'reverse.gray.colors.2'
                    )
                  )
                ),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      ns('spec_ovlp'),
                      'Overlap',
                      value = 0,
                      min = 0,
                      max = 100
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      ns('zeroPadding'),
                      'Zero Padding',
                      value = 0,
                      min = 0
                    )
                  )
                ),
                uiOutput(ns('freqSpecRange')),
                fluidRow(
                  column(
                    6,
                    checkboxInput(
                      inputId = ns('showWave'),
                      label = 'Show Waveform',
                      value = FALSE
                    )
                  ),
                  column(
                    6,
                    checkboxInput(
                      inputId = ns('interpolate'),
                      label = 'Interpolate',
                      value = TRUE
                    )
                  )
                ),
                selectInput(
                  inputId = ns('wavePalette'),
                  label = 'Waveform Color:',
                  choices = list(Black = "#000000", Blue = "#006DDB", Red = "#A50021", Green = "#004949", Brown = "#662700", Orange = "#DB6D00", Pink = "#FF6DB6", Purple = "#490092"),
                  selected = "Black"
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
                h3('Audio Modifications'),
                checkboxInput(
                  inputId = ns('denoise'),
                  label = 'Remove noise',
                  value = FALSE
                ),
                uiOutput(ns('freqFilter')),
                sliderInput(
                  inputId = ns('aFilter'),
                  label = 'Amplitude Filter (%)',
                  min = 0,
                  max = 10,
                  value = 0
                ),
                actionButton(
                  inputId = ns('resetAudioFilters'),
                  label = 'Reset Audio Filters'
                )
              )
            )
          ),
          textOutput(ns('filters_applied')),
          actionButton(ns('apply_filters'), 'Apply Filters', class = "btn-warning"),
          textOutput(ns('num_audio'))
        )
      ),
      column(
        10,
        textOutput(ns('audio_meta')),
        #player + spectro ---------------------
        fluidRow(
          column(
            12,
            uiOutput(ns("player")),
            div(
              class = "wave-plot",
              id = ns("wave-plots"),
              plotOutput(
                ns('plot_wave'),
                width = "100%",
                height = "200px"
              ),
              plotOutput(
                ns('plotline_wave'),
                width = "100%",
                height = "200px",
                click = ns("spec_click")
              ), # the line
            ),
            div(
              class = "large-plot",
              id = ns("spectro-plots"),
              plotOutput(
                ns('plot_bg'),
                width = "100%",
                height = "300px"
              ), # the_spec
              plotOutput(
                ns('plotline'),
                width = "100%",
                height = "300px"
              ), # the line
              {if (viewer_mode == 'tagger') {
                plotOutput(
                  ns("plotx"),
                  width = "100%",
                  height = "300px",
                  brush = brushOpts(id = ns("spec_brush"), resetOnNew = TRUE),
                  dblclick = ns("spec_dbl"),
                  click = ns("spec_click"),
                  hover = hoverOpts(id = ns("spec_hover"), delay = 100, delayType = "debounce")
                )
              } else {
                plotOutput(
                  ns("plotx"),
                  width = "100%",
                  height = "300px",
                  click = ns("spec_click"),
                  hover = hoverOpts(id = ns("spec_hover"), delay = 100, delayType = "debounce")
                )
              }
              },
              uiOutput(ns("hover_info"))
            ),
            fluidRow(
              column(
                4,
                actionButton(inputId = ns("prev_spec"), label = "Previous Spec"),
                actionButton(inputId = ns("next_spec"), label = "Next Spec")
              ),
              tags$style(
                type="text/css",
                "#inline-label label{
          display: table-cell;
          text-align: center;
          vertical-align: middle;
        }
        #inline-label label.control-label {
          padding-right: 10px;
        }
        #inline-label .form-group{
          display: table-row;
        }"
              )
            ),
            br(),
            #prev/next recording --------------
            fluidRow(
              column(
                8,
                switch(
                  viewer_mode,
                  modelOutputs = tagList(
                    actionButton(inputId = ns("prev_file"), label = "Previous file"),
                    actionButton(inputId = ns("next_file"), label = "Next file"),
                    actionButton(ns('save_metadata'), 'Save Labels')
                  ),
                  verifier = tagList(
                    actionButton(inputId = ns("prev_file"), label = "Previous file"),
                    actionButton(inputId = ns("next_file"), label = "Next file"),
                    actionButton(ns('save_metadata'), 'Save Labels')
                  ),
                  tagger = tagList(
                    actionButton(inputId = ns("prev_file"), label = "Previous file"),
                    actionButton(inputId = ns("next_file"), label = "Next file"),
                    actionButton(ns('save_metadata'), 'Save Labels')
                  ),
                  tagList(
                    actionButton(inputId = ns("prev_file"), label = "Previous file"),
                    actionButton(inputId = ns("next_file"), label = "Next file"),
                    shinyjs::hidden(actionButton(ns('save_metadata'), 'Save Labels'))
                  )
                )
              )
            )
          )
        )
      )
    ),
    tags$style(paste0(
      "
        .large-plot {
            position: relative;
            height: 300px;
        }
        #", id, "-plot_bg {
            position: absolute;
        }
        #", id, "-plotline {
            position: absolute;
        }
        #", id, "-plotx {
            position: absolute;
        }
        .wave-plot {
            position: relative;
            height: 200px;
        }
        #", id, "-plot_wave {
            position: absolute;
        }
        #", id, "-plotline_wave {
            position: absolute;
        }
        "
    ))
  )
}

# Per-recording comment box. Rendered separately from audio_player_ui() so it
# can be placed lower on the page (e.g. below annotation_viewer_tables_ui's
# tag tables), while still binding to the same audio_player_server instance --
# pass it the same id used for the corresponding audio_player_ui() call.
audio_comment_box_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      8,
      textAreaInput(
        ns('recording_comments'),
        'Comments:',
        value = "",
        width = '100%',
        rows = 2,
        resize = 'vertical'
      ),
      actionButton(ns('save_comment'), 'Save Comment'),
      tags$br(), tags$br(),
      textOutput(ns('comment_save_status'))
    ),
    column(
      4,
      textInput(
        ns('manual_detx'),
        'Manual Detections (defaults to model count; overwrite as needed):',
        value = "",
        width = '100%'
      ),
      actionButton(ns('save_manual_detx'), 'Save Detections'),
      tags$br(), tags$br(),
      textOutput(ns('manual_detx_save_status'))
    )
  )
}

audio_player_server <- function(id, selectedUser = NA, active = reactive(TRUE), updateTags = reactive(NA), viewer_mode, annotations_cache = reactive(NA), verifications_cache = reactive(NA), selected_rows = reactive(NA), deleted_rows = reactive(NA)) {
  moduleServer(id, function(input, output, session) {
    # Temp directory for filtered audios
    td <- tempdir(check = TRUE)
    addResourcePath("audiodir", td)

    ns <- session$ns

    # Hard ceiling on how many recordings' worth of annotations/modeloutputs/
    # mediatags get pulled into memory in one batch. cache_size.txt or a
    # user-typed value can otherwise request an unbounded batch, which turns
    # into a huge SQL "IN (...)" list and has previously crashed the app with
    # an out-of-memory error. This clamp applies regardless of that setting.
    MAX_CACHE_SIZE <- 10000
    effective_cache_size <- reactive({
      req(input$cache_size)
      min(input$cache_size, MAX_CACHE_SIZE)
    })

    # Define reactive values
    i_audio <- reactiveVal(1) # Index of current audio displayed
    last_audio <- reactiveVal('') # Index of last audio displayed
    visitID <- reactiveVal() # Visit ID (if selected in filters)
    the_bboxes <- reactiveVal(data.frame(
      x_min = numeric(0),
      x_max = numeric(0),
      y_min = numeric(0),
      y_max = numeric(0)
    ))
    update_boxes <- reactiveVal(0) # Update bounding boxes upon annotation deletion
    point_cache <- reactiveVal() # Keep track of points/clicks for bounding boxes

    date_ranges <- reactiveVal(AMMonitor::qryMediaDateRange(con(), "audio"))

    if (file.exists(paste(ammPath, 'settings', 'cache_size.txt', sep = '/'))) {
      cache_size <- read.csv(
        paste(ammPath, 'settings', 'cache_size.txt', sep = '/'),
        header = F
      )[,]
      if (is.numeric(cache_size)) {
        updateNumericInput(
          session,
          'cache_size',
          'Cache size (# of recordings, max 10000)',
          value = min(cache_size, MAX_CACHE_SIZE),
          min = 1,
          max = 10000,
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
          'Auto-save Rate (# of recordings)',
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

    observe({
      req(deleted_rows())
      if (length(deleted_rows()$annotags) > 0) {
        for (i in seq_len(nrow(deleted_rows()$annotags))) {
          i_delete <- which(metadata_cache$cache$annotations$pk_annotationid == deleted_rows()$annotags$pk_annotationid[i])

          # Remove the annotags and annotations
          if (!is.na(deleted_rows()$annotags$pk_annotagid[i])) {
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

    #existing annotations ----------
    current_taxon_annotations <- reactiveVal({
      data.frame(
        fk_personid = character(0),
        fk_mediaid = numeric(0),
        fk_taxonid = character(0),
        x_min = numeric(0),
        x_max = numeric(0),
        y_min = numeric(0),
        y_max = numeric(0),
        selected_row = logical(0)
      )
    })

    # Set media path for audio
    observeEvent(input$audioPathURL, {
      AUDIO_PATH(input$audioPathURL)
    })

    observeEvent(AUDIO_PATH(), {
      updateTextInput(
        session = session,
        'audioPathURL',
        value = AUDIO_PATH()
      )
    })

    audio_on_startup <- reactiveVal(1) # For altering startup behavior of apply_filters

    # Filtered dataframe of available recordings
    audio_avail <- eventReactive(list(input$apply_filters, audio_on_startup), {
      output$filters_applied <- renderText("")
      audios <- switch(
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
          newOnly = FALSE,
          mediaType = "audio",
          limit = 2500 # can change if need be
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
          mediaType = "audio"
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
          mediaType = "audio"
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
          mediaType = "audio"
        )
      )

      if (nrow(audios) > 0 && input$manualDetxFilter != 'all') {
        manualdetx_status <- dbGetQuery(
          con(),
          paste0(
            'SELECT pk_mediaid, ManualDetx FROM media WHERE pk_mediaid IN (',
            paste(audios$pk_mediaid, collapse = ', '),
            ');'
          )
        )
        keep_ids <- switch(
          input$manualDetxFilter,
          unsaved = manualdetx_status$pk_mediaid[is.na(manualdetx_status$ManualDetx)],
          saved = manualdetx_status$pk_mediaid[!is.na(manualdetx_status$ManualDetx)]
        )
        audios <- audios[audios$pk_mediaid %in% keep_ids, ]
      }

      if (input$random_order) {
        audios <- audios[sample(1:nrow(audios)),]
      }
      i_audio(1)
      i_cache(1)
      audios
    })

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
      req(!any(is.na(metadata_cache$cache)))
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


    # Update the metadata cache
    observe({
      # First, save any unsaved tags (if needed)
      if (
        audio_on_startup() != 1 &&
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

      i_cache_start <- max(1, effective_cache_size()*(i_cache()-1))
      i_cache_end <- min(effective_cache_size()*i_cache(), nrow(audio_avail()))
      if (nrow(audio_avail()) == 0) {
        cache_media <- integer(0)
        cache_annotations <- DBI::dbGetQuery(con(), 'SELECT * FROM annotations WHERE fk_mediaID = -99;')
        cache_annotags <- dbGetQuery(con(), 'SELECT annotags.*, fk_librarylistid, item FROM annotags INNER JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid WHERE fk_annotationid = -99;')
        cache_annotags <- dbGetQuery(con(), 'SELECT annotags.*, fk_librarylistid, item FROM annotags INNER JOIN librarylistitems ON annotags.fk_librarylistitemid = librarylistitems.pk_librarylistitemid WHERE fk_annotationid = -99;')
        cache_modeloutputs <- dbGetQuery(con(), 'SELECT model_name, pk_modeloutputid, fk_librarylistitemid, fk_mediaid, fk_medialistitemid, x_min, x_max, y_min, y_max, modeloutputs.fk_taxonid, value_num FROM modeloutputs INNER JOIN models ON modeloutputs.fk_modelid = models.pk_modelid WHERE fk_mediaid = -99;')
        cache_mediatags <- dbGetQuery(con(), 'SELECT pk_mediatagid, fk_mediaid, fk_personid, fk_medialistid, item, value_num FROM mediatags INNER JOIN medialistitems ON mediatags.fk_medialistitemid = medialistitems.pk_medialistitemid WHERE pk_mediatagid = -99;')
      } else {
        cache_media <- audio_avail()$pk_mediaid[i_cache_start:i_cache_end]
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
              paste0(" AND annotations.fk_taxonid = '", input$filterTaxa, "'")
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
              paste0(" AND modeloutputs.fk_taxonid = '", input$filterTaxa, "'")
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
            'SELECT pk_mediaid, fk_locationid, comments, ManualDetx FROM media INNER JOIN visits ON media.fk_visitid = visits.pk_visitid WHERE pk_mediaid IN (',
            ifelse(
              length(cache_media) != 0,
              paste(cache_media, collapse = ', '),
              "NULL"
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
    }) |> bindEvent(audio_avail(), i_cache(), effective_cache_size())

    # Update cache counter when needed
    observe({
      if (i_audio() < metadata_cache$i_cache_start || i_audio() > metadata_cache$i_cache_end) {
        i_cache(ceiling(i_audio() / effective_cache_size()))
        save_metadata_now(TRUE)
      }
    })

    # Display audio metadata (above the audio)
    output$audio_meta <- renderText({
      if (nrow(audio_avail()) >= i_audio()) {
        paste0(
          'Recording ',
          i_audio(),
          ' of ',
          nrow(audio_avail()),
          '; FileName: ',
          audio_avail()$filename[i_audio()],
          '; Location: ',
          metadata_cache$cache$mediaMetaData$fk_locationid[
            metadata_cache$cache$mediaMetaData$pk_mediaid == audio_avail()$pk_mediaid[i_audio()]
          ],
          '; Date/Time: ',
          paste(audio_avail()[i_audio(), c('start_date', 'start_time')], collapse = ' ')
        )
      } else {
        ""
      }
    })

    # Load the current recording's comment into the comment box
    observe({
      req(nrow(audio_avail()) > 0)
      current_pk <- audio_avail()$pk_mediaid[i_audio()]
      current_comment <- metadata_cache$cache$mediaMetaData$comments[
        metadata_cache$cache$mediaMetaData$pk_mediaid == current_pk
      ]
      updateTextAreaInput(
        session = session,
        inputId = 'recording_comments',
        value = if (length(current_comment) && !is.na(current_comment[1])) current_comment[1] else ""
      )
      output$comment_save_status <- renderText("")
    })

    # Save the comment box back to media.comments for the current recording
    observeEvent(input$save_comment, {
      req(nrow(audio_avail()) > 0)
      current_pk <- audio_avail()$pk_mediaid[i_audio()]

      rs <- DBI::dbSendQuery(
        con(),
        "UPDATE media SET comments = $1 WHERE pk_mediaid = $2;"
      )
      DBI::dbBind(rs, list(input$recording_comments, current_pk))
      DBI::dbClearResult(rs)

      # Keep the cache in sync so paging away and back doesn't show a stale value
      metadata_cache$cache$mediaMetaData$comments[
        metadata_cache$cache$mediaMetaData$pk_mediaid == current_pk
      ] <- input$recording_comments

      output$comment_save_status <- renderText(paste0('Saved ', format(Sys.time(), '%H:%M:%S')))
    })

    # Live count of modeloutputs currently cached for this recording -- used
    # as the displayed default whenever media.ManualDetx hasn't been set.
    # Includes unverified detections and those verified valid; excludes only
    # detections some verifier has explicitly marked invalid (is_valid = 0).
    current_model_output_count <- reactive({
      req(nrow(audio_avail()) > 0)
      current_pk <- audio_avail()$pk_mediaid[i_audio()]

      these_ids <- metadata_cache$cache$modeloutputs$pk_modeloutputid[
        metadata_cache$cache$modeloutputs$fk_mediaid == current_pk &
          !is.na(metadata_cache$cache$modeloutputs$value_num) &
          metadata_cache$cache$modeloutputs$value_num >= 14
      ]
      invalid_ids <- unique(metadata_cache$cache$modelverifications$fk_modeloutputid[
        metadata_cache$cache$modelverifications$is_valid == 0
      ])
      model_count <- sum(!(these_ids %in% invalid_ids))

      # Manual annotations (from the Tagger) for this recording, any taxon,
      # any annotator, excluding any pending-deletion in the cache. Summed
      # with the model count per user request -- no de-duplication against
      # model detections.
      annotation_count <- sum(
        metadata_cache$cache$annotations$fk_mediaid == current_pk &
          metadata_cache$cache$annotations$is_delete == 0 &
          metadata_cache$cache$annotations$fk_taxonid != 'no-species',
        na.rm = TRUE
      )

      model_count + annotation_count
    })

    # Load the current recording's ManualDetx into the box, defaulting to the
    # live model-output count when no manual value has been saved
    observe({
      req(nrow(audio_avail()) > 0)
      current_pk <- audio_avail()$pk_mediaid[i_audio()]
      stored_value <- metadata_cache$cache$mediaMetaData$ManualDetx[
        metadata_cache$cache$mediaMetaData$pk_mediaid == current_pk
      ]
      display_value <- if (length(stored_value) && !is.na(stored_value[1])) {
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

    # Save the (possibly overwritten) detection count back to media.ManualDetx.
    # Clearing the box back to blank and saving reverts it to NULL, so the
    # recording goes back to showing the live model-output count next time.
    observeEvent(input$save_manual_detx, {
      req(nrow(audio_avail()) > 0)
      current_pk <- audio_avail()$pk_mediaid[i_audio()]

      new_value <- suppressWarnings(as.integer(input$manual_detx))
      if (input$manual_detx != "" && is.na(new_value)) {
        output$manual_detx_save_status <- renderText("Enter a whole number, or leave blank to use the model count.")
        return()
      }

      rs <- DBI::dbSendQuery(
        con(),
        "UPDATE media SET ManualDetx = $1 WHERE pk_mediaid = $2;"
      )
      DBI::dbBind(rs, list(new_value, current_pk))
      DBI::dbClearResult(rs)

      metadata_cache$cache$mediaMetaData$ManualDetx[
        metadata_cache$cache$mediaMetaData$pk_mediaid == current_pk
      ] <- new_value

      output$manual_detx_save_status <- renderText(paste0('Saved ', format(Sys.time(), '%H:%M:%S')))
    })

    # Display a warning if there are un-applied filters selected
    observe({
      if (audio_on_startup() != 1) {
        output$filters_applied <- renderText({
          "Warning: Un-applied filters selected. Press \"apply filters\" to apply changes."
        })
      }
      audio_on_startup(0)
    }) |> bindEvent(
      input$filterLocation,
      input$filterTaxa,
      input$filterDateRange,
      input$excludeAnnotated,
      input$excludeAnnoVerified,
      input$modelID,
      input$modelConf,
      input$modelLessThan,
      input$manualDetxFilter,
      visitID(),
      input$random_order,
      ignoreInit = TRUE
    )

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

    # Updated when the current playback time needs updating (after page switch)
    updateCurTime <- reactiveVal()

    #functionality for prev/next spec buttons ---------------
    observeEvent(input$prev_spec, {
      if (startTime() > input$specLength) {
        startTime(startTime() - input$specLength)
      } else {
        startTime(0)
      }

      updateCurTime(startTime())
    })

    observeEvent(input$next_spec, {
      if ((startTime() + input$specLength) < (duration() - input$specLength)) {
        startTime(startTime() + input$specLength)
      } else {
        startTime(max(duration() - input$specLength, 0))
      }

      updateCurTime(startTime())
    })

    # Update start time and clear old .wav files when switching to a new audio file
    observe({
      audio_avail()$pk_mediaid[i_audio()]
      updateCurTime(0)
      startTime(0)
      for (file_name in list.files(td)) {
        if (!is.null(audio_path())) {
          if (grepl("\\.wav$", file_name) && file_name != basename(audio_path())) {
            unlink(paste0(td, "/", file_name))
          }
        } else if (grepl("\\.wav$", file_name)) {
          unlink(paste0(td, "/", file_name))
        }

      }
    }) |> bindEvent(audio_avail()$pk_mediaid[i_audio()])


    # Update tags (and clear old ones) when switching to a new audio file
    # Remove new boxes when recording changes
    observe({
      req(nrow(audio_avail()) > 0)
      updateTags()
      update_boxes()
      # Remove all pending bounding boxes
      # Subset of annotations with matching mediaID
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
          metadata_cache$cache$modeloutputs$fk_mediaid == audio_avail()$pk_mediaid[i_audio()] & scores_conditions_mask,
        ]

      } else {
        the_bboxes(the_bboxes()[0,])
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
          metadata_cache$cache$annotations$fk_mediaid == audio_avail()$pk_mediaid[i_audio()] & user_conditions_annotations_mask,
        ]

        the_mediatags <- metadata_cache$cache$mediatags[
          metadata_cache$cache$mediatags$fk_mediaid == audio_avail()$pk_mediaid[i_audio()] & user_conditions_mediatags_mask,
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
        metadata_cache$cache$modeloutputs$fk_mediaid == audio_avail()$pk_mediaid[i_audio()],
      ]

      # Reset the bounding boxes for the existing annotations
      current_taxon_annotations(
        data.frame(
          fk_personid = character(0),
          fk_mediaid = numeric(0),
          fk_taxonid = character(0),
          x_min = numeric(0),
          x_max = numeric(0),
          y_min = numeric(0),
          y_max = numeric(0),
          selected_row = logical(0)
        )
      )

      current_taxon_annotations(rbind(current_taxon_annotations(), the_annotations))

      if (viewer_mode == 'viewer') {
        if (input$viewModelOutputs) {

          if (nrow(the_modelOutputs) != 0) {
            if (length(setdiff(names(the_annotations), names(the_modelOutputs))) != 0) {
              the_modelOutputs[setdiff(names(the_annotations), names(the_modelOutputs))] <- NA
            }

            if (nrow(the_annotations) != 0) {
              the_annotations[setdiff(names(the_modelOutputs), names(the_annotations))] <- NA
            }
            current_taxon_annotations(
              rbind(the_annotations, the_modelOutputs)
            )
          }
        }
      }

      if (nrow(current_taxon_annotations()) != 0) {
        current_taxon_annotations(cbind(current_taxon_annotations(), selected_row = FALSE))
      }

    }) |> bindEvent(audio_avail()$pk_mediaid[i_audio()], deleted_rows(), updateTags(), update_boxes(), input$viewModelOutputs)

    #audio player-----------------
    output$player <- renderUI({
      updatePlayer()
      if (nrow(audio_avail())) {
        tryCatch(
          {
            audio_src <- ifelse(
              grepl("^http", audio_path()),
              audio_path(),
              paste0("data:audio/wav; base64,", base64enc::base64encode(audio_path()))
            )

            audio_src <- ifelse(
              filters_used(),
              paste0("data:audio/wav; base64,", base64enc::base64encode(paste0(td, "/", basename(audio_path())))),
              audio_src
            )

            HTML(paste0(
              '<audio id="', ns("audio_player"), '" controls>
          <source src = "', audio_src, '" type="audio/wav"></source>
          Your browser does not support HTML5 audio.
          </audio>

          <script>
          myAudio = document.getElementById("', ns("audio_player"), '");
          function updateTime() {
            Shiny.onInputChange("', ns("curTime"), '", myAudio.currentTime);
            if (myAudio.currentTime < ', round(isolate(startTime())), ' || myAudio.currentTime >= ', round(isolate(startTime()+input$specLength)),') {
              myAudio.pause();
            }
          }

          myAudio.ontimeupdate = function() {updateTime();};
          </script>

          <script>
          // Update the current time, when triggered
          myAudio.currentTime = ', isolate(updateCurTime()),
              '</script>'
            ))
          },
          error = function(e) {
            tags$img(
              src = 'NoAudioAvailable.jpg',
              height = '300px'
            )
          }
        )
      } else {
        tags$img(
          src = 'NoAudioAvailable.jpg',
          height = '300px'
        )
      }
    })

    observe({
      shinyjs::runjs(paste0(

        'if (typeof myAudio !== "undefined" && myAudio) {
          function updateTime() {
            Shiny.onInputChange("', ns("curTime"), '", myAudio.currentTime);
            if (myAudio.currentTime < ', round(startTime()), ' || myAudio.currentTime >= ', round(startTime()+input$specLength),') {
              myAudio.pause();
            }
          }

          myAudio.ontimeupdate = function() {updateTime();};

          // Update the current time, when triggered
          myAudio.currentTime = ', updateCurTime(), ';
        }'
      ))
    }) |> bindEvent(updateCurTime(), startTime(), input$specLength)

    audio_path <- reactive({
      if (nrow(audio_avail()) && !is.na(audio_avail()$filename[i_audio()])) {
        if (grepl("google.com", audio_avail()$filepath[i_audio()])) {
          temp_path <- paste(tempdir(), audio_avail()$filename[i_audio()], sep = '/')
          googledrive::local_drive_quiet()
          googledrive::drive_download(
            file = audio_avail()$filepath[i_audio()],
            path = temp_path,
            overwrite = TRUE
          )
          temp_path
        } else if (! audio_avail()$filepath[i_audio()] %in% c(NA, "")) {
          audio_avail()$filepath[i_audio()]
        } else {
          paste0(
            input$audioPathURL,
            ifelse(endsWith(input$audioPathURL, '/') || input$audioPathURL == "", "", "/"),
            audio_avail()$filename[i_audio()]
          )
        }
      } else {
        NULL
      }
    })

    fullAudio <- reactive({
      req(audio_path())
      if (grepl("^www.|^http:|^https:", audio_path())) {
        temp.file <- tempfile()
        dl_ok <- tryCatch({
          utils::download.file(
            url = utils::URLencode(audio_path(), reserved = FALSE),
            destfile = temp.file,
            quiet = TRUE,
            mode = "wb",
            cacheOK = TRUE
          )
          file.exists(temp.file)
        }, error = function(e) FALSE, warning = function(w) FALSE)
        # A failed/slow download shouldn't crash every output depending on
        # fullAudio() (spectrogram, waveform, frequency filters) at once --
        # req(FALSE) quietly suspends this reactive instead of throwing.
        req(dl_ok)
        tuneR::readWave(temp.file)
      } else {
        tuneR::readWave(audio_path())
      }
    })

    updatePlayer <- reactiveVal(1)
    filters_used <- reactiveVal(FALSE) # Keep track of whether audio is filtered

    filteredAudio <- reactive({

      # Check if any filters were used
      ffilter <- !is.null(input$audioFreqRange) && (input$audioFreqRange[1] != 0 || input$audioFreqRange[2] != fullAudio()@samp.rate/2000)
      denoise <- !is.null(input$denoise) && input$denoise
      aFilter <- !is.null(input$aFilter) && input$aFilter != 0

      isolate({
        if (denoise == TRUE && duration() > (2000000/fullAudio()@samp.rate)) {
          showModal(
            modalDialog(
              "This recording is too long, so noise will not be removed.",
              title = "Noise Removal Error",
              easyClose = TRUE
            )
          )
          denoise <- FALSE
        }
      })

      # Initialize filtered audio
      filtered <- fullAudio()
      filters_used(FALSE)

      # Run the filters
      if (denoise == TRUE) {
        filtered <- seewave::rmnoise(wave = filtered, output = "Wave")
        filters_used(TRUE)
      }
      if (aFilter == TRUE) {
        filtered <- seewave::afilter(
          wave = filtered,
          threshold = input$aFilter,
          output = "Wave"
        )
        filters_used(TRUE)
      }
      if (ffilter == TRUE) {
        filtered <- seewave::ffilter(
          filtered,
          from = input$audioFreqRange[1] * 1000,
          to = input$audioFreqRange[2] * 1000,
          output = "Wave"
        )
        filters_used(TRUE)
      }

      isolate(updatePlayer(updatePlayer()+1))

      # Save wave to tempdir as basename(audio_path)
      isolate({
        if (filters_used()) {
          unlink(paste0(td, "/", basename(audio_path())), force = TRUE)
          seewave::savewav(
            filtered,
            filename = paste0(td, "/", basename(audio_path()))
          )
        }
      })

      filtered
    })

    duration <- reactive(seewave::duration(fullAudio()))
    startTime <- reactiveVal(0)

    w <- reactive({
      endTime <- min(
        ((startTime()*filteredAudio()@samp.rate + input$specLength*filteredAudio()@samp.rate)),
        length(filteredAudio())
      )
      filteredAudio()[(startTime()*filteredAudio()@samp.rate):endTime]
    })

    s <- reactive({
      req(nrow(audio_avail()))
      seewave::spectro(
        w(),
        wl = input$spec_wl,
        wn = input$spec_wn,
        zp = input$zeroPadding,
        ovlp = input$spec_ovlp,
        fastdisp = TRUE,
        plot = FALSE
      )
    })

    observeEvent(input$specLength, {
      req(input$curTime)
      if ((startTime() + input$specLength) < input$curTime) {
        startTime(max(0, input$curTime - input$specLength/2))
      }
      if (startTime() + input$specLength > duration()) {
        startTime(max(0,duration() - input$specLength))
      }
    })

    observe({
      shinyjs::toggle(id = 'spectro-plots', condition = nrow(audio_avail()))
      shinyjs::toggle(id = 'specFreqRange', condition = nrow(audio_avail()))
      shinyjs::toggle(id = 'audioFreqRange', condition = nrow(audio_avail()))
    })

    #waveform -------------------
    output$plot_wave <- renderPlot({
      req(s())
      if ('left' %in% slotNames(class(w()))) {
        w_df <- data.frame(amp = w()@left/max(abs(w()@left)))
      } else {
        w_df <- data.frame(amp = w()/max(abs(w())))
      }

      w_df$time <- (1:nrow(w_df))/w()@samp.rate

      ggplot(data = w_df, aes(x = time, y = amp)) +
        geom_line(color = input$wavePalette, na.rm = TRUE) +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_blank(),
          panel.grid.major = element_line(color = "grey68"),
          panel.grid.minor.x = element_line(color = "grey68"),
          panel.grid.minor.y = element_blank(),
          legend.position = "none",
          legend.background = element_blank(),
          legend.box.background = element_blank()
        ) +
        scale_x_continuous(
          name = "Time (s)",
          limits = c(0, max(s()$time)),
          breaks = seq(0, max(s()$time), max(0.5, floor(max(s()$time)/10))),
          labels = seq(
            from = startTime(),
            to = startTime() + max(s()$time),
            by = max(0.5, floor(max(s()$time)/10))
          ),
          expand = c(0,0)
        ) +
        #maybe replace limits with input values? numeric inputs for y axis limits
        scale_y_continuous(name = " Relative Amplitude", expand = c(0,0), limits = c(-1, 1))
    }, bg = "transparent")

    output$plotline_wave <- renderPlot({
      req(s())
      ggplot() +
        geom_vline(xintercept=(input$curTime - startTime()), na.rm = TRUE) +
        theme(
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.position = "none"
        ) +
        scale_x_continuous(
          name = "Time (s)",
          limits = c(0, max(s()$time)),
          breaks = seq(0, max(s()$time), max(0.5, floor(max(s()$time)/10))),
          labels = seq(
            from = startTime(),
            to = startTime() + max(s()$time),
            by = max(0.5, floor(max(s()$time)/10))
          ),
          expand = c(0,0)
        ) +
        #maybe replace limits with input values? numeric inputs for y axis limits
        scale_y_continuous(name = " Relative Amplitude", expand = c(0,0), limits = c(-1, 1))
    }, bg="transparent")

    observeEvent(input$showWave, {
      shinyjs::toggle(id = "wave-plots", condition = input$showWave && nrow(audio_avail()))
      shinyjs::toggle(id = "wavePalette", condition = input$showWave && nrow(audio_avail()))
    })

    #spectrogram-----------------------
    output$plot_bg <- renderPlot({
      req(s())
      s_amp <- s()$amp
      rownames(s_amp) <- s()$freq
      colnames(s_amp) <- s()$time
      s_df <- reshape2::melt(s_amp)
      names(s_df) <- c("freq", "time", "amp")

      ggplot(data = s_df, aes(x = time, y = freq, fill = amp)) +
        geom_raster(interpolate = input$interpolate, na.rm = TRUE) +
        scale_fill_gradientn(
          colours = eval(parse(text = paste0('seewave::', input$palette, '(255)')))
        ) +
        theme(
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          legend.background = element_blank(),
          legend.box.background = element_blank()
        ) +
        scale_x_continuous(
          name = "Time (s)",
          limits = c(0, max(s()$time)),
          breaks = seq(0, max(s()$time), max(0.5, floor(max(s()$time)/10))),
          labels = seq(
            from = startTime(),
            to = startTime() + max(s()$time),
            by = max(0.5, floor(max(s()$time)/10))
          ),
          expand = c(0,0)
        ) +
        #maybe replace limits with input values? numeric inputs for y axis limits
        scale_y_continuous(name = "Frequency (kHz)", limits = c(max(0, input$specFreqRange[1]), min(input$specFreqRange[2], w()@samp.rate/2e3)), expand = c(0,0))
    }, bg = "transparent")

    output$plotline <- renderPlot({
      req(s())
      ggplot() +
        geom_vline(xintercept=(input$curTime - startTime()), na.rm = TRUE) +
        theme(
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.position = "none"
        ) +
        scale_x_continuous(
          name = "Time (s)",
          limits = c(0, max(s()$time)),
          breaks = seq(0, max(s()$time), max(0.5, floor(max(s()$time)/10))),
          labels = seq(
            from = startTime(),
            to = startTime() + max(s()$time),
            by = max(0.5, floor(max(s()$time)/10))
          ),
          expand = c(0,0)
        ) +
        scale_y_continuous(name = "Frequency (kHz)", limits = c(max(0, input$specFreqRange[1]), min(input$specFreqRange[2], w()@samp.rate/2e3)), expand = c(0,0))
    }, bg="transparent")

    #boxes
    output$plotx <- renderPlot({
      req(s())

      # Set spectrogram frequency (y-) bounds
      spec_range <- c(
        max(0, input$specFreqRange[1]),
        min(input$specFreqRange[2], w()@samp.rate/2e3)
      )

      # rectangles to plot
      rects <- the_bboxes()[which(
        the_bboxes()$x_min >= startTime() &
          the_bboxes()$x_max <= (startTime() + input$specLength)
      ), ]

      rects2 <- current_taxon_annotations()[which(
        current_taxon_annotations()$fk_mediaid == audio_avail()$pk_mediaid[i_audio()] &
          current_taxon_annotations()$x_min >= startTime() &
          current_taxon_annotations()$x_max <= (startTime() + input$specLength) &
          (current_taxon_annotations()$y_min >= spec_range[1]) %in% c(1, NA) &
          (current_taxon_annotations()$y_max <= spec_range[2]) %in% c(1, NA) &
          current_taxon_annotations()$is_delete == 0
      ), c("x_min", "y_min", "x_max", "y_max")]

      # Set frequency (y-) bounds for boxes to max when unspecified
      rects2$y_min <- ifelse(is.na(rects2$y_min), spec_range[1], rects2$y_min)
      rects2$y_max <- ifelse(is.na(rects2$y_max), spec_range[2], rects2$y_max)

      if (nrow(rects)) {
        rects <- rects - t(matrix(rep(c(startTime(),0,startTime(),0), nrow(rects)), nrow = 4))
      }

      if (nrow(rects2)) {
        rects2 <- rects2 - t(matrix(rep(c(startTime(),0,startTime(),0), nrow(rects2)), nrow = 4))
      }

      #cbind taxon ids to saved rects for labels
      rects2 <- cbind(rects2, current_taxon_annotations()[which(
        current_taxon_annotations()$fk_mediaid == audio_avail()$pk_mediaid[i_audio()] &
          current_taxon_annotations()$x_min >= startTime() &
          current_taxon_annotations()$x_max <= (startTime() + input$specLength) &
          (current_taxon_annotations()$y_min >= spec_range[1]) %in% c(1, NA) &
          (current_taxon_annotations()$y_max <= spec_range[2]) %in% c(1, NA) &
          current_taxon_annotations()$is_delete == 0
      ), c("fk_taxonid", "selected_row")])

      ggplot() +
        geom_rect(
          data = rects2,
          aes(xmin = x_min, ymin = y_min, xmax = x_max, ymax = y_max, linewidth = factor(selected_row)),
          fill = "transparent",
          color = "green"
        ) +
        geom_label(
          data = rects2,
          aes(x = x_min, y = y_max, label = fk_taxonid, fill = fk_taxonid),
          colour = "black",
          hjust = "left",
          vjust = "top"
        ) +
        geom_rect(
          data = rects,
          aes(xmin = x_min, ymin = y_min, xmax = x_max, ymax = y_max),
          fill = "transparent",
          color = "red"
        ) +
        geom_label(
          data = rects,
          aes(x = x_min, y = y_max),
          label = "Not Saved",
          fill = "red",
          colour = "white",
          hjust = "left"
        ) +
        theme(
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.position = "none"
        ) +
        scale_x_continuous(
          name = "Time (s)",
          limits = c(0, max(s()$time)),
          breaks = seq(0, max(s()$time), max(0.5, floor(max(s()$time)/10))),
          labels = seq(
            from = startTime(),
            to = startTime() + max(s()$time),
            by = max(0.5, floor(max(s()$time)/10))
          ),
          expand = c(0,0)
        ) +
        scale_y_continuous(name = "Frequency (kHz)", limits = spec_range, expand = c(0,0)) +
        scale_linewidth_manual(
          values = {if(nrow(rects2)) {c("FALSE" = 0.5, "TRUE" = 1.5)} else {NA}}
        )
    }, bg="transparent")

    #input for time ? ----------------
    output$timeSelect <- renderUI({
      numericInput(
        inputId = ns("progress"),
        label = "Playback Time (s):",
        min = 0,
        max = duration(),
        value = 0
      )
    })

    #input for max freq -------------------
    #NOTE: currently refreshes every time s() re-renders (window size change, next page, etc)
    output$freqFilter <- renderUI({
      req(fullAudio())
      sliderInput(
        inputId = ns("audioFreqRange"),
        label = "Audio Frequency Range (kHz):",
        min = 0,
        max = fullAudio()@samp.rate/2000,
        step = 0.01,
        value = c(0, fullAudio()@samp.rate/2000)
      )
    })

    output$freqSpecRange <- renderUI({
      req(fullAudio())
      sliderInput(
        inputId = ns("specFreqRange"),
        label = "Spectrogram Frequency Range (kHz):",
        min = 0,
        max = fullAudio()@samp.rate/2000,
        step = 0.01,
        value = c(0, min(8, fullAudio()@samp.rate/2000))
      )
    })

    observeEvent(input$resetAudioFilters, {
      updateSliderInput(
        session,
        'audioFreqRange',
        value = c(0, fullAudio()@samp.rate/2000)
      )
      updateCheckboxInput(
        session,
        'denoise',
        value = FALSE
      )
      updateSliderInput(
        session,
        'aFilter',
        value = 0
      )
    })

    # Change page automatically based on current playback time
    # NOTE: This may make the app laggy, check for performance issues
    curTimeReact <- reactive({input$curTime})
    pausedTime <- debounce(curTimeReact, 200)
    observe({
      pausedTime()

      if ((input$curTime < startTime()) || (input$curTime >= (startTime()+input$specLength))) {
        if (input$curTime >= duration()) {
          startTime(max(0, duration() - input$specLength))
          updateCurTime(startTime())
        } else {
          startTime(floor(input$curTime))
          updateCurTime(startTime())
        }

        if (duration() - input$curTime < input$specLength) {
          startTime(duration() - input$specLength)
        }
      }

    }) |> bindEvent(pausedTime())

    # When an annotation row is selected, jump to selection
    observe({
      req(selected_rows())
      if (nrow(selected_rows()) != 0 && !all(is.na(selected_rows()))) {

        # Vector of true/false for boxes. %in% (rather than ==) is used
        # because selected_rows() can now hold more than one row when
        # multi-select is enabled (e.g. modelOutputs-audio) -- == would try
        # to recycle the two sides against each other instead of checking
        # set membership, and blows up when the lengths don't line up.
        modelOutputID_match <- current_taxon_annotations()$pk_modeloutputid %in% selected_rows()$pk_modeloutputid
        annotationID_match <- current_taxon_annotations()$pk_annotationid %in% selected_rows()$pk_annotationid
        mediatagID_match <- current_taxon_annotations()$pk_mediatagid %in% selected_rows()$pk_mediatagid

        if (length(annotationID_match) == 0) {
          annotationID_match <- rep(FALSE, times = max(length(modelOutputID_match), length(mediatagID_match)))
        }

        if (length(modelOutputID_match) == 0) {
          modelOutputID_match <- rep(FALSE, times = max(length(annotationID_match), length(mediatagID_match)))
        }

        if (length(mediatagID_match) == 0) {
          mediatagID_match <- rep(FALSE, times = max(length(annotationID_match), length(modelOutputID_match)))
        }

        selected_row_id <- annotationID_match | modelOutputID_match | mediatagID_match

        # Selected row(s) -- can be more than one when multi-select is enabled
        selected_box <- current_taxon_annotations()[which(selected_row_id),]
        # Use just the first selected row to decide whether/where to jump the view
        selected_box_first <- selected_box[1,]

        if (nrow(selected_box) > 0 && !is.na(selected_box_first$x_min) && !is.na(selected_box_first$x_max)) {
          # Cbind true/false to dataframe of boxes
          current_taxon_annotations({
            cta <- current_taxon_annotations()
            cta$selected_row <- ifelse(is.na(selected_row_id), FALSE, selected_row_id)
            cta
          })

          # Jump to box if not already visible
          if (xor(selected_box_first$x_min < startTime(), selected_box_first$x_max > (startTime() + input$specLength))) {
            startTime(max(0, floor((selected_box_first$x_min + selected_box_first$x_max)/2 - input$specLength/2)))
          } else if (selected_box_first$x_min < startTime() && selected_box_first$x_max > (startTime() + input$specLength)) {
            startTime(max(0, floor(selected_box_first$x_min)))
          }
          updateCurTime(startTime())
        } else {
          current_taxon_annotations({
            cta <- current_taxon_annotations()
            cta$selected_row <- rep(FALSE, times = nrow(cta))
            cta
          })
        }
      } else {
        current_taxon_annotations({
          cta <- current_taxon_annotations()
          cta$selected_row <- rep(FALSE, times = nrow(cta))
          cta
        })
      }
    }) |> bindEvent(selected_rows(), audio_avail()$pk_mediaid[i_audio()])

    #bb coords
    output$coords <- renderText({
      paste0(
        "x_min = ", round(input$spec_brush$xmin * 1, 2),
        " x_max = ", round(input$spec_brush$xmax * 1, 2),
        " y_min = ", round(input$spec_brush$ymin * 1, 2),
        " y_max = ", round(input$spec_brush$ymax * 1, 2)
      )
    })

    #observe new boxes
    observeEvent(input$spec_brush, {
      # Only create the bbox if it is at least 0.5 units in area
      if (
        (input$spec_brush$xmax - input$spec_brush$xmin) *
        (input$spec_brush$ymax - input$spec_brush$ymin) >= 0.5
      ) {
        new_dat <- data.frame(
          x_min = input$spec_brush$xmin + startTime(),
          y_min = input$spec_brush$ymin,
          x_max = input$spec_brush$xmax + startTime(),
          y_max = input$spec_brush$ymax
        )

        if (nrow(the_bboxes()) > 0) {
          if (all(round(new_dat[1,], 2) == round(the_bboxes()[nrow(the_bboxes()),], 2)) == FALSE) {
            the_bboxes(rbind(the_bboxes(), new_dat))
          }
        } else {
          the_bboxes(rbind(the_bboxes(), new_dat))
        }
      }
    })

    observeEvent(input$spec_dbl, {

      valid_rows <- !(
        input$spec_dbl$x + startTime() > the_bboxes()$x_min &
          input$spec_dbl$x + startTime() < the_bboxes()$x_max &
          input$spec_dbl$y > the_bboxes()$y_min &
          input$spec_dbl$y < the_bboxes()$y_max
      )

      the_bboxes(the_bboxes()[valid_rows,])
    })

    # Update time by clicking on spectrogram
    observeEvent(input$spec_click, {
      updateCurTime(input$spec_click$x + startTime())
    })

    # Tooltip
    output$hover_info <- renderUI({
      if (is.null(input$spec_hover)) {
        return(NULL)
      }

      hover <- input$spec_hover

      hover_time <- hover$x + isolate(startTime())
      hover_freq <- hover$y

      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y

      style <- paste0("position:absolute; z-index:100;
                      background-color: rgba(30, 30, 30, 0.85); color: rgb(255, 255, 255);
                      padding-left: 3px; padding-right: 3px;
                      padding-top: 2px; padding-bottom: 2px;",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")


      wellPanel(
        style = style,
        paste0(round(hover_time, 2), ", ", round(hover_freq, 2))
      )

    })

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # ALL THINGS FILTERS --------------
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

    # Audio filters --------------

    temp_locs <- AMMonitor::qryMediaLocations(con(), "audio")

    updateSelectizeInput(
      session,
      'filterLocation',
      choices = c('all', temp_locs),
      selected = 'all'
    )

    # Visit table filter ----------------

    visitTable <- reactive({
      AMMonitor::qryVisitTable(con(), "audio", input$filterLocation, selectedUser())
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
    })

    # Date range filter ------------------------
    # Reset date ranges when you select a new location
    if (isolate(nrow(audio_avail())) != 0) {
      updateDateRangeInput(
        session,
        'filterDateRange',
        start = date_ranges()$startdate,
        end = date_ranges()$enddate
      )
    }

    observeEvent(input$filterLocation, priority = 9998, {
      if (!is.null(input$filterVisitTable__reactable__selected)) {
        visitID(NULL)
      }
      if (isolate(nrow(audio_avail())) != 0) {
        updateDateRangeInput(
          session,
          'filterDateRange',
          start = date_ranges()$startdate,
          end = date_ranges()$enddate
        )
      }
    }, ignoreInit = TRUE)

    # Taxon Filters ---------------------------

    updateSelectizeInput(
      session,
      'filterTaxa',
      choices = c('all', sort(
        taxon_names$pk_taxonid,
      )),
      selected = 'all'
    )

    # Show the number of audios found with the given filters
    output$num_audio <- renderText({paste(nrow(audio_avail()), 'recordings found')})

    # Jump-to recording options and actions
    observeEvent(audio_avail(), priority = 9999, {
      updateSelectizeInput(
        session = session,
        inputId = 'goto_audio',
        choices = audio_avail()[['filename']],
        options = list(
          placeholder = 'Select Recording ID',
          maxOptions = 5000
        ),
        server = TRUE
      )

      if (nrow(audio_avail()) > 5000) {
        output$truncated_list <- renderText({
          paste(
            'Only the first 5000 recordings of',
            nrow(audio_avail()),
            'selected recordings are displayed in the search below.',
            'Use the available filters to reduce your options',
            'to choose remaining recordings by name.'
          )
        })
      } else {
        output$truncated_list <- renderText({""})
      }
    }, ignoreInit = FALSE)

    observeEvent(input$goto_audio, {
      req(nrow(audio_avail()) > 0)
      if (input$goto_audio != "") {
        i_audio(which(audio_avail()[['filename']] == input$goto_audio))
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

    # Navigate between availabile recordings
    observeEvent(input$prev_file, {
      if (active() && i_audio() > 1) {
        i_audio(i_audio() - 1)
      }
    })

    observeEvent(input$next_file, {
      if (active() && i_audio() < nrow(audio_avail())) {
        i_audio(i_audio() + 1)
      }
    })

    observeEvent(input$reset_app, {
      # Send a message to trigger the page reload
      session$sendCustomMessage("reload", list())
    })

    return(reactiveValues(
      audio_name = reactive(audio_avail()$pk_mediaid[i_audio()]),
      last_audio_name = reactive(ifelse(
        test = isolate(i_audio()) == 1,
        NA,
        audio_avail()$pk_mediaid[i_audio()-1]
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
