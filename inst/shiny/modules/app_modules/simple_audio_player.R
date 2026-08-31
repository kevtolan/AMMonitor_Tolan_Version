#!! ModName = simple_audio_player
#!! ModDisplayName = Audio Player (Basic)
#!! ModDescription = Simplified version of the audio player
#!! ModCitation = Tang, Caroline.  (2023). simple_audio_player. [Source code].
#!! ModNotes = Small update by Kaitlin Huber (2025).
#!! ModActive = 1
#!! FunctionArg = recordingID !! audio recording ID !! character
#!! FunctionArg = selectedRow !! selected annotation row ID !! numeric
#!! FunctionArg = showOnlySelected !! show only the selected row !! logical
#!! FunctionReturn = audioURL !! file directory/URL !! character
#!! FunctionReturn = spec_wl !! spectrogram window length !! numeric
#!! FunctionReturn = spec_wn !! spectrogram window name !! character
#!! FunctionReturn = spec_ovlp !! spectrogram window overlap !! numeric
#!! FunctionReturn = amp_filter !! amplitude filter !! numeric
#!! Package = shinydashboard !! 0.7.2 !!
#!! Package = ggplot2 !! 3.4.1 !!
#!! Package = seewave !! 2.2.0 !!
#!! Package = DBI !! 1.1.3 !!
#!! Package = base64enc !! 0.1.3 !!
#!! Package = tuneR !! 1.4.7 !!


# the ui function
simple_audio_player_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Spectrogram settings
      shinydashboard::box(
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        title = "Spectrogram and Audio settings",
        column(
          6,
          wellPanel(
            h3('Audio Settings'),
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
        ),
        column(
          6,
          wellPanel(
            h3('Spectrogram Settings'),
            textInput(
              ns('audioPathURL'),
              'Audio Directory Path/URL:'
            ),
            numericInput(
              inputId = ns("specLength"),
              label = 'Spectrogram Length (s):',
              min = 1,
              value = 10
            ),
            uiOutput(ns('freqSpecRange')),
            numericInput(
              ns('spec_wl'),
              'Window Length',
              value = 512,
              min = 0
            ),
            selectInput(
              ns('spec_wn'),
              'Window Name',
              choices = c("hamming","bartlett","blackman","flattop","hanning","rectangle"),
              selected = 'hanning'
            ),
            numericInput(
              ns('spec_ovlp'),
              'Overlap',
              value = 0,
              min = 0,
              max = 100
            ),
            checkboxInput(
              inputId = ns('binary_plot'),
              label = "Color amplitude as binary",
              value = FALSE
            ),
            shinyjs::hidden(uiOutput(ns('bin_plot_settings'))),
            selectInput(
              inputId = ns('palette'),
              label = 'Spectrogram Color Palette:',
              choices = c('temp.colors', 'reverse.gray.colors.1', 'reverse.gray.colors.2', 'reverse.heat.colors', 'reverse.terrain.colors', 'reverse.topo.colors', 'reverse.cm.colors'),
              selected = 'reverse.gray.colors.2'
            )
          )
        )
      )
    ),
    fluidRow(
      textOutput(ns('audio_meta'))
    ),
    # Audio player
    fluidRow(
      uiOutput(ns('player')),
    ),
    # Spectrogram
    fluidRow(
      column(
        10,
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
          plotOutput(
            ns("plotx"),
            width = "100%",
            height = "300px",
            click = ns("spec_click"),
            hover = hoverOpts(id = ns("spec_hover"), delay = 100, delayType = "debounce")
          ),
          uiOutput(ns('hover_info'))
        )
      )
    ),
    fluidRow(
      column(
        4,
        # offset = 2,
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
        }"
    ))
  )
}


# the server function
simple_audio_player_server <- function(id, recordingID = reactive(NULL), selectedRow = reactive(NULL), showOnlySelected = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Temp directory for filtered audios
    td <- tempdir(check = TRUE)
    addResourcePath("audiodir", td)
    
    # Annotations for recording
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
    
    recording_metadata <- reactive({
      req(recordingID())
      dbGetQuery(
        con(),
        paste0("SELECT filename, filepath FROM media WHERE pk_mediaid = ", recordingID(), ";")
      )
    })
    
    # Fill the URL automatically if possible
    observe({
      req(recordingID())
      if (
        any(AMMonitor::qryCheckMediaFilePaths(con(), mediaType = "audio"))
      ) {
        
        # First, check for a file path saved in the "settings"
        if (file.exists(paste0(ammPath, '/settings/audio_path.txt'))) {
          updateTextInput(
            session,
            "audioPathURL",
            value = read.csv(paste0(ammPath, '/settings/audio_path.txt'), header = F)[,]
          )
          
        } else if (grepl("https:", AUDIO_PATH())) {
          
          # if AUDIO_PATH maps to online source, use as URL
          
          # remove slash at end of audio path first, if needed.
          if (substr(AUDIO_PATH(), nchar(AUDIO_PATH()), nchar(AUDIO_PATH())) == "/") {
            url_value <- substr(AUDIO_PATH(), 1, nchar(AUDIO_PATH()) - 1)
          } else {
            url_value <- AUDIO_PATH()
          }
          
          updateTextInput(
            session,
            "audioPathURL",
            value = url_value
          )
          
          # If no cached path/url, ask the user
        } else {
          showModal(
            do.call(modalDialog, list(
              title = 'Set file path/URL:"',
              size = "m",
              easyClose = FALSE,
              footer = tagList(
                actionButton(
                  class = "btn-success",
                  inputId = ns("setPathURL"),
                  label = "Submit"
                ),
                modalButton("Close")
              ),
              tagList(
                'The AMMonitor database associated with this project has one or more
            audio files without a stored file path. If these files are stored outside
            of an AMMontior project\'s "recordings" folder, please enter a directory or
            URL to be used when trying to view recordings without file paths.',
                textInput(ns('audioPathURLModal'), 'Audio directory path/URL')
              )
            )) #end do.call for modal dialogue
          ) #end show modal
        }
      }
    })
    
    observeEvent(input$audioPathURLModal, {
      updateTextInput(session, "audioPathURL", value = input$audioPathURLModal)
    })
    
    # Display audio metadata (above the audio)
    output$audio_meta <- renderText({
      if (!is.null(recordingID())) {
        paste0(
          'Recording: ',
          recording_metadata()$filename,
          ' Source: ',
          input$audioPathURL
        )
      } else {
        ""
      }
    })
    
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
    }) |> bindEvent(recordingID())
    
    # Update tags (and clear old ones) when switching to a new audio file
    # Remove new boxes when recording changes
    observe({
      # Subset of annotations with matching AnnotationID
      the_annotations <- AMMonitor::qryCheckTags(
        con(),
        recordingID(),
        personid = NA, excludeperson = FALSE,
        mediatags = FALSE, exists = FALSE, disconnect = FALSE
      )
      
      if (showOnlySelected()) {
        the_annotations <- the_annotations[the_annotations$pk_annotationid == selectedRow(), ]
      }
      
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
      
      if (nrow(current_taxon_annotations()) != 0) {
        current_taxon_annotations(cbind(current_taxon_annotations(), selected_row = FALSE))
      }
      
    }) |> bindEvent(recordingID())
    
    #audio player-----------------
    output$player <- renderUI({
      updatePlayer()
      if (!is.null(recordingID()) && recordingID() != "") {
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
          function myFunction() {
            Shiny.onInputChange("', ns("curTime"), '", myAudio.currentTime);
            if (myAudio.currentTime < ', round(startTime()), ' || myAudio.currentTime >= ', round(startTime()+input$specLength),') {
              myAudio.pause();
            }
          }

          myAudio.ontimeupdate = function() {myFunction();};
          </script>

          <script>
          // Update the current time, when triggered
          myAudio.currentTime = ', updateCurTime(),
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
    
    audio_path <- reactive({
      req(recording_metadata()$filename)
      if (is.na(recording_metadata()$filepath)) {
        if (!input$audioPathURL == "") {
          file.path(input$audioPathURL, recording_metadata()$filename)
        } else {
          if (
            file.exists(file.path(ammPath, "recordings", recording_metadata()$filename))
          ) {
            file.path(ammPath, "recordings", recording_metadata()$filename)
          } else {
            ""
          }
        }
      } else {
        recording_metadata()$filepath
      }
    }) |> bindEvent(recording_metadata(), input$audioPathURL)
    
    fullAudio <- reactive({
      req(audio_path())
      if (grepl("^www.|^http:|^https:", audio_path())) {
        temp.file <- tempfile()
        utils::download.file(
          url = audio_path(),
          destfile = temp.file,
          quiet = TRUE,
          mode = "wb",
          cacheOK = TRUE
        )
        if (!file.exists(temp.file)) stop("File couldn't be downloaded")
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
      
      # Initialize filtered audio
      filtered <- fullAudio()
      filters_used(FALSE)
      
      # Don't denoise if the audio is too long
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
      req(w())
      seewave::spectro(
        w(),
        wl = input$spec_wl,
        wn = input$spec_wn,
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
      shinyjs::toggle(id = 'spectro-plots', condition = recordingID())
      shinyjs::toggle(id = 'specFreqRange', condition = recordingID())
      shinyjs::toggle(id = 'audioFreqRange', condition = recordingID())
    }) |> bindEvent(recordingID())
    
    #spectrogram-----------------------
    output$plot_bg <- renderPlot({
      req(s())
      s_amp <- s()$amp
      rownames(s_amp) <- s()$freq
      colnames(s_amp) <- s()$time
      s_df <- reshape2::melt(s_amp)
      names(s_df) <- c("freq", "time", "amp")
      
      if (input$binary_plot) {
        req(input$bin_amp_threshold)
        s_df$bin_amp <- s_df$amp > input$bin_amp_threshold
        s_aes <- aes(x = time, y = freq, fill = bin_amp)
        s_colors <- scale_fill_manual(
          values = eval(parse(text = paste0('seewave::', input$palette, '(2)')))
        )
      } else {
        s_aes <- aes(x = time, y = freq, fill = amp)
        s_colors <- scale_fill_gradientn(
          colours = eval(parse(text = paste0('seewave::', input$palette, '(255)')))
        )
      }
      
      ggplot(data = s_df, s_aes) +
        geom_raster(interpolate = TRUE, na.rm = TRUE) +
        s_colors +
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
        scale_y_continuous(name = "Frequency (kHz)", limits = c(max(0, input$specFreqRange[1]), min(input$specFreqRange[2], max(s()$freq))), expand = c(0,0))
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
        scale_y_continuous(name = "Frequency (kHz)", limits = c(max(0, input$specFreqRange[1]), min(input$specFreqRange[2], max(s()$freq))), expand = c(0,0))
    }, bg="transparent")
    
    #boxes
    output$plotx <- renderPlot({
      req(s())
      
      rects2 <- current_taxon_annotations()[which(
        current_taxon_annotations()$fk_mediaid == recordingID() &
          current_taxon_annotations()$x_min >= startTime() &
          current_taxon_annotations()$x_max <= (startTime() + input$specLength)
      ), c("x_min", "y_min", "x_max", "y_max")]
      
      rects2$y_min <- ifelse(is.na(rects2$y_min), max(0, input$specFreqRange[1]), rects2$y_min)
      rects2$y_max <- ifelse(is.na(rects2$y_max), min(input$specFreqRange[2], max(s()$freq)), rects2$y_max)
      
      if (nrow(rects2)) {
        rects2 <- rects2 - t(matrix(rep(c(startTime(),0,startTime(),0), nrow(rects2)), nrow = 4))
      }
      #cbind taxon ids to saved rects for labels
      rects2 <- cbind(rects2, current_taxon_annotations()[which(
        current_taxon_annotations()$fk_mediaid == recordingID() &
          current_taxon_annotations()$x_min >= startTime() &
          current_taxon_annotations()$x_max <= (startTime() + input$specLength)
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
        scale_y_continuous(name = "Frequency (kHz)", limits = c(max(0, input$specFreqRange[1]), min(input$specFreqRange[2], max(s()$freq))), expand = c(0,0)) +
        scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 1.5))
    }, bg="transparent")
    
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
        value = c(0, fullAudio()@samp.rate/2000)
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
      if (length(selectedRow()) != 0 && !all(is.na(selectedRow()))) {
        
        # Vector of true/false for boxes
        annotationID_match <- current_taxon_annotations()$pk_annotationid == selectedRow()
        
        # Selected row
        selected_box <- current_taxon_annotations()[which(annotationID_match),]
        
        if (nrow(selected_box) > 0 && !is.na(selected_box$x_min) && !is.na(selected_box$x_max)) {
          # Cbind true/false to dataframe of boxes
          current_taxon_annotations({
            cta <- current_taxon_annotations()
            cta$selected_row <- ifelse(is.na(annotationID_match), FALSE, annotationID_match)
            cta
          })
          
          # Jump to box if not already visible
          if (xor(selected_box$x_min < startTime(), selected_box$x_max > (startTime() + input$specLength))) {
            startTime(max(0, floor((selected_box$x_min + selected_box$x_max)/2 - input$specLength/2)))
          } else if (selected_box$x_min < startTime() && selected_box$x_max > (startTime() + input$specLength)) {
            startTime(max(0, floor(selected_box$x_min)))
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
    }) |> bindEvent(selectedRow(), recordingID())
    
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
    
    output$bin_plot_settings <- renderUI({
      req(s())
      valid_s <- s()$amp[is.finite(s()$amp)]
      isolate(
        sliderInput(
          inputId = ns('bin_amp_threshold'),
          label = 'Binary Amplitude Threshold (dB)',
          min = round(min(valid_s)),
          max = round(max(valid_s)),
          step = 1,
          value = ifelse(is.null(input$bin_amp_threshold), round(quantile(valid_s, probs = 0.7)), input$bin_amp_threshold),
          round = TRUE
        )
      )
      
    })
    
    # Toggle threshold slider
    observeEvent(input$binary_plot, {
      shinyjs::toggle(id = 'bin_plot_settings', condition = input$binary_plot)
    })
    
    return(
      reactiveValues(
        audioURL = reactive(audio_path()),
        spec_wl = reactive(input$spec_wl),
        spec_wn = reactive(input$spec_wn),
        spec_ovlp = reactive(input$spec_ovlp),
        amp_filter = reactive(input$bin_amp_threshold)
      )
    )
  })
}