#!! ModName = template_score
#!! ModDisplayName = Template Scoring
#!! ModDescription = Select final templates and modify score cutoffs
#!! ModCitation = Tang, Caroline.  (2023). template_score. [Source code].
#!! ModNotes = Updated by Kaitlin Huber (2025).
#!! ModActive = 1/0
#!! FunctionArg = taxon !! target taxon for templates !! character
#!! FunctionArg = binTemplates !! potential binary matching templates !! binTemplateLists
#!! FunctionArg = corTemplates !! potential correlation templates !! corTemplateLists
#!! FunctionReturn = finalBinTemplates !! selected binary matching templates !! vector of binTemplateLists
#!! FunctionReturn = finalCorTemplates !! selected correlation templates !! vector of corTemplateLists
#!! Package = monitoR !! 1.0.7 !!
#!! Package = tuneR !! 1.4.7 !!
#!! Package = DBI !! 1.1.3 !!
#!! Package = howler !! 0.3.0 !!

template_score_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(
          6,
          selectInput(
            inputId = ns('templateType'),
            label = "Template Type",
            choices = c("Binary Matching Template" = 'binTemplate', "Correlation Matching Template" = 'corTemplate')
          ),
          h3('Select templates and score cutoffs'),
          uiOutput(ns('template_settings')),
          actionButton(
            ns('score_templates'),
            label = "Score Templates"
          )
        ),
        column(
          6,
          numericInput(
            inputId = ns('recording_limit'),
            label = "Maximum number of recordings to retrieve",
            min = 1,
            value = 5
          ),
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
        )
      )
    ),
    h3("Template Performance"),
    textOutput(ns('audioInfo')),
    
    #howler audio player
    uiOutput(ns('howler')),
    
    br(),
    
    # template score outputs and controls
    textOutput(ns('audioTime')),
    plotOutput(ns('detections')),
    br(),
    fluidRow(
      shinyjs::disabled(actionButton(
        inputId = ns('prev_audio'),
        label = "Previous Audio"
      )),
      shinyjs::disabled(actionButton(
        inputId = ns('prev_plot'),
        label = "Previous 30s"
      )),
      actionButton(
        inputId = ns('next_plot'),
        label = "Next 30s"
      ),
      actionButton(
        inputId = ns('next_audio'),
        label = "Next Audio"
      )
    ),
    br()
  )
}


# the server function
template_score_server <- function(id, taxon, binTemplates, corTemplates, input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get all the recordings that have the selected taxon as an annotation
    
    output$sound_playing <- renderText({
      if (isTRUE(input$sound_playing)) "Yes" else "No"
    })
    
    
    selected_recordings <- reactive({
      req(taxon(), input$recording_limit)
      
      params <- list(taxon(), input$recording_limit)
      
      rs <- DBI::dbSendQuery(
        con(),
        statement = paste0(
          "SELECT  * FROM
          (SELECT DISTINCT pk_mediaid, filename, filepath FROM media
          LEFT JOIN annotations ON annotations.fk_mediaid = media.pk_mediaid
          WHERE annotations.fk_taxonid = $1 AND media.media_type = 'audio') m
          ORDER BY RANDOM()
          LIMIT $2;"
        )
      )
      dbBind(rs, params)
      recordings <- dbFetch(rs)
      dbClearResult(rs)
      
      recordings
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
    
    audio_path <- reactive({
      req(selected_recordings())
      if (nrow(selected_recordings()) && !is.na(selected_recordings()$filename[i_audio()])) {
        if (grepl("google.com", selected_recordings()$filepath[i_audio()])) {
          temp_path <- paste(tempdir(), selected_recordings()$filename[i_audio()], sep = '/')
          googledrive::local_drive_quiet()
          googledrive::drive_download(
            file = selected_recordings()$filepath[i_audio()],
            path = temp_path,
            overwrite = TRUE
          )
          temp_path
        } else if (! selected_recordings()$filepath[i_audio()] %in% c(NA, "")) {
          selected_recordings()$filepath[i_audio()]
        } else {
          paste0(input$audioPathURL, selected_recordings()$filename[i_audio()])
        }
      } else {
        NULL
      }
    })
    
    # Index of audio
    i_audio <- reactiveVal(1)
    
    # Copy of templates to keep
    finalBinTemplates <- reactiveVal(binTemplates())
    finalCorTemplates <- reactiveVal(corTemplates())
    
    output$templateInfo <- renderPrint({
      if (input$templateType == "binTemplate") {
        summary(binTemplates())
      } else {
        summary(corTemplates())
      }
    })
    
    # Create slider input for each template
    output$template_settings <- renderUI({
      req(input$templateType)
      if (input$templateType == "binTemplate") {
        if (length(binTemplates()) == 0) {
          shinyjs::disable('score_templates')
          tagList(HTML("<span style=\"color:red\">No binary templates available. Please try selecting correlation templates.</span>"))
        } else {
          # Make a slider input for each template for the score cutoff (0-1)
          sliders <- tagList(
            lapply(
              monitoR::templateNames(binTemplates()),
              function(x) {
                fluidRow(
                  column(
                    1,
                    checkboxInput(
                      inputId = ns(paste0("use_", x)),
                      label = "",
                      value = TRUE
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = ns(paste0("cutoff_", x)),
                      label = x,
                      min = 0,
                      value = monitoR::templateCutoff(binTemplates())[[x]]
                    )
                  )
                )
              }
            )
          )
          sliders
        }
        
      } else if (input$templateType == "corTemplate") {
        if (length(corTemplates()) == 0) {
          shinyjs::disable('score_templates')
          tagList(HTML("<span style=\"color:red\">No correlation templates available. Please try selecting binary templates.</span>"))
        } else {
          sliders <- tagList(
            lapply(
              monitoR::templateNames(corTemplates()),
              function(x) {
                fluidRow(
                  column(
                    1,
                    checkboxInput(
                      inputId = ns(paste0("use_", x)),
                      label = "",
                      value = TRUE
                    )
                  ),
                  column(
                    6,
                    sliderInput(
                      inputId = ns(paste0("cutoff_", x)),
                      label = x,
                      min = 0,
                      max = 1,
                      value = monitoR::templateCutoff(corTemplates())[[x]]
                    )
                  )
                )
              }
            )
          )
          sliders
        }
      }
    })
    
    observe({
      req(input$templateType)
      tempNames <- NULL
      if (input$templateType == 'binTemplate') {
        if (length(binTemplates()) != 0) {
          tempNames <- monitoR::templateNames(binTemplates())
        }
      } else if (input$templateType == 'corTemplate') {
        if (length(corTemplates()) != 0) {
          tempNames <- monitoR::templateNames(corTemplates())
        }
      }
      req(input[[paste0("cutoff_", tempNames[1])]])
      shinyjs::enable('score_templates')
    })
    
    output$audioInfo <- renderText({
      
      req(input$score_templates)
      
      paste0(
        "Recording ", i_audio(), " of ", nrow(selected_recordings()), ": ",
        selected_recordings()[i_audio(), 'filename']
      )
    })
    
    output$audioTime <- renderText({
      
      req(input$score_templates)
      
      endTime <- min(startTime() + 30, round(duration()))
      
      paste0("Partial Audio Clip for Template Testing (30s increments)")
      
    })
    
    
    observeEvent(input$next_audio, {
      if (i_audio() < nrow(selected_recordings())) {
        i_audio(i_audio() + 1)
        shinyjs::enable('prev_audio')
      }
    }) # end next audio
    
    observeEvent(input$prev_audio, {
      if (i_audio() > 1) {
        i_audio(i_audio() - 1)
        shinyjs::enable('next_audio')
      }
    })
    
    observeEvent(i_audio(), {
      if (i_audio() == nrow(selected_recordings())) {
        shinyjs::disable('next_audio')
      } else {
        shinyjs::enable('next_audio')
      }
      
      if (i_audio() == 1) {
        shinyjs::disable('prev_audio')
      } else {
        shinyjs::enable('prev_audio')
      }
    })
    
    # create audio_file() (and download if necessary)
    audio_file <- reactive({
      
      req(audio_path())
      
      if (dir.exists(AUDIO_PATH())) {
        # stored locally
        addResourcePath("recordings", AUDIO_PATH())
        
        file.path("recordings",
                  basename(audio_path())
        )
        
      } else {
        # for web-hosted files
        recording_path <- paste0(tempdir(), '/recordings')
        
        dir.create(recording_path)
        
        temp.file <- paste0(recording_path,
                            "/",
                            basename(audio_path()))
        
        utils::download.file(
          url = audio_path(),
          destfile = temp.file,
          quiet = TRUE,
          mode = "wb",
          cacheOK = TRUE
        )
        
        # for howler
        addResourcePath("recordings", recording_path)
        
        file.path("recordings", basename(temp.file))
        
      }
    }) # end getting audio_file
    
    # Load audio
    startTime <- reactiveVal(0)
    
    audio_wave <- reactive({
      req(audio_path())
      as(if (grepl("^www.|^http:|^https:", audio_path())) {
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
      , "Wave")
    })
    
    duration <- reactive({length(audio_wave())/audio_wave()@samp.rate})
    
    # Scoring templates
    scores <- reactiveVal(NULL)
    
    
    observe({
      req(audio_wave(), input$templateType)
      endTime <- min(startTime()*audio_wave()@samp.rate + 30*audio_wave()@samp.rate, length(audio_wave()))
      current_audio <- audio_wave()[(startTime()*audio_wave()@samp.rate):endTime]
      
      if (input$templateType == "binTemplate") {
        req(binTemplates())
        
        # Get selected templates
        i_templates <- unlist(lapply(
          monitoR::templateNames(binTemplates()),
          function(x) {
            input[[paste0("use_", x)]]
          }
        ))
        
        if (any(i_templates)) {
          template_cutoffs <- unlist(lapply(
            monitoR::templateNames(binTemplates()),
            function(x) {
              input[[paste0("cutoff_", x)]]
            }
          ))
          
          selected_temps <- new("binTemplateList", templates = binTemplates()@templates[i_templates])
          
          # If only one template, name it 'default'
          selected_scores <- template_cutoffs[i_templates]
          if (length(selected_scores) == 1) {
            selected_scores <- c(default = selected_scores)
          }
          
          # Reassign template cutoffs
          monitoR::templateCutoff(selected_temps) <- selected_scores
          
          # Save selected templates to be passed on
          finalBinTemplates(selected_temps)
          
          # Score with currently selected audio
          tryCatch(
            {
              scores(monitoR::binMatch(current_audio, finalBinTemplates(), write.wav = TRUE, time.source = "fileinfo"))
            },
            error = function(e) {
              scores(NULL)
              showModal(
                modalDialog(as.character(e), easyClose = TRUE)
              )
            }
          )
          
        } else {
          finalBinTemplates(NULL)
          scores(NULL)
        }
        
      } else if (input$templateType == "corTemplate") {
        req(corTemplates())
        
        i_templates <- unlist(lapply(
          monitoR::templateNames(corTemplates()),
          function(x) {
            input[[paste0("use_", x)]]
          }
        ))
        
        if (any(i_templates)) {
          template_cutoffs <- unlist(lapply(
            monitoR::templateNames(corTemplates()),
            function(x) {
              input[[paste0("cutoff_", x)]]
            }
          ))
          
          selected_temps <- new("corTemplateList", templates = corTemplates()@templates[i_templates])
          
          # If only one template, name it 'default'
          selected_scores <- template_cutoffs[i_templates]
          if (length(selected_scores) == 1) {
            selected_scores <- c(default = selected_scores)
          }
          
          # Reassign template cutoffs
          monitoR::templateCutoff(selected_temps) <- selected_scores
          
          # Save selected templates to be passed on
          finalCorTemplates(selected_temps)
          
          # Score with currently selected audio
          tryCatch(
            {
              scores(monitoR::corMatch(current_audio, finalCorTemplates(), write.wav = TRUE, time.source = "fileinfo"))
            },
            error = function(e) {
              scores(NULL)
              showModal(
                modalDialog(as.character(e), easyClose = TRUE)
              )
            }
          )
        } else {
          finalCorTemplates(NULL)
          scores(NULL)
        }
        
      }
      
      unlink("current_audio.wav")
      
    }) |> bindEvent(input$score_templates, i_audio(), startTime())
    
    # Render the plot of detections
    output$detections <- renderPlot({
      req(scores())
      detects <- findPeaks(scores())
      
      # give different plot values for each template type
      if (input$templateType == 'binTemplate') {
        score_lim <- c(0,25)
        axis_position <- 4.25
      } else {
        score_lim <- c(0,1.5)
        axis_position <- 0.255
      }
      
      # get rid of original x axis for scores plot
      original_pars <- par()
      par(xaxt = 'n')
      
      # plot detections
      plot(detects,
           ask = FALSE,
           scorelim = score_lim)
      
      # reset original par
      par(xaxt = original_pars$xaxt )
      
      # add axis for full audio time
      axis(side = 1,
           at = seq(0, 30, by = 2),
           labels = as.character(
             seq(0, 30, by = 2) + startTime()
           ),
           pos = axis_position
      )
      
    })
    
    # Navigate between sections of the audio
    observeEvent(input$next_plot, {
      if (startTime() + 30 < duration()) {
        startTime(startTime() + 30)
      }
    })
    
    
    observeEvent(input$prev_plot, {
      if (startTime() - 30 >= 0) {
        startTime(startTime() - 30)
      }
      
    })
    
    # # howler audio player
    observeEvent(input$score_templates, {
      
      output$howler <- renderUI({
        req(audio_file())
        fluidPage(
          tags$br(),
          tags$p("Full Audio Player"),
          
          howler::howler(elementId = ns(paste0('sound_',
                                               i_audio(),
                                               input$templateType)),
                         list(c(audio_file()))),
          
          howler::howlerSeekSlider(ns(paste0('sound_',
                                             i_audio(),
                                             input$templateType))),
          
          howler::howlerPlayPauseButton(ns(paste0('sound_',
                                                  i_audio(),
                                                  input$templateType))),
          
          howler::howlerVolumeSlider(ns(paste0('sound_',
                                               i_audio(),
                                               input$templateType))),
          tags$p(
            "Duration:",
            textOutput(ns('sound_seek'),
                       container = tags$strong,
                       inline = TRUE),
            "/",
            textOutput(ns('sound_duration'),
                       container = tags$strong,
                       inline = TRUE)
          )
        )
        
      })
    })
    
    # howler outputs- must index to input with correct id for i_audio and templateType
    output$sound_duration <- renderText({
      req(input[[paste0("sound_", i_audio(), input$templateType, "_duration")]])
      paste0(
        sprintf(
          "%02d",
          round(
            reactiveValuesToList(input)[[
              paste0("sound_", i_audio(), input$templateType, "_duration")
            ]]
          )
        ),
        ' s'
      )
    })
    
    
    output$sound_seek <- renderText({
      req(input[[paste0("sound_", i_audio(), input$templateType, "_seek")]])
      sprintf(
        "%02d",
        round(
          reactiveValuesToList(input)[[
            paste0("sound_", i_audio(), input$templateType, "_seek")
          ]]
        )
      )
    })
    
    observeEvent(startTime(), {
      if (startTime() + 30 >= duration()) {
        shinyjs::disable('next_plot')
      } else {
        shinyjs::enable('next_plot')
      }
      if (startTime() == 0) {
        shinyjs::disable('prev_plot')
      } else {
        shinyjs::enable('prev_plot')
      }
    })
    
    # Reset startTime when changing between audios
    observeEvent(i_audio(), {
      startTime(0)
    })
    
    return(
      reactiveValues(
        finalBinTemplates = reactive(finalBinTemplates()),
        finalCorTemplates = reactive(finalCorTemplates())
      )
    )
  })
}
