#!! ModName = classifier_selectTaxon
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). classifier_selectTaxon. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionReturn = model_id !! model id of selected model !! integer
#!! FunctionReturn = verifications !! model outputs with verifications !! data.frame
#!! FunctionReturn = verification_bboxes !! spectrograms of model outputs !! list
#!! Package = tuneR !! 1.4.7 !!


# the ui function
classifier_selectTaxon_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h3("Select a model as the base for the classifier:"),
        uiOutput(ns('selectModel')),
        h3("Model Info:"),
        reactableOutput(ns("modelInfo")),
        actionButton(ns('getData'), label = "Get Data", class = "btn-large")
      ),
      column(
        8,
        h3("Model Outputs and Verifications:"),
        textOutput(ns("modelOutputData")),
        plotOutput(ns("modelVerifications")),
      )
    ),
    br()
  )
}


# the server function
classifier_selectTaxon_server <- function(id, argName1, argName2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$selectModel <- renderUI({
      models <- DBI::dbGetQuery(con(), "SELECT pk_modelid, model_name FROM models")
      choices <- models$pk_modelid
      names(choices) <- models$model_name
      selectizeInput(
        ns("modelName"),
        label = "Select a model:",
        choices = choices
      )
    })
    
    output$modelInfo <- renderReactable({
      rs <- dbSendQuery(
        con(),
        "SELECT * FROM models WHERE pk_modelid = $1;"
      )
      dbBind(rs, list(input$modelName))
      model <- dbFetch(rs)
      dbClearResult(rs)
      
      reactable(model, sortable = FALSE)
    })
    
    verifications <- reactive({
      req(input$modelName)
      qryModelVerificationConsensus(con(), input$modelName, disconnect = FALSE)
    })
    
    output$modelOutputData <- renderText({
      req(input$modelName, verifications())
      totalOutputs <- dbGetQuery(
        con(),
        paste0(
          "SELECT ROUND(COUNT(*)) FROM modeloutputs 
          WHERE modeloutputs.fk_modelid = ", input$modelName, ";"
        )
      )[,]
      
      totalVerified <- nrow(verifications())
      
      paste0("Total model outputs: ", totalOutputs, 
             " Total verified outputs: ", totalVerified)
      
    })
    
    output$modelVerifications <- renderPlot({
      req(nrow(verifications()) > 0)
      ggplot(verifications(), aes(x = factor(is_valid))) +
        geom_bar() +
        theme_bw() +
        scale_x_discrete(labels = c("False positive", "True positive")) +
        labs(
          title = paste0("Total verifications: ", nrow(verifications())),
          x = "Outcomes",
          y = "Count"
        )
    })
    
    observeEvent(input$getData, {
      req(verifications())
      if (nrow(verifications()) == 0) {
        showModal(
          modalDialog(
            "No model outputs with verifications were found. Please try another model.",
            easyClose = TRUE
          )
        )
      } else {
        showModal(
          modalDialog(
            "Please verify that this is the path to use to access the audio files. 
            If it is blank, the file paths in the database will be used. Please make 
            sure these are accurate, or errors may occur.",
            textInput(
              ns("audio_path"),
              "Audio File Path/URL",
              value = AUDIO_PATH()
            ),
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitPath"), "Submit and Get Data")
            )
          )
        )
      } 
    })
    verifications_bboxes <- reactiveVal(list())
    
    observeEvent(input$submitPath, {
      audio_url <- input$audio_path
      if (grepl("\\/$", audio_url)) {
        audio_url <- substr(audio_url, 1, nchar(audio_url) - 1)
      }
      removeModal()
      progress <- shiny::Progress$new(min = 0, max = 1)
      progress$set(message = "Obtaining spectrogram data...", value = 0)
      on.exit(progress$close())
      
      n <- nrow(verifications())
      media <- unique(verifications()$fk_mediaid)
      bboxes <- list()
      for (i in media) {
        ver_subset <- verifications()[which(verifications()$fk_mediaid == i),]
        media_db <- dbGetQuery(
          con(),
          paste0("SELECT * FROM media WHERE pk_mediaid = ", i, ";")
        )
        if (is.null(audio_url) || audio_url == "") {
          if (grepl("google.com", media_db$filepath)) {
            audio_path <- paste(tempdir(), media_db$filename, sep = '/')
            googledrive::local_drive_quiet()
            googledrive::drive_download(
              file = media_db$filepath,
              path = audio_path,
              overwrite = TRUE
            )
          } else {
            audio_path <- media_db$filepath
          }
          
        } else {
          audio_path <- file.path(audio_url, media_db$filename)
        }

        tryCatch(
          {
            if (grepl("^www.|^http:|^https:", audio_path)) {
              temp.file <- tempfile()
              utils::download.file(
                url = audio_path, 
                destfile = temp.file, 
                quiet = TRUE, 
                mode = "wb", 
                cacheOK = TRUE
              )
              if (!file.exists(temp.file)) stop("File couldn't be downloaded")
              audio_wave <- tuneR::readWave(temp.file)
            } else {
              audio_wave <- tuneR::readWave(audio_path)
            }
            
            for (modeloutput in ver_subset$pk_modeloutputid) {
              mo_row <- ver_subset[which(ver_subset$pk_modeloutputid == modeloutput),]
              bboxes[[as.character(modeloutput)]] <- seewave::spectro(
                audio_wave,
                tlim = c(mo_row$x_min, mo_row$x_max),
                flim = c(mo_row$y_min, mo_row$y_max),
                plot = FALSE
              )
              progress$inc(1/n)
            }
          },
          error = function(e) {
            showModal(modalDialog(as.character(e), easyClose = FALSE))
          }
        )
      }
      
      verifications_bboxes(bboxes)
      showModal(
        modalDialog(
          paste0(
            "Spectrogram data successfully retrieved for ", 
            length(verifications_bboxes()), 
            " records."
          ),
          title = "Success!",
          easyClose = TRUE
        )
      )
    })
    
    return(
      reactiveValues(
        model_id = reactive(input$modelName),
        verifications = reactive(verifications()),
        verification_bboxes = reactive(verifications_bboxes())
      )
    )
  })
}
