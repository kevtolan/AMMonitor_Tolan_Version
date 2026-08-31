#!! ModName = template_create
#!! ModDisplayName = Create Templates
#!! ModDescription = Select a template type for each annotation
#!! ModCitation = Tang, Caroline.  (2023). template_create. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = annotations !! dataframe with annotations !! data.frame
#!! FunctionArg = audioPath !! file directory/URL !! character
#!! FunctionArg = spec_wl !! spectrogram window length !! numeric
#!! FunctionArg = spec_wn !! spectrogram window type !! character
#!! FunctionArg = spec_ovlp !! spectrogram window overlapL !! numeric
#!! FunctionArg = amp_filter !! amplitude filter !! numeric
#!! FunctionReturn = curAnnoID !! current annotation ID !! numeric
#!! FunctionReturn = curRecordingID !! current recording ID !! character
#!! FunctionReturn = corTemplates !! list of correlation templates !! corTemplateList
#!! FunctionReturn = binTemplates !! list of binary matching templates !! binTemplateList
#!! FunctionReturn = metadata !! Model name and path to audio file !! data.frame
#!! Package = monitoR !! 1.0.7 !! 
#!! Package = tuneR !! 1.4.7 !!


# the ui function
template_create_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns('templateInfo')),
    textInput(
      ns('templateName'),
      label = "Create a name for your template:"
    ),
    htmlOutput(ns('templateNameWarning')),
    htmlOutput(ns('templateNameWarning2')),
    selectInput(
      ns('templateType'),
      label = "Select a template type",
      choices = c("Correlation Matching Template" = 'corTemplate', "Binary Matching Template" = 'binTemplate')
    ),
    htmlOutput(ns('templateSettings')),
    actionButton(
      ns('createTemplate'),
      label = "Create Template"
    ),
    shinyjs::disabled(
      actionButton(
        ns('prevAnno'),
        label = "Previous Annotation"
      )
    ),
    actionButton(
      ns('nextAnno'),
      label = "Next Annotation"
    ),
    fluidRow(
      column(
        6,
        wellPanel(
          h3('Binary Templates'),
          verbatimTextOutput(ns('createdBinTemplates'))
        )
      ),
      column(
        6,
        wellPanel(
          h3('Correlation Templates'),
          verbatimTextOutput(ns('createdCorTemplates'))
        )
      )
    )
  )
}


# the server function
template_create_server <- function(id, annotations = reactive(NULL), audioPath = reactive(""), spec_wl = reactive(512), spec_wn = reactive("hanning"), spec_ovlp = reactive(0), amp_filter = reactive(0)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    i_anno <- reactiveVal(1)
    
    # Current row
    current_annotation <- reactive({
      req(nrow(annotations()))
      annotations()[i_anno(),]
    })
    
    # Initialize template lists
    corTemps <- reactiveVal(NULL)
    binTemps <- reactiveVal(NULL)
    temp_metadata <- reactiveVal(data.frame(model_name = c(), original_file = c()))
    
    # Get existing models
    existing_models <- reactiveVal(dbGetQuery(con(), 'SELECT model_name FROM models')$model_name)
    
    output$templateInfo <- renderText({
      paste0(
        "<h4>Annotation ", 
        i_anno(), 
        " of ", 
        nrow(annotations()),
        "</h4>"
      )
    })
    
    output$templateSettings <- renderText({
      if (input$templateType == "binTemplate") {
        if (is.null(amp_filter())) {
          "<span style = \"color:red\">Use the amplitude slider in the spectrogram settings to select the amplitude threshold.</span>"
        } else {
          paste0("Current amplitude threshold: ", amp_filter())
        }
        
      } else {
        ""
      }
    })
    
    # Suggest a template name for each new annotation
    observe({
      filename <- tools::file_path_sans_ext(basename(audioPath()))
      x <- paste0("template_", filename)
      if (x %in% existing_models()) {
        clone_counter <- 2
        new_name <- paste0(x, '_', clone_counter)
        while (new_name %in% existing_models()) {
          clone_counter <- clone_counter + 1
          new_name <- paste0(x, '_', clone_counter)
        }
      } else {
        new_name <- x
      }
      
      updateTextInput(
        session,
        'templateName',
        value = new_name
      )
    })
    
    output$templateNameWarning <- renderText("<span style=\"color:red\">Template name cannot be blank.</span>")
    output$templateNameWarning2 <- renderText(
      "<span style=\"color:red\">A model with this name already exists. Please choose a different name.</span>"
    )
    
    observeEvent(input$templateName, {
      if (!is.null(input$templateName) && nchar(input$templateName) > 0) {
        shinyjs::hide('templateNameWarning')
      }
      
      if (!(input$templateName %in% existing_models())) {
        shinyjs::hide('templateNameWarning2')
      }
    })
    
    observeEvent(input$createTemplate, {
      if (is.null(input$templateName) | nchar(input$templateName) == 0) {
        shinyjs::show('templateNameWarning')
      }
      
      if (nchar(input$templateName) > 0 && input$templateName %in% existing_models()) {
        shinyjs::show('templateNameWarning2')
      }
      
      req(nchar(input$templateName) > 0 && !(input$templateName %in% existing_models()))
      
      fullAudio <- as(
        if (grepl("^www.|^http:|^https:", audioPath())) {
          temp.file <- tempfile()
          utils::download.file(
            url = audioPath(), 
            destfile = temp.file, 
            quiet = TRUE, 
            mode = "wb", 
            cacheOK = TRUE
          )
          if (!file.exists(temp.file)) stop("File couldn't be downloaded")
          tuneR::readWave(
            temp.file, 
            from = current_annotation()$x_min,
            to = current_annotation()$x_max,
            units = "sec")
        } else {
          tuneR::readWave(
            audioPath(),
            from = current_annotation()$x_min,
            to = current_annotation()$x_max,
            units = "sec")
        }, "Wave")
      
      if (input$templateType == "binTemplate") {
        req(amp_filter())

        # Create binTemplate and append
        newBinTemp <- monitoR::makeBinTemplate(
          fullAudio,
          name = input$templateName,
          frq.lim = c(current_annotation()$y_min, current_annotation()$y_max),
          amp.cutoff = as.numeric(amp_filter()),
          wl = spec_wl(),
          ovlp = spec_ovlp(),
          wn = spec_wn(),
          write.wav = TRUE
        )
        
        if (is.null(binTemps())) {
          binTemps(newBinTemp)
        } else {
          binTemps(monitoR::combineBinTemplates(binTemps(), newBinTemp))
        }
        
      } else {
        
        # Create corTemplate and append
        newCorTemp <- monitoR::makeCorTemplate(
          fullAudio,
          name = input$templateName,
          frq.lim = c(current_annotation()$y_min, current_annotation()$y_max),
          wl = spec_wl(),
          ovlp = spec_ovlp(),
          wn = spec_wn(),
          write.wav = TRUE
        )
        if (is.null(corTemps())) {
          corTemps(newCorTemp)
        } else {
          corTemps(monitoR::combineCorTemplates(corTemps(), newCorTemp))
        }
        
      }
      existing_models(c(existing_models(), input$templateName))
      temp_metadata(rbind(
        temp_metadata(), 
        data.frame(
          model_name = input$templateName, 
          original_file = audioPath()
        )
      ))
      unlink('fullAudio.wav')
      
    })
    
    observeEvent(input$nextAnno, {
      if (i_anno() < nrow(annotations())) {
        i_anno(i_anno() + 1)
        shinyjs::enable('prevAnno')
      }
    })
    
    observeEvent(input$prevAnno, {
      if (i_anno() > 1) {
        i_anno(i_anno() - 1)
        shinyjs::enable('nextAnno')
      }
    })
    
    observe({
      req(i_anno())
      if (i_anno() == nrow(annotations())) {
        shinyjs::disable('nextAnno')
      } else {
        shinyjs::enable('nextAnno')
      }
      
      if (i_anno() == 1) {
        shinyjs::disable('prevAnno')
      } else {
        shinyjs::enable('prevAnno')
      }
    })
    
    output$createdBinTemplates <- renderPrint({
      binTemps()
    })
    
    output$createdCorTemplates <- renderPrint({
      corTemps()
    })
    
    
    return(
      reactiveValues(
        curAnno = reactive(current_annotation()$pk_annotationid),
        curRecording = reactive(current_annotation()$fk_mediaid),
        corTemplates = reactive(corTemps()),
        binTemplates = reactive(binTemps()),
        metadata = reactive(temp_metadata())
      )
    )
  })
}
