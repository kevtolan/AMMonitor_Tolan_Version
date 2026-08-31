#!! ModName = template_save
#!! ModDisplayName = Save Templates
#!! ModDescription = Save templates to an AMModels library
#!! ModCitation = Tang, Caroline.  (2023). template_save. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = taxon !! target taxon for templates !! character
#!! FunctionArg = binTemps !! final binary templates to be saved !! binTemplateList
#!! FunctionArg = corTemps !! final correlation templates to be saved !! corTemplateList
#!! FunctionArg = metadata !! Model name and path to audio file !! data.frame
#!! FunctionReturn = ammlib !! final AMModels Library !! AMModels Library
#!! Package = AMModels !! 0.1.4
#!! Package = monitoR !! 1.0.7


# the ui function
template_save_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        5,
        h3("Upload an AMModels Library"),
        fileInput(
          inputId = ns('uploadLib'),
          label = "Select an Existing AMModels Library",
          accept = ".rds"
        ),
        verbatimTextOutput(ns('libInfo')),
      ),
      column(
        2,
        h3("or")
      ),
      column(
        5,
        h3("Create a New AMModels Library"),
        textInput(
          inputId = ns('libName'),
          label = "Library Name",
          value = "templateLibrary"
        ),
        textInput(
          inputId = ns('owner'),
          label = "Owner",
          placeholder = "Name of owner"
        ),
        textAreaInput(
          inputId = ns('description'),
          label = "Library Description",
          value = "This library stores audio binary matching and correlation templates."
        ),
        actionButton(inputId = ns('createLib'), label = "Create New AMModels Library")
      )
    ),
    h3("Create descriptions for each template:"),
    fluidRow(
      column(
        6,
        "Binary Templates:",
        uiOutput(ns('binTempDesc'))
      ),
      column(
        6,
        "Correlation Templates:",
        uiOutput(ns('corTempDesc'))
      )
    ),
    "Download the AMModels Library to save any changes.",
    fluidRow(
      actionButton(inputId = ns('saveTemplates'), label = "Save Templates to Library"),
      downloadButton(outputId = ns('saveLib'), label = "Download Library")
    ),
    tags$head(tags$style(paste0('#', id, '-libInfo{overflow-y:scroll; max-height:200px;}')))
  )
}


# the server function
template_save_server <- function(id, taxon, binTemps, corTemps, metadata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to hold model library
    modelLib <- reactiveVal(NULL)
    
    # Assign modelLib through upload
    observe({
      file_info <- input$uploadLib
      req(file_info)
      ext <- tools::file_ext(file_info$datapath)
      if (tolower(ext) == "rds") {
        modelLib(readRDS(file_info$datapath))
      }
      
      updateTextInput(
        session = session,
        inputId = 'libName',
        value = tools::file_path_sans_ext(basename(file_info$name))
      )
    }) |> bindEvent(input$uploadLib)
    
    # Assign modelLib through creation
    observeEvent(input$createLib, {
      modelLib(
        AMModels::amModelLib(
          description = input$description, 
          info = list(owner = ifelse(input$owner == "", "Unknown", input$owner))
        )
      )
    })
    
    # View model information
    output$libInfo <- renderPrint({
      summary(modelLib())
    })
    
    output$binTempDesc <- renderUI({
      if (!is.null(binTemps())) {
        tagList(
          lapply(monitoR::templateNames(binTemps()), function(x) {
            textAreaInput(
              inputId = ns(x),
              label = x
            )
          })
        )
      }
      
    })
    
    output$corTempDesc <- renderUI({
      if (!is.null(corTemps())) {
        tagList(
          lapply(monitoR::templateNames(corTemps()), function(x) {
            textAreaInput(
              inputId = ns(x),
              label = x
            )
          })
        )
      }
      
    })
    
    observeEvent(input$saveTemplates, {
      req(class(modelLib()) == "amModelLib")
      if (!is.null(binTemps())) {
        # Make each template separate and save to library + db
        modelList <- list()
        for (x in monitoR::templateNames(binTemps())) {
          temp_metadata <- metadata()[which(metadata()$model_name == x),]
          # separate templates
          new_temp <- new("binTemplateList", templates = binTemps()@templates[x])
          modelList[[x]] <- AMModels::amModel(
            new_temp, 
            taxon = taxon(),
            model_type = 'Binary Template',
            original_file = temp_metadata$original_file, 
            description = input[[x]]
          )
          
          # register in db
          model_info <- data.frame(
            model_name = x,
            amml = ifelse(input$libName == "", "templateLibrary.RDS", paste0(input$libName, ".RDS")),
            model_type = 'Binary Template',
            model_description = input[[x]],
            fk_taxonid = taxon()
          )
          dbAppendTable(con(), 'models', model_info)
        }
        modelLib(AMModels::insertAMModelLib(models = modelList, amml = modelLib()))
      }
      
      if (!is.null(corTemps())) {
        # Make each template separate and save to library + db
        modelList <- list()
        for (x in monitoR::templateNames(corTemps())) {
          temp_metadata <- metadata()[which(metadata()$model_name == x),]
          # Separate templates
          new_temp <- new("corTemplateList", templates = corTemps()@templates[x])
          modelList[[x]] <- AMModels::amModel(
            new_temp, 
            taxon = taxon(),
            model_type = 'Correlation Template',
            original_file = temp_metadata$original_file, 
            description = input[[x]]
          )
          
          # Register in db
          model_info <- data.frame(
            model_name = x,
            amml = ifelse(input$libName == "", "templateLibrary.RDS", paste0(input$libName, ".RDS")),
            model_type = 'Correlation Template',
            model_description = input[[x]],
            fk_taxonid = taxon()
          )
          dbAppendTable(con(), 'models', model_info)
        }
        modelLib(AMModels::insertAMModelLib(models = modelList, amml = modelLib()))
      }
    })
    
    output$saveLib <- downloadHandler(
      filename = ifelse(input$libName == "", "templateLibrary.RDS", paste0(input$libName, ".RDS")),
      content = function(file) {
        saveRDS(modelLib(), file = file)
      }
    )
    
    return(
      reactiveValues(
        ammlib = reactive(modelLib())
      )
    )
  })
}
