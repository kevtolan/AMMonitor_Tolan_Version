#!! ModName = classifier_saveModels
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). classifier_saveModels. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = final_models !! list of trained models !! list
#!! FunctionArg = model_id !! pk_modelid of model whose outputs are used as data !! integer
#!! FunctionReturn = ammlib !! final AMModels Library !! AMModels Library
#!! Package = AMModels !! 0.1.4


# the ui function
classifier_saveModels_ui <- function(id) {
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
          value = "classifierLibrary"
        ),
        textInput(
          inputId = ns('owner'),
          label = "Owner",
          placeholder = "Name of owner"
        ),
        textAreaInput(
          inputId = ns('description'),
          label = "Library Description",
          value = "This library stores models that classify other model outputs."
        ),
        actionButton(inputId = ns('createLib'), label = "Create New AMModels Library")
      )
    ),
    h3("Create descriptions for each model:"),
    uiOutput(ns('models')),
    "Download the AMModels Library to save any changes.",
    fluidRow(
      actionButton(inputId = ns('saveModels'), label = "Save Models to Library"),
      downloadButton(outputId = ns('saveLib'), label = "Download Library")
    ),
    tags$head(tags$style(paste0('#', id, '-libInfo{overflow-y:scroll; max-height:200px;}')))
  )
}


# the server function
classifier_saveModels_server <- function(id, final_models, model_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get information about the parent model
    parent_info <- reactive({
      req(model_id())
      dbGetQuery(
        con(),
        paste0("SELECT * FROM models WHERE pk_modelid = ", model_id(), ";")
      )
    })
    
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
    
    # Create input boxes for description of every model
    output$models <- renderUI({
      if (!is.null(final_models())) {
        tagList(
          lapply(names(final_models()), function(x) {
            textAreaInput(
              inputId = ns(x),
              label = x
            )
          })
        )
      }
    })
    
    observeEvent(input$saveModels, {
      req(class(modelLib()) == "amModelLib")

      # Separately save each model to library + db
      modelList <- list()
      for (mod in names(final_models())) {
        modelList[[mod]] <- AMModels::amModel(
          final_models()[[mod]], # caret train object
          taxon = parent_info()$fk_taxonid,
          model_type = final_models()[[mod]]$method,
          parent_model = parent_info()$model_name,
          description = input[[mod]]
        )
        
        model_info <- data.frame(
          model_name = mod,
          amml = ifelse(input$libName == "", "classifierLibrary.RDS", paste0(input$libName, ".RDS")),
          model_type = final_models()[[mod]]$method,
          model_description = input[[mod]],
          fk_taxonid = parent_info()$fk_taxonid,
          fk_parentid = parent_info()$pk_modelid
        )
        dbAppendTable(con(), 'models', model_info)
      }
      
      modelLib(AMModels::insertAMModelLib(models = modelList, amml = modelLib()))
    })
    
    output$saveLib <- downloadHandler(
      filename = ifelse(input$libName == "", "classifierLibrary.RDS", paste0(input$libName, ".RDS")),
      content = function(file) {
        saveRDS(modelLib(), file = file)
      }
    )
    
    return(
      reactiveValues(
        ammLib = reactive(modelLib())
      )
    )
  })
}
