#!! ModName = classifier_selectModels
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). classifier_selectModels. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionReturn = selectedModels !! list of models to be trained !! list
#!! Package = caret !! 6.0.94 !!


# the ui function
classifier_selectModels_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Classifier models"),
    uiOutput(ns("model_checkbox")),
    actionButton(ns("confirm"), "Select these models"),
    br(),
    "These models will be trained on the data:",
    verbatimTextOutput(ns("models_selected"))
  )
}


# the server function
classifier_selectModels_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$model_checkbox <- renderUI({
      models <- caret::getModelInfo()
      subsetModels <- models[names(models) %in% c('glmnet', 'svmLinear', 'svmRadial', 'rf', 'kknn')]
      subsetModelNames <- unlist(lapply(subsetModels, function(x) x$label))
      names(subsetModelNames) <- NULL
      
      checkboxGroupInput(
        inputId = ns("models"),
        label = "Select classifier models to create:",
        selected = names(subsetModels),
        choiceNames = subsetModelNames,
        choiceValues = names(subsetModels)
      )
    })
    chosenModels <- reactiveVal(list())
    
    observeEvent(input$confirm, {
      models <- caret::getModelInfo()
      chosenModels(models[names(models) %in% input$models])
    })
    
    output$models_selected <- renderPrint(names(chosenModels()))
    
    return(
      reactiveValues(
        selectedModels = reactive(chosenModels())
      )
    )
  })
}
