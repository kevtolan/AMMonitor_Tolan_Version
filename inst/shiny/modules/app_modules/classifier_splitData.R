#!! ModName = classifier_splitData
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). classifier_splitData. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = verifications !! dataframe of outputs with verifications !! data.frame
#!! FunctionArg = verification_bboxes !! spectrograms of each output bbox !! list
#!! FunctionReturn = train_valid !! dataframe of training data !! data.frame
#!! FunctionReturn = train_bbox !! list of spectrograms for training data !! list
#!! FunctionReturn = test_valid !! dataframe of test data !! data.frame
#!! FunctionReturn = test_bbox !! list of spectrograms for test data !! list

# the ui function
classifier_splitData_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Split verification data into train and test sets"),
    sliderInput(
      ns("trainprop"),
      "Proportion of Train Data",
      min = 0,
      max = 1,
      step = 0.05,
      value = 0.8
    ),
    numericInput(
      ns("seed"),
      "Random Seed",
      value = 1
    ),
    actionButton(
      ns("split"),
      "Split Data"
    ),
    textOutput(ns('traintestrows')),
    plotOutput(ns('trainPosRatio'))
  )
}


# the server function
classifier_splitData_server <- function(id, verifications, verification_bboxes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    train_valid <- reactiveVal(NULL)
    train_bboxes <- reactiveVal(list())
    test_valid <- reactiveVal(NULL)
    test_bboxes <- reactiveVal(list())
    
    observeEvent(input$split, {
      set.seed(input$seed)
      trainIndex <- caret::createDataPartition(
        y = factor(verifications()$is_valid),
        p = input$trainprop,
        list = FALSE
      )
      
      train_valid(verifications()[trainIndex,])
      test_valid(verifications()[-trainIndex,])
      
      train_bboxes(verification_bboxes()[as.character(train_valid()$pk_modeloutputid)])
      test_bboxes(verification_bboxes()[as.character(test_valid()$pk_modeloutputid)])
    })
    
    output$traintestrows <- renderText({
      paste("Number of rows in training data:", nrow(train_valid()),
            "Number of rows in test data:", nrow(test_valid()))
    })
    
    output$trainPosRatio <- renderPlot({
      req(nrow(train_valid()) > 0)
      ggplot(train_valid(), aes(x = factor(is_valid))) +
        geom_bar(stat = "count") +
        scale_x_discrete(labels = c("False positive", "True positive")) +
        theme_bw() +
        labs(
          title = "Counts of true and false positives in training data", 
          x = "Verification",
          y = "Count"
        )
    })
    
    return(
      reactiveValues(
        train_valid = reactive(train_valid()),
        train_bbox = reactive(train_bboxes()),
        test_valid = reactive(test_valid()),
        test_bbox = reactive(test_bboxes())
      )
    )
  })
}
