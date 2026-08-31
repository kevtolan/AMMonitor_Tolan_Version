#!! ModName = classifier_testModels
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). classifier_testModels. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = trained_models !! list of trained models !! list
#!! FunctionArg = test_valid !! model output test data !! data.frame
#!! FunctionArg = test_bboxes !! list of spectrograms for each model output !! list
#!! FunctionReturn = final_models !! list of models to save !! list
#!! Package = reactable !! 1.14.8 !!
#!! Package = caret !! 6.0.94


# the ui function
classifier_testModels_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      4,
      h3("Select Models to Test"),
      "The models with the best parameters from the search in the previous tab are displayed here.",
      uiOutput(ns("modelnames")),
      actionButton(ns("test"), "Test models")
    ),
    column(
      8,
      h3("Model Performance"),
      reactableOutput(ns("metrics")),
      br(),
      plotOutput(ns("rocplot"))
    )
  )
}


# the server function
classifier_testModels_server <- function(id, trained_models, test_valid, test_bboxes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    final_models <- reactiveVal(NULL)
    
    # Create test data
    test_data <- reactive({
      req(test_valid(), test_bboxes())
      
      # Replace 0s and 1s with characters
      tv <- test_valid()[c("pk_modeloutputid", "value_num", "is_valid")]
      tv$is_valid <- factor(ifelse(tv$is_valid == 1, "TP", "FP"), levels = c("TP", "FP"))
      
      # Convert bboxes to horizontal vectors and merge them with modeloutput data
      bboxes_long <- lapply(test_bboxes(), function(x) {
        as.data.frame(t(as.vector(x$amp)))
      })
      
      bboxes_df <- as.data.frame(
        data.table::rbindlist(bboxes_long, use.names = TRUE, idcol = "pk_modeloutputid")
      )
      
      merged <- merge(tv, bboxes_df, all = TRUE, by = "pk_modeloutputid")
      
      merged[-which(names(merged) %in% c("pk_modeloutputid"))]
    })
    
    # copy trainModels display functions
    
    output$modelnames <- renderUI({
      req(trained_models())
      
      # Create checkbox group input of all models
      checkboxGroupInput(
        ns("selected_models"),
        label = NULL,
        choices = names(trained_models()),
        selected = names(trained_models())
      )
    })
    
    # Test models on test data when button is pressed
    observeEvent(input$test, {
      req(trained_models(), input$selected_models)
      final_models(trained_models()[input$selected_models])
      
    })
    
    # Get performance metrics (Recall, Specificity, Precision, F1)
    output$metrics <- renderReactable({
      req(final_models())
      
      model_perf <- data.frame()
      
      for (model in names(final_models())) {
        testpreds <- caret::predict.train(final_models()[[model]], newdata = test_data(), type = "raw")
        test_mat <- caret::confusionMatrix(testpreds, test_data()$is_valid, positive = "TP")
        scores <- t(as.data.frame(round(test_mat$byClass, 3)))
        
        if (nrow(model_perf) == 0) {
          model_perf <- scores
        } else {
          model_perf <- rbind(model_perf, scores)
        }
      }
      
      rownames(model_perf) <- names(final_models())
      
      reactable(model_perf)
      
    })
    
    # Plot ROC
    output$rocplot <- renderPlot({
      req(final_models())
      
      pred_rates <- data.frame(model = character(0), tpr = numeric(0), fpr = numeric(0))
      
      for (model in names(final_models())) {
        preds <- caret::predict.train(final_models()[[model]], newdata = test_data(), type = "prob")
        probs <- preds$TP
        actual <- test_data()$is_valid[order(-probs)]
        ordered_probs <- probs[order(-probs)]
        
        t_probs <- ordered_probs[actual == "TP"]
        f_probs <- ordered_probs[actual == "FP"]
        
        tpr <- 0
        fpr <- 0
        
        for (p in ordered_probs) {
          tpr <- c(tpr, mean(t_probs >= p))
          fpr <- c(fpr, mean(f_probs >= p))
        }
        
        pred_rates <- rbind(pred_rates, data.frame(model = model, tpr = tpr, fpr = fpr))
        
      }
      ggplot(pred_rates, aes(x = fpr, y = tpr, colour = model)) +
        geom_line() +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "darkgrey") +
        xlim(0, 1) +
        ylim(0, 1) +
        theme_bw() +
        labs(title = "ROC plot", x = "False positive rate", y = "True positive rate", colour = "Model")
      
    })
    
    return(
      reactiveValues(
        final_models = reactive(final_models())
      )
    )
    
  })
}
