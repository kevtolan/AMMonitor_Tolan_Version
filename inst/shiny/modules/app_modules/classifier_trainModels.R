#!! ModName = classifier_trainModels
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). classifier_trainModels. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = model_id !! pk_modelid of model whose outputs are used as data !! integer
#!! FunctionArg = selectedModels !! list of models to be trained !! list
#!! FunctionArg = train_valid !! model output training data !! data.frame
#!! FunctionArg = train_bboxes !! list of spectrograms for each model output !! list
#!! FunctionReturn = trained_models !! a list of trained models !! list
#!! Package = data.table !! 1.14.8 !!
#!! Package = caret !! 6.0.94 !!
#!! Package = reactable !! 1.14.8 !!


# the ui function
classifier_trainModels_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h3("Model Training Parameters"),
        textOutput(ns("modelinfo")),
        numericInput(
          ns("cvfolds"),
          "Cross-Validation folds (integer)",
          value = 5
        ),
        sliderInput(
          ns("cvprop"),
          "Cross-Validation training split proportion",
          min = 0,
          max = 1,
          step = 0.05,
          value = 0.8
        ),
        numericInput(
          ns("repeats"),
          "Repeats (integer)",
          value = 1
        ),
        numericInput(
          ns("seed"),
          "Set Seed Value",
          value = 1
        ),
        h3("Grid Search Parameters"),
        "Add all grid search parameters here. 
        To use default model values, leave the inputs blank. 
        For numeric values, to use a fixed value in the grid search, 
        enter the same value for both the min and max.",
        uiOutput(ns("paramgrid")),
        actionButton(ns("train"), "Train model")
      ),
      column(
        8,
        h3("Model Performance"),
        reactableOutput(ns("best_params")),
        br(),
        verbatimTextOutput(ns("metrics")),
        plotOutput(ns("rocplot")),
        actionButton(ns("prev_model"), "Previous Model"),
        actionButton(ns("next_model"), "Next Model")
      )
    )
  )
}


# the server function
classifier_trainModels_server <- function(id, model_id, selectedModels, train_valid, train_bboxes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    model_i <- reactiveVal(1)
    
    model_name <- reactive({
      req(model_id())
      dbGetQuery(
        con(),
        paste0("SELECT model_name FROM models WHERE pk_modelid = ", model_id(), ";")
      )[,]
    })
    
    current_model <- reactive({
      req(selectedModels())
      selectedModels()[model_i()]
    })
    
    new_model_name <- reactive({
      req(current_model(), model_name())
      
      existing_names <- dbGetQuery(
        con(),
        paste0("SELECT model_name FROM models;")
      )[,]
      
      x <- paste0(model_name(), "_", names(current_model()))
      
      if (x %in% existing_names) {
        clone_counter <- 2
        new_name <- paste0(x, "_", clone_counter)
        while (new_name %in% existing_names) {
          clone_counter <- clone_counter + 1
          new_name <- paste0(x, '_', clone_counter)
        }
      } else {
        new_name <- x
      }
      new_name
    })
    
    trained_models <- reactiveVal(list())
    
    observeEvent(input$next_model, {
      if (model_i() < length(selectedModels())) {
        model_i(model_i() + 1)
      }
    })
    
    observeEvent(input$prev_model, {
      if (model_i() > 1) {
        model_i(model_i() - 1)
      }
    })
    
    train_data <- reactive({
      req(train_valid(), train_bboxes())
      
      # Replace 0s and 1s with characters
      tv <- train_valid()[c("pk_modeloutputid", "value_num", "is_valid")]
      tv$is_valid <- factor(ifelse(tv$is_valid == 1, "TP", "FP"), levels = c("TP", "FP"))
      
      # Convert bboxes to horizontal vectors and merge them with modeloutput data
      bboxes_long <- lapply(train_bboxes(), function(x) {
        as.data.frame(t(as.vector(x$amp)))
      })
      
      bboxes_df <- as.data.frame(
        data.table::rbindlist(bboxes_long, use.names = TRUE, idcol = "pk_modeloutputid")
      )
      
      merged <- merge(tv, bboxes_df, all = TRUE, by = "pk_modeloutputid")
      
      merged[-which(names(merged) %in% c("pk_modeloutputid"))]
    })
    
    output$modelinfo <- renderText({
      req(current_model())
      paste0(
        "Model ", model_i(), " of ", length(selectedModels()), ": ", 
        current_model()[[1]]$label
      )
    })
    
    output$paramgrid <- renderUI({
      req(current_model())
      
      params <- current_model()[[1]]$parameters
      input_list <- list()
      
      # Create numeric inputs for min/max/# of steps for numeric parameters
      # Text input for character parameters (not sure how to check validity though)
      for (i in seq_len(nrow(params))) {
        current_row <- params[i,]
        if (current_row$parameter != "parameter") {
          if (current_row$class == "numeric") {
            input_list <- append(
              input_list,
              list(
                numericInput(
                  ns(paste0(current_row$parameter, "_min")),
                  paste0(current_row$parameter, " Minimum Value"),
                  value = NA
                ),
                numericInput(
                  ns(paste0(current_row$parameter, "_max")),
                  paste0(current_row$parameter, " Maximum Value"),
                  value = NA
                ),
                numericInput(
                  ns(paste0(current_row$parameter, "_steps")),
                  paste0(current_row$parameter, " Step Size"),
                  value = 1
                )
              )
            )
          } else {
            input_list <- append(
              input_list,
              list(
                textInput(
                  ns(paste0(current_row$parameter, "_values")),
                  paste0(current_row$parameter, " Values (comma separated)"),
                  value = ""
                )
              )
            )
          }
        }
      }
      if (length(input_list) > 0) {
        tagList(input_list)
      } else {
        NULL
      }
      
    })
    
    observeEvent(input$train, {
      progress <- shiny::Progress$new(min = 0, max = 1)
      progress$set(message = "Configuring settings...", value = 0)
      on.exit(progress$close())
      
      # Create train control object
      fitControl <- caret::trainControl(
        method = "repeatedcv",
        number = input$cvfolds,
        repeats = input$repeats,
        search = "grid",
        summaryFunction = twoClassSummary,
        classProbs = TRUE,
        savePredictions = "final",
        seeds = NA, # Value of NA will stop the seed from being set within the worker processes 
        verboseIter = FALSE
      )
      
      progress$set(message = "Creating grid...", value = 0.1)
      # Train model on data using hyperparameter grid search
      params <- current_model()[[1]]$parameters
      param_values <- list()
      
      # Get the values for the grid for numeric/character parameters
      for (i in seq_len(nrow(params))) {
        current_row <- params[i,]
        if (current_row$class == "numeric") {
          grid_min <- input[[paste0(current_row$parameter, "_min")]]
          grid_max <- input[[paste0(current_row$parameter, "_max")]]
          grid_steps <- input[[paste0(current_row$parameter, "_steps")]]
          if (is.na(grid_min) && is.na(grid_max)) {
            next
          } else if (is.na(grid_min)) {
            param_values[[current_row$parameter]] <- grid_max
          } else if (is.na(grid_max)) {
            param_values[[current_row$parameter]] <- grid_min
          } else if (is.na(grid_steps))
            param_values[[current_row$parameter]] <- seq(grid_min, grid_max)
          else {
            param_values[[current_row$parameter]] <- seq(grid_min, grid_max, by = grid_steps)
          }
        } else {
          input_vals <- input[[paste0(current_row$parameter, "_values")]]
          if (input_vals == "") {
            next
          } else {
            param_values[[current_row$parameter]] <- trimws(unlist(strsplit(input_vals, ",")))
          }
        }
      }
      
      param_grid <- expand.grid(param_values)
      
      
      if (names(current_model()) %in% c('svmLinear', 'svmRadial')) {
        scale <- FALSE
      } else {
        scale <- TRUE
      }
      progress$set(message = "Training model...", value = 0.3)
      new_model <- list()
      tryCatch(
        {
          set.seed(input$seed)
          new_model[[new_model_name()]] <- caret::train(
            train_data()[-which(names(train_data()) == "is_valid")], 
            train_data()$is_valid,
            method = names(current_model()),
            trControl = fitControl,
            scale = scale,
            metric = 'ROC',
            preProcess = NULL,
            tuneGrid = param_grid
          )
        },
        error = function(e) {
          showModal(
            modalDialog(
              as.character(e),
              title = paste0("Error in training model ", names(current_model())),
              easyClose = TRUE
            )
          )
        }
      )
      trained_models(append(trained_models(), new_model))
      progress$set(message = "Training model...", value = 1)
    })
    
    # Best parameters
    output$best_params <- renderReactable({
      req(new_model_name() %in% names(trained_models()))
      reactable(trained_models()[[new_model_name()]]$bestTune)
    })
    
    # Get performance metrics (Recall, Specificity, Precision, F1)
    output$metrics <- renderPrint({
      req(new_model_name() %in% names(trained_models()))
      
      trainpreds <- caret::predict.train(trained_models()[[new_model_name()]], type = "raw")
      train_mat <- caret::confusionMatrix(trainpreds, train_data()$is_valid, positive = "TP")
      
      round(train_mat$byClass, 3)
    })
    
    # Plot ROC
    output$rocplot <- renderPlot({
      req(new_model_name() %in% names(trained_models()))
      preds <- trained_models()[[new_model_name()]]$pred
      probs <- preds$TP
      actual <- preds$obs[order(-probs)]
      ordered_probs <- probs[order(-probs)]
      
      t_probs <- ordered_probs[actual == "TP"]
      f_probs <- ordered_probs[actual == "FP"]
      
      tpr <- 0
      fpr <- 0
      
      for (p in ordered_probs) {
        tpr <- c(tpr, mean(t_probs >= p))
        fpr <- c(fpr, mean(f_probs >= p))
      }
      
      ggplot(mapping = aes(x = fpr, y = tpr)) +
        geom_line() +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "darkgrey") +
        xlim(0, 1) +
        ylim(0, 1) +
        theme_bw() +
        labs(title = "ROC plot", x = "False positive rate", y = "True positive rate")
    })
    
    return(
      reactiveValues(
        trained_models = reactive(trained_models())
      )
    )
  })
}
