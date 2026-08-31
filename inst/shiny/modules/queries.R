library(shiny)

# the ui function
queries_ui <- function(id) {
  ns <- NS(id)
  ui <- fluidPage(
    tags$script("
      Shiny.addCustomMessageHandler('resetValue', function(variableName) {
        Shiny.onInputChange(variableName, null);
      });
    "),
    wellPanel(
      tags$h3('1. Select a Query'),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns('qry_type'),
            'Query Type',
            choices = qry_info$qryType
          )
        ),
        column(
          width = 4,
          selectInput(
            ns('qry_name'),
            'Query Name',
            choices = qry_info$qryName
          )
        )
      ),
      tags$h4('Query Description:'),
      textOutput(ns('qry_descrip'))
    ),
    wellPanel(
      tags$h3('2. Enter parameters'),
      uiOutput(ns('params')),
    ),
    wellPanel(
      tags$h3('3. View/Save Results'),
      reactableOutput(ns('qry_result')),
      downloadButton(ns("downloadData"), "Download")
    )
  )
}

# the server function
queries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$qry_type, {
      updateSelectInput(
        session,
        'qry_name',
        'Query Name',
        choices = qry_info$qryName[qry_info$qryType == input$qry_type]
      )
    })

    output$qry_descrip <- renderText({
      qry_info$description[qry_info$qryName == input$qry_name]
    })
    
    observeEvent(input$qry_name, {
      old_params <- setdiff(
        names(input)[startsWith(names(input), 'qry_param_')],
        paste0('qry_param_', strsplit(qry_info$params[qry_info$qryName == input$qry_name], ', ')[[1]])
      )
      
      sapply(
        old_params,
        function(x) {
          removeUI(selector = paste0("div:has(> #", x, ")"))
        }
      )
    })
    
    output$params <- renderUI({
      req(input$qry_name)
  
      do.call(
        tagList,
        lapply(
          strsplit(qry_info$params[qry_info$qryName == input$qry_name], ', ')[[1]], 
          FUN = function(x) {
            do.call(textInput,
              list(inputId = ns(paste('qry_param', x, sep = '_')), label = x)
            )
          }
        )
      )
    })
    
    qry_result <- reactive({
      param_fields <- paste0('qry_param_', strsplit(qry_info$params[qry_info$qryName == input$qry_name], ', ')[[1]])
      
      # If parameters provided:
      if (length(param_fields) != 0 && any(sapply(param_fields, function(x) {!is.null(input[[x]]) && input[[x]] != ""}))) {

        params <- list()
        
        for (param in param_fields) {
          if (input[[param]] != "") {
            params[sub('qry_param_', "", param)] <- input[[param]]
          }
        }
        rs <- AMMonitor::qryBuiltIn(dbPath, input$qry_name, data.frame(params))
      } else {
        rs <- AMMonitor::qryBuiltIn(dbPath, input$qry_name)
      }
      rs
    })
    
    output$qry_result <- renderReactable({
      reactable(qry_result())
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(
          input$qry_name,
          format(Sys.time(), '_%Y%m%d_%H%M'),
          '.csv'
        )
      },
      content = function(file) {
        write.csv(qry_result(), file, row.names = FALSE, na = "")
      }
    )
  })
}
