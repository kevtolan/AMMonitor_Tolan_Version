library(shiny)

# the ui function
queries_custom_ui <- function(id) {
  ns <- NS(id)
  ui <- fluidPage(
    tagList(
      wellPanel(
        tags$h3("Enter a query statement"),
        textAreaInput(ns('stmnt'), 'SQL Statement:'),
        actionButton(ns('execute'), 'Run Query'),
        verbatimTextOutput(ns('qry_status')),
        reactableOutput(ns('rslt')),
        downloadButton(ns("downloadData"), "Download")
      )
    )
  )
}

# the server function
queries_custom_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    qry_rslt <- reactiveValues(
      status = 0,
      rslt = NA,
      message = 'Awaiting query.'
    )
    
    observeEvent(input$execute, {
      rs <- tryCatch(
        {
          dbGetQuery(
            con(),
            input$stmnt
          )
        },
        error = function(cond) {cond}
      )
      
      if (!is.data.frame(rs)) {
        qry_rslt$status <- 0
        qry_rslt$rslt <- NA
        qry_rslt$message <- rs$message
      } else {
        qry_rslt$status <- 1
        qry_rslt$rslt <- rs
        qry_rslt$message <- "query successfully completed"
      }
      
      output$qry_status <- renderText({
        qry_rslt$message
      })
      
      output$rslt <- renderReactable({
        if (is.data.frame(qry_rslt$rslt)) {
          reactable(qry_rslt$rslt)
        }
      })
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(
          'custom_query',
          format(Sys.time(), '_%Y%m%d_%H%M'),
          '.csv'
        )
      },
      content = function(file) {
        write.csv(qry_rslt$rslt, file, row.names = FALSE, na = "")
      }
    )
  })
}
    