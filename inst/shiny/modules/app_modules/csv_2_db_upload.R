#!! ModName = csv_2_db_upload
#!! ModDisplayName = csv_2_db.
#!! ModDescription = Import a CSV to a database table.
#!! ModCitation = Larry Clarfeld.  (2024). csv_2_db_upload. [Source code].
#!! ModNotes = 
#!! ModActive = 1

# the ui function
csv_2_db_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("table_name"), label = "Select table", choices = unique(dictionary$pk_tablename)),
    checkboxInput(ns("show_fields"), "Show table fields"),
    conditionalPanel(
      sprintf("input['%s']", ns("show_fields")),
      reactable::reactableOutput(ns("table_fields"))
    ),
    fileInput(ns("csv_file"), "Select CSV file", accept = ".csv"),
    shinyjs::disabled(actionButton(ns("append_db"), "Append to Table")),
    tags$br(),
    tags$br(),
    verbatimTextOutput(ns("db_append_status")),
    reactable::reactableOutput(ns("table_contents")),
    tags$br()
  )
}

# the server function
csv_2_db_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$table_fields <- reactable::renderReactable({
      reactable::reactable(
        dictionary[
          dictionary$pk_tablename == input$table_name,
          c("pk_fieldname", "var_type", "description")
        ]
      )
    })
    
    new_records <- reactive({
      file <- input$csv_file
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      read.csv(file$datapath)
    })
    
    observe({
      req(new_records())
      shinyjs::enable("append_db")
      
      # Columns in the csv that are not in the specified table:
      missing_columns <- setdiff(
        names(new_records()),
        dictionary$pk_fieldname[dictionary$pk_tablename == input$table_name]
      )
      output$db_append_status <- renderText({
        ifelse(
          length(missing_columns) == 0,
          "",
          paste0(
            "Error: The following columns from the csv are not present in the table '",
            input$table_name, 
            "': \n",
            paste(missing_columns, collapse = "; ")
          )
        )
      })
    })
    
    output$table_contents <- reactable::renderReactable({
      reactable::reactable(new_records())
    })
    
    observeEvent(input$append_db, {
      tryCatch(
        {
          rs <- DBI::dbAppendTable(
            con(),
            input$table_name,
            new_records()
          )
          output$db_append_status <- renderText({
            paste0(
              "Upload Successful\n",
              "Added ", rs, " new record(s) to the ",
              input$table_name, " table."
            )
          })
        },
        error = function(cond) {
          output$db_append_status <- renderText({
            paste(
              "Error:",
              cond$message
            )
          })
        }
      )
    })
  })
}
