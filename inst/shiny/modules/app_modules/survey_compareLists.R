#!! ModName = survey_compareLists
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). survey_compareLists. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = survey_qs !! the survey sheet of the Survey123 Excel form !! data.frame
#!! FunctionArg = survey_choices !! the choices sheet of the Survey123 Excel from !! data.frame
#!! Package = reactable !! 0.4.3 !! notes

# the ui function
survey_compareLists_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns('list_name'),
      label = "Select a list in the survey",
      choices = ""
    ),
    fluidRow(
      column(
        6,
        h3("Survey choices"),
        reactableOutput(outputId = ns("survey_choices"))
      ),
      column(
        6,
        h3("Database list items"),
        reactableOutput(outputId = ns("db_listitems"))
      )
    )
  )
}


# the server function
survey_compareLists_server <- function(id, survey_qs = reactive(NULL), survey_choices = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      # Update the list dropdown when available
      req(survey_choices())
      list_names <- unique(survey_choices()$list_name[which(!is.na(survey_choices()$list_name))])
      updateSelectizeInput(
        session = session,
        inputId = "list_name",
        choices = c("", sort(list_names)),
      )
    })
    
    current_list <- reactive({
      req(input$list_name)
      survey_choices()[which(survey_choices()$list_name == input$list_name),]
    })
    
    output$survey_choices <- renderReactable({
      reactable(current_list())
    })
    
    
    
    output$db_listitems <- renderReactable({
      visits <- dbGetQuery(
        con(), 
        "SELECT pk_fieldname, var_type, foreign_key_table, foreign_key_field, fk_listid, description 
    FROM dbdictionary WHERE pk_tablename = 'visits';"
      )
      
      req(current_list())
      list_meta <- survey_qs()[which(grepl(input$list_name, survey_qs()$type)),]
      list_col <- merge(list_meta, visits, by.x = "name", by.y = "pk_fieldname", all.x = TRUE)
      
      db_list <- data.frame(
        fk_listid = character(0),
        item = character(0)
      )
      
      if (!is.na(list_col$foreign_key_table)) {
        db_list <- data.frame(
          fk_listid = list_col$foreign_key_table,
          item = dbGetQuery(
            con(), 
            paste0(
              "SELECT ", list_col$foreign_key_field, 
              " FROM ", list_col$foreign_key_table
            )
          )[,]
        )
      } else if (!is.na(list_col$fk_listid)) {
        db_list <- dbGetQuery(
          con(),
          paste0(
            "SELECT fk_listid, item FROM listitems 
            WHERE fk_listid = '", input$list_name,"';"
          )
        )
      }
      
      reactable(db_list)
      
    })
    
    return()
  })
}
