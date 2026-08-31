#!! ModName = survey_compareVisits
#!! ModDisplayName = Enter your module shiny display name here.
#!! ModDescription = Enter your module description here.
#!! ModCitation = Tang, Caroline.  (2023). survey_compareVisits. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1/0
#!! FunctionArg = survey_qs !! survey sheet of the Excel form !! data.frame
#!! FunctionReturn = survey_visits !! The merged survey questions with the visits table !! data.frame
#!! Package = reactable !! 0.4.3 !! notes

# the ui function
survey_compareVisits_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Survey123 Form:"),
    reactableOutput(
      outputId = ns("survey_names")
    ),
    h4("Unmatched columns in the visits table:"),
    reactableOutput(
      outputId = ns("unmatched_names")
    )
  )
}


# the server function
survey_compareVisits_server <- function(id, survey_qs = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    comparison_table <- reactive({
      req(survey_qs())
      # Start with survey sheet
      survey_meta <- survey_qs()[
        which(!is.na(survey_qs()$label)), 
        c("label", "name", "type", "choice_filter", "appearance", "required")
      ]
      
      # Get column names and datatypes from dbDictionary
      visit_metadata <- DBI::dbGetQuery(
        con(), 
        "SELECT pk_fieldname, var_type, foreign_key_table, foreign_key_field, fk_listid, description 
    FROM dbdictionary WHERE pk_tablename = 'visits';"
      )
      
      # Outer join
      merged <- merge(
        survey_meta, visit_metadata, 
        by.x = "name", by.y = "pk_fieldname", 
        all = TRUE, sort = FALSE
      )
      
      merged
    })
    
    output$survey_names <- renderReactable({
      reactable(comparison_table()[which(!is.na(comparison_table()$label)),])
    })
    
    output$unmatched_names <- renderReactable({
      reactable(comparison_table()[which(is.na(comparison_table()$label)),])
    })
    
    return(
      reactiveValues(
        survey_visits = reactive(comparison_table())
      )
    )
  })
}
