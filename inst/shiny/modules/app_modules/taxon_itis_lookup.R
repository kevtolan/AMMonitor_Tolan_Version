#!! ModName = taxon_itis_lookup
#!! ModDisplayName = Taxon Look-up
#!! ModDescription = Look-up a taxon from itis to add to the database.
#!! ModCitation = Laurence Clarfeld.  (2023). taxon_itis_lookup. [Source code].
#!! ModNotes = Enter your module notes here.
#!! ModActive = 1
#!! FunctionReturn = tsn !! Taxonomic Serial Number of selected taxon !! character
#!! Package = ritis !! 1.0.0 !! API to ITIS
#!! Packate = reactable !! 0.3.0 !! For rendering table of taxon search results


# the ui function
taxon_itis_lookup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        textInput(ns('taxonSearchName'), 'Taxon common/scientific name')
      ),
      column(
        width = 2,
        div(
          style = "margin-top: 20px;", 
          actionButton(ns('search'), 'Search ITIS')
        )
      ),
      column(
        width = 4,
        offset = 2,
        wellPanel(
          style = "background: aquamarine",
          textInput(ns('tsn'), 'Taxonomic Serial Number (TSN)')
        )
      )
    ),
    fluidRow(
      reactableOutput(ns('searchResultsTable'))
    )
  )
}


# the server function
taxon_itis_lookup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    taxon_search_rslt <- eventReactive(input$search, {
      
      tryCatch({
        match_df <- data.frame(
          author = character(0),
          matchType = character(0),
          sciName = character(0),
          tsn = character(0),
          common_commonName = character(0),
          common_language = character(0),
          common_tsn = character(0)
        )
        match_df <- rbind(match_df, ritis::search_anymatch(input$taxonSearchName))
      },
        error = function(cond) {
          showModal(modalDialog(
            paste(cond, 'Please try again later.', sep = '\n'),
            title = 'ITIS query failed',
            footer = modalButton("Dismiss"),
            easyClose = TRUE,
            fade = TRUE
          ))
        },
      finally = {match_df}
      )
      
      # ritis::search_anymatch(input$taxonSearchName)
    })
    
    output$searchResultsTable <- renderReactable({
      req(taxon_search_rslt())
      reactable(
        taxon_search_rslt(),
        selection = "single",
        onClick = "select"
      )
    })
    
    observeEvent(input$searchResultsTable__reactable__selected, {
      updateTextInput(
        session,
        'tsn',
        'Taxonomic Serial Number (TSN)',
        taxon_search_rslt()$tsn[input$searchResultsTable__reactable__selected]
      )
    })
    
    return(
      reactiveValues(
        tsn = reactive(input$tsn)
      )
    )
  })
}
