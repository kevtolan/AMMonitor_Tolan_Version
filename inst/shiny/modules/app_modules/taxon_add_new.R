#!! ModName = taxon_add_new
#!! ModDisplayName = Add Taxon
#!! ModDescription = Add a new taxon (from ITIS) to the database.
#!! ModCitation = Laurence Clarfeld.  (2023). taxon_add_new. [Source code].
#!! ModNotes = Ingests a taxonomic serial number (TSN), displays info, and tries to add it.
#!! ModActive = 1
#!! FunctionArg = tsn !! taxonomic serial number (TSN) from ITIS !! character
#!! FunctionReturn = taxonAddResults !! Results of attempted taxon additions !! data.frame


# the ui function
taxon_add_new_ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns('selectedTaxon')),
    tags$h3('Common Names'),
    reactableOutput(ns('taxonCommonNames')),
    tags$h3('Taxonomic Hierarchy'),
    reactableOutput(ns('taxonHierarchy')),
    tags$br(),
    textInput(ns('commonName'), 'Desired Common Name'),
    checkboxInput(ns('overwrite'), 'Overwrite existing record'),
    actionButton(ns('addTaxon'), 'Add Taxon to Database')
  )
}

# the server function
taxon_add_new_server <- function(id, tsn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    taxonAddResults <- reactiveVal(NA)
    
    itis_record <- reactive({
      req(tsn())
      
      itis_full_record <- tryCatch(
        ritis::full_record(tsn()),
        error = function(cond) {
          cond
        }
      )
      
      if (any(class(itis_full_record) == 'error')) {
        showModal(modalDialog(
          title = "ITIS Query Failed",
          itis_full_record$message
        ))
        NA
      } else {
        itis_full_record
      }
    })
    
    output$selectedTaxon <- renderText({
      req(itis_record(), itis_record())
      paste(
        paste('You have selected Taxonomic Serial Number:', tsn()),
        paste('Scientific Name:', itis_record()$scientificName$combinedName),
        paste('Author:', itis_record()$scientificName$author),
        paste('Taxon Rank:', trimws(itis_record()$taxRank$rankName)),
        paste('Taxon Status:', itis_record()$coreMetadata$taxonUsageRating),
        sep = '\n'
      )
    })
    
    output$taxonCommonNames <- renderReactable({
      req(itis_record(), itis_record())
      if (any(!is.na(itis_record()$commonNameList$commonNames))) {
        reactable(
          itis_record()$commonNameList$commonNames[,c('commonName', 'language')],
          pagination = FALSE
        )
      } else {
        reactable(data.frame(commonName = character(0), language = character(0)))
      }
    })
    
    output$taxonHierarchy <- renderReactable({
      req(itis_record(), itis_record())
      taxonTree <- ritis::hierarchy_full(tsn())[,c('rankname', 'taxonname')]
      reactable(
        taxonTree[1:which(taxonTree$rankname == trimws(itis_record()$taxRank$rankName)),],
        pagination = FALSE
      )
    })
    
    observeEvent(input$addTaxon, {
      req(itis_record(), itis_record())
      rs <- AMMonitor::taxaAdd(
        con = con(),
        tsns = tsn(), 
        common_names = input$commonName,
        overwrite = input$overwrite
      )
      if (rs$status == 'success') {
        success_message <- paste0(
          'The new taxon (tsn =',
          rs$tsn,
          ') was successfully added'
        )
        taxonAddResults(success_message)
        showModal(modalDialog(
          success_message,
          title = 'New taxon successfully added',
          footer = modalButton("Dismiss"),
          easyClose = FALSE,
          fade = TRUE
        ))
      } else {
        showModal(modalDialog(
          paste0(
            'The new taxon (tsn =',
            rs$tsn,
            ') failed to be added:',
            rs$reason
          ),
          title = 'New taxon addition failed',
          footer = modalButton("Dismiss"),
          easyClose = FALSE,
          fade = TRUE
        ))
      }
    })
    
    return(
      reactiveValues(
        taxonAddResults = reactive(taxonAddResults())
      )
    )
  })
}
