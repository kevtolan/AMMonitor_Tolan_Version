#!! ModName = selectMedia
#!! ModDisplayName = Select a visit from the visits table.
#!! ModDescription = Select the pk_visitid for row from the visits table.
#!! ModCitation = Laurence Clarfeld.  (2023). selectMedia. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionReturn = mediaType !! Media type (photos/recordings) !! character
#!! FunctionReturn = mediaIDs !! Primary key values for selected media !! character


# the ui function
selectMedia_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$h3('Step 1: Select Media Type'),
      selectizeInput(
        ns('mediaType'),
        'Media Type',
        choices = c('photos', 'recordings'),
        options = list(
          placeholder = 'Media Type',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    fluidRow(
      tags$h3('Step 2: Select a Location'),
      selectizeInput(
        ns('fk_locationid'),
        'Location ID',
        choices = dbGetQuery(con(), 'SELECT pk_locationid FROM locations;')[,],
        #         choices = dbGetQuery(
        #           con(),
        #           paste0(
        #             "SELECT visits.fk_locationid, equipmodels.equip_type
        # FROM (visits INNER JOIN equipment ON visits.fk_equipmentid = equipment.pk_equipmentid) INNER JOIN equipmodels ON equipment.fk_equipmodelid = equipmodels.pk_equipmodelid
        # WHERE (((equipmodels.equip_type)='",
        #             switch(input$mediaType, 'photos' = 'camera', 'recordings' = 'recorder')
        #             , "'));"
        #           )
        #         ),
        options = list(
          placeholder = 'Visit location',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    fluidRow(
      tags$h3('Step 3: Select a Visit'),
      fluidRow(
        reactableOutput(ns('visitChoices'))
      )
    ),
    fluidRow(
      tags$h3('Step 4: Select media'),
      tags$i('Note: All media files are selected by default'),
      fluidRow(
        reactableOutput(ns('mediaChoices')),
        textOutput(ns('nSelected'))
      )
    )
  )
}


# the server function
selectMedia_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(input$mediaType)
      updateSelectInput(
        session = session,
        'fk_locationid',
        'Location ID',
        choices = dbGetQuery(
          con(),
          paste0(
            "SELECT visits.fk_locationid 
            FROM visits INNER JOIN equipment ON visits.fk_equipmentid = equipment.pk_equipmentid 
WHERE equipment.equip_type='",
            switch(input$mediaType, 'photos' = 'camera', 'recordings' = 'recorder')
            , "';"
          )
        )[,]
      )
    })
    
    visit_options <- reactive({
      tryCatch(
        dbGetQuery(
          con(),
          paste0(
            "SELECT DISTINCT(pk_visitid), visit_date, visit_time, fk_locationid, fk_equipmentid, visit_type ",
            "FROM visits INNER JOIN media ",
            "ON visits.pk_visitid = media.fk_visitid ",
            "WHERE fk_locationid = '", input$fk_locationid, "' ",
            "ORDER BY visit_date, visit_time;"
          )
        ),
        error = function(cond) {
          data.frame(pk_visitid = character(0))
        }
      )
    })
    
    media_options <- reactive({
      req(input$visitChoices__reactable__selected)
      tryCatch(
        dbGetQuery(
          con(),
          paste0(
            'SELECT pk_mediaid, start_date, start_time', 
            ' FROM media',
            ' WHERE fk_visitid = ', 
            visit_options()$pk_visitid[input$visitChoices__reactable__selected],
            ' ORDER BY start_date, start_time;'
          )
        ),
        error = function(cond) {
          data.frame(pk_mediaid = character(0))
        }
      )
    })
    
    
    output$visitChoices <- renderReactable({
      req(visit_options())
      reactable(
        visit_options(),
        selection = "single",
        onClick = "select"
      )
    })
    
    output$mediaChoices <- renderReactable({
      req(media_options())
      reactable(
        media_options(),
        selection = "multiple",
        defaultSelected = seq_len(nrow(media_options())),
        onClick = "select"
      )
    })
    
    output$nSelected <- renderText({
      req(input$mediaChoices__reactable__selected)
      paste(
        length(input$mediaChoices__reactable__selected), 
        input$mediaType, 
        'selected'
      )
    })
    
    return(
      reactiveValues(
        mediaType = reactive(input$mediaType),
        mediaIDs = reactive(media_options()[input$mediaChoices__reactable__selected,])
      )
    )
  })
}
