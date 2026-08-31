#!! ModName = shiftMediaDateTime
#!! ModDisplayName = Shift Media Date/Time.
#!! ModDescription = Shift date/time of selected media by a specified amount.
#!! ModCitation = Laurence Clarfeld.  (2023). shiftMediaDateTime. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = mediaType !! Media type (photos/recoredings) !! character
#!! FunctionArg = mediaIDs !! IDs of selected media items !! character
#!! FunctionReturn = shiftMinutes !! Duration of time shift (minutes) !! numeric
#!! FunctionReturn = status !! Was the time change applied? !! logical


# the ui function
shiftMediaDateTime_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      numericInput(
        ns('shiftMinutes'), 
        label = 'Minutes time shift', 
        value = 0,
        step = 1
      )
    ),
    fluidRow(
      reactableOutput(ns('mediaDates'))
    ),
    tags$br(),
    wellPanel(
      style = "background: aquamarine",
      tags$i('Note: this action will update the date/times of the specified media files in your database:'),
      actionButton(
        ns('updateDateTimes'), 
        "Update Date/Time", 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )
  )
}


# the server function
shiftMediaDateTime_server <- function(id, mediaType, mediaIDs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mediaDateChangeTable <- reactive({
      req(input$shiftMinutes)
      oldDates <- as.POSIXct(paste(mediaIDs()$start_date, mediaIDs()$start_time))
      newDates <- oldDates + input$shiftMinutes*60
      data.frame(
        mediaID = mediaIDs()$pk_mediaid,
        oldDate = mediaIDs()$start_date,
        oldTime = mediaIDs()$start_time,
        newDate = format(newDates, '%Y-%m-%d'),
        newTime = format(newDates, '%H:%M:%S')
      )
    })
    
    output$mediaDates <- renderReactable({
      req(mediaIDs())
      reactable(
        mediaDateChangeTable()
      )
    })
    
    observeEvent(input$updateDateTimes, {
      req(mediaIDs())
      if (!is.na(input$shiftMinutes) && input$shiftMinutes != 0 && nrow(mediaIDs()) != 0) {
        tryCatch(
          {
            # Update date/time for all relevant records from the media table
            sapply(
              seq_len(nrow(mediaIDs())),
              function(x) {
                params <- list(
                  mediaDateChangeTable()$newDate[x], 
                  mediaDateChangeTable()$newTime[x],
                  mediaIDs()$pk_mediaid[x]
                )
                rs <- dbSendStatement(
                  con(), 
                  "UPDATE media 
                  SET start_date = $1,
                  start_time = $2 
                  WHERE pk_mediaid = $3;",
                )
                dbBind(rs, params)
                dbClearResult(rs)
              }
            )
            
            # Make a note in the associated visit
            visitID <- dbGetQuery(
              con(), 
              paste0(
                'SELECT fk_visitid FROM media',
                ' WHERE pk_mediaid = ', mediaIDs()[1,'pk_mediaid'], ';'
              )
            )[,]
            visitNote <- dbGetQuery(con(), paste('SELECT visit_notes FROM visits WHERE pk_visitid =', visitID, ';'))[,]
            rs <- dbSendQuery(
              con(),
              paste0(
                'UPDATE visits SET visit_notes = ',
                ifelse(
                  is.na(visitNote) || visitNote == "",
                  '"',
                  paste0('"', visitNote, '" || "; ')
                ),
                'date/time for ', nrow(mediaIDs()), ' ', mediaType(), ' shifted by ', 
                input$shiftMinutes, ' minutes"',
                ' WHERE pk_visitid = ', visitID, ';'
              )
            )
            dbClearResult(rs)
          },
          error = function(cond) {stop('Error in updating date/time')}
        )
        showModal(modalDialog(
          paste(nrow(mediaIDs()), mediaType(), 'updated.'),
          tags$br(), 
          paste0(
            'A note of this update had been made for the associated visit (visitID=',
            visitID,
            ').'
          ),
          tags$br(), 
          'If you wish to save a record of which files were modified, click "next" below. To make further date/time changes, press the "Reset analysis" button above.',
          title = 'Date/Time Update Complete',
          footer = modalButton("Dismiss"),
          easyClose = TRUE,
          fade = TRUE
        ))
        shinyjs::disable('updateDateTimes')
      }
    })
    
    
    return(
      reactiveValues(
        shiftMinutes = reactive(input$shiftMinutes),
        status = reactive(as.logical(input$updateDateTimes))
      )
    )
  })
}
