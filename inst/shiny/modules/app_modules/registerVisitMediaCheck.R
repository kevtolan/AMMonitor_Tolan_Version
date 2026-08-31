#!! ModName = registerVisitMediaCheck
#!! ModDisplayName = Check Media Files
#!! ModDescription = Check media files for any potential issues
#!! ModCitation = Laurence Clarfeld.  (2023). registerVisitMediaCheck. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = mediaType !! Media type (photos/recordings/videos) !! character
#!! FunctionArg = visitMetadata !! Metadata for the new visit record !! data.frame
#!! FunctionArg = selectedFiles !! File paths for media to be registered !! Character
#!! FunctionArg = audio_fn_format !! File naming format for audio files !! Character
#!! FunctionReturn = screenData !! status, warnings, and exif data !! list
#!! FunctionReturn = includeLogFile !! Whether to add a log file !! logical


# the ui function
registerVisitMediaCheck_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns('getWarnings'), 'Generate warnings'),
    reactable::reactableOutput(ns('warningsTable')),
    checkboxInput(
      ns('confirmWarnings'), 
      label = 'Create a log file with warnings to be associated with the visit',
      value = TRUE
    ), 
  )
}


# the server function
registerVisitMediaCheck_server <- function(id, mediaType, visitMetadata, selectedFiles, audio_fn_format) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$confirmWarnings, {
      if (input$confirmWarnings) {
        if (!dir.exists(paste(ammPath, 'logs', sep = '/'))) {
          showModal(modalDialog(
            title = "Log Path Not Found",
            "The current working directory does not contain a \"logs\" subdirectory. Make sure your current working directory is properly set to an AMMonitor project directory and create the \"logs\" folder, if needed. Then, try agian.",
            easyClose = TRUE,
            footer = NULL
          ))
          updateCheckboxInput(
            session, 
            'confirmWarnings', 
            label = 'Create a log file with warnings to be associated with the visit',
            value = FALSE
          )
        }
      }
    })
    
    screen_data <- eventReactive(input$getWarnings, {
      screenNewVisit(
        visitMetadata = visitMetadata(),
        visitID = NA,
        mediaType = mediaType(),
        selectedFiles = selectedFiles(),
        audio_fn_format = audio_fn_format(),
        con = con()
      )
    })
    
    output$warningsTable <- reactable::renderReactable({
      reactable::reactable(
        screen_data()$warnings[
          order(screen_data()$warnings$severity, decreasing = TRUE),
        ],
        columns = list(severity = reactable::colDef(
          style =  reactable::JS(
            "function(rowInfo) {
              if (rowInfo.values['severity'] == 3) {
                return { backgroundColor: 'red', color: 'black', fontWeight: 600}
              } else if (rowInfo.values['severity'] == 2) {
                return { backgroundColor: 'orange', color: 'black', fontWeight: 600 }
              } else {
                return { backgroundColor: 'yellow', color: 'black', fontWeight: 600 }
              }
            }"
          )
        ))
      )
    })
    return(
      reactiveValues(
        screenData = reactive(screen_data()),
        includeLogFile = reactive(input$confirmWarnings)
      )
    )
  })
}
