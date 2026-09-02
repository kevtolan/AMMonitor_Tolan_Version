#!! ModName = registerVisitMediaType
#!! ModDisplayName = Media Type
#!! ModDescription = Select a media type from photo/recording/video.
#!! ModCitation = Laurence Clarfeld.  (2023). registerVisitMediaType. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionReturn = mediaType !! Media Type !! Character


# the ui function
registerVisitMediaType_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns('mediaType'),
      'Select Media Type',
      choices = c('photo', 'audio'),
      selected = 'audio'
    )
  )
}


# the server function
registerVisitMediaType_server <- function(id, argName1, argName2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    return(
      reactiveValues(
        mediaType = reactive(input$mediaType)
      )
    )
  })
}
