audio_tools_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h2('About AMMonitor Audio Tools'),
    tags$p(
      'This is where you can interact with your recordings These tools are geared around playing, adding, modifying, and verifying recording “labels”. Used here, labels refers to human-created labels (annotations, annotags, and mediatags) and model-created labels (modeloutputs). 
'
    ),
    tags$ul(
      tags$li(
        tags$b('Player'), '- The player lets a user play recordings and view spectrograms, along with any labels created by any user.'
      ),
      tags$li(
        tags$b('Tagger'), '- The tagger allows users to add new human-created labels (annotations, annotags, and mediatags). When in “tagger” mode, you can only see labels that you’ve created.'
      ),
      tags$li(
        tags$b('Annotation Verifier'), '- The annotation verifier allows a user to verify all human-created labels (annotations, annotags, and mediatags) created by other users (you can’t verify your own labels).' 
      ),
      tags$li(
        tags$b('Model Verifier'), '- The model verifier allows users to verify model-based labels (modeloutputs).'
      )
    ),
    tags$h3('Cheat Sheets'),
    tags$a("Aduio Tools Cheat Sheet", href = "Audio-Cheat-Sheets.pdf", target = "_blank"),
    tags$h3('Video Tutorials'),
    tags$a("Audio Player", href = "https://www.youtube.com/watch?v=Xqj7KjLv5Vg", target = "_blank"), tags$br(),
    tags$a("Audio Tagger", href = "https://www.youtube.com/watch?v=-zxi2EI0TzA", target = "_blank"), tags$br(),
    tags$a("Audio Verifiers", href = "https://www.youtube.com/watch?v=43BuqpJBAbA", target = "_blank")
  )
}

audio_tools_about_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}