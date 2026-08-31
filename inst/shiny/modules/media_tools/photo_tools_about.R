photo_tools_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h2('About AMMonitor Photo Tools'),
    tags$p(
      'This is where you can interact with your photos. These tools are geared around viewing, adding, modifying, and verifying image “labels”. Used here, labels refers to human-created labels (annotations, annotags, and mediatags) and model-created labels (modeloutputs). 
'
    ),
    tags$ul(
      tags$li(
        tags$b('Viewer'), '– The viewer lets a user see photos along with any labels created by any user.'
      ), 
      tags$li(
        tags$b('Tagger'), '– The tagger allows users to add new human-created labels (annotations, annotags, and mediatags). When in “tagger” mode, you can only see labels that you’ve created.'
      ), 
      tags$li(
        tags$b('Annotation Verifier'), '– The annotation verifier allows a user to verify all human-created labels (annotations, annotags, and mediatags) created by other users (you can’t verify your own labels).'
      ),
      tags$li(
        tags$b('Model Verifier'), '– The model verifier allows users to verify model-based labels (modeloutputs).'
      )
    ),
    tags$h3('Cheat Sheets'),
    tags$a("Photo Tools Cheat Sheet", href = "Photo-Cheat-Sheets.pdf", target = "_blank"),
    tags$h3('Video Tutorials'),
    tags$a("Photo Viewer", href = "https://www.youtube.com/watch?v=bNlmGHEx3do", target = "_blank"), tags$br(),
    tags$a("Photo Tagger", href = "https://www.youtube.com/watch?v=wz6KvP6d2iI", target = "_blank"), tags$br(),
    tags$a("Photo Verifiers", href = "https://www.youtube.com/watch?v=IuTTQle2Jh0", target = "_blank")
  )
}

photo_tools_about_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )
}