#!! ModName = survey_upload
#!! ModDisplayName = Survey Upload
#!! ModDescription = Upload a Survey123 Excel form
#!! ModCitation = Tang, Caroline.  (2023). survey_upload. [Source code].
#!! ModActive = 1
#!! FunctionReturn = survey_qs !! The survey sheet of the Excel form !! data.frame
#!! FunctionReturn = survey_choices !! The choices sheet of the Excel form !! data.frame
#!! Package = reactable !! 0.4.3 !! notes
#!! Package = readxl !! 1.4.3 !! 


# the ui function
survey_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      "Welcome to the Survey123 form comparison tool. 
      Please note that this is only a tool to compare the spreadsheet form to the
      relevant parts of the database, and changes must be made manually within 
      the Excel file."
    ),
    fileInput(
      inputId = ns('survey_file'),
      label = "Upload a Survey123 Excel Form:",
      multiple = FALSE,
      accept = ".xlsx"
    ),
    h4("Preview File:"),
    reactableOutput(
      outputId = ns('survey_qs')
    )
  )
}


# the server function
survey_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    survey_qs <- reactive({
      req(input$survey_file)
      file <- input$survey_file
      readxl::read_excel(file$datapath, sheet = "survey")
    })
    
    output$survey_qs <- renderReactable({
      req(survey_qs())
      reactable(survey_qs(), sortable = FALSE)
    })
    
    survey_choices <- reactive({
      req(input$survey_file)
      file <- input$survey_file
      readxl::read_excel(file$datapath, sheet = "choices")
    })
    
    return(
      reactiveValues(
        survey_qs = reactive(survey_qs()),
        survey_choices = reactive(survey_choices())
      )
    )
  })
}
