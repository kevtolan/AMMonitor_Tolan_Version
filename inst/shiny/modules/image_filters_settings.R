image_filters_settings_ui <- function(id, viewer_mode) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        5,
        wellPanel(
          tags$h3('Visit Filters'),
          shiny::tags$br(),
          shiny::tags$i('Select a location to show visits'),
          reactable::reactableOutput(ns('filterVisitTable'))
        ),
        wellPanel(
          tags$h3('Jump to photo'),
          span(textOutput(ns('truncated_list')), style="color:red"),
          tags$br(),
          selectInput(
            ns('goto_photo'),
            'Select Photo:',
            choices = NULL
          )
        )
      ),
      column(
        3,
        wellPanel(
          tags$h3('Image Filters'),
          selectInput(
            ns('filterLocation'),
            'Select a location',
            choices = 'all'
          ),
          # Check this alternative out: https://rdrr.io/cran/shinyWidgets/man/airDatepicker.html
          dateRangeInput(
            ns('filterDateRange'),
            'Select date range (default includes all dates)',
            start = '1900-01-01',
            end = '2100-01-01',
            min = '1900-01-01',
            max = '2100-01-01',
            startview = 'year'
          ),
          selectInput(
            ns('filterTaxa'),
            'Select taxa',
            choices = c('all')
          )
        )
      ),
      column(
        2,
        wellPanel(
          shiny::tags$h3('Annotation Filters'),
          checkboxInput(
            ns('showAnnotated'),
            'Show annotated images',
            value = 1
          ),
          checkboxInput(
            ns('showUNAnnotated'),
            'Show un-annotated images',
            value = 1
          )
        )
      ),
      column(
        2,
        wellPanel(
          shiny::tags$h3('Photo Settings'),
          textInput(
            ns('imagePathURL'),
            'Image directory path/URL'
          ),
          selectInput(
            ns('dim'),
            'Image size',
            choices = c(600,800,1000,1200), 
            selected = '800'
          )
        ),
        switch(
          viewer_mode,
          'tagger' = wellPanel(
            shiny::tags$h3('Tagger Settings'),
            numericInput(
              ns('autosave_rate'),
              'Auto-save Rate (# of images)',
              value = 50, 
              min = 1,
              step = 1
            )
          ),
          'viewer' = character(0)
        )
      )
    ),
    textOutput(ns('num_photos'))
  )
}
  
image_filters_settings_server <- function(id, selectedUser = NA) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filtered dataframe of available photos
    photos_avail <- reactive({
      filter_photos(
        con,
        ifelse(is.null(input$filterLocation), 'all', input$filterLocation),
        ifelse(
          is.null(input$filterDateRange), 
          list(c(
            dbGetQuery(con, 'SELECT MIN(startDate) FROM photos;'), 
            dbGetQuery(con, 'SELECT MAX(startDate) FROM photos;')
          )), 
          list(input$filterDateRange)
        ),
        visitID(),
        ifelse(is.null(input$filterTaxa), 'all', input$filterTaxa),
        input$showAnnotated,
        input$showUNAnnotated
      )
    })
    
    
    
  })
}
