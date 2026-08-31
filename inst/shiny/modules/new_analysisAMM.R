
new_analysisAMM_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("apps_available")),
    shinyjs::disabled(actionButton(
      ns('start_new_analysis'),
      'Select New App'
    )),
    actionButton(
      ns('reset_new_analysis'),
      'Reset App'
    ),
    wellPanel(
      tags$div(id = 'app_ui_goes_here')
    ),
    reactable::reactableOutput(ns('appsTable')),
  )
}

new_analysisAMM_server <- function(id, tabSelect) {
  moduleServer(id, function(input, output, session) {
    ns_ct <- reactiveVal(1)
    ns <- session$ns
    
    # Get a list of available apps (from the directory contents)
    apps_list <- sort(tools::file_path_sans_ext(list.files(paste(find.package('AMMonitor', lib.loc = .libPaths()), 'shiny/modules/apps', sep = '/'))))
    
    # Get the apps info
    apps_info <- read.csv('www/apps.csv')
    apps_info <- apps_info[apps_info$appName %in% apps_list,] # Subset to only apps that have scripts
    apps_info <- apps_info[order(apps_info$group, apps_info$appName),] # Re-order rows
    
    # Making sure buttons are enabled properly, when required
    needs_reset <- reactiveValues(reset = reactive(TRUE))
    observe({
      if (needs_reset$reset()) {
        shinyjs::enable('select_analysis')
        shinyjs::enable('start_new_analysis')
        shinyjs::show('appsTable')
        needs_reset$reset <- reactive(FALSE)
      }
    })
    
    output$appsTable <- reactable::renderReactable({
      reactable::reactable(
        apps_info,
        filterable = TRUE,
        selection = 'single',
        onClick = 'select'
      )
    })
    
    #  Render selectizeInput for choosing which analysis to launch
    output$apps_available <- renderUI({
      selectizeInput(
        ns("select_analysis"),
        "Select an app",
        choices = apps_list,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = 'Select an app to run.', maxItems = 1)
      )
    })
    
    observeEvent(input$appsTable__reactable__selected, {
      updateSelectizeInput(
        session,
        'select_analysis',
        'Select an app',
        choices = apps_list,
        selected = apps_info$appName[input$appsTable__reactable__selected],
        options = list(placeholder = 'Select an app to run.', maxItems = 1)
      )
    })

    observeEvent(input$start_new_analysis, {
      
      req(input$select_analysis)
      
      shinyjs::disable('start_new_analysis')
      shinyjs::disable('select_analysis')
      shinyjs::hide('appsTable')
      
      # Insert the UI for the App
      insertUI(
        selector = '#app_ui_goes_here',
        where = "beforeEnd",
        ui = tags$div(
          id = 'app_ui',
          tagList(
            eval(
              parse(
                text = paste0(
                  input$select_analysis,
                 '_ui(ns("mod_',
                  ns_ct(),
                  '"))'
                )
              )
            )
          )
        )
      )
  
      # Run the server for the App
      eval(
        parse(
          text = paste0(
            'rslt <- ',
            input$select_analysis,
            '_server("mod_',
            ns_ct(),
            '")'
          )
        )
      )
    
      ns_ct(ns_ct() + 1) # Increment namespace counter
      
    })
    
    # Reset the analysis
    observeEvent(input$reset_new_analysis, {
      req(input$start_new_analysis)
      needs_reset$reset <- reactive(TRUE)
      
      removeUI(
        selector = '#app_ui',
        session = session
      )
    })

    # Needed to re-enable buttons on tab change before "start" button clicked once
    observeEvent(tabSelect(), {
      if (!is.null(input$start_new_analysis) && input$start_new_analysis == 0) {
        needs_reset$reset <- reactive(TRUE)
      }
    })
  })
}
