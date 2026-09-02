#!! ModName = registerVisitMetadata
#!! ModDisplayName = Enter Visit MetaData
#!! ModDescription = Visit metadata entered here, associated with media registration.
#!! ModCitation = Laurence Clarfeld.  (2023). registerVisitMetadata. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = mediaType !! Media Type (photos/recordings/videos) !! Character
#!! FunctionReturn = visitMetadata !! returnDescription !! returnClass

# the ui function
registerVisitMetadata_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      wellPanel(
        tags$p("To add a new visit (with or without media), enter the new visit metadata below. To add media to an existing visit, select a Visit ID to associate with your new media files."),
        selectizeInput(
          ns('pk_visitid'),
          'Select existing visit',
          choices = character(0),
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      wellPanel(
        htmlOutput(ns('visit_metadata_header')),
        selectizeInput(
          ns('fk_locationid'),
          'Location ID',
          choices = dbGetQuery(con(), 'SELECT pk_locationid FROM locations;')[,],
          options = list(
            placeholder = 'Visit location',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        fluidRow(
          column(
            3,
            dateInput(
              ns('visit_date'),
              'Visit Date'
            )
          ),
          column(
            3,
            selectizeInput(
              ns('hours'),
              'Time (hours)',
              choices = c(paste0('0', 0:9), 10:23)
            )
          ),
          column(
            3,
            selectizeInput(
              ns('minutes'),
              'Time (minutes)',
              choices = c(paste0('0', 0:9), 10:59)
            )
          )
        ),
        fluidRow(
          uiOutput(ns('equipmentUI'))
        ),
        fluidRow(
          selectizeInput(
            ns('fk_personid'),
            'User ID',
            choices = dbGetQuery(con(), 'SELECT pk_personid FROM people;')[,],
            options = list(
              placeholder = 'Select the person who conducted the visit',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        fluidRow(
          tags$div(
            title = paste(
              "Set - When equipment is initially deployed",
              "Check - Checking deployed equipment that stays deployed",
              "Pull - Checking equipment that is then pulled (includes lost/stolen)",
              sep = "\n"
            ),
            selectizeInput(
              ns('visit_type'),
              'Visit Type',
              choices = c('set', 'check', 'pull'),
              options = list(
                placeholder = 'Select the visit type',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
        ),
        fluidRow(
          column(
            6,
            tags$h2('Enter additional visit metadata'),
            htmlOutput(ns('addVisit'))
          ),
          column(
            6,
            wellPanel(
              tags$h2('Visit Fields'),
              tags$i('All available definitions for additional visit fields are provided for reference.'),
              tableOutput(ns('visitFields'))
            )
          )
        )
      )
    )
  )
}


# the server function
registerVisitMetadata_server <- function(id, mediaType) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Basic fields (server) ---------------
    
    visit_time <- reactive({
      paste(input$hours, input$minutes, "00", sep = ":")
    })
    
    output$visit_metadata_header <- renderUI({
      if(input$pk_visitid == "") {
        tags$h1('Add New Visit Metadata')
      } else {
        tags$h1('Existing Visit Metadata')
      }
    })
    
    observe({
      req(mediaType())
      
      existing_visits <- DBI::dbGetQuery(
        con(),
        paste0(
          "SELECT pk_visitid, fk_locationid, visit_date FROM visits INNER JOIN equipment ON visits.fk_equipmentid = equipment.pk_equipmentid WHERE equip_type IS NULL OR equip_type = 'cell phone' OR ",
          switch(
            mediaType(),
            'photo' = "equip_type IN ('camera', 'drone')",
            'audio' = "equip_type = 'recorder'"
          ),
          ";"
        )
      )

      updateSelectizeInput(
        session = session,
        inputId = 'pk_visitid',
        choices = setNames(
          existing_visits$pk_visitid,
          paste0(existing_visits$pk_visitid, " -- ", existing_visits$fk_locationid, " -- ", existing_visits$visit_date)
        )
      )
    })
    
    observeEvent(input$pk_visitid, {
      
      if (input$pk_visitid != "") {
        # Fetch visit data
        visit_metadata <- DBI::dbGetQuery(
          con(),
          paste0(
            "SELECT * FROM visits WHERE pk_visitid = ",
            input$pk_visitid,
            ";"
          )
        )
        
        # Update form fields
        updateSelectizeInput(
          session = session, 
          inputId = 'fk_locationid', 
          selected = visit_metadata$fk_locationid
        )
        shinyjs::disable('fk_locationid')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'fk_equipmentid', 
          selected = visit_metadata$fk_equipmentid
        )
        shinyjs::disable('fk_equipmentid')
        
        updateDateInput(
          session = session,
          inputId = 'visit_date',
          value = visit_metadata$visit_date
        )
        shinyjs::disable('visit_date')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'hours', 
          selected = substr(visit_metadata$visit_time, 1, 2)
        )
        shinyjs::disable('hours')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'minutes', 
          selected = substr(visit_metadata$visit_time, 4, 5)
        )
        shinyjs::disable('minutes')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'fk_personid', 
          selected = visit_metadata$fk_personid
        )
        shinyjs::disable('fk_personid')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'visit_type', 
          selected = visit_metadata$visit_type
        )
        shinyjs::disable('visit_type')
        
        # Add extra visit fields
        for (visit_field in fieldInfo$pk_fieldname[!fieldInfo$pk_fieldname %in% basicVisitFields]) {
          
          shinyinput_id <- paste0(id, '-new_', visit_field)
          shinyinput_type <- dictionary$shiny_input[dictionary$pk_fieldname == visit_field]
          
          shinyjs::disable(shinyinput_id)
          
          if (shinyinput_type %in% c("foreignKey", "dropdown")) {
            updateSelectizeInput(
              session = session,
              inputId = shinyinput_id,
              selected = visit_metadata[[visit_field]]
            )
          } else if (shinyinput_type == "text") {
            updateTextInput(
              session = session,
              inputId = shinyinput_id,
              value = visit_metadata[[visit_field]]
            )
          } else if (shinyinput_type == "longtext") {
            updateTextAreaInput(
              session = session,
              inputId = shinyinput_id,
              value = visit_metadata[[visit_field]]
            )
          } else if (shinyinput_type == "numeric") {
            updateNumericInput(
              session = session,
              inputId = shinyinput_id,
              value = visit_metadata[[visit_field]]
            )
          } else if (shinyinput_type == "checkbox") {
            updateCheckboxInput(
              session = session,
              inputId = shinyinput_id,
              value = visit_metadata[[visit_field]]
            )
          }
        }
      } else {
        # Update form fields
        updateSelectizeInput(
          session = session, 
          inputId = 'fk_locationid', 
          selected = character(0)
        )
        shinyjs::enable('fk_locationid')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'fk_equipmentid', 
          selected = character(0)
        )
        shinyjs::enable('fk_equipmentid')
        
        updateDateInput(
          session = session,
          inputId = 'visit_date',
          value = Sys.Date()
        )
        shinyjs::enable('visit_date')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'hours', 
          selected = "00"
        )
        shinyjs::enable('hours')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'minutes', 
          selected = "00"
        )
        shinyjs::enable('minutes')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'fk_personid', 
          selected = character(0)
        )
        shinyjs::enable('fk_personid')
        
        updateSelectizeInput(
          session = session, 
          inputId = 'visit_type', 
          selected = character(0)
        )
        shinyjs::enable('visit_type')
        
        # Add extra visit fields
        for (visit_field in fieldInfo$pk_fieldname[!fieldInfo$pk_fieldname %in% basicVisitFields]) {
          
          shinyinput_id <- paste0(id, '-new_', visit_field)
          shinyinput_type <- dictionary$shiny_input[dictionary$pk_fieldname == visit_field]
          
          shinyjs::enable(shinyinput_id)
          
          if (shinyinput_type %in% c("foreignKey", "dropdown")) {
            updateSelectizeInput(
              session = session,
              inputId = shinyinput_id,
              selected = character(0)
            )
          } else if (shinyinput_type == "text") {
            updateTextInput(
              session = session,
              inputId = shinyinput_id,
              value = ""
            )
          } else if (shinyinput_type == "longtext") {
            updateTextAreaInput(
              session = session,
              inputId = shinyinput_id,
              value = NA
            )
          } else if (shinyinput_type == "numeric") {
            updateNumericInput(
              session = session,
              inputId = shinyinput_id,
              value = NA
            )
          } else if (shinyinput_type == "checkbox") {
            updateCheckboxInput(
              session = session,
              inputId = shinyinput_id,
              value = 0
            )
          }
        }
      }
    })
    
    output$equipmentUI <- renderUI({
      req(mediaType())
      selectizeInput(
        ns('fk_equipmentid'),
        'Equipment ID',
        choices = DBI::dbGetQuery(
          con(), 
          paste0(
            "SELECT pk_equipmentid FROM ",
            "equipment WHERE equip_type IS NULL OR equip_type = 'cell phone' OR ",
            switch(
              mediaType(),
              'photo' = "equip_type IN ('camera', 'drone')",
              'audio' = "equip_type = 'recorder'"
            ),
            ";"
          )
        )[,],
        selected = "0001",
        options = list(
          placeholder = 'Select the equipment ID',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    })
    
    # Additional fields (server) ---------------
    dictionary <- DBI::dbGetQuery(
      conn = con(),
      statement = paste0(
        "SELECT * FROM dbdictionary 
        WHERE pk_tablename = 'visits' 
        ORDER BY sort_order NULLS LAST;"
      )
    )
    
    fieldInfo <- dbGetQuery(
      con(),
      'SELECT pk_fieldname, description 
      FROM dbdictionary 
      WHERE pk_tablename = "visits" 
      ORDER BY pk_fieldname;'
    )
    
    basicVisitFields <- c(
      'pk_visitid', 
      'fk_personid', 
      'fk_locationid', 
      'fk_equipmentid', 
      'visit_type', 
      'visit_date', 
      'visit_time'
    )
    
    output$visitFields <- renderTable({
      fieldInfo[!fieldInfo$pk_fieldname %in% basicVisitFields, ]
    })
    
    output$addVisit <- renderUI({
      the_modal <- getModalUI('visits', session$ns(id), dictionary, con(), FALSE)
      the_modal[! dbListFields(con(), 'visits') %in% basicVisitFields]
    })
    
    visitMetadata <- reactive({
      vmd <- list()
      for (visitField in names(input)[startsWith(names(input), paste0(id, '-new'))]) {
        vmd[[strsplit(visitField, '-new_')[[1]][2]]] <- ifelse(
          input[[visitField]] == "",
          NA,
          input[[visitField]]
        )
      }
      vmd[['visit_time']] <- visit_time()
      vmd[['visit_date']] <- as.character(input$visit_date)
      vmd[['fk_personid']] <- input$fk_personid
      vmd[['fk_locationid']] <- input$fk_locationid
      vmd[['fk_equipmentid']] <- input$fk_equipmentid
      vmd[['visit_type']] <- input$visit_type
      vmd[['pk_visitid']] <- input$pk_visitid
      as.data.frame(vmd)
    })
    
    return(
      reactiveValues(
        visitMetadata = reactive(visitMetadata())
      )
    )
  })
}
