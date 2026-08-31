
my_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    wellPanel(
      id = "welcome",
      style = "background: lightblue",
      shiny::tags$i(
        "Welcome to AMMonitor! Please select a valid person ID to sign in."
      )
    ),
    textInput(
      ns('selectedUser'),
      'Select user',
      value = ifelse(
        test = file.exists(paste0(ammPath, '/settings/default_user.txt')),
        yes = read.csv(paste0(ammPath, '/settings/default_user.txt'), header = F)[,],
        no = ""
      )
    ),
    actionButton(
      ns('connect2db'),
      'Connect to database'
    ),
    tags$br(),
    tags$br(),
    wellPanel(
      uiOutput(ns('db_info'))
    )
  ) #end tagList
} #end ui function

my_home_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      con <<- reactiveVal(NA)
      onStop(function() {
        cat("Closing Database Connections")
        isolate({
          if (isS4(con())) {
            DBI::dbDisconnect(con())
          }
          rm(con, envir = .GlobalEnv)
        })
      })
      
      user_first_name <- reactiveVal(character())
      
      observeEvent(input$connect2db, {
        #connect to database
        dbPath <- paste(ammPath, 'database', dir(paste(ammPath, 'database', sep = '/'), pattern = ".sqlite"), sep = '/')
        
        if (file.exists(dbPath)) {
          con(DBI::dbConnect(RSQLite::SQLite(), dbname = dbPath))
          
          user_first_name(
            dbGetQuery(
              con(), 
              paste0("SELECT first_name FROM people WHERE pk_personid = '", input$selectedUser, "';")
            )[,]
          )
          
          if (length(user_first_name()) != 0) {
            # Turn the SQLite foreign constraints on
            RSQLite::dbExecute(conn = con(), statement = "PRAGMA foreign_keys = ON;")
            
            shinyjs::disable(id = 'selectedUser')
            shinyjs::hide(id = 'connect2db')
            
            # Load dictionary and shinytable for DB front end
            dictionary <<- dbGetQuery(
              con(),
              'SELECT * FROM dbdictionary ORDER BY pk_tablename, sort_order, pk_fieldname NULLS LAST;'
            )
            
            shiny_table <<- dbGetQuery(con(), "SELECT * FROM shinytable;")
            
            # Make taxon names readily available
            taxon_names <<- dbGetQuery(con(), 'SELECT * FROM taxa ORDER BY pk_taxonid;')
            
            mediatags <- AMMonitor::qryTags(con(), media = TRUE)
            
            non_taxa_label_options <<- unique(mediatags[c("fk_medialistid", "photos", "recordings", "videos", "list_type")])
            names(non_taxa_label_options)[1] <<- "pk_medialistid"
            
            medialistitems <<- subset(mediatags, calculated == 0)
            
            taxontags <- AMMonitor::qryTags(con())
            
            taxa_label_options <<- unique(
              taxontags[
                which(taxontags$taxa_list != 1), 
                c("fk_librarylistid", "photos", "recordings", "videos", 
                  "list_type", "fk_taxonid", "fk_child_librarylistid")
              ]
            )
            names(taxa_label_options)[1] <<- "pk_librarylistid"
            
            librarylistitems <<- taxontags
            
            onStop(function() {
              rm(
                list = c('dictionary', 'librarylistitems', 'medialistitems', 'non_taxa_label_options', 'shiny_table', 'taxa_label_options', 'taxon_names'),
                envir = .GlobalEnv
              )
            })
            
          } else {
            dbDisconnect(con())
            con(NA)
            showModal(modalDialog(
              title = 'Database Error',
              'Invalid User.', 
              easyClose = TRUE,
              footer = modalButton("Cancel")
            ))
          }
        } else {
          showModal(modalDialog(
            title = 'Database Error',
            'No database file found via the current working directory.', 
            easyClose = TRUE,
            footer = modalButton("Cancel")
          ))
        }
      })
      
      output$db_info <- renderUI({
        if (!isS4(con())) {
          tags$i('Please connect to the database to continue.')
        } else {
          home_metadata <- dbGetQuery(
            con(), 
            "SELECT 'nlocs' AS ct_type, ROUND(COUNT(pk_locationid)) AS cts 
              FROM locations WHERE pk_locationid != 'unknownLocation' 
            UNION SELECT 'nphoto' AS ct_type, ROUND(COUNT(pk_mediaid)) AS cts 
              FROM media WHERE media_type = \'photo\' 
            UNION SELECT 'naudio' AS ct_type, ROUND(COUNT(pk_mediaid)) AS cts 
              FROM media WHERE media_type = \'audio\';"
          )
          tagList(
            wellPanel(
              tags$h2(paste0(
                'Hello, ', 
                user_first_name(),
                ', you are now signed into the database!'
              )),
              fluidRow(
                column(4, wellPanel(
                  paste('Locations:', home_metadata$cts[home_metadata$ct_type == "nlocs"])
                )),
                column(4, wellPanel(
                  paste('Photos:', home_metadata$cts[home_metadata$ct_type == "nphoto"])
                )),
                column(4, wellPanel(
                  paste('Recordings:', home_metadata$cts[home_metadata$ct_type == "naudio"])
                ))
              )
            )
          )
        }
      })
      
      return(
        reactiveValues(
          selectedUser = reactive(input$selectedUser)
        )
      )
    } #end moduleServer function
  ) #end moduleServer
} #end server function
