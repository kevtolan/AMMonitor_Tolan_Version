my_db_ui <- function(id) {
  tagList(
    wellPanel(
      id = "database",
      style = "background:lightblue",
      tags$h5("Welcome to the AMMonitor database!")
    ), #end wellPanel
    do.call(
      tabsetPanel,
      c(
        id = NS(id, "main_tab"),
        type = "tabs",
        lapply(
          unique(shiny_table$primary_tab),
          FUN = function(X) {
            X_id <- gsub(" ", "_", X)
            tabPanel(
              title = X,
              do.call(
                tabsetPanel,
                c(
                  id = NS(id, paste0(X_id, "_tabgroup")),
                  type = "tabs",
                  lapply(
                    X = shiny_table$fk_tablename[which(gsub(" ", "_", shiny_table$primary_tab) == X_id)],
                    FUN = function(X) {
                      tabPanel(
                        title = X,
                        value = X,
                        uiOutput(NS(id, paste0(X, "_output")))
                      )
                    }
                  ) #end inner lapply
                )
              ) #end inner tabSetPanel
            ) #end primary tabPanel
          }
        ) #end outer lapply
      ) #end arguments
    )
  ) #end tagList
} #end ui function

my_db_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #creating content for each tab
      observe({
        req(con())
        lapply(
          X = shiny_table$fk_tablename,
          FUN = function(X) {
            output[[paste0(X, "_output")]] <- renderUI({table_ui(X)})
          }
        )
      })
      
      return(reactiveValues(
        db_tab_selected = input$main_tab
      ))
    } #end moduleServer function
  ) #end moduleServer
} #end server function
