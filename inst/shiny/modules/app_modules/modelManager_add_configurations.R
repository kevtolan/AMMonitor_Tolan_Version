#!! ModName = modelManager_add_configurations
#!! ModDisplayName = Model Manager: Add Model Configurations.
#!! ModDescription = Add, edit and delete (ML) model configurations.
#!! ModCitation = Laurence Clarfeld.  (2025). modelManager_add_configurations. [Source code].
#!! ModNotes = Provides an intuitive interface to the "models" family of tables.
#!! ModActive = 1
#!! FunctionArg = update_models !! trigger to update model options !! numeric


# the ui function
modelManager_add_configurations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          tags$h2("Model Configurations"),
          wellPanel(
            tags$h3("1. Choose a (machine learning) model"),
            selectInput(
              ns("model_id"), 
              "Select model ID",
              choices = setNames(
                dbGetQuery(con(), "SELECT pk_modelid FROM models;")[,],
                dbGetQuery(con(), "SELECT model_name FROM models;")[,]
              )
            )
          ),
          wellPanel(
            tags$h3("2. Add a (machine learning) model configuration"),
            wellPanel(
              tags$h4("Config Details"),
              textInput(ns("pk_mconfignameid"), "Config Name"),
              textAreaInput(ns("mconfigname_description"), "Description"),
              textInput(ns("mconfigname_filename"), "Filename"),
              checkboxInput(ns("is_default"), "Is Default")
            ),
            wellPanel(
              tags$h4("Config Settings"),
              uiOutput(ns("config_options"))
            ),
            actionButton(ns("add_config"), "Add configuration"),
          ),
          wellPanel(
            tags$h3("3. Edit a configuration"),
            reactable::reactableOutput(ns("mconfignames"))
          )
        )
      )
    )
  )
}


# the server function
modelManager_add_configurations_server <- function(id, update_models) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    update_configs_table <- reactiveVal(1)
    update_subTable <- reactiveVal(1)
    
    models <- reactive({
      update_models()
      dbGetQuery(con(), "SELECT pk_modelid, model_name FROM models;")
    })
    
    observe({
      updateSelectInput(
        session, 
        "model_id",
        choices = setNames(models()$pk_modelid, models()$model_name),
        selected = models()[1]
      )
    })
    
    configurations <- reactive({
      update_configs_table()
      dbGetQuery(con(), paste0("SELECT * FROM mconfignames WHERE fk_modelid = '", input$model_id, "';"))
    })
    
    subTable <- reactive({
      update_subTable()
      dbGetQuery(con(), paste0("SELECT msettingnames.setting_name, msettingoptions.option_name, mconfigvalues.* FROM msettingnames INNER JOIN mconfigvalues ON msettingnames.pk_msettingnameid = mconfigvalues.fk_msettingnameid LEFT JOIN msettingoptions ON msettingoptions.pk_msettingoptionid = mconfigvalues.fk_msettingoptionid;"))
    })
    
    observeEvent(input$add_config, {
      
      # browser()
      
      req(input$pk_mconfignameid)
      new_config <- data.frame(
        mconfigname = input$pk_mconfignameid,
        fk_modelid = input$model_id,
        description = input$mconfigname_description,
        filename = input$mconfigname_filename,
        is_default = as.numeric(input$is_default)
      )
      
      appendResult <- AMMonitor::addRecord(
        con = con(), 
        table_name = "mconfignames",
        new_record = new_config
      )
      
      if (appendResult$status == FALSE) {
        showModal(
          modalDialog(
            title = "Could not add new record",
            appendResult$message,
            easyClose = FALSE
          )
        )
      } else {
        mconfigname_id <- dbGetQuery(con(), paste0("SELECT pk_mconfignameid FROM mconfignames WHERE mconfigname = '", new_config$mconfigname, "';"))[,]
        new_mconfigvalues <- data.frame()
        for (i in seq_len(nrow(setting_names()))) {
          
          setting_value <- input[[paste0(setting_names()$setting_name[i], "_new")]]
          if (is.numeric(setting_value)) {
            settingoption_id <- NA
            value_num <- setting_value
          } else {
            settingoption_id <- as.numeric(setting_value)
            value_num <- NA
          }
          
          new_mconfigvalue <- data.frame(
            fk_mconfignameid = mconfigname_id,
            fk_msettingnameid = setting_names()$pk_msettingnameid[i],
            fk_msettingoptionid = settingoption_id,
            value_num = value_num
          )
          
          appendResult <- AMMonitor::addRecord(
            con = con(), 
            table_name = "mconfigvalues",
            new_record = new_mconfigvalue
          )
          
          if (appendResult$status == FALSE) {
            showModal(
              modalDialog(
                title = "Could not add new record",
                appendResult$message,
                easyClose = FALSE
              )
            )
          }
        }
        
        update_configs_table(update_configs_table() + 1)
        update_subTable(update_subTable() + 1)
        showModal(
          modalDialog(
            title = "Append Successful",
            appendResult$message,
            easyClose = TRUE
          )
        )        
      }
    })
    
    output$mconfignames <- reactable::renderReactable({
      reactable::reactable(
        cbind(
          configurations(),
          editRow = rep(NA, nrow(configurations())),
          delete = rep(NA, nrow(configurations()))
        ),
        columns = list(
          pk_mconfignameid = colDef(show = FALSE),
          fk_modelid = colDef(show = FALSE),
          delete = colDef(
            name = "",
            sortable = FALSE,
            cell = function() htmltools::tags$button(icon("trash")),
            sticky = "right",
            width = 40
          ),
          editRow = colDef(
            name = "",
            sortable = FALSE,
            cell = function() htmltools::tags$button(icon("pencil")),
            sticky = "right",
            width = 40
          )
        ),
        details = function(i) {
          if (nrow(configurations()) != 0) {
            subTableRows <- subTable()[
              subTable()[["fk_mconfignameid"]] %in% configurations()[["pk_mconfignameid"]][i],
            ]
            
            if (nrow(subTableRows) != 0) {
              #render reactable
              output[[paste0("subTable_", i)]] <- renderReactable({
                reactable(
                  cbind(
                    subTableRows,
                    editRow2 = NA,
                    buffer = NA
                  ),
                  style = JS("background: rgba(0, 0, 0, 0.02)"),
                  outlined = TRUE,
                  columns = list(
                    pk_mconfigvalueid = colDef(show = FALSE),
                    fk_mconfignameid = colDef(show = FALSE),
                    fk_msettingnameid = colDef(show = FALSE),
                    fk_msettingoptionid = colDef(show = FALSE),
                    editRow2 = colDef(
                      name = "",
                      sortable = FALSE,
                      cell = function() htmltools::tags$button(icon("pencil")),
                      sticky = "right",
                      width = 40
                    ),
                    buffer = colDef(
                      name = "",
                      sortable = FALSE,
                      width = 40
                    )
                  ),
                  onClick = JS(paste0(
                    "function(rowInfo, column, instance) {
                          Shiny.setInputValue('", ns('buttonClick'), "', Date.now())
                          Shiny.setInputValue('", ns('row_vals'), "', rowInfo.values)
                          Shiny.setInputValue('", ns('col_val'), "', column.id)
                          if (column.id == 'editRow2') {
                            Shiny.setInputValue('", ns('edit2'), "', Date.now())
                          }
                        }"
                  ))
                )
              })
              reactableOutput(ns(paste0("subTable_", i)))
            }
          } else {
            NULL
          }
        },
        onClick = JS(paste0(
          "function(rowInfo, column) {
              Shiny.setInputValue('", ns('buttonClick'), "', Date.now())
              Shiny.setInputValue('", ns('row_vals'), "', rowInfo.values)
              Shiny.setInputValue('", ns('col_val'), "', column.id)
              if (column.id == 'delete') {
                Shiny.setInputValue('", ns('delete'), "', Date.now())
              } else if (column.id == 'editRow') {
                Shiny.setInputValue('", ns('edit'), "', Date.now())
              }
            }"
        ))
      )
    })
    
    #edit record button click-------------
    observeEvent(
      eventExpr = input$edit,
      handlerExpr = {
        showModal(
          do.call(modalDialog, list(
            title = paste(
              'Editing record',
              input$row_vals$pk_mconfignameid,
              'setting:'
            ),
            size = "m",
            easyClose = FALSE,
            footer = tagList(
              actionButton(
                class = "btn-success",
                inputId = ns("update_row"),
                label = "Submit"
              ),
              modalButton("Cancel")
            ),
            tagList(
              textInput(
                ns("pk_mconfignameid_edit"), 
                "Config Name",
                value = input$row_vals$mconfigname
              ),
              textAreaInput(
                ns("mconfigname_description_edit"), 
                "Description",
                value = input$row_vals$description
              ),
              textInput(
                ns("mconfigname_filename_edit"), 
                "Filename",
                value = input$row_vals$filename
              ),
              checkboxInput(
                ns("mconfigname_is_default_edit"),
                "Is Default"
              )
            )
          )) #end do.call for modal dialogue
        ) #end show modal
      }
    ) #end add new row
    
    #submit update button----------------
    observeEvent(
      eventExpr = input$update_row,
      handlerExpr = {
        
        # Initialize parameter list
        params <- list(
          mconfigname = input$pk_mconfignameid_edit,
          description = input$mconfigname_description_edit,
          filename = input$mconfigname_filename_edit,
          is_default = as.numeric(input$mconfigname_is_default_edit)
        )
        
        #create statement
        updateStatement <- paste(
          "UPDATE mconfignames SET mconfigname = $1, description = $2, filename = $3, is_default = $4 WHERE pk_mconfignameid =",
          input$row_vals$pk_mconfignameid,
          ";"
        )
        
        #insert into database
        tryCatch(
          expr = {
            
            #update table
            rs <- DBI::dbSendStatement(con(), updateStatement)
            DBI::dbBind(rs, unname(params))
            updateResult <- DBI::dbGetRowsAffected(rs)
            DBI::dbClearResult(rs)
            
            #only runs if successful
            update_configs_table(update_configs_table() + 1)
            
            showModal(
              modalDialog(
                title = "Update Successful",
                paste0(updateResult, " record has successfully been updated." ),
                easyClose = TRUE
              )
            )
          },
          
          #show error modal if error occurs
          error = function(x) {
            showModal(
              modalDialog(
                title = "Could not update record",
                x,
                easyClose = FALSE
              )
            )
          }
        ) #end tryCatch update
      }
    )#end observe update_row
    
    setting_names <- reactive({dbGetQuery(con(), paste0("SELECT pk_msettingnameid, setting_name FROM msettingnames WHERE fk_modelid = '", input$model_id, "';"))})
    
    output$config_options <- renderUI({
      lapply(
        seq_len(nrow(setting_names())),
        function(i) {
          setting_options <- dbGetQuery(
            con(), 
            paste0(
              "SELECT * FROM msettingoptions WHERE fk_msettingnameid = '",
              setting_names()$pk_msettingnameid[i],
              "';"
            )
          )
          if (nrow(setting_options) != 0) {
            tagList(
              selectInput(
                ns(paste(setting_names()$setting_name[i], "new", sep = "_")),
                as.character(setting_names()$setting_name[i]),
                choices = setNames(setting_options$pk_msettingoptionid, setting_options$option_name)
              ),
              shinyjs::hidden(numericInput(
                ns(paste(setting_names()$setting_name[i], "new", sep = "_")),
                as.character(setting_names()$setting_name[i]),
                value = NULL
              ))
            )
          } else {
            tagList(
              shinyjs::hidden(selectInput(
                ns(paste(setting_names()$setting_name[i], "new", sep = "_")),
                as.character(setting_names()$setting_name[i]),
                choices = NULL
              )),
              numericInput(
                ns(paste(setting_names()$setting_name[i], "new", sep = "_")),
                as.character(setting_names()$setting_name[i]),
                value = NULL
              )
            )
          }
        }
      )
    })
    
    #edit subtatble record button click-------------
    observeEvent(
      eventExpr = input$edit2,
      handlerExpr = {
        setting_options <- dbGetQuery(
          con(), 
          paste0(
            "SELECT pk_msettingoptionid, option_name FROM msettingoptions 
                      WHERE fk_msettingnameid = ",
            input$row_vals$fk_msettingnameid,
            ";"
          )
        )
        showModal(
          do.call(modalDialog, list(
            title = paste(
              'Editing record',
              input$row_vals$pk_mconfigvalueid,
              'configuration value:'
            ),
            size = "m",
            easyClose = FALSE,
            footer = tagList(
              actionButton(
                class = "btn-success",
                inputId = ns("update_row2"),
                label = "Submit"
              ),
              modalButton("Cancel")
            ),
            if (nrow(setting_options) != 0) {
              tagList(
                selectInput(
                  ns("option_name"),
                  "Option name",
                  choices = setNames(setting_options$pk_msettingoptionid, setting_options$option_name),
                  selected = setNames(input$row_vals$fk_msettingoptionid, input$row_vals$option_name)
                ),
                shinyjs::hidden(numericInput(
                  ns("value_num_edit"), 
                  "value_num", 
                  value = input$row_vals$value_num
                ))
              )
            } else {
              tagList(
                shinyjs::hidden(selectInput(
                  ns("option_name"),
                  "Option name",
                  choices = setNames(setting_options$pk_msettingoptionid, setting_options$option_name),
                  selected = setNames(input$row_vals$fk_msettingoptionid, input$row_vals$option_name)
                )),
                numericInput(
                  ns("value_num_edit"), 
                  "value_num", 
                  value = input$row_vals$value_num
                )
              )
            }
          )) #end do.call for modal dialogue
        ) #end show modal
      }
    ) #end add new row
    
    #submit subtable update button----------------
    observeEvent(
      eventExpr = input$update_row2,
      handlerExpr = {
        # Initialize parameter list
        if (is.na(input$value_num_edit)) {
          params <- list(
            fk_msettingoptionid = as.numeric(input$option_name),
            value_num = NA
          )
        } else {
          params <- list(
            fk_msettingoptionid = NA,
            value_num = input$value_num_edit
          )
        }
        
        #create statement
        updateStatement <- paste(
          "UPDATE mconfigvalues SET fk_msettingoptionid = $1, value_num = $2 WHERE pk_mconfigvalueid = ",
          input$row_vals$pk_mconfigvalueid,
          ";"
        )
        
        #insert into database
        tryCatch(
          expr = {
            
            #update table
            rs <- DBI::dbSendStatement(con(), updateStatement)
            DBI::dbBind(rs, unname(params))
            updateResult <- DBI::dbGetRowsAffected(rs)
            DBI::dbClearResult(rs)
            
            #only runs if successful
            update_subTable(update_subTable() + 1)
            
            showModal(
              modalDialog(
                title = "Update Successful",
                paste0(updateResult, " record has successfully been updated." ),
                easyClose = TRUE
              )
            )
          },
          
          #show error modal if error occurs
          error = function(x) {
            showModal(
              modalDialog(
                title = "Could not update record",
                x,
                easyClose = FALSE
              )
            )
          }
        ) #end tryCatch update
      }
    )#end subtable observe update_row
    
    #add record button click-------------
    observeEvent(
      eventExpr = input$new,
      handlerExpr = {
        showModal(
          do.call(modalDialog, list(
            title = paste(
              'Add a new settings option for the',
              input$row_vals$setting_name,
              'setting:'
            ),
            size = "m",
            easyClose = FALSE,
            footer = tagList(
              actionButton(
                class = "btn-success",
                inputId = ns("insert_row"),
                label = "Submit"
              ),
              modalButton("Cancel")
            ),
            tagList(
              textInput(ns("option_name"), "Setting Option Name"),
              textAreaInput(ns("option_description"), "Setting Option Description")
            )
          )) #end do.call for modal dialogue
        ) #end show modal
      }
    ) #end add new row
    
    #delete row button-------------------
    observeEvent(
      eventExpr = input$delete,
      handlerExpr = {
        showModal(
          modalDialog(
            title = "Confirm deletion of row with identifiers:",
            HTML(paste0(
              "pk_mconfignameid = ",
              input$row_vals$pk_mconfignameid,
              '<br><br><b>This action cannot be undone.</b>'
            )),
            footer = tagList(
              actionButton(
                inputId = ns("confirm_delete"),
                label = "Confirm",
                class = "btn-danger"
              ),
              modalButton("Cancel")
            )
          )
        ) #end modal
      } #end handlerExpr
    ) #end delete row first button
    
    #delete confirmation----------------------
    observeEvent(input$confirm_delete, {
      # Check for foreign key references
      fk_ref_ct <- dbGetQuery(
        con(), 
        paste0(
          "SELECT COUNT(*) FROM modeloutputs WHERE fk_mconfignameid = '",
          input$row_vals$pk_mconfignameid,
          "';"
        )
      )[,]
      
      if (fk_ref_ct != 0) {
        showModal(
          modalDialog(
            title = "Could not delete record",
            paste("There are", fk_ref_ct, "modeloutputs referencing this configuration."),
            easyClose = FALSE
          )
        )
      } else {
        # Remove configvalues
        rs <- tryCatch(
          {
            stmnt <- paste0(
              "DELETE FROM mconfigvalues WHERE fk_mconfignameid = '",
              input$row_vals$pk_mconfignameid,
              "';"
            )
            dbExecute(con(), stmnt)
          },
          error = function(x) {
            showModal(
              modalDialog(
                title = "Could not delete record",
                x,
                easyClose = FALSE
              )
            )
          }
        )
        
        # Remove db record, along with any associated files and SB records
        if (is.numeric(rs)) {
          rs <- AMMonitor::deleteRecord(
            con = con(),
            table_name = "mconfignames",
            selected_row = input$row_vals
          )
          
          # Display results of attempted deletion
          if (rs$status) {
            update_configs_table(update_configs_table() + 1)
            showModal(
              modalDialog(
                title = "Deletion successful",
                rs$message,
                easyClose = TRUE
              )
            )
          } else {
            showModal(
              modalDialog(
                title = "Deletion failed",
                rs$message,
                easyClose = TRUE
              )
            )
          }
        }
      }
    }) #end observe delete confirmation
    
    return(reactiveValues())
  })
}
