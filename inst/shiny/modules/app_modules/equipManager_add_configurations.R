#!! ModName = equipManager_add_configurations
#!! ModDisplayName = Equipment Manager: Add Model Configurations.
#!! ModDescription = Add, edit and delete equipment model configurations.
#!! ModCitation = Laurence Clarfeld.  (2025). equipManager_add_configurations. [Source code].
#!! ModNotes = Provides an intuitive interface to the "equipment" family of tables.
#!! ModActive = 1
#!! FunctionArg = update_equip_models !! trigger to update equipment models options !! numeric

# the ui function
equipManager_add_configurations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          tags$h2("Model Settings"),
          wellPanel(
            tags$h3("1. Choose an equipment model"),
            selectInput(
              ns("model_id"), 
              "Select equipment model ID",
              choices = dbGetQuery(con(), "SELECT pk_equipmodelid FROM equipmodels;")[,]
            )
          ),
          wellPanel(
            tags$h3("2. Add an equipment model configuration"),
            wellPanel(
              tags$h4("Config Details"),
              textInput(ns("pk_econfignameid"), "Config Name"),
              textAreaInput(ns("econfigname_description"), "Description"),
              textInput(ns("econfigname_filename"), "Filename")
            ),
            wellPanel(
              tags$h4("Config Settings"),
              uiOutput(ns("config_options"))
            ),
            actionButton(ns("add_config"), "Add configuration"),
          ),
          wellPanel(
            tags$h3("3. Edit a configuration"),
            reactable::reactableOutput(ns("econfignames"))
            # selectInput(
            #   ns("model_id"), 
            #   "Select equipment model ID",
            #   choices = dbGetQuery(con(), "SELECT pk_equipmodelid FROM equipmodels;")[,]
            # )
          )
        )
      )
    )
  )
}


# the server function
equipManager_add_configurations_server <- function(id, update_equip_models) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    update_configs_table <- reactiveVal(1)
    update_subTable <- reactiveVal(1)
    
    equip_models <- reactive({
      update_equip_models()
      dbGetQuery(con(), "SELECT pk_equipmodelid FROM equipmodels;")[,]
    })
    
    observe({
      updateSelectInput(
        session, 
        "model_id",
        choices = equip_models(),
        selected = equip_models()[1]
      )
    })
    
    equip_configurations <- reactive({
      update_configs_table()
      dbGetQuery(con(), paste0("SELECT econfignames.*  FROM econfignames WHERE fk_equipmodelid = '", input$model_id, "';"))
    })
    
    subTable <- reactive({
      update_subTable()
      dbGetQuery(con(), paste0("SELECT esettingnames.setting_name, esettingoptions.option_name, econfigvalues.* FROM esettingnames INNER JOIN econfigvalues ON esettingnames.pk_esettingnameid = econfigvalues.fk_esettingnameid LEFT JOIN esettingoptions ON esettingoptions.pk_esettingoptionid = econfigvalues.fk_esettingoptionid;"))
    })
    
    observeEvent(input$add_config, {
      req(input$pk_econfignameid)
      new_config <- data.frame(
        pk_econfignameid = input$pk_econfignameid,
        fk_equipmodelid = input$model_id,
        description = input$econfigname_description,
        filename = input$econfigname_filename
      )
      
      appendResult <- AMMonitor::addRecord(
        con = con(), 
        table_name = "econfignames",
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
        new_econfigvalues <- data.frame()
        for (i in seq_len(nrow(setting_names()))) {
          
          setting_value <- input[[paste0(setting_names()$setting_name[i], "_new")]]
          if (is.numeric(setting_value)) {
            settingoption_id <- NA
            value_num <- setting_value
          } else {
            settingoption_id <- as.numeric(setting_value)
            value_num <- NA
          }
          
          new_econfigvalue <- data.frame(
            fk_econfignameid = new_config$pk_econfignameid,
            fk_esettingnameid = setting_names()$pk_esettingnameid[i],
            fk_esettingoptionid = settingoption_id,
            value_num = value_num
          )
          
          appendResult <- AMMonitor::addRecord(
            con = con(), 
            table_name = "econfigvalues",
            new_record = new_econfigvalue
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
    
    output$econfignames <- reactable::renderReactable({
      reactable::reactable(
        cbind(
          equip_configurations(),
          editRow = rep(NA, nrow(equip_configurations())),
          delete = rep(NA, nrow(equip_configurations()))
        ),
        columns = list(
          fk_equipmodelid = colDef(show = FALSE),
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
          if (nrow(equip_configurations()) != 0) {
            subTableRows <- subTable()[
              subTable()[["fk_econfignameid"]] %in% equip_configurations()[["pk_econfignameid"]][i],
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
                    pk_econfigvalueid = colDef(show = FALSE),
                    fk_econfignameid = colDef(show = FALSE),
                    fk_esettingnameid = colDef(show = FALSE),
                    fk_esettingoptionid = colDef(show = FALSE),
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
              input$row_vals$pk_esettingnameid,
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
                ns("pk_econfignameid_edit"), 
                "Config Name",
                value = input$row_vals$pk_econfignameid
              ),
              textAreaInput(
                ns("econfigname_description_edit"), 
                "Description",
                value = input$row_vals$description
              ),
              textInput(
                ns("econfigname_filename_edit"), 
                "Filename",
                value = input$row_vals$filename
              ),
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
          pk_econfignameid = input$pk_econfignameid_edit,
          description = input$econfigname_description_edit,
          filename = input$econfigname_filename_edit
        )
        
        #create statement
        updateStatement <- paste(
          "UPDATE econfignames SET pk_econfignameid = $1, description = $2, filename = $3 WHERE pk_econfignameid =",
          paste0("'", input$row_vals$pk_econfignameid, "'"),
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
    
    setting_names <- reactive({dbGetQuery(con(), paste0("SELECT pk_esettingnameid, setting_name FROM esettingnames WHERE fk_equipmodelid = '", input$model_id, "';"))})
    
    output$config_options <- renderUI({
      lapply(
        seq_len(nrow(setting_names())),
        function(i) {
          setting_options <- dbGetQuery(
            con(), 
            paste0(
              "SELECT * FROM esettingoptions WHERE fk_esettingnameid = '",
              setting_names()$pk_esettingnameid[i],
              "';"
            )
          )
          if (nrow(setting_options) != 0) {
            tagList(
              selectInput(
                ns(paste(setting_names()$setting_name[i], "new", sep = "_")),
                as.character(setting_names()$setting_name[i]),
                choices = setNames(setting_options$pk_esettingoptionid, setting_options$option_name)
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
            "SELECT pk_esettingoptionid, option_name FROM esettingoptions 
                      WHERE fk_esettingnameid = ",
            input$row_vals$fk_esettingnameid,
            ";"
          )
        )
        showModal(
          do.call(modalDialog, list(
            title = paste(
              'Editing record',
              input$row_vals$pk_econfigvalueid,
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
                  choices = setNames(setting_options$pk_esettingoptionid, setting_options$option_name),
                  selected = setNames(input$row_vals$fk_esettingoptionid, input$row_vals$option_name)
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
                  choices = setNames(setting_options$pk_esettingoptionid, setting_options$option_name),
                  selected = setNames(input$row_vals$fk_esettingoptionid, input$row_vals$option_name)
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
            fk_esettingoptionid = as.numeric(input$option_name),
            value_num = NA
          )
        } else {
          params <- list(
            fk_esettingoptionid = NA,
            value_num = input$value_num_edit
          )
        }
        
        #create statement
        updateStatement <- paste(
          "UPDATE econfigvalues SET fk_esettingoptionid = $1, value_num = $2 WHERE pk_econfigvalueid = ",
          input$row_vals$pk_econfigvalueid,
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
              "pk_econfignameid = ",
              input$row_vals$pk_econfignameid,
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
          "SELECT COUNT(*) FROM visits WHERE fk_econfigid = '",
          input$row_vals$pk_econfignameid,
          "';"
        )
      )[,]
      
      if (fk_ref_ct != 0) {
        showModal(
          modalDialog(
            title = "Could not delete record",
            paste("There are", fk_ref_ct, "visits referencing this configuration."),
            easyClose = FALSE
          )
        )
      } else {
        # Remove configvalues
        rs <- tryCatch(
          {
            stmnt <- paste0(
              "DELETE FROM econfigvalues WHERE fk_econfignameid = '",
              input$row_vals$pk_econfignameid,
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
            table_name = "econfignames",
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
