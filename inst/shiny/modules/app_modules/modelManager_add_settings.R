#!! ModName = modelManager_add_settings
#!! ModDisplayName = Model Manager: Add Model Settings.
#!! ModDescription = Create, edit, and delete (ML) models and settings.
#!! ModCitation = Laurence Clarfeld.  (2025). modelManager_add_settings. [Source code].
#!! ModNotes = Provides an intuitive interface to the "models" family of tables.
#!! ModActive = 1
#!! FunctionReturn = update_models !! A trigger to update models dropdown !! numeric


# the ui function
modelManager_add_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 4,
        wellPanel(
          tags$h3("Add New (Machine Learning) Models"),
          textInput(ns('model_name'), "Model Name"),
          selectInput(
            ns("model_type"),
            "Model Type",
            choices = dbGetQuery(con(), "SELECT item FROM listitems WHERE fk_listid = 'model_type';")[,]
          ),
          textInput(ns('model_url'), "Model Website (URL)"),
          textInput(ns('amml'), "AMModels Library"),
          textInput(ns('user_manual'), "User Manual"),
          wellPanel(fluidRow(
            tags$p("Choose either an fk_taxonid or create a librarylist ith the taxa for which this model can be used for classification."),
            column(
              width = 6,
              selectInput(
                ns("fk_taxonid"),
                "Taxon",
                choices = c("", dbGetQuery(con(), "SELECT pk_taxonid FROM taxa;")[,]),
                selected = ""
              )
            ),
            column(
              width = 6,
              selectInput(
                ns("fk_librarylistid"),
                "Taxon List",
                choices = c("", dbGetQuery(con(), "SELECT pk_librarylistid FROM librarylists WHERE taxa_list = 1;")[,]),
                selected = ""
              )
            )
          )),
          textInput(ns('model_citation'), "Citation"),
          textAreaInput(ns('model_description'), "Description"),
          actionButton(ns("add_model"), "Add model")
        ),
      ),
      column(
        width = 8,
        wellPanel(
          tags$h3("Model Settings"),
          wellPanel(
            tags$h4("1. Choose a (machine learning) model"),
            selectInput(
              ns("model_id"), 
              "Select model ID",
              choices = setNames(dbGetQuery(con(), "SELECT pk_modelid FROM models;")[,], dbGetQuery(con(), "SELECT model_name FROM models;")[,])
            )
          ),
          wellPanel(
            tags$h4("2. Add New Settings"),
            tags$i("(skip this step to add options for existing settings)"),
            textInput(ns("setting_name"), "Setting Name"),
            textAreaInput(ns("setting_description"), "Setting Description"),
            actionButton(ns("add_setting"), "Add Setting")
          ),
          wellPanel(
            tags$h4("3. Add setting options"),
            tags$i("(only required for settings with categorical options)"),
            reactable::reactableOutput(ns("model_settings"))
          )
        )
      )
    )
  )
}


# the server function
modelManager_add_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    update_models_table <- reactiveVal(1)
    update_settings_table <- reactiveVal(1)
    update_subTable <- reactiveVal(1)
    
    models <- reactive({
      update_models_table()
      dbGetQuery(isolate(con()), "SELECT pk_modelid, model_name FROM models;")[,]
    })
    
    model_settings <- reactive({
      update_settings_table()
      dbGetQuery(con(), paste0("SELECT pk_msettingnameid, setting_name, description FROM msettingnames WHERE fk_modelid = '", input$model_id, "';"))
    })
    
    subTable <- reactive({
      update_subTable()
      dbGetQuery(con(), paste0("SELECT * FROM msettingoptions;"))
    })
    
    # Only allow fk_taxonid OR fk_librarylistid for new models (not both)
    observeEvent(input$fk_librarylistid, {
      req(input$fk_librarylistid != "")
      updateSelectInput(
        session = session, 
        inputId = "fk_taxonid", 
        selected = ""
      )
    })
    
    observeEvent(input$fk_taxonid, {
      req(input$fk_taxonid != "")
      updateSelectInput(
        session = session, 
        inputId = "fk_librarylistid", 
        selected = ""
      )
    })
    
    observe({
      updateSelectInput(
        session = session, 
        inputId = "model_id", 
        choices = setNames(models()$pk_modelid, models()$model_name)
      )
    })
    
    observeEvent(input$add_model, {
      req(models())
      req(input$model_name)
      
      # browser()
      
      new_model <- data.frame(
        model_name = input$model_name,
        model_url = ifelse(input$model_url == "", NA, input$model_url),
        amml = ifelse(input$amml == "", NA, input$amml),
        model_type = input$model_type,
        model_description = ifelse(input$model_description == "", NA, input$model_description),
        model_citation = ifelse(input$model_citation == "", NA, input$model_citation),
        fk_taxonid = ifelse(input$fk_taxonid == "", NA, input$fk_taxonid),
        fk_librarylistid = ifelse(input$fk_librarylistid == "", NA, input$fk_librarylistid)
      )
      
      rs <- AMMonitor::addRecord(
        con = con(), 
        table_name = "models", 
        new_record = new_model
      )
      
      if (rs$status == TRUE) {
        update_models_table(update_models_table() + 1)
      }
      
      showModal(modalDialog(
        title = "Adding Model",
        rs$message,
        easyClose = TRUE
      ))
    })
    
    output$model_settings <- reactable::renderReactable({
      reactable::reactable(
        cbind(
          model_settings(),
          editRow = rep(NA, nrow(model_settings())),
          delete = rep(NA, nrow(model_settings())),
          addSubtableRow = rep(NA, nrow(model_settings()))
        ),
        columns = list(
          pk_msettingnameid = colDef(show = FALSE),
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
          ),
          addSubtableRow = colDef(
            name = "",
            sortable = FALSE,
            cell = function() htmltools::tags$button(icon("plus")),
            sticky = "right",
            width = 40
          )
        ),
        details = function(i) {
          if (nrow(model_settings()) != 0) {
            subTableRows <- subTable()[
              subTable()[["fk_msettingnameid"]] %in% model_settings()[["pk_msettingnameid"]][i], 
            ]
            
            if (nrow(subTableRows) != 0) {
              #render reactable
              output[[paste0("subTable_", i)]] <- renderReactable({
                reactable(
                  cbind(
                    subTableRows,
                    editRow2 = NA,
                    delete2 = NA,
                    buffer = NA
                  ),
                  style = JS("background: rgba(0, 0, 0, 0.02)"),
                  outlined = TRUE,
                  columns = list(
                    pk_msettingoptionid = colDef(show = FALSE),
                    fk_msettingnameid = colDef(show = FALSE),
                    editRow2 = colDef(
                      name = "",
                      sortable = FALSE,
                      cell = function() htmltools::tags$button(icon("pencil")),
                      sticky = "right",
                      width = 40
                    ), 
                    delete2 = colDef(
                      name = "",
                      sortable = FALSE,
                      cell = function() htmltools::tags$button(icon("trash")),
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
                          if (column.id == 'delete2') {
                            Shiny.setInputValue('", ns('delete2'), "', Date.now())
                          } else if (column.id == 'editRow2') {
                            Shiny.setInputValue('", ns('edit2'), "', Date.now())
                          }
                        }"
                  ))
                )
              })
              reactable::reactableOutput(ns(paste0("subTable_", i)))
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
              else if (column.id == 'addSubtableRow') {
                Shiny.setInputValue('", ns('new'), "', Date.now())
              }
            }"
        ))
      )
    })
    
    observeEvent(input$add_setting, {
      req(input$setting_name)
      
      new_setting <- data.frame(
        setting_name = input$setting_name,
        fk_modelid = input$model_id,
        description = input$setting_description
      )
      
      rs <- AMMonitor::addRecord(
        con = con(), 
        table_name = "msettingnames", 
        new_record = new_setting
      )
      
      if (rs$status == TRUE) {
        update_settings_table(update_settings_table() + 1)
        
        updateTextInput(session, "setting_name", value = "")
        updateTextAreaInput(session, "setting_description", value = "")
      }
      
      showModal(modalDialog(
        title = "Adding Model Setting",
        rs$message,
        easyClose = TRUE
      ))
      
    })
    
    #edit record button click-------------
    observeEvent(
      eventExpr = input$edit,
      handlerExpr = {
        showModal(
          do.call(modalDialog, list(
            title = paste(
              'Editing record',
              input$row_vals$pk_msettingnameid,
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
                ns("setting_name_edit"),
                "Setting Name",
                value = input$row_vals$setting_name
              ),
              textAreaInput(
                ns("setting_description_edit"), 
                "Setting Description",
                value = input$row_vals$description
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
          setting_name = input$setting_name_edit, 
          description = input$setting_description_edit
        )
        
        #create statement
        updateStatement <- paste(
          "UPDATE msettingnames SET setting_name = $1, description = $2 WHERE pk_msettingnameid = ",
          input$row_vals$pk_msettingnameid,
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
            update_settings_table(update_settings_table() + 1)
            
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
    
    #edit subtatble record button click-------------
    observeEvent(
      eventExpr = input$edit2,
      handlerExpr = {
        showModal(
          do.call(modalDialog, list(
            title = paste(
              'Editing record',
              input$row_vals$pk_msettingoptionid,
              'setting option:'
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
            tagList(
              textInput(
                ns("option_name_edit"),
                "Setting Option Name",
                value = input$row_vals$option_name
              ),
              textAreaInput(
                ns("option_description_edit"),
                "Setting Option Description",
                value = input$row_vals$description
              )
            )
          )) #end do.call for modal dialogue
        ) #end show modal
      }
    ) #end add new row
    
    #submit subtable update button----------------
    observeEvent(
      eventExpr = input$update_row2,
      handlerExpr = {
        # Initialize parameter list
        params <- list(
          option_name = input$option_name_edit, 
          description = input$option_description_edit
        )
        
        #create statement
        updateStatement <- paste(
          "UPDATE msettingoptions SET option_name = $1, description = $2 WHERE pk_msettingoptionid = ",
          input$row_vals$pk_msettingoptionid,
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
    
    #record submit button----------------
    observeEvent(
      eventExpr = input$insert_row,
      handlerExpr = {
        #create dataframe with inputs
        new_settings_option <- data.frame(
          fk_msettingnameid = input$row_vals$pk_msettingnameid,
          option_name = input$option_name,
          description = input$option_description
        )
        
        appendResult <- AMMonitor::addRecord(
          con = con(), 
          table_name = "msettingoptions",
          new_record = new_settings_option
        )
        
        if (appendResult$status == TRUE) {
          update_subTable(update_subTable() + 1)
          showModal(
            modalDialog(
              title = "Append Successful",
              appendResult$message,
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modalDialog(
              title = "Could not add new record",
              appendResult$message,
              easyClose = FALSE
            )
          )
        }
      }
    ) #end insert record button
    
    #delete row button-------------------
    observeEvent(
      eventExpr = input$delete,
      handlerExpr = {
        showModal(
          modalDialog(
            title = "Confirm deletion of row with identifiers:",
            HTML(paste0(
              "pk_msettingnameid = ", 
              input$row_vals$pk_msettingnameid,
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
      # Remove db record, along with any associated files and SB records
      rs <- AMMonitor::deleteRecord(
        con = con(), 
        table_name = "msettingnames", 
        selected_row = input$row_vals
      )
      
      # Display results of attempted deletion
      if (rs$status) {
        update_settings_table(update_settings_table() + 1)
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
    }) #end observe delete confirmation
    
    #delete subtable row button-------------------
    observeEvent(
      eventExpr = input$delete2,
      handlerExpr = {
        showModal(
          modalDialog(
            title = "Confirm deletion of row with identifiers:",
            HTML(paste0(
              "pk_msettingoptionid = ", 
              input$row_vals$pk_msettingoptionid,
              '<br><br><b>This action cannot be undone.</b>'
            )), 
            footer = tagList(
              actionButton(
                inputId = ns("confirm_delete2"),
                label = "Confirm",
                class = "btn-danger"
              ),
              modalButton("Cancel")
            )
          )
        ) #end modal 
      } #end handlerExpr
    ) #end delete row first button
    
    #delete subtable confirmation----------------------
    observeEvent(input$confirm_delete2, {
      # Remove db record, along with any associated files and SB records
      rs <- AMMonitor::deleteRecord(
        con = con(), 
        table_name = "msettingoptions", 
        selected_row = input$row_vals
      )
      
      # Display results of attempted deletion
      if (rs$status) {
        update_subTable(update_subTable() + 1)
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
    }) #end observe delete subtable confirmation
    
    return(reactiveValues(
      update_models = reactive(update_models_table())
    ))
  })
}
