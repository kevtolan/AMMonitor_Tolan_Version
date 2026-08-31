#!! ModName = equipManager_add_settings
#!! ModDisplayName = Equipment Manager: Add Model Settings.
#!! ModDescription = Create, edit, and delete equipment models and settings.
#!! ModCitation = Laurence Clarfeld.  (2025). equipManager_add_settings. [Source code].
#!! ModNotes = Provides an intuitive interface to the "equipment" family of tables.
#!! ModActive = 1
#!! FunctionReturn = update_equip_models !! A trigger to update equip models dropdown !! numeric

# the ui function
equipManager_add_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 4,
        wellPanel(
          tags$h3("Add New Equipment Models"),
          textInput(ns('pk_equipmodelid'), "Equipment Model Name"),
          selectInput(
            ns("equip_type"),
            "Equip Type",
            choices = dbGetQuery(con(), "SELECT item FROM listitems WHERE fk_listid = 'equip_type';")[,]
          ),
          textInput(ns('manufacturer'), "Manufacturer"),
          textInput(ns('user_manual'), "User Manual"),
          actionButton(ns("add_equip_model"), "Add model")
        ),
      ),
      column(
        width = 8,
        wellPanel(
          tags$h3("Model Settings"),
          wellPanel(
            tags$h4("1. Choose an equipment model"),
            selectInput(
              ns("model_id"), 
              "Select equipment model ID",
              choices = dbGetQuery(con(), "SELECT pk_equipmodelid FROM equipmodels;")[,]
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
equipManager_add_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    update_equip_models_table <- reactiveVal(1)
    update_settings_table <- reactiveVal(1)
    update_subTable <- reactiveVal(1)
    
    equip_models <- reactive({
      update_equip_models_table()
      dbGetQuery(con(), "SELECT pk_equipmodelid FROM equipmodels;")[,]
    })
    
    equip_model_settings <- reactive({
      update_settings_table()
      dbGetQuery(con(), paste0("SELECT pk_esettingnameid, setting_name, description FROM esettingnames WHERE fk_equipmodelid = '", input$model_id, "';"))
    })
    
    subTable <- reactive({
      update_subTable()
      dbGetQuery(con(), paste0("SELECT * FROM esettingoptions;"))
    })
    
    observe({
      updateSelectInput(
        session, 
        "model_id",
        choices = equip_models(),
        selected = equip_models()[1]
      )
    })
    
    observeEvent(input$add_equip_model, {
      req(equip_models())
      req(input$pk_equipmodelid)
      
      new_equipmodel <- data.frame(
        pk_equipmodelid = input$pk_equipmodelid,
        equip_type = input$equip_type,
        manufacturer = ifelse(input$manufacturer == "", NA, input$manufacturer),
        user_manual = ifelse(input$user_manual == "", NA, input$user_manual)
      )
      
      rs <- AMMonitor::addRecord(
        con = con(), 
        table_name = "equipmodels", 
        new_record = new_equipmodel
      )
      
      # if (rs$status == TRUE) {equip_models(c(equip_models(), input$pk_equipmodelid))}
      if (rs$status == TRUE) {
        update_equip_models_table(update_equip_models_table() + 1)
      }
      
      showModal(modalDialog(
        title = "Adding Equipment Model",
        rs$message,
        easyClose = TRUE
      ))
    })
    
    output$model_settings <- reactable::renderReactable({
      reactable::reactable(
        cbind(
          equip_model_settings(),
          editRow = rep(NA, nrow(equip_model_settings())),
          delete = rep(NA, nrow(equip_model_settings())),
          addSubtableRow = rep(NA, nrow(equip_model_settings()))
        ),
        columns = list(
          pk_esettingnameid = colDef(show = FALSE),
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
          if (nrow(equip_model_settings()) != 0) {
            subTableRows <- subTable()[
              subTable()[["fk_esettingnameid"]] %in% equip_model_settings()[["pk_esettingnameid"]][i], 
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
                    pk_esettingoptionid = colDef(show = FALSE),
                    fk_esettingnameid = colDef(show = FALSE),
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
        fk_equipmodelid = input$model_id,
        description = input$setting_description
      )
      
      rs <- AMMonitor::addRecord(
        con = con(), 
        table_name = "esettingnames", 
        new_record = new_setting
      )
      
      if (rs$status == TRUE) {
        update_settings_table(update_settings_table() + 1)
        
        updateTextInput(session, "setting_name", value = "")
        updateTextAreaInput(session, "setting_description", value = "")
      }
      
      showModal(modalDialog(
        title = "Adding Equipment Model Setting",
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
          "UPDATE esettingnames SET setting_name = $1, description = $2 WHERE pk_esettingnameid = ",
          input$row_vals$pk_esettingnameid,
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
              input$row_vals$pk_esettingoptionid,
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
          "UPDATE esettingoptions SET option_name = $1, description = $2 WHERE pk_esettingoptionid = ",
          input$row_vals$pk_esettingoptionid,
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
        new_equip_settings_option <- data.frame(
          fk_esettingnameid = input$row_vals$pk_esettingnameid,
          option_name = input$option_name,
          description = input$option_description
        )
        
        appendResult <- AMMonitor::addRecord(
          con = con(), 
          table_name = "esettingoptions",
          new_record = new_equip_settings_option
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
              "pk_esettingnameid = ", 
              input$row_vals$pk_esettingnameid,
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
        table_name = "esettingnames", 
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
              "pk_esettingoptionid = ", 
              input$row_vals$pk_esettingoptionid,
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
        table_name = "esettingoptions", 
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
      update_equip_models = reactive(update_equip_models_table())
    ))
  })
}
