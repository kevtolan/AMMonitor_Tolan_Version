# Function for custom pagination implementation
fetch_page <- function(tableName, page_number, page_size, filters, sortCol, descend) {
  table_info <- dictionary[dictionary$pk_tablename == tableName,]
  
  where_clauses <- character(0)
  
  for (field in names(filters)) {
    fieldName <- substr(field, 8, nchar(field))
    if (filters[[field]] != "") {
      if (table_info$var_type[table_info$pk_fieldname == fieldName] %in% c('INTEGER', 'REAL')) {
        if (!is.na(as.numeric(filters[[field]]))) {
          where_clauses <- c(
            where_clauses,
            paste0(
              fieldName,
              ' = ',
              filters[[field]]
            )
          )
        }
      } else {
        where_clauses <- c(
          where_clauses,
          paste0(
            fieldName,
            ' like \'%',
            gsub("'", "''", filters[[field]]),
            '%\''
          )
        )
      }
    }
  }
  
  if (length(where_clauses) != 0) {
    where_clauses <- paste(' WHERE', paste(where_clauses, collapse = ' and '))
  }
  
  if (!is.null(sortCol) && sortCol != "") {
    sort_clause <- paste(
      sortCol,
      ifelse(descend, 'DESC', 'ASC')
    )
  } else {
    sort_clause <- paste(table_info$pk_fieldname[table_info$pk == 1], collapse = ',')
  }
  
  stmnt <- paste0(
    'SELECT * FROM ',
    tableName,
    where_clauses,
    ' ORDER BY ',
    sort_clause,
    ' LIMIT ',
    page_size,
    ' OFFSET ',
    page_size*(page_number-1),
    ';'
  )
  
  retrieved_data <- dbGetQuery(
    con(),
    stmnt
  )
  
  max_ct <- dbGetQuery(
    con(),
    paste0(
      'SELECT ROUND(COUNT(*)) FROM ',
      tableName,
      where_clauses,
      ';'
    )
  )
  
  list(
    data = retrieved_data,
    max_ct = max_ct
  )
}


table_ui <- function(id) {
  tagList(
    shinyjs::useShinyjs(),
    div(style = "display: none;", icon("trash")),
    tags$br(),
    fluidRow(
      column(
        width = 3,
        actionButton(
          class = "btn-success",
          inputId = NS(id, "new"),
          label = "Add new record",
          icon = icon("plus")
        )
      ),
      column(
        width = 3,
        actionButton(
          class = "btn-secondary",
          inputId = NS(id, "refresh"),
          label = "Refresh Table",
          icon = icon("rotate")
        )
      )
    ),
    br(),
    h3("Click on an existing record to edit or delete"),
    
    # Pagination upate - inserting the custom pagination controls
    fluidRow(
      column(
        4,
        column(1, actionButton(NS(id, "prev_page"), label = "", icon = icon("angle-left"))),
        column(3, numericInput(NS(id, 'pageNum'), NULL, 1, min = 1, step = 1)),
        column(2, textOutput(NS(id, 'n_pages'))),
        column(1, actionButton(NS(id, "next_page"), label = "", icon = icon("angle-right"))),
        column(3, checkboxInput(NS(id, 'showPanel'), 'Show Filters', FALSE),),
        column(2, downloadButton(NS(id, 'downloadTable'), 'Download Table'))
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = 'input.showPanel', 
        ns = NS(id), # 'input.showPanel',
        wellPanel(
          uiOutput(NS(id, 'the_filters'))
        )
      )
    ),
    fluidRow(textOutput(NS(id, 'n_rows'))),
    reactableOutput(outputId = NS(id, id = "show_table"))
  ) #end taglist
} #end ui function

table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    auth <- reactiveValues(
      status = 0,
      creds = NA,
      type = NA
    )
    
    # Pagination functionality --------------
    page_number <- reactiveVal(1) # current page number
    page_size <- 20 # Records per page
    
    # Fields by which the table can be filtered
    filters <- reactiveValues()
    observe({
      req(filters)
      sapply(names(input), function(x) {
        if (startsWith(x, 'filter_')) {
          filters[[x]] <- input[[x]]
        }
      })
    })
    
    # Render filter inputs
    output$the_filters <- renderUI({
      
      # Add pagination filters
      tagList(
        wellPanel(
          tags$h2('Sorting'),
          selectizeInput(
            NS(id, 'sortColumn'), 
            'Sort by column',
            choices = table_info$pk_fieldname,
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          ),
          checkboxInput(NS(id, 'sortDescending'), 'Descending?')
        ),
        wellPanel(
          tags$h2('Filters'),
          lapply(
            X = table_info$pk_fieldname,
            FUN = function(i) {
              textInput(
                inputId = NS(id, paste0("filter_", i)),
                label = i
              )
            }
          )
        )
      )
    })
    
    # Total records found in table with applied filters
    output$n_rows <- renderText({
      paste('Total records found:', current_data()$max_ct)
    })
    
    # Total pages found in table with applied filters
    output$n_pages <- renderText({
      paste('of', ceiling(current_data()$max_ct/page_size))
    })
    
    # Update page number displayed for pagination
    observe({
      if (current_data()$max_ct == 0) {
        page_number(1)
      } else if (page_number() < 1) {
        page_number(1)
      } else if (page_number() > ceiling(current_data()$max_ct / page_size)) {
        page_number(ceiling(current_data()$max_ct[,] / page_size))
      } else if (page_number() %% 1 != 0) {
        page_number(round(page_number()))
      } else {
        updateNumericInput(
          session = session, 
          inputId = 'pageNum', 
          label = NULL, 
          value = page_number(), 
          min = 1, 
          step = 1
        )
      }
    })
    
    # Update page number
    observe({
      page_number(input$pageNum)
    })
    
    # Go to previous page
    observeEvent(input$prev_page, {
      if (page_number() != 1) {
        current_page_number <- page_number()
        page_number(current_page_number - 1)
      }
    })
    
    # Go to next page
    observeEvent(input$next_page, {
      if (page_number() != ceiling(current_data()$max_ct / page_size)) {
        current_page_number <- page_number()
        page_number(current_page_number + 1)
      }
    })
    
    # Load the current page for the given table, with applied filters
    current_data <- reactive({
      req(page_number())
      update_trigger()
      fetch_page(id, page_number(), page_size, filters, input$sortColumn, input$sortDescending)
    })
    
    #make dataframe of subset of dbDictionary
    table_info <- dictionary[which(dictionary$pk_tablename == id),]
    
    #get list of column names for the table
    fieldlist <- table_info$pk_fieldname
    
    #get primary keys
    primarykeys <- table_info$pk_fieldname[which(table_info$pk == 1)]
    
    #get shinyTable entry
    cur_table <- shiny_table[which(shiny_table$fk_tablename == id),]
    
    #get shinyTable entry for subtable
    cur_subtable <- shiny_table[which(shiny_table$fk_tablename == cur_table$subtable),]
    
    # set up table-------------
    # set a counter which will call the database when triggered
    update_trigger <- reactiveVal(1) 
    
    db_table <- eventReactive(
      eventExpr = {c(update_trigger(), current_data())},
      valueExpr = {
        dbTab <- current_data()$data 
        columnOrder <- vector()
        endCols <- vector()
        for (i in seq_len(nrow(table_info))) {
          if (!is.na(table_info$sort_order[i])) {
            columnOrder <- c(columnOrder, table_info$pk_fieldname[i])
          } else {
            endCols <- c(endCols, table_info$pk_fieldname[i])
          }
        }
        columnOrder <- c(columnOrder, endCols)
        dbTab <- dbTab[columnOrder]
      }
    )
    
    #setting up row buttons -----------------------
    delete_button <- function(tbl, db_table){
      function(i){
        sprintf(
          '<button id="tableName_%s_%d" class="fa fa-trash" type="button" onclick="%s"></button>', 
          tbl, 
          i, 
          paste0(
            "Shiny.setInputValue('", NS(id, 'buttonClick'), "', Date.now()); ",
            "Shiny.setInputValue('", NS(id, 'delete'), "', Date.now());",
            "Shiny.setInputValue('", NS(id, 'tableName'), "', '", id,"'); ",
            "Shiny.setInputValue('", NS(id, 'rowNum'), "', ", i, "); "
          )
        )
      }
    }
    
    edit_button <- function(tbl, db_table){
      function(i){
        sprintf(
          '<button id="edit_%s_%d" class="fa fa-pencil" type="button" onclick="%s"></button>', 
          tbl, 
          i, 
          paste0(
            "Shiny.setInputValue('", NS(id, 'buttonClick'), "', Date.now()); ",
            "Shiny.setInputValue('", NS(id, 'edit'), "', Date.now());",
            "Shiny.setInputValue('", NS(id, 'tableName'), "', '", id,"'); ",
            "Shiny.setInputValue('", NS(id, 'rowNum'), "', ", i, "); "
          )
        )
      }
    }
    
    
    #setting up subTables ----------------------------
    if (nrow(cur_table) != 0) {
      
      if (!is.na(cur_table$subtable) & cur_table$subtable != "") {
        #get subtable_info
        subtable_info <- dictionary[which(dictionary$pk_tablename == cur_table$subtable),]
        
        #get primary keys
        primarykeys2 <- subtable_info$pk_fieldname[which(subtable_info$pk == 1)]
        
        #get subTable
        subTable <- eventReactive(
          eventExpr = {update_trigger()},
          valueExpr = {
            subTab <- DBI::dbGetQuery(
              conn = con(),
              statement = paste0(
                "SELECT * FROM ", 
                cur_table$subtable,
                ";"
              )
            )
            columnOrder <- subtable_info$pk_fieldname
            subTab <- subTab[columnOrder]
          }
        )
      }
    }
    
    # Keep track of delete trigger received from trash can button clicks
    row_click_R <- reactiveValues(addSubtableRow = FALSE)
    
    # update reactable-------------------
    # make sure this happens before the delete button is triggered
    observeEvent(input$buttonClick, priority = 9999, {
      
      if (input$col_val == 'addSubtableRow') {
        row_click_R$addSubtableRow <- TRUE
      } else {
        row_click_R$addSubtableRow <- FALSE
      }
      
      # Remove the "extra" columns for the buttons
      row_click_R$selectedRow <- input$row_vals[
        !names(input$row_vals) %in% c('delete', 'delete2', 'editRow', 'editRow2', 'addSubtableRow', 'buffer')
      ]
      
      # Do some sleuthing to infer the tableName (based on fields in the selected row)        
      tableCts <- table(
        dictionary$pk_tablename[
          dictionary$pk_fieldname %in% names(row_click_R$selectedRow)
        ]
      )
      row_click_R$tableName <- names(tableCts[tableCts == length(row_click_R$selectedRow)])
      
      if (length(row_click_R$tableName) == 0) {
        stop('There was a problem displaying the selected row, suggesting a misalignment between the database schema and dbdictionary.')
      }
      
      # Primary key columns
      row_click_R$pkFields <- dictionary[which(dictionary$pk_tablename == row_click_R$tableName & dictionary$pk == 1),]
      
    })
    
    #create reactable--------------------
    output$show_table <- renderReactable({
      primaryTable <- list()
      primaryTable$table <- cbind(
        db_table(), 
        editRow = rep(NA, nrow(db_table())),
        delete = rep(NA, nrow(db_table()))
      )
      primaryTable$colDefs <- list(
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
      )
      if (cur_table$subtable != "" & !is.na(cur_table$subtable)) {
        primaryTable$table <- cbind(
          primaryTable$table,
          addSubtableRow = rep(NA, nrow(db_table()))
        )
        primaryTable$colDefs$addSubtableRow <- colDef(
          name = "",
          sortable = FALSE,
          cell = function() htmltools::tags$button(icon("plus")),
          sticky = "right",
          width = 40
        )
      }
      
      reactable(
        data = primaryTable$table,
        # filterable = TRUE,
        filterable = FALSE,
        sortable = FALSE,
        showSortIcon = FALSE,
        pagination = FALSE,
        columns = primaryTable$colDefs,
        details = function(i) {
          if (nrow(cur_table) != 0) {
            if (cur_table$subtable != "" & !is.na(cur_table$subtable)) {
              subTableRows <- subTable()[
                subTable()[[
                  subtable_info$pk_fieldname[
                    which(subtable_info$foreign_key_field %in% primarykeys)
                  ]
                ]] %in% db_table()[[primarykeys]][i], 
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
                          Shiny.setInputValue('", NS(id, 'buttonClick'), "', Date.now())
                          Shiny.setInputValue('", NS(id, 'row_vals'), "', rowInfo.values)
                          Shiny.setInputValue('", NS(id, 'col_val'), "', column.id)
                          if (column.id == 'delete2') {
                            Shiny.setInputValue('", NS(id, 'delete'), "', Date.now())
                          } else if (column.id == 'editRow2') {
                            Shiny.setInputValue('", NS(id, 'edit'), "', Date.now())
                          } 
                        }"
                    ))
                  )
                })
                reactableOutput(NS(id, paste0("subTable_", i)))
              }
            }
          } else {
            NULL
          }
        },
        onClick = JS(paste0(
          "function(rowInfo, column) {
              Shiny.setInputValue('", NS(id, 'buttonClick'), "', Date.now())
              Shiny.setInputValue('", NS(id, 'row_vals'), "', rowInfo.values)
              Shiny.setInputValue('", NS(id, 'col_val'), "', column.id)
              if (column.id == 'delete') {
                Shiny.setInputValue('", NS(id, 'delete'), "', Date.now())
              } else if (column.id == 'editRow') {
                Shiny.setInputValue('", NS(id, 'edit'), "', Date.now())
              } 
              else if (column.id == 'addSubtableRow') {
                Shiny.setInputValue('", NS(id, 'new'), "',", input$new + 1, ")
              }
            }"
        ))
      )
    })
    
    #add record button click-------------
    observeEvent(
      eventExpr = input$new,
      handlerExpr = {
        
        # If this is a subtable row addition, do that...
        if (row_click_R$addSubtableRow) {
          subtableName <- shiny_table[which(shiny_table$fk_tablename == row_click_R$tableName), 'subtable']
          subtable_dict <- dictionary[which(dictionary$pk_tablename == subtableName),]
          rowVals <- list()
          fkFields <- subtable_dict[
            (!is.na(subtable_dict$foreign_key_field)) & (subtable_dict$foreign_key_table %in% id),
            c('pk_fieldname', 'foreign_key_field')
          ]
          rowVals[fkFields$pk_fieldname] <- row_click_R$selectedRow[fkFields$foreign_key_field]
          modalUI <- getModalUI(subtableName, id, subtable_dict, con(), TRUE, rowVals)
        } else {
          modalUI <- getModalUI(id, id, table_info, con(), FALSE)
        }
        showModal(
          do.call(modalDialog, list(
            title = paste0(
              'Add a new record to the "',
              ifelse(
                row_click_R$addSubtableRow,
                subtableName,
                id
              ),
              '" table:'
            ),
            size = "m",
            easyClose = FALSE,
            footer = tagList(
              actionButton(
                class = "btn-success",
                inputId = NS(id, "insert_row"),
                label = "Submit"
              ),
              modalButton("Cancel")
            ),
            modalUI
          )) #end do.call for modal dialogue
        ) #end show modal
      }
    ) #end add new row
    
    #record submit button----------------
    observeEvent(
      eventExpr = input$insert_row,
      handlerExpr = {
        #create dataframe with inputs
        tempList <- list()
        
        if (row_click_R$addSubtableRow) {
          subtableName <- shiny_table[which(shiny_table$fk_tablename == row_click_R$tableName), 'subtable']
          addFields <- subtable_info$pk_fieldname
          theDictionary <- subtable_info
        } else {
          addFields <- fieldlist
          theDictionary <- table_info
        }
        
        if (!any(
          sapply(
            paste0("new_", addFields[startsWith(addFields, 'pk')]), 
            function(x) {input[[x]]}) %in% c("", NA)
        )){
          
          for (i in addFields) {
            inputType <- tolower(theDictionary$shiny_input[which(theDictionary$pk_fieldname == i)])
            if (!is.na(inputType)) {
              if (tolower(theDictionary$shiny_input[which(theDictionary$pk_fieldname == i)]) == "time") {
                timeVals <- sapply(
                  c('-hours', '-minutes', '-seconds'), 
                  FUN = function(x) {input[[paste0("new_", i, x)]]}
                )
                if (all(timeVals == "") || (timeVals[1] == "" & any(timeVals[2:3] != ""))) {
                  tempList[[i]] <- NA
                } else {
                  timeVals[timeVals == ""] <- "00"
                  tempList[[i]] <- paste(timeVals, collapse = ":")
                }
              } else if (tolower(theDictionary$shiny_input[which(theDictionary$pk_fieldname == i)]) == "date") {
                tempList[[i]] <- ifelse(
                  test = length(input[[paste0("new_", i)]]) != 0,
                  yes = as.character(input[[paste0("new_", i)]]),
                  no = NA
                )
              } else if (tolower(theDictionary$shiny_input[which(theDictionary$pk_fieldname == i)]) == "checkbox") {
                tempList[[i]] <- as.numeric(input[[paste0("new_", i)]])
              } else if (tolower(theDictionary$shiny_input[which(theDictionary$pk_fieldname == i)]) != "locked") {
                tempList[[i]] <- ifelse(
                  test = ! input[[paste0("new_", i)]] %in% "",
                  yes = input[[paste0("new_", i)]],
                  no = NA
                )
              } 
            }}
          
          tempdf <- as.data.frame(tempList)
          
          #insert into database
          tryCatch(
            expr = {
              
              appendResult <- AMMonitor::addRecord(
                con = con(), 
                table_name = ifelse(
                  row_click_R$addSubtableRow,
                  subtableName,
                  id
                ),
                new_record = tempdf
              )
              
              #only runs if successful
              if (appendResult$status == TRUE) {
                update_trigger(update_trigger() + 1)
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
            },
            
            #show error modal if error occurs
            error = function(x) {
              showModal(
                modalDialog(
                  title = "Could not add new record",
                  x,
                  easyClose = FALSE
                )
              )
            }
          ) #end tryCatch
        } else {
          showModal(
            modalDialog(
              title = "Primary key missing, Could not add new record",
              paste('Check primary key fields:', paste(addFields[startsWith(addFields, 'pk')], collapse = ', ')),
              easyClose = FALSE
            )
          )
        } # end if/else valid pk
      } 
    ) #end insert record button
    
    #edit record button---------------
    observeEvent(
      eventExpr = input$edit,
      handlerExpr = {
        selectedRow <- row_click_R$selectedRow
        selectedCols <- names(selectedRow)
        selectedDict <- dictionary[which(dictionary$pk_tablename == row_click_R$tableName),]
        # TODO - TO DO - Come back and fix this to show compound primary keys
        #editing windows
        showModal(
          do.call(
            modalDialog,
            args = list(
              title = paste0(
                "Editing record ", 
                paste(
                  paste(
                    row_click_R$pkFields$pk_fieldname, 
                    row_click_R$selectedRow[row_click_R$pkFields$pk_fieldname], 
                    sep = ' = '
                  ),
                  collapse = ', '
                )
              ),
              easyClose = FALSE,
              footer = tagList(
                actionButton(
                  class = "btn-success",
                  inputId = NS(id, "update_row"),
                  label = "Submit"
                ),
                modalButton("Cancel")
              ),
              getModalUI(row_click_R$tableName, id, selectedDict, con(), FALSE, selectedRow)
            ) #end args list
          ) #end do.call
        ) #end showModal for editing
      } #end handlerExpr
    ) #end observeEvent
    
    #submit update button----------------
    observeEvent(
      eventExpr = input$update_row,
      handlerExpr = {
        # Get info on selected row
        tableName <- row_click_R$tableName
        selectedRow <- row_click_R$selectedRow
        selectedCols <- names(selectedRow)
        selectedDict <- dictionary[which(dictionary$pk_tablename == tableName),]
        tablePk <- row_click_R$pkFields$pk_fieldname
        
        # Initialize parameter list
        params <- list()
        
        #loop through all the columns 
        for (i in selectedCols) {
          
          inputType <- tolower(selectedDict$shiny_input[which(selectedDict$pk_fieldname == i)])
          if (is.na(inputType)) {inputType <- 'text'}
          
          if (inputType == "locked") {
            #skip locked fields
            next
          }
          
          if (selectedDict$var_type[which(selectedDict$pk_fieldname == i)] %in% c("INTEGER", "REAL")) {
            #make sure updates to null in numeric inputs get recorded
            if (is.null(input[[paste0("new_", i)]]) | is.na(input[[paste0("new_", i)]]) | input[[paste0("new_", i)]] == "") {
              params[i] <- NA
            } else {
              #numbers with no quotes
              params[i] <- input[[paste0("new_", i)]]
            }
            
          } else if(inputType == "date") {
            if (length(input[[paste0("new_", i)]]) == 0) {
              params[i] <- NA
            } else {
              #convert dates to string
              params[i] <- as.character(input[[paste0("new_", i)]])
            }
          } else if (inputType == "time") {
            #convert times to string
            timeVals <- sapply(
              c('-hours', '-minutes', '-seconds'), 
              FUN = function(x) {input[[paste0("new_", i, x)]]}
            )
            if (all(timeVals == "") || (timeVals[1] == "" & any(timeVals[2:3] != ""))) {
              timeVals <- NA
            } else {
              timeVals[timeVals == ""] <- "00"
              timeVals <- paste(timeVals, collapse = ":")
            }
            params[i] <- timeVals
          } else if(inputType == "checkbox") {
            params[i] <- as.numeric(input[[paste0("new_", i)]])
          } else {
            newVal <- ifelse(
              test = input[[paste0("new_", i)]] == "",
              yes = NA,
              no = input[[paste0("new_", i)]]
            )
            params[i] <- newVal
          }
        }
        
        #create statement
        updateStatement <- paste(
          "UPDATE", tableName, "SET",
          paste(paste0(names(params), " = $", 1:length(params)), collapse = ', ')
        )
        
        # Add WHERE clause to UPDATE statement
        if (length(tablePk) >= 1) {
          i_pk <- (length(params) + 1):(length(params) + length(tablePk))
          updateStatement <- paste0(
            updateStatement,
            " WHERE ",
            paste(
              paste0(tablePk, " = $", i_pk), 
              collapse = " AND "
            ),
            ";"
          )
          params[i_pk] <- selectedRow[tablePk]
        } else {
          updateStatement <- paste0(updateStatement, "rowid = ", rowIndex, ";")
        }
        
        #insert into database
        tryCatch(
          expr = {
            
            #update table
            rs <- DBI::dbSendStatement(con(), updateStatement)
            DBI::dbBind(rs, unname(params))
            updateResult <- DBI::dbGetRowsAffected(rs)
            DBI::dbClearResult(rs)
            
            #only runs if successful
            update_trigger(update_trigger() + 1)
            
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
    
    #delete row button-------------------
    observeEvent(
      eventExpr = input$delete,
      handlerExpr = {
        showModal(
          modalDialog(
            title = "Confirm deletion of row with identifiers:",
            HTML(paste(
              paste(
                row_click_R$pkFields$pk_fieldname, 
                row_click_R$selectedRow[row_click_R$pkFields$pk_fieldname], 
                sep = ' = '
              ),
              '<br><br>',
              {
                if (row_click_R$tableName %in% c('media', 'spatials', 'models', 'logs')) {
                  checkboxInput(
                    NS(id, 'delete_file'),
                    'Delete associated file'
                  )
                } else {
                  shinyjs::hidden(checkboxInput(
                    NS(id, 'delete_file'),
                    'Delete associated file'
                  ))
                }
              },
              collapse = ', '
            ), 
            '<br><br><b>This action cannot be undone.</b>'),
            footer = tagList(
              actionButton(
                inputId = NS(id, "confirm_delete"),
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
    observeEvent(
      eventExpr = input$confirm_delete,
      handlerExpr = {
        # Remove db record, along with any associated files and SB records
        rs <- AMMonitor::deleteRecord(con = con(), table_name = row_click_R$tableName, selected_row = row_click_R$selectedRow, delete_file = input$delete_file, media_paths = list(IMG_PATH = IMG_PATH(), AUDIO_PATH = AUDIO_PATH()))
        
        # Display results of attempted deletion
        if (rs$status) {
          update_trigger(update_trigger() + 1)
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
    ) #end observe delete confirmation
    
    # Downloadable csv of selected table ----
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste(id, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(dbReadTable(con(), id), file, row.names = FALSE, na = "")
      }
    )
    
    #refresh table (update trigger) ----------------------
    observeEvent(
      eventExpr = input$refresh,
      handlerExpr = {
        update_trigger(update_trigger() + 1)
      }
    )
  }) #end moduleServer
}
