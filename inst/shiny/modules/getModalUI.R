getModalUI <- function(tableName, id, dictionary, con, isSubtable, rowVals = list()) {
  lapply(
    X = dbListFields(con, tableName),
    FUN = function(i) {
      inputRow <- dictionary[dictionary$pk_fieldname == i,]

      if (is.na(inputRow$shiny_input)) {
        inputRow$shiny_input <- 'text'
      }
      
      #make different inputs depending on column type
      if (tolower(inputRow$shiny_input) == "locked") {
        #skip locked ones (can't assign inputs)
        paste0(i, " is a locked field.")
        
      } else if (tolower(inputRow$shiny_input) == "numeric") {
        # Get current value for the numeric input
        # Are there existing values? If so, it's an edit, else use default value
        if (length(rowVals[[i]])) {
          val <- rowVals[[i]]
        } else {
          val <- inputRow$default_value
        }
        
        #numeric input
        numericInput(
          inputId = NS(id, paste0("new_", i)),
          label = i,
          min = inputRow$min,
          max = inputRow$max,
          value = val
        )
        
      } else if (tolower(inputRow$shiny_input) == "checkbox") {
        #checkbox
        checkboxInput(
          inputId = NS(id, paste0("new_", i)),
          label = i,
          value = ifelse(length(rowVals[[i]]) == 0 || !rowVals[[i]] %in% c(0,1), 0, rowVals[[i]])
        )
        
      } else if (tolower(inputRow$shiny_input) == "date") {
        #date input
        dateInput(
          inputId = NS(id, paste0("new_", i)),
          label = i,
          format = "yyyy-mm-dd", 
          value = rowVals[[i]]
        )
        
      } else if (tolower(inputRow$shiny_input) == "time") {
        #time input (dropdowns)
        tagList(
          i,
          fluidRow(
            column(
              width = 3,
              selectizeInput(
                inputId = NS(id, paste0("new_", i, "-hours")),
                label = "Hours",
                choices = c("", formatC(00:23, width = 2, format = "d", flag = "0")),
                selected = gsub("(\\d*).*", "\\1", rowVals[[i]])
              )
            ),
            column(
              width = 3,
              selectizeInput(
                inputId = NS(id, paste0("new_", i, "-minutes")),
                label = "Minutes",
                choices = c("", formatC(00:59, width = 2, format = "d", flag = "0")),
                selected = gsub("\\d+:(\\d+).*", "\\1", rowVals[[i]])
              )
            ),
            column(
              width = 3,
              selectizeInput(
                inputId = NS(id, paste0("new_", i, "-seconds")),
                label = "Seconds",
                choices = c("", formatC(00:59, width = 2, format = "d", flag = "0")),
                selected = gsub("\\d+:\\d+:(\\d+)", "\\1", rowVals[[i]])
              )
            )
          )
        )
        
      } else if (tolower(inputRow$shiny_input) == "foreignkey") {
        # If there are too many options, use a text input
        # Otherwise use selectize input
        n_keys <- DBI::dbGetQuery(
          con, 
          statement = paste0("SELECT ROUND(COUNT(*)) FROM ", inputRow$foreign_key_table, ";")
        )
        
        if (n_keys > 1000) {
          if (isSubtable && (i %in% names(rowVals))) {
            shinyjs::disabled(textInput(
              inputId = NS(id, paste0("new_", i)),
              label = i,
              value = rowVals[[i]]
            ))
          } else {
            textInput(
              inputId = NS(id, paste0("new_", i)),
              label = i,
              value = rowVals[[i]]
            )
          }
        } else {
          fk_choices <- c("", DBI::dbGetQuery(
            conn = con,
            statement = paste0(
              "SELECT DISTINCT ", 
              inputRow$foreign_key_field, 
              " FROM ", 
              inputRow$foreign_key_table, 
              ";"
            )
          )[[inputRow$foreign_key_field]])
          if (isSubtable && (i %in% names(rowVals))) {
            shinyjs::disabled(selectizeInput(
              inputId = NS(id, paste0("new_", i)),
              label = i,
              choices = fk_choices,
              selected = rowVals[[i]]
            ))
          } else {
            selectizeInput(
              inputId = NS(id, paste0("new_", i)),
              label = i,
              choices = fk_choices,
              selected = rowVals[[i]]
            )
          }
        }
        
        
      } else if (tolower(inputRow$shiny_input) == "dropdown") {
        #select input with query to list in fk_listid
        selectizeInput(
          inputId = NS(id, paste0("new_", i)),
          label = i,
          choices = c(
            "",
            DBI::dbGetQuery(
              conn = con,
              statement = paste0(
                "SELECT DISTINCT item FROM listitems WHERE fk_listid = '", 
                inputRow$fk_listid, 
                "';"
              )
            )[["item"]]
          ),
          selected = rowVals[[i]]
        )
        
      } else if (tolower(inputRow$shiny_input) == "radio") {
        #radioButtons with query to list in fk_listid
        radioButtons(
          inputId = NS(id, paste0("new_", i)),
          label = i,
          choices = DBI::dbGetQuery(
            conn = con,
            statement = paste0(
              "SELECT DISTINCT item FROM listitems WHERE fk_listid = '", 
              inputRow$fk_listid, 
              "';"
            )
          )[["item"]], 
          selected = rowVals[[i]]
        )
        
      } else if (tolower(inputRow$shiny_input) %in% c("text", "restrictedtext")) {
        #short text
        textInput(
          inputId = NS(id, paste0("new_", i)),
          label = i,
          placeholder = inputRow$shiny_placeholder,
          value = rowVals[[i]]
        )
        
      } else {
        #longtext
        textAreaInput(
          inputId = NS(id, paste0("new_", i)),
          label = i,
          placeholder = inputRow$shiny_placeholder, 
          value = rowVals[[i]]
        )
      }
    }
  )
}