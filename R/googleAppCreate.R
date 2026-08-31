#' @name googleAppCreate
#' @title Creates a data source spreadsheet for Google AppSheet from 
#' an AMMonitor database
#' @description This function creates an Excel workbook from a 
#' given AMMonitor database that can be used to generate a 
#' new visit survey in Google AppSheet. The spreadsheet is created in
#' your working directory.
#' 
#' This function should be used after updating the database 
#' with any new locations, people, equipment, user fields, etc. 
#' This will prevent data entry errors 
#' where a new location, person, etc. is not available for 
#' survey users. 
#'  
#' See details for instructions on creating the survey app 
#' in Google AppSheet
#' @param con Open connection to an AMMonitor database
#' @param equip_type Type of equipment to include from 
#' the database. Options are 'camera', 'recorder', or 'all' ('all' is 
#' the default).
#' @param disconnect TRUE or FALSE. Should the 
#' database connection be severed on exit? 
#' Default is FALSE
#' @details This function writes an Excel file to the user's working 
#' directory with all of the information needed to create a cell phone
#' app in Google AppSheet.
#' The function will exclude 
#' foreign key tables and listitems without entries in the database.
#' 
#'  For detailed step-by-step instructions for creating a cell phone app with
#'  Google AppSheets, please see the
#'  AMMonitor tutorial for 'mobile_apps' by running
#'  \code{learnr::run_tutorial(name = "mobile_apps", package = "AMMonitor")}
#' 
#' @importFrom DBI dbDisconnect dbIsValid dbReadTable
#' @importFrom writexl write_xlsx
#' @usage googleAppCreate(con, equip_type = 'all', disconnect = FALSE)
#' @return filepath to the data source .xlsx file
#' @export
#' @export
#' @examples
#' \dontrun{
#' 
#' # Create a mini demo directory and database
#' demo_fp <- ammCreateMiniDemo(tempdir())
#' 
#' # set path to the database
#' dbPath <- paste0(demo_fp, "/database/demo.sqlite")
#' 
#' # create a connection to the database
#' conx <- dbSetCon(dbPath)
#' 
#' # create the spreadsheet and capture the filepath
#' filepath <- googleAppCreate(
#'   con = conx,
#'   equip_type = 'camera'
#' )
#' 
#' # the file is written to your working directory in R
#' filepath
#' 
#' # now read the mobile_apps tutorial!
#' }

googleAppCreate <- function(con, equip_type = 'all', disconnect = FALSE) {
  
  # disconnect on exit
  if (disconnect == TRUE) {
    on.exit(
      DBI::dbDisconnect(con)
    )
  }
  
  # test connection
  if (DBI::dbIsValid(con) == FALSE) {
    stop('Please enter an open connection to an AMMonitor database.')
  }
  
  # test data_type
  if (equip_type != 'all' &
      equip_type != 'camera' &
      equip_type != 'recorder') {
    stop('Please enter a valid equip_type value: "all", "camera", or "recorder"')
  }
  
  # read in dictionary 
  dictionary <- DBI::dbReadTable(conn = con,
                                 name = 'dbdictionary')
  
  # get dictionary entries for the visits table 
  dict_visits <- dictionary[
    which(dictionary$pk_tablename == 'visits')
    ,]
  
  # remove autonumber for visit id
  index <- which(dict_visits$shiny_input == 'locked')
  
  if (index > 0) {
    dict_visits <- dict_visits[-index,]
  }
  
  # loop through dictionary for visits and add to the survey tab========
  
  survey <- c('ID')
  sheets <- list()
  sheet_names <- c()
  listitems <- DBI::dbReadTable(con, 'listitems')
  
  for (i in 1:nrow(dict_visits)) {
    
    fieldname <- dict_visits$pk_fieldname[i]
    
    if (fieldname == 'fk_listid') next
    
    # foreign keys 
    if (dict_visits$shiny_input[i] == 'foreignKey') {
      
      table <- dict_visits$foreign_key_table[i]
      
      table_data <- DBI::dbReadTable(con, table)
      
      # subset by equip type if argument given
      if (fieldname == 'fk_equipmentid' &
          equip_type != 'all') {
        
        table_data <- table_data[
          which(table_data$equip_type  == equip_type),
        ]
        
        # if no equipment of that type in database, stop
        if (nrow(table_data) == 0) {
          stop(paste0("There are no equipment of type '",
                      equip_type,
                      "' in the database. Please update the 'equipment' table of the database if the equip_type column is NA, or choose another equip_type. The default is 'all'."))
        }
      } # end subset by equip_type
      
      
      # skip if table empty
      if (nrow(table_data) == 0) next
      
      # populate sheet data with table data and row number for googleApp key
      sheet_data <- data.frame(table_data[,1],
                               table_data[,1])
      names(sheet_data) <- c('ID', 'choices')
      
      # add to sheets list
      sheets <- append(sheets, list(sheet_data))
      
      # add to sheet_names list
      sheet_names <- c(sheet_names, fieldname)
      
    } # end foreign keys
    
    # dropdowns
    if (dict_visits$shiny_input[i] == 'dropdown') {
      
      # get list id
      list <- dict_visits$fk_listid[i]
      
      # subset listitems by list
      subset_listitems <- listitems[
        which(listitems$fk_listid == list),
      ]
      
      # for lists without listitems, exclude with message to update db
      if (nrow(subset_listitems) == 0) {
        message(paste0("The list '",
                       list, 
                       " has no entries in the listitems table. It has been excluded from the survey form. You may update the database and re-run this function to include that field."))
        next
      }
      
      # create emtpy sheet dataframe
      sheet_data <- data.frame(
        matrix(nrow = nrow(subset_listitems),
               ncol = 2)
      )
      names(sheet_data) <- c('ID', 'choices')
      
      # populate sheet data
      sheet_data[,1] <- subset_listitems$item
      sheet_data[,2] <- paste0(
        toupper(subset_listitems$item),
        ': ',
        subset_listitems$description
      )
      
      # add to sheets list
      sheets <- append(sheets, list(sheet_data))
      
      # add to sheet_names list
      sheet_names <- c(sheet_names, fieldname)
      
    } # end dropdowns
    
    # add to survey form
    survey <- append(survey, fieldname)
    
  } # end loop through dictionary for visits
  
  # add coordinates
  survey <- append(survey, 'Coordinates')
  
  # create survey sheet with all form columns
  survey_sheet <- data.frame(
    matrix(nrow = 0,
           ncol = length(survey))
  )
  
  colnames(survey_sheet) <- survey
  
  # name the sheets list
  names(sheets) <- sheet_names
  
  # add survey sheet to sheets list object
  sheets <- append(list('survey_fields' = survey_sheet), sheets)
  
  writexl::write_xlsx(sheets,
                      'googleApp_data.xlsx',
                      col_names = TRUE)
  
  message(paste0("A file 'googleApp_data.xlsx' has been written to your working directory, which can now be used as the data source for an app in Google AppSheet."))
  
  return(paste0(getwd(), "/googleApp_data.xlsx"))
  
} # end function
