#' @name equipModelAdd
#' @title Adds an equipment model, settings, and options to an AMMonitor
#' database
#' @description This function adds a new equipment model, along with any 
#' settings and associated options, to an AMMonitor database using an Excel
#' spreadsheet. See Details for full instructions.
#' @param con An open connection to an AMMonitor database
#' @param excel_fp A filepath to an Excel file containing new equipment model 
#' information.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage equipModelAdd(con, excel_fp, disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery dbBegin dbAppendTable
#' dbRollback dbCommit
#' @importFrom readxl read_excel
#' @return An updated database with new equipment model info
#' @export
#' @details
#' The equipModelAdd() function simplifies the process of adding new equipment
#' models, settings, and options to an AMMonitor database using an Excel
#' spreadsheet. This approach can be useful for adding equipment models
#' programmatically, or for adding an equipment model to multiple projects. New
#' equipment models can also be added using the equipManager app; for more
#' information, see the "equipment" tutorial.
#' 
#' A template spreadsheet is provided and can be copied from the
#' package to a directory of your choice using the following code:
#' 
#' \code{fp <- system.file("extdata/equipModelAdd.xlsx", package = "AMMonitor")}
#'  
#' \code{file.copy(from = fp, to = tempdir(), overwrite = FALSE)}
#' 
#' Open the copied Excel file and fill out the listed fields for the equipment
#' model you'd like to add. Do not alter the original spreadsheet; AMMonitor
#' expects a specific format to successfully add the new entries to the 
#' database. Each table in the spreadsheet corresponds to a database table:
#' 
#' Table 1: EquipModel (Blue) corresponds to the equipmodels table.
#' 
#' Table 2: Settings (Green) corresponds to the esettingnames table.
#' 
#' Table 3: Options (Orange) corresponds to the esettingoptions table.
#' 
#' Fields in the spreadsheet represent the database columns of the same names.
#' For more information about these tables and adding equipment models to an
#' AMMonitor database, see the "equipment" tutorial. It can be accessed with the
#' following code:
#' \code{learnr::run_tutorial(name = "equipment", package = "AMMonitor")}
#' 
#' @examples
#' \dontrun{
#' 
#' # Create a demo database
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Connect to the database
#' conx <- dbSetCon(file.path(demo_fp, "/database/demo.sqlite"))
#' 
#' # Copy the template spreadsheet to your demo directory
#' fp <- system.file("extdata/equipModelAdd.xlsx", package = "AMMonitor")
#' file.copy(from = fp, to = demo_fp, overwrite = FALSE)
#' 
#' # Fill in the tables with example equipment model information
#' 
#' # Add the model to the database
#' equipModelAdd(
#'   con = conx,
#'   excel_fp = file.path(demo_fp, "equipModelAdd.xlsx"),
#'   disconnect = FALSE)
#'   
#' # Check the updated equipmodels table
#' DBI::dbReadTable(conx, "equipmodels")
#' 
#' # Check the updated esettingnames table
#' DBI::dbReadTable(conx, "esettingnames")
#' 
#' # Check the updated esettingoptions table
#' DBI::dbReadTable(conx, "esettingoptions")
#' 
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' }
#' 

equipModelAdd <- function(con, excel_fp, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Read in the equipmodels table for checks
  equip_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM equipmodels"
  )
  
  # Read in the tables from the Excel spreadsheet
  equip_info <- readxl::read_excel(
    path = excel_fp,
    range = "A1:D2",
    col_types = rep("text", 4)
  )
  
  setting_info <- readxl::read_excel(
    path = excel_fp,
    range = "A4:B19",
    col_types = rep("text", 2)
  )
  
  option_info <- readxl::read_excel(
    path = excel_fp,
    range = "D4:F19",
    col_types = rep("text", 3)
  )
  
  # Remove rows of all NAs
  setting_info <- setting_info[rowSums(is.na(setting_info)) < ncol(setting_info), ]
  option_info <- option_info[rowSums(is.na(option_info)) < ncol(option_info), ]
  
  # Make sure the equipmodel isn't already in the database
  if (equip_info$Equipment_Model_ID %in% equip_check$pk_equipmodelid) stop(
    "The provided equipment model ID is already in use. Please check your database and choose a new ID."
  )
  
  # Make sure the Equip_Type is valid if the list is somehow broken
  if (!(equip_info$Equip_Type %in% 
        c("camera", "recorder", "cell phone", "video camera", "drone"))) stop(
    "The provided equipment type is invalid. Valid entries include 'camera',
    'recorder', 'cell phone', 'video camera', and 'drone'."
  )
  
  # Add the equipment model -------------------------------------------------
  
  # Format the spreadsheet data like the equipmodels table
  equip_to_add <- data.frame(
    pk_equipmodelid = equip_info$Equipment_Model_ID,
    equip_type = equip_info$Equip_Type,
    manufacturer = equip_info$Manufacturer,
    user_manual = equip_info$User_Manual
  )
  
  # Start a database transaction so it can be rolled back if there's an error
  DBI::dbBegin(con)
  
  # Start error catch
  test <- try({
  
  # Append to the equipmodels table
  t <- try(
    DBI::dbAppendTable(
      conn = con,
      name = "equipmodels",
      value = equip_to_add
    )
  )
  
  # Stop, rollback, and output error if there's an issue
  if ("try-error" %in% class(t)) {
    try(DBI::dbRollback(con))
    stop(
      paste0("Error: ", t, 
             " /n Changes to the database have been rolled back.")
    )
  }
  
  # Add equipment settings ----------------------------------------------------
  
  # Construct the setting table to add
  settings_to_add <- data.frame(
    setting_name = setting_info$Setting_Name,
    fk_equipmodelid = rep(equip_info$Equipment_Model_ID, nrow(setting_info)),
    description = setting_info$Setting_Description
  )
  
  # Add the setting names to the database
  t <- try(
    DBI::dbAppendTable(
      conn = con,
      name = "esettingnames",
      value = settings_to_add
    )
  )
  
  # Stop, rollback, and output error if there's an issue
  if ("try-error" %in% class(t)) {
    try(DBI::dbRollback(con))
    stop(
      paste0("Error: ", t, 
             " /n Changes to the database have been rolled back.")
    )
  }
  
  # Add setting options ------------------------------------------------------
  
  # Get the new esetting primary keys
  setting_ids <- DBI::dbGetQuery(
    con,
    "SELECT pk_esettingnameid, setting_name FROM esettingnames"
  )
  
  # Match the IDs to the options by setting_name
  options_w_ids <- merge(
    option_info, setting_ids,
    by.x = "Setting_Name", by.y = "setting_name",
    all.x = TRUE)
  
  # Construct options to add to the database
  options_to_add <- data.frame(
    fk_esettingnameid = options_w_ids$pk_esettingnameid,
    option_name = options_w_ids$Setting_Option,
    description = options_w_ids$Option_Description
  )
  
  # Append to the database
  t <- try(
    DBI::dbAppendTable(
      conn = con,
      name = "esettingoptions",
      value = options_to_add
    )
  )
  
  # Stop, rollback, and output error if there's an issue
  if ("try-error" %in% class(t)) {
    try(DBI::dbRollback(con))
    stop(
      paste0("Error: ", t, 
             " /n Changes to the database have been rolled back.")
    )
  }
  
  }) # End of try
  
  # If all has gone well, commit the changes. Otherwise, rollback
  if ("try-error" %in% class(test)) {
    try(DBI::dbRollback(con))
    stop(
      paste0("Error: ", test, 
             " /n Changes to the database have been rolled back.")
    )
  } else {
    DBI::dbCommit(con)
    print("New equipment model added.")
  }
  
}