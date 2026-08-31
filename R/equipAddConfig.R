#' @name equipAddConfig
#' @title Adds an equipment configuration to an AMMonitor database
#' @description This function adds a new equipment configuration to an AMMonitor
#' database and updates all associated tables. 
#' @param con An open connection to an AMMonitor database
#' @param excel_fp A filepath to an Excel file containing new equipment
#' configuration information.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage equipAddConfig(con, excel_fp, disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery dbBegin dbAppendTable
#' dbRollback dbCommit
#' @importFrom readxl read_excel
#' @return An updated database containing the new equipment configuration
#' @export
#' @details
#' The equipAddConfig function can be used to easily add new equipment 
#' configurations to an AMMonitor database through a simple Excel spreadsheet.
#' This process updates all relevant AMMonitor tables. A spreadsheet can be
#' generated for a specific equipment model using the equipConfigWriteXL()
#' function. 
#' 
#' This workflow is useful for adding equipment configurations programmatically,
#' or for adding a configuration to multiple projects (in this case, check your
#' keys). Configurations can also be added through the AMMonitor app using the
#' equipManager app; see the "equipment" tutorial for more information.
#' 
#' If an equipment model isn't in the database yet, see equipModelAdd().
#' 
#' Open your generated spreadsheet and fill out each sheet to create a new
#' equipment configuration. Each sheet corresponds to a database table as
#' follows:
#' 
#' Configuration_Name corresponds to the econfignames table. The fk_equipmodelid
#' should be automatically filled in when the spreadsheet is created. All other
#' fields should be filled in.
#' 
#' Configuration_Values corresponds to the econfigvalues table. This sheet will
#' include rows for all settings associated with the equipment model specified,
#' along with descriptions and Option_Name and Value columns to fill in. If
#' a setting has options, they will be listed next to the row. Please fill in
#' the Option_Name and Value columns. Leave an empty cell for blank entries.
#' 
#' Setting_Options provides a list of all valid entries for settings with 
#' defined options in the database. If a setting has no options listed, it takes
#' a numeric entry in the Value column. This sheet does not need to be edited, 
#' but should be used for reference information about any setting options.
#' 
#' For more information about equipment configurations and the associated 
#' tables, or for a full walkthrough, refer to the "equipment" tutorial, 
#' which can be run using the following code:
#' \code{learnr::run_tutorial(name = "equipment", package = "AMMonitor")}
#' 
#' @examples
#' \dontrun{
#' # Create a demo database
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Connect to the database
#' conx <- dbSetCon(file.path(demo_fp, "/database/demo.sqlite"))
#'
#' # Take a look at the equipmodels table
#' DBI::dbReadTable(conx, "equipmodels")
#' 
#' # Let's make a spreadsheet for recorder_model_x and write it to the demo dir
#' equipConfigWriteXL(
#'   con = conx,
#'   equipmodelid = "recorder_model_x",
#'   output_dir = demo_fp,
#'   disconnect = FALSE)
#' 
#' # Fill in the spreadsheet with equipment configuration info
#' print(paste(
#'   "To complete this demo, open the excel file at",
#'   file.path(demo_fp, "equipAddConfig.xlsx"),
#'   "and add the configuration info.",
#'   "Note: the model name and config options are",
#'   "pre-populated in this spreadsheet."
#' ))
#' 
#' # Add the new configuration to the database
#' equipAddConfig(
#'   con = conx,
#'   excel_fp = file.path(demo_fp, "equipAddConfig.xlsx"),
#'   disconnect = FALSE)
#'   
#' # Retrieve config name from spreadsheet
#' config_name <- readxl::read_excel(
#'   file.path(demo_fp, "equipAddConfig.xlsx"), 
#'   sheet = "Configuration_Name", 
#'   col_names = FALSE, 
#'   range = "A2"
#' )
#'   
#' # Query the database for your new configuration
#' qryEconfiguration(
#'   con = conx,
#'   econfigname = config_name,
#'   disconnect = FALSE)
#'   
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' }
#' 

equipAddConfig <- function(con, excel_fp, disconnect = FALSE) {
  
  # Setup and Error Checks ------------------------------------------
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Read in econfignames table for checks
  config_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM econfignames"
  )
  
  # Read in the equipmodels table for checks
  equip_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM equipmodels"
  )
  
  # Read in equipment settings and options for checks
  names_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM esettingnames"
  )
  
  options_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM esettingoptions"
  )
  
  # Read in the excel info
  config_name <- readxl::read_excel(
    path = excel_fp,
    sheet = "Configuration_Name",
    col_types = rep("text", 4)
  )
  
  config_values <- readxl::read_excel(
    path = excel_fp,
    sheet = "Configuration_Values",
    col_types = c("text", "text", "text", "numeric", "text")
  )
  
  # Remove rows of all NAs
  config_values <- config_values[rowSums(is.na(config_values)) < ncol(config_values), ]
  
  # Make sure the econfigname isn't already used to avoid confusion
  if (config_name$econfigname %in% config_check$econfigname) stop(
    "The provided model configuation name is already in use. Please choose a 
    new model configuration name."
  )
  
  equip_w_config <- subset(equip_check, 
                           pk_equipmodelid == config_name$fk_equipmodelid)
  
  # Limit existing configs list to just this model
  config_check <- subset(config_check, 
                         fk_equipmodelid == equip_w_config$pk_equipmodelid)
  
  # Filter settings for just this model as well
  names_check <- subset(
    names_check, 
    fk_equipmodelid == equip_w_config$pk_equipmodelid)
  options_check <- subset(
    options_check, 
    fk_esettingnameid %in% names_check$pk_esettingnameid)
  
  # Make sure all setting names are listed for the model
  if (any(!(config_values$Setting_Name %in% names_check$setting_name))) stop(
    "All equipment settings listed were not found in the database. Check your
    esettingnames table to see listed settings, or add new settings."
  )
  
  # Make sure all setting options are listed for the setting names
  if (any(!(config_values$Option_Name[!(is.na(config_values$Option_Name))]) 
        %in% options_check$option_name)) stop(
    "All selected setting options were not found in the database. Check your
    esettingoptions table to see available options, or add new options."
  )
  
  # Add econfigname ----------------------------------------------------
  
  # Format data like the econfigname table
  config_to_add <- data.frame(
    pk_econfignameid = config_name$econfigname,
    fk_equipmodelid = equip_w_config$pk_equipmodelid,
    description = config_name$description,
    filename = config_name$filename
  )
  
  # Begin a database transaction so it can be rolled back if needed
  DBI::dbBegin(con)
  
  # Start try to catch and rollback errors
  test <- try({
  
  # Attempt to append the new config
  t <- try(
    DBI::dbAppendTable(
      conn = con,
      name = "econfignames",
      value = config_to_add
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
  
  # Add config values --------------------------------------------------------
  
  # Match setting names to primary keys
  settings_w_keys <- merge(
    config_values, names_check,
    by.x = "Setting_Name", by.y = "setting_name")
  
  # Merge with options to match option names
  if (nrow(options_check) > 0) {
    settings_w_ops <- merge(
      settings_w_keys, options_check,
      by.x = "Option_Name", by.y = "option_name",
      all.x = TRUE
    )
  } else {
    settings_w_ops <- cbind(
      settings_w_keys, data.frame(
        pk_esettingoptionid = rep(NA, nrow(settings_w_keys)))
    )
  }
  
  # Construct the econfigvalues table
  values_to_add <- data.frame(
    fk_econfignameid = rep(config_name$econfigname, nrow(settings_w_ops)),
    fk_esettingnameid = settings_w_ops$pk_esettingnameid,
    fk_esettingoptionid = settings_w_ops$pk_esettingoptionid,
    value_num = settings_w_ops$Value
  )
  
  # Attempt to append to econfigvalues
  t <- try(
    DBI::dbAppendTable(
      conn = con,
      name = "econfigvalues",
      value = values_to_add
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
    print("New equipment configuration added.")
  }
  
}