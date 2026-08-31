#' @name modelAddConfig
#' @title Adds a model configuration to an AMMonitor database
#' @description This function adds a model configuration to an AMMonitor
#' database and updates all associated tables. See details for further info.
#' @param con An open connection to an AMMonitor database
#' @param excel_fp A filepath to an Excel file containing new model 
#' configuration information.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage modelAddConfig(con, excel_fp, disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery dbBegin dbAppendTable 
#' dbRollback dbSendQuery dbBind dbFetch dbClearResult dbCommit
#' @importFrom readxl read_excel
#' @return An updated database containing a new model configuration
#' @export
#' @details
#' The modelAddConfig() function is designed to enable easy addition of new
#' model configurations to an AMMonitor database using an Excel spreadsheet. You
#' can generate an Excel spreadsheet for a specific model using the
#' modelConfigWriteXL() function. 
#' 
#' If the model hasn't been added to the database yet, see the modelAdd() 
#' function. Models and configurations can also be added to the database using
#' the modelManager app in AMMonitor; see the "models" tutorial for more
#' information on both methods.
#' 
#' To add a configuration via spreadsheet, open up the newly generated 
#' spreadsheet and fill out the fields to create a new model configuration. Each
#' of the three sheets corresponds to different database tables:
#' 
#' Configuration_Name corresponds to the mconfignames table and associated
#' fields. The model ID is automatically filled in when the spreadsheet is
#' generated; other fields should be filled in.
#' 
#' Configuration_Values corresponds to the mconfigvalues table and provides the
#' names and descriptions of all valid settings for the specified model, as well
#' as Option_Name and Value columns to choose settings. Fill in the Option_Name
#' and Value columns, and leave a blank cell for empty entries.
#' 
#' Setting_Options corresponds to the msettingnames and msettingoptions tables.
#' It provides a list of all valid options for settings with options in the
#' database. This sheet does not need to be updated, but can be used for
#' reference when selecting setting options.
#' 
#' You can find more information about model configurations and the associated
#' database tables in the "models" tutorial, including a full walkthrough of
#' adding new configurations. You can launch this tutorial as follows:
#' \code{learnr::run_tutorial(name = "models", package = "AMMonitor")}
#' 
#' @examples
#' \dontrun{
#' # Create a demo database
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Connect to the database
#' conx <- dbSetCon(file.path(demo_fp, "/database/demo.sqlite"))
#' 
#' # Take a look at the models table
#' DBI::dbReadTable(conx, "models")
#' 
#' # Let's make a spreadsheet for model 6, BirdNET, and write it to the demo dir
#' modelConfigWriteXL(
#'   con = conx,
#'   modelid = 6,
#'   output_dir = demo_fp,
#'   disconnect = FALSE)
#' 
#' # Fill in the spreadsheet with example model configuration info
#' 
#' # Add your new configuration to the database
#' modelAddConfig(
#'   con = conx,
#'   excel_fp = file.path(demo_fp, "modelAddConfig.xlsx")),
#'   disconnect = FALSE)
#'   
#' # Run a query to see your new model configuration
#' qryMconfiguration(
#'   con = conx, 
#'   mconfigid = 3,
#'   disconnect = FALSE)
#' }
#' 

modelAddConfig <- function(con, excel_fp, disconnect = FALSE) {
  
  # Setup and Error Checks ------------------------------------------
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Read in mconfignames table for checks
  config_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM mconfignames"
  )
  
  # Read in the models table for checks
  models_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM models"
  )
  
  # Read in model settings and options for checks
  names_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM msettingnames"
  )
  
  options_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM msettingoptions"
  )
  
  # Read in the excel info
  config_name <- readxl::read_excel(
    path = excel_fp,
    sheet = "Configuration_Name",
    col_types = c("text", "numeric", "text", "text", "numeric")
  )
  
  config_values <- readxl::read_excel(
    path = excel_fp,
    sheet = "Configuration_Values",
    col_types = c("text", "text", "text", "numeric")
  )
  
  # Remove rows of all NAs
  config_values <- config_values[rowSums(is.na(config_values)) < ncol(config_values), ]
  
  # Make sure the mconfigname isn't already used to avoid confusion
  if (config_name$mconfigname %in% config_check$mconfigname) stop(
    "The provided model configuation name is already in use. Please choose a 
    new model configuration name."
  )
  
  model_w_config <- subset(models_check, pk_modelid == config_name$fk_modelid)
  
  # Limit existing configs list to just this model
  config_check <- subset(config_check, fk_modelid == model_w_config$pk_modelid)
  
  # If the config should be default, make sure there's no existing default
  if (config_name$is_default == 1 & any(config_check$is_default == 1)) stop(
    "The listed model already has a default configuration. Update the existing
    default configuration, or set is_default to 0."
  )
  
  # Filter settings for just this model as well
  names_check <- subset(names_check, fk_modelid == model_w_config$pk_modelid)
  options_check <- subset(
    options_check, 
    fk_msettingnameid %in% names_check$pk_msettingnameid)
  
  # Make sure all setting names are listed for the model
  if (any(!(config_values$Setting_Name %in% names_check$setting_name))) stop(
    "All model settings listed were not found in the database. Check your
    msettingnames table to see listed settings, or add new settings."
  )
  
  # Make sure all setting options are listed for the setting names
  if (any(!(config_values$Option_Name[!(is.na(config_values$Option_Name))]) 
             %in% options_check$option_name)) stop(
    "All selected setting options were not found in the database. Check your
    msettingoptions table to see available options, or add new options."
  )
  
  # Add mconfigname ----------------------------------------------------
  
  # Format data like the mconfigname table
  config_to_add <- data.frame(
    mconfigname = config_name$mconfigname,
    fk_modelid = model_w_config$pk_modelid,
    description = config_name$description,
    filename = config_name$filename,
    is_default = config_name$is_default
  )
  
  # Begin a database transaction so it can be rolled back if needed
  DBI::dbBegin(con)
  
  # Wrap everything from here in a giant try to catch and rollback errors
  test <- try({
  
  # Attempt to append the new config
  t <- try(
    DBI::dbAppendTable(
      conn = con,
      name = "mconfignames",
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
  
  # Get the pk_mconfignameid for the new model configuration
  rs <- DBI::dbSendQuery(
    con,
    "SELECT pk_mconfignameid FROM mconfignames WHERE mconfigname = $1"
  )
  DBI::dbBind(rs, list(config_name$mconfigname))
  mconfig_id <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
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
        pk_msettingoptionid = rep(NA, nrow(settings_w_keys)))
    )
  }
  
  # Construct the mconfigvalues table
  values_to_add <- data.frame(
    fk_mconfignameid = rep(as.numeric(mconfig_id), nrow(settings_w_ops)),
    fk_msettingnameid = settings_w_ops$pk_msettingnameid,
    fk_msettingoptionid = settings_w_ops$pk_msettingoptionid,
    value_num = settings_w_ops$Value
  )
  
  # Attempt to append to mconfigvalues
  t <- try(
    DBI::dbAppendTable(
      conn = con,
      name = "mconfigvalues",
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
    print("New model configuration added.")
  }
  
}