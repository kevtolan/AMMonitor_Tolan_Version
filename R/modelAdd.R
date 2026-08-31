#' @name modelAdd
#' @title Adds a model, settings, and options to an AMMonitor database
#' @description This function adds a model, along with its settings and any
#' associated options, to an AMMonitor database using an Excel spreadsheet
#' provided with this package. See Details for more information.
#' @param con An open connection to an AMMonitor database
#' @param excel_fp A filepath to an Excel file containing new model information.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage modelAdd(con, excel_fp, disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery dbBegin dbAppendTable
#' dbRollback dbSendQuery dbBind dbFetch dbClearResult dbCommit
#' @importFrom readxl read_excel
#' @return An updated database with new model info
#' @export
#' @details 
#' The modelAdd() function simplifies the process of adding models to your
#' database by allowing you to add a new model, its associated settings, and any
#' setting options in a single function. This approach can be useful for adding
#' models programmatically, or for easily adding a single model to multiple
#' projects. Models can also be added to the database using the modelManager
#' app; full documentation of this app can be found in the "models" tutorial.
#' 
#' This function relies on a template Excel file that is provided with this 
#' package. You can copy the template to a directory of your choice with the 
#' following code:
#' 
#' \code{fp <- system.file("extdata/modelAdd.xlsx", package = "AMMonitor")}
#'  
#' \code{file.copy(from = fp, to = tempdir(), overwrite = FALSE)}
#' 
#' Open this Excel file and fill in the provided fields for the model you'd like
#' to add. Do not alter the original template; AMMonitor relies on this format
#' to read in the provided model info properly. Each table in the spreadsheet 
#' corresponds to a table in the database:
#' 
#' Table 1: Model (Blue) corresponds to the models table.
#' 
#' Table 2: Settings (Green) corresponds to the msettingnames table.
#' 
#' Table 3: Options (Orange) corresponds to the msettingoptions table.
#' 
#' The fields in the spreadsheet correspond to the database tables by name. For
#' more information AMMonitor, see the "models" tutorial, which can be launched 
#' with the following code:
#' \code{learnr::run_tutorial(name = "models", package = "AMMonitor")}
#' 
#' @examples
#' 
#' \dontrun{
#' # Create a demo database
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Connect to the database
#' conx <- dbSetCon(file.path(demo_fp, "/database/demo.sqlite"))
#' 
#' # Copy the template spreadsheet to your demo directory
#' fp <- system.file("extdata/modelAdd.xlsx", package = "AMMonitor")
#' file.copy(from = fp, to = demo_fp, overwrite = FALSE)
#' 
#' # Fill in the tables with example model information
#' 
#' # Add the model to the database
#' modelAdd(
#'   con = conx,
#'   excel_fp = file.path(demo_fp, "modelAdd.xlsx"),
#'   disconnect = FALSE)
#'   
#' # Check the updated models table
#' DBI::dbReadTable(conx, "models")
#' 
#' # Check the updated msettingnames table
#' DBI::dbReadTable(conx, "msettingnames")
#' 
#' # Check the updated msettingoptions table
#' DBI::dbReadTable(conx, "msettingoptions")
#' 
#' # remove the excel template
#' unlink(file.path(tempdir(), "modelAdd.xlsx"))
#' 
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' }
#' 

modelAdd <- function(con, excel_fp, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Read in the models table for checks
  models_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM models"
  )
  
  # Read in the tables from the Excel spreadsheet
  model_info <- readxl::read_excel(
    path = excel_fp,
    range = "A1:F2",
    col_types = rep("text", 6)
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
  setting_info <- setting_info[apply(!is.na(setting_info), MARGIN = 1, FUN = all), ]
  option_info <- option_info[apply(!is.na(option_info), MARGIN = 1, FUN = all), ]
  
  # Make sure the model isn't already in the database
  if (model_info$Model_Name %in% models_check$model_name) stop(
    "The provided model name is already in use. Please choose another model name."
  )
  
  # Adding the model ---------------------------------------------------------
  
  # Format data from the spreadsheet like the models table
  model_to_add <- data.frame(
    model_name = model_info$Model_Name,
    model_url = model_info$URL,
    amml = model_info$Model_Library,
    model_type = model_info$Model_Type,
    model_description = model_info$Description,
    model_citation = model_info$Citation
  )
  
  # Start a database transaction so it can be rolled back if there's an error
  DBI::dbBegin(con)
  
  # Start error catching
  test <- try({
  
  # Append to the models table
  t <- try(
    DBI::dbAppendTable(
    conn = con,
    name = "models",
    value = model_to_add
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
  
  # Adding model settings ----------------------------------------------------
  
  # Get the pk_modelid for the new model
  rs <- DBI::dbSendQuery(
    con,
    "SELECT pk_modelid FROM models WHERE model_name = $1"
  )
  DBI::dbBind(rs, list(model_info$Model_Name))
  model_id <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  # Construct the setting table to add
  settings_to_add <- data.frame(
    setting_name = setting_info$Setting_Name,
    fk_modelid = rep(as.numeric(model_id), nrow(setting_info)),
    description = setting_info$Setting_Description
  )
  
  # Add the setting names to the database
  t <- try(
    DBI::dbAppendTable(
    conn = con,
    name = "msettingnames",
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
  
  # Add setting options ----------------------------------------------------
 
  # Get the new setting name IDs
  rs <- DBI::dbSendQuery(
    con,
    "SELECT pk_msettingnameid, setting_name FROM msettingnames"
  )
  setting_ids <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  # Match the IDs to the options by setting_name
  options_w_ids <- merge(
    option_info, setting_ids,
    by.x = "Setting_Name", by.y = "setting_name",
    all.x = TRUE)
  
  # Construct options to add to the database
  options_to_add <- data.frame(
    fk_msettingnameid = options_w_ids$pk_msettingnameid,
    option_name = options_w_ids$Setting_Option,
    description = options_w_ids$Option_Description
  )
  
  # Append to the database
  t <- try(
    DBI::dbAppendTable(
    conn = con,
    name = "msettingoptions",
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
  
  }) # End of Try
  
  # If all has gone well, commit the changes. Otherwise, rollback
  if ("try-error" %in% class(test)) {
    try(DBI::dbRollback(con))
    stop(
      paste0("Error: ", test, 
             " /n Changes to the database have been rolled back.")
    )
  } else {
    DBI::dbCommit(con)
    print("New model added.")
  }
  
}