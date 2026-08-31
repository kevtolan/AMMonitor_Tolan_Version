#' @name equipConfigWriteXL
#' @title Writes an Excel spreadsheet to be used to easily add a new equipment
#' model configuration to an AMMonitor database
#' @description This function writes an Excel spreadsheet to a provided 
#' directory which can be used to easily add a new equipment configuration to an
#' AMMonitor database. This spreadsheet contains multiple sheets to specify
#' configuration names and values, as well as listing all valid settings for the
#' provided equipment model. For more information, see the Details section.
#' @param con An open connection to an AMMonitor database
#' @param equipmodelid The pk_equipmodelid of the equipment model you'd like to add a
#' configuration for, as listed in the "equipmodels" table
#' @param output_dir A filepath to the directory where you'd like the output
#' Excel spreadsheet to be saved.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage equipConfigWriteXL(con, equipmodelid, output_dir, disconnect = FALSE)
#' @return An Excel spreadsheet containing tables to add a new equipment model 
#' configuration to the database in the specified directory
#' @details
#' This function creates an Excel spreadsheet to enable the easy addition of new
#' equipment configuration to an AMMonitor database. It queries the database
#' based on the provided equipment model ID to output a spreadsheet that 
#' includes relevant settings and options for the selected model. This
#' spreadsheet can be completed and input into equipAddConfig() to update the
#' database with a new equipment configuration.
#' 
#' New equipment configurations can also be added using the equipManager app in
#' AMMonitor. For more information on using the app, see the "equipment" 
#' tutorial.
#' 
#' The output spreadsheet will be named "equipAddConfig.xlsx" and stored in the
#' specified output directory. It contains three sheets:
#' 
#' Configuration_Name corresponds to the econfignames table. The fk_equipmodelid
#' will be automatically filled in based on the equipment model specified in
#' this function. All other details can be filled in for the new configuration.
#' 
#' Configuration_Values corresponds to the econfigvalues table. This sheet will
#' include rows for all settings associated with the equipment model specified,
#' along with descriptions and Option_Name and Value columns to fill in.
#' 
#' Setting_Options provides a list of all valid entries for settings with 
#' defined options in the database. If a setting has no options listed, it takes
#' a numeric entry in the Value column.
#' 
#' For information on adding a completed spreadsheet to the database, see
#' equipAddConfig(). For a full walkthrough, see the "equipment" tutorial,
#' which can be launched with the following code:
#' \code{learnr::run_tutorial(name = "equipment", package = "AMMonitor")}
#' 
#' @importFrom DBI dbIsValid dbDisconnect
#' @importFrom writexl write_xlsx
#' @export
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
#' }
#' 

equipConfigWriteXL <- function(con, equipmodelid, output_dir, 
                               disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Query the database for settings for the listed model
  settings <- qryEquipModelSettings(
    con = con,
    equipmodelid = equipmodelid,
    disconnect = FALSE
  )
  
  # Create a dataframe for the model config name
  econfigname <- data.frame(
    econfigname = "Enter Configuration Name",
    fk_equipmodelid = equipmodelid,
    description = "Enter description",
    filename = "Enter a filename, or leave blank"
  )
  
  # Create a dataframe for config values
  configvalues <- data.frame(
    Setting_Name = unique(settings$setting_name),
    Description = unique(settings$setting_description),
    Option_Name = rep(NA, length(unique(settings$setting_name))),
    Value = rep(NA, length(unique(settings$setting_name)))
  )
  
  # Produce a list of valid options
  if (any(!(is.na(settings$option_name)))) {
    
    Valid_Options <- aggregate(
      option_name ~ setting_name,
      data = settings,
      FUN = function(x) paste(x, collapse = ", ")
    )
    colnames(Valid_Options) <- c("Setting_Name", "Valid_Options")
    
    # Merge with configvalues
    configvalues <- merge(
      configvalues, Valid_Options,
      all.x = TRUE
    )
    
  }
  
  # Create a dataframe of setting options
  settingoptions <- data.frame(
    Setting_Name = settings$setting_name,
    Option_Name = settings$option_name,
    Option_Description = settings$option_description
  )
  
  # Turn all of these dataframes into a named list
  named_list <- list(
    Configuration_Name = econfigname,
    Configuration_Values = configvalues,
    Setting_Options = settingoptions
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(output_dir, "equipAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
}