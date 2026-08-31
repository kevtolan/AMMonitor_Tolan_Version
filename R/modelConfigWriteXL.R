#' @name modelConfigWriteXL
#' @title Writes an Excel spreadsheet to be used to easily add a new model
#' configuration to an AMMonitor database
#' @description This function writes an Excel spreadsheet to a given directory
#' with sheets to add a new model configuration to an AMMonitor database. Each
#' sheet corresponds to a different database table. For more information, see 
#' the Details section.
#' @param con An open connection to an AMMonitor database
#' @param modelid The pk_modelid of the model you'd like to add a configuration
#' for, as listed in the "models" table
#' @param output_dir A filepath to the directory where you'd like the output
#' Excel spreadsheet to be saved.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage modelConfigWriteXL(con, modelid, output_dir, disconnect = FALSE)
#' @return An Excel spreadsheet containing tables to add a new model 
#' configuration to the database in the specified directory
#' @details
#' This function creates an Excel spreadsheet to enable easy addition of model
#' configuration to an AMMonitor database. It queries the database based on the
#' provided model ID to find all settings and setting options for the given
#' model and provides an organized spreadsheet with relevant entires to create
#' a new model configuration.
#' 
#' The resulting spreadsheet will be named "modelAddConfig.xlsx" and written to
#' the provided output directory. This spreadsheet contains three sheets:
#' 
#' Configuration_Name corresponds to the mconfignames table and associated
#' fields. The model ID is automatically filled in based on the provided modelid
#' in the function.
#' 
#' Configuration_Values corresponds to the mconfigvalues table and provides the
#' names and descriptions of all valid settings for the specified model, as well
#' as Option_Name and Value columns to choose settings.
#' 
#' Setting_Options corresponds to the msettingnames and msettingoptions tables.
#' It provides a list of all valid options for settings with options in the
#' database. An empty row next to a setting indicates that the setting takes a
#' numeric input in the Value column.
#' 
#' Model configurations can also be added using the modelManager app in 
#' AMMonitor. For details on using the app to add configurations, see the
#' "models" tutorial.
#' 
#' For details on adding the configuration to the database via this spreadsheet,
#' see modelAddConfig(). For a full walkthrough, see the "models" tutorial,
#' which can be launched using the following code:
#' \code{learnr::run_tutorial(name = "models", package = "AMMonitor")}
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
#' # Take a look at the models table
#' DBI::dbReadTable(conx, "models")
#' 
#' # Let's make a spreadsheet for model 6, BirdNET, and write it to the demo dir
#' modelConfigWriteXL(
#'   con = conx,
#'   modelid = 6,
#'   output_dir = demo_fp,
#'   disconnect = FALSE)
#' }
#' 

modelConfigWriteXL <- function(con, modelid, output_dir, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Query the database for settings for the listed model
  settings <- qryModelSettings(
    con = con,
    modelid = modelid,
    disconnect = FALSE
  )
  
  # Create a dataframe for the model config name
  mconfigname <- data.frame(
    mconfigname = "Enter Configuration Name",
    fk_modelid = modelid,
    description = "Enter description",
    filename = "Enter a filename, or leave blank",
    is_default = "0 or 1, should this be the default configuration?"
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
    Configuration_Name = mconfigname,
    Configuration_Values = configvalues,
    Setting_Options = settingoptions
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(output_dir, "modelAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
}