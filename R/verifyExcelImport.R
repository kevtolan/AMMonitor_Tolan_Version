#' @name verifyExcelImport
#' @aliases verifyExcelImport
#' @title Import annotation and modeloutput verifications from an Excel 
#' spreadsheet
#' @description Import verifications from an Excel spreadsheet created with
#' verifyExcelCreate to an AMMonitor database
#' @param con An open connection to an AMMonitor database.
#' @param excel_file The file path to the excel file containing the 
#' verifications to be imported.
#' @param verified_what Were annotations or model outputs verified? Options are 
#' "annotations" or "modeloutputs".
#' @param person_id The pk_personid of the person who completed the 
#' verifications, as stored in the "people" table of the database
#' @param disconnect TRUE or FALSE. Should the connection to the database be
#' severed on exit? Default is FALSE.
#' @usage verifyExcelImport(con, excel_file, verified_what, person_id, 
#' disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery dbAppendTable
#' @importFrom readxl read_excel
#' @details
#' This function takes a verifications spreadsheet, created using 
#' verifyExcelCreate(), and imports all verifications into the AMMonitor 
#' database. Note that all verifications in the Excel file must be input as TRUE
#' or FALSE for this function to work properly. For more details on creating
#' verification spreadsheets, see the verifyExcelCreate() helpfile.
#' 
#' The user must specify what was verified, "annotations" or "modeloutputs", as 
#' well as who completed the verifications using the pk_personid listed in the
#' database's "people" table. These details will be used to add all 
#' verifications to the proper table. If any outputs were not verified and left
#' blank, no new record will be added to the verifications table for that 
#' output.
#' 
#' For more details on annotations and verifications, please check the learnr 
#' "annotations" tutorial using the following code:
#' \code{learnr::run_tutorial(name = "annotations", package = "AMMonitor")}
#' 
#' @family verifications
#' @export
#' @examples
#' \dontrun{
#' 
#' # Create an AMMonitor demo database, to be deleted later
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Set a connection to the database
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # Get the media IDs to create a verification spreadsheet
#' media_ids <- DBI::dbGetQuery(
#'   conn = conx,
#'   statement = "SELECT pk_mediaid FROM media;"
#'   )[,,drop = TRUE]
#'   
#' # Set a file path for the output Excel sheet (including filename)
#' outfile_path <- paste0(demo_fp, "/validate_me.xlsx")
#' 
#' # Create the verifications spreadsheet
#' results <- verifyExcelCreate(
#'  con = conx, 
#'  media_ids = media_ids,
#'  verify_what = "modeloutputs",
#'  media_root_path = 
#'  "https://code.usgs.gov/vtcfwru/ammonitor/-/raw/AMMonitor2.2/inst/extdata/demoAMM",
#'  outfile_path =  outfile_path,
#'  disconnect = FALSE)
#'
#' ## NOTE: Open up your new verification spreadsheet and add some validations
#' ## to the "verified" column using TRUE and FALSE
#' 
#' # Import the verifications from the output file as gandalf
#' verifyExcelImport(
#'  con = conx,
#'  excel_file = outfile_path,
#'  verified_what = "modeloutputs",
#'  person_id = "gandalf",
#'  disconnect = FALSE)
#' 
#' # Look at the model verifications table to see the added verifications
#' View(DBI::dbReadTable(conx, "modelverifications"))
#' 
#' }
#' 

verifyExcelImport <- function(con, excel_file, verified_what, person_id, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Check the verified_what argument
  if(length(verified_what) > 1) stop("Please only provide one of 'annotations' or 'modeloutputs'.")
  if (verified_what %in% c("annotations", "modeloutputs") == FALSE) stop("Invalid input. Valid inputs are 'annotations' and 'modeloutputs'")
  
  # Check if the pk_personid is in the database
  people <- DBI::dbGetQuery(con, "SELECT pk_personid FROM people")
  person_exists <- grepl(
    pattern = person_id,
    x = people$pk_personid
  )
  
  if (all(person_exists != TRUE)) {stop("The provided person_id is not in the database. Please check the people table of your database.")}
  
  # Read the excel file
  excel_data <- suppressMessages(readxl::read_excel(path = excel_file, col_names = TRUE))
  
  # Convert TRUE and FALSE to 1 and 0 for the database
  suppressWarnings(excel_data$verified_bin[excel_data$verified == TRUE] <- 1)
  excel_data$verified_bin[excel_data$verified == FALSE] <- 0
  
  # Create a table of verifications based on the excel sheet
  if (verified_what == "modeloutputs") {
    verifications <- data.frame(
      pk_modelverificationid = NA,
      fk_modeloutputid = excel_data$pk_modeloutputid,
      fk_personid = person_id,
      is_valid = excel_data$verified_bin,
      timestamp = strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = "")
    )
  } else {
    verifications <- data.frame(
      pk_annoverificationid = NA,
      is_valid = excel_data$verified_bin,
      fk_personid = person_id,
      fk_annotationid = excel_data$pk_annotationid,
      timestamp = strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = "")
    )
  }
  
  # Remove any rows which have no verification
  verifications <- verifications[!is.na(verifications$is_valid), ]
  
  # Get the proper verification table based on type
  ver_table <- ifelse(
    test = verified_what == "modeloutputs",
    yes = "modelverifications",
    no = "annotationverifications"
  )
  
  # Append the verifications to the proper table
  append_result <- DBI::dbAppendTable(
    conn = con,
    name = ver_table,
    value = verifications
  )
  
  if (append_result == nrow(verifications)) {return("Verifications successfully added to the database.")}
  else {return("Excel import failed.")}
  
}