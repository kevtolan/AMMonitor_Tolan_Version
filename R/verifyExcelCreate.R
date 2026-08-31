#' @name verifyExcelCreate
#' @aliases  verifyExcelCreate
#' @title Export annotations or modeloutputs table to Excel for verification
#' @description A quick and dirty way to verify annotations or modeloutputs by 
#' exporting records to an Excel spreadsheet along with cloud-based 
#' media links 
#' @param con An open database connection
#' @param media_ids Vector of pk_mediaids from the media table.
#' @param media_root_path  Character vector that designates the cloud file
#' root path for media files
#' @param verify_what  Should annotations or model outputs be verified? Options
#' are "annotations" or "modeloutputs"
#' @param outfile_path Character vector specifying path and file name of the 
#' output Excel file
#' @param disconnect TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @usage verifyExcelCreate(con, media_ids, media_root_path, 
#' verify_what = "modeloutputs", outfile_path, disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery
#' @importFrom writexl write_xlsx
#' @details
#' This function provides an alternative way for users to verify annotations or
#' machine model outputs by exporting the media links and human or machine 
#' labels to a spreadsheet. The user can then view the media and input 
#' verifications in the spreadsheet to be uploaded to the database.
#' 
#' The returned Excel file has a blank column named "hyperlink", situated to the 
#' right of the file's url.  Here, manually enter the formula =HYPERLINK(cell 
#' with the url) for the first record, and copy down to create a clickable link 
#' to each media file. 
#' 
#' Verifiers should use the final column to validate each record, where TRUE = 
#' true result and FALSE = incorrect result. It is helpful to set data 
#' validations on this column to restrict entries to either TRUE or FALSE.  
#' Empty cells indicate that a record was neither confirmed or unconfirmed.
#' 
#' The completed spreadsheet can then be imported back to the database with the 
#' function, verifyExcelImport().
#'  
#' See the "annotations" learnr tutorial for more details. The tutorial can be 
#' launched with
#' \code{learnr::run_tutorial(name = "annotations", package = "AMMonitor")}.
#' 
#' @export
#' @md
#' @family verifications
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor file structure in a temporary directory
#' # (to be deleted)
#' 
#' # run the function and capture the connection
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # list the tables in the AMMonitor database
#' DBI::dbListTables(conx)
#' 
#' # look at the media table
#' head(DBI::dbReadTable(conx, name = "media"))
#' 
#' # look at the modelouputs table
#' head(DBI::dbReadTable(conx, name = "modeloutputs"))
#' 
#' # set the media_ids
#' media_ids <- DBI::dbGetQuery(
#'   conn = conx,
#'   statement = "SELECT pk_mediaid FROM media;"
#'   )[,,drop = TRUE]
#'   
#' # set the output file path (including filename)
#' outfile_path <- paste0(demo_fp, "/validate_me.xlsx")
#' 
#' # create the verifications spreadsheet
#' results <- verifyExcelCreate(
#'  con = conx, 
#'  media_ids = media_ids,
#'  verify_what = "modeloutputs",
#'  media_root_path = 
#'  "https://code.usgs.gov/vtcfwru/ammonitor/-/raw/AMMonitor2.2/inst/extdata/demoAMM",
#'  outfile_path =  outfile_path,
#'  disconnect = FALSE)
#'  
#' # or run this to verify annotations
#' results <- verifyExcelCreate(
#'  con = conx, 
#'  media_ids = media_ids,
#'  verify_what = "annotations",
#'  media_root_path = 
#'  "https://code.usgs.gov/vtcfwru/ammonitor/-/raw/AMMonitor2.2/inst/extdata/demoAMM",
#'  outfile_path =  outfile_path,
#'  disconnect = FALSE)
#'  
#' # check the results
#' str(results)
#' 
#' # look for the Excel file creation
#' file.exists(outfile_path)
#' 
#' # distribute for validation!
#' 
#' # -----------------------------------------------------
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' unlink(outfile_path)
#' 
#' }

verifyExcelCreate <- function(con, media_ids, media_root_path, verify_what = "modeloutputs", 
                        outfile_path, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # check the verify_what option
  if(length(verify_what) > 1) stop("Please only provide one of 'annotations' or 'modeloutputs'.")
  if (verify_what %in% c("annotations", "modeloutputs") == FALSE) stop("Invalid input. Valid inputs are 'annotations' and 'modeloutputs'")

  # initialize results list
  results <- data.frame()
  
  # set the query statement
  media_str <- paste(media_ids, collapse = ', ')
  
  stmt <- ifelse(
    test = verify_what == "annotations",
    yes = paste0("SELECT annotations.pk_annotationid, media.media_type, media.filename, 
     annotations.fk_taxonid, annotations.x_max, annotations.x_max, annotations.x_min, 
     annotations.y_max, annotations.y_min
     FROM media INNER JOIN annotations ON media.pk_mediaid = annotations.fk_mediaid
     WHERE pk_mediaid IN (", media_str, ");"),
    no = paste0("SELECT modeloutputs.pk_modeloutputid, modeloutputs.fk_mediaid, media.media_type, 
     media.filename, modeloutputs.fk_taxonid, modeloutputs.x_min, modeloutputs.x_max, 
     modeloutputs.y_min, modeloutputs.y_max, modeloutputs.value_num
     FROM media INNER JOIN modeloutputs ON media.pk_mediaid = modeloutputs.fk_mediaid
     WHERE pk_mediaid IN (", media_str, ");")
  )
  
  # run the query
  rs <- DBI::dbGetQuery(con, statement = stmt)
  
  # update results for Excel
  rs$folder <- ifelse(
    rs$media_type == "audio", 
    yes = "recordings", 
    no = "photos"
  )
  
  rs$url <- ifelse(
    test = rs$media_type == "audio", 
    yes = paste0(media_root_path, '/', rs$folder, '/', rs$filename, '#t=', rs$x_min),
    no = paste0(media_root_path, '/', rs$folder, '/', rs$filename)
  )
  
  # add placeholders
  rs$hyperlink <- NA
  
  rs$verified <- NA
  
 # write the excel file
  writexl::write_xlsx(
    x = rs, 
    path = outfile_path,
    col_names = TRUE,
    format_headers = TRUE
  )

  return(rs)
}
