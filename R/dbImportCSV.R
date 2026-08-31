#' @name dbImportCSV
#' @title Import records from a csv into an AMMonitor table
#' @description Import records from a csv into an AMMonitor table
#' @param con An open database connection
#' @param csv_path File path to a csv containing rows to be added
#' @param table_name The name of the table to import records into
#' @param row_numbers A vector of row numbers to import, if only a subset is
#' to be imported. Default is NA, which will import all of the records in the 
#' CSV file
#' @param disconnect TRUE or FALSE. Should the database connection be severed?
#' Default is FALSE.
#' @importFrom DBI dbDisconnect dbGetQuery dbSendQuery dbBind dbFetch 
#' dbClearResult dbIsValid
#' @importFrom utils read.csv
#' @usage dbImportCSV(con, csv_path, table_name, row_numbers, disconnect = FALSE)
#' @return A data.frame that stores the import status for each record in the CSV
#' @export
#' @family database
#' @examples
#' \dontrun{
#'
#' # create a new database in the temporary directory
#' dbCreate(new_db_name = "demo.sqlite", new_db_filepath = tempdir())
#'
#' # run dbSetCon to set and store the open connection
#' conx <- dbSetCon(dbPath = file.path(tempdir(), "demo.sqlite"))
#'
#' # write a csv with a record to add to the "people" table
#' csv_path <- file.path(tempdir(), 'test.csv')
#' 
#' utils::write.csv(
#'   x = data.frame(
#'     pk_personid = c("bbaggins", "fbaggins"),
#'     first_name = c("Bilbo", "Frodo"),
#'     last_name = c("Baggins", "Baggins"),
#'     project_role = c("Tagger", "Field technician"),
#'     display_name = c("blue", "red")), 
#'   file = csv_path
#' )
#' 
#' # import records from the CSV file into the people table
#' rs <- dbImportCSV(
#'   con = conx,
#'   csv_path = csv_path,
#'   table_name = "people",
#'   disconnect = FALSE
#' )
#' 
#' # inspect the returned results
#' rs
#' 
#' # check to see that the new records were added:
#' DBI::dbReadTable(conx, "people")
#' 
#' # disconnect from the database and clean up temporary files
#' DBI::dbDisconnect(conx)
#' 
#' unlink(file.path(tempdir(), "demo.sqlite"))
#' unlink(csv_path)
#'
#' }
#' 

dbImportCSV <- function(con, csv_path, table_name, row_numbers = NA, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Get database table information
  dict <- DBI::dbGetQuery(
    con, 
    paste0(
      "SELECT * FROM dbDictionary WHERE pk_tablename = '",
      table_name,
      "';"
    )
  )
  
  # Read in data from CSV
  csv_headers <- utils::read.csv(csv_path, header = FALSE, nrows = 1, col.names = 'headers')
  header_var_types <- sapply(
    dict$var_type[match(csv_headers$headers, dict$pk_fieldname)],
    function(x) {ifelse(x %in% c("INTEGER", "REAL"), 'numeric', 'character')},
    USE.NAMES = F
  )
  new_data <- utils::read.csv(
    file = csv_path,
    na.strings = "",
    colClasses = header_var_types
  )
  
  # trim the data as requested
  if (!is.na(row_numbers)) {
    new_data <- new_date[row_numbers,]
  }
  
  # Initialize results
  db_log <- data.frame(
    status = numeric(nrow(new_data)), 
    message = character(nrow(new_data))
  )
  
  # Add each new record
  for (i in seq_len(nrow(new_data))) {
    # Get valid fields
    valid_fields_mask <- names(new_data) %in% dict$pk_fieldname & !is.na(new_data[i,])
    
    # Try to add the new record
    new_id <- tryCatch(
      {
        rs <- DBI::dbSendQuery(
          con, 
          paste0(
            "INSERT INTO ",
            table_name,
            " (",
            paste(names(new_data)[valid_fields_mask], collapse = ', '),
            ") VALUES(",
            paste("$", 1:sum(valid_fields_mask), sep = "", collapse =  ", "),
            ") RETURNING ",
            dict$pk_fieldname[dict$pk == 1],
            ";"
          )
        )
        DBI::dbBind(rs, unname(new_data[i, valid_fields_mask]))
        new_id <- DBI::dbFetch(rs)[,]
        DBI::dbClearResult(rs)
        new_id
      },
      error = function(cond) {
        DBI::dbClearResult(rs)
        cond
        },
      warning = function(cond) {cond}
    )
    
    # Add results of attempted INSERT to the results log
    if (any(class(new_id) %in% c("error", "warning"))) {
      db_log$status[i] <- 0
      db_log$message[i] <- new_id$message
    } else {
      db_log$status[i] <- 1
      db_log$message[i] <- paste('Added record with ID:', new_id)
    }
  }
  return(db_log)
}
