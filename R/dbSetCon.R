#' @name dbSetCon
#' @title Set a SQLite connection to a database and turn on foreign keys
#' @description Creates a SQLite connection to a database and turns on foreign keys
#' @param dbPath Path to SQLite database
#' @importFrom DBI dbConnect  dbSendQuery dbClearResult
#' @importFrom RSQLite dbDriver
#' @usage dbSetCon(dbPath)
#' @return An object that stores the open database connection
#' @export
#' @examples
#' \dontrun{
#'
#' # create an new database in the working directory
#' dbCreate(new_db_name = "demo.sqlite", new_db_filepath = tempdir())
#'
#' # run dbSetCon to set and store the open connection
#' con <- dbSetCon(dbPath = file.path(tempdir(), "demo.sqlite"))
#'
#' # look at the stored connection object
#' class(con)
#' con
#'
#' }


dbSetCon <- function(dbPath) {

  # stop if database already exists
  if (file.exists(dbPath) == FALSE) {
    stop("The database path could not be found. Try dbPath <- file.choose() to navigate to the database and store the path.")
  }

  # connect to copied database
  con <- DBI::dbConnect(
    drv = RSQLite::dbDriver("SQLite"),
    dbname = dbPath
  )

  # turn on foreign keys
  rs <- DBI::dbSendQuery(con, statement = "PRAGMA foreign_keys = ON;")
  DBI::dbClearResult(rs)

  return(con)

}

