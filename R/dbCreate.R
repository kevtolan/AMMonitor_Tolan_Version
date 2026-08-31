#' @name dbCreate
#' @title Create a new AMMonitor SQLite database
#' @description Create a new AMMonitor SQLite database.  All tables and keys 
#' are generated automatically. The "default" database is a new database
#' populated with default values and ready to intake new information. See 
#' details.
#' @param new_db_name Name of the database.
#' @param new_db_filepath  File path to the directory that will house the
#'  database.  This should be the "database" directory of an AMMonitor project.
#' @param db_source Source of the database dictionary and lists (options 
#' include "default", "existing_db", "sciencebase"). The "default" option
#' should be used to create a new database from scratch.
#' @param source_filepath If db_source = "existing_db" or "sciencebase", 
#' provide
#' the file path to the existing database or science base volume.
#' @usage dbCreate(new_db_name, new_db_filepath, db_source = "default", 
#'   source_filepath = NA)
#' @return A new AMMonitor SQLite database.
#' @details The database should reside in the AMMonitor project (the working 
#' directory) within the directory named "database".
#' 
#' This function should be called directly only in cases where a new,
#' default AMMonitor database is to be created. The default dictionary comes 
#' with the AMMonitor package, where the
#' dictionary provides all information needed to create a new SQLite database.
#' If creating a new AMMonitor database from a data release published in the 
#' USGS ScienceBase repository, please use the \code{sbRehydrate()} function
#' instead.
#'
#' Users can interact with their database in two ways: 1) With R code;
#' 2) with the AMMonitor RShiny front end. Launch the Shiny application with 
#' the \code{launchApp()} function.
#'
#' See the "database" learnr tutorial for more details on the database.
#' The tutorial can be launched with \code{learnr::run_tutorial(name = "database",
#' package = "AMMonitor")}.
#' @family database
#' @importFrom DBI dbConnect dbSendQuery dbAppendTable dbListTables dbDisconnect
#' dbGetQuery dbClearResult
#' @importFrom RSQLite dbDriver
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{
#'
#' # ------------------------------------------------------------
#' # Create a new default AMMonitor database for demonstration
#' # in your temporary directory (to be deleted).
#' # ------------------------------------------------------------
#'
#' # create the default database (to be deleted):
#' new_db_filepath <- tempdir()
#' new_db_name <- "demo.sqlite"
#' db_source <- "default"
#' source_filepath <- NULL
#' 
#' # create the new SQLite database
#' dbCreate(new_db_name, new_db_filepath, db_source, source_filepath)
#'
#' # verify that the database exists in your current working directory
#' file.exists(new_db_name)
#'
#' # to work with a database in code, you must set a connection
#' conx <- dbSetCon(paste0(new_db_filepath, "/", new_db_name))
#' 
#' # look at the connection
#' conx
#'
#' # look at the built-in lists table
#' DBI::dbGetQuery(conn = conx, statement = "SELECT * FROM lists")
#' 
#' # look at the built-in people table; notice the default unknownPerson
#' DBI::dbGetQuery(conn = conx, statement = "SELECT * FROM people")
#'
#' # add a record to person table
#' rs <- DBI::dbExecute(
#'  conn = conx,
#'  statement = "
#'   INSERT INTO people(pk_personid, first_name, last_name,
#'     project_role, email, phone, sb_login, display_name)
#'   VALUES('fbaggins', 'Frodo', 'Baggins', 'Technician', 'frodo@gmail.com',
#'    '800-THE-RING', 'NA', 'Frodo');"
#' )
#'
#' # look at the updated people table
#' DBI::dbGetQuery(conn = conx, statement = "SELECT * FROM people")
#' 
#' # look at the dbdictionary table; it provides information about the 
#' # database schema
#' View(DBI::dbGetQuery(conn = conx, statement = "SELECT * FROM dbdictionary"))
#'
#' # disconnect from database when finished
#' DBI::dbDisconnect(conx)
#'
#' # delete the test database and remove it from your working directory
#' unlink(new_db_name)
#' 
#' }
#'
dbCreate <- function(new_db_name, new_db_filepath, db_source = "default", source_filepath = NA) {
  
  # Add a forward slash to folder file_path if missing
  if (grepl("\\/$", new_db_filepath) == FALSE) {
    new_db_filepath <- paste0(new_db_filepath, "/")
  }

  # Stop if database already exists
  if (file.exists(paste0(new_db_filepath, "/", new_db_name)) == TRUE) {
    stop("An existing database with the same name already exists. 
      Choose another name or delete the existing database (with care).")
  }

  # Create new database and connect to it
  conx <- DBI::dbConnect(
    drv = RSQLite::dbDriver("SQLite"),
    dbname = paste0(new_db_filepath, new_db_name)
  )

  # Disconnect from database on function exit
  on.exit(expr = {
    DBI::dbDisconnect(conx)
  })

  # Read in the database dictionary
  dictionary <- switch(db_source,
      "default" = {
        readxl::read_excel(
          path = paste0(
            find.package("AMMonitor", lib.loc = .libPaths()), 
            "/extdata/dbCreate.xlsx"),
          sheet = "dbdictionary"
        )
      },
      "existing_db" = {
        con_old <-  DBI::dbConnect(
          drv = RSQLite::dbDriver("SQLite"),
          dbname = source_filepath
        )
        tmp <- DBI::dbGetQuery(
          conn = con_old,
          statement = "SELECT * from dbdictionary"
        )
        DBI::dbDisconnect(con_old)
        tmp
      }, 
      "sciencebase" = {
        utils::read.csv(source_filepath, na.strings = "")
      }
  )
  
  # convert to dataframe
  dictionary <- as.data.frame(dictionary)
  
  all_tables <- unique(dictionary$pk_tablename)
  
  db_tables <- vector()
  
  while (length(all_tables) > 0) {
    
    for (tbl in all_tables) {
      if (tbl %in% db_tables) next
      
      # subset the dictionary
      tbl_i <- dictionary[which(dictionary$pk_tablename == tbl), ]
      
      # get the foreign keys
      fk_i <- tbl_i[
        which(!is.na(tbl_i$foreign_key_table) & tbl_i$foreign_key_table != ""), 
        "foreign_key_table", 
        drop = TRUE]
      if (length(fk_i) == 0 | all(fk_i %in% db_tables | fk_i == tbl)) {
        db_tables <-  append(db_tables, tbl)
      }
    } # end of tbl
    
    all_tables <- all_tables[-which(all_tables %in% db_tables)]
    
  } # end of while
 
  # loop through tables and create them
  for (i in 1:length(db_tables)) {

    print(paste0("table", i, ": ",  db_tables[i]))

    # Get table and foreign key info for the table
    tbl_info <- dictionary[which(dictionary$pk_tablename == db_tables[i]), ]
    
    # order by sort order
    indices <- order(tbl_info$pk_tablename, tbl_info$sort_order)
    tbl_info <- tbl_info[indices,]
    
    # get the foreign keys
    fk_info <- subset(tbl_info, !is.na(tbl_info$foreign_key_table) & tbl_info$foreign_key_table != "")

    # Logicals indicating whether the table has a foreign key or compound primary key
    has_compound_pk <- sum(tbl_info$pk != 0) > 1
    has_fk <- nrow(fk_info) != 0

    # Begin the query statement (stmnt will be concatenated to to build the query)
    stmnt <- paste("CREATE TABLE", db_tables[i], "(\n")

    # Loop through each field in the table
    for (j in 1:nrow(tbl_info)) {

      # Add Field name and data type
      stmnt <- paste(
        stmnt,
        tbl_info$pk_fieldname[j],
        tbl_info$var_type[j]
      )

      # Add NOT NULL clause
      if (!is.na(tbl_info$not_null_clause[j])) {
        stmnt <- paste(stmnt, tbl_info$not_null_clause[j])
      }

      # Add default value
      if (!is.na(tbl_info$default_value[j])) {
        stmnt <- paste(stmnt, "DEFAULT", tbl_info$default_value[j])
      }

      # add primary key if not compound 
     if (!has_compound_pk && tbl_info[j, "pk"] == TRUE) {
        stmnt <- paste(stmnt, "PRIMARY KEY")
      }

      if (j != nrow(tbl_info) || has_compound_pk) {
        stmnt <- paste0(stmnt, ",")
      }

      # Add the field description (as a comment)
      if (!is.na(tbl_info$description[j])) {
        stmnt <- paste(stmnt, "--", tbl_info$description[j], "\n")
      }
    }

    # Add compound primary key
    if (has_compound_pk) {
      stmnt <- paste(
        stmnt,
        "PRIMARY KEY (",
        paste(
          tbl_info$pk_fieldname[tbl_info$pk != 0],
          collapse = ", "
        ),
        ")"
      )
    }

    # Add foreign keys
    if (has_fk) {
      stmnt <- paste0(stmnt, ",\n")
      for (j in 1:nrow(fk_info)) {
        stmnt <- paste0(
          stmnt,
          " FOREIGN KEY (",
          fk_info$pk_fieldname[j],
          ") REFERENCES ",
          fk_info$foreign_key_table[j], "(",
          fk_info$foreign_key_field[j],
          ") ON UPDATE ",
          fk_info$on_update[j],
          " ON DELETE ",
          fk_info$on_delete[j]
        )
      }
    }

    # Close off the query statement
    stmnt <- paste0(stmnt, "\n)")
    
    # execute the query
    rs <- tryCatch({
      DBI::dbSendQuery(
      conn = conx,
      statement = stmnt
    )
    },
    error = function(cond) {
      stop(paste("Unable to create table:", cond))
    },
    warning = function(cond) {
      stop(paste("Unable to create table:", cond))
    })

    # clear the result
    DBI::dbClearResult(rs)
  } # end of table i
 
 # Populate the "dbdictionary" table
 DBI::dbAppendTable(conx, 'dbdictionary', dictionary)
 
  if (db_source == "default") {
    
    default_tbls <- c("lists", "listitems", "temporallists", "temporallistitems",
        "medialists", "medialistitems", "librarylists", "librarylistitems", "shinytable")
    
    for (tbl in default_tbls) {
      # Populate the "lists" table
      DBI::dbAppendTable(
        conn = conx, 
        name = tbl, 
        value = readxl::read_excel(
          path = paste0(
            find.package("AMMonitor", lib.loc = .libPaths()), 
            "/extdata/dbCreate.xlsx"),
          sheet = tbl
        )
      )
    }


    # Add rows to some tables =====================
    DBI::dbAppendTable(
      conx,
      'people',
      data.frame(
        pk_personid = "unknownPerson",
        first_name = "Unknown",
        last_name = "Person",
        display_name = "unknownPerson"
      )
    )
    DBI::dbAppendTable(
      conx,
      'equipment',
      data.frame(
        pk_equipmentid = "unknownEquipment"
      )
    )
    DBI::dbAppendTable(
      conx,
      'locations',
      data.frame(
        pk_locationid = "unknownLocation",
        spatial_geometry = "point"
      )
    )
    DBI::dbAppendTable(
      conx,
      'taxa',
      data.frame(
        pk_taxonid = c('no-species', 'animal sp.', 'Human' ),
        common_name = c('no-species', 'animal sp.', 'Human'),
        tsn = c("0", "202423", "180092"),
        taxon_rank = c(NA, "kingdom", "species"),
        rank_kingdom = c(NA, "Animalia", "Animalia"),
        notes = c('No taxa detected in media; non-existent TSN', 'Animal kingdom', 'Human')
      )
    )
  }  # end of defaults == TRUE
 
  if (db_source == "existing_db") {
    # to do:  add in shinytable, lists, listitems, etc.
    message("Not yet implemented.")
    
  }
 

  # Send a user message:
  message(paste0(
    "An AMMonitor database has been created with the name ",
    new_db_name, " which consists of the following tables: "
  ))
  message(paste(DBI::dbListTables(conn = conx), sep = "", collapse = ", "))


  success <- file.exists(paste0(new_db_filepath, "/", new_db_name))

  # return result
  return(success)

}
