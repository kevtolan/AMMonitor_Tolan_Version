#' @name dbUpdateVersion
#' @title Update AMMonitor databases to the latest version
#' @description Updates an AMMonitor SQLITE database to match the schema of a 
#' given AMMonitor package version.
#' @param db_path A file path to an AMMonitor SQLite database
#' @param from_version Version of the existing AMMonitor database to convert from
#' @param to_version New AMMonitor database version to convert to
#' @param dest Directory path for where to save the updated database. Defaults
#' to the directory containing the unconverted database \code{(dirname(db_path))}
#' @importFrom tools file_path_sans_ext
#' @importFrom utils write.csv
#' @importFrom DBI dbReadTable dbAppendTable dbDisconnect
#' @usage dbUpdateVersion(db_path, from_version, to_version, dest = NA)
#' @return File path to the newly created, updated database.
#' @details This function can be used to update an AMMonitor SQLite database. In
#' some new versions, the database schema is slightly altered, and this function
#' allows a user to specify the current version of their AMMonitor database and
#' the desired, updated version. The original database will not be altered and
#' a new database, updated database will be created. It is recommended that 
#' users use the latest released version of AMMonitor and update their database 
#' accordingly.
#' 
#' One important caveats is that before using this function, it is highly
#' recommended that you confirm database integrity before proceeding. You can 
#' check the status of your database with the AMMonitor functions `dbCheckCore()`,
#' `dbCheckData()`, and `dbCheckup()`. 
#' 
#' This function was introduced in AMMonitor version 2.2 and currently
#' only has the ability to update a database from version 2.1 to 2.2. If you have
#' used either the *monitoR* database or the database from the first release of 
#' *AMMonitor*, please contact one of the package authors for advice on how to 
#' convert your database to the latest version. 
#' 
#' See the "database" learnr tutorial for more details on the
#' database. The tutorial can be launched with
#' \code{learnr::run_tutorial(name = "database", package = "AMMonitor")}.
#' 
#' @export
#' @family database
#' @examples
#' \dontrun{
#' 
#' Download the version 2.1 demo database (to be deleted)
#' db_path_original <- file.path(tempdir(), "demo_v21.sqlite")
#' 
#' utils::download.file(
#'   url = "https://code.usgs.gov/vtcfwru/ammonitor/-/raw/AMMonitor2.1/inst/extdata/demoAMM/database/demo.sqlite", 
#'   destfile = db_path_original, 
#'   mode = "wb"
#' )
#' 
#' # Connect to the demo database and explore some V2.1 distinguishing features
#' conx <- AMMonitor::dbSetCon(db_path_original)
#' 
#' # Should be FALSE, since modellables table introduced in V2.2
#' "modellabels" %in% DBI::dbListTables(conx)
#' 
#' DBI::dbDisconnect(conx)
#' 
#' # Update the database from V2.1 to V2.2
#' db_path_updated <- AMMonitor::dbUpdateVersion(
#'   db_path = db_path_original,
#'   from_version = "2.1",
#'   to_version = "2.2",
#'   dest = tempdir()
#' )
#' 
#' # Check that the updated db contains V2.2 features
#' conx <- AMMonitor::dbSetCon(db_path_updated)
#' 
#' # Should be TRUE, since modellables table introduced in V2.2
#' "modellabels" %in% DBI::dbListTables(conx)
#' 
#' DBI::dbDisconnect(conx)
#' 
#' # Clean-up
#' unlink(db_path_original)
#' unlink(db_path_updated)
#' 
#' }

dbUpdateVersion <- function(db_path, from_version, to_version, dest = NA) {
  
  # Preliminary checks and setup ---------
  
  # Check if conversion is supported
  if (from_version != "2.1" || to_version != "2.2") {
    stop("Currently only updates from Version 2.1 to 2.2 are supported. Please contact the package authors for advice on updating your database.")
  }
  
  # Connection to original database
  con_old <- AMMonitor::dbSetCon(db_path)
  
  # Set destination directory
  if (is.na(dest)) {dest <- dirname(db_path)}
  
  # Create updated dbdictionary ----------------
  
  # Existing dictionary
  dictionary <- DBI::dbReadTable(con_old, "dbdictionary")
  
  # Dictionary updates
  switch(
    paste(from_version, to_version, sep = "->"),
    "2.1->2.2" = {
      
      # Add modellables table
      modellabels_csv_txt <- '
"pk_tablename","pk_fieldname","core","var_type","not_null_clause","pk","foreign_key_table","foreign_key_field","on_update","on_delete","shiny_placeholder","shiny_input","default_value","fk_listid","sb_include","min","max","sort_order","description"
"modellabels","pk_modellabelid",1,"INTEGER",,1,,,,,,"locked",,,0,,,1,"Primary key for the modellabels table (an auto-number)."
"modellabels","fk_modelid",1,"INTEGER","NOT NULL",0,"models","pk_modelid","CASCADE","RESTRICT",,"foreignKey",,,0,,,2,"Maps to a pk_modelid from the models table"
"modellabels","fk_taxonid",1,"VARCHAR(255)",,0,"taxa","pk_taxonid","CASCADE","RESTRICT",,"foreignKey",,,0,,,3,"Maps to a pk_taxonid from the taxa table"
"modellabels","fk_librarylistitemid",1,"INTEGER",,0,"librarylistitems","pk_librarylistitemid","CASCADE","RESTRICT",,"foreignKey",,,0,,,4,"Maps to a pk_librarylistitemid from the librarylistitems table"
"modellabels","fk_medialistitemid",1,"INTEGER",,0,"medialistitems","pk_medialistitemid","CASCADE","RESTRICT",,"foreignKey",,,0,,,5,"Maps to a pk_medialistitemid from the medialistitems table"
"modellabels","original_label",1,"VARCHAR(255)",,0,,,,,,"text",,,0,,,6,"Original label produced by the model"
'
      dictionary <- rbind(
        dictionary,
        read.csv(text = modellabels_csv_txt, stringsAsFactors = FALSE)
      )
      
      # Add fk_equipmodelid column to econfignames table
      fk_equipmodelid_csv_txt <- '"pk_tablename","pk_fieldname","core","var_type","not_null_clause","pk","foreign_key_table","foreign_key_field","on_update","on_delete","shiny_placeholder","shiny_input","default_value","fk_listid","sb_include","min","max","sort_order","description"
"econfignames","fk_equipmodelid",1,"VARCHAR(255)",,0,"equipmodels","pk_equipmodelid","CASCADE","RESTRICT",,"foreignKey",,,1,,,2,"Foreign key to the equipmodels table, referencing pk_equipmodelid."'
      dictionary <- rbind(
        dictionary,
        read.csv(text = fk_equipmodelid_csv_txt, stringsAsFactors = FALSE)
      )
      
      # Add NOT NULL clauses
      dictionary$not_null_clause[dictionary$pk == 1 & dictionary$var_type != "INTEGER"] <- "NOT NULL"
    }
  )
  
  # Create the new database from the updated dbdictionary ----------
  
  # Create new filepath for updated database
  
  new_db_name <- paste0(
    tools::file_path_sans_ext(basename(db_path)), 
    "_updates_v22.sqlite"
  )
  
  db_path_new <- file.path(
    dest,
    new_db_name
  )

  # Create the database
  csv_path_tmp <- tempfile()
  utils::write.csv(dictionary, csv_path_tmp, row.names = FALSE, na = "")
  AMMonitor::dbCreate(
    new_db_name = new_db_name,
    new_db_filepath = dest, 
    db_source = "sciencebase", 
    source_filepath = csv_path_tmp
  )
  unlink(csv_path_tmp)
  
  # Copy data from old to new database -----------
  con_new <- AMMonitor::dbSetCon(db_path_new)
  
  dictionary <- DBI::dbReadTable(con_new, 'dbdictionary')
  
  # Order the tables
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
  
  # Remove dictionary from db_tables
  db_tables <- db_tables[-which(db_tables == "dbdictionary")]

  # List existing tablenames  
  db_old_tablenames <- DBI::dbListTables(con_old)
  
  # Re-populate tables from csv (without foreign keys because order may not allow for that)
  for (db_table in db_tables) {
    
    if (!db_table %in% db_old_tablenames) {next}
    
    # read in the data as characters
    insert_data <- DBI::dbReadTable(con_old, db_table)
    
    if (nrow(insert_data) == 0) {next}
    
    current_data <- DBI::dbReadTable(con_new, db_table)
    
    # alter the datatypes to match the dictionary
    keep_col <- character()
    for (i in 1:ncol(insert_data)) {
      # get column name
      col_name <- names(insert_data)[i]
      
      # find this in the dictionary
      index <- which(dictionary$pk_tablename == db_table & dictionary$pk_fieldname == col_name)
      
      # Drop missing columns
      if (length(index) == 0) {
        warning(paste("Column", col_name, "not found."))
        next
      }
      
      keep_col <- c(keep_col, col_name)
      
      col_type <- dictionary[index, "var_type", drop = TRUE]
      
      if (col_type == "REAL" | col_type == "INTEGER") {
        insert_data[,i] <- switch(
          col_type,
          "INTEGER" = as.integer(insert_data[,i]),
          "REAL" = as.numeric(insert_data[,i])
        )
      }
      
      # Avoid duplicate primary keys
      if (dictionary[index, "pk"] == 1) {
        insert_data <- insert_data[
          !insert_data[[col_name]] %in% current_data[[col_name]],
        ]
      }
    } # end of column i
    
    # Only keep core columns
    insert_data <- insert_data[,keep_col]
    
    # append the data
    DBI::dbAppendTable(
      conn = con_new,
      name = db_table,
      value = insert_data
    )
  }
  
  # Add update-related data records ------------
  switch(
    paste(from_version, to_version, sep = "->"),
    "2.1->2.2" = {
      # Add modellabels to shinyTable
      DBI::dbAppendTable(
        con_new,
        "shinytable",
        data.frame(
          fk_tablename = "modellabels",
          primary_tab = "ML Models",
          sort_order = 4
        )
      )
    }
  )
  
  # Disconnect from databases
  DBI::dbDisconnect(con_old)
  DBI::dbDisconnect(con_new)
  
  return(db_path_new)
}