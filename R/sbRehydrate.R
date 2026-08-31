#' @name sbRehydrate
#' @aliases sbRehydrate
#' @title Recreate any AMMonitor project from a ScienceBase release
#' @description \code{sbRehydrate} recreates an AMMonitor project using files 
#' included in
#' a data release of an AMMonitor project on ScienceBase.
#' @param ammPath A path to the directory which will house the AMMonitor project.
#' Default is getwd().
#' @param sbItemNum The item ID for an AMMonitor project data release on ScienceBase.
#' @param projectName A name for the newly created AMMonitor project. If not
#' provided, the title of the release will be used.
#' @usage sbRehydrate(ammPath = getwd(), sbItemNum, projectName = NULL)
#' @details The sbRehydrate() function can be used to "rehydrate" data releases
#' published on the AMMonitor ScienceBase community.  This community is found 
#' at https://www.sciencebase.gov/catalog/item/6188c0c4d34ec04fc9c4f7a4. Each 
#' project is an AMMonitor project that has been released following strict
#' guidelines for releases on this USGS repository.  The sbRehydrate() function
#' will download all files associated with a release, and then will create a 
#' new AMMonitor project and database, ready for analysis.  The media files
#' associated with a release are linked as "child" items and should be downloaded
#' separately (they are very large zip files).
#' 
#' For more information, please see the "sciencebase" learnr tutorial:
#' \code{learnr::run_tutorial(name = "sciencebase", package = "AMMonitor")}.
#' @return Returns the filepath to the rehydrated AMMonitor project. 
#' @importFrom DBI dbConnect  dbAppendTable 
#' dbReadTable dbDisconnect dbExecute 
#' @importFrom sbtools item_get_fields item_file_download item_list_children
#' item_list_files
#' @export
#' @examples 
#' \dontrun{
#' # look at a release to be rehydrated 
#' browseURL("https://www.sciencebase.gov/catalog/item/654a576bd34ee4b6e05c24d6")
#' 
#' # create a rehydrated AMMonitor project (to be deleted)
#' project_fp <- sbRehydrate(
#'   ammPath = getwd(), 
#'   sbItemNum = "654a576bd34ee4b6e05c24d6",
#'   projectName = "MiddleEarth"
#' )
#' 
#' # look at the the filepath of the rehydrated project 
#' project_fp
#' 
#' # look at the folders within an AMMonitor project
#' list.files(project_fp, recursive = TRUE)
#' 
#' # look at the media directories - they are empty! You 
#' # need to download the media files manually
#' list.files(file.path(project_fp, "photos"), recursive = FALSE)
#' list.files(file.path(project_fp, "recordings"), recursive = FALSE)
#' 
#' # see who the default user is for the database
#' readLines(file.path(project_fp, "settings", "default_user.txt"))
#' 
#' # look at the sample database in the database directory
#' dbname <- list.files(file.path(project_fp, "database"))
#' dbname
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(project_fp, "database", dbname))
#' 
#' # list the tables in the AMMonitor database
#' DBI::dbListTables(conx)
#' 
#' # the release information is stored within the "sciencebase" table
#' DBI::dbReadTable(conx, "sciencebase")
#' 
#' # note the media files are now associated with the release via fk_sciencebaseid
#' DBI::dbReadTable(conx, "media")
#' 
#' # run the database checks to ensure that rehydrated database has no issues
#' dbCheckup(conx)
#' dbCheckData(conx)
#' dbCheckCore(conx)
#' 
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#' 
#' # to work with the Shiny app, just point to the AMMonitor project directory
#' # the app will set the database connection and disconnect
#' launchApp(amm_project_path = project_fp)
#' 
#' # remove the rehydrated project 
#' unlink(project_fp, recursive = TRUE, force = TRUE)
#' 
#' }

sbRehydrate <- function(ammPath = getwd(), sbItemNum, projectName = NULL) {
  
  # Disconnect from database on function exit
  on.exit(expr = {
    DBI::dbDisconnect(con)
  })
  
  releaseName <- sbtools::item_get_fields(sbItemNum, "title")
  
  # get project name if not provided
  if (is.null(projectName)) {
    projectName <- releaseName
  }
  
  # create AMMonitor directory -------
  ammCreateDirectories(amm_dirname = projectName, filepath = ammPath)
  
  
  # add default settings to the project ---------
  writeLines(
    text = paste0(ammPath,  "/", projectName, "/recordings/"),
    con = file(paste0(ammPath, "/", projectName, "/settings/audio_path.txt"))
  )
  
  # image_path.txt
  writeLines(
    text = paste0(ammPath,  "/", projectName, "/photos/"),
    con = file(paste0(ammPath,  "/", projectName, "/settings/image_path.txt"))
  )
  
  # default_user.txt
  writeLines(
    text = 'unknownPerson', 
    con = file(paste0(ammPath,  "/", projectName, "/settings/default_user.txt"))
  )
  
  # create some filepaths -------
  projectPath <- file.path(ammPath, projectName)
  dbFilePath <- file.path(projectPath, "database")
  
  # Download database files from science base ---------
  sbtools::item_file_download(sb_id = sbItemNum, dest_dir = dbFilePath)
  
  # Re-create the AMMonitor database -----------
  dbCreate(
    new_db_name  = paste0(projectName, ".sqlite"), 
    new_db_filepath = dbFilePath, 
    db_source = "sciencebase", 
    source_filepath = file.path(dbFilePath, "dbdictionary.csv"))
  
  unlink(file.path(dbFilePath, "dbdictionary.csv"))
  
  # Connect to new db 
  con <- dbSetCon(dbPath = file.path(dbFilePath, paste0(projectName, ".sqlite")))
  
  # read in the dictionary table
  dictionary <- DBI::dbReadTable(con, "dbdictionary")
  
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
  
  # Re-populate tables from csv (without foreign keys because order may not allow for that)
  for (db_table in db_tables) {
    
    tbl_fp <- paste0(dbFilePath, "/", db_table, ".csv")
    
    if (file.exists(tbl_fp)) {
      
      message(paste0("\nInserting data into the ", db_table , " table."))
      
      # read in the data as characters
      insert_data <- read.csv(tbl_fp, header = TRUE, colClasses = "character", na.strings = "")

      # alter the datatypes to match the dictionary
      for (i in 1:ncol(insert_data)) {
        # get column name
        col_name <- names(insert_data)[i]
        
        # find this in the dictionary
        index <- which(dictionary$pk_tablename == db_table & dictionary$pk_fieldname == col_name)
        col_type <- dictionary[index, "var_type", drop = TRUE]
        
        if (col_type == "REAL" | col_type == "INTEGER") {
          insert_data[,i] <- switch(col_type,
              "INTEGER" = as.integer(insert_data[,i]),
              "REAL" = as.numeric(insert_data[,i])
          )
        }
      } # end of column i
      
      # append the data
      DBI::dbAppendTable(
        conn = con,
        name = db_table,
        value = insert_data
      )
      
      # remove the csv file
      unlink(tbl_fp)
    }
  }

  
  # check to make sure the default unknownPerson, unknownEquipment, unknownLocation
  # are listed as primary key
  # read the people table
  new_people <- dbReadTable(con, "people")
  
  if ("unknown person" %in% new_people$pk_personid) {
    DBI::dbExecute(con,
      statement = "UPDATE people SET pk_personid = 'unknownPerson' 
        WHERE pk_personid = 'unknown person';"
    )
    
    DBI::dbExecute(con,
      statement = "UPDATE people SET first_name = 'Unknown' 
        WHERE pk_personid = 'unknownPerson';"
    )
  }
  
  # Populate the sciencebase table from the item metadata -----
  xmlPath <- file.path(dbFilePath, paste0(releaseName, ".xml"))
  file.copy(from = xmlPath, to = projectPath, overwrite = TRUE)

  sbInsertMetadata(
    con = con, 
    sbItemNum = sbItemNum, 
    xmlPath = xmlPath,
    fromRehydrate = TRUE,
    disconnect = FALSE
  )
  
  # delete any non sqlite files in database folder ----
  for (f_name in list.files(dbFilePath)) {
    if (!grepl(".sqlite", f_name)) {
      unlink(file.path(dbFilePath, f_name))
    }
  }
  
  
  # send message
  message(paste0("An AMMonitor project has been created with the name ", projectName, 
    " from the ScienceBase item ", sbItemNum, ".  
    \nThe filepath for this project is
     ", projectPath, ".  Please navigate to the ScienceBase page and manually 
    download the zipped media files, and extract to an appropriate directory (photos
    or recordings on a local drive or in the cloud.)"))
  
  return(projectPath)
  
} # end of function
