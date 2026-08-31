#' @name deleteRecord
#' @title Deletes a record from a specified table and/or any associated files
#' @description Deletes a record from a table and its associated file
#' (if applicable) in local, Google Drive, AWS S3, or SharePoint directories.
#' @usage deleteRecord(con, table_name, selected_row, delete_file = FALSE, 
#'   media_paths = NA, amm_project_path = getwd())
#' @param con A connection to the database
#' @param table_name A string, the name of the table to delete a record from
#' @param selected_row The contents of the selected row (dataframe)
#' or character vector (vector of primary key values, in the order that they
#' are in the table)
#' @param delete_file TRUE or FALSE, specifying whether to delete associated files, 
#' if any. Default is FALSE
#' @param media_paths Media paths (for use in AMMonitor's Shiny app, default = NA)
#' @param amm_project_path Filepath to the outermost directory for an AMMonitor
#' project
#' @importFrom aws.s3 delete_object head_object
#' @importFrom DBI dbGetQuery dbExecute dbBegin dbCommit dbRollback dbIsValid
#' @importFrom Microsoft365R get_sharepoint_site
#' @importFrom googledrive as_id drive_rm with_drive_quiet local_drive_quiet
#' @return Returns a list with the status of the deletion (TRUE if successful) and
#' a message indicating any errors encountered, if the deletion failed.
#' @details When deleting a record from the AMMonitor database, the media, models, 
#' logs, and spatials tables all reference external files, which are tracked with 
#' the filepath field (media, spatials), amml field (models), or pk_logid field 
#' (logs). This delete function performs a regular delete query for all other tables, 
#' but for these four, this function attempts to delete the associated file before 
#' deleting the database record, when specified, based on the associated file path.
#' @family database
#' @export
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor file structure in a temporary directory
#' # (to be deleted)
#' 
#' # run the function and capture the connection
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # add and person to the database
#' new_person <- data.frame(
#'   pk_personid = 'test_person', 
#'   display_name = 'test')
#'   
#' DBI::dbAppendTable(conx, 'people', new_person)
#' 
#' # look at the new addition
#' DBI::dbReadTable(conx, name = "people")
#' 
#' # now delete that record, referencing the table and primary key
#' rs <- deleteRecord(
#'   con = conx, 
#'   table_name = 'people', 
#'   selected_row = new_person)
#' 
#' # print the status of the deletion
#' print(rs)
#' 
#' # delete a media record from the demo db, along with the associated file
#' # get the media file to be deleted
#' media_to_delete <- DBI::dbGetQuery(con, 'SELECT * FROM media WHERE pk_mediaid = 1;')
#' 
#' # check to see that the associated file exists
#' (file.exists(
#'   paste(demo_fp, paste0(
#'     media_to_delete$media_type, 's'), media_to_delete$filename, sep = '/')
#'   )
#'  )
#' 
#' rs <- deleteRecord(
#'   con = conx, 
#'   table_name = 'media', 
#'   selected_row = media_to_delete , 
#'   delete_file = TRUE, 
#'   amm_project_path = demo_fp
#' )
#' 
#' # print the status of the deletion
#' print(rs)
#' 
#' # check to see that the associated file was deleted
#' file.exists(
#'   paste(
#'     demo_fp, 
#'     paste0(media_to_delete$media_type, 's'), media_to_delete$filename, sep = '/')
#'  )
#' 
#' # disconnect from the database when finished
#' DBI::dbDisconnect(con)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#'
#' }

deleteRecord <- function(con, table_name, selected_row, delete_file = FALSE, media_paths = NA, amm_project_path = getwd()) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # Step 0: Initialize a few re-used variables ----------------
  statusMessage <- "" # Initialize failure message
  file_deleted <- FALSE
  record_deleted <- FALSE
  
  # Step 1: Delete the row from the database ----------------------
  
  # Set up WHERE conditions for query statement.
  #get primary keys
  tablePk <- DBI::dbGetQuery(
    conn = con,
    statement = paste0(
      "SELECT pk_fieldname, var_type FROM dbdictionary WHERE pk_tablename = '",
      table_name,
      "' AND pk = 1;"
    )
  )
  
  whereConditions <- paste(
    ifelse(
      tablePk$var_type == "INTEGER",
      paste0(tablePk$pk_fieldname, " = ", selected_row[tablePk$pk_fieldname]),
      paste0(tablePk$pk_fieldname, " = '", gsub("'", "''", selected_row[tablePk$pk_fieldname]), "'")
    ),
    collapse = " AND "
  )
  
  #create delete statement
  deleteStatement <- paste0(
    "DELETE FROM ",
    table_name,
    " WHERE ",
    whereConditions,
    ";"
  )

  # Begin the db transaction
  DBI::dbBegin(con)
  # try to delete record from database
  deleted <- tryCatch(
    expr = {
      DBI::dbExecute(conn = con, statement = deleteStatement)
    },
    error = function(x) {
      paste0(x)
    }
  )
  
  if (is.numeric(deleted) && deleted > 0) {
    record_deleted <- TRUE
  } else {
    statusMessage <- paste('Database record deletion failed:', deleted)
  }
  
  # Step 2: Delete the file ----------------
  # (skip step 2 if no associated files)
  if (record_deleted && table_name %in% c("media", "spatials", "models", "logs") && delete_file == TRUE) {
    # get any root file paths from settings
    
    if (is.list(media_paths)) {
      IMG_PATH <- media_paths$IMG_PATH
      AUDIO_PATH <- media_paths$AUDIO_PATH
    } else {
      if (file.exists(paste0(amm_project_path, '/settings/image_path.txt'))) {
        IMG_PATH <- read.csv(paste0(amm_project_path, '/settings/image_path.txt'), header = F)[,]
        if (!dir.exists(IMG_PATH)) {
          IMG_PATH <- gsub("//", "/", file.path(amm_project_path, IMG_PATH))
        }
      } else {
        IMG_PATH <- ""
      }
      if (file.exists(paste0(amm_project_path, '/settings/audio_path.txt'))) {
        AUDIO_PATH <- read.csv(paste0(amm_project_path, '/settings/audio_path.txt'), header = F)[,]
        if (!dir.exists(AUDIO_PATH)) {
          AUDIO_PATH <- gsub("//", "/", file.path(amm_project_path, AUDIO_PATH))
        }
      } else {
        AUDIO_PATH <- ""
      }
    }
    
    #get file path
    filepath <- switch(
      table_name,
      models = selected_row$amml,
      spatials = selected_row$filepath,
      media = {
        if (is.null(selected_row$filepath) || is.na(selected_row$filepath)) {
          switch(
            selected_row$media_type,
            photo = ifelse(
              IMG_PATH == "",
              NA,
              paste(IMG_PATH, selected_row$filename, sep = '/')
            ),
            audio = ifelse(
              AUDIO_PATH == "",
              NA,
              paste(AUDIO_PATH, selected_row$filename, sep = '/')
            )
          )
        } else {
          selected_row$filepath
        }
      },
      logs = selected_row$pk_logid
    )

    if (length(filepath) == 0 || is.na(filepath)) {
      file_deleted = FALSE
      statusMessage = 'No file path is provided for an associated file. To delete this record and associated file, please update the file path.'
    }
    
    # Get the storage type (e.g., local, dropbox, etc.)
    if (startsWith(as.character(filepath), 'http') %in% c(NA, FALSE)) {
      storage <- 'local'
    } else if (grepl('sharepoint.com', filepath)) {
      storage <- 'sharepoint'
    } else if (grepl('google.com', filepath)) {
      storage <- 'googledrive'
    } else if (grepl('s3.amazonaws.com', filepath)) {
      storage <- 's3'
    } else if (grepl('dropbox.com', filepath)) {
      storage <- 'dropbox'
      file_deleted <- FALSE
      statusMessage <- paste(statusMessage, ",", storage, " cloud storage not currently supported. ")
    } else {
      storage <- 'unknown'
      file_deleted <- FALSE
      statusMessage <- paste(statusMessage, "unrecognized cloud storage.")
    }
    
    # Attempt to remove the file
    deletion_status <- switch(
      storage,
      "local" = {
        tryCatch(
          {
            #delete file locally, change success to true
            file.remove(filepath)
            list(
              status = TRUE,
              message = paste(statusMessage, 'File removed from disc.')
            )
          },
          error = function(x) {
            list(
              status = FALSE,
              message = paste(statusMessage, paste0("File could not be deleted: ", x))
            )
          },
          warning = function(x) {
            list(
              status = FALSE,
              message = paste(statusMessage, paste0("File could not be deleted: ", x))
            )
          }
        )
      },
      'sharepoint' = {
        tryCatch(
          {
            site_SharePoint <- sapply(
              strsplit(filepath, '/'),
              function(x) {paste(x[1:5], collapse = '/')}
            )
            
            # Extract the tenant from the SharePoint site name
            tenant <- regmatches(
              site_SharePoint,regexec(
                "https://\\s*(.*?)\\s*.sharepoint.com",
                site_SharePoint
              )
            )[[1]][2]
            site <- Microsoft365R::get_sharepoint_site(
              site_url = site_SharePoint,
              tenant = tenant
            )
            drive <- site$get_drive()
            drive$delete_item(gsub(paste(site_SharePoint, 'Shared%20Documents', sep = '/'), "", filepath))
            list(
              status = TRUE,
              message = paste(statusMessage, 'File removed from SharePoint.')
            )
          },
          error = function(x) {
            list(
              status = FALSE,
              message = paste(statusMessage, paste0("File on SharePoint could not be deleted: ", x))
            )
          }
        )
      },
      'googledrive' = {
        googledrive::local_drive_quiet()
        tryCatch(
          {
            if (tablePk$pk_fieldname[1] == 'pk_mediaid' && selected_row$media_type == "photo") {
              rs <- googledrive::drive_rm(
                id = googledrive::as_id(gsub(".*id=(.+)&sz.*", "\\1", filepath))
              )
            } else {
              rs <- googledrive::drive_rm(
                id = googledrive::as_id(gsub(".*id=(.+)", "\\1", filepath))
              )
            }
            
            list(
              status = rs,
              message = paste(statusMessage, 'File removed from Google Drive.')
            )
          },
          error = function(x) {
            list(
              status = FALSE,
              message = paste(statusMessage, paste0("File on Google Drive could not be deleted: ", x))
            )
          }
        )
      },
      's3' = {
        tryCatch(
          {
            # Check that the object exists
            exists_before <- suppressMessages(aws.s3::head_object(
              object = gsub("^.*s3.amazonaws.com/([^//.]+)", "\\1", selected_row$filepath), 
              bucket = gsub("^.*https:/([^//.]+).*", "\\1", selected_row$filepath)
            ))
            
            # Try to delete the object
            is_deleted <- suppressMessages(aws.s3::delete_object(
              object = gsub("^.*s3.amazonaws.com/([^//.]+)", "\\1", selected_row$filepath), 
              bucket = gsub("^.*https:/([^//.]+).*", "\\1", selected_row$filepath)
            ))
            
            # Check that the object exists
            exists_after <- suppressMessages(aws.s3::head_object(
              object = gsub("^.*s3.amazonaws.com/([^//.]+)", "\\1", selected_row$filepath), 
              bucket = gsub("^.*https:/([^//.]+).*", "\\1", selected_row$filepath)
            ))
            
            remove_success <- exists_before && is_deleted && !exists_after
            
            list(
              status = remove_success,
              message = paste(
                statusMessage, 
                ifelse(
                  remove_success,
                  'File removed from AWS S3',
                  'File on AWS S3 could not be deleted. '
                )
              )
            )
          },
          error = function(x) {
            list(
              status = FALSE,
              message = paste(statusMessage, paste0("File on AWS S3 could not be deleted: ", x))
            )
          }
        )
      },
      list(
        status = FALSE,
        message = statusMessage
      )
    )  
    file_deleted <- deletion_status$status
    statusMessage <- paste(statusMessage, deletion_status$message)
  }
  
  # Step 3: Commit the deletion, or rollback and return an error ------------
  if (record_deleted && !xor(delete_file, file_deleted)) {
    DBI::dbCommit(con)
    return(
      list(
        status = TRUE,
        message = paste(statusMessage, 'Database records deleted:', deleted)
      )
    )
  } else {
    DBI::dbRollback(con)
    return(
      list(
        status = FALSE,
        message = statusMessage
      )
    )
  }
}
