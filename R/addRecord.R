#' @name addRecord
#' @aliases addRecord
#' @title Adds a record to a table along with an (optional) associated file
#' @description Adds a record to a table along with an (optional) associated file
#' in local, Google Drive, AWS S3, or SharePoint directories.
#' @usage addRecord(con, table_name, new_record, add_file = FALSE, 
#' storage_type = NA,  file_path = NA)
#' @param con An open connection to the database
#' @param table_name A string, the name of the table to add the record to
#' @param new_record A dataframe of new records, with 
#' columns that match the table to be appended to. 
#' @param add_file TRUE/FALSE.  Whether to add an associated file, if any 
#' (default FALSE)
#' @param storage_type Where files associated with new records should be stored.
#' Options are "local", "Google Drive", "AWS S3", or "SharePoint". If NA, the
#' files are assumed to be stored in subdirectories of an AMMonitor project.
#' @param file_path Filepath of file associated with new db record
#' @importFrom aws.s3 put_object
#' @importFrom DBI dbBegin dbBind dbClearResult dbCommit dbGetRowsAffected dbRollback dbSendStatement
#' @importFrom Microsoft365R get_sharepoint_site
#' @importFrom googledrive drive_upload with_drive_quiet
#' @return Returns a list with the status of the addition (TRUE if successful) and
#' a message indicating any errors encountered, if the addition failed.
#' @details When adding a record to the AMMonitor database, the media, models, 
#' logs, and spatials tables all reference external files, which are tracked with 
#' the  filepath field (media, spatials, and logs tables) or amml field (models 
#' table). The addRecord() function performs a regular insert query for all other 
#' tables, but for these four, this function attempts to add the associated file 
#' before adding the database record, when specified, based on the associated 
#' filepath. This function performs a copy of the specified file to the location
#' specified in the new database record's filepath/amml column.
#' 
#' See the "database" learnr tutorial for more details on the AMMonitor database
#' tables. The tutorial can be launched with \code{learnr::run_tutorial(name = 
#' "database", package = "AMMonitor")}. For more details on cloud storage options
#' see the "media" tutorial.
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
#' # add a person to the database
#' new_person <- data.frame(pk_personid = 'test_person', display_name = 'test')
#' 
#' rs <- addRecord(con = conx, table_name = 'people', new_record = new_person)
#' 
#' # print the status of the addition
#' print(rs)
#' 
#' # demonstrate an error returned when addRecord fails
#' rs <- addRecord(con = conx, table_name = 'people', new_record = new_person)
#'   
#' # print the status of the addition
#' print(rs)
#' 
#' # Demonstrate adding a record with a file ------------------
#' 
#' # photo to add to the database (use one of the demo photos)
#' file_source <- dir(
#'   file.path(
#'     find.package("AMMonitor", lib.loc = .libPaths()), 
#'     "extdata/demoAMM/photos"
#'   ), 
#'   full.names = T
#' )[1]
#' 
#' # where the new photo should be saved (AMMonitor project's "photos" directory)
#' file_destination <- file.path(demo_fp, 'photos/test.JPG')
#' 
#' # create dataframe to be sent to the addRecord function
#' new_media <- data.frame(
#'   media_type = "photo",
#'   filename = "test.JPG",
#'   fk_visitid = 1,
#'   start_date = "1900-01-01",
#'   start_time = "00:00:00",
#'   filepath = file_destination
#' )
#' 
#' # add the record with media
#' rs <- addRecord(
#'   con = conx,
#'   table_name = "media",
#'   new_record = new_media,
#'   add_file = TRUE,
#'   storage_type = "local",
#'   file_path = file_source
#' )
#' 
#' # print the status of the addition
#' print(rs)
#' 
#' # confirm the new file was added
#' file.exists(new_media$filepath)
#' 
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#'
#' }

addRecord <- function(con, table_name, new_record, add_file = FALSE, storage_type = NA, file_path = NA) {
  # Step 0: Initialize a few re-used variables ----------------
  statusMessage <- "" # Initialize failure message
  file_added <- FALSE
  record_added <- FALSE
  
  # Step 1: Add the row to the database ----------------------
  
  # Begin the db transaction
  DBI::dbBegin(con)

  # try to delete record from database
  added <- tryCatch(
    expr = {
      insert_stmnt <- paste0(
        "INSERT INTO ",
        table_name,
        " (",
        paste(names(new_record), collapse = ", "),
        ") VALUES (",
        paste(paste0("$", 1:ncol(new_record)), collapse = ", "),
        ");"
      )
      rs <- DBI::dbSendStatement(con, insert_stmnt)
      DBI::dbBind(rs, unname(new_record))
      rows_affected <- DBI::dbGetRowsAffected(rs)
      DBI::dbClearResult(rs)
      rows_affected
    },
    error = function(x) {
      paste0(x)
    }
  )
  
  if (is.numeric(added) && added == 1) {
    record_added <- TRUE
  } else {
    if (exists("rs") && class(rs)[1] %in% c("SQLiteResult", "PqResult")) {
      DBI::dbClearResult(rs)
    }
    DBI::dbRollback(con)

    return(
      list(
        status = FALSE,
        message = paste0('Database insert failed: ', trimws(added), ". ")
      )
    )
  }
  
  # Step 2: Add the file ----------------------
  
  if (table_name %in% c('media', 'spatials', 'models', 'logs') && !is.na(file_path)) {
    
    file_path_col <- switch(
      table_name,
      "media" = "filepath",
      "spatials" = "filepath",
      "models" = "amml",
      "logs" = "pk_logid"
    )
    
    if (storage_type == "local") {
      addition_status <- tryCatch(
        {
          copy_success <- file.copy(
            from = file_path,
            to = new_record[[file_path_col]]
          )
          list(
            status = copy_success,
            message = ifelse(
              copy_success,
              paste0(statusMessage, "File copy success. "),
              paste0(statusMessage, "File copy failed. ")
            )
          )
        },
        error = function(x) {
          list(
            status = FALSE,
            message = paste0(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        },
        warning = function(x) {
          list(
            status = FALSE,
            message = paste0(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        }
      )
      
    } else if (storage_type == "Google Drive") {
      addition_status <- tryCatch(
        {
          #delete file locally, change success to true
          rs <- googledrive::with_drive_quiet(googledrive::drive_upload(
            media = file_path, 
            path = paste0(dirname(new_record[[file_path_col]]), '/'), 
            name = basename(new_record[[file_path_col]])
          ))
          new_record[[file_path_col]] <- switch(
            new_record$media_type,
            'photo' = paste0(
              'https://drive.google.com/thumbnail?id=',
              rs$id,
              '&sz=w1200'
            ),
            'audio' = paste0(
              'https://docs.google.com/uc?export=download&id=',
              rs$id
            )
          )
          list(
            status = (nrow(rs) == 1),
            message = paste0(statusMessage, 'File added to Google Drive. ')
          )
        },
        error = function(x) {
          list(
            status = FALSE,
            message = paste0(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        },
        warning = function(x) {
          list(
            status = FALSE,
            message = paste0(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        }
      )
    } else if (storage_type == "AWS S3") {
      bucket_name <- gsub("^.*https://([^//.]+).*", "\\1", new_record$filepath)
      addition_status <- tryCatch(
        {
          #add file to AWS S3
          rs <- aws.s3::put_object(
            file = file_path, 
            object = gsub(paste0("https://", bucket_name, ".s3.amazonaws.com/"), "", new_record$filepath), 
            bucket = bucket_name
          )
          list(
            status = rs,
            message = paste0(statusMessage, 'File added to AWS S3. ')
          )
        },
        error = function(x) {
          list(
            status = FALSE,
            message = paste0(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        },
        warning = function(x) {
          list(
            status = FALSE,
            message = paste0(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        }
      )
    } else if (storage_type == "SharePoint") {
      addition_status <- tryCatch(
        {
          #add file to SharePoint
          site <- Microsoft365R::get_sharepoint_site(
            site_url = gsub("/Shared%20Documents/.*$", "", new_record$filepath),
            tenant = gsub("^.*https://([^//.]+).*", "\\1", new_record$filepath), 
          )
          drv <- site$get_drive()
          
          sharepoint_folder <- gsub(
            '^.*/Shared%20Documents/', 
            "",
            dirname(new_record$filepath)
          )
          
          if (sharepoint_folder == "") {
            rs <- drv$upload_file(
              src = file_path, 
              dest = new_record$filename
            )
          } else {
            rs <- drv$upload_file(
              src = file_path, 
              dest = paste(sharepoint_folder, new_record$filename, sep = '/')
            )
          }
          
          list(
            status = 1,
            message = paste0(statusMessage, 'File added to SharePoint. ')
          )
        },
        error = function(x) {
          list(
            status = FALSE,
            message = paste0(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        },
        warning = function(x) {
          list(
            status = FALSE,
            message = paste(statusMessage, paste0("File could not be added: ", x, ". "))
          )
        }
      )
    }
    file_added <- addition_status$status
    statusMessage <- addition_status$message
  } else {
    file_added <- FALSE
  }
  
  # Step 3: Commit the addition, or rollback and return an error ------------
  if (record_added && !xor(add_file, file_added)) {
    DBI::dbCommit(con)
    return(
      list(
        status = TRUE,
        message = paste0(statusMessage, 'Database records added: ', added, ". ")
      )
    )
  } else {
    DBI::dbRollback(con)
    return(
      list(
        status = FALSE,
        message = paste0('Database insert failed: ', statusMessage, ". ")
      )
    )
  }
}
