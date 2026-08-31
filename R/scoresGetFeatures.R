#' @name scoresGetFeatures
#' @title Get the spectrogram features of each detection by a particular template.
#' @description \code{scoresGetFeatures} obtains the spectrogram for each verified 
#' detection by a particular template and associates them with the model verifications
#' so that they can be used to train classifier models. Only outputs where there 
#' is a consensus in the verifications (everyone who verified agreed that the output
#' is a true or false positive) will be obtained. While this function is designed 
#' for use with template outputs, it can also be used with other models that produce
#' outputs with identically sized bounding boxes.
#' @param con A connection to a SQLite or Postgres AMMonitor database.
#' @param templateName A character vector specifying which template (by name) to get the outputs for.
#' @param recordingRootPath A character string indicating the path to the directory 
#' containing the recordings, can be local or a URL. Defaults to the local recordings
#' folder of an AMMonitor project
#' @param showProgress Default = FALSE. If TRUE, progress will be reported during 
#' the spectrogram retrieval.
#' @param disconnect Default = FALSE. If TRUE, the connection to the database will be closed
#' upon exit from the function
#' @return dataframe of verifications and the spectrogram values as columns
#' @importFrom DBI dbIsValid dbSendQuery dbFetch dbBind dbClearResult dbDisconnect
#' @importFrom tuneR readWave
#' @importFrom seewave spectro
#' @importFrom methods as new
#' @importFrom utils txtProgressBar setTxtProgressBar download.file
#' @importFrom data.table rbindlist
#' 
#' @export
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # look at the models table
#' # notices that models 1:4 are templates created in AMMonitor
#' DBI::dbReadTable(conn = conx, name = "models")
#' 
#' # look at the modeloutputs table
#' # Note that model 3, oven_ct, has three outputs, 7:9
#' DBI::dbReadTable(conn = conx, name = "modeloutputs")
#' 
#' # look at the modelverifications table
#' # note that outputs 8 and 9 have been verified
#' DBI::dbReadTable(conn = conx, name = "modelverifications")
#' 
#' # retrieve the spectrograms for verified outputs of model 3, oven_ct
#' spectrograms <- scoresGetFeatures(
#'   con = conx, 
#'   templateName = "oven_ct", 
#'   recordingRootPath = paste0(demo_fp, "/recordings/"),
#'   disconnect = TRUE)
#'   
#' # unlink the demo
#' unlink(demo_fp)
#' }

scoresGetFeatures <- function(con, templateName, recordingRootPath = '/recordings/', showProgress = FALSE, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  if (grepl("\\/$", recordingRootPath)) {
    recordingRootPath <- substr(recordingRootPath, 1, nchar(recordingRootPath) - 1)
  }
  
  # get the modelID of the model
  rs <- DBI::dbSendQuery(con, "SELECT pk_modelid FROM models WHERE model_name = $1")
  DBI::dbBind(rs, list(templateName))
  templateID <- DBI::dbFetch(rs)$pk_modelid
  DBI::dbClearResult(rs)
  
  # get model outputs and verifications if there is a consensus only
  verifications <- qryModelVerificationConsensus(con, templateID)
  n <- nrow(verifications)
  if (n == 0) {
    stop("No model outputs with verifications were found. Please try another model.")
  } else {
    message(paste(n, "model outputs were found."))
  }
  
  if (showProgress) {
    pb <- utils::txtProgressBar(min = 0, max = 1, title = "Obtaining spectrogram data...")
    pb_value <- 1/n
  }
  
  # loop through each media file and get the necessary detection spectrograms
  media <- unique(verifications$fk_mediaid)
  bboxes <- list()
  for (i in media) {
    ver_subset <- verifications[which(verifications$fk_mediaid == i),]
    media_db <- DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM media WHERE pk_mediaid = ", i, ";")
    )
    # determining the file path
    if (is.null(recordingRootPath) || recordingRootPath == "") {
      if (grepl("google.com", media_db$filepath)) {
        audio_path <- paste(tempdir(), media_db$filename, sep = '/')
        googledrive::local_drive_quiet()
        googledrive::drive_download(
          file = media_db$filepath,
          path = audio_path,
          overwrite = TRUE
        )
      } else {
        audio_path <- media_db$filepath
      }
      
    } else {
      audio_path <- file.path(recordingRootPath, media_db$filename)
    }
    # actually getting the spectrograms
    tryCatch(
      {
        if (grepl("^www.|^http:|^https:", audio_path)) {
          temp.file <- tempfile()
          utils::download.file(
            url = audio_path, 
            destfile = temp.file, 
            quiet = TRUE, 
            mode = "wb", 
            cacheOK = TRUE
          )
          if (!file.exists(temp.file)) stop("File couldn't be downloaded")
          audio_wave <- tuneR::readWave(temp.file)
        } else {
          audio_wave <- tuneR::readWave(audio_path)
        }
          
        for (modeloutput in ver_subset$pk_modeloutputid) {
          mo_row <- ver_subset[which(ver_subset$pk_modeloutputid == modeloutput),]
          bboxes[[as.character(modeloutput)]] <- seewave::spectro(
            audio_wave,
            tlim = c(mo_row$x_min, mo_row$x_max),
            flim = c(mo_row$y_min, mo_row$y_max),
            plot = FALSE
          )
          if (showProgress) {
            utils::setTxtProgressBar(pb, pb_value)
            pb_value <- pb_value + 1/n
          }
        }
      },
      error = function(e) {
        message(as.character(e))
      }
    )
  }
  if (showProgress) {
    close(pb)
  }
  # convert is_valid to factor (true/false positives) and remove unnecessary columns for training
  tv <- verifications[c("pk_modeloutputid", "value_num", "is_valid")]
  tv$is_valid <- factor(ifelse(tv$is_valid == 1, "TP", "FP"), levels = c("TP", "FP"))
  
  # merge with bounding boxes
  bboxes_long <- lapply(bboxes, function(x) {
    as.data.frame(t(as.vector(x$amp)))
  })
  
  bboxes_df <- as.data.frame(
    data.table::rbindlist(bboxes_long, use.names = TRUE, idcol = "pk_modeloutputid", fill = TRUE)
  )
  
  merged <- merge(tv, bboxes_df, all = TRUE, by = "pk_modeloutputid")
  
  return(merged)
}
