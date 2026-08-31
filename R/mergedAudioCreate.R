#' @name mergedAudioCreate
#' @aliases mergedAudioCreate
#' @title Create a merged audio file from specified annotations/modeloutputs
#' @description Create a merged audio file from specified 
#' annotations/modeloutputs to facilitate the verification workflow.
#' @param con An open connection to an AMMonitor database
#' @param annotation_ids Annotation ID's for annotations to be included
#' @param modeloutput_ids Model Output ID's for model outputs to be included
#' @param buffer_before Number of seconds of audio to add before each annotation
#' @param buffer_after Number of seconds of audio to add after each annotation
#' @param buffer_btwn Number of seconds of silence to add between each annotation
#' @param sample_rate Sample rate of the merged recording
#' @param ammPath Path to the directory for the AMMonitor project (default NA)
#' @param storage_type What type of storage to use (defaults to "local")
#' @param media_folder Root file path or URL where the merged audio should be saved
#' @param disconnect  TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE.
#' @usage mergedAudioCreate(con, annotation_ids = NA, modeloutput_ids = NA,
#' buffer_before = 5, buffer_after = 5, buffer_btwn = 0.5, sample_rate = 24000,
#' ammPath = NA, storage_type = "local", media_folder = NA, disconnect = FALSE)
#' @importFrom utils download.file
#' @return Returns 1 if successful
#' @details  
#' This function merges clips of user-specified annotations and model outputs 
#' into a single, merged audio file for easy verification.
#' 
#' The procedure for verifying audio annotations and model outputs often involves
#' reviewing a random sample of annotations/modeloutputs spread across many audio
#' files. This can create a bottleneck in the verification pipeline where a 
#' verifier must wait for an entire audio file to load in order to review just a 
#' small portion of it.
#' 
#' The user specifies the ID's of any annotations and/or model outputs that need
#' to be verified. A merged audio file will be created and associated with a 
#' special visit with visit_type "merged-verification". A mapping will be formed 
#' between annotations/modeloutputs in the merged audio and the annotations and 
#' modeloutputs from the original audio files. Once the merged audio file has 
#' been verified, the verifications from the merged audio file can be 
#' reconstituted into the database using the "mergedAudioRemove()" function.
#' @family annotationverifications, modeloutputverifications
#' @importFrom DBI dbGetQuery dbAppendTable
#' @importFrom tuneR readWave bind writeWave
#' @importFrom monitoR changeSampRate
#' @export
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # Define input parameters (model outputs and annotations to be merged)
#' modeloutput_ids <- 7:9
#' annotation_ids <- 26:27
#' 
#' # Create a merged audio file and register in the database with the specified 
#' # annotations and model outputs
#' mergedAudioCreate(
#'   con = conx,
#'   annotation_ids = annotation_ids,
#'   modeloutput_ids = modeloutput_ids,
#'   ammPath = demo_fp
#'   )
#'   
#'   
#' # Look at the new visit that is associated with the merged file
#' (new_visit <- DBI::dbGetQuery(
#'   conx,
#'   "SELECT * FROM visits WHERE visit_type = 'merged-verification';"
#'   ))
#'
#' # Look at the new, merged audio file; note the media type and filename
#' (merged_audio <- DBI::dbGetQuery(
#'   conx,
#'   paste("SELECT * FROM media WHERE fk_visitid = ", new_visit$pk_visitid, ";")
#'   ))  
#' 
#' # Verify the merged audio file exists in the recordings directory
#' file.exists(merged_audio$filepath)
#' list.files(file.path(demo_fp, "recordings"))
#' 
#' # Look at the annotations from the merged audio file
#' DBI::dbGetQuery(
#'   conx,
#'   paste(
#'     "SELECT * FROM annotations WHERE fk_mediaid = ",
#'     merged_audio$pk_mediaid,
#'     ";")
#'   )
#'
#' # Look at the modeloutputs from the merged audio file
#' DBI::dbGetQuery(
#'   conx,
#'   paste(
#'     "SELECT * FROM modeloutputs WHERE fk_mediaid = ",
#'     merged_audio$pk_mediaid,
#'     ";")
#'   )
#'   
#'  # Look at the audio file in the shiny app; 
#'  # log in as unknownPerson
#'  # click on the Audio Tools tab
#'  # click on Annotation Verifications or Model Verifications to verify records 
#'  # in the merged audio file
#'  
#'  launchApp(demo_fp)
#'  
#'  # import records to the database with mergedAudioRemove()
#'
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' }

mergedAudioCreate <- function(con, annotation_ids = NA, modeloutput_ids = NA, buffer_before = 5, buffer_after = 5, buffer_btwn = 0.5, sample_rate = 24000, ammPath = NA, storage_type = "local", media_folder = NA, disconnect = FALSE) {
  
  if (all(is.na(c(annotation_ids, modeloutput_ids)))) {
    stop("Unspecified annotations/modeloutputs.")
  }
  
  if (is.na(media_folder)) {
    if (is.na(ammPath)) {
      stop("Either an ammPath or media_folder must be specified.")
    }
    media_folder <- file.path(ammPath, "recordings")
  }
  
  # Get the metadata annotation/modeloutput ----
  annotations <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT media_type, filename, filepath, annotations.* FROM media INNER JOIN annotations ON media.pk_mediaid = annotations.fk_mediaid WHERE pk_annotationid IN (",
      ifelse(any(is.na(annotation_ids)), "", paste(annotation_ids, collapse = ", ")),
      ") AND media_type = 'audio';"
    )
  )
  
  modeloutputs <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT media_type, filename, filepath, modeloutputs.* FROM media INNER JOIN modeloutputs ON media.pk_mediaid = modeloutputs.fk_mediaid WHERE pk_modeloutputid IN (",
      ifelse(any(is.na(modeloutput_ids)), "", paste(modeloutput_ids, collapse = ", ")),
      ") AND media_type = 'audio';"
    )
  )
  
  # Initialize wave file for merged audio
  w_merged <- new('Wave')
  w_merged@samp.rate <- sample_rate
  w_merged@stereo <- FALSE
  
  # Initialize a silent buffer to place between samples
  w_space <- new('Wave')
  w_space@samp.rate <- sample_rate
  w_space@stereo <- FALSE
  w_space@left <- rep(0, sample_rate*buffer_btwn)
  
  # Initialize dataframes to hold labels
  modeloutput_labels <- data.frame()
  annotation_labels <- data.frame()
  
  # Create the merged audio file ----
  
  # Add for each audio file
  audio_files <- unique(rbind(
    annotations[,c('filename', 'filepath')],
    modeloutputs[,c('filename', 'filepath')]
  ))
  audio_dir <- ifelse(
    file.exists(file.path(ammPath, "settings", "audio_path.txt")),
    read.csv(
      file.path(ammPath, "settings", "audio_path.txt"),
      header = FALSE
    )[,],
    NA
  )
  for (i_audio_file in seq_len(nrow(audio_files))) {
    
    filename <- audio_files$filename[i_audio_file]
    
    # Get the filepath for the audio file
    if (!is.na(audio_files$filepath[i_audio_file])) {
      fp <- audio_files$filepath[i_audio_file]
    } else if (!is.na(audio_dir)) {
      if (dir.exists(audio_dir)) {
        fp <- gsub("//", "/", file.path(audio_dir, filename))
      } else if (dir.exists(gsub("//", "/", file.path(ammPath, audio_dir)))) {
        fp <- gsub("//", "/", file.path(ammPath, audio_dir, filename))
      } else {
        stop("Unable to resolve directory path specified in audio_path.txt")
      }
    } else if (!is.na(ammPath)) {
      fp <- file.path(ammPath, "recordings", filename)
    }
    
    # Download audio file, if not local
    if (grepl("^www.|^http:|^https:", fp)) {
      temp.file <- tempfile()
      utils::download.file(
        url = fp, 
        destfile = temp.file, 
        quiet = TRUE, 
        mode = "wb", 
        cacheOK = TRUE
      )
      if (!file.exists(temp.file)) stop("File couldn't be downloaded")
      fp <- temp.file
      temp_file <- TRUE
    } else {
      temp_file <- FALSE
    }
    
    # Read in the wave, extract some metadata, get the media's model outputs
    media_meta <- tuneR::readWave(fp, header = TRUE)
    duration_media <- media_meta$samples/media_meta$sample.rate
    mo_sample <- modeloutputs[modeloutputs$filename == filename,]
    anno_sample <- annotations[annotations$filename == filename,]
    
    # Clip the model outputs out of the media file, with a buffer, 
    # and append them to the sample wave file
    for (i_mo in seq_len(nrow(mo_sample))) {
      # Read in any of the wave that is needed:
      x_from <- max(0, mo_sample$x_min[i_mo] - buffer_before)
      x_to <- min(mo_sample$x_max[i_mo] + buffer_after, duration_media)
      
      # If x-lims not specified, include whole recording
      if (is.na(x_from)) {x_from <- 0}
      if (is.na(x_to)) {x_to <- duration_media}
      
      w <- tuneR::readWave(
        filename = fp,
        from = x_from,
        to = x_to,
        units = "seconds"
      )
      
      # Convert the original file sample rate (if needed)
      if (w@samp.rate != sample_rate) {
        w <- monitoR::changeSampRate(wchange = w, sr.new = sample_rate)
      }
      
      if (tuneR::nchannel(w) == 2) {
        
        w <- tuneR::channel(w, "left")
        
      }
      
      duration_sample <- length(w_merged@left) / w_merged@samp.rate
      duration_buffer <- mo_sample$x_min[i_mo] - x_from
      duration_mo <- mo_sample$x_max[i_mo] - mo_sample$x_min[i_mo]
      
      # Add model output metadata to the sample labels
      modeloutput_labels <- rbind(
        modeloutput_labels,
        data.frame(
          fk_modelid = mo_sample$fk_modelid[i_mo],
          fk_parentid = mo_sample$pk_modeloutputid[i_mo],
          fk_taxonid = mo_sample$fk_taxonid[i_mo],
          fk_librarylistitemid = mo_sample$fk_librarylistitemid[i_mo],
          fk_medialistitemid = mo_sample$fk_medialistitemid[i_mo],
          x_min = duration_sample + duration_buffer,
          x_max = duration_sample + duration_buffer + duration_mo,
          y_min =  mo_sample$y_min[i_mo],
          y_max =  mo_sample$y_max[i_mo],
          value_num =  mo_sample$value_num[i_mo]
        )
      )
      
      w_merged <- tuneR::bind(w_merged, w, w_space)
    }
    
    # Clip the annotations out of the media file, with a buffer, 
    # and append them to the sample wave file
    for (i_anno in seq_len(nrow(anno_sample))) {
      # Read in any of the wave that is needed:
      x_from <- max(0, anno_sample$x_min[i_anno] - buffer_before)
      x_to <- min(anno_sample$x_max[i_anno] + buffer_after, duration_media)

      # If x-lims not specified, include whole recording
      if (is.na(x_from)) {x_from <- 0}
      if (is.na(x_to)) {x_to <- duration_media}
      
      w <- tuneR::readWave(
        filename = fp,
        from = x_from,
        to = x_to,
        units = "seconds"
      )
      
      # Convert the original file sample rate (if needed)
      if (w@samp.rate != sample_rate) {
        w <- monitoR::changeSampRate(wchange = w, sr.new = sample_rate)
      }
      
      if (tuneR::nchannel(w) == 2) {
        
        w <- tuneR::channel(w, "left")
        
      }
      
      duration_sample <- length(w_merged@left) / w_merged@samp.rate
      duration_buffer <- anno_sample$x_min[i_anno] - x_from
      duration_anno <- anno_sample$x_max[i_anno] - anno_sample$x_min[i_anno]
      
      # Add annotation metadata to the sample labels
      annotation_labels <- rbind(
        annotation_labels,
        data.frame(
          fk_personid = anno_sample$fk_personid[i_anno],
          fk_taxonid = anno_sample$fk_taxonid[i_anno],
          x_min = duration_sample + duration_buffer,
          x_max = duration_sample + duration_buffer + duration_anno,
          y_min =  anno_sample$y_min[i_anno],
          y_max =  anno_sample$y_max[i_anno],
          notes = anno_sample$pk_annotationid[i_anno]
        )
      )
      
      w_merged <- tuneR::bind(w_merged, w, w_space)
    }

    # Remove the temporary audio file (if needed)
    if (temp_file == TRUE) {unlink(fp)}
  }
  
  # Create a visit for the merged audio file -----
  visit_id <- DBI::dbGetQuery(con, "SELECT MAX(pk_visitid)+1 FROM visits;")[,]
  DBI::dbAppendTable(
    conn = con,
    "visits",
    data.frame(
      pk_visitid = visit_id,
      fk_personid = "unknownPerson",
      fk_locationid = "unknownLocation",
      fk_equipmentid = "unknownEquipment",
      visit_type = "merged-verification",
      visit_date = "1900-01-01",
      visit_time = "12:00:00",
      visit_notes = "Temporary 'merged verification' file."
    )
  )
  
  # Add the merged audio file to the media table ----
  filename <- paste0("merged_validation_", as.numeric(Sys.time()), ".wav")
  
  fp_merged_audio <- file.path(
    tempdir(), 
    filename
  )
  
  # Save the merged wave object to a file
  tuneR::writeWave(
    object = w_merged, 
    filename = fp_merged_audio
  )
  
  media_id <- DBI::dbGetQuery(con, "SELECT MAX(pk_mediaid)+1 FROM media;")[,]
  
  rs <- AMMonitor::addRecord(
    con = con,
    table_name = 'media',
    new_record = data.frame(
      pk_mediaid = media_id,
      media_type = "audio",
      filename = filename,
      fk_visitid = visit_id,
      start_date = "1900-01-01",
      start_time = "12:00:00",
      filepath = file.path(media_folder, filename),
      sb_exclude = 1
    ),
    add_file = TRUE,
    storage_type = storage_type,
    file_path = fp_merged_audio
  )
  
  if (rs$status != TRUE) {
    # TODO - Add more error handling here... should we rollback db operations?
    stop(rs$message)
  }
  
  # Add the new modeloutputs to the database
  if (nrow(modeloutput_labels) != 0) {
    DBI::dbAppendTable(
      con,
      "modeloutputs",
      cbind(
        fk_mediaid = media_id,
        modeloutput_labels
      )
    )
  }

  # Add the new annotations to the database
  if (nrow(annotation_labels) != 0) {
    DBI::dbAppendTable(
      con,
      "annotations",
      cbind(
        fk_mediaid = media_id,
        annotation_labels
      )
    )
  }
}
