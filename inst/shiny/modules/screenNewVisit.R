#' @name screenNewVisit
#' @title Screen a new visit for potential logic errors
#' @description Performs several checks on new visit metadata based on selected
#'  media files and existing database records
#' @details
#'   Current implementation checks:
#'    - Are any basic fields missing? (location, equipment, person, visit type)
#'    - Is the visit_type "set" with associated media files?
#'    - Is this a check/pull with no associated media files?
#'    - Does the visit already exist in the db?
#'    - If the visit_type is "set", is the equipment already deployed?
#'    - If visit_type is "check/pull", is there a matching "set/check" in the database?
#'    - Are there any corrupted (SIZE=0) media files?
#'    - Do (camera only) times from EXIF data and visit time differ more than expected?
#'
#' @param visitMetadata The visit metadata for the visit to be registered
#' @param visitID The visitID of an existing visit from the visits table
#' @param mediaType Type of media
#' @param selectedFiles File paths to media files to be uploaded
#' @param audio_fn_format File naming format (for audio files only)
#' @param con  An open connection to the AMMonitor database
#' @usage screenNewVisit(visitMetadata = NA, visitID = NA, mediaType, selectedFiles, con)
#' @return A list containing the status of the survey response, any issues found (dataframe), and the exif data for any media files
#' @importFrom exifr read_exif
#' @importFrom DBI dbSendQuery dbBind dbFetch dbClearResult

screenNewVisit <- function(visitMetadata = NA, visitID = NA, mediaType, selectedFiles, audio_fn_format = NA, con) {
  # Hard-coded Values
  MAX_MINUTES_DIFF <- 360
  
  # If a visitID is provided, try to use it (potentially overwriting visitMetadata)
  if (!is.na(visitID)) {
    rs <- DBI::dbSendQuery(con, 'SELECT * FROM visits WHERE pk_visitid = $1;')
    DBI::dbBind(rs, list(visitID))
    visitMetadata <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    if (nrow(visitMetadata) == 0) {
      stop('Invalid visitID.')
    }
  } else if (methods::is(visitMetadata,'logical') && is.na(visitMetadata)) {
    stop('Need visitMetadata or visitID.')
  }
  
  status <- 1 # Whether minimal QC standards are met to add the visit to the db
  exif_data <- data.frame()
  warnings <- data.frame(
    warning = character(0),
    description = character(0),
    solution = character(0),
    severity = integer(0)
  )
  
  # Check for missing basic fields ------------
  for (basicField in c('fk_locationid', 'fk_equipmentid', 'fk_personid', 'visit_type')) {
    if (visitMetadata[[basicField]] == "") {
      warnings <- rbind(
        warnings,
        data.frame(
          warning = paste('missing', basicField, sep = '_'),
          description = paste('Field', basicField, 'is missing'),
          solution = paste('Go back and add', basicField, 'to visit metadata'),
          severity = 3
        )
      )
      status <- 0
    }
  }
  
  # Visit type "set" with media files specified --------------
  if (visitMetadata$visit_type == "set" && length(selectedFiles) != 0) {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'setWithFiles',
        description = 'Visit type "set" with associated files',
        solution = 'A "set" should never have media files. Review the definition of each visit type and correct the visit type to a "check" or "pull" if media files are included.',
        severity = 3
      )
    )
  }
  
  # Missing media files for a set or pull --------------
  if (!visitMetadata$visit_type %in% c("", "set") && length(selectedFiles) == 0) {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = paste0(visitMetadata$visit_type, 'NoFiles'),
        description = paste('Visit type', visitMetadata$visit_type, 'with no associated files'),
        solution = 'Media files typically expected for a "check" or "pull". Make sure it is correct that no media files exist for the associated visit before continuing.',
        severity = 2
      )
    )
  }
  
  # Check for duplicate visit -----------
  # Matching same location, person, equipment, and date
  params <- list(
    visitMetadata$fk_locationid, 
    visitMetadata$fk_equipmentid,
    visitMetadata$fk_personid,
    visitMetadata$visit_date
  )
  rs <- DBI::dbSendQuery(
    con,
    "SELECT * FROM visits WHERE fk_locationid = $1
    AND fk_equipmentid = $2 
    AND fk_personid = $3 
    AND visit_date = $4;"
  )
  DBI::dbBind(rs, params)
  matchingRow <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  if (nrow(matchingRow) != 0) {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'duplicateVisit',
        description = 'A visit with matching location, equipment, person, and date already exists in the database.',
        solution = 'Check to make sure this visit was not already added to the database.',
        severity = 3
      )
    )
  }
  
  # Setting an already-deployed equipment --------------
  params <- list(visitMetadata$fk_equipmentid, paste0(visitMetadata$visit_date, visitMetadata$visit_time))
  rs <- DBI::dbSendQuery(
    con,
    "SELECT visit_type, fk_locationid FROM visits WHERE fk_equipmentid = $1 
    AND visit_date || visit_time <= $2 
    ORDER BY visit_date DESC LIMIT 1;"
  )
  DBI::dbBind(rs, params)
  lastEquipVisit <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  if (nrow(lastEquipVisit) != 0 && lastEquipVisit$visit_type != 'pull' & visitMetadata$visit_type == 'set') {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'setDeployedEquip',
        description = paste(
          'Equipment',
          visitMetadata$fk_equipmentid,
          'is already deployed at location',
          lastEquipVisit$fk_locationid
        ),
        solution = 'Be sure a "pull" visit for the given equipment is registered before it is set in a new location.',
        severity = 2
      )
    )
  }
  
  # Check for matching set/check (for any check/pull) ----------
  rs <- DBI::dbSendQuery(
    con,
    "SELECT * FROM visits WHERE fk_locationid = $1
    AND fk_equipmentid = $2 
    AND visit_date < $3 
    AND NOT visit_type = 'pull' 
    ORDER BY visit_date DESC LIMIT 1;"
  )
  DBI::dbBind(
    rs, params = list(
      visitMetadata$fk_locationid,
      visitMetadata$fk_equipmentid,
      visitMetadata$visit_date
    )
  )
  matching_visit <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  if (nrow(matching_visit) == 0 && visitMetadata$visit_type != "set") {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'matchingVisit',
        description = paste(
          'Visit of type',
          visitMetadata$visit_type,
          'with no matching set/check visit.'
        ),
        solution = 'Make sure to enter a matching set/check for the visit.',
        severity = 2
      )
    )
  }
  
  # Check for corrupted (size ZERO) files --------------
  if (length(selectedFiles) != 0) {
    fileSizes <- file.size(selectedFiles)
    if (any(fileSizes == 0)) {
      warnings <- rbind(
        warnings,
        data.frame(
          warning = 'corruptedFiles',
          description = paste('There are', sum(fileSizes), 'corrupted files found (size 0MB).'),
          solution = 'Try retreiving the corrupted files again from their source (SD card, cloud storage, etc.) to see if a non-corrupted version of the file exists.',
          severity = 3
        )
      )
    }
  }
  
  # Check dates/times -------------
  perform_date_check <- FALSE # Initialize date check (only perform if all media have valid dates)
  if (!is.null(selectedFiles) && mediaType == 'photo') {
    exif_data <- exifr::read_exif(selectedFiles)
    
    if (! 'DateTimeOriginal' %in% names(exif_data) || any(is.na(exif_data$DateTimeOriginal))) {
      warnings <- rbind(
        warnings,
        data.frame(
          warning = 'missingEXIF',
          description = 'The EXIF data is missing for one or more files.',
          solution = 'EXIF data is needed to infer the date/time each image was taken. Without EXIF data, image dates/times can be added direcly using the database portal at a later time.',
          severity = 3
        )
      )
      new_media_datetime <- ""
    } else {
      new_media_datetime <- as.POSIXlt.character(
        exif_data$DateTimeOriginal,
        format = "%Y:%m:%d %H:%M:%S"
      )
      perform_date_check <- TRUE
    }
  } else if (!is.null(selectedFiles) && mediaType == "audio") {
    # audio data
    new_media_datetime <- switch(
      audio_fn_format,
      '*YYYYMMDD_HHMMSS.*' = as.POSIXlt.character(
        sapply(
          tools::file_path_sans_ext(selectedFiles),
          function(f) {
            substr(f, nchar(f)-14, nchar(f))
          }, 
          USE.NAMES = FALSE
        ),
        format = '%Y%m%d_%H%M%S'
      )
    )
    
    if (all(!is.na(new_media_datetime))) {
      perform_date_check <- TRUE
    } else {
      warnings <- rbind(
        warnings,
        data.frame(
          warning = 'invalidAudioFileFormat',
          description = 'The file name did not match the specified format.',
          solution = 'Properly formatted file names are needed to infer the date/time each recording was made. Either rename the files to match the specified format, or specify a different format.',
          severity = 3
        )
      )
    }
  } else {
    new_media_datetime <- integer(0)
  }
  
  if (perform_date_check == TRUE) {
    # Latest photo time later than visit time by > MAX_MINUTES_DIFF ----------
    
    # Minutes after visit_time for each photo
    timeDiffAfter <- difftime(
      new_media_datetime,
      as.POSIXct(paste(visitMetadata$visit_date, visitMetadata$visit_time)),
      units = 'min'
    )
    
    if (max(timeDiffAfter) > MAX_MINUTES_DIFF) {
      warnings <- rbind(
        warnings,
        data.frame(
          warning = 'invalidDateLATE',
          description = paste0(
            'The date/time of one or more files are greater than the maximum (',
            MAX_MINUTES_DIFF, ' minutes) later than the visit date/time.'
          ),
          solution = 'Check to see if dates were set incorrectly on the camera/recorder. If so, use EXIF-editting software to correct the embedded timestamps (photos) or fix the file names (recordings) before processing, or adjust the date/time after the media files have been uploaded using AMMonitor app "shiftMediaTime".',
          severity = 2
        )
      )
    }
    
    # Minutes before visit_time for each photo
    if (nrow(matching_visit) != 0) {
      timeDiffBefore <- difftime(
        as.POSIXct(paste(matching_visit$visit_date, matching_visit$visit_time)),
        new_media_datetime,
        units = 'min'
      )
      
      if (max(timeDiffBefore) > MAX_MINUTES_DIFF) {
        warnings <- rbind(
          warnings,
          data.frame(
            warning = 'invalidDateEARLY',
            description = paste0(
              'The date/time of one or more files are less than the maximum (',
              MAX_MINUTES_DIFF, ' minutes) earlier than the visit date/time.'
            ),
            solution = 'Check to see if dates were set incorrectly on the camera/recorder. If so, use EXIF-editting software to correct the embedded timestamps (photos) or fix the file names (recordings) before processing, or adjust the date/time after the media files have been uploaded using AMMonitor app "shiftMediaTime".',
            severity = 2
          )
        )
      }
    }
  }
  
  return(list(
    status = status,
    warnings = warnings,
    media_datetime = new_media_datetime
  ))
  
}
