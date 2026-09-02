#!! ModName = registerVisitUpdateDB
#!! ModDisplayName = Register a visit (and media) into the database.
#!! ModDescription = Adds new row(s) to visits, photos/recordings/videos, and logs tables.
#!! ModCitation = Laurence Clarfeld.  (2023). registerVisitUpdateDB. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = mediaType !! Type of media (photos/recordings/videos) !! character
#!! FunctionArg = visitMetadata !! Values of each column for the new row in visits table !! data.frame
#!! FunctionArg = screenData !! Data returned by screening function (including exif data) !! list
#!! FunctionArg = filePaths !! File paths to new media (photos/recordings/videos) !! character
#!! FunctionArg = storage_type !! Where new media files should be stored !! Character
#!! FunctionArg = includeLogFile !! Data returned by screening function (including exif data) !! logical
#!! FunctionReturn = status !! Whether the registration was a success !! logical
#!! FunctionReturn = newVisit !! The new row that was added to the visit table !! data.frame

# ---- AudioMoth WAV comment metadata parsing ----
# At registration time, audio files still sit at their local source path
# (filePaths()), so the comment can be read directly off disk -- no S3
# involved yet. Mirrors the parsing logic used by Backfill_AudioMoth_Metadata.R.

# Reads a little-endian uint32 from 4 raw bytes
registerVisit_le_uint32 <- function(raw4) {
  sum(as.integer(raw4) * c(1, 256, 65536, 16777216))
}

# Pulls the ICMT (comment) string out of a WAV header's raw bytes, if present
registerVisit_parse_icmt <- function(raw_bytes) {
  idx <- grepRaw("ICMT", raw_bytes, fixed = TRUE)
  if (length(idx) == 0) return(NA_character_)
  idx <- idx[1]
  if (idx + 8 > length(raw_bytes)) return(NA_character_)
  size <- registerVisit_le_uint32(raw_bytes[(idx + 4):(idx + 7)])
  end <- idx + 8 + size - 1
  if (end > length(raw_bytes)) return(NA_character_)
  txt_bytes <- raw_bytes[(idx + 8):end]
  txt_bytes <- txt_bytes[txt_bytes != as.raw(0)]
  if (length(txt_bytes) == 0) return(NA_character_)
  rawToChar(txt_bytes)
}

# AudioMoth comment format:
# "Recorded at 02:00:00 07/04/2026 (UTC) by AudioMoth 24F319075F7DF877 at medium
#  gain while battery was 4.0V and temperature was 0.9C."
registerVisit_audiomoth_pattern <- paste0(
  "Recorded at (\\d{2}:\\d{2}:\\d{2}) (\\d{2}/\\d{2}/\\d{4}) \\(UTC\\) ",
  "by AudioMoth ([0-9A-Fa-f]+) ",
  "at ([A-Za-z\\-]+) gain ",
  "while battery was ([0-9.]+)V ",
  "and temperature was (-?[0-9.]+)C\\."
)

registerVisit_parse_audiomoth_comment <- function(comment) {
  if (is.na(comment)) return(NULL)
  m <- regmatches(comment, regexec(registerVisit_audiomoth_pattern, comment))[[1]]
  if (length(m) != 7) return(NULL)  # didn't match -- not an AudioMoth comment we recognize
  list(
    time_str = m[2],
    date_str = m[3],
    device_serial = m[4],
    gain_setting = m[5],
    battery_voltage = as.numeric(m[6]),
    temperature_c = as.numeric(m[7])
  )
}

# Reads a local WAV file's AudioMoth comment and returns a one-row data.frame
# of the parsed fields (NA-filled columns if the file isn't from an AudioMoth
# or can't be read), ready to be cbind-ed onto a media row.
registerVisit_audiomoth_metadata <- function(file_path, tz) {
  blank <- data.frame(
    recorded_datetime_utc = NA_character_,
    recorded_datetime_local = NA_character_,
    device_serial = NA_character_,
    gain_setting = NA_character_,
    battery_voltage = NA_real_,
    temperature_c = NA_real_,
    stringsAsFactors = FALSE
  )

  tryCatch({
    con_file <- file(file_path, "rb")
    raw_header <- readBin(con_file, "raw", n = 4096)
    close(con_file)

    comment <- registerVisit_parse_icmt(raw_header)
    parsed <- registerVisit_parse_audiomoth_comment(comment)
    if (is.null(parsed)) return(blank)

    utc_dt <- lubridate::dmy_hms(paste(parsed$date_str, parsed$time_str), tz = "UTC")
    local_dt <- lubridate::with_tz(utc_dt, tzone = tz)

    data.frame(
      recorded_datetime_utc = format(utc_dt, "%Y-%m-%d %H:%M:%S"),
      recorded_datetime_local = format(local_dt, "%Y-%m-%d %H:%M:%S"),
      device_serial = parsed$device_serial,
      gain_setting = parsed$gain_setting,
      battery_voltage = parsed$battery_voltage,
      temperature_c = parsed$temperature_c,
      stringsAsFactors = FALSE
    )
  }, error = function(e) blank)
}


# the ui function
registerVisitUpdateDB_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns('mediaFileFormat'),
      'Media file format',
      choices = c(
        'locationID_equipmentID_YYYYMMDD_HHMMSS_#.ext',
        'locationID_YYYYMMDD_HHMMSS.ext'
      )
    ),
    actionButton(ns('addVisit'), 'Add the new visit'),
    verbatimTextOutput(ns('status'))
  )
}


# the server function
registerVisitUpdateDB_server <- function(id, mediaType, visitMetadata, screenData, filePaths, dirDest, storage_type, includeLogFile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    statusText <- reactiveVal(character(0))
    newVisit <- reactiveVal(data.frame())
    
    observeEvent(input$addVisit, {
      # Abort if severe data quality issues detected
      if (screenData()$status == 0) {
        statusText('Severe data issues detected, return to the previous tab for hints on how to resolve warnings.')
        output$status <- renderText({statusText()})
        
        return(
          reactiveValues(
            status = reactive(statusText()),
            newVisit = reactive(NA)
          )
        )
      }
      
      # get id of existing visit or create new visit
      if (visitMetadata()$pk_visitid != "") {
        newVisitID <- visitMetadata()$pk_visitid
        statusText(
          paste(
            statusText(),
            paste('Existing visit selected:', newVisitID),
            sep = '\n'
          )
        )
      } else {
        # Get the new visitID and create the new row to be added to the visits table
        newVisitID <- dbGetQuery(con(), 'SELECT MAX(pk_visitid) FROM visits;')[,]+1
        newVisitID <- ifelse(is.na(newVisitID), 1, newVisitID) # Correct if first visit
        
        newVisit(data.frame(cbind(
          pk_visitid = newVisitID, 
          visitMetadata()[names(visitMetadata()) != "pk_visitid"]
        )))
        
        # Try to add the visit
        rs <- tryCatch(
          {
            DBI::dbAppendTable(
              con(),
              'visits',
              newVisit()
            )
          },
          error = function(cond) {cond},
          warning = function(cond) {cond}
        )
        
        # Abort if there is an error
        if (any(class(rs) == 'error')) {
          statusText(
            paste(
              statusText(), 
              paste('Failed to add visit to database.'),
              rs$message,
              sep = '\n'
            )
          )
          output$status <- renderText({statusText()})
          return(
            reactiveValues(
              status = reactive(statusText()),
              newVisit = reactive(NA)
            )
          )
        }
      }
      
      # If existing visit or new visit successfully added, next add the media
      
      # Generate the new file names ------------
      # Set up dates from screening data
      if (!is.null(filePaths())) {
        
        media_dates <- format(screenData()$media_datetime, '%Y-%m-%d')
        media_times <- format(screenData()$media_datetime, '%H:%M:%S')
        
        newFileNames <- switch(
          input$mediaFileFormat,
          'locationID_equipmentID_YYYYMMDD_HHMMSS_#.ext' = paste(
            paste(
              visitMetadata()$fk_locationid,
              visitMetadata()$fk_equipmentid,
              gsub("[[:punct:]]", "", media_dates),
              gsub("[[:punct:]]", "", media_times),
              seq_len(length(filePaths())),
              sep = '_'
            ),
            tools::file_ext(filePaths()),
            sep = '.'
          ),
          'locationID_YYYYMMDD_HHMMSS.ext' = paste(
            paste(
              visitMetadata()$fk_locationid,
              gsub("[[:punct:]]", "", media_dates),
              gsub("[[:punct:]]", "", media_times),
              sep = '_'
            ),
            tools::file_ext(filePaths()),
            sep = '.'
          )
        )
        
        newMedia <- data.frame(
          fk_visitid = newVisitID,
          media_type = mediaType(),
          start_date = media_dates,
          start_time = media_times,
          filepath = paste(sub("/+$", "", dirDest()), newFileNames, sep = '/')
        )
        newMedia["filename"] <- newFileNames

        # For audio, pull AudioMoth comment metadata straight from the local
        # source file before it's copied/uploaded. Non-audio media, or audio
        # from other equipment, just get NA-filled columns here.
        if (mediaType() == "audio") {
          location_tz <- dbGetQuery(
            con(),
            "SELECT tz FROM locations WHERE pk_locationid = ?;",
            params = list(visitMetadata()$fk_locationid)
          )[1, "tz"]
          if (is.na(location_tz) || location_tz == "") location_tz <- "UTC"

          audiomoth_meta <- do.call(
            rbind,
            lapply(filePaths(), registerVisit_audiomoth_metadata, tz = location_tz)
          )
          newMedia <- cbind(newMedia, audiomoth_meta)
        }

        media_log <- data.frame()
        
        # Add each media file
        withProgress(
          message = "Copying media files", 
          value = 0,
          for (i_media in seq_len(nrow(newMedia))) {
            incProgress(1/nrow(newMedia), detail = paste(i_media, "of", nrow(newMedia)))
            add_status <- AMMonitor::addRecord(
              con = con(),
              table_name = 'media',
              new_record = newMedia[i_media,],
              add_file = TRUE,
              storage_type = storage_type(),
              file_path = filePaths()[i_media]
            )
            
            # Append media log
            media_log <- rbind(
              media_log,
              data.frame(
                filename = newMedia$filename[i_media],
                source_path = filePaths()[i_media],
                dest_path = newMedia$filepath[i_media],
                storage_type = storage_type(),
                copy_status = as.integer(add_status$status),
                copy_message = add_status$message
              )
            )
          }
        )
        
        # Add the log files -------------
        if (includeLogFile()) {
          
          # Abort if logs directory doesn't exist
          if (dir.exists(paste0(ammPath, '/logs/'))) {
            # Add visit log
            rs <- tryCatch(
              {
                log_path <- paste0(ammPath, '/logs/', 'visit', newVisitID, '_warnings.csv')
                ct <- 1
                while (file.exists(log_path)) {
                  log_path <- paste0(ammPath, '/logs/', 'visit', newVisitID, '_warnings', ct, '.csv')
                  ct <- ct + 1
                }
                DBI::dbAppendTable(
                  con(),
                  'logs',
                  data.frame(
                    pk_logid = log_path,
                    fk_visitid = newVisitID
                  )
                )
                write.csv(screenData()$warnings, log_path, row.names = FALSE, na = "")
                statusText(
                  paste(
                    statusText(), 
                    paste('Visit "warnings" log saved and added to database.'),
                    sep = '\n'
                  )
                )
              },
              error = function(cond) {
                statusText(
                  paste(
                    statusText(), 
                    'Failed to add visit log to database.',
                    cond,
                    sep = '\n'
                  )
                )
              },
              warning = function(cond) {
                statusText(
                  paste(
                    statusText(), 
                    'Failed to add visit log to database.',
                    cond,
                    sep = '\n'
                  )
                )
              }
            )
            
            # Add media log
            rs <- tryCatch(
              {
                log_path <- paste0(ammPath, '/logs/', 'visit', newVisitID, '_media.csv')
                ct <- 1
                while (file.exists(log_path)) {
                  log_path <- paste0(ammPath, '/logs/', 'visit', newVisitID, '_media', ct, '.csv')
                  ct <- ct + 1
                }
                DBI::dbAppendTable(
                  con(),
                  'logs',
                  data.frame(
                    pk_logid = log_path,
                    fk_visitid = newVisitID
                  )
                )
                write.csv(media_log, log_path, row.names = FALSE, na = "")
                statusText(
                  paste(
                    statusText(), 
                    paste('Visit "media" log saved and added to database.'),
                    sep = '\n'
                  )
                )
              },
              error = function(cond) {
                statusText(
                  paste(
                    statusText(), 
                    'Failed to add media log to database.',
                    cond,
                    sep = '\n'
                  )
                )
              },
              warning = function(cond) {
                statusText(
                  paste(
                    statusText(), 
                    'Failed to add media log to database.',
                    cond,
                    sep = '\n'
                  )
                )
              }
            )
          } else {
            statusText(
              paste(
                statusText(), 
                'Failed to add logs, no "logs" directory found in the ammPath.',
              )
            )
          }
        }
        statusText(
          paste(
            statusText(),
            paste('Successfully added', sum(media_log$copy_status), 'of', length(filePaths()), 'media files to database.'),
            sep = '\n'
          )
        )
        
      }
      if (visitMetadata()$pk_visitid == "") {
        statusText(
          paste(
            statusText(),
            paste('Visit', newVisitID, 'added to database.'),
            'Visit registration complete',
            sep = '\n'
          )
        )
      }
    })
    
    output$status <- renderText({statusText()})
    
    return(
      reactiveValues(
        status = reactive(cbind(pk_visitid = newVisitID, visitMetadata())),
        newVisit = reactive(newVisit())
      )
    )
  })
}
