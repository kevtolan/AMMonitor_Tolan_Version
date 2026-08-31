#' @name dbCheckData
#' @title Performs basic database integrity checks
#' @description Checks database integrity by checking for inconsistencies in 
#' data entries.
#' @param con An open database connection
#' @param check_list List of data quality checks to perform (default NA for "all")
#' @param verbose Whether or not to print the status of each check (default TRUE)
#' @param disconnect TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @param ... Optional arguments to be used in checks (see Details)
#' @importFrom DBI dbIsValid dbDisconnect dbReadTable 
#' @details
#' This function performs data checks to ensure data is in the right format and
#' is logically coherent. A data.frame is returned that names each data issue
#' encountered, with a description and advice for resolving the issue. 
#' By default, all checks are performed. 
#' 
#' Checks that can be performed by this function include:
#'  * visit_date: all visit dates are in a valid format
#'  * visit_time: all visit times are in a valid format
#'  * media_time: all media dates are in a valid format
#'  * media_time: all media times are in a valid format
#'  * visit_type_sequence: all visits appropriately sequenced by type
#'  * media_start_times: media dates/times consistent with their associated visits
#'  
#' Note that the natural order that checks are run is deliberate, and that it is 
#' sometimes the case that the first issues presented must be addressed before 
#' later issues. For example, the media_start_times check may require that all 
#' visit and media dates/times are in a valid format to work properly.
#' 
#' For the media_start_times check, the optional parameter "time_threshold" can 
#' be entered to specify a buffer time (in seconds) around the visit time to 
#' allow for media times to fall outside of the specified visits by a given amount
#' without returning a warning. The default for this parameter is 60\*60\*6 =21600
#' seconds, or 6 hours.
#' 
#' Some other helpful AMMonitor functions for checking data quality are:
#'  - dbCheckup: Checks for foreign key constraint violations and mismatches
#'    between the dbDictionary and database schema.
#'  - dbGetSummaryData: Creates a list of dataframes from a database for summary
#'    purposes. This list of dataframes can then be used to create a variety of
#'    summary statistics and figures using \code{dbPlotSummary()} or 
#'    \code{dbTableSummary()}, which can be helpful for spotting outliers that
#'    may result from data entry mistakes.
#'    
#' See the "database" learnr tutorial for more details on the
#' database. The tutorial can be launched with
#' \code{learnr::run_tutorial(name = "database", package = "AMMonitor")}.
#' 
#' @export
#' @md
#' @family database
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor file structure in a temporary directory
#' # (to be deleted)
#' 
#' # run the function and capture the connection
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # list the tables in the AMMonitor database
#' DBI::dbListTables(conx)
#' 
#' # look at the media table
#' DBI::dbReadTable(conx, name = "visits")
#' 
#' # check the database; do not disconnect on exit
#' results <- dbCheckData(con = conx, disconnect = FALSE)
#'  
#' # the errors, if present, will be given in the captured output
#' str(results)
#' 
#' # -----------------------------------------------------
#' # the demo has no errors
#' # let's change some data entries so you can see the errors are returned
#' 
#' # introduce an out-of-sequence pull visit before a new set at this location
#' DBI::dbAppendTable(
#'     conx,
#'     name = 'visits',
#'     value = data.frame(
#'       pk_visitid = 100,
#'       fk_personid = "fbaggins",
#'       fk_locationid = "locationA",
#'       fk_equipmentid = "camera1",
#'       visit_type = "pull", 
#'       visit_date = "2024-01-01",
#'       visit_time = "12:00:00",
#'       visit_notes = NA,
#'       fk_econfigid = NA)
#'  )
#' 
#' # add a visit with invalid visit time
#' DBI::dbAppendTable(
#'     conx,
#'     name = 'visits',
#'     value = data.frame(
#'       pk_visitid = 101,
#'       fk_personid = "fbaggins",
#'       fk_locationid = "locationB",
#'       fk_equipmentid = "camera2",
#'       visit_type = "set", 
#'       visit_date = "2024-01-01",
#'       visit_time = "2:00",
#'       visit_notes = NA,
#'       fk_econfigid = NA
#'     ))
#'  
#' # run the check again, disconnect on exit
#' results <- dbCheckData(con = conx, disconnect = TRUE)
#' 
#' # look at the results
#' str(results)
#' 
#' # -----------------------------------------------------
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' 
#' }

dbCheckData <- function(con, check_list = NA, verbose = TRUE, disconnect = FALSE, ...) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }

  # Default: perform all checks
  if (is.na(check_list)) {
    check_list <- list(
      'visit_date',
      'visit_time',
      'media_date',
      'media_time',
      'visit_type_sequence',
      'media_start_times'
    )
  }
  
  # Initialize results list
  results <- data.frame(
    name = character(0),
    description = character(0),
    advice = character(0)
  )
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Check to make sure all visit dates are valid -----------------
  
  if ('visit_date' %in% check_list) {
    if (verbose) print('Checking: visit_date')
    
    visit_datetime <- dbGetQuery(con, 'SELECT pk_visitid, visit_date, visit_time FROM visits;')
    
    valid_visit_dates_mask <- sapply(
      visit_datetime$visit_date,
      function(x) {
        tryCatch(
          {
            as.POSIXlt.character(x, tryFormats = "%Y-%m-%d")
            TRUE
          },
          error = function(cond) {
            FALSE
          }
        )
      }
    )
    
    if (!all(valid_visit_dates_mask)) {
      results <- rbind(
        results,
        data.frame(
          name = 'invalid_visit_dates',
          description = paste(
            'Invalid visit dates for visits with pk_visitid = {',
            paste(visit_datetime$pk_visitid[!valid_visit_dates_mask], collapse = ', '),
            '}',
            sep = ""
          ),
          advice = 'Make sure all visits have dates in the format "YYYY-MM-DD".
            If dates are unavailable, consider using "1900-01-01" as a placeholder 
            so that queries will work properly in the AMMonitor application.'
        )
      )
    }
  }
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Check to make sure all visit times are valid -----------------
  
  if ('visit_time' %in% check_list) {
    if (verbose) print('Checking: visit_time')
    
    valid_visit_times_mask <- sapply(
      visit_datetime$visit_time,
      function(x) {
        tryCatch(
          {
            as.POSIXlt.character(x, tryFormats = "%H:%M:%S")
            TRUE
          },
          error = function(cond) {
            FALSE
          }
        )
      }
    )
    
    if (!all(valid_visit_times_mask)) {
      results <- rbind(
        results,
        data.frame(
          name = 'invalid_visit_times',
          description = paste(
            'Invalid visit times for visits with pk_visitid = {',
            paste(visit_datetime$pk_visitid[!valid_visit_times_mask], collapse = ', '),
            '}',
            sep = ""
          ),
          advice = 'Make sure all visits have times in the format "HH:MM:SS".
            If times are unavailable, consider using "12:00:00" as a placeholder 
            so that queries will work properly in the AMMonitor application.'
        )
      )
    }
  }
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Check to make sure all media dates are valid -----------------
  
  if ('media_date' %in% check_list) {
    if (verbose) print('Checking: media_date')
    
    valid_media_dates_mask <- sapply(
      dbGetQuery(con, 'SELECT start_date FROM media;')[,],
      function(x) {
        tryCatch(
          {
            as.POSIXlt.character(x, tryFormats = "%Y-%m-%d")
            TRUE
          },
          error = function(cond) {
            FALSE
          }
        )
      }
    )
    
    if (!all(valid_media_dates_mask)) {
      results <- rbind(
        results,
        data.frame(
          name = 'invalid_media_dates',
          description = paste(
            'Invalid dates for', sum(!valid_media_dates_mask), 'media files'
          ),
          advice = 'Make sure all media have dates in the format "YYYY-MM-DD".
            If dates are unavailable, consider using "1900-01-01" as a placeholder 
            so that queries will work properly in the AMMonitor application.'
        )
      )
    }
  }
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Check to make sure all media times are valid -----------------
  
  if ('media_time' %in% check_list) {
    if (verbose) print('Checking: media_time')
    
    valid_media_times_mask <- sapply(
      dbGetQuery(con, 'SELECT start_time FROM media;')[,],
      function(x) {
        tryCatch(
          {
            as.POSIXlt.character(x, tryFormats = "%H:%M:%S")
            TRUE
          },
          error = function(cond) {
            FALSE
          }
        )
      }
    )
    
    if (!all(valid_media_times_mask)) {
      results <- rbind(
        results,
        data.frame(
          name = 'invalid_media_times',
          description = paste(
            'Invalid times for', sum(!valid_media_times_mask), 'media files'
          ),
          advice = 'Make sure all media have dates in the format "YYYY-MM-DD".
            If dates are unavailable, consider using "1900-01-01" as a placeholder 
            so that queries will work properly in the AMMonitor application.'
        )
      )
    }
  }
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Are all visits appropriately sequenced? (i.e., set-check-check-check-pull) ----
  
  if ('visit_type_sequence' %in% check_list) {
    if (verbose) print('Checking: visit_type_sequence')
    
    all_locs <- dbReadTable(con, 'locations')
    for (i in 1:nrow(all_locs)) {
      
      loc <- all_locs$pk_locationid[i]
      
      # Get all visits
      visits <- dbGetQuery(
        con, 
        paste0(
          "SELECT * FROM visits WHERE fk_locationid = '",
          loc,
          "' ORDER BY visit_date, visit_time;"
        )
      )
      
      # Does visit type sequence make sense?
      equip_loc <- unique(visits$fk_equipmentid) # Unique equipment used at this location
      
      for (equip in equip_loc) {
        
        visit_seq <- visits$visit_type[visits$fk_equipmentid == equip]
        
        last_type <- NA
        for (visit_type in visit_seq) {
          
          # A "set" can only follow NA or a "pull"
          if (visit_type == "set" && !last_type %in% c(NA, 'pull')) {
            results <- rbind(
              results,
              data.frame(
                name = 'visit_type_mismatch',
                description = paste(
                  'A "set" can only follow NA or a "pull" ',
                  '{loc = ', loc, '; equip = ', equip, '}',
                  sep = ""
                ),
                advice = 'Look at the visit sequence for the specified location and equipment. Check for missing visits and/or visits containing errors (wrong date, lcoation, equipment, or visit type). Review the definition of each visit type carefully.'
              )
            )
          }
          
          # A "check" can only follow a "set" or a "check"
          if (visit_type == "check" && !last_type %in% c('set', 'check')) {
            results <- rbind(
              results,
              data.frame(
                name = 'visit_type_mismatch',
                description = paste(
                  "A 'check' can only follow a 'set' or a 'check'",
                  '{loc = ', loc, '; equip = ', equip, '}',
                  sep = ""
                ),
                advice = 'Look at the visit sequence for the specified location and equipment. Check for missing visits and/or visits containing errors (wrong date, lcoation, equipment, or visit type). Review the definition of each visit type carefully.'
              )
            )
          }
          
          # A "pull" can only follow a "set" or a "check"
          if (visit_type == "pull" && !last_type %in% c('set', 'check')) {
            results <- rbind(
              results,
              data.frame(
                name = 'visit_type_mismatch',
                description = paste(
                  'A "pull" can only follow a "set" or a "check"',
                  '{loc = ', loc, '; equip = ', equip, '}',
                  sep = ""
                ),
                advice = 'Look at the visit sequence for the specified location and equipment. Check for missing visits and/or visits containing errors (wrong date, lcoation, equipment, or visit type). Review the definition of each visit type carefully.'
              )
            )
          }
          last_type <- visit_type
        }
      }
    }
  }

  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Are media dates/times consistent with their associated visits? ----
  if ('media_start_times' %in% check_list) {
    if (verbose) print('Checking: media_start_times')
    
    time_threshold <- ifelse(
      'time_threshold' %in% names(list(...)),
      list(...)[['time_threshold']],
      21600 # seconds in 6 hours
    )
    
    all_locs <- dbReadTable(con, 'locations')
    for (i in 1:nrow(all_locs)) {
      
      loc <- all_locs$pk_locationid[i]
      
      # Get all visits
      visits <- dbGetQuery(
        con, 
        paste0(
          "SELECT * FROM visits WHERE fk_locationid = '",
          loc,
          "' ORDER BY visit_date, visit_time;"
        )
      )
      
      if (nrow(visits) == 0) {next}
      if (nrow(visits) == 1) {next}
      
      equipment <- DBI::dbReadTable(con, 'equipment')
      
      equiptypes <- unique(equipment$equip_type)
      
      if (!all(is.na(unique(equiptypes)))) {
        equiptypes <- equiptypes[!is.na(equiptypes)]
      
          for (type in equiptypes) {
       
            equipmentids <- equipment$pk_equipmentid[
              which(equipment$equip_type == type |
                      is.na(equipment$equip_type))]
            
            visits_subset <- visits[which(
              visits$fk_equipmentid %in% equipmentids),]
            
            if (nrow(visits_subset) < 2) {next}
            
              for (j in 2:nrow(visits_subset)) {
              
              photos <- dbGetQuery(
                con, 
                paste0(
                  'SELECT * FROM media WHERE fk_visitid = ', visits$pk_visitid[j], ';'
                )
              )
      
              photo_dates <- paste(photos$start_date, photos$start_time)
              set_date <- paste(visits[j-1,c('visit_date', 'visit_time')], collapse = " ")
              set_date_buff <- as.character(as.POSIXct(set_date)-time_threshold)
              pull_date <- paste(visits[j,c('visit_date', 'visit_time')], collapse = " ")
              pull_date_buff <- as.character(as.POSIXct(pull_date)+time_threshold)
             
              if (!all(photo_dates > set_date_buff)) {
                print(paste('set', loc, pull_date))
                results <- rbind(
                  results,
                  data.frame(
                    name = 'out_of_bound_startDate',
                    description = paste(
                      'Media start_date more than ', time_threshold, 
                      ' seconds before matching visit date ',
                      '{loc = ', loc, '; visit = ', visits$pk_visitid[j-1], '}',
                      sep = ""
                    ),
                    advice = 'First, make sure all dates/times of visits and media are valid. Next, make sure all visit sequences are valid. Assuming all times and visit sequences are valid, the most likely cause of this error is that visit times or media times are valid but incorrect. To correct media times for a specified visit, you can use the "shiftMediaTime" app.'
                  )
                )
              }
              if (!all(photo_dates < pull_date_buff)) {
                results <- rbind(
                  results,
                  data.frame(
                    name = 'out_of_bound_startDate',
                    description = paste(
                      'Media start_date more than ', time_threshold, 
                      ' seconds after matching visit date ',
                      '{loc = ', loc, '; visit = ', visits$pk_visitid[j], '}', "visit_time = ", visits$visit_time[j],
                      sep = ""
                    ),
                    advice = 'First, make sure all dates/times of visits and media are valid. Next, make sure all visit sequences are valid. Assuming all times and visit sequences are valid, the most likely cause of this error is that visit times or media times are valid but incorrect. To correct media times for a specified visit, you can use the "shiftMediaTime" app.'
                  )
                )
              }
              
            }
        }
        
      } else { # end if not all equiptypes are na

          for (j in 2:nrow(visits)) {
            
            photos <- dbGetQuery(
              con, 
              paste0(
                'SELECT * FROM media WHERE fk_visitid = ', visits$pk_visitid[j], ';'
              )
            )
            
            photo_dates <- paste(photos$start_date, photos$start_time)
            set_date <- paste(visits[j-1,c('visit_date', 'visit_time')], collapse = " ")
            set_date_buff <- as.character(as.POSIXct(set_date)-time_threshold)
            pull_date <- paste(visits[j,c('visit_date', 'visit_time')], collapse = " ")
            pull_date_buff <- as.character(as.POSIXct(pull_date)+time_threshold)
            
            if (!all(photo_dates > set_date_buff)) {
              print(paste('set', loc, pull_date))
              results <- rbind(
                results,
                data.frame(
                  name = 'out_of_bound_startDate',
                  description = paste(
                    'Media start_date more than ', time_threshold, 
                    ' seconds before matching visit date ',
                    '{loc = ', loc, '; visit = ', visits$pk_visitid[j-1], '}',
                    sep = ""
                  ),
                  advice = 'First, make sure all dates/times of visits and media are valid. Next, make sure all visit sequences are valid. Assuming all times and visit sequences are valid, the most likely cause of this error is that visit times or media times are valid but incorrect. To correct media times for a specified visit, you can use the "shiftMediaTime" app.'
                )
              )
            }
            if (!all(photo_dates < pull_date_buff)) {
              results <- rbind(
                results,
                data.frame(
                  name = 'out_of_bound_startDate',
                  description = paste(
                    'Media start_date more than ', time_threshold, 
                    ' seconds after matching visit date ',
                    '{loc = ', loc, '; visit = ', visits$pk_visitid[j], '}', "visit_time = ", visits$visit_time[j],
                    sep = ""
                  ),
                  advice = 'First, make sure all dates/times of visits and media are valid. Next, make sure all visit sequences are valid. Assuming all times and visit sequences are valid, the most likely cause of this error is that visit times or media times are valid but incorrect. To correct media times for a specified visit, you can use the "shiftMediaTime" app.'
                )
              )
            } # end pull date error
          } # end looping through visits
        }# end else (where all equiptypes are na)
        
      } # end looping through all_locs
    } # end media start times
  

  return(results)
}
