#' @name dbGetSummaryData
#' @aliases dbGetSummaryData
#' @title Creates a list of dataframes from a database for summary purposes
#' @description Creates a list of dataframes suitable for passing into the 
#' \code{dbTableSummary()} or \code{dbPlotSummary()} functions.
#' @param con An open database connection
#' @param m_dates  A vector of the start and end dates of media to be 
#' included in the summary.
#' Default is NA, which summarizes the entire database.
#' @param disconnect TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @usage dbGetSummaryData(con, m_dates = c(NA, NA), disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery dbReadTable 
#' @export
#' @concept media
#' @concept people
#' @concept visits
#' @concept locations
#' @concept taxa
#' @concept annotations
#' @concept modeloutputs
#' @details  \code{dbGetSummaryData()} generates a list of database dataframes that
#' is suitable for passing to the \code{dbPlotSummary()} or \code{dbTableSummary()}
#' functions. The returned object may be quite large, but enables users to work 
#' with database information with R functions rather than making SQL calls to the 
#' database. 
#' 
#' The dataframes returned include:
#' \itemize{
#'   \item \strong{media}: Dataframe that contains information on the media 
#'   collected within the specified time span.
#'   \item \strong{field_visits}: Dataframe that contains information
#'   on field visits done within the specified time span.
#'   \item \strong{media_visits}: Dataframe that contains information 
#'   that links media dates with visit dates.
#'   \item \strong{locations}: Dataframe that contains information on
#'   locations associated with media files collected within the 
#'   specified time span.
#'   \item \strong{annotations}: Dataframe containing information on
#'   annotations made on media files collected within the specified
#'   time span.
#'   \item \strong{taxa}: Dataframe containing information on taxa
#'   associated with annotations made on media files collected within the 
#'   specified time span.
#'   \item \strong{modeloutputs}: Dataframe containing information on
#'   on machine learning model outputs associated with media files collected
#'   within the specified time span.
#'}
#'
#' See the "dbsummary" learnr tutorial for more details on the
#' dbGetSummaryData() function. The tutorial can be launched with
#' \code{learnr::run_tutorial(name = "dbsummary", package = "AMMonitor")}.
#' 
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # look at the folders within an AMMonitor project; notice the database folder
#' list.files(demo_fp, recursive = FALSE)
#' 
#' # the sample database is named "demo.sqlite"
#' list.files(file.path(demo_fp, "database"))
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # get summary data and disconnect from database
#' results <- dbGetSummaryData(con = conx, disconnect = FALSE)
#'  
#' # look at the returned object; it is a list of dataframes
#' str(results, max.level = 1)
#' 
#' # look at the first 3 records of each dataframe
#' lapply(results,  FUN = head, n = 3)
#' 
#' # an example with date subsetting ----------------------
#' 
#' # get summary data within specified data range and disconnect from database
#' results <- dbGetSummaryData(
#'   con = conx,
#'   m_dates = c("2023-05-01", "2023-12-31"),
#'   disconnect = TRUE
#' )
#' 
# look at the structure of the results
#' str(results, max.level = 1)
#' 
#' # -----------------------------------------------------
#' 
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' 
#' } 
#' 
#' 
dbGetSummaryData <- function(con, m_dates = c(NA, NA), disconnect = FALSE){
  
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
 # create list to store results
  results <- list()
  
  # check dates entered
  if (!is.na(m_dates[1])) {
    tryCatch( {
      as.Date(m_dates[1])
    }, error = function(e) {
      stop('Please enter valid dates in the format YYYY-MM-DD.')
    })
  }
  
  if (!is.na(m_dates[2])) {
    tryCatch( {
      as.Date(m_dates[2])
    }, error = function(e) {
      stop('Please enter valid dates in the format YYYY-MM-DD.')
    })
  }

  startDate <- ifelse(test = is.na(m_dates[1]), yes = "1900-01-01", no = m_dates[1])
  endDate <- ifelse(test = is.na(m_dates[2]), yes = format(Sys.Date(), "%Y-%m-%d"), no = m_dates[2])

  # get media =====================
  stmnt <- paste0(
    "SELECT media.pk_mediaid, media.fk_visitid, media.media_type, media.start_date, media.start_time, visits.fk_locationid
     FROM visits INNER JOIN media ON visits.pk_visitid = media.fk_visitid
     WHERE (start_date >= date('", startDate, "') AND start_date <= date('", endDate, "'));")


  media <- DBI::dbGetQuery(con, stmnt)
  media$start_date <- as.Date(media$start_date)
  media <- media[order(media$start_date), ]
  media$year <- format(media$start_date,"%Y")
  media$month <- format(media$start_date,"%m")
  

  
  
  # field visits =========================
  stmnt <- paste0("SELECT * FROM visits
    WHERE  (visit_date >= date('", startDate, "') AND visit_date <= date('", endDate, "'));")
  field_visits <- DBI::dbGetQuery(con, stmnt)

  field_visits$visit_date <- as.Date(field_visits$visit_date)
  field_visits$year <- format(field_visits$visit_date,"%Y")
  field_visits <- field_visits[order(field_visits$visit_date), ]
  field_visits$month <- format(field_visits$visit_date,"%m")
  
  # ensure there are visits and/or media within given date range
  if (nrow(field_visits) == 0 & nrow(media) == 0) {
    stop('There are no field visits or media within the given date range.')
  }
  
   # add to results
  results$media <- media
  message(paste0("The media dataframe of ", nrow(media), " rows has been added to the results list."))
  
  # add to results
  results$field_visits <- field_visits
  message(paste0("The field_visits dataframe of ", nrow(field_visits), " rows has been added to the results list."))
  
  # entire list of field visits, to be used for survey effort in tables
  all_field_visits <- DBI::dbReadTable(con, name = 'visits')
  all_field_visits$visit_date <- as.Date(all_field_visits$visit_date)
  all_field_visits$year <- format(all_field_visits$visit_date,"%Y")
  all_field_visits <- all_field_visits[order(all_field_visits$visit_date), ]
  all_field_visits$month <- format(all_field_visits$visit_date,"%m")
  
  results$all_field_visits <- all_field_visits
  message(paste0("The all_field_visits dataframe of ", nrow(all_field_visits),
                 " rows has been added to the results list."))
  
  # get visits of media ======================
  unique_visits <- unique(media$fk_visitid)
  visit_string <- paste(unique_visits, collapse = ",")
    
  stmnt <-  paste0(
      "SELECT visits.pk_visitid, visits.visit_type, visits.visit_date, visits.visit_time, 
         visits.fk_personid, visits.fk_locationid, visits.fk_equipmentid
       FROM visits
       WHERE pk_visitid IN (", visit_string, ");"
  )
  
  visits <- DBI::dbGetQuery(con, stmnt)
  visits$visit_date <- as.Date(visits$visit_date)
  visits$year <- format(visits$visit_date,"%Y")
  visits <- visits[order(visits$visit_date), ]
  visits$month <- format(visits$visit_date,"%m")

  
  
  # add to results
  results$media_visits <- visits
  message(paste0("The media_visits dataframe of ", nrow(visits), " rows has been added to the results list."))
  
  # get survey effort dates ============
  
  survey_effort <- qryEffort(con)
  
  survey_effort <- survey_effort[
    which(survey_effort$pk_visitid %in% visits$pk_visitid),]
  
  results$survey_effort <- survey_effort
  
  # get locations ====================
  unique_locations <- unique(visits$fk_locationid)
  location_string <-  paste0("'", unique_locations, "'", collapse = ", ")
  
  stmnt <-  paste0(
    "SELECT locations.pk_locationid, locations.location_type, 
      locations.lat, locations.long, locations.location_status
    FROM locations
    WHERE pk_locationid IN (", location_string, ");"
  )
  
  locations <- DBI::dbGetQuery(con, stmnt)
  locations[which(is.na(locations$location_type)), "location_type"] <- "unknown"
  locations[which(is.na(locations$location_status)), "location_status"] <- "unknown"
  
  results$locations <- locations
  message(paste0("The locations dataframe of ", nrow(locations), " rows has been added to the results list."))
  
  # get annotations ==============================
  media_string <- paste0(media$pk_mediaid, collapse = ", ")

  stmnt <- paste0(
    "SELECT annotations.pk_annotationid, annotations.fk_personid, annotations.fk_mediaid, 
     annotations.fk_searchlistid, annotations.fk_taxonid, taxa.common_name, media.fk_visitid, visits.fk_locationid, media.start_date
    FROM (visits INNER JOIN (media INNER JOIN annotations ON media.pk_mediaid = annotations.fk_mediaid) ON visits.pk_visitid = media.fk_visitid) INNER JOIN taxa ON annotations.fk_taxonid = taxa.pk_taxonid
    WHERE fk_mediaid IN (", media_string, ");"
  )
  
  annotations <- DBI::dbGetQuery(con, stmnt)
  annotations$start_date <- as.Date(annotations$start_date)
  annotations$year <- format(annotations$start_date,"%Y")
  annotations$month <- format(annotations$start_date,"%m")
  
  results$annotations <- annotations
  message(paste0("The annotations dataframe of ", nrow(annotations), " rows has been added to the results list."))
  
  # get annotationverifications
  annotation_string <- paste0(annotations$pk_annotationid, collapse = ", ")
  
  stmnt <- paste0(
    "SELECT annotationverifications.*, annotations.*, media.fk_visitid, visits.fk_locationid, media.start_date
FROM visits INNER JOIN (media INNER JOIN (annotations INNER JOIN annotationverifications ON annotations.pk_annotationid = annotationverifications.fk_annotationid) ON media.pk_mediaid = annotations.fk_mediaid) ON visits.pk_visitid = media.fk_visitid
    WHERE fk_annotationid IN (", annotation_string, ");"
  )
  
  annotationverifications <- DBI::dbGetQuery(con, stmnt)
  annotationverifications$start_date <- as.Date(annotationverifications$start_date)
  annotationverifications$year <- format(annotationverifications$start_date,"%Y")
  annotationverifications$month <- format(annotationverifications$start_date,"%m")
  
  
  results$annotationverifications <- annotationverifications
  message(paste0("The annotationverifications dataframe of ", nrow(annotationverifications), " rows has been added to the results list."))
  
  # get annotags ===============================
  tag_string <- paste0(annotations$pk_annotationid, collapse = ", ")
  stmnt <- paste0("SELECT annotations.pk_annotationid, annotations.fk_mediaid, annotations.fk_taxonid,
     librarylistitems.fk_librarylistid, librarylistitems.item, annotags.value_num
     FROM librarylistitems 
     INNER JOIN (annotags INNER JOIN annotations ON annotags.fk_annotationid = annotations.pk_annotationid) 
     ON librarylistitems.pk_librarylistitemid = annotags.fk_librarylistitemid
     WHERE pk_annotagid IN (", tag_string, ");")

  annotags <- DBI::dbGetQuery(con, stmnt)
  
  results$annotags <- annotags
  message(paste0("The annotags dataframe of ", nrow(annotags), " rows has been added to the results list."))
  

  # get modeloutputs =================================
  outputs_string <- media_string
  
  stmnt <- paste0(
    "SELECT modeloutputs.pk_modeloutputid, modeloutputs.fk_mediaid, modeloutputs.fk_modelid, 
      modeloutputs.fk_taxonid, models.model_name, visits.fk_locationid, media.start_date
     FROM visits INNER JOIN (media INNER JOIN (models INNER JOIN modeloutputs ON models.pk_modelid = modeloutputs.fk_modelid) ON media.pk_mediaid = modeloutputs.fk_mediaid) ON visits.pk_visitid = media.fk_visitid
     WHERE fk_mediaid IN (", outputs_string, ");"
  )
  
  modeloutputs <- DBI::dbGetQuery(con, stmnt)
  modeloutputs$start_date <- as.Date(modeloutputs$start_date)
  modeloutputs$year <- format(modeloutputs$start_date,"%Y")
  modeloutputs$month <- format(modeloutputs$start_date,"%m")
  
  results$modeloutputs <- modeloutputs
  message(paste0("The modeloutputs dataframe of ", nrow(modeloutputs), " rows has been added to the results list."))

  # get taxa ===============================
  unique_taxa <- unique(c(annotations$fk_taxonid, modeloutputs$fk_taxonid))
  unique_taxa <- sapply(unique_taxa, function(x) {gsub(pattern = "'", replacement = "''", x = x)}) # Handle apostrophes in taxa names
  taxa_string <- paste0("'", unique_taxa, "'", collapse = ", ")
  stmnt <- paste0("SELECT * FROM taxa 
     WHERE pk_taxonid IN (", taxa_string, ");")
  taxa <- DBI::dbGetQuery(con, stmnt)
  
  results$taxa <- taxa
  message(paste0("The taxa dataframe of ", nrow(taxa), " rows has been added to the results list."))
  
  # get modelverifications
  modeloutputs_string <- paste0(modeloutputs$pk_modeloutputid, collapse = ", ")
  
  stmnt <- paste0(
    "SELECT modelverifications.*, visits.fk_locationid, models.model_name, modeloutputs.fk_taxonid
    FROM visits, models INNER JOIN (modeloutputs INNER JOIN modelverifications ON modeloutputs.pk_modeloutputid = modelverifications.fk_modeloutputid) ON models.pk_modelid = modeloutputs.fk_modelid
    WHERE fk_modeloutputid IN (", modeloutputs_string, ");"
  )
  
  modelverifications <- DBI::dbGetQuery(con, stmnt)
  results$modelverifications <- modelverifications
  message(paste0("The modelverifications dataframe of ", nrow(modelverifications), " rows has been added to the results list."))
   
  # return results
  return(results)
  
 
 

} # end of function
