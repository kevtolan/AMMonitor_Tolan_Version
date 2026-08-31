# qry_row --------------
#' @name qry_row
#' @aliases qry_row
#' @title Retrieve one or more rows from a specified table from the AMMonitor
#' database given a set of conditions on one or more columns.
#' @description Returns dataframe containing specified columns and rows from the
#' AMMonitor database based on specified conditions.
#' @param con The filepath to the AMMonitor database file
#' @param tableName The name of the table of the AMMonitor database (e.g. 
#' people, media, locations)
#' @param rowConditions A dataframe where the keys correspond to columns of the
#' specified dataframe and key values correspond to the equality condition that
#' must be satisfied by any returning rows, else returns all rows 
#' (default returns all rows).
#' @param colConditions A vector specifying the names of columns to be returned
#' from the query (default returns all columns).
#' @param con Existing connection to the database
#' @param disconnect TRUE or FALSE. Should the connection be severed on exit?
#' Default is FALSE.
#' @usage qry_row(con, tableName, rowConditions = NA, colConditions = "*", disconnect = FALSE)
#' @importFrom DBI dbIsValid dbGetQuery dbDisconnect
#' @return  Dataframe consisting of the specified rows and columns
#' @family qry
#' @export
#' @examples
#'\dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' qry_row(
#'   con = conx,
#'   tableName = 'locations',
#'   rowConditions = list(location_type = 'monitoring_station'),
#'   colConditions = 'pk_locationid',
#'   disconnect = TRUE
#' )
#' 
#' # unlink the demo
#' unlink(demo_fp)
#'
#'}
NULL

qry_row <- function(con = NA, tableName, rowConditions = NA, colConditions = '*', disconnect = FALSE){
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  stmnt <- paste("SELECT", paste(colConditions, collapse = ', '))
  stmnt <- paste(stmnt, 'FROM', tableName)
  
  if (is.list(rowConditions)) {
    for (i in 1:length(rowConditions)) {
      if (i == 1) {
        stmnt <- paste(stmnt, 'WHERE')
      } else {
        stmnt <- paste(stmnt, 'AND')
      }
      if (is.numeric(rowConditions[[i]])) {
        stmnt <- paste0(
          stmnt,
          " (((",
          tableName,
          ".",
          names(rowConditions)[i],
          ")=",
          rowConditions[[i]],
          "))"
        )
      } else {
        stmnt <- paste0(
          stmnt,
          " (((",
          tableName,
          ".",
          names(rowConditions)[i],
          ")='",
          gsub("'", "''", rowConditions[[i]]),
          "'))"
        )
      }
    }
  }
  stmnt <- paste0(stmnt, ';')
  
  # run query
  rs <- DBI::dbGetQuery(
    con,
    statement = stmnt
  )
  

  # return result
  return(rs)
}

# qryTags --------------
#' @name qryTags
#' @aliases qryTags
#' @title Retrieves all tags that can be applied to a media file
#' @description  Retrieves all tags that can be applied to a media file. (These
#' are not the actual tags; rather they are the possible tags.)
#' @param con An existing database connection
#' @param media  TRUE or FALSE. Should tags associated with full media be 
#' returned? Default is FALSE.
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#'  exit?  Default is FALSE.
#' @importFrom DBI  dbIsValid dbClearResult dbSendQuery dbDisconnect dbFetch
#' @usage qryTags(con, media = FALSE, disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept medialistitems
#' @concept medialists
#' @concept librarylists
#' @concept librarylistitems
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' # Retrieve library tags only (tags that can be applied to taxa)
#' qryTags(con = conx, media = FALSE, disconnect = FALSE)
#'
#' # Retrieve library and media tags
#' qryTags(con = conx, media = TRUE, disconnect = TRUE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryTags <- function(con, media = FALSE, disconnect = FALSE) {

  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")

  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }

  if (media == TRUE) {
    statement <- "SELECT medialistitems.pk_medialistitemid, medialistitems.fk_medialistid,
    medialistitems.item, medialistitems.sort_order, medialists.photos, medialists.recordings, 
    medialists.videos, medialists.list_type, medialistitems.calculated
    FROM medialists LEFT JOIN medialistitems 
    ON medialists.pk_medialistid = medialistitems.fk_medialistid
    ORDER BY fk_medialistid, sort_order;"

  } else {
    statement <- "SELECT librarylistitems.pk_librarylistitemid, librarylistitems.item,
    librarylistitems.sort_order, librarylistitems.fk_librarylistid, librarylists.photos,
    librarylists.recordings, librarylists.videos, librarylists.list_type,
    librarylists.fk_taxonid, librarylists.taxa_list, librarylists.fk_child_librarylistid
    FROM librarylists LEFT JOIN librarylistitems
    ON librarylists.pk_librarylistid = librarylistitems.fk_librarylistid
    ORDER BY fk_librarylistid, sort_order;"
  }

  # send the query
  result <- DBI::dbSendQuery(
    conn = con,
    statement = statement)

  # fetch and return result
  rslt <- DBI::dbFetch(result)
  DBI::dbClearResult(result)

  return(rslt)
}

# qryItems ---------------------------
#' @name qryItems
#' @title Retrieves all items from a given list
#' @description  Retrieve items associated with a list, a librarylist, or a medialist.
#' @param con An existing database connection
#' @param table  The database table name. Options are "lists", "medialists", 
#'  "librarylists" or "temporallists"
#' @param listname The name of a list
#' @param item The name of a particular item with in a list, if desired.
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbFetch dbClearResult dbSendQuery dbDisconnect
#' @usage qryItems(con, table, listname, item = NA, disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept medialistitems
#' @concept medialists
#' @concept librarylists
#' @concept librarylistitems
#' @concept lists
#' @concept listitems
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' # retrieve items from the listitems table
#' qryItems(
#'   con = conx, 
#'   table = "lists", 
#'   listname = "datum", 
#'   item = "WGS84",
#'   disconnect = FALSE
#' )
#'
#' # retrieve all items from the media_type list
#' qryItems(
#'   con = conx, 
#'   table = "lists", 
#'   listname = "media_type",
#'   disconnect = TRUE
#'  )
#'  
#' # unlink the demo
#' unlink(demo_fp)
#'
#' }
NULL

qryItems <- function(con, table,  listname, item = NA, disconnect = FALSE) {
  
  # check if valid con
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # ensure table argument is correct
  if (table %in% c("lists", "librarylists", "medialists", "temporallists") == FALSE) {
    stop("This function retrieves list items from the lists, librarylists, medialists,
         or temporallists tables only.")
  }
  
  # get the child table
  tbl <- switch(
    table,
    "lists" = "listitems",
    "librarylists" = "librarylistitems",
    "medialists" = "medialistitems",
    "temporallists" = "temporallistitems")
  
  
  # get the foreign key
  fk <- switch(
    table,
    "lists" = "fk_listid",
    "librarylists" = "fk_librarylistid",
    "medialists" = "fk_medialistid",
    "temporallists" = "fk_temporallistid")
  
  # write the query statement
  if (is.na(item)) {
    statement <- paste0(
      "SELECT * FROM ", tbl, " WHERE ", fk, " = '",
      listname, "' ;")
  } else {
    statement <- paste0(
      "SELECT * FROM ", tbl, " WHERE ",fk, " = '",
      listname, "' AND item = '", item, "';")
  }
  
  # send the query
  result <- DBI::dbSendQuery(
    conn = con,
    statement = statement)
  
  # fetch and return result
  rslt <- DBI::dbFetch(result)
  DBI::dbClearResult(result)
  
  return(rslt)
}

# qryMediaLocations ------------------
#' @name qryMediaLocations
#' @title Retrieves locations with media files of a certain type
#' @description Retrieves distinct locations with specified types of media files
#' @param con An existing database connection
#' @param mediaType Type of media, one of "photo", "audio", "video"
#' @param disconnect  TRUE or FALSE. Should the database connection be closed 
#' on exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult 
#' dbDisconnect
#' @usage qryMediaLocations(con, mediaType, disconnect)
#' @return character vector of all available locations
#' @export
#' @family query
#' @concept media
#' @concept visits 
#' @concept locations
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # get the locations with photos
#' qryMediaLocations(con = conx, mediaType = "photo", disconnect = FALSE)
#' 
#' # get the locations with recordings
#' qryMediaLocations(con = conx, mediaType = "audio", disconnect = TRUE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryMediaLocations <- function(con, mediaType, disconnect = FALSE) {
  # check if valid con
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  rs <- DBI::dbSendQuery(
    con,
    "SELECT DISTINCT fk_locationid FROM visits 
    INNER JOIN media ON visits.pk_visitid = media.fk_visitid 
    WHERE media.media_type = $1 
    ORDER BY fk_locationid;"
  )
  
  DBI::dbBind(rs, list(mediaType))
  locations <- DBI::dbFetch(rs)[,]
  DBI::dbClearResult(rs)
  
  locations
}

# qryVisitTable ----------------------
#' @name qryVisitTable
#' @aliases qryVisitTable
#' @title Retrieves counts of files and annotated files per visit to a location
#' @description Retrieves counts of files and annotated files per visit to a location
#' @param con An existing database connection
#' @param mediaType type of media to filter for. Options are "audio" or "photo".
#' @param location location to retrieve visits from
#' @param personid personid to retrieve annotations from
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult
#'  dbDisconnect
#' @usage qryVisitTable(con, mediaType, location, personid, disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept media
#' @concept visits 
#' @concept people
#' @concept locations
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # get information on photo media files collected at locationA 
#' qryVisitTable(
#'  con = conx, 
#'  mediaType = "photo", 
#'  location = "locationA", 
#'  personid = NA, 
#'  disconnect = FALSE)
#'  
#' # get information on audio media files collected at locationA 
#' qryVisitTable(
#'  con = conx, 
#'  mediaType = "audio", 
#'  location = "locationA", 
#'  personid = NA, 
#'  disconnect = TRUE)
#'
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryVisitTable <- function(con, mediaType, location, personid = NA, disconnect = FALSE) {
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  total_media_col_header <- switch(
    mediaType,
    "photo" = "total_photos",
    "audio" = "total_recordings"
  )
  
  params <- list(mediaType, location)
  stmnt <- paste0(
    "SELECT pk_visitid, q1visitdate AS visit_date, totalmedia as ", total_media_col_header, ", totalannotated AS totalannotated  FROM 
    ((SELECT visits.pk_visitid, visits.fk_locationid AS q1location, visits.visit_date AS q1visitdate, Round(Count(DISTINCT media.pk_mediaid)) AS totalmedia
    FROM visits INNER JOIN media ON visits.pk_visitid = media.fk_visitid  
    WHERE media.media_type = $1 AND visits.fk_locationid = $2
    GROUP BY visits.fk_locationid, visits.pk_visitid) AS v1 LEFT JOIN 
    (SELECT visits.fk_locationid AS q2location, visits.visit_date AS q2visitdate, Round(Count(DISTINCT media.pk_mediaid)) AS totalAnnotated 
    FROM (annotations INNER JOIN media ON annotations.fk_mediaid = media.pk_mediaid) INNER JOIN visits ON media.fk_visitid = visits.pk_visitid "
  )
  
  # Filter by user for tag totals, if user specified
  if (!is.na(personid)) {
    params <- append(params, personid)
    stmnt <- paste0(stmnt, 'AND annotations.fk_personid = $3 ')
  }
  
  # Add group by statement
  stmnt <- paste0(stmnt, "GROUP BY visits.fk_locationid, visits.pk_visitid) AS v2 ON q1location = q2location AND q1visitdate = q2visitdate);")
  
  rs <- DBI::dbSendQuery(con, stmnt)
  DBI::dbBind(rs, params)
  visits <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  return(visits)
}

# qryMedia ----------------------
#' @name qryMedia
#' @title Filter a list of media based on many criteria
#' @description Filter a list of media based on many criteria
#' @param con SQLite database connection
#' @param disconnect  TRUE or FALSE Should the database connection be closed on 
#' exit? Default is FALSE.
#' @param locationID specify a location (pk_locationid)
#' @param dateRange Specify a date range
#' @param visitID Specify a specific visit (pk_visitid)
#' @param taxonID Specify a specific taxon (pk_taxonid)
#' @param excludeAnnotated Specify when to exclude based on annotation status
#' @param excludeAnnoVerified Specify when to exclude based on verification status
#' @param showAnnotated TRUE or FALSE. Show only annotated files. 
#' Default is FALSE
#' @param showUNAnnotated TRUE or FALSE. Show only un-annotated files. 
#' Default is FALSE
#' @param selectedUser The selected user (if any) to filter by taxon 
#' (pk_personid)
#' @param verify TRUE or FALSE. If tagger is in "verify" mode. Default is FALSE
#' @param mediaType type of media to filter for, one of (photo, audio, video).
#' Default is "photo."
#' @param limit Maximum number of records to be returned
#' @param offset The number of records to offset the query by
#' @importFrom DBI dbIsValid dbDisconnect dbSendQuery dbBind dbFetch
#' dbClearResult
#' @usage qryMedia(con, 
#'  disconnect = FALSE, 
#'  locationID = 'all',
#'  dateRange = list(c('1900-01-01', as.character(Sys.Date()))), 
#'  visitID = NULL, 
#'  taxonID = 'all', 
#'  excludeAnnotated = 'NA', 
#'  excludeAnnoVerified = 'NA', 
#'  showAnnotated = FALSE, 
#'  showUNAnnotated = FALSE, 
#'  selectedUser = NA, 
#'  verify = FALSE, 
#'  mediaType = "photo",
#'  limit = Inf,
#'  offset = 0
#' )
#' @family query
#' @concept media
#' @concept locations
#' @concept taxa
#' @concept visits
#' @concept annotations
#' @concept annotationverification
#' @return A dataframe with key columns for all media that meet the given criteria
#' @export
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # run the query to get media that have moose annotations
#' qryMedia(conx, taxonID = "moose", disconnect = FALSE)
#' 
#' # run a location query to get media associated with locationA
#' qryMedia(conx, locationID = "locationA", disconnect = FALSE)
#' 
#' # run a query to get media associate with visit 1
#' qryMedia(conx, visitID = 10, disconnect = TRUE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
#' 
NULL

qryMedia <- function(con, disconnect = FALSE, locationID = 'all', dateRange = list(c('1900-01-01', as.character(Sys.Date()))), visitID = NULL, taxonID = 'all', excludeAnnotated = 'NA', excludeAnnoVerified = 'NA', showAnnotated = FALSE, showUNAnnotated = FALSE, selectedUser = NA, verify = FALSE, mediaType = "photo", limit = Inf, offset = 0) {
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  params <- list(mediaType, as.character(dateRange[[1]][1]), as.character(dateRange[[1]][2]))
  if (verify) {
    stmnt <- paste0("SELECT DISTINCT media.pk_mediaid, media.filename, media.filepath, media.start_date, media.start_time FROM (media INNER JOIN visits ON media.fk_visitid = visits.pk_visitid) 
    INNER JOIN annotations ON media.pk_mediaid = annotations.fk_mediaid LEFT JOIN annotationverifications ON annotations.pk_annotationid = annotationverifications.fk_annotationid  WHERE media.media_type = $1")
  } else {
    stmnt <- paste0("SELECT DISTINCT media.pk_mediaid, media.filename, media.filepath, media.start_date, media.start_time FROM (media INNER JOIN visits ON media.fk_visitid = visits.pk_visitid) 
    LEFT JOIN annotations ON media.pk_mediaid = annotations.fk_mediaid WHERE media.media_type = $1")
  }
  
  where_clauses <- c(
    paste0('start_date >= $2'),
    paste0('start_date <= $3')
  )
  
  param_counter <- 4
  
  if (locationID != 'all') {
    where_clauses <- c(where_clauses, paste0('fk_locationid = $', param_counter))
    params[[param_counter]] <- locationID
    param_counter <- param_counter + 1
  }
  
  if (!is.null(visitID)) {
    where_clauses <- c(where_clauses, paste0('fk_visitid = $', param_counter))
    params[[param_counter]] <- visitID
    param_counter <- param_counter + 1
  }
  
  if (taxonID != 'all') {
    where_clauses <- c(where_clauses, paste0('annotations.fk_taxonid = $', param_counter))
    params[[param_counter]] <- taxonID
    param_counter <- param_counter + 1
    
    if (!is.na(selectedUser)) {
      where_clauses <- c(
        where_clauses,
        ifelse(
          verify,
          paste0('annotations.fk_personid != $', param_counter),
          paste0('annotations.fk_personid = $', param_counter)
        )
      )
      verify <- FALSE
      params[[param_counter]] <- selectedUser
      param_counter <- param_counter + 1
    }
  }
  
  switch(
    excludeAnnotated,
    'NA' = {},
    'Me' = {
      where_clauses <- c(
        where_clauses,
        paste0('media.pk_mediaid NOT IN (SELECT fk_mediaid FROM annotations WHERE annotations.fk_personid = $', param_counter, ')')
      )
      params[[param_counter]] <- selectedUser
      param_counter <- param_counter + 1
    },
    'Anyone' = {
      where_clauses <- c(
        where_clauses,
        'annotations.pk_annotationid IS NULL'
      )
    }
  )
  
  switch(
    excludeAnnoVerified,
    'NA' = {},
    'Me' = {
      where_clauses <- c(
        where_clauses,
        paste0('annotations.pk_annotationid NOT IN (SELECT fk_annotationid FROM annotationverifications WHERE annotationverifications.fk_personid = $', param_counter, ')')
      )
      params[[param_counter]] <- selectedUser
      param_counter <- param_counter + 1
    },
    'Anyone' = {
      where_clauses <- c(
        where_clauses,
        'annotationverifications.is_valid IS NULL'
      )
    }
  )
  
  if (verify) {
    where_clauses <- c(
      where_clauses,
      paste0('annotations.fk_personid != $', param_counter)
    )
    params[[param_counter]] <- selectedUser
    param_counter <- param_counter + 1
  }
  
  if (length(where_clauses) != 0) {
    stmnt <- paste(stmnt, 'AND', paste(where_clauses, collapse = ' AND '))
  }
  
  # Add ordering
  stmnt <- paste(stmnt, ' ORDER BY start_date, start_time')
  
  # Add LIMIT clause
  if (limit != Inf) {
    stmnt <- paste(stmnt, 'LIMIT', limit)
  }
  
  # Add OFFSET clause
  if (offset != 0) {
    stmnt <- paste(stmnt, 'OFFSET', offset)
  }
  
  stmt <- paste0(stmnt, ";")
  
  result <- DBI::dbSendQuery(con, stmnt)
  DBI::dbBind(result, params)
  media <- DBI::dbFetch(result)
  DBI::dbClearResult(result)
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  return(media)
}

# qryModelOutputsMedia -----------------------
#' @name qryModelOutputsMedia
#' @title Filter a list of media with model outputs based on many criteria
#' @description Filter a list of media based on many criteria, but only include
#' media with model outputs.
#' @param con An open SQLite database connection
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on 
#' exit?  Default is FALSE.
#' @param locationID Specify a location (pk_locationid) from the locations
#' table. Default is "all".
#' @param dateRange Specify a date range. Default is "1900-01-01" to current
#' date.
#' @param visitID Specify a specific visit (pk_visitid) from the visits table.
#' Default is NULL.
#' @param taxonID Specify a specific taxon (pk_taxonid) from the taxa table. 
#' Default is "all".
#' @param excludeAnnoVerified Specify when to exclude based on verification 
#' status. Default is "NA".
#' @param confValue Specify a confidence level to filter modeloutputs 
#' (model_value).  Default is 0.
#' @param lessThan TRUE or FALSE. Specify if outputs should be less than or 
#' greater than confValue. Default is FALSE.
#' @param selectedUser The fk_personid who verified modeloutputs
#' (pk_personid from the people table). Default is NA.
#' @param model Which model to display (pk_modelid from the models table). 
#' Default is "all".
#' @param mediaType Type of media. Options are "photo" (default) or "audio". 
#' @param newOnly TRUE or FALSE. Default is FALSE.
#' @param limit Maximum number of records to be returned
#' @param offset The number of records to offset the query by
#' @usage qryModelOutputsMedia(
#'   con, 
#'   disconnect = FALSE, 
#'   locationID = 'all', 
#'   dateRange = list(c('1900-01-01', as.character(Sys.Date()))), 
#'   visitID = NULL, 
#'   taxonID = 'all', 
#'   excludeAnnoVerified = 'NA',
#'   selectedUser = NA, 
#'   model = "all", 
#'   confValue = 0, 
#'   lessThan = FALSE, 
#'   newOnly = FALSE, 
#'   mediaType = "photo",
#'   limit = Inf,
#'   offset = 0
#' )
#' @importFrom DBI dbIsValid dbDisconnect dbSendQuery dbBind dbFetch 
#' dbClearResult
#' @family query
#' @concept media
#' @concept models
#' @concept modeloutputs
#' @return A dataframe with key columns from all photos that meet the given criteria
#' @export
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # look at the models table
#' DBI::dbReadTable(conx, name = "models")
#' 
#' # look at the modeloutputs table
#' DBI::dbReadTable(conx, name = "modeloutputs")
#' 
#' # look at the modelverifications table
#' DBI::dbReadTable(conx, name = "modelverifications")
#' 
#' # run the query to get media that have modeloutputs with "btnw"
#' qryModelOutputsMedia(conx, taxonID = 'btnw', mediaType = "audio")
#' 
#' # get media files that were tagged by MegaDetector (model 5)
#' qryModelOutputsMedia(conx,  mediaType = "photo", model = 5)
#' 
#' # get media photo files that have a high confidence score
#' qryModelOutputsMedia(conx,  mediaType = "photo", confValue = 0.9, disconnect = TRUE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryModelOutputsMedia <- function(con, disconnect = FALSE, locationID = 'all', dateRange = list(c('1900-01-01', as.character(Sys.Date()))), visitID = NULL, taxonID = 'all', excludeAnnoVerified = 'NA', selectedUser = NA, model = 'all', confValue = 0, lessThan = FALSE, newOnly = FALSE, mediaType = "photo", limit = Inf, offset = 0) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")

  params <- list(mediaType, as.character(dateRange[[1]][1]), as.character(dateRange[[1]][2]))
  
  stmnt <- paste0(
    "SELECT DISTINCT media.pk_mediaid, media.filename, media.filepath, media.start_date, media.start_time FROM 
  (media INNER JOIN visits ON media.fk_visitid = visits.pk_visitid) 
  INNER JOIN modeloutputs ON media.pk_mediaid = modeloutputs.fk_mediaid 
  LEFT JOIN modelverifications ON modelverifications.fk_modeloutputid = modeloutputs.pk_modeloutputid
  WHERE media.media_type = $1")
  
  where_clauses <- c(
    paste0('start_date >= $2'),
    paste0('start_date <= $3')
  )
  
  param_counter <- 4
  
  if (locationID != 'all') {
    where_clauses <- c(where_clauses, paste0('fk_locationid = $', param_counter))
    params[[param_counter]] <- locationID
    param_counter <- param_counter + 1
  }
  
  if (!is.null(visitID)) {
    where_clauses <- c(where_clauses, paste0('fk_visitid = $', param_counter))
    params[[param_counter]] <- visitID
    param_counter <- param_counter + 1
  }
  
  if (taxonID != 'all') {
    where_clauses <- c(
      where_clauses,
      paste0('modeloutputs.fk_taxonid = $', param_counter)
    )
    params[[param_counter]] <- taxonID
    param_counter <- param_counter + 1
  }
  
  if (newOnly && !is.na(selectedUser)) {
    where_clauses <- c(
      where_clauses,
      paste0('modelverifications.fk_personid != $', param_counter)
    )
    params[[param_counter]] <- selectedUser
    param_counter <- param_counter + 1
  }
  
  if (model != 'all') {
    where_clauses <- c(
      where_clauses,
      paste0('fk_modelid = $', param_counter)
    )
    params[[param_counter]] <- model
    param_counter <- param_counter + 1
  }
  
  if (!is.na(confValue)) {
    where_clauses <- c(
      where_clauses,
      paste0('value_num ', ifelse(lessThan, paste0("<= $", param_counter), paste0(">= $", param_counter))))
    params[[param_counter]] <- confValue
    param_counter <- param_counter + 1
  }
  
  switch(
    excludeAnnoVerified,
    'NA' = {},
    'Me' = {
      where_clauses <- c(
        where_clauses,
        paste0('modeloutputs.pk_modeloutputid NOT IN (SELECT fk_modeloutputid FROM modelverifications WHERE modelverifications.fk_personid = $', param_counter, ')')
      )
      params[[param_counter]] <- selectedUser
      param_counter <- param_counter + 1
    },
    'Anyone' = {
      where_clauses <- c(
        where_clauses,
        'modelverifications.is_valid IS NULL'
      )
    }
  )
  
  if (length(where_clauses) != 0) {
    stmnt <- paste(stmnt, 'AND', paste(where_clauses, collapse = ' AND '))
  }
  
  # Add ordering
  stmnt <- paste(stmnt, ' ORDER BY start_date, start_time')
  
  # Add LIMIT clause
  if (limit != Inf) {
    stmnt <- paste(stmnt, 'LIMIT', limit)
  }
  
  # Add OFFSET clause
  if (offset != 0) {
    stmnt <- paste(stmnt, 'OFFSET', offset)
  }
  
  stmnt <- paste0(stmnt, ";")
  
  result <- DBI::dbSendQuery(con, stmnt)
  DBI::dbBind(result, params)
  media <- DBI::dbFetch(result)
  DBI::dbClearResult(result)
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  return(media)
}

# qryModelVerificationConsensus ----------------
#' @name qryModelVerificationConsensus
#' @title Retrieves model outputs of a particular model with a consensus in
#' verifications -- all people who verified agree on 1 or 0 (even if there is 
#' just one person making the verifications)
#' @description Retrieves model outputs of a particular model with a consensus 
#' in verifications (for >= 1 person).  If there are conflicts (e.g., one
#' person confirms valid and another confirms invalid, these records are 
#' excluded)
#' @param con An open database connection
#' @param modelid Model ID of a model (integer, pk_modelid from the models table)
#' @param disconnect TRUE or FALSE. Should the database connection be closed on 
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult dbDisconnect
#' @usage qryModelVerificationConsensus(con, modelid, disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept models
#' @concept modeloutputs
#' @concept modelverifications
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # look at the models table; note that MegaDetector is model 5
#' DBI::dbReadTable(conx, name = "models")
#' 
#' # look at the modeloutputs table
#' (mo <- DBI::dbReadTable(conx, name = "modeloutputs"))
#' 
#' # look at the modelverifications table; note sgamgee made verifications
#' # on multiple model outputs, some of them are MegaDetector outputs
#' (mvs <- DBI::dbReadTable(conx, name = "modelverifications"))
#' 
#' # add Gandalf's modelverifications as well - but set these to random values!!
#' # (Gandalf should verify more carefully)
#' DBI::dbAppendTable(
#'   conn = conx,
#'   name = "modelverifications",
#'   value = data.frame(
#'    pk_modelverificationid = NA,
#'    fk_modeloutputid = mo$pk_modeloutputid,
#'    fk_personid = "gandalf",
#'    is_valid = stats::rbinom(n = nrow(mo), size = 1, prob = 0.5),
#'    timestamp =format(Sys.time())
#'   )
#'  )
#' 
#' # run the query to get MegaDetector modeloutputs that have been validated by 
#' # at least two people; note these contain only MegaDetector outputs (model 5)
#' result <- qryModelVerificationConsensus(conx, modelid = 5, disconnect = TRUE)
#' 
#' # graph the consensus
#' ggplot2::ggplot(result, aes(x = as.factor(is_valid))) +
#'   geom_bar() + 
#'   labs(
#'     x = "Is Valid",
#'     y = "Count")
#' 
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryModelVerificationConsensus <- function(con, modelid, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  rs <- DBI::dbSendQuery(
    con,
    "SELECT modeloutputs.*, 
    AVG(is_valid) AS is_valid 
    FROM modeloutputs 
    INNER JOIN modelverifications 
    ON modeloutputs.pk_modeloutputid = modelverifications.fk_modeloutputid 
    WHERE modeloutputs.fk_modelid = $1
    GROUP BY modeloutputs.pk_modeloutputid 
    HAVING AVG(modelverifications.is_valid) = 0 OR AVG(modelverifications.is_valid) = 1;"
  )
  
 
  
  
  DBI::dbBind(rs, list(modelid))
  vers <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  return(vers)
}
# qryCheckMediaFilePaths ---------------
#' @name qryCheckMediaFilePaths
#' @title Check if any row in the media table does not have a file path
#' @description Checks if there exists any row in the media table that meets 
#' certain conditions (media type or media ID) and has no file path
#' @param con An existing database connection
#' @param mediaType Media type to filter for, if any. One of "photo", "audio", or 
#' "video". If mediaIDs are specified, this argument will be ignored.
#' @param mediaIDs Integer vector of media IDs from the media table (pk_mediaid), 
#' if specific ones should be selected.
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @details
#' File paths are not required in the media table! This function is used 
#' internally to determine if file paths are provided, and if not, the Shiny app
#' will look for the media root file path in the "settings" directory. If not 
#' present the app will assume the media are present in the "photos" or 
#' "recordings" directory of any AMMonitor project.
#' 
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult dbDisconnect
#' @usage qryCheckMediaFilePaths(con, mediaType = NA, mediaIDs = NA, 
#' disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept media
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # look at the models table; note that filepaths are not present
#' DBI::dbReadTable(conx, name = "media")
#' 
#' # check if filepaths are provided
#' qryCheckMediaFilePaths(conx, mediaType = "photo", disconnect = TRUE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryCheckMediaFilePaths <- function(con, mediaType = NA, mediaIDs = NA, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  params <- list()
  stmnt <- "SELECT (1) FROM media WHERE "
  
  if (any(!is.na(mediaIDs))) {
    media_str <- paste(mediaIDs[!is.na(mediaIDs)], collapse = ", ")
    stmnt <- paste0(stmnt, "pk_mediaid IN ($1) AND ")
    params <- list(media_str)
  } else if (!is.na(mediaType)) {
    stmnt <- paste0(stmnt, "media_type = $1 AND ")
    params <- list(mediaType)
  }
  
  stmnt <- paste0(stmnt, "filepath IS NULL;")
  
  rs <- DBI::dbSendQuery(
    con,
    stmnt
  )
  if (any(!is.na(mediaIDs)) | !is.na(mediaType)) {
    DBI::dbBind(rs, params)
  }
  nfp <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  return(nfp)
}
# qryCheckTags --------------
#' @name qryCheckTags
#' @title Query annotations or mediatags for specific media files
#' @description Check if annotations or mediatags exist for a particular media file
#' @param con An existing database connection
#' @param mediaid One or more mediaids as an integer vector, as saved in the database.
#' @param personid A personid as saved in the database, if specified, then
#' filters for or out annotations by the specified user. Default NA
#' @param excludeperson TRUE or FALSE. Whether to include or exclude annotations
#' by the specified user. Default is FALSE
#' @param mediatags TRUE or FALSE. Should the function check for mediatags 
#' instead of annotations? Default is FALSE
#' @param exists TRUE or FALSE. Whether to check for existing records or to 
#' retrieve those records. Default is FALSE
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult dbDisconnect
#' @usage qryCheckTags(
#'   con, 
#'   mediaid, 
#'   personid = NA, 
#'   excludeperson = FALSE, 
#'   mediatags = FALSE, 
#'   exists = FALSE, 
#'   disconnect = FALSE
#' )
#' @return dataframe
#' @export
#' @family query
#' @concept annotations
#' @concept mediatags
#' @concept media
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # look at the media table; note that filepaths are not present
#' DBI::dbReadTable(conx, name = "media")
#' 
#' # return all library tags for media 1:5
#' qryCheckTags(conx, mediaid = 1:5, disconnect = FALSE)
#' 
#' # look at the mediatags table; there are none
#' DBI::dbReadTable(conx, name = "mediatags")
#' 
#' # look at the media tag items available
#' qryItems(
#'   con = conx, 
#'   table = "medialists", 
#'   listname = "media_checkboxes"
#'  )
#' 
#' # add a few media-level tags for media 1:5
#' DBI::dbAppendTable(
#'  conn = conx,
#'  name = "mediatags",
#'  value = data.frame(
#'   pk_mediatagid = NA,
#'   fk_mediaid = 1:5,
#'   fk_medialistitemid = 1,  # highlight
#'   fk_personid = 'gandalf'
#'   )
#'  )
#' 
#' # check all media tags for media 1:5
#' qryCheckTags(conx, mediaid = 1:5, mediatags = TRUE, disconnect = TRUE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryCheckTags <- function(con, mediaid, personid = NA, excludeperson = FALSE, mediatags = FALSE, exists = FALSE, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  media_str <- paste(mediaid, collapse = ", ")
  params <- list(mediaid)
  
  if (exists == TRUE) {
    selection <- "(1)"
  } else {
    selection <- "*"
  }
  
  if (mediatags == TRUE) {
    stmnt <- paste0("SELECT ", selection, " FROM mediatags WHERE fk_mediaid IN ($1)")
  } else {
    stmnt <- paste0("SELECT ", selection, " FROM annotations WHERE fk_mediaid IN ($1)")
  }
  
  if (length(personid) == 1 && all(!is.na(personid))) {
    if (excludeperson == TRUE) {
      stmnt <- paste0(stmnt, " AND fk_personid != $2")
    } else {
      stmnt <- paste0(stmnt, " AND fk_personid = $2")
    }
    params <- append(params, personid)
  }
  
  stmnt <- paste0(stmnt, ";")
  
  rs <- DBI::dbSendQuery(con, stmnt)
  DBI::dbBind(rs, params)
  annos <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  return(annos)
}

# qryMediaDateRange ----------------
#' @name qryMediaDateRange
#' @title Queries the date range of all media files or a type of media files
#' @description Queries the date range of all media files or a type of media files
#' @param con An existing database connection
#' @param mediaType Type of media to filter for, if any. One of "photo", "audio"
#' or "video". Default is NA (all media).
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on 
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult dbDisconnect
#' @usage qryMediaDateRange(con, mediaType = NA, disconnect = FALSE)
#' @return dataframe with the first column being the start date and the second column being the end date
#' @export
#' @family query
#' @concept media
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # look at the media table
#' DBI::dbReadTable(conx, name = "media")
#' 
#' # return the date range of photos
#' qryMediaDateRange(conx, mediaType = "photo", disconnect = FALSE)
#' 
#' # return the date range of photos
#' qryMediaDateRange(conx, mediaType = "audio", disconnect = TRUE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryMediaDateRange <- function(con, mediaType = NA, disconnect = FALSE) {
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  stmnt <- "SELECT MIN(start_date) AS startdate, MAX(start_date) AS enddate FROM media"
  
  if (length(mediaType) == 1 && all(!is.na(mediaType))) {
    stmnt <- paste0(stmnt, " WHERE media_type = $1")
    params <- list(mediaType)
  }
  
  stmnt <- paste0(stmnt, ";")
  
  rs <- DBI::dbSendQuery(con, stmnt)
  if (all(!is.na(mediaType))) {
    DBI::dbBind(rs, params)
  }
  dates <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  return(dates)
}

# qryVerifications ------------------------
#' @name qryVerifications
#' @title Queries the annotations, mediatags, or model outputs tables and 
#' respective verifications
#' @description Queries the annotations, mediatags, or model outputs tables and 
#' their respective verifications tables. Verifications can be filtered by 
#' database user
#' @param con An existing database connection
#' @param table Type of verifications to query, one of "annotations", "mediatags", 
#' or "modeloutputs".
#' @param personid personid as saved in the database. If not specified, then 
#' verifications by all users will be retrieved.
#' @param disconnect TRUE or FALSE. Should the database connection be closed on 
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult dbDisconnect
#' @usage qryVerifications(con, table, personid = NA, disconnect = FALSE)
#' @return dataframe with the first column being the start date and the second 
#' column being the end date
#' @export
#' @family query
#' @concept annotations
#' @concept mediatags
#' @concept modeloutputs
#' @concept annotationverifications
#' @concept mediatagverifications
#' @concept modelverifications
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
#' # return annotation verifications
#' qryVerifications(conx, table = 'annotations', disconnect = FALSE)
#' 
#' # return modeloutputs verifications
#' rslt <- qryVerifications(conx, table = 'modeloutputs', disconnect = FALSE)
#' 
#' # graph the result
#' ggplot2::ggplot(
#'  data = rslt[,1:11],
#'  mapping = aes(
#'    fill = as.factor(is_valid), 
#'    x = fk_taxonid)) +
#'  geom_bar() +
#'  labs(
#'    x = "Taxa",
#'    y = "Count",
#'    fill = "Valid") 
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryVerifications <- function(con, table, personid = NA, disconnect = FALSE) {
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  vertablename <- switch(
    table,
    annotations = "annotationverifications",
    mediatags = "mediatagverifications",
    modeloutputs = "modelverifications",
    stop("table must be one of: annotations, mediatags, modeloutputs.")
  )
  
  stmnt <- switch(
    table,
    annotations = "SELECT * FROM annotationverifications 
    LEFT JOIN annotations 
    ON annotationverifications.fk_annotationid = annotations.pk_annotationid",
    mediatags = "SELECT * FROM mediatagverifications 
    LEFT JOIN mediatags 
    ON mediatagverifications.fk_mediatagid = mediatags.pk_mediatagid",
    modeloutputs = "SELECT * FROM modelverifications 
    LEFT JOIN modeloutputs 
    ON modelverifications.fk_modeloutputid = modeloutputs.pk_modeloutputid"
  )
  
  if (length(personid) == 1 && all(!is.na(personid))) {
    stmnt <- paste0(stmnt, " WHERE ", vertablename, ".fk_personid = $1")
    params <- list(personid)
  }
  
  stmnt <- paste0(stmnt, ";")
  
  rs <- DBI::dbSendQuery(con, stmnt)
  if (all(!is.na(personid))) {
    DBI::dbBind(rs, params)
  }
  vers <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  return(vers)
}

# qryModelOutputs -----------------------
#' @name qryModelOutputs
#' @title Queries the model outputs table, with filters by score, model, and media 
#' file
#' @description Queries the model outputs table, with filters by score, model, and 
#' media file
#' @param con An existing database connection
#' @param mediaIDs Integer vector - One or more media IDs (as saved in the database) 
#' to get model outputs for
#' @param modelIDs Integer vector - One or more model IDs (as saved in the database) 
#' to get model outputs for
#' @param scoreThreshold Numeric - Threshold of scores to filter for
#' @param lessThan TRUE or FALSE. Whether or not retrieved outputs should have a 
#' score greater than (default) or less than the threshold. Only applicable if 
#' scoreThreshold is specified. Default is FALSE, which means that outputs 
#' retrieved will have scores greater than the threshold.
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbSendQuery dbBind dbFetch dbClearResult dbDisconnect
#' @usage qryModelOutputs(con, mediaIDs = NA, modelIDs = NA, scoreThreshold = NA,
#' lessThan = FALSE, disconnect = FALSE)
#' @return dataframe with the first column being the start date and the second 
#' column being the end date
#' @export
#' @family query
#' @concept modeloutputs
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
#'   
#' # return modeloutputs from media 10
#' qryModelOutputs(
#'   con = conx, 
#'   mediaIDs = 10:12, 
#'   modelIDs = NA, 
#'   scoreThreshold = NA, 
#'   lessThan = TRUE, 
#'   disconnect = FALSE)
#'   
#' # return modeloutputs from all media, model 5
#' qryModelOutputs(
#'   con = conx, 
#'   mediaIDs = NA, 
#'   modelIDs = 5, 
#'   scoreThreshold = NA, 
#'   lessThan = TRUE, 
#'   disconnect = FALSE)
#'   
#' # return modeloutputs from media 10-12
#' qryModelOutputs(
#'   con = conx, 
#'   mediaIDs = 10:12, 
#'   modelIDs = 5:6, 
#'   scoreThreshold = NA, 
#'   lessThan = TRUE, 
#'   disconnect = FALSE)
#'   
#' # return modeloutputs from media 10-12 with high score threshold
#' qryModelOutputs(
#'   con = conx, 
#'   mediaIDs = 10:12, 
#'   modelIDs = 5:6, 
#'   scoreThreshold = .95, 
#'   lessThan = TRUE, 
#'   disconnect = FALSE)
#' 
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryModelOutputs <- function(con, mediaIDs = NA, modelIDs = NA, scoreThreshold = NA, lessThan = FALSE, disconnect = FALSE) {
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(DBI::dbDisconnect(con))
  }

  stmnt <- "SELECT * FROM modeloutputs"
  params <- list()
  param_counter <- 1
  
  if (any(all(!is.na(mediaIDs)) | all(!is.na(modelIDs)) | all(!is.na(scoreThreshold)))) {
    stmnt <- paste0(stmnt, " WHERE")
  }
  if (all(!is.na(mediaIDs))) {
    # media_str <- paste(mediaIDs[!is.na(mediaIDs)], collapse = ", ")
    # stmnt <- paste0(stmnt, " fk_mediaid IN ($", param_counter, ")")
    stmnt <- paste0(stmnt, " fk_mediaid IN (", paste0("$", param_counter:(param_counter + length(mediaIDs)-1), collapse = ", "), ")")
    params <- append(params, mediaIDs)
    param_counter <- param_counter + length(mediaIDs) 
  }
  
  if (all(!is.na(modelIDs))) {
    
    if (param_counter > 1) {
      stmnt <- paste0(stmnt, " AND")
    }
    # model_str <- paste(modelIDs[!is.na(modelIDs)], collapse = ", ")
    # stmnt <- paste0(stmnt, " fk_modelid IN ($", param_counter, ")")
    stmnt <- paste0(stmnt, " fk_modelid IN (", paste0("$", param_counter:(param_counter + length(modelIDs)-1), collapse = ", "), ")")
    params <- append(params, modelIDs)
    param_counter <- param_counter + length(modelIDs)
  }
  
  if (length(scoreThreshold) == 1 && !is.na(scoreThreshold)) {
    if (param_counter > 1) {
      stmnt <- paste0(stmnt, " AND")
    }
    stmnt <- paste0(
      stmnt, 
      " value_num ", ifelse(lessThan, "<= ", ">= "), "$", param_counter
    )
    params <- append(params, scoreThreshold)
    param_counter <- param_counter + 1
  }
  
  stmnt <- paste0(stmnt, ";")
  
  rs <- DBI::dbSendQuery(con, stmnt)
  if (length(params) > 0) {
    DBI::dbBind(rs, params)
  }
  modeloutputs <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  return(modeloutputs)
}

# qryPeopleActivity --------------
#' @name qryPeopleActivity
#' @aliases qryPeopleActivity
#' @title Returns information about activities by person as 
#' logged in the database
#' @description  Returns information about activities as by person
#' logged in the database
#' @param con An existing database connection
#' @param m_dates  A vector of length 2 giving start and end date ranges 
#' for report, in 
#' the format YYYY-MM-DD. Default is c(NA, NA).
#' @param disconnect  TRUE or FALSE.  Should the database connection be closed 
#' on exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery
#' @usage qryPeopleActivity(con, m_dates = c(NA, NA), disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept people
#' @concept annotations
#' @concept annotationverifications
#' @concept modelverifications
#' @concept visits
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#'
#' # Retrieve a full report for a given person
#' results <- qryPeopleActivity(
#'  con = conx, 
#'  m_dates = c(NA, NA),
#'  disconnect = TRUE)
#'  
#'  # look at the results
#'  lapply(results, FUN = head, n = 10)
#'  
#' ggplot2::ggplot(
#'  data = results$visits,
#'  mapping = aes(
#'    fill = as.factor(visit_type), 
#'    x = fk_personid)) +
#'  geom_bar() +
#'  labs(
#'    x = "Person",
#'    y = "Count",
#'    fill = "Visit Type") +
#'  scale_y_continuous(breaks=c(1:9))
#'  
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryPeopleActivity <- function(con, m_dates = c(NA, NA), disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # create a list to hold results
  results <- list()
  
  startDate <- ifelse(test = is.na(m_dates[1]), yes = "1900-01-01", no = m_dates[1])
  endDate <- ifelse(test = is.na(m_dates[2]), yes = format(Sys.Date(), "%Y-%m-%d"), no = m_dates[2])

  
  # run query 1; visits -----------
  stmnt <- paste0(
   "SELECT visits.fk_personid, visits.fk_locationid, visits.visit_type, Count(*) 
    AS count
    FROM visits
    WHERE (visit_date > '", 
    startDate, 
    "' AND visit_date < '", 
    endDate, 
    "') 
    GROUP BY visits.fk_personid, visits.fk_locationid, visits.visit_type
    ORDER BY visits.fk_personid, visits.fk_locationid;")
   
  
  visits <- DBI::dbGetQuery(conn = con, statement = stmnt)
  
  # run query 2; annotations ---------
  stmnt <- paste0(
    "SELECT annotations.fk_personid, media.media_type, 
    Count(annotations.pk_annotationid) AS count
    FROM media INNER JOIN annotations 
    ON media.pk_mediaid = annotations.fk_mediaid
    WHERE (annotations.timestamp > '", 
    startDate, 
    "' AND annotations.timestamp < '", 
    endDate, 
    "') 
    GROUP BY annotations.fk_personid, media.media_type
    ORDER BY annotations.fk_personid, media.media_type;"
  )
  annotations <- DBI::dbGetQuery(conn = con, statement = stmnt)
  
  # run query 3: annotation verifications
  stmnt <- paste0(
  "SELECT annotationverifications.fk_personid, media.media_type,
  Count(annotationverifications.pk_annoverificationid) AS count
  FROM media 
  INNER JOIN (annotations INNER JOIN annotationverifications 
  ON annotations.pk_annotationid = annotationverifications.fk_annotationid)
  ON media.pk_mediaid = annotations.fk_mediaid 
  WHERE (annotationverifications.timestamp > '", 
  startDate, 
  "' AND annotationverifications.timestamp < '", 
  endDate, 
  "')
  GROUP BY annotationverifications.fk_personid, media.media_type 
  ORDER BY annotationverifications.fk_personid, media.media_type;")
  
  annotation_verifications <- DBI::dbGetQuery(conn = con, statement = stmnt)

  # run query 4: model verifications --------
  stmnt <- paste0(
    "SELECT modelverifications.fk_personid, models.model_name, Count(modelverifications.pk_modelverificationid) AS count
FROM models INNER JOIN (modeloutputs INNER JOIN modelverifications ON modeloutputs.pk_modeloutputid = modelverifications.fk_modeloutputid) ON models.pk_modelid = modeloutputs.fk_modelid
  WHERE (modelverifications.timestamp > '", 
    startDate, 
    "' AND modelverifications.timestamp < '", 
    endDate, 
    "')
 GROUP BY modelverifications.fk_personid, models.model_name
 ORDER BY modelverifications.fk_personid, models.model_name;")
  
  model_verifications <- DBI::dbGetQuery(conn = con, statement = stmnt)
  
  # combine results -------
  results$visits <- visits
  results$annotations <- annotations
  results$annotation_verifications <- annotation_verifications
  results$model_verifications <- model_verifications 
  
  # return the results
  return(results)
  
} # end of function
NULL

# qryLocationCounts --------------
#' @name qryLocationCounts
#' @aliases qryLocationCounts
#' @title Returns counts of locations by type as 
#' logged in the database
#' @description  Returns counts of locations by type as 
#' logged in the database
#' @param con An existing database connection
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery
#' @usage qryLocationCounts(con, disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept locations
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#'
#' # retrieve a count of locations by type
#' qryLocationCounts(
#'  con = conx,
#'  disconnect = TRUE)
#'  
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryLocationCounts <- function(con, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # set the statement
  stmnt <- paste0(
    "SELECT locations.location_type, locations.location_status, 
    Count(locations.pk_locationid) AS count
    FROM locations
    WHERE locations.pk_locationid <> 'unknownLocation' 
    GROUP BY locations.location_type, locations.location_status
    ORDER BY locations.location_type, locations.location_status;")
  
  # run the query
  result <- DBI::dbGetQuery(conn = con, statement = stmnt)
  
  # return the result
  return(result)
}
NULL

# qryEconfiguration --------------
#' @name qryEconfiguration
#' @aliases qryEconfigurations
#' @title Returns settings and values for a given equipment configuration
#' @description  Returns settings and values for a given equipment configuration
#' @param con An existing database connection
#' @param econfigname The primary key in the econfignames table
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery
#' @usage qryEconfiguration(con, econfigname, disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept econfignames
#' @concept econfigvalues
#' @concept esettingnames
#' @concept esettingoptions
#' @concept equipment
#' @concept equipmodels
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # look at the econfigs table
#' DBI::dbReadTable(conx, name = "econfignames")
#'
#' # Retrieve a list of settings for a given equipment configuration
#' qryEconfiguration(
#'  con = conx,
#'  econfigname = "default recorder settings",
#'  disconnect = TRUE)
#'  
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryEconfiguration <- function(con, econfigname, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # set the statement
  stmnt <- paste0(
    "SELECT econfigvalues.fk_econfignameid,
    esettingnames.setting_name, econfigvalues.value_num,
    esettingnames.pk_esettingnameid
    FROM esettingnames INNER JOIN econfigvalues 
    ON esettingnames.pk_esettingnameid = econfigvalues.fk_esettingnameid
    WHERE (((econfigvalues.fk_econfignameid)='", econfigname, "'))
    ORDER BY econfigvalues.fk_esettingnameid;")
  
  # run the query
  result1 <- DBI::dbGetQuery(conn = con, statement = stmnt)
  
  # merge with setting names
  result2 <- DBI::dbGetQuery(
    conn = con,
    statement = paste0(
      "SELECT econfigvalues.fk_econfignameid, econfigvalues.fk_esettingnameid, 
      econfigvalues.fk_esettingoptionid, esettingoptions.option_name
      FROM esettingoptions 
      INNER JOIN econfigvalues 
      ON esettingoptions.pk_esettingoptionid = econfigvalues.fk_esettingoptionid
      WHERE (((econfigvalues.fk_econfignameid)='", econfigname,"'));"
    )
  )
  
  result <- merge(result1, result2, 
                  by.x = "pk_esettingnameid",
                  by.y = "fk_esettingnameid",
                  all = TRUE)
  result <- result[, c("fk_econfignameid.x", "setting_name", "option_name", "value_num")]
  names(result)[names(result) == "fk_econfignameid.x"] <- "fk_econfignameid"
    
  # return the result
  return(result)
}

# qryAnnotations --------------
#' @name qryAnnotations
#' @aliases qryAnnotations
#' @title Returns a count of annotations by taxa in the database. 
#' @description Returns a count of annotations by taxa in the database and the 
#' number of records verified
#' @param con An existing database connection
#' @param disconnect TRUE or FALSE. Should the connection to the database be 
#' closed on exit? Default is FALSE.
#' @usage qryAnnotations(con,  disconnect = FALSE)
#' @importFrom DBI dbIsValid dbGetQuery dbDisconnect
#' @return  Dataframe consisting of the specified rows and columns
#' @family qry
#' @concept annotation
#' @concept annotationverifications
#' @export
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' rslt <- qryAnnotations(
#'   con = conx,
#'   disconnect = TRUE)
#' 
#' # show the result
#' rslt
#' 
#' # make a table with the package gt
#' gt::gt(rslt) |> 
#'   gt::cols_label(
#'     fk_taxonid = "Taxa",
#'     total_annotations = "Annotations",
#'     total_verified = "Verifications") 
#'  
#' # unlink the demo
#' unlink(demo_fp)
#' 
#' 
#' 
#' 
#' # unlink the demo
#' unlink(demo_fp)
#'}
#'
NULL
qryAnnotations <- function(con, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # set the statement
  stmnt <- "SELECT annotations.fk_taxonid, Count(annotations.pk_annotationid) AS total_annotations, Count(annotationverifications.is_valid) AS total_verified
FROM annotations LEFT JOIN annotationverifications ON annotations.pk_annotationid = annotationverifications.fk_annotationid
GROUP BY annotations.fk_taxonid
ORDER BY annotations.fk_taxonid;"
  
  # run the query
  result <- DBI::dbGetQuery(conn = con, statement = stmnt)
  
  # return the result
  return(result)
}

# qryMconfiguration --------------
#' @name qryMconfiguration
#' @aliases qryMconfiguration
#' @title Returns settings and values for a given model configuration
#' @description  Returns settings and values for a given model configuration
#' @param con An existing database connection
#' @param mconfigid The pk_mconfigid from the mconfignames table
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery
#' @usage qryMconfiguration(con, mconfigid, disconnect = FALSE)
#' @return dataframe
#' @details
#' This query retrieves information about model configurations and presents it
#' in an easy-to-read format. By default, this information will be output in a
#' data frame.
#' 
#' @export
#' @family query
#' @concept mconfigvalues
#' @concept mconfignames
#' @concept msettingnames
#' @concept msettingoptions
#' @concept models
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # look at the mconfignames table
#' # note that the BirdNET default settings are pk_mconfigid 2
#' DBI::dbReadTable(conx, name = "mconfignames")
#'
#' # Retrieve the settings and values for the default BirdNET configuration
#' qryMconfiguration(
#'  con = conx,
#'  mconfigid = 2,
#'  disconnect = TRUE)
#'  
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryMconfiguration <- function(con, mconfigid, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # we need to transform mconfigname into pk_mconfignameid
  
  # set the statement
  stmnt <- paste0(
    "SELECT mconfigvalues.fk_mconfignameid,
    msettingnames.setting_name, mconfigvalues.value_num,
    msettingnames.pk_msettingnameid
    FROM msettingnames INNER JOIN mconfigvalues 
    ON msettingnames.pk_msettingnameid = mconfigvalues.fk_msettingnameid
    WHERE (((mconfigvalues.fk_mconfignameid)='", mconfigid, "'))
    ORDER BY mconfigvalues.fk_msettingnameid;")
  
  # run the query
  result1 <- DBI::dbGetQuery(conn = con, statement = stmnt)
  
  # merge with setting names
  result2 <- DBI::dbGetQuery(
    conn = con,
    statement = paste0(
      "SELECT mconfigvalues.fk_mconfignameid, mconfigvalues.fk_msettingnameid, 
      mconfigvalues.fk_msettingoptionid, msettingoptions.option_name
      FROM msettingoptions 
      INNER JOIN mconfigvalues 
      ON msettingoptions.pk_msettingoptionid = mconfigvalues.fk_msettingoptionid
      WHERE (((mconfigvalues.fk_mconfignameid)='", mconfigid,"'));"
    )
  )
  
  result <- merge(result1, result2, 
                  by.x = "pk_msettingnameid",
                  by.y = "fk_msettingnameid",
                  all = TRUE)
  result <- result[, c("fk_mconfignameid.x", "setting_name", "option_name", "value_num")]
  names(result)[names(result) == "fk_mconfignameid.x"] <- "fk_mconfignameid"
  
  # return the result
  return(result)
  
}

# qryUnannotated --------------
#' @name qryUnannotated
#' @title Returns files without annotations
#' @description Returns files without annotations
#' @param con An existing database connection
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery
#' @usage qryUnannotated(con, disconnect = FALSE)
#' @return dataframe
#' @export
#' @family query
#' @concept annotations
#' @concept media
#' @concept visits
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # look at the "annotations" table.
#' # note that all of the media in the demo have annotations
#' DBI::dbReadTable(conn = conx, name = "annotations")
#' 
#' # delete some of the example annotations
#' DBI::dbExecute(
#'  conn = conx,
#'  statement = "DELETE FROM annotations
#'  WHERE fk_mediaid = 1;")
#'  
#' # run the query to see a list of files lacking annotations
#' qryUnannotated(con = conx, disconnect = TRUE)
#'  
#' # unlink the demo
#' unlink(demo_fp)
#' }
NULL

qryUnannotated <- function(con, disconnect = FALSE) {

if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")

if (disconnect == TRUE) {
  on.exit(expr = {
    DBI::dbDisconnect(con)
  })
}

# set the statement
stmnt <- " SELECT visits.pk_visitid, visits.fk_locationid, media.pk_mediaid, 
  media.filename, annotations.pk_annotationid
  FROM visits 
  INNER JOIN (media LEFT JOIN annotations ON media.pk_mediaid = annotations.fk_mediaid) 
  ON visits.pk_visitid = media.fk_visitid
  WHERE (((annotations.pk_annotationid) Is Null));"

# run the query
result <- DBI::dbGetQuery(conn = con, statement = stmnt)

# return the result
return(result)
}

# qryEffort --------------
#' @name qryEffort
#' @title Returns dates used to estimate survey effort per visit
#' @description Returns lower-bound (based on media dates) and upper-bound
#'  (based on visit dates and equipment deployment) date ranges used to 
#'  calculate survey effort per visit. Visits with 'NA' in the lower-bound
#'   columns were not associated with collected media. Visits with 'NA' 
#'   in the upper-bound start date 'activeStartUB' have no prior visit in 
#'   the database.
#' @param con An existing database connection
#' @param disconnect  TRUE or FALSE. Should the database connection be closed on
#' exit? Default is FALSE.
#' @details
#' This function provides estimates of survey effort with upper and lower bounds.
#' Lower bound estimates for each visit were calculated from the minimum and 
#' maximum media dates for that visit. The upper bound estimates for each visit 
#' were calculated as the difference between a visit's date and the date of the 
#' previous visit at that location.
#' 
#' @importFrom DBI dbIsValid dbDisconnect dbGetQuery
#' @usage qryEffort(con, disconnect = FALSE)
#' @return List containing two dataframes
#' @export
#' @family query
#' @concept visits
#' @examples
#' \dontrun{
#' # create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # retrieve estimates of survey effort
#' effort <- qryEffort(con = conx, disconnect = TRUE)
#' 
#' # look at the dates estimating survey effort per visit
#' effort
#'  
#' # unlink the demo
#' unlink(demo_fp)
#' }
#' 
NULL

qryEffort <- function(con, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # set the statement for lowerbound
  stmnt1 <- "SELECT visits.pk_visitid, fk_locationid, Min(media.start_date) 
    AS activeStartLB, Max(media.start_date) AS activeEndLB
    FROM media INNER JOIN visits ON media.fk_visitid = visits.pk_visitid
    WHERE (((visits.visit_type)='check' Or (visits.visit_type)='pull'))
    GROUP BY visits.pk_visitid"
  
  # run the query
  result1 <- DBI::dbGetQuery(conn = con, statement = stmnt1)
  
  # set the statement for upperbound
  stmnt2 <- "SELECT t.pk_visitid, t.fk_locationid, MAX(prior.visit_date) 
    AS activeStartUB, t.visit_date AS activeEndUB
    FROM visits AS t
    LEFT JOIN visits AS prior 
    ON (t.fk_locationid=prior.fk_locationid AND prior.visit_date < t.visit_date)
    GROUP BY t.pk_visitid, t.visit_date;"
  
  # run the query
  result2 <- DBI::dbGetQuery(conn = con, statement = stmnt2)
  
  # merge the dataframes
  result <- merge(result2,
                  result1,
                  by = 'pk_visitid',
                  all = TRUE)
  
  col_order <- c('pk_visitid', 'fk_locationid.x',
                 'activeStartLB', 'activeEndLB',
                 'activeStartUB', 'activeEndUB')
  
  result <- result[, col_order]
  
  colnames(result) <- c('pk_visitid', 'fk_locationid',
                        'activeStartLB', 'activeEndLB',
                        'activeStartUB', 'activeEndUB')
  
  # return the result
  return(result)
  
}

# qryModelOutputsFull ----------------------------------------------------
#' @name qryModelOutputsFull
#' @title Returns a data frame of model outputs that meet a variety of criteria
#' @description This function allows for complex filtering of model outputs to
#' suit various needs. Filtering options include date range, location, taxa,
#' value_num, model, and media.
#' @param con An open connection to an AMMonitor database
#' @param mediaIDs A vector of pk_mediaids to get model outputs for. Default is 
#' NA.
#' @param modelIDs A vector of pk_modelids to get model outputs for. Default is
#' NA.
#' @param locationIDs A vector of pk_locationids to get model outputs for. 
#' Default is NA.
#' @param dateRange A range of dates to get model outputs from. Dates should be
#' formatted as character data and placed in a list. Default is 1900-01-01 to 
#' the current date.
#' @param taxonIDs A vector of pk_taxonids to get model outputs for. Default is
#' NA.
#' @param value_num A value to filter model outputs by, based on the value_num
#' in the model outputs table (such as a confidence score). Default is NA.
#' @param lessThan TRUE or FALSE. Should outputs be less than the specified
#' value_num? Default is FALSE.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage qryModelOutputsFull(
#'    con, 
#'    mediaIDs = NA,
#'    modelIDs = NA,
#'    locationIDs = NA,
#'    dateRange = list(c("1900-01-01", as.character(Sys.Date()))),
#'    taxonIDs = NA,
#'    value_num = NA,
#'    lessThan = FALSE,
#'    disconnect = FALSE
#'    )
#' @return A data.frame of all model outputs which match the provided criteria
#' @details
#' This function builds upon the functionality of qryModelOutputs to allow for
#' more complex filtering of model outputs. All arguments except for dateRange
#' can be set to NA to ignore them; in the case of dateRange, the default will
#' include all possible model outputs. All IDs should be entered as they are 
#' listed in the corresponding primary key (pk) in the database. The returned
#' data frame will include all key information from the modeloutputs table for
#' matching outputs, as well as the start_date and location for the output.
#' 
#' @family query
#' @concept modeloutputs
#' @importFrom DBI dbIsValid dbDisconnect dbSendQuery dbBind dbFetch 
#' dbClearResult
#' @export
#' @examples
#' \dontrun{
#' 
#' # Create a demo AMMonitor project (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # To work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # Only get modeloutputs for Ovenbird (oven)
#' qryModelOutputsFull(
#'    con = conx,
#'    taxonIDs = "oven",
#'    disconnect = FALSE)
#'
#' # Only get modeloutputs from BirdNET (modelid = 6)
#' qryModelOutputsFull(
#'    con = conx,
#'    modelIDs = 6,
#'    disconnect = FALSE)
#' }
#' 
NULL

qryModelOutputsFull <- function(con, 
                                mediaIDs = NA, 
                                modelIDs = NA, 
                                locationIDs = NA, 
                                dateRange = list(c("1900-01-01", as.character(Sys.Date()))),
                                taxonIDs = NA, 
                                value_num = NA, 
                                lessThan = FALSE, 
                                disconnect = FALSE) {
  
  # Check database connection and set disconnect
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Error Checks
  if(length(value_num) > 1) stop("Invalid value_num. Please enter a single value.")
  
  # Start the parameter list with dates
  params <- list(as.character(dateRange[[1]][1]), as.character(dateRange[[1]][2]))
  
  # Construct the baseline SQL query
  stmnt <- "SELECT modeloutputs.pk_modeloutputid, modeloutputs.fk_mediaid, modeloutputs.fk_modelid, modeloutputs.fk_mconfignameid, modeloutputs.fk_taxonid, modeloutputs.x_min, modeloutputs.x_max, modeloutputs.y_min, modeloutputs.y_max, modeloutputs.value_num, media.start_date, visits.fk_locationid 
  FROM modeloutputs 
  INNER JOIN media ON modeloutputs.fk_mediaid = media.pk_mediaid
  INNER JOIN visits ON media.fk_visitid = visits.pk_visitid
  WHERE start_date >= $1
  AND start_date <= $2"
  
  # Set up a list of parameters to add and a counter
  where_clauses <- c()
  param_counter <- 3
  
  # Go through all the arguments and add parameters where relevant
  
  # Media IDs
  if (any(!is.na(mediaIDs))) {
    where_clauses <- c(where_clauses, paste0(
      "fk_mediaid IN (", 
      paste0("$", param_counter:(param_counter + length(mediaIDs) - 1), 
             collapse = ", "),
      ")"))
    params <- append(params, mediaIDs)
    param_counter <- param_counter + length(mediaIDs)
  }
  
  # Model IDs
  if (any(!is.na(modelIDs))) {
    where_clauses <- c(where_clauses, paste0(
      "fk_modelid IN (", 
      paste0("$", param_counter:(param_counter + length(modelIDs) - 1), 
             collapse = ", "),
      ")"))
    params <- append(params, modelIDs)
    param_counter <- param_counter + length(modelIDs)
  }
  
  # Location IDs
  if (any(!is.na(locationIDs))) {
    where_clauses <- c(where_clauses, paste0(
      "fk_locationid IN (", 
      paste0("$", param_counter:(param_counter + length(locationIDs) - 1), 
             collapse = ", "),
      ")"))
    params <- append(params, locationIDs)
    param_counter <- param_counter + length(locationIDs)
  }
  
  # Taxon IDs
  if (any(!is.na(taxonIDs))) {
    where_clauses <- c(where_clauses, paste0(
      "fk_taxonid IN (", 
      paste0("$", param_counter:(param_counter + length(taxonIDs) - 1), 
             collapse = ", "),
      ")"))
    params <- append(params, taxonIDs)
    param_counter <- param_counter + length(taxonIDs)
  }
  
  # value_num
  if (!is.na(value_num)) {
    where_clauses <- c(
      where_clauses,
      paste0('value_num ', ifelse(lessThan, paste0("<= $", param_counter), paste0(">= $", param_counter))))
    params[[param_counter]] <- value_num
    param_counter <- param_counter + 1
  }
  
  # Add all where clauses to the original statement
  if (length(where_clauses) != 0) {
    stmnt <- paste(stmnt, 'AND', paste(where_clauses, collapse = ' AND '))
  }
  
  # Finish the statement
  stmnt <- paste0(stmnt, ";")
  
  result <- DBI::dbSendQuery(con, stmnt)
  DBI::dbBind(result, params)
  modeloutputs <- DBI::dbFetch(result)
  DBI::dbClearResult(result)
  
  return(modeloutputs)
  
}

# qryModelSettings --------------------------------------------------------
#' @name qryModelSettings
#' @title Queries the database and returns a data frame including all settings
#' and setting options for a given model
#' @description This query provides a full list of settings and setting options
#' associated with a given model ID.
#' @param con An open connection to an AMMonitor database
#' @param modelid A pk_modelid, as listed in the "models" table of the database
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage qryModelSettings(con, modelid, disconnect = FALSE)
#' @return A dataframe containing all valid settings and setting options for
#' the provided model ID
#' @family query
#' @concept msettingnames 
#' @concept msettingoptions 
#' @concept models
#' @importFrom DBI dbIsValid dbDisconnect dbSendQuery dbBind dbFetch 
#' dbClearResult
#' @export
#' @examples
#' \dontrun{
#' # Create a demo database
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Connect to the database
#' conx <- dbSetCon(file.path(demo_fp, "/database/demo.sqlite"))
#' 
#' # Look at the models table
#' DBI::dbReadTable(conx, "models")
#' 
#' # Query settings for model 6, BirdNET
#' qry <- qryModelSettings(
#'  con = conx,
#'  modelid = 6,
#'  disconnect = FALSE)
#'  
#' # View the query results
#' View(qry)
#' }
#' 

qryModelSettings <- function(con, modelid, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Run a parameterized query to find the model settings
  rs <- DBI::dbSendQuery(
    conn = con,
    statement = "SELECT msettingnames.setting_name, msettingnames.description,
  msettingoptions.option_name, msettingoptions.description
  FROM msettingnames LEFT JOIN msettingoptions
  ON msettingnames.pk_msettingnameid = msettingoptions.fk_msettingnameid
  WHERE msettingnames.fk_modelid = $1;"
  )
  DBI::dbBind(rs, list(modelid))
  settings <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  colnames(settings) <- c("setting_name", "setting_description", "option_name",
                          "option_description")
  
  return(settings)

}

# qryEquipModelSettings --------------------------------------------------------
#' @name qryEquipModelSettings
#' @title Queries the database and returns a data frame including all settings
#' and setting options for a given equipment model
#' @description This query provides a full list of settings and setting options
#' associated with a given equipmodel ID.
#' @param con An open connection to an AMMonitor database
#' @param equipmodelid A pk_equipmodelid, as listed in the "equipmodels" table 
#' of the database
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#' exit? Default is FALSE.
#' @usage qryEquipModelSettings(con, equipmodelid, disconnect = FALSE)
#' @return A dataframe containing all valid settings and setting options for
#' the provided equipmodel ID
#' @family query
#' @concept esettingnames 
#' @concept esettingoptions 
#' @concept equipmodels
#' @importFrom DBI dbIsValid dbDisconnect dbSendQuery dbBind dbFetch 
#' dbClearResult
#' @export
#' @examples
#' \dontrun{
#' # Create a demo database
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Connect to the database
#' conx <- dbSetCon(file.path(demo_fp, "/database/demo.sqlite"))
#' 
#' # Look at the equipmodels table
#' DBI::dbReadTable(conx, "equipmodels")
#' 
#' # Query settings for recorder_model_x
#' qry <- qryEquipModelSettings(
#'  con = conx,
#'  equipmodelid = "recorder_model_x",
#'  disconnect = FALSE)
#'  
#' # View the query results
#' View(qry)
#' }
#' 

qryEquipModelSettings <- function(con, equipmodelid, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Run a parameterized query to find the model settings
  rs <- DBI::dbSendQuery(
    conn = con,
    statement = "SELECT esettingnames.setting_name, esettingnames.description,
  esettingoptions.option_name, esettingoptions.description
  FROM esettingnames LEFT JOIN esettingoptions
  ON esettingnames.pk_esettingnameid = esettingoptions.fk_esettingnameid
  WHERE esettingnames.fk_equipmodelid = $1;"
  )
  DBI::dbBind(rs, list(equipmodelid))
  settings <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  
  colnames(settings) <- c("setting_name", "setting_description", "option_name",
                          "option_description")
  
  return(settings)
  
}