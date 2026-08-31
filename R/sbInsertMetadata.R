#' @name sbInsertMetadata
#' @aliases sbInsertMetadata
#' @title Retrieve metadata about a ScienceBase release and insert it into the 
#' database
#' @description \code{sbInsertMetadata} retrieves metadata about a ScienceBase 
#' item and inserts it into an AMMonitor database.
#' @param con A connection to the AMMonitor database
#' @param sbItemNum The ScienceBase item ID for an AMMonitor project data 
#' release on ScienceBase.
#' @param xmlPath The file path to the XML file containing metadata about the
#' ScienceBase release.
#' @param fromRehydrate TRUE or FALSE. Is the function called from sbRehydrate?
#' Default is TRUE
#' @param disconnect TRUE or FALSE, whether to disconnect from the database 
#' after the function is run. Default is FALSE
#' @return This function does not return a value, but will add the metadata 
#' about a release
#' as a row in the sciencebase table in the AMMonitor database.
#' @importFrom DBI dbIsValid dbGetQuery dbExecute dbAppendTable dbDisconnect
#' dbReadTable 
#' @details
#' #' For more information, please see the "sciencebase" learnr tutorial:
#' \code{learnr::run_tutorial(name = "sciencebase", package = "AMMonitor")}.
#' 
#' @importFrom sbtools item_get item_file_download
#' @importFrom stats na.omit
#' @usage sbInsertMetadata(con, sbItemNum, xmlPath, fromRehydrate = TRUE, 
#' disconnect = FALSE )
#' @export
#' @examples 
#' \dontrun{
#' # --------------------------------------------
#' # for this example, we will assume that the demo database is a "master"
#' # --------------------------------------------
#' 
#' # create the mini demo project
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite")) 
#' 
#' # list the tables of the database
#' DBI::dbListTables(conx)
#' 
#' # let's assume the data were released on ScienceBase; have a look
#' browseURL("https://www.sciencebase.gov/catalog/item/654a576bd34ee4b6e05c24d6")
#' 
#' # the master database needs to be associated with this release
#' 
#' # note the master sciencebase table is empty 
#' DBI::dbReadTable(con, name = 'sciencebase')
#' 
#' # note the media files are not associated with the release (fk_sciencebaseid)
#' DBI::dbReadTable(con, name = 'media')
#' 
#' # now let's insert the release info into the "master" database  --------
#' 
#' # set Sciencebase item number
#' sbItemNum <- "654a576bd34ee4b6e05c24d6"
#' 
#' # download the release xml file  
#' fp <- sbtools::item_file_download(
#'   sb_id = sbItemNum,
#'   names = "Middle Earth Wildlife Study Volume 1 (2023 - 2023).xml",
#'   destinations = "Middle Earth Wildlife Study Volume 1 (2023 - 2023).xml")
#'   
#' 
#' # insert the metadata to the database
#' sbInsertMetadata(
#'   con = con,
#'   sbItemNum = "654a576bd34ee4b6e05c24d6",
#'   xmlPath = fp,
#'   fromRehydrate = FALSE,
#'   disconnect = FALSE
#'  )
#' 
#' # look at the sciencebase table again: note the primary key = 1
#' DBI::dbReadTable(con, name = 'sciencebase')
#' 
#' # confirm the media files are now associated with release 1
#' DBI::dbReadTable(con, name = 'media')
#' 
#' }

sbInsertMetadata <- function(con, sbItemNum, xmlPath, fromRehydrate = TRUE, disconnect = FALSE) {
  
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (disconnect == TRUE) {
    on.exit(
      DBI::dbDisconnect(con)
    )
  }

  # read in an empty table for sciencebase
  sb_new <- DBI::dbGetQuery(con, "SELECT * FROM sciencebase LIMIT 0")
  
  # new index
  max_index <- DBI::dbGetQuery(con, "SELECT MAX(pk_sciencebaseid) FROM sciencebase")[,]
  i_new <- ifelse(is.na(max_index), 1, max_index + 1)
  
  # get metadata about the item
  metadata <- sbtools::item_get(sbItemNum)
  
  # read in xml file
  xml_metadata <- readLines(xmlPath)
  
  # Start making the row for insertion into database
  # Uses metadata from JSON whenever possible
  sb_new[1, 'pk_sciencebaseid'] <- i_new
  sb_new$fk_personid <- "unknownPerson"
  sb_new$sb_project_item <- metadata$parentId
  sb_new$sb_volume_item <- metadata$id
  sb_new$ipds <- unlist(lapply(metadata$identifiers, function(x) {
    if (x$type == "IPDS") {
      x$key
    }
  }))
  #sb_new$rights <- metadata$rights
  
  # date columns
  date_cols <- c("pubdate", "begdate", "enddate")
  for (colname in date_cols) {
    date_str <- searchText(paste0("(.*<", colname, ">)(.*)(</", colname, ">.*)"), "\\2", xml_metadata)
    sb_new[[colname]] <- paste0(substr(date_str, 1, 4), "-", substr(date_str, 5, 6), "-", substr(date_str, 7, 8))
  }
  
  # keywords 
  fixed_kt <- character(0)
  fixed_kw <- character(0)
  place_kt <- character(0)
  place_kw <- character(0)
  taxa_kt <- character(0)
  taxa_kw <- character(0)
  other_kt <- character(0)
  other_kw <- character(0)
  for (tag in metadata$tags) {
    if (tag$type == "Place") {
      place_kt <- c(place_kt, tag$scheme)
      place_kw <- c(place_kw, tag$name)
    } else {
      if (tag$scheme == "None") {
        other_kt <- c(other_kt, "None")
        other_kw <- c(other_kw, tag$name)
      } else  {
        fixed_kt <- c(fixed_kt, tag$scheme)
        fixed_kw <- c(fixed_kw, tag$name)
      } 
    }
  }
  sb_new$themekeythesaurusfixed <- paste(unique(fixed_kt), collapse = ", ")
  sb_new$themekeywordsfixed <- paste(unique(fixed_kw), collapse = ", ")
  sb_new$placekt <- paste(unique(place_kt), collapse = ", ")
  sb_new$placekey <- paste(unique(place_kw), collapse = ", ")
  #sb_new$themekeytaxat <- paste(unique(taxa_kt), collapse = ", ")
  #sb_new$themekeytaxakey <- paste(unique(taxa_kw), collapse = ", ")
  sb_new$themekeythesaurusnone <- paste(unique(other_kt), collapse = ", ")
  sb_new$themekeywords <- paste(unique(other_kw), collapse = ", ")
  
  # originators
  sb_new$origin <- paste(searchText("(.*<origin>)(.*)(</origin>.*)", "\\2", xml_metadata), collapse = ", ")
  
  # update frequency
  sb_new$sbupdate <- searchText("(.*<update>)(.*)(</update>.*)", "\\2", xml_metadata)
  
  # single-use metadata nodes where the column name is the same
  single_nodes <- c(
    "accconst", "useconst", "onlink", "title", "publish", "pubplace", "geoform", "abstract", "purpose",
    "progress", "supplinf", "datacred", "native", "distliab", "formname", 
    "fees", "networkr", "metd", "metstdv", "metstdn"
  )
  for (colname in single_nodes) {
    sb_new[[colname]] <- searchText(paste0("(.*<", colname, ">)(.*)(</", colname, ">.*)"), "\\2", xml_metadata)
  }
  
  # point of contact information
  ptc <- xml_metadata[which(grepl("<ptcontac>", xml_metadata)):which(grepl("</ptcontac>", xml_metadata))]
  contact_cols <- c(
    "cntorg", "cntper", "address", "city", "state", "postal", "country", 
    "cntvoice", "cntemail", "addrtype"
  )
  for (colname in contact_cols) {
    sb_new[[colname]] <- searchText(paste0("(.*<", colname, ">)(.*)(</", colname, ">.*)"), "\\2", ptc)
  }
  
  # data quality columns
  data_cols <- c(
    "current", "attraccr", "logic", "complete", "horizpar", "vertaccr"
  )
  for (colname in data_cols) {
    sb_new[[colname]] <- searchText(paste0("(.*<", colname, ">)(.*)(</", colname, ">.*)"), "\\2", xml_metadata)
  }
  
  # md process steps
  procsteps <- searchText("(.*<procdesc>)(.*)(</procdesc>.*)", "\\2", xml_metadata)
  procdates <- searchText("(.*<procdate>)(.*)(</procdate>.*)", "\\2", xml_metadata)
  for (i in seq_len(min(length(procsteps), 5))) {
    sb_new[[paste0("md_process_step", i)]] <- procsteps[i]
    sb_new[[paste0("md_process_date", i)]] <- paste0(substr(procdates[i], 1, 4), "-", substr(procdates[i], 5, 6), "-", substr(procdates[i], 7, 8))
  }
  
  # geography
  geog_cols <- c("descgeog", "westbc", "northbc", "eastbc", "southbc")
  for (colname in geog_cols) {
    sb_new[[colname]] <- searchText(paste0("(.*<", colname, ">)(.*)(</", colname, ">.*)"), "\\2", xml_metadata)
  }
  
  # distributor contact info
  distrib <- xml_metadata[which(grepl("<distrib>", xml_metadata)):which(grepl("</distrib>", xml_metadata))]
  for (colname in contact_cols) {
    sb_new[[paste0("dist_", colname)]] <- searchText(paste0("(.*<", colname, ">)(.*)(</", colname, ">.*)"), "\\2", distrib)
  }
  
  # metadata contact info
  metc <- xml_metadata[which(grepl("<metc>", xml_metadata)):which(grepl("</metc>", xml_metadata))]
  for (colname in contact_cols) {
    sb_new[[paste0("meta_", colname)]] <- searchText(paste0("(.*<", colname, ">)(.*)(</", colname, ">.*)"), "\\2", metc)
  }
  
  # add row to database
  dbAppendTable(
    con,
    "sciencebase",
    sb_new
  )
  
  # update media table with release info
  if (fromRehydrate == TRUE) {
    
    # update media table with release id = 1
    DBI::dbExecute(
      conn = con,
      statement = "UPDATE media SET  fk_sciencebaseid = 1;"
    )
  } else {
    
    # read in the release media csv
    fp <- file.path(tempdir(), 'media.csv')
    
    sbtools::item_file_download(
      sb_id = sbItemNum, 
      names = "media.csv", 
      destinations = fp,
      overwrite_file = TRUE)
    
    media <- read.csv(fp)
    
    # extract the media filenames
    media_fn <- media[, "filename", drop = TRUE]
    
    # get indices of media in the master database
    
    db_media <- DBI::dbReadTable(con, "media")
    indices <- match(db_media$filename, media_fn, nomatch = NA)
    indices <- stats::na.omit(indices)
    
    names_string <- paste0("'", db_media[indices, "filename", drop = TRUE], collapse = "', ")
    names_string <- paste0(names_string, "'")
    
    # update the media table
    DBI::dbExecute(
      conn = con,
      statement = paste0(
        "UPDATE media SET  fk_sciencebaseid = ", 
        i_new, 
        " WHERE filename IN (", names_string , ") ;")
    )
    
  }

  return(sb_new)
}

#' @name searchText
#' @title Search text for every instance of a pattern in a character vector, and return only those instances
#' @description Searches a vector of character strings for a pattern, and changes them
#' to empty strings if the pattern isn't found. 
#' @param pattern Regex pattern to be passed to grepl and gsub
#' @param replacement Replacement for pattern, passed to gsub
#' @param x Character vector to search
#' @return A character vector of the same length as x or less, with only the instances of the pattern

searchText <- function(pattern, replacement, x) {
  contains <- grepl(pattern, x)
  if (sum(contains) == 0) {
    return(NA)
  } else {
    matches <- trimws(gsub(pattern, replacement, x[contains]))
    matches[which(matches == "NA")] <- NA
    return(matches)
  }
}
