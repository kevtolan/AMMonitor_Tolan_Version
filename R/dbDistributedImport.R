#' @name dbDistributedImport
#' @aliases dbDistributedImport
#' @title Import data from a distributed database
#' @description Function to be used by AMMonitor database managers to import
#' externally created annotations or verifications created from the 
#' \code{dbDistributed} function. 
#' @param con  An open connection to the master database
#' @param con_distributed An open connection to the distributed database
#' @param mode What data should be imported: "annotate" or "verify"
#' @param disconnect  TRUE or FALSE. Should the master database connection be severed
#' on exit? Default is FALSE
#' @usage dbDistributedImport(con, con_distributed, mode, disconnect = FALSE)
#' @return Message indicating whether new data was successfully imported
#' @details  
#' See the "dbdistributed" learnr tutorial for more details on the database.
#' The tutorial can be launched with learnr::run_tutorial(name = "dbdistributed",
#' package = "AMMonitor").
#' @family annotation
#' @importFrom DBI dbIsValid dbDisconnect dbReadTable dbAppendTable
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
#' # look at the annotations table; note that Bilbo made 25 annotations
#' DBI::dbReadTable(conx, name = "annotations")
#' 
#' # look at the verifications table; note that Sam verified several
#' # of Bilbo's annotations
#' DBI::dbReadTable(conx, name = "annotationverifications")
#' 
#' # let's let Gandalf verify Bilbo's records too
#' # create a distributed database for Gandalf to work on; 
#' # Gandalf will add verifications to media files 1:15
#' 
#' distributed_fp <- dbDistributed(
#'  con = conx,
#'  out_filepath = tempdir(),
#'  person_id = "gandalf", 
#'  media_ids = 1:15, 
#'  mode = "verify", 
#'  use_renv = FALSE,
#'  disconnect = FALSE)
#'  
#'  # make sure the distributed mini project exists
#'  file.exists(distributed_fp)
#'  
#'  # zip and distribute the project to Gandalf to add verifications ----
#'  # simulate some new verifications by Gandalf
#'  # we'll assume Gandalf verifies the first 15 annotations as correct annotations
#'  db_name <- list.files(paste0(distributed_fp, "/database"), full.names = TRUE)
#'  gandalf_con <- dbSetCon(db_name)
#'  
#'  DBI::dbAppendTable(
#'   conn = gandalf_con,
#'   name = "annotationverifications",
#'   value = data.frame(
#'     pk_annoverificationid = NA,
#'     is_valid = 1,
#'     fk_personid = "gandalf",
#'     fk_annotationid = 1:15,
#'     timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#'   )
#'  )
#'  
#'  # confirm gandalf's records in his database
#'  DBI::dbReadTable(gandalf_con, name = "annotationverifications")
#'  
#'  # import Gandalf's verifications to the master database
#'  dbDistributedImport(
#'   con = conx,
#'   con_distributed = gandalf_con,
#'   mode = "verify",
#'   disconnect = FALSE
#'  )
#'  
#'  # check the master annotationverifications table
#'  DBI::dbReadTable(conx, "annotationverifications")
#'
#' }
#'

dbDistributedImport <- function(con, con_distributed, mode, disconnect = FALSE){
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  if (DBI::dbIsValid(con_distributed) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  on.exit(expr = {
    if (disconnect == TRUE) 
      DBI::dbDisconnect(con)
      DBI::dbDisconnect(con_distributed)
  }) 
  
  if (mode == "annotate") {
    
    # import media tags -------------------
    new_mediatags <- DBI::dbReadTable(con_distributed, "mediatags")
    if (nrow(new_mediatags) > 0) {
      
      # reset mediatag_id and append to master
      new_mediatags[, "pk_mediatagid"] <- NULL
      DBI::dbAppendTable(con, name = "mediatags", new_mediatags)
      message(paste("Added", nrow(new_mediatags), 'new mediatags'))
    }
      
    # import annotations and annotags
    new_annotations <- DBI::dbReadTable(con_distributed, "annotations")

    if (nrow(new_annotations) > 0) {
      
      start_annoID <- dbGetQuery(con, 'SELECT MAX(ABS(pk_annotationid))+1 FROM annotations;')[,]
      start_annoID <- ifelse(is.na(start_annoID), 1, start_annoID)
      old_annoIDs <- new_annotations$pk_annotationid
      new_annoIDs <- start_annoID:(start_annoID+nrow(new_annotations)-1)
      
      new_annotations$pk_annotationid <- new_annoIDs
      
      dbAppendTable(con, 'annotations', new_annotations)
      
      message(paste("Added", nrow(new_annotations), 'new annotations'))
      
      new_annotags <-  DBI::dbReadTable(con_distributed, "annotags")
      
      if (nrow(new_annotags) > 0) {
        
        # retrieve new records to get primary keys
        new_fkAnnoIDs <- new_annoIDs[match(new_annotags$fk_annotationid, old_annoIDs)]
        
        # add annotags with new fk reference
        start_tagID <- dbGetQuery(con, 'SELECT MAX(ABS(pk_annotagid))+1 FROM annotags;')[,]
        start_tagID <- ifelse(is.na(start_tagID), 1, start_tagID)
        old_tagIDs <- new_annotags$pk_tagid
        new_tagIDs <- start_tagID:(start_tagID+nrow(new_annotags)-1)
        new_annotags$pk_annotagid <- new_tagIDs
        new_annotags$fk_annotationid[!is.na(new_fkAnnoIDs)] <- new_fkAnnoIDs[!is.na(new_fkAnnoIDs)]
        
        dbAppendTable(con, 'annotags', new_annotags)
        
        message(paste("Added", nrow(new_annotags), 'new annotags.'))
        
      } # end of new annotags
      
    } # end of new annotations

  } # end of annotations
    
  if (mode == "verify") {
    
    # read in mediatag verifications
    new_mediatagverifications <- DBI::dbReadTable(con_distributed, "mediatagverifications")
    if (nrow(new_mediatagverifications) > 0) {
      new_mediatagverifications[, "pk_mediatagverificationid"] <- NA
      DBI::dbAppendTable(con, name = "mediatagverifications",  new_mediatagverifications)
      message(paste("Added", nrow(new_mediatagverifications), 'new mediatagverifications'))
    }
    
    # annotation verifications
    new_annotationverifications <- DBI::dbReadTable(con_distributed, "annotationverifications")
    if (nrow(new_annotationverifications) > 0) {
      new_annotationverifications[, "pk_annoverificationid"] <- NA
      DBI::dbAppendTable(con, name = "annotationverifications",  new_annotationverifications)
      message(paste("Added", nrow(new_annotationverifications), 'new annotationverifications'))
    }
    
    # annotag verifications
    new_annotagverifications <- DBI::dbReadTable(con_distributed, "annotagverifications")
    if (nrow(new_annotagverifications) > 0) {
      new_annotagverifications[, "pk_tagverificationid"] <- NA
      DBI::dbAppendTable(con, name = "annotagverifications",  new_annotagverifications)
      message(paste("Added", nrow(new_annotagverifications), 'new annotagverifications'))
    }
    
    # model verifications
    new_modelverifications <- DBI::dbReadTable(con_distributed, "modelverifications")
    if (nrow(new_modelverifications) > 0) {
      new_modelverifications[, "pk_modelverificationid"] <- NULL
      DBI::dbAppendTable(con, name = "modelverifications",  new_modelverifications)
      message(paste("Added", nrow(new_modelverifications), 'new modelverifications'))
    }
    
  } # end of verifications

} # end of function
