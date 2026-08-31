#' @name mergedAudioRemove
#' @aliases mergedAudioRemove
#' @title Remove a merged audio file and associate verifications with the 
#' original annotations/modeloutputs
#' @description Removes a merged audio file created with mergedAudioCreate() 
#' from the database and associates all verifications with the original 
#' annotations and/or modeloutputs
#' @param con An open connection to an AMMonitor database.
#' @param visitID the pk_visitid of the visit associated with the merged media 
#' to be removed. Default is NA. Use only one of visitID and mediaID.
#' @param mediaID the pk_mediaid of the merged media to be removed. Default is 
#' NA. Use only one of visitID and mediaID.
#' @param ammPath the path to the directory of the AMMonitor project. Default is 
#' getwd().
#' @param disconnect TRUE or FALSE. Should the database connection be severed 
#' on exit? Default is FALSE.
#' @usage mergedAudioRemove(con, visitID = NA, mediaID = NA, ammPath = getwd(), 
#' disconnect = FALSE)
#' @details This function takes a merged audio file created by 
#' "mergedAudioCreate()" and associates any verifications with the original
#' media file(s). The merged media file and any associated database entries will
#' then be deleted.
#' 
#' The user provides either a pk_visitid or pk_mediaid associated with the 
#' merged media file to be deleted. This function will then use the mapping 
#' formed by mergedAudioCreate() to assign any verifications to the original 
#' media. Once this process is complete, the function will remove all remaining 
#' references to the merged audio file from the database. All records will 
#' appear in the database as if they had been applied to the original audio 
#' files.
#' @family annotationverifications, modeloutputverifications
#' @importFrom DBI dbExecute dbDisconnect dbIsValid dbSendQuery dbBind dbFetch
#' dbClearResult
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' # Create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # Set a connection to the demo database
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # Create a merged audio file from specified annotations and model verifications
#' modeloutput_ids <- 10:12
#' annotation_ids <- 26:27
#' 
#' mergedAudioCreate(
#'   con = conx,
#'   annotation_ids = annotation_ids,
#'   modeloutput_ids = modeloutput_ids,
#'   ammPath = demo_fp
#'   )
#' 
#' Look at the new visit that is associated with the merged file
#' DBI::dbGetQuery(
#'   conx,
#'   "SELECT * FROM visits WHERE visit_type = 'merged-verification';"
#'   )
#' 
#' # Confirm the new audio is present
#' list.files(file.path(demo_fp, "recordings"))
#'   
#' # Add some model verifications to the audio file as an example
#' # normally this would be done in the shiny app, but here coded for demonstration
#' DBI::dbAppendTable(
#'   conn = conx,
#'   name = "modelverifications",
#'   value = data.frame(
#'     pk_modelverificationid = NA,
#'     fk_modeloutputid = 56:58,
#'     fk_personid = "gandalf",
#'     is_valid = 1,
#'     timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#'     )
#'   )
#'
#' # Add annotation verifications to the audio file as an example
#' # normally this would be done in the shiny app, but here coded for demonstration
#' DBI::dbAppendTable(
#'   conn = conx,
#'   name = "annotationverifications",
#'   value = data.frame(
#'     pk_annoverificationid = NA,
#'     is_valid = 1,
#'     fk_personid = "gandalf",
#'     fk_annotationid = 28:29,
#'     timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#'     )
#'   )   
#' 
#' # remove the merged media file and move verifications to the original media
#' mergedAudioRemove(
#'   con = conx,
#'   mediaID = 27,
#'   ammPath = demo_fp,
#'   disconnect = FALSE)
#'   
#' # Examine the updated modelverifications table
#' DBI::dbReadTable(
#'   conn = conx,
#'   name = "modelverifications")
#'   
#' # Examine the updated annotationverifications table
#' DBI::dbReadTable(
#'   conn = conx,
#'   name = "annotationverifications")
#' 
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' }

mergedAudioRemove <- function(con, visitID = NA, mediaID = NA, ammPath = getwd(), disconnect = FALSE){
  
  # Run some basic checks
  if (xor(is.na(visitID), is.na(mediaID)) == 0) {
    stop("Please input one visit ID or media ID.")
  }
  
  if (DBI::dbIsValid(con) == FALSE) {
    stop("The database connection is not valid.")
  }
  
  # Set up the disconnect argument
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Get modeloutputIDs and annotationIDS associated with the merged media -----------------------------
  if (!is.na(visitID)) {
    
    # Get the media entry associated with the selected visit
    rs <- DBI::dbSendQuery(
      con,
      "SELECT * FROM media WHERE fk_visitid = $1;"
    )
    DBI::dbBind(rs, list(visitID))
    (mediaID_full <- DBI::dbFetch(rs))
    DBI::dbClearResult(rs)
    
    # extract just the pk_mediaID
    mediaID <- mediaID_full$pk_mediaid
    
  }
  
  if (length(mediaID) > 1) {
    stop("Multiple media files found. Please check your visit/mediaID.")
  }
    
  if (!is.na(mediaID)) {
    
    # Get the full media entry, if it doesn't already exist
    if (exists("mediaID_full") == FALSE) {
      
      # Get the full media table entry for the merged media
      rs <- DBI::dbSendQuery(
        con,
        "SELECT * FROM media WHERE pk_mediaid = $1;"
      )
      DBI::dbBind(rs, list(mediaID))
      (mediaID_full <- DBI::dbFetch(rs))
      DBI::dbClearResult(rs)
      
    }
    
    # Get the full visit entry
    if (is.na(visitID)) {
      
      # Get the visitID from the full media entry
      visitID <- mediaID_full$fk_visitid
      
    }

    rs <- DBI::dbSendQuery(
      con,
      "SELECT * FROM visits WHERE pk_visitid = $1;"
    )
    DBI::dbBind(rs, list(visitID))
    (visitID_full <- DBI::dbFetch(rs))
    DBI::dbClearResult(rs)
    
    # Check to see if the selected media is a merged file
    if (visitID_full$visit_type != "merged-verification") {
      stop("Selected media is not a merged audio file. Please check your visit/media ID.")
    }
    
    # Get the modeloutput entries of all outputs associated with the selected media
    rs <- DBI::dbSendQuery(
      con,
      "SELECT * FROM modeloutputs WHERE fk_mediaid = $1;"
    )
    DBI::dbBind(rs, list(mediaID))
    (outputIDs <- DBI::dbFetch(rs))
    DBI::dbClearResult(rs)
    
    # Get the annotation entries of all annotations associated with the selected media
    rs <- DBI::dbSendQuery(
      con,
      "SELECT * FROM annotations WHERE fk_mediaid = $1;"
    )
    DBI::dbBind(rs, list(mediaID))
    (annoIDs <- DBI::dbFetch(rs))
    DBI::dbClearResult(rs)
    
  }
  
  # Update all fk_modeloutputids in modelverifications based on the fk_parentid
  # This will associate verifications with the original outputs

   for (i_output in seq_len(nrow(outputIDs))) {
    
    # Get the parent ID from the current output
    parentID <- outputIDs$fk_parentid[i_output]
    
    # update the modeloutputid
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE modelverifications SET ",
        "fk_modeloutputid = ", parentID,
        " WHERE fk_modeloutputid = ", outputIDs$pk_modeloutputid[i_output]
      ))
    
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM modeloutputs WHERE pk_modeloutputid = ",
        outputIDs$pk_modeloutputid[i_output], 
        ";"
      )
    )
  
  }
  
  # Update all fk_annotationids in annotationverifications based on the notes
  for (i_anno in seq_len(nrow(annoIDs))) {
    
    # Get the old ID from the notes section and set it as a number
    oldID <- as.numeric(annoIDs$notes[i_anno])
    
    # update the annotationid
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE annotationverifications SET ",
        "fk_annotationid = ", oldID,
        " WHERE fk_annotationid = ", annoIDs$pk_annotationid[i_anno]
      )
    )
    
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM annotations WHERE pk_annotationid = ",
        annoIDs$pk_annotationid[i_anno], 
        ";"
      )
    )
    
    
  }
  
  # Delete the merged media file -------------------------------

  # Delete the merged media file and remove it from the database
  delete_media <- AMMonitor::deleteRecord(
    con,
    table_name = "media",
    selected_row = mediaID_full,
    delete_file = TRUE,
    amm_project_path = ammPath
  )
  
  # Delete the associated visit from the database
  delete_visit <- AMMonitor::deleteRecord(
    con,
    table_name = "visits",
    selected_row = visitID_full
  )
  
  # return results
  if (!isTRUE(delete_visit$status) | !isTRUE(delete_media$status)) {
    
    stop("Unable to remove merged media file.")
    
  } else {
    
    return("Merged media successfully removed.")
    
  }
  
}
