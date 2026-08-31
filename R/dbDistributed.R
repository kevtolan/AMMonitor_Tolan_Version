#' @name dbDistributed
#' @aliases dbDistributed
#' @title Create a stand-alone database for distribution to
#' volunteer taggers or verifiers
#' @description Function to be used by AMMonitor database managers to create a
#' stand-alone mini AMMonitor project in a specified directory for
#' distribution to others to permit external collaborators to tag or verify
#' media without access to the master AMMonitor database.
#' @param con  An open connection to an AMMonitor database
#' @param out_filepath The path to store the mini project
#' @param person_id  The tagger/verifier's pk_personid from the people table.
#' @param media_ids Vector of pk_mediaids from the media table.
#' @param mode "annotate" or "verify".
#' @param use_renv TRUE or FALSE. Should renv be used to create the distributed 
#' file? Currently unsupported and default is FALSE.
#' @param disconnect  TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE.
#' @usage dbDistributed(con, out_filepath, person_id, media_ids, mode = "verify",
#' use_renv = FALSE, disconnect = FALSE)
#' @return A directory of a mini AMMonitor project that can be compressed and 
#' distributed to taggers/verifiers for independent work
#' @details  
#' See the "dbdistributed" learnr tutorial for more details on the use
#' of this function.
#' 
#' The tutorial can be launched with learnr::run_tutorial(name = "dbdistributed",
#' package = "AMMonitor").
#' @family annotation
#' @importFrom DBI dbSendQuery dbSendStatement dbGetQuery dbReadTable
#' dbAppendTable dbListTables dbIsValid dbDisconnect dbClearResult dbExecute
#' @importFrom renv init
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
#' # look at the annotations table; Bilbo made annotations; Frodo will verify
#' # them
#' DBI::dbReadTable(conx, name = "annotations")
#' 
#' # create a mini AMMonitor project to send to Frodo
#' distributed_fp <- dbDistributed(
#'  con = conx,
#'  out_filepath = tempdir(),
#'  person_id = "fbaggins", 
#'  media_ids = 1:15, 
#'  mode = "verify", 
#'  use_renv = FALSE,
#'  disconnect = FALSE)
#'  
#' # go look at the newly created mini-project; zip and send to Frodo
#'  dir.exists(distributed_fp)
#'  
#' # show the files in the mini project
#'  list.files(distributed_fp, recursive = TRUE)
#'  
#' # launch the mini project in Shiny 
#'  launchApp(distributed_fp)
#'  
#' # another example; let Gandalf make annotations too
#'  distributed_fp <- dbDistributed(
#'  con = conx,
#'  out_filepath = tempdir(),
#'  person_id = "gandalf", 
#'  media_ids = 1:15, 
#'  mode = "annotate", 
#'  use_renv = FALSE,
#'  disconnect = FALSE)
#'  
#'  # go look at the newly created mini-project
#'  dir.exists(distributed_fp)
#'  
#'  # test the mini project by launching the mini project in Shiny 
#'  launchApp(distributed_fp)
#'  
#'  # if all ok, zip and send to Gandalf
#' }
#'

dbDistributed <- function(
    con,
    out_filepath,
    person_id,
    media_ids,
    mode = "verify",
    use_renv = FALSE,
    disconnect = FALSE
    ) {

  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  if (use_renv == TRUE) stop("The renv option is not yet implemented in the current version of AMMonitor")
  
  # Disconnect from old database if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }

  # ensure correct mode input
  if (!mode %in% c("annotate", "verify")) {
    stop("The mode can only be annotate or verify.")
  }
  
  # get the AMMonitor project directory
  amm_dir <- file.path(dirname(dirname(con@dbname)))
  db_name <- basename(con@dbname)
  
  # create the outfile directory (a stripped ammonitor directory structure) ----
  amm_new_name <- paste0(
    person_id, "_", mode, "_",
    format(Sys.time(), format = '%Y_%m_%d-%H_%M')
  )

  amm_new_path <- paste0(out_filepath, "/", amm_new_name)

  # create subdirectories
  dir.create(paste0(out_filepath, "/", amm_new_name))
  dir.create(paste0(amm_new_path, "/settings"))
  dir.create(paste0(amm_new_path, "/database"))
  dir.create(paste0(amm_new_path, "/recordings"))
  dir.create(paste0(amm_new_path, "/photos"))
  
  # create default user as person_id
  writeLines(
    text = person_id, 
    con = file(paste0(amm_new_path, "/settings/default_user.txt"))
  )
  
  # copy database
  file.copy(
    from = con@dbname,
    to = file.path(amm_new_path, "database", db_name),
    overwrite = TRUE)
  
  # connect to the new database
  conx_new <- dbSetCon(file.path(amm_new_path, "database", db_name))
  
  # check if person is in database
  people <- DBI::dbGetQuery(
    conn = conx_new,
    statement = "SELECT pk_personid FROM people")

  if (!person_id %in% people$pk_personid) stop(
  "The person_id is not listed in your database's people table.")

  # table manipulation --------------------------------
  empty_tables <- switch(
    mode,
    "annotate" = c("accounts", "analyses", "analysisoutputs", "annotagverifications", "annotationverifications", "econfigvalues", "esettingoptions", "languages", "logs", "mconfigvalues", "mediatagverifications", "modeloutputs", "mconfignames", "modelverifications", "msettingnames", "msettingoptions", "modellabels", "models", "objectives", "priorities", "sciencebase", "spatials", "taxonlanguage", "temporallistitems", "temporallists", "temporals"),
    "verify" = c("accounts", "analyses", "analysisoutputs", "econfigvalues", "esettingoptions", "languages", "logs", "msettingoptions", "objectives", "priorities", "sciencebase", "spatials", "taxonlanguage", "temporallistitems", "temporallists", "temporals")
  )

  # delete records from unneeded tables
  for (i in seq_len(length(empty_tables))) {
    rs <- DBI::dbSendStatement(
      con = conx_new,
      statement = paste0("DELETE from ", empty_tables[i], ";")
    )
    DBI::dbClearResult(rs)
  }
  
  omit_media_string <- paste(media_ids, collapse = ",")
  
  # delete all media not associated identified in media ids
  stmt <- paste0("DELETE FROM media WHERE pk_mediaid not in (", omit_media_string , ");")
  rs <- DBI::dbExecute(conn = conx_new, statement = stmt)
  
  # database adjustments by mode -------------------------------------
  if (mode == "annotate") {
    
    # empty annotations table; deletions should remove all verifications
    DBI::dbExecute(conx_new, statement = "DELETE FROM annotations;")
    # empty media tags table; deletions should remove all verifications
    DBI::dbExecute(conx_new, statement = "DELETE FROM mediatags;")
    
  } else {
    
    # keep only other's annotations to verify
    DBI::dbExecute(
      conx_new, 
      paste0("DELETE FROM annotations WHERE  fk_personid = '", person_id, "';"))
    
    DBI::dbExecute(
      conx_new, 
      paste0("DELETE FROM mediatags WHERE  fk_personid = '", person_id, "';"))
    
    # delete current verifications as only new records can be added (?)
    DBI::dbExecute(conx_new, statement = "DELETE FROM annotationverifications;")
    DBI::dbExecute(conx_new, statement = "DELETE FROM mediatagverifications;")
    DBI::dbExecute(conx_new, statement = "DELETE FROM annotagverifications;")
    DBI::dbExecute(conx_new, statement = "DELETE FROM modelverifications;")
  }
  
  # media to distribute
  media_files <- DBI::dbReadTable(conx_new, "media")
  
  # Prep settings files
  audio_setting_path <- file.path(amm_dir, "settings/audio_path.txt")
  audio_setting <- ifelse(
    file.exists(audio_setting_path),
    readLines(audio_setting_path),
    NA
  )
  photo_setting_path <- file.path(amm_dir, "settings/image_path.txt")
  photo_setting <- ifelse(
    file.exists(photo_setting_path),
    readLines(photo_setting_path),
    NA
  )
  
  # Copy each media file
  for (i_media in seq_len(nrow(media_files))) {
    
    if (media_files$media_type[i_media] == "photo") {
      media_folder <- "photos"
      media_setting <- photo_setting
    } else {
      media_folder <- "recordings"
      media_setting <- audio_setting
    }
    
    # Resolve filepath for media
    if (!is.na(media_files$filepath[i_media])) {
      media_path <- media_files$filepath[i_media]
    } else {
      if (!is.na(media_setting)) {
        media_path <- file.path(media_setting, media_files$filename[i_media])
      } else {
        media_path <- file.path(
          amm_dir,
          media_folder,
          media_files$filename[i_media]
        )
        if (!file.exists(media_path)) {
          stop(paste('Media file not found:', media_files$filename[i_media]))
        }
      }
    }
    
    # Determine if file is stored locally
    if (!grepl("https://", media_path)) {
      # Copy the file (if local)
      file.copy(
        from = media_path,
        to = file.path(amm_new_path, media_folder, media_files$filename[i_media])
      )
      
      # Update filepath to NA (to use relative path for end-user's file system)
      dbExecute(
        conx_new,
        paste0(
          "UPDATE media SET filepath = NULL WHERE pk_mediaid = ",
          media_files$pk_mediaid[i_media],
          ";"
        )
      )
    } else {
      # Include filepath for all cloud-based storage
      dbExecute(
        conx_new,
        paste0(
          "UPDATE media SET filepath = '",
          media_path,
          "' WHERE pk_mediaid = ",
          media_files$pk_mediaid[i_media],
          ";"
        )
      )
    }
  }
  
  # Disconnect from new database on exit
  on.exit(expr = DBI::dbDisconnect(conx_new))
  
  if (use_renv == TRUE) {

    # https://rstudio.github.io/renv/articles/renv.html

    # copy the start-up file script
    file.copy(
      from = paste0(find.package("AMMonitor"), "/extdata/data-raw/standalone_tagger_startup_renv.R"),
      to = amm_new_name)

    # initialize renv on the output folder  ----------------------------------
    renv::init(
      project = amm_new_name,
      restart = FALSE
    )
  } 

  # compact database
  rs <- DBI::dbSendStatement(conn = conx_new, statement = "VACUUM;")
  DBI::dbClearResult(rs)
  
  message(paste0("A new mini-project has been created at ", amm_new_path), "\n")
  message(paste0("You can  zip this directory and send to ", person_id, "\n 
   to add annotations or verifications."))
  message("This mini project has the following files: \n")
  print(list.files(amm_new_path, recursive = T))
  return(amm_new_path)

} # end of function
