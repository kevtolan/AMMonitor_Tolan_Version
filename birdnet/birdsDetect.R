#' @name birdsDetect
#' @title Detect bird species using BirdNET (birdnetR)
#' @description Runs BirdNET (via the birdnetR package) against recordings
#' and either returns the detections for review or inserts them into the
#' modeloutputs table. Mirrors the calling convention of
#' AMMonitor::scoresDetect() (con/recordingNames/dbInsert/showProgress).
#' @param con A connection to the AMMonitor SQLite database.
#' @param recordingNames Default = "all". Character vector of media
#' filenames to process, or "all" for every audio recording in the database.
#' @param minConfidence Default = 0.1. Minimum BirdNET confidence to keep a
#' detection (passed through to birdnetR::predict_species_from_audio_file).
#' @param speciesList Default = NULL. NULL uses the managed working list
#' (birdSpeciesList(), see birdSpeciesList.R -- add/remove species there).
#' Pass NA to disable species filtering entirely (BirdNET's full global
#' species list -- not recommended, produces geographically implausible
#' detections). Pass a character vector to use a one-off list for this call
#' only, without touching the managed list.
#' @param modelVersion Default = "v2.4". BirdNET model version.
#' @param language Default = "en_us". Language for species common names.
#' @param dbInsert Default = FALSE. If TRUE, results are inserted into
#' modeloutputs (skipping anything that's already there for the same
#' model+recording+species+time, so it's safe to re-run). If FALSE, results
#' are returned as a data.frame for review, nothing is written.
#' @param showProgress Default = FALSE.
#' @return A data.frame of detections (if dbInsert = FALSE), or invisible
#' NULL (if dbInsert = TRUE).
#' @details Species not yet present in the taxa table are dropped with a
#' warning naming them -- run Register_BirdNET_Species.R (or
#' AMMonitor::taxaAdd()) to register new species before they can be stored.
#' Recordings hosted on S3 (filepath starting with http/https) are
#' downloaded to a temp file for analysis and removed immediately after.

birdsDetect <- function(
    con,
    recordingNames = "all",
    minConfidence = 0.1,
    speciesList = NULL,
    modelVersion = "v2.4",
    language = "en_us",
    dbInsert = FALSE,
    showProgress = FALSE
) {

  if (!requireNamespace("birdnetR", quietly = TRUE)) {
    stop("Package 'birdnetR' is required. Install with install.packages('birdnetR').")
  }

  # ---- Resolve the registered BirdNET model row ----
  model_name <- paste0("BirdNET_", modelVersion)
  modelID <- DBI::dbGetQuery(
    con,
    paste0("SELECT pk_modelid FROM models WHERE model_name = '", model_name, "';")
  )[, 1]
  if (length(modelID) == 0) {
    stop(
      "No model named '", model_name, "' found in the models table. ",
      "Run Register_BirdNET_Model.R first (or register it under a different modelVersion)."
    )
  }

  # ---- Resolve which recordings to process ----
  if (identical(recordingNames, "all")) {
    media <- DBI::dbGetQuery(con, "SELECT pk_mediaid, filename, filepath FROM media WHERE media_type = 'audio';")
  } else {
    in_list <- paste(sprintf("'%s'", recordingNames), collapse = ", ")
    media <- DBI::dbGetQuery(con, paste0("SELECT pk_mediaid, filename, filepath FROM media WHERE filename IN (", in_list, ");"))
  }

  if (nrow(media) == 0) {
    message("No matching recordings found.")
    return(invisible(NULL))
  }

  # ---- Load the model once, reused across all recordings ----
  model <- birdnetR::birdnet_model_tflite(version = modelVersion, language = language)

  # ---- Resolve species filter ----
  # BirdNET's filter_species must be in its internal "Scientific name_Common
  # name" label format, not plain common names -- convert here.
  if (identical(speciesList, NA)) {
    filter_species <- NULL  # no filtering -- full global BirdNET species list
  } else {
    common_names <- if (is.null(speciesList)) {
      if (!exists("birdSpeciesList")) {
        stop("birdSpeciesList() not found -- source birdSpeciesList.R first, or pass speciesList explicitly.")
      }
      birdSpeciesList()
    } else {
      speciesList
    }

    all_labels <- birdnetR::read_labels(birdnetR::labels_path(model, language = language))
    label_common_names <- sub("^.*_", "", all_labels)

    filter_species <- all_labels[label_common_names %in% common_names]
    unmatched <- setdiff(common_names, label_common_names)
    if (length(unmatched) > 0) {
      warning(
        length(unmatched), " species in the species list don't match any BirdNET label and will be ignored: ",
        paste(unmatched, collapse = ", "),
        call. = FALSE
      )
    }
  }

  td <- tempdir(check = TRUE)
  all_results <- vector("list", nrow(media))

  for (i in seq_len(nrow(media))) {
    if (showProgress) cat(i, "/", nrow(media), ":", media$filename[i], "\n")

    fp <- media$filepath[i]
    is_remote <- grepl("^https?://", fp)
    local_path <- if (is_remote) {
      dest <- file.path(td, media$filename[i])
      ok <- tryCatch({
        utils::download.file(fp, dest, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (ok) dest else NA_character_
    } else if (file.exists(fp)) {
      fp
    } else {
      NA_character_
    }

    if (is.na(local_path)) {
      warning("Could not access recording for ", media$filename[i], "; skipping.")
      next
    }

    preds <- tryCatch(
      birdnetR::predict_species_from_audio_file(
        model,
        local_path,
        min_confidence = minConfidence,
        filter_species = filter_species,
        keep_empty = TRUE
      ),
      error = function(e) {
        warning("BirdNET failed on ", media$filename[i], ": ", conditionMessage(e))
        NULL
      }
    )

    if (is_remote && file.exists(local_path)) unlink(local_path)
    if (is.null(preds)) next

    real_detections <- preds[!is.na(preds$common_name), ]

    if (nrow(real_detections) == 0) {
      all_results[[i]] <- data.frame(
        fk_mediaid = media$pk_mediaid[i],
        fk_modelid = modelID,
        fk_taxonid = "no-species",
        x_min = NA_real_,
        x_max = NA_real_,
        y_min = NA_real_,
        y_max = NA_real_,
        value_num = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      all_results[[i]] <- data.frame(
        fk_mediaid = media$pk_mediaid[i],
        fk_modelid = modelID,
        fk_taxonid = real_detections$common_name,
        x_min = real_detections$start,
        x_max = real_detections$end,
        y_min = NA_real_,
        y_max = NA_real_,
        value_num = real_detections$confidence,
        stringsAsFactors = FALSE
      )
    }
  }

  results <- do.call(rbind, all_results)

  if (is.null(results) || nrow(results) == 0) {
    message("No results produced.")
    return(invisible(NULL))
  }

  # Drop (with a warning) any species not yet registered as taxa -- a
  # foreign key violation would otherwise reject the whole insert.
  known_taxa <- DBI::dbGetQuery(con, "SELECT pk_taxonid FROM taxa;")[, 1]
  unknown <- setdiff(unique(results$fk_taxonid), known_taxa)
  if (length(unknown) > 0) {
    warning(
      length(unknown), " detected species are not yet registered in the taxa table and were dropped: ",
      paste(unknown, collapse = ", "),
      ". Run Register_BirdNET_Species.R to add them, then re-run birdsDetect().",
      call. = FALSE
    )
    results <- results[results$fk_taxonid %in% known_taxa, ]
  }

  if (nrow(results) == 0) {
    message("Nothing left to insert after dropping unregistered species.")
    return(invisible(NULL))
  }

  if (!dbInsert) {
    rownames(results) <- NULL
    return(results)
  }

  # Skip anything already in modeloutputs for this model+file+species+time,
  # so re-running birdsDetect() on the same recordings is a no-op for
  # anything already stored (mirrors scoresDetect's dedup-before-insert).
  existing <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT fk_mediaid, fk_taxonid, x_min, x_max FROM modeloutputs WHERE fk_modelid = ", modelID,
      " AND fk_mediaid IN (", paste(unique(results$fk_mediaid), collapse = ", "), ");"
    )
  )
  if (nrow(existing) > 0) {
    dup_key <- function(df) paste(df$fk_mediaid, df$fk_taxonid, df$x_min, df$x_max, sep = "|")
    results <- results[!(dup_key(results) %in% dup_key(existing)), ]
  }

  if (nrow(results) == 0) {
    message("All results already exist in modeloutputs; nothing new to insert.")
    return(invisible(NULL))
  }

  rownames(results) <- NULL
  DBI::dbAppendTable(con, "modeloutputs", results)
  message("Inserted ", nrow(results), " new modeloutputs rows.")
  invisible(NULL)
}
