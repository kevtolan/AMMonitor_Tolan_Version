##### from Richard Single

multifig <- function(rows,cols)
{
  cat("Usage: multifig(rows,cols)\n")
  tmp <- try(par(mfrow=c(rows,cols)))
  ifelse(is(tmp,"try-error"),"There was an error or warning - See 'Usage' above","OK")
}

#------------------------------------------------------------------------------
delete_media <- function(conx, delete_these_files) {
    for (filename in delete_these_files) {
	query <- paste0("DELETE FROM media WHERE filename = '", filename, "';")
        starttime <- Sys.time()
	dbExecute(conx, query)
	endtime <- Sys.time()
        cat(paste0(Sys.time()), "Deleted file:", filename, " - Elapsed time: ", difftime(endtime, starttime, unit="mins"), "minutes", "\n")
  }
    cat("All files have been processed and removed from the database.\n")
}
#------------------------------------------------------------------------------
subset_files <- function(conx, site, start_date, stop_date) {
  mediafiles <- RSQLite::dbReadTable(conn = conx, name = 'media')
  mediafiles$Site <- sub("_\\d{8}_\\d{6}\\.[^.]+$", "", mediafiles$filename)
  mediafiles <- mediafiles[mediafiles$Site == site, ]
  mediafiles$date <- paste0(mediafiles$start_date, " ", mediafiles$start_time) %>%
    as.POSIXlt()
  DATESTART <- as.Date(start_date)
  DATESTOP <- as.Date(stop_date)
  mediasubset <- mediafiles %>% filter(between(date, DATESTART, DATESTOP)) }

#------------------------------------------------------------------------------
library(DBI)
library(parallel)

delete_media_parallel <- function(db_connection_details, delete_these_files) {
  # Number of cores to use
  num_cores <- detectCores() - 1

  # Split the list of files into chunks for parallel processing
  file_chunks <- split(delete_these_files, cut(seq_along(delete_these_files), num_cores, labels = FALSE))

  # Create a cluster
  cl <- makeCluster(num_cores)

  # Export necessary variables to the cluster
  clusterExport(cl, varlist = c("db_connection_details"), envir = environment())
  clusterEvalQ(cl, library(DBI))  # Load DBI in worker processes

  # Define a worker function
  worker <- function(files, db_connection_details) {
    # Establish a new database connection for each worker
    conx <- do.call(dbConnect, db_connection_details)
    on.exit(dbDisconnect(conx))  # Ensure the connection is closed after use

    for (filename in files) {
      query <- paste0("DELETE FROM media WHERE filename = '", filename, "';")
      starttime <- Sys.time()
      dbExecute(conx, query)
      endtime <- Sys.time()
      cat(paste0(Sys.time(), " Deleted file: ", filename, " - Elapsed time: ",
                 difftime(endtime, starttime, unit = "mins"), " minutes", "\n"))
    }
  }

  # Run the worker function on each chunk in parallel
  results <- parLapply(cl, file_chunks, function(chunk) {
    worker(chunk, db_connection_details)
  })

  # Stop the cluster
  stopCluster(cl)

  cat("All files have been processed and removed from the database.\n")
}


# Import birdnet to demo_v2.sqlite
#' @name import_birdnet
#' @title Import birdnet results to the modeloutputs table
#' @description Import birdnet results to the modeloutputs table
#' @param con The filepath to the AMMonitor database file
#' @param birdnet_outputs_files Vector with the full file path of birdnet output
#' files.
#' @usage import_birdnet(con, birdnet_outputs_files)
#' @return NULL.
#' @details Used to import birdnet results to the modeloutputs table.
#' @importFrom DBI dbGetQuery, dbAppendTable
#' @importFrom utils read.csv
#'

# import_birdnet <- function(con, birdnet_outputs_files) {
#
#   taxa <- DBI::dbGetQuery(con, 'SELECT pk_taxonid FROM taxa;')
#
#   for (birdnet_file in birdnet_outputs_files) {
#
#     birdnet_data <- suppressWarnings(utils::read.csv(paste(birdnet_file, sep = '/')))
#
#     mediaID <- DBI::dbGetQuery(
#       con,
#       paste0(
#         "SELECT pk_mediaid FROM media WHERE filename = '",
#         paste0(strsplit(basename(birdnet_file), '\\.')[[1]][1], '.wav'),
#         "';"
#       )
#     )[,]
#
#     if (length(mediaID) == 0) {print(paste('Matching media file not found:', mediaID)); next}
#
#     if (nrow(birdnet_data) == 0) {
#       DBI::dbAppendTable(
#         con,
#         'modeloutputs',
#         data.frame(
#           fk_mediaid = mediaID,
#           fk_modelid = 1,
#           fk_taxonid = "no-species"
#         )
#       )
#       next
#     }
#     valid_tags <- birdnet_data[birdnet_data$common_name %in% taxa$pk_taxonid,]
#
#     if (nrow(valid_tags) == 0) {next}
#
#     new_modelOutputs <- data.frame(
#       fk_mediaid = mediaID,
#       fk_taxonid = valid_tags$common_name,
#       fk_modelid = 1,
#       x_min = valid_tags$start,
#       x_max = valid_tags$end,
#       value_num = valid_tags$confidence
#     )
#
#     DBI::dbAppendTable(con, 'modeloutputs', new_modelOutputs)
#
#   }-
# }
import_birdnet <- function(con, birdnet_outputs_files) {

  taxa <- DBI::dbGetQuery(con, 'SELECT pk_taxonid FROM taxa;')

  for (birdnet_file in birdnet_outputs_files) {

    birdnet_data <- suppressWarnings(utils::read.csv(paste(birdnet_file, sep = '/')))

    # Extract filename without extension
    base_filename <- strsplit(basename(birdnet_file), '\\.')[[1]][1]

    # Try first with lowercase '.wav'
    mediaID <- DBI::dbGetQuery(
      con,
      paste0("SELECT pk_mediaid FROM media WHERE filename = '", base_filename, ".wav';")
    )[,]

    # If no match, try with uppercase '.WAV'
    if (length(mediaID) == 0) {
      mediaID <- DBI::dbGetQuery(
        con,
        paste0("SELECT pk_mediaid FROM media WHERE filename = '", base_filename, ".WAV';")
      )[,]
    }

    # If still not found, print error message and continue
    if (length(mediaID) == 0) {
      print(paste('Matching media file not found:', birdnet_file))
      next
    }

    # If birdnet_data is empty, insert a placeholder record and continue
    if (nrow(birdnet_data) == 0) {
      DBI::dbAppendTable(
        con,
        'modeloutputs',
        data.frame(
          fk_mediaid = mediaID,
          fk_modelid = 1,
          fk_taxonid = "no-species"
        )
      )
      next
    }

    # Filter valid tags that exist in the taxa table
    valid_tags <- birdnet_data[birdnet_data$common_name %in% taxa$pk_taxonid,]

    if (nrow(valid_tags) == 0) next

    # Create data frame for insertion
    new_modelOutputs <- data.frame(
      fk_mediaid = mediaID,
      fk_taxonid = valid_tags$common_name,
      fk_modelid = 1,
      x_min = valid_tags$start,
      x_max = valid_tags$end,
      value_num = valid_tags$confidence
    )

    # Append results to database
    DBI::dbAppendTable(con, 'modeloutputs', new_modelOutputs)

  }
}

#------------------------------------------------------------------------------
#' @name testThreshold
#' @title Recording-level precision/recall/F1 across model-output score thresholds
#' @description For a given taxon and set of models, sweeps a range of
#' modeloutputs score thresholds and computes a recording-level confusion
#' matrix (TP/FP/TN/FN) at each one. Ground truth per recording, in priority
#' order: media.ManualDetx (if not NA, always wins: >0 is a true positive, 0 is
#' a true negative); otherwise, for a recording the model flagged, whether any
#' of its qualifying modeloutputs were accepted (unverified or verified valid,
#' i.e. NOT all rejected in modelverifications); otherwise, for a recording the
#' model didn't flag, a manual annotation of the taxon (false negative -- the
#' model missed it) or of "no-species" (true negative). Recordings with no
#' ground truth signal at all are excluded. Same logic as the recording-level
#' Wood Frog confusion matrix worked out in AM_VPMon.R, generalized to any
#' taxon/model set and swept across a vector of thresholds instead of one.
#' @param conx An open RSQLite/DBI connection to the AMMonitor database.
#' @param taxon Taxon id to evaluate (fk_taxonid in modeloutputs/annotations).
#' Default "Wood Frog".
#' @param model_ids Vector of fk_modelid values to include. Default c(4, 5),
#' the two Wood Frog binary-template models.
#' @param thresholds Vector of score thresholds to sweep. Default
#' seq(14, 30, by = 1).
#' @param make_plot Whether to build a ggplot of precision/recall/F1 vs.
#' threshold. Default TRUE.
#' @param plot_path If not NULL (and make_plot is TRUE), the plot is saved to
#' this file path via ggsave.
#' @usage testThreshold(conx, taxon = "Wood Frog", model_ids = c(4, 5), thresholds = seq(14, 30, by = 1))
#' @return A list with `sensitivity` (one row per threshold: TP/FP/TN/FN,
#' precision, recall, f1, accuracy, error_rate) and `plot` (a ggplot object,
#' or NULL if make_plot = FALSE).
testThreshold <- function(conx,
                                            taxon = "Wood Frog",
                                            model_ids = c(4, 5),
                                            thresholds = seq(14, 30, by = 1),
                                            make_plot = TRUE,
                                            plot_path = NULL) {

  ## ---- pull each source table ONCE, unfiltered by score, then re-filter per threshold ----
  wf_outputs_all <- DBI::dbGetQuery(
    conx,
    paste0("SELECT pk_modeloutputid, fk_mediaid, fk_modelid, value_num FROM modeloutputs WHERE fk_taxonid = '", taxon, "'")
  ) %>%
    dplyr::filter(fk_modelid %in% model_ids)

  verifs <- DBI::dbGetQuery(conx, "SELECT fk_modeloutputid, is_valid FROM modelverifications")

  annotations <- DBI::dbGetQuery(conx, "SELECT fk_mediaid, fk_taxonid FROM annotations") %>%
    dplyr::group_by(fk_mediaid) %>%
    dplyr::summarize(
      has_taxon     = any(fk_taxonid == taxon),
      has_nospecies = any(fk_taxonid == "no-species"),
      .groups = 'drop'
    )

  manualdetx <- DBI::dbGetQuery(conx, "SELECT pk_mediaid AS fk_mediaid, ManualDetx FROM media") %>%
    dplyr::filter(!is.na(ManualDetx))

  ## ---- recording-level confusion matrix at a single score threshold ----
  confusion_at_threshold <- function(score_threshold) {

    wf_outputs <- wf_outputs_all %>%
      dplyr::filter(value_num >= score_threshold) %>%
      dplyr::left_join(verifs, by = c("pk_modeloutputid" = "fk_modeloutputid"))

    flagged <- wf_outputs %>%
      dplyr::group_by(fk_mediaid) %>%
      dplyr::summarize(
        any_accepted = any(is.na(is_valid) | is_valid == 1),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(model_flag = TRUE, verif_truth = any_accepted)

    recordings <- dplyr::full_join(flagged, annotations, by = "fk_mediaid") %>%
      dplyr::full_join(manualdetx, by = "fk_mediaid") %>%
      dplyr::mutate(
        model_flag = dplyr::coalesce(model_flag, FALSE),
        truth = dplyr::case_when(
          !is.na(ManualDetx) ~ ManualDetx > 0,
          model_flag         ~ verif_truth,
          has_taxon          ~ TRUE,
          has_nospecies      ~ FALSE,
          TRUE ~ NA
        )
      ) %>%
      dplyr::filter(!is.na(truth)) %>%
      dplyr::mutate(call = dplyr::case_when(
        model_flag  & truth  ~ "TP",
        model_flag  & !truth ~ "FP",
        !model_flag & truth  ~ "FN",
        !model_flag & !truth ~ "TN"
      ))

    tab <- table(recordings$call)
    TP <- unname(tab["TP"]); TP[is.na(TP)] <- 0
    FP <- unname(tab["FP"]); FP[is.na(FP)] <- 0
    TN <- unname(tab["TN"]); TN[is.na(TN)] <- 0
    FN <- unname(tab["FN"]); FN[is.na(FN)] <- 0

    data.frame(
      threshold = score_threshold,
      TP = TP, FP = FP, TN = TN, FN = FN,
      precision  = TP / (TP + FP),
      recall     = TP / (TP + FN),
      f1         = 2 * (TP / (TP + FP)) * (TP / (TP + FN)) / ((TP / (TP + FP)) + (TP / (TP + FN))),
      accuracy   = (TP + TN) / (TP + TN + FP + FN),
      error_rate = (FP + FN) / (TP + TN + FP + FN)
    )
  }

  sensitivity <- do.call(rbind, lapply(thresholds, confusion_at_threshold))
  rownames(sensitivity) <- NULL

  p <- NULL
  if (make_plot) {
    sensitivity_long <- sensitivity %>%
      dplyr::select(threshold, precision, recall, f1) %>%
      tidyr::pivot_longer(-threshold, names_to = "metric", values_to = "value")

    p <- ggplot2::ggplot(sensitivity_long, ggplot2::aes(x = threshold, y = value, color = metric)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(breaks = thresholds) +
      ggplot2::labs(
        title = paste0(taxon, " detector: threshold sensitivity"),
        subtitle = "recording-level precision / recall / F1, ManualDetx overrides applied",
        x = "score threshold", y = NULL, color = NULL
      ) +
      ggplot2::theme_minimal()

    if (!is.null(plot_path)) {
      ggplot2::ggsave(plot_path, p, width = 7, height = 4.5, dpi = 150)
    }
  }

  list(sensitivity = sensitivity, plot = p)
}
