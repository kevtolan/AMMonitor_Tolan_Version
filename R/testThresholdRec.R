#' @name testThresholdRec
#' @title Recording-level precision/recall/F1 across model-output score thresholds
#' @description \code{testThresholdRec} sweeps a range of \code{modeloutputs}
#' score thresholds for a given taxon/model set and, at each one, computes a
#' recording-level confusion matrix (true/false positives/negatives) and the
#' resulting precision, recall, F1, accuracy, and error rate. Useful for
#' picking a score threshold for a template/model: raising it trades recall
#' for precision (fewer false alarms to manually reject, but a few more real
#' detections missed), and this shows exactly where that tradeoff lands
#' across a range of candidate cutoffs. A whole recording counts as one
#' unit here regardless of how many individual detections it holds; see
#' \code{\link{testThresholdDetx}} for the individual-detection-level
#' version of this same analysis.
#' @param con An open database connection.
#' @param model_ids Vector of \code{fk_modelid} values (from the models
#' table) to include -- e.g. the model(s) trained/run for a particular
#' template or species.
#' @param taxon_id A valid primary key reference to the taxa table (the
#' species/template being evaluated).
#' @param thresholds Vector of \code{value_num} score thresholds to sweep.
#' Default = NULL, which derives 17 evenly-spaced thresholds spanning the
#' observed score range for \code{taxon_id}/\code{model_ids} in this
#' database (model-output score scales vary widely -- e.g. 0-1 for BirdNET
#' confidence vs. tens for monitoR binary-template scores -- so there's no
#' single sensible default range across models).
#' @param make_plot Default = TRUE. Whether to also build a ggplot of
#' precision/recall/F1 vs. threshold.
#' @return A list with two elements: \code{sensitivity}, a data.frame with
#' one row per threshold (threshold, TP, FP, TN, FN, precision, recall, f1,
#' accuracy, error_rate); and \code{plot}, a ggplot object (or NULL if
#' make_plot = FALSE).
#' @details Ground truth for a recording is established in priority order:
#' (1) \code{media.ManualDetx}, if that column exists in this database and
#' isn't NA for the recording -- a manual override count, treated as a true
#' positive if > 0 and a true negative if 0; (2) otherwise, for a recording
#' with at least one qualifying \code{modeloutputs} row (score >=
#' threshold), whether any of them were accepted -- unverified, or verified
#' valid in \code{modelverifications} -- rather than all rejected; (3)
#' otherwise, for a recording with no qualifying modeloutputs, a manual
#' \code{annotations} entry for \code{taxon_id} (a false negative -- the
#' model missed a real detection) or for \code{"no-species"} (a true
#' negative -- a human confirmed nothing was there). Recordings with none of
#' the above are excluded from the confusion matrix at that threshold, since
#' there's no ground truth to score them against.
#' @family classifier
#' @importFrom DBI dbGetQuery dbListFields
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_color_manual scale_x_continuous labs theme_minimal
#' @export
#' @examples
#' \dontrun{
#'
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#'
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' # sweep score thresholds for a given model/taxon
#' result <- testThresholdRec(
#'   con = conx,
#'   model_ids = 6,
#'   taxon_id = "oven"
#' )
#'
#' # one row per threshold: TP/FP/TN/FN, precision, recall, f1, ...
#' result$sensitivity
#'
#' # precision/recall/F1 vs. threshold
#' result$plot
#'
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#'
#' }

testThresholdRec <- function(con, model_ids, taxon_id, thresholds = NULL, make_plot = TRUE) {

  wf_outputs_all <- DBI::dbGetQuery(
    con,
    "SELECT pk_modeloutputid, fk_mediaid, fk_modelid, value_num FROM modeloutputs WHERE fk_taxonid = $taxon_id;",
    params = list(taxon_id = taxon_id)
  )
  wf_outputs_all <- wf_outputs_all[wf_outputs_all$fk_modelid %in% model_ids, ]

  if (nrow(wf_outputs_all) == 0) {
    stop("No modeloutputs found for taxon_id '", taxon_id, "' with fk_modelid in {", paste(model_ids, collapse = ", "), "}.")
  }

  if (is.null(thresholds)) {
    thresholds <- seq(
      min(wf_outputs_all$value_num, na.rm = TRUE),
      max(wf_outputs_all$value_num, na.rm = TRUE),
      length.out = 17
    )
  }

  verifs <- DBI::dbGetQuery(con, "SELECT fk_modeloutputid, is_valid FROM modelverifications;")

  # Per-recording ground truth from manual annotations: has this recording
  # been tagged with taxon_id (a real detection), or with "no-species" (a
  # human reviewed it and confirmed nothing's there)?
  annotations_raw <- DBI::dbGetQuery(con, "SELECT fk_mediaid, fk_taxonid FROM annotations;")
  if (nrow(annotations_raw) == 0) {
    annotations <- data.frame(fk_mediaid = integer(0), has_taxon = logical(0), has_nospecies = logical(0))
  } else {
    has_taxon_by_media <- tapply(annotations_raw$fk_taxonid, annotations_raw$fk_mediaid, function(x) any(x == taxon_id))
    has_nospecies_by_media <- tapply(annotations_raw$fk_taxonid, annotations_raw$fk_mediaid, function(x) any(x == "no-species"))
    annotations <- data.frame(
      fk_mediaid = as.integer(names(has_taxon_by_media)),
      has_taxon = as.vector(has_taxon_by_media),
      has_nospecies = as.vector(has_nospecies_by_media)
    )
  }

  # media.ManualDetx is a manual detection-count override some AMMonitor
  # projects add to their media table -- not part of the standard schema,
  # so only use it if it's actually present in this database.
  has_manualdetx <- "ManualDetx" %in% DBI::dbListFields(con, "media")
  manualdetx <- if (has_manualdetx) {
    md <- DBI::dbGetQuery(con, "SELECT pk_mediaid AS fk_mediaid, ManualDetx FROM media;")
    md[!is.na(md$ManualDetx), ]
  } else {
    data.frame(fk_mediaid = integer(0), ManualDetx = numeric(0))
  }

  # Recording-level confusion matrix at a single score threshold.
  confusion_at_threshold <- function(score_threshold) {

    wf_outputs <- wf_outputs_all[wf_outputs_all$value_num >= score_threshold, ]
    wf_outputs <- merge(wf_outputs, verifs, by.x = "pk_modeloutputid", by.y = "fk_modeloutputid", all.x = TRUE)

    if (nrow(wf_outputs) == 0) {
      flagged <- data.frame(fk_mediaid = integer(0), verif_truth = logical(0), model_flag = logical(0))
    } else {
      verif_truth_by_media <- tapply(wf_outputs$is_valid, wf_outputs$fk_mediaid, function(x) any(is.na(x) | x == 1))
      flagged <- data.frame(
        fk_mediaid = as.integer(names(verif_truth_by_media)),
        verif_truth = as.vector(verif_truth_by_media),
        model_flag = TRUE
      )
    }

    recordings <- merge(flagged, annotations, by = "fk_mediaid", all = TRUE)
    recordings <- merge(recordings, manualdetx, by = "fk_mediaid", all.x = TRUE)
    recordings$model_flag[is.na(recordings$model_flag)] <- FALSE

    # Ground truth, in priority order: ManualDetx override (if present) >
    # model verification (for flagged recordings) > manual annotation (for
    # unflagged recordings). Recordings matching none of these are excluded.
    recordings$truth <- with(recordings, ifelse(
      !is.na(ManualDetx), ManualDetx > 0,
      ifelse(
        model_flag, verif_truth,
        ifelse(
          !is.na(has_taxon) & has_taxon, TRUE,
          ifelse(!is.na(has_nospecies) & has_nospecies, FALSE, NA)
        )
      )
    ))
    recordings <- recordings[!is.na(recordings$truth), ]

    TP <- sum(recordings$model_flag & recordings$truth)
    FP <- sum(recordings$model_flag & !recordings$truth)
    FN <- sum(!recordings$model_flag & recordings$truth)
    TN <- sum(!recordings$model_flag & !recordings$truth)

    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)

    data.frame(
      threshold = score_threshold,
      TP = TP, FP = FP, TN = TN, FN = FN,
      precision = precision,
      recall = recall,
      f1 = 2 * precision * recall / (precision + recall),
      accuracy = (TP + TN) / (TP + TN + FP + FN),
      error_rate = (FP + FN) / (TP + TN + FP + FN)
    )
  }

  sensitivity <- do.call(rbind, lapply(thresholds, confusion_at_threshold))
  rownames(sensitivity) <- NULL

  p <- NULL
  if (make_plot) {
    sensitivity_long <- data.frame(
      threshold = rep(sensitivity$threshold, 3),
      metric = rep(c("precision", "recall", "f1"), each = nrow(sensitivity)),
      value = c(sensitivity$precision, sensitivity$recall, sensitivity$f1)
    )

    p <- ggplot2::ggplot(sensitivity_long, ggplot2::aes(x = threshold, y = value, color = metric)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point() +
      ggplot2::scale_color_manual(values = c(precision = "#F6511D", recall = "#FFB400", f1 = "#00A6ED")) +
      ggplot2::scale_x_continuous(breaks = thresholds) +
      ggplot2::labs(
        title = paste0(taxon_id, " detector: threshold sensitivity"),
        subtitle = "recording-level precision / recall / F1",
        x = "score threshold", y = NULL, color = NULL
      ) +
      ggplot2::theme_minimal()
  }

  list(sensitivity = sensitivity, plot = p)
}
