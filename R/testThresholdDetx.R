#' @name testThresholdDetx
#' @title Detection-level precision/recall/F1 across model-output score thresholds
#' @description \code{testThresholdDetx} sweeps a range of \code{modeloutputs}
#' score thresholds for a given taxon/model set and, at each one, computes a
#' detection-level precision, recall, and F1 -- each individual model
#' detection (a single \code{modeloutputs} row, i.e. one scored time window)
#' is its own unit here, rather than a whole recording as in
#' \code{\link{testThresholdRec}}. Useful when a recording can contain many
#' separate calls and you want to know how many of the model's individual
#' detections are right, and how many individual real calls it misses,
#' rather than just whether it got a recording right overall.
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
#' one row per threshold (threshold, TP, FP, FN, precision, recall, f1);
#' and \code{plot}, a ggplot object (or NULL if make_plot = FALSE).
#' @details Each qualifying \code{modeloutputs} row (score >= threshold) is
#' scored individually against \code{modelverifications}: a true positive if
#' verified valid, a false positive if verified invalid. Unverified
#' detections are excluded (there's no ground truth yet for that specific
#' call) -- they are not assumed correct the way an unverified recording is
#' in \code{testThresholdRec}'s whole-recording ground truth, since assuming
#' every unverified individual call is correct would inflate TP with no real
#' evidence. False negatives come from a separate, independent source of
#' real events -- \code{annotations} of \code{taxon_id} -- rather than from
#' modeloutputs at all: an annotation is a miss (FN) if no accepted
#' modeloutput (score >= threshold, and either unverified or verified valid)
#' in the same recording overlaps its time window
#' (\code{x_min}/\code{x_max}). There is no detection-level true negative:
#' unlike a whole recording, a stretch of time with no detection and no
#' annotation isn't a discrete, countable event, so (unlike
#' \code{testThresholdRec}) this returns precision/recall/F1 only, with no
#' accuracy or error rate. \code{media.ManualDetx} (used by
#' \code{testThresholdRec} when present) isn't used here either -- it's a
#' whole-recording detection count with no per-event time window, so it has
#' no natural role in a detection-level analysis.
#' @family classifier
#' @importFrom DBI dbGetQuery
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
#' # sweep score thresholds for a given model/taxon, at the detection level
#' result <- testThresholdDetx(
#'   con = conx,
#'   model_ids = 6,
#'   taxon_id = "oven"
#' )
#'
#' # one row per threshold: TP/FP/FN, precision, recall, f1
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

testThresholdDetx <- function(con, model_ids, taxon_id, thresholds = NULL, make_plot = TRUE) {

  wf_outputs_all <- DBI::dbGetQuery(
    con,
    "SELECT pk_modeloutputid, fk_mediaid, fk_modelid, value_num, x_min, x_max FROM modeloutputs WHERE fk_taxonid = $taxon_id;",
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

  # Independent real-event catalog for false negatives -- manual detections
  # of taxon_id, each with its own time window, regardless of whether the
  # model ever produced a corresponding modeloutput at all.
  annotations <- DBI::dbGetQuery(
    con,
    "SELECT fk_mediaid, x_min, x_max FROM annotations WHERE fk_taxonid = $taxon_id;",
    params = list(taxon_id = taxon_id)
  )
  annotations$annotation_row <- seq_len(nrow(annotations))

  # Detection-level precision/recall/F1 at a single score threshold.
  confusion_at_threshold <- function(score_threshold) {

    wf_outputs <- wf_outputs_all[wf_outputs_all$value_num >= score_threshold, ]
    wf_outputs <- merge(wf_outputs, verifs, by.x = "pk_modeloutputid", by.y = "fk_modeloutputid", all.x = TRUE)

    TP <- sum(wf_outputs$is_valid == 1, na.rm = TRUE)
    FP <- sum(wf_outputs$is_valid == 0, na.rm = TRUE)

    if (nrow(annotations) == 0) {
      FN <- 0
    } else {
      accepted <- wf_outputs[is.na(wf_outputs$is_valid) | wf_outputs$is_valid == 1, ]
      if (nrow(accepted) == 0) {
        FN <- nrow(annotations)
      } else {
        candidates <- merge(annotations, accepted, by = "fk_mediaid", suffixes = c("_anno", "_mo"))
        overlap <- candidates$x_min_anno < candidates$x_max_mo & candidates$x_max_anno > candidates$x_min_mo
        matched <- unique(candidates$annotation_row[overlap])
        FN <- nrow(annotations) - length(matched)
      }
    }

    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)

    data.frame(
      threshold = score_threshold,
      TP = TP, FP = FP, FN = FN,
      precision = precision,
      recall = recall,
      f1 = 2 * precision * recall / (precision + recall)
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
        subtitle = "detection-level precision / recall / F1",
        x = "score threshold", y = NULL, color = NULL
      ) +
      ggplot2::theme_minimal()
  }

  list(sensitivity = sensitivity, plot = p)
}
