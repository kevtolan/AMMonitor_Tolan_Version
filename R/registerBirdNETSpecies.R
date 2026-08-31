#' @name registerBirdNETSpecies
#' @title Register every species in a BirdNET species list as a taxon
#' @description One-time (safe to re-run) setup that registers every common
#' name in a managed BirdNET species list (see \code{\link{birdSpeciesList}})
#' as a taxon via ITIS TSN lookup, so \code{\link{birdsDetect}} can actually
#' store detections for them. Only looks up/adds species that aren't already
#' registered. Scientific-name lookups that turn up a deprecated TSN are
#' automatically resolved to their current accepted TSN, and any species
#' that can't be matched by scientific name falls back to an ITIS common-name
#' search before being reported as unmatched.
#' @param con A connection to a SQLite or Postgres AMMonitor database.
#' @param speciesListPath Character path to the managed species list CSV
#' (see \code{\link{birdSpeciesList}}).
#' @param modelVersion Default = 'v2.4'. BirdNET model version, used only to
#' load the matching label set for scientific-name resolution.
#' @param language Default = 'en_us'. BirdNET label language, passed to
#' \code{birdnetR::labels_path()}.
#' @param unmatchedPath Optional character path. If provided, any species
#' that still can't be matched to a TSN are written here as a CSV.
#' @return Invisibly, a data.frame with one row per species considered this
#' run and its lookup status ('found', 'found_via_common_name',
#' 'resolved_deprecated_tsn', or 'no_tsn_match').
#' @examples
#' \dontrun{
#' registerBirdNETSpecies(conx, speciesListPath = "~/birdnet_species_list.csv")
#' }
#' @export
registerBirdNETSpecies <- function(con,
                                    speciesListPath,
                                    modelVersion = "v2.4",
                                    language = "en_us",
                                    unmatchedPath = NULL) {

  common_names <- birdSpeciesList(speciesListPath)

  # BirdNET labels are "Scientific name_Common name" -- use this to get the
  # correct scientific name for each species in the working list.
  model <- birdnetR::birdnet_model_tflite(version = modelVersion)
  all_labels <- birdnetR::read_labels(birdnetR::labels_path(model, language = language))
  label_df <- data.frame(
    scientific_name = sub("_.*$", "", all_labels),
    common_name = sub("^.*_", "", all_labels),
    stringsAsFactors = FALSE
  )

  to_register <- label_df[label_df$common_name %in% common_names, ]

  existing_taxa <- DBI::dbGetQuery(con, "SELECT pk_taxonid FROM taxa;")[, 1]
  to_register <- to_register[!(to_register$common_name %in% existing_taxa), ]

  unmatched_labels <- setdiff(common_names, label_df$common_name)
  if (length(unmatched_labels) > 0) {
    message(
      length(unmatched_labels), " species in the working list don't match a BirdNET label and will be skipped:\n",
      paste(unmatched_labels, collapse = ", ")
    )
  }

  message(nrow(to_register), " species need TSN lookup and registration.")

  results <- data.frame(
    common_name = character(), scientific_name = character(),
    tsn = character(), status = character(), stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(to_register))) {
    sci <- to_register$scientific_name[i]
    com <- to_register$common_name[i]

    tsn <- NA_character_
    status <- "no_tsn_match"

    sci_tsn <- tryCatch({
      hits <- as.data.frame(ritis::search_scientific(sci))
      exact <- hits[hits$combinedName == sci, ]
      if (nrow(exact) >= 1) as.character(exact$tsn[1]) else NA_character_
    }, error = function(e) NA_character_)

    if (!is.na(sci_tsn)) {
      tsn <- sci_tsn
      status <- "found"

      # resolve deprecated/invalid TSNs to their current accepted TSN
      resolved <- tryCatch({
        acc <- as.data.frame(ritis::accepted_names(sci_tsn))
        if (nrow(acc) >= 1 && !is.na(acc$acceptedTsn[1]) && acc$acceptedTsn[1] != "") {
          as.character(acc$acceptedTsn[1])
        } else {
          NA_character_
        }
      }, error = function(e) NA_character_)

      if (!is.na(resolved) && resolved != tsn) {
        tsn <- resolved
        status <- "resolved_deprecated_tsn"
      }
    } else {
      # fall back to an ITIS common-name search
      com_tsn <- tryCatch({
        hits <- as.data.frame(ritis::search_common(com))
        exact <- hits[tolower(hits$commonName) == tolower(com), ]
        if (nrow(exact) >= 1) as.character(exact$tsn[1]) else NA_character_
      }, error = function(e) NA_character_)

      if (!is.na(com_tsn)) {
        tsn <- com_tsn
        status <- "found_via_common_name"
      }
    }

    results <- rbind(
      results,
      data.frame(common_name = com, scientific_name = sci, tsn = tsn, status = status, stringsAsFactors = FALSE)
    )

    if (i %% 25 == 0) message(i, " / ", nrow(to_register))
  }

  found <- results[results$status != "no_tsn_match", ]
  message(nrow(found), " of ", nrow(results), " species found a TSN.")

  if (nrow(found) > 0) {
    add_status <- taxaAdd(
      con = con,
      tsns = found$tsn,
      common_names = found$common_name,
      overwrite = FALSE
    )
    print(add_status)
  }

  missing <- results[results$status == "no_tsn_match", ]
  if (nrow(missing) > 0) {
    message("Could not find a TSN for these -- add manually with taxaAdd() if needed:")
    print(missing[, c("common_name", "scientific_name")])
    if (!is.null(unmatchedPath)) {
      write.csv(missing, unmatchedPath, row.names = FALSE)
      message("(saved to ", unmatchedPath, ")")
    }
  }

  message("Total taxa now: ", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM taxa;")[1, 1])

  invisible(results)
}
