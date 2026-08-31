# One-time (re-runnable) setup: register every species in the managed
# BirdNET species list (birdnet_species_list.csv) as a taxon, so
# birdsDetect() can actually store detections for them. Safe to re-run --
# only looks up/adds species that aren't already registered.

library(RSQLite)
suppressPackageStartupMessages(library(ritis))
suppressPackageStartupMessages(library(AMMonitor))
suppressPackageStartupMessages(library(birdnetR))

# birdSpeciesList(), birdsDetect(), etc. ship as real exported functions in
# AMMonitor -- no source() needed as long as the package is loaded above.
species_list_path <- '~/R/AMMonitor_VPMon/birdnet_species_list.csv'

db.path <- '~/R/AMMonitor_VPMon/VPMon_AMM/database/VPMon_AMM.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

common_names <- birdSpeciesList(species_list_path)

# BirdNET labels are "Scientific name_Common name" -- use this to get the
# correct scientific name for each species in the working list.
model <- birdnet_model_tflite()
all_labels <- read_labels(labels_path(model, language = "en_us"))
label_df <- data.frame(
  scientific_name = sub("_.*$", "", all_labels),
  common_name = sub("^.*_", "", all_labels),
  stringsAsFactors = FALSE
)

to_register <- label_df[label_df$common_name %in% common_names, ]

existing_taxa <- DBI::dbGetQuery(conx, "SELECT pk_taxonid FROM taxa;")[, 1]
to_register <- to_register[!(to_register$common_name %in% existing_taxa), ]

unmatched <- setdiff(common_names, label_df$common_name)
if (length(unmatched) > 0) {
  cat(
    length(unmatched), "species in the working list don't match a BirdNET label and will be skipped:\n",
    paste(unmatched, collapse = ", "), "\n\n"
  )
}

cat(nrow(to_register), "species need TSN lookup and registration.\n\n")

results <- data.frame(
  common_name = character(), scientific_name = character(),
  tsn = character(), status = character(), stringsAsFactors = FALSE
)

for (i in seq_len(nrow(to_register))) {
  sci <- to_register$scientific_name[i]
  com <- to_register$common_name[i]

  tsn <- tryCatch({
    hits <- as.data.frame(ritis::search_scientific(sci))
    exact <- hits[hits$combinedName == sci, ]
    if (nrow(exact) >= 1) as.character(exact$tsn[1]) else NA_character_
  }, error = function(e) NA_character_)

  results <- rbind(
    results,
    data.frame(
      common_name = com, scientific_name = sci, tsn = tsn,
      status = ifelse(is.na(tsn), "no_tsn_match", "found"),
      stringsAsFactors = FALSE
    )
  )

  if (i %% 25 == 0) cat(i, "/", nrow(to_register), "\n")
}

found <- results[results$status == "found", ]
cat("\n", nrow(found), "of", nrow(results), "species found a TSN.\n")

if (nrow(found) > 0) {
  add_status <- taxaAdd(
    con = conx,
    tsns = found$tsn,
    common_names = found$common_name,
    overwrite = FALSE
  )
  print(add_status)
}

missing <- results[results$status != "found", ]
if (nrow(missing) > 0) {
  cat("\nCould not find a TSN for these -- add manually with taxaAdd() if needed:\n")
  print(missing[, c("common_name", "scientific_name")])
  write.csv(missing, "~/R/AMMonitor_VPMon/birdnet_species_unmatched.csv", row.names = FALSE)
  cat("(saved to birdnet_species_unmatched.csv)\n")
}

cat("\nTotal taxa now:", DBI::dbGetQuery(conx, "SELECT COUNT(*) FROM taxa;")[1,1], "\n")

RSQLite::dbDisconnect(conx)
