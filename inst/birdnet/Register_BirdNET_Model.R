# One-time setup: register BirdNET as a model in the models table.
# Safe to re-run -- skips if already registered.

library(RSQLite)

db.path <- '~/R/AMMonitor_VPMon/VPMon_AMM/database/VPMon_AMM.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

model_name <- "BirdNET_v2.4"

existing <- RSQLite::dbGetQuery(
  conx,
  paste0("SELECT pk_modelid FROM models WHERE model_name = '", model_name, "';")
)

if (nrow(existing) > 0) {
  cat("Already registered as pk_modelid =", existing$pk_modelid, "\n")
} else {
  next_id <- RSQLite::dbGetQuery(conx, "SELECT MAX(pk_modelid) FROM models;")[1, 1]
  next_id <- ifelse(is.na(next_id), 1, next_id + 1)

  new_model <- data.frame(
    pk_modelid = next_id,
    model_name = model_name,
    model_url = "https://birdnet-team.github.io/birdnetR/",
    amml = NA_character_,
    model_type = "CNN",
    model_description = "BirdNET v2.4 bird sound classifier, run via the birdnetR R package. Detections are restricted to the species in birdnet_species_list.csv (see birdSpeciesList.R).",
    model_citation = "Kahl, S., Wood, C. M., Eibl, M., & Klinck, H. (2021). BirdNET: A deep learning solution for avian diversity monitoring. Ecological Informatics, 61, 101236. https://doi.org/10.1016/j.ecoinf.2021.101236",
    fk_taxonid = NA_character_,
    fk_librarylistid = NA_character_,
    fk_parentid = NA_integer_,
    stringsAsFactors = FALSE
  )

  RSQLite::dbAppendTable(conx, "models", new_model)
  cat("Registered BirdNET as pk_modelid =", next_id, "\n")
}

RSQLite::dbDisconnect(conx)
