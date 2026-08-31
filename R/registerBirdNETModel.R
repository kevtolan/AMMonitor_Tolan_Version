#' @name registerBirdNETModel
#' @title Register BirdNET as a model in the models table
#' @description One-time (safe to re-run) setup that adds a BirdNET model
#' row to the models table, so \code{\link{birdsDetect}} can find it by
#' name. Skips if a model is already registered under the given
#' modelVersion in this database.
#' @param con A connection to a SQLite or Postgres AMMonitor database.
#' @param modelVersion Default = 'v2.4'. BirdNET model version -- registered
#' as \code{model_name = paste0("BirdNET_", modelVersion)}, matching what
#' \code{\link{birdsDetect}} looks for.
#' @return The pk_modelid of the (possibly newly created) BirdNET model row,
#' invisibly.
#' @examples
#' \dontrun{
#' registerBirdNETModel(conx)
#' }
#' @export
registerBirdNETModel <- function(con, modelVersion = "v2.4") {
  model_name <- paste0("BirdNET_", modelVersion)

  existing <- DBI::dbGetQuery(
    con,
    paste0("SELECT pk_modelid FROM models WHERE model_name = '", model_name, "';")
  )

  if (nrow(existing) > 0) {
    message("Already registered as pk_modelid = ", existing$pk_modelid[1])
    return(invisible(existing$pk_modelid[1]))
  }

  next_id <- DBI::dbGetQuery(con, "SELECT MAX(pk_modelid) FROM models;")[1, 1]
  next_id <- ifelse(is.na(next_id), 1, next_id + 1)

  new_model <- data.frame(
    pk_modelid = next_id,
    model_name = model_name,
    model_url = "https://birdnet-team.github.io/birdnetR/",
    amml = NA_character_,
    model_type = "CNN",
    model_description = paste0(
      "BirdNET ", modelVersion, " bird sound classifier, run via the birdnetR R package ",
      "(see birdsDetect()). Detections are restricted to whichever species list you pass ",
      "as speciesListPath."
    ),
    model_citation = "Kahl, S., Wood, C. M., Eibl, M., & Klinck, H. (2021). BirdNET: A deep learning solution for avian diversity monitoring. Ecological Informatics, 61, 101236. https://doi.org/10.1016/j.ecoinf.2021.101236",
    fk_taxonid = NA_character_,
    fk_librarylistid = NA_character_,
    fk_parentid = NA_integer_,
    stringsAsFactors = FALSE
  )

  DBI::dbAppendTable(con, "models", new_model)
  message("Registered ", model_name, " as pk_modelid = ", next_id)
  invisible(next_id)
}
