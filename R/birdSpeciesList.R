#' @name birdSpeciesList
#' @title Manage the species list used to restrict BirdNET detections
#' @description Reads/adds/removes species from a species list CSV (one
#' common name per line) used to restrict \code{\link{birdsDetect}}
#' detections to a plausible regional set. A starter list is bundled with
#' the package at \code{system.file("extdata/birdnet_species_list.csv",
#' package = "AMMonitor")} -- copy it to your own project directory before
#' editing, the same way you would with \code{\link{modelAdd}}'s Excel
#' template.
#' @param path Filepath to the species list CSV.
#' @return Character vector of common names.
#' @examples
#' \dontrun{
#'
#' # copy the starter list to your own project directory
#' fp <- system.file("extdata/birdnet_species_list.csv", package = "AMMonitor")
#' file.copy(from = fp, to = "birdnet_species_list.csv", overwrite = FALSE)
#'
#' birdSpeciesList("birdnet_species_list.csv")
#' birdSpeciesAdd("Roseate Spoonbill", "birdnet_species_list.csv")
#' birdSpeciesRemove("Roseate Spoonbill", "birdnet_species_list.csv")
#' }
#' @export
birdSpeciesList <- function(path) {
  path <- path.expand(path)
  if (!file.exists(path)) {
    stop("Species list not found at ", path)
  }
  species <- readLines(path, warn = FALSE)
  species <- trimws(species)
  species[species != ""]
}

#' @rdname birdSpeciesList
#' @param species Character vector of common names to add. Any already
#' present in the list are silently skipped.
#' @export
birdSpeciesAdd <- function(species, path) {
  current <- birdSpeciesList(path)
  new_species <- setdiff(trimws(species), current)
  if (length(new_species) == 0) {
    message("Nothing to add -- already in the list.")
    return(invisible(current))
  }
  updated <- sort(c(current, new_species))
  writeLines(updated, path.expand(path))
  message("Added: ", paste(new_species, collapse = ", "))
  invisible(updated)
}

#' @rdname birdSpeciesList
#' @param species Character vector of common names to remove. Any not
#' present in the list are silently skipped.
#' @export
birdSpeciesRemove <- function(species, path) {
  current <- birdSpeciesList(path)
  to_remove <- intersect(trimws(species), current)
  if (length(to_remove) == 0) {
    message("Nothing to remove -- not found in the list.")
    return(invisible(current))
  }
  updated <- setdiff(current, to_remove)
  writeLines(updated, path.expand(path))
  message("Removed: ", paste(to_remove, collapse = ", "))
  invisible(updated)
}
