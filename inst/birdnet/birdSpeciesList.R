#' @name birdSpeciesList
#' @title Manage the species list used to restrict BirdNET detections
#' @description The working species list lives at birdnet_species_list.csv
#' (one common name per line). birdsDetect() reads it fresh on every call
#' via birdSpeciesList(), so edits here take effect on the next run without
#' needing to touch birdsDetect.R itself.
#' @details Originally seeded from
#' ~/Python/BirdNET-Analyzer/species_list_common.csv (BirdNET-Analyzer's
#' regional species list), then copied here as the list AMMonitor manages
#' going forward -- the two files are independent from this point on.

birdnet_species_list_path <- "~/R/AMMonitor_VPMon/birdnet_species_list.csv"

#' @describeIn birdSpeciesList Read the current working species list.
#' @return Character vector of common names.
birdSpeciesList <- function(path = birdnet_species_list_path) {
  path <- path.expand(path)
  if (!file.exists(path)) {
    stop("Species list not found at ", path)
  }
  species <- readLines(path, warn = FALSE)
  species <- trimws(species)
  species[species != ""]
}

#' @describeIn birdSpeciesList Add one or more species to the working list.
#' Silently ignores any that are already present.
#' @param species Character vector of common names to add.
birdSpeciesAdd <- function(species, path = birdnet_species_list_path) {
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

#' @describeIn birdSpeciesList Remove one or more species from the working
#' list. Silently ignores any that aren't present.
#' @param species Character vector of common names to remove.
birdSpeciesRemove <- function(species, path = birdnet_species_list_path) {
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
