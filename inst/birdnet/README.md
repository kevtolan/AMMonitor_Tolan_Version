# BirdNET integration

Adds BirdNET (via the [birdnetR](https://birdnet-team.github.io/birdnetR/)
package) as a detection model, replacing the old Python + CSV-import
workflow.

`birdsDetect()`, `birdSpeciesList()`, `birdSpeciesAdd()`, `birdSpeciesRemove()`,
`registerBirdNETModel()`, and `registerBirdNETSpecies()` are all real exported
AMMonitor functions -- `library(AMMonitor)` is all you need, no `source()`
required. See `?birdsDetect`, `?birdSpeciesList`, `?registerBirdNETModel`, and
`?registerBirdNETSpecies`.

One-time setup, safe to re-run (skips anything already registered):

```r
registerBirdNETModel(conx)                                              # registers BirdNET_v2.4 in the models table
registerBirdNETSpecies(conx, speciesListPath = "birdnet_species_list.csv") # registers your species list as taxa (via ritis/ITIS TSN lookup)
```

Both take `con` as an explicit argument, same as every other AMMonitor
function -- point them at whichever database connection you have open. No
database path is hardcoded anywhere in these functions, so it's safe to run
against a trial/scratch database without risk of silently touching a
different one.

## Getting your own species list

A starter list (247 species, US Northeast) ships at
`system.file("extdata/birdnet_species_list.csv", package = "AMMonitor")`.
Copy it to your own project directory before editing -- same convention as
`modelAdd()`'s Excel template:

```r
fp <- system.file("extdata/birdnet_species_list.csv", package = "AMMonitor")
file.copy(from = fp, to = "birdnet_species_list.csv", overwrite = FALSE)
```

`birdSpeciesAdd()`/`birdSpeciesRemove()` edit that file in place -- don't
point them at the copy inside an installed package, it'll be wiped on the
next reinstall/update.

## Usage

```r
library(AMMonitor)

# review first, don't write anything yet
birdscores <- birdsDetect(
  con = conx,
  recordingNames = "some_file.wav",
  speciesListPath = "birdnet_species_list.csv",
  dbInsert = FALSE
)

# once you're happy with it
birdsDetect(
  con = conx,
  recordingNames = "all",
  speciesListPath = "birdnet_species_list.csv",
  dbInsert = TRUE,
  showProgress = TRUE
)
```

`birdsDetect()` mirrors `AMMonitor::scoresDetect()`'s calling convention
(`con`, `recordingNames = "all"`, `dbInsert = FALSE`, `showProgress = FALSE`).
Pass a one-off `speciesList =` character vector instead of `speciesListPath`
for a single call without touching any file, or `speciesList = NA` to
disable filtering entirely (not recommended -- produces geographically
implausible detections).
