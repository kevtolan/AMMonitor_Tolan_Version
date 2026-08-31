# BirdNET integration

Standalone scripts that add BirdNET (via the [birdnetR](https://birdnet-team.github.io/birdnetR/)
package) as a detection model, replacing the old Python + CSV-import
workflow. These are project scripts, not part of the AMMonitor package
build -- source them manually, same as you would `import_birdnet.R`.

These files are **not** wired into the package's NAMESPACE, so
`library(AMMonitor)` alone will never expose `birdsDetect()` the way it
does `scoresDetect()` -- `source()` is required every session, regardless
of whether you're running from this repo directly or from an installed
copy (`system.file("birdnet", package = "AMMonitor")`).

Day-to-day, keep working from your own editable copy (e.g.
`~/R/AMMonitor_VPMon/`) rather than the one bundled in an installed
package -- an installed package's files get wiped on every reinstall/update,
so it's a bad place to keep something like `birdnet_species_list.csv` that
`birdSpeciesAdd()`/`birdSpeciesRemove()` edit in place. The copy here is
mainly a portable starting point (e.g. setting up on a new machine).

## One-time setup

```r
source("birdnet/Register_BirdNET_Model.R")   # registers BirdNET_v2.4 in the models table
source("birdnet/Register_BirdNET_Species.R") # registers birdnet_species_list.csv species as taxa (via ritis/ITIS TSN lookup)
```

## Usage

```r
source("birdnet/birdSpeciesList.R")
source("birdnet/birdsDetect.R")
# or, from an installed copy:
# source(system.file("birdnet/birdSpeciesList.R", package = "AMMonitor"))
# source(system.file("birdnet/birdsDetect.R", package = "AMMonitor"))

# review first, don't write anything yet
birdscores <- birdsDetect(conx, recordingNames = "some_file.wav", dbInsert = FALSE)

# once you're happy with it
birdsDetect(conx, recordingNames = "all", dbInsert = TRUE, showProgress = TRUE)
```

`birdsDetect()` mirrors `AMMonitor::scoresDetect()`'s calling convention
(`con`, `recordingNames = "all"`, `dbInsert = FALSE`, `showProgress = FALSE`).
Detections are restricted to `birdnet_species_list.csv` by default --
manage it with `birdSpeciesList()` / `birdSpeciesAdd()` / `birdSpeciesRemove()`
in `birdSpeciesList.R`, or pass a one-off `speciesList =` vector to
`birdsDetect()` for a single call, or `speciesList = NA` to disable
filtering entirely (not recommended -- produces geographically implausible
detections).
