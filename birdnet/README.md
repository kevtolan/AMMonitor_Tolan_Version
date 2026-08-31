# BirdNET integration

Standalone scripts that add BirdNET (via the [birdnetR](https://birdnet-team.github.io/birdnetR/)
package) as a detection model, replacing the old Python + CSV-import
workflow. These are project scripts, not part of the AMMonitor package
build -- source them manually, same as you would `import_birdnet.R`.

## One-time setup

```r
source("birdnet/Register_BirdNET_Model.R")   # registers BirdNET_v2.4 in the models table
source("birdnet/Register_BirdNET_Species.R") # registers birdnet_species_list.csv species as taxa (via ritis/ITIS TSN lookup)
```

## Usage

```r
source("birdnet/birdSpeciesList.R")
source("birdnet/birdsDetect.R")

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
