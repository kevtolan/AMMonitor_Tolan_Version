# Local customizations

This fork carries local changes on top of upstream AMMonitor `AMMonitor2.2`
(commit `585b4a79`), maintained at
[code.usgs.gov/vtcfwru/ammonitor](https://code.usgs.gov/vtcfwru/ammonitor/-/tree/master).
Changes fall into three groups: new package functions (`R/`), the Shiny app
(`inst/shiny/`), and bug fixes to existing package functions.

## New package functions -- BirdNET integration

Adds [BirdNET](https://birdnet-team.github.io/birdnetR/) as a detection
model via the `birdnetR` package, replacing the project's old Python +
CSV-import workflow (`import_birdnet.R`). This documented was generated using AI.

- **`R/birdsDetect.R`** -- runs BirdNET against recordings and either
  returns detections for review or inserts them into `modeloutputs`.
  Mirrors `scoresDetect()`'s calling convention (`con`/`recordingNames`/
  `dbInsert`/`showProgress`). Supports a managed species list
  (`speciesList`/`speciesListPath`) to restrict detections to expected
  regional species; recordings hosted remotely (S3, etc.) are downloaded to
  a temp file for analysis.
- **`R/birdSpeciesList.R`** -- `birdSpeciesList()`, `birdSpeciesAdd()`,
  `birdSpeciesRemove()`: manage a plain-text CSV species list used by
  `birdsDetect()`'s species filter.
- **`R/registerBirdNETModel.R`** / **`R/registerBirdNETSpecies.R`** --
  one-time (safe to re-run) setup: register a BirdNET model row in
  `models`, and register every species in a species list as a taxon via
  ITIS TSN lookup (`ritis`), with automatic deprecated-TSN resolution and a
  common-name fallback search for anything that doesn't resolve by
  scientific name.

## New package functions -- shared detection-function improvements

Applied to both `scoresDetect()` (upstream) and `birdsDetect()` (above),
since they share a calling convention:

- **`R/reportDetectionSpeed.R`** -- when `showProgress = TRUE`, both
  functions now automatically time themselves and print a `cli`-styled
  summary: recordings processed, total audio duration, elapsed wall-clock
  time, throughput, and how many times faster than real-time the analysis
  ran. Also returned invisibly for programmatic use.
- **Multicore support (`numCores` argument)** -- both functions accept
  `numCores`; when > 1, recordings are split into that many chunks and
  processed concurrently via `parallel::mclapply` (fork-based, Unix/macOS
  only -- falls back to sequential with a warning on Windows).
  `birdsDetect()` has each worker load its own BirdNET model instance
  rather than sharing the parent's, since a loaded TensorFlow Lite
  interpreter isn't guaranteed to survive a fork cleanly. `scoresDetect()`
  has each worker run in its own temporary working directory, since
  monitoR's `binMatch()`/`corMatch()` write a shared `current_audio.wav`
  side-effect file that would otherwise collide across concurrent workers
  (see new internal `scoresDetectParallelChunks()`).

## Bug fixes to existing package functions

- **`R/birdsDetect.R`** -- recordings whose filename contains a space
  produced an S3 URL with a literal unescaped space, which `download.file()` can't fetch (fails
  silently, surfaced as "Could not access recording"). Fixed by
  URL-encoding the remote path before download.
- **`inst/shiny/modules/app_modules/registerVisitUpdateDB.R`** -- media
  uploaded to the root of an S3 bucket (no subfolder) got a double slash in
  its stored `filepath` (e.g. `https://bucket.s3.amazonaws.com//file.WAV`),
  since the destination path already ended in `/` and a second `/` was
  added unconditionally when appending the filename. Fixed by stripping
  any trailing slash from the destination path first.

## Shiny app (`inst/shiny/`)

### `modules/media_tools/audio_player.R`

- Per-recording **Comments** box + save button (new `media.comments` column).
- **Manual Detections** box: defaults to a live model-output count
  (`modeloutputs` with `value_num >= 14`, excluding anything a verifier
  marked invalid, plus manual annotation counts, excluding `no-species`
  tags), overwritable and saved to a new `media.ManualDetx` column.
- **Manual Detections** filter (All / Not yet saved / Already saved).
- Hard cap (`MAX_CACHE_SIZE`, currently 2500) on how many recordings' worth
  of data get batched into one query, regardless of `cache_size.txt` -- an
  unbounded value previously caused an out-of-memory crash.
- Default Spectrogram Frequency Range changed to 0-8 kHz; default
  Spectrogram Length changed to 20s.
- `audio_comment_box_ui()` extracted so the comment/detections box can be
  placed independently of the player (used across Player, Tagger,
  Annotation Verifications, and Model Verifications tabs).
- Fixed a crash in the "jump to selected row" observer when multiple rows
  are selected (`==` replaced with `%in%`; scalar checks now use the first
  selected row explicitly).

### `modules/media_tools/audio_annotator.R`

- Same Manual Detections box as above, added to the Tagger's annotation
  panel. Tracks its own just-saved state locally, since it receives the
  shared metadata cache as a read-only reactive getter rather than a
  `reactiveValues` object.

### `modules/media_tools/annotation_viewer_tables.R`

- "Taxon Model Outputs" table: `model_name` column no longer wraps;
  multi-select enabled for audio (was single-select), with a slightly
  darker highlight on selected rows; click-and-drag range selection across
  rows.
- Flag shown above the table when the current recording has manual (human)
  annotations, so a model-output reviewer knows tagger activity exists on
  this file even though the table itself only shows model detections.

### `modules/app_modules/registerVisitUpdateDB.R`

- New audio recordings registered through Add Visit automatically get
  AudioMoth WAV comment metadata parsed and stored: `recorded_datetime_utc`,
  `recorded_datetime_local`, `device_serial`, `gain_setting`,
  `battery_voltage`, `temperature_c` (all new `media` columns).

### `ui.R`

- Wires `audio_comment_box_ui()` into all four Audio sub-tabs.

## Related database schema changes (not in this repo)

Live SQLite schema changes needed for the above, made via
`AMMonitor::dbAddUserCol()` and registered in `dbdictionary`:
`media.comments`, `media.ManualDetx`, `media.recorded_datetime_utc`,
`media.recorded_datetime_local`, `media.device_serial`,
`media.gain_setting`, `media.battery_voltage`, `media.temperature_c`. A
`BirdNET_v2.4` model row is added per-database via `registerBirdNETModel()`.
