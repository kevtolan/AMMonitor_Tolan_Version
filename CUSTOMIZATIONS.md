# Local customizations

This fork carries local changes on top of upstream AMMonitor `AMMonitor2.2`
(commit `585b4a79`), maintained at
[code.usgs.gov/vtcfwru/ammonitor](https://code.usgs.gov/vtcfwru/ammonitor/-/tree/master).
Changes fall into three groups: new package functions (`R/`), the Shiny app
(`inst/shiny/`), and bug fixes to existing package functions. This documented was generated using AI.

## New package functions -- BirdNET integration

Adds [BirdNET](https://birdnet-team.github.io/birdnetR/) as a detection
model via the `birdnetR` package, replacing the project's old Python +
CSV-import workflow (`import_birdnet.R`). 

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
- **`birdsDetect()` no longer writes a `"no-species"` placeholder row** to
  `modeloutputs` when BirdNET finds nothing for a recording -- it now
  stores nothing at all for that recording. `"no-species"` as a taxon tag
  is reserved for manual annotation (a human confirming the absence of a
  call in the Tagger), not an automated model result. Re-running
  `birdsDetect()` on a recording that previously had zero detections will
  re-analyze it (no db row exists to mark it as already-checked-and-empty)
  -- accepted tradeoff, re-analysis is cheap relative to correctness of the
  taxon tag's meaning.
  - **`R/qry.R`'s `qryModelOutputsMedia()`** (the query behind the "Model
    Verifications" recording browse list) uses an `INNER JOIN` on
    `modeloutputs`, same as upstream -- only media with at least one actual
    model detection is browsable there. (Briefly changed to a `LEFT JOIN`
    so zero-detection recordings stayed reachable/verifiable as
    "checked, nothing found," but reverted: browsing model outputs should
    only surface media with real detections, not the majority of
    recordings that have none.)

## New package functions -- shared detection-function improvements

Applied to both `scoresDetect()` (upstream) and `birdsDetect()` (above),
since they share a calling convention:

- **`R/reportDetectionSpeed.R`** -- when `showProgress = TRUE`, both
  functions now automatically time themselves and print a `cli`-styled
  summary: recordings processed, total audio duration, elapsed wall-clock
  time, throughput, and how many times faster than real-time the analysis
  ran. Also returned invisibly for programmatic use.
- **`birdsDetect()` progress output** -- prints a "Recording X of N" line
  (X being the recording's true position in the full requested set, even
  when split across parallel workers) before each file's BirdNET progress
  bar, when `showProgress = TRUE`.
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
- **`inst/shiny/modules/media_tools/audio_player.R`** -- same space-in-URL
  issue as `birdsDetect()` above, but in the `fullAudio()` reactive that
  loads a recording for the spectrogram/waveform view: `download.file()`
  failed with "cannot open URL" for any file whose name contains a space,
  even though the URL itself was valid and public. Fixed the same way, by
  URL-encoding the path before download.
- **`inst/shiny/modules/media_tools/audio_player.R`** -- a client-side JS
  race: a standalone `observe()` referenced the `myAudio` audio-element
  variable (defined inside a separate `renderUI`-generated `<script>` tag)
  without checking it had actually been created yet. Whenever that observer
  fired first, the browser threw `ReferenceError: myAudio is not defined`,
  which halted Shiny's client-side reactivity for the rest of the session --
  surfacing as "nothing loads" (filters, tables, detections all appear
  empty) even though the server side was working fine. Fixed by guarding
  the script with `if (typeof myAudio !== "undefined" && myAudio) { ... }`.
  Also hardened `fullAudio()` so a failed/slow download quietly suspends
  that one reactive (`req(FALSE)`) instead of throwing and taking every
  dependent output (spectrogram, waveform, frequency filters) down with it.
- **`inst/shiny/modules/media_tools/annotation_viewer_tables.R`** -- the
  Start/End time range-filter inputs on the Taxon Model Outputs table
  computed `ceiling(max(values))` with no guard for an empty/all-NA column
  (e.g. a recording whose only detections are `no-species`, where
  `x_min`/`x_max` are `NA`), which could produce `NA`/`-Inf` as the input's
  `max` attribute. Guarded against empty/all-NA `values`. Also added
  `ignoreInit = TRUE` to the `updateReactable()` observer tied to the same
  table, since it otherwise fires on the very first reactive flush too, at
  the same moment as the table's own initial render.
  **Known open issue**: independent of the two fixes above, this table can
  still intermittently throw a client-side React error
  (`Cannot read properties of null (reading 'hasOwnProperty')`) together
  with Shiny "output ... is in an unexpected state" desync errors, on this
  fork's current test data. Root cause not yet isolated -- confirmed NOT
  caused by `updateReactable()` (still reproduces with that observer fully
  disabled), so the issue is inside `renderReactable()`/`output$taxon_table`
  itself. Needs further investigation with real production data.

## Shiny app (`inst/shiny/`)

### `modules/media_tools/audio_player.R`

- Per-recording **Comments** box + save button (new `media.comments` column).
- **Manual Detections** box: defaults to a live model-output count
  (`modeloutputs` with `value_num >= 14`, excluding anything a verifier
  marked invalid, plus manual annotation counts, excluding `no-species`
  tags), overwritable and saved to a new `media.ManualDetx` column.
- **Manual Detections** filter (All / Not yet saved / Already saved).
- Hard cap (`MAX_CACHE_SIZE`, currently 10000) on how many recordings' worth
  of data get batched into one query, regardless of `cache_size.txt` -- an
  unbounded value previously caused an out-of-memory crash. Cache size
  input defaults to 2500.
- **Select Taxa** and **Select a Location** filters are searchable (type to
  filter) instead of plain scrolling dropdowns (`selectInput` ->
  `selectizeInput`).
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
