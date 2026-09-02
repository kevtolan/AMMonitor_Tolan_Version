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
- **`birdsDetect()` start-time banner** -- when `showProgress = TRUE`,
  prints `Starting birdsDetect() at <timestamp>` as the very first thing
  the function does (before even the `birdnetR` install check), so a long
  run's start time is visible at the top of the console output rather
  than only inferable from the final speed summary.
- **`birdsDetect()` progress output** -- prints an "M/N filename" line
  after each recording finishes, when `showProgress = TRUE`, where M is a
  running count of recordings *completed so far* (shared across all
  workers), not that recording's position in the requested list. An
  earlier version reported position instead; under `numCores > 1` that
  printed out of order (e.g. `1, 4, 6, 9, ...` -- the first index of each
  worker's own chunk) since independent workers finish at their own pace,
  which read as broken even though each number was individually correct.
  The completed-count is tracked via a shared temp file every worker
  appends one line to per finished recording (`file.create()`'d before
  `mclapply`, cleaned up via `on.exit()`); a worker's reported M is
  `length(readLines(...))` right after its own append. A single small
  append (one line, well under `PIPE_BUF`) is an atomic write on POSIX, so
  concurrent workers can't corrupt each other's entries without explicit
  locking -- verified under 8 concurrent workers: correct total, no
  corruption, occasional duplicate/skipped M by 1 from the unlocked
  read-after-append race, acceptable for a cosmetic progress line. Prints
  by opening `"/dev/stderr"` by its literal path (Unix only) rather than
  `cat()`/`message()` to R's `stdout()`/`stderr()` connections: under
  `numCores > 1` this runs inside a forked `mclapply()` worker, and in
  RStudio neither `cat()` nor `message()` from a forked child ever reaches
  the console -- both go through R's own connection/callback layer, which
  only the main (non-forked) session is wired up to. BirdNET's Python tqdm
  bar (`Predicting species: 100%|...`) shows up fine because it writes
  straight to the OS file descriptor with no R connection involved;
  opening `/dev/stderr` by path does the same raw write from R's side,
  bypassing R's connection layer entirely. Falls back to `message()` on
  non-Unix (where forking -- and thus this problem -- doesn't happen
  anyway).
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
- **`birdsDetect()` silences the Python `resource_tracker` leaked-semaphore
  warning** (`UserWarning: resource_tracker: There appear to be N leaked
  semaphore objects...`) printed at Python interpreter shutdown -- harmless,
  but noisy, especially with `numCores > 1` where it can print once per
  forked worker. Sets `PYTHONWARNINGS` both via `Sys.setenv()` (so a fresh
  Python interpreter, including each `mclapply` fork, picks it up at
  startup) and directly in Python's `os.environ` via `reticulate::py_run_string()`
  when Python is already running (since `PYTHONWARNINGS` is otherwise only
  read once at interpreter startup, and BirdNET's Python interpreter
  persists across repeated `birdsDetect()` calls in the same R session).
  Scoped narrowly to `UserWarning`s from `multiprocessing.resource_tracker`
  only, so other Python warnings still surface normally.

## Bug fixes to existing package functions

- **`inst/shiny/modules/app_modules/registerVisitUpdateDB.R`** -- the
  AudioMoth WAV comment parser only recognized one narrow comment format
  and silently produced all-NA metadata for anything else. Real AudioMoth
  firmware comment text varies (see
  [metamoth's firmware history](https://metamoth.readthedocs.io/en/latest/firmwares.html)):
  "gain" vs. "gain setting", "battery was" vs. "battery state was", an
  optional timezone offset in the UTC parenthetical, no temperature field
  on older firmware, and -- most significantly -- firmware 1.5.0+ replaces
  the "by AudioMoth &lt;serial&gt;" clause entirely with "during deployment
  &lt;id&gt;" when a deployment ID is configured, rather than adding to it.
  Reworked the regex to accept all of these; verified against real
  documented comment strings for each variant.
- **`inst/shiny/modules/app_modules/registerVisitUpdateDB.R`** -- adding
  media to a *pre-existing* visit crashed immediately on `Add the new
  visit`: the status-message builder referenced `rs$message`, but `rs` is
  only ever assigned in the *other* branch (creating a brand-new visit).
  Selecting an existing visit hit an undefined-variable error before any
  media could be added. Removed the stray reference.
- **`inst/shiny/modules/app_modules/registerVisitUpdateDB.R`** -- the
  AudioMoth-metadata timezone lookup built its `SELECT tz FROM locations
  WHERE pk_locationid = '...'` query via raw string concatenation, so a
  location name containing an apostrophe (e.g. "Smith's Field") broke the
  SQL and threw `near "s": syntax error`, blocking Add Visit entirely for
  audio at that location. Switched to a parameterized query
  (`params = list(...)`), which also closes the door on this for any
  future location/site name with a quote in it.
- **`R/scoresDetect.R`** -- `scoreThresholds` was matched by name against
  template names; an unnamed vector (`names(scoreThresholds)` is `NULL`)
  matched nothing and was silently ignored -- no error, no warning, the
  function just ran with the templates' own built-in cutoffs instead.
  Reworked to accept three forms: a single number (applied to every
  template), a named vector (matched by name, as before), or an unnamed
  vector with exactly one value per template (applied by position, in
  `monitoR::templateNames()` order -- matching what the original
  docs already claimed but the code never actually did). Any name that
  doesn't match a template, or a vector of some other length, now warns
  explicitly instead of failing silently.
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
- **Manual Detections** box: defaults to a live model-output count (every
  non-invalidated `modeloutputs` row for the recording -- presence in the
  table already means it cleared whatever threshold the model was run
  with, plus manual annotation counts, excluding `no-species` tags),
  overwritable and saved to a new `media.ManualDetx` column. On the Model
  Verifications page, the count also respects the live "Model
  Value"/"Less than Value" filter so it matches the table shown below it.
  (Originally hardcoded a `value_num >= 14` cutoff meant for monitoR's
  score scale; this silently zeroed the count for BirdNET, whose
  confidence values run 0-1 -- fixed here and in the matching copy in
  `audio_annotator.R`.)
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
- `audio_avail` (the filtered recording list) no longer auto-loads with
  blank/default filters on tab open. Previously an `eventReactive`
  triggered by an `audio_on_startup` flag, it now fires only on the "Apply
  Filters" button (`ignoreInit = TRUE`) -- fixes a slow, unwanted
  spectrogram render for an arbitrary recording every time Model
  Verifications (or any other audio_player-based tab) is opened, before
  the user has chosen a filter.
- `cache_size.txt` is now read with `readLines()` instead of `read.csv()`,
  so a file without a trailing newline (the common case when it's been
  hand-edited) no longer prints an "incomplete final line" warning on
  every app start. Same change in `image_viewer.R`.

### `modules/media_tools/audio_annotator.R`

- Same Manual Detections box as above, added to the Tagger's annotation
  panel. Tracks its own just-saved state locally, since it receives the
  shared metadata cache as a read-only reactive getter rather than a
  `reactiveValues` object.

### `modules/media_tools/image_viewer.R`

- Same fix as `audio_player.R` above: `photos_avail` now only loads on
  "Apply Filters" (`ignoreInit = TRUE`) instead of also firing once on tab
  open with blank/default filters.

### `modules/media_tools/annotation_viewer_tables.R`

- "Taxon Model Outputs" table: `model_name` column no longer wraps;
  multi-select enabled for audio (was single-select), with a slightly
  darker highlight on selected rows; click-and-drag range selection across
  rows.
- Flag shown above the table when the current recording has manual (human)
  annotations, so a model-output reviewer knows tagger activity exists on
  this file even though the table itself only shows model detections.
- Fixed a crash in that same manual-annotation flag: `metadata_cache()`'s
  `annotations` field starts out as a bare `NA` (not a data.frame) until
  the audio player's own cache-population observer has run at least once
  for the newly-selected recording. Since Shiny doesn't guarantee that
  observer runs before this flag's `renderUI` on the same reactive flush,
  reading `NA$fk_mediaid` off it could throw `$ operator is invalid for
  atomic vectors` when opening Model Verifications (or right after
  pressing Apply Filters). Now guarded with
  `req(is.data.frame(metadata_cache()$cache$annotations))`.

### `modules/app_modules/registerVisitUpdateDB.R`

- New audio recordings registered through Add Visit automatically get
  AudioMoth WAV comment metadata parsed and stored: `recorded_datetime_utc`,
  `recorded_datetime_local`, `device_serial`, `gain_setting`,
  `battery_voltage`, `temperature_c` (all new `media` columns).

### `modules/app_modules/registerVisitMediaType.R`

- "Select Media Type" defaults to `audio` instead of `photo`.

### `modules/app_modules/registerVisitMetadata.R`

- "Select existing visit" shows location and visit date alongside the
  visit ID (`"1 -- Site_A2024 -- 2025-02-27"`) instead of a bare
  visit number, so it's actually possible to tell visits apart when
  picking one to add media to. The underlying selected value is still
  just the plain `pk_visitid`.

### `ui.R`

- Wires `audio_comment_box_ui()` into all four Audio sub-tabs.

### `server.R`

- Fixed a fatal regression from the `audio_avail`/`photos_avail`
  `ignoreInit = TRUE` fix above: the "run once when this tab is first
  opened" blocks (one per Photos/Audio sub-tab, e.g.
  `audio_model_verifier_loaded`) gated *when* they first proceeded past
  their `req()`, but nothing stopped them from running *again* later if
  Shiny re-triggered the observer -- which re-called
  `annotation_viewer_tables_server()`/`audio_player_server()` /
  `image_viewer_server()` a second time with the same module id
  (unsupported by `moduleServer()`), surfacing as `object
  '..._player_output' not found` elsewhere in the app. Restructured each
  to `req(isolate(loaded_flag()) == FALSE)` so every run after the first
  is a true no-op.

### `modules/media_tools/audio_player.R` and `modules/media_tools/image_viewer.R`

- Fixed the actual root cause of the above regression report (a second,
  independent bug, not the `server.R` one): a **top-level** `if
  (isolate(nrow(audio_avail())) != 0) { ... }` /
  `if (isolate(nrow(photos_avail())) != 0) { ... }` ran once, eagerly,
  during each module's own setup (not inside any `observe()`/`reactive()`
  wrapper) to sync the date-range filter if the recording list already had
  data at startup. Since `audio_avail()`/`photos_avail()` no longer load
  anything until Apply Filters is pressed, this now *always* reads an
  eventReactive that has never fired, which throws a `shiny.silent.error`
  (same condition class `req(FALSE)` raises) -- and because this code sat
  outside any reactive wrapper, nothing caught it: it aborted
  `audio_player_server()`/`image_viewer_server()`'s entire setup call
  before it returned, which is what left `..._player_output` undefined
  for every downstream module that referenced it. Removed the now-dead
  block entirely (the equivalent logic inside the `observeEvent` right
  below it already handles the meaningful case -- updating the date range
  when the user picks a new location -- safely, since `observeEvent` has
  its own silent-error boundary).
- Also hardened the two "Update the metadata cache" blocks' `nrow(...) !=
  0` checks with a preceding `is.data.frame(...)` check:
  `nrow(metadata_cache$cache$mediaMetaData)` is `NULL` (not `0`) while the
  cache is unpopulated, so `NULL != 0` is `logical(0)` -- which does not
  reliably short-circuit the rest of the `&&` chain and could still reach
  a `[` subset against the cache's other still-NA placeholder fields,
  throwing "incorrect number of dimensions". `is.data.frame(NA)` is a
  clean `FALSE`, which `&&` does short-circuit on.
- (audio only) `metadata_cache`'s `i_cache_start`/`i_cache_end` now
  default to `1`/`0` (an empty range) instead of `NA`, for the same
  reason: an unguarded `if (i_audio() < i_cache_start || i_audio() >
  i_cache_end)` elsewhere threw "missing value where TRUE/FALSE needed"
  as soon as anything read `i_audio()` before the cache populated.
  `i_cache_end` already doubles as an "is the cache populated yet" flag
  elsewhere (`i_cache_end != 0`), so `0` is the semantically correct
  empty-state value. Same fix applied to `image_viewer.R`'s copy.
- `save_metadata_cache()` (in `save_metadata_cache.R`, shared by both
  files) now returns immediately if `metadata_cache$cache$mediaMetaData`
  isn't a data.frame yet, guarding a call site
  (`audio_player.R`/`image_viewer.R`'s `save_metadata_now` observer) that
  had no other check before unconditionally trying to flush the cache.
- All of the above were found and fixed by actually launching the app
  (`launchApp()` against the trial database) and reproducing the crash in
  a browser after every candidate fix, rather than reasoning about the
  reactive graph in the abstract -- the first two fixes attempted (before
  the top-level `isolate(audio_avail())` block was found) resolved
  different, real, but insufficient parts of the failure.

## Related database schema changes (not in this repo)

Live SQLite schema changes needed for the above, made via
`AMMonitor::dbAddUserCol()` and registered in `dbdictionary`:
`media.comments`, `media.ManualDetx`, `media.recorded_datetime_utc`,
`media.recorded_datetime_local`, `media.device_serial`,
`media.gain_setting`, `media.battery_voltage`, `media.temperature_c`. A
`BirdNET_v2.4` model row is added per-database via `registerBirdNETModel()`.
