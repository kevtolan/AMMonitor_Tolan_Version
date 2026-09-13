# Local customizations

This fork carries local changes on top of upstream AMMonitor `AMMonitor2.2`
(commit `585b4a79`), maintained at
[code.usgs.gov/vtcfwru/ammonitor](https://code.usgs.gov/vtcfwru/ammonitor/-/tree/master).
Changes fall into three groups: new package functions (`R/`), the Shiny app
(`inst/shiny/`), and bug fixes to existing (pre-fork) code in either. 

This document was generated using AI.

## New package functions -- BirdNET integration

Adds [BirdNET](https://birdnet-team.github.io/birdnetR/) as a detection
model via the `birdnetR` package, replacing the project's old Python +
CSV-import workflow (`import_birdnet.R`). 

- **`R/birdsDetect.R`** -- runs BirdNET against recordings and either
  returns detections for review or inserts them into `modeloutputs`.
  Mirrors `scoresDetect()`'s calling convention (`con`/`recordingNames`/
  `dbInsert`/`showProgress`). Supports a managed species list
  (`speciesList`/`speciesListPath`) to restrict detections to expected
  regional species; recordings hosted remotely (S3, etc.) are downloaded
  to a temp file for analysis, URL-encoding the remote path first so a
  filename containing a space downloads correctly.
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
- **`birdsDetect()` progress output** -- when `showProgress = TRUE`,
  prints an "M/N filename" line as each recording finishes, where M is a
  running count of recordings completed so far across all workers (a
  recording's position in the requested list isn't used, since that
  doesn't print in a useful order once the list is split across parallel
  workers). The completed-count is tracked via a shared temp file every
  worker appends one line to per finished recording (`file.create()`'d
  before `mclapply`, cleaned up via `on.exit()`); a worker's reported M is
  `length(readLines(...))` right after its own append. A single small
  append (one line, well under `PIPE_BUF`) is an atomic write on POSIX, so
  concurrent workers don't corrupt each other's entries without needing
  explicit locking. Prints by opening `"/dev/stderr"` by its literal path
  (Unix only) rather than through R's own `stdout()`/`stderr()`
  connections, since under `numCores > 1` this runs inside a forked
  `mclapply()` worker, and in RStudio neither `cat()` nor `message()` from
  a forked child reaches the console (both go through R's
  connection/callback layer, which only the main session is wired up to).
  BirdNET's Python tqdm bar (`Predicting species: 100%|...`) shows up fine
  because it writes straight to the OS file descriptor with no R
  connection involved; opening `/dev/stderr` by path does the same raw
  write from R's side. Falls back to `message()` on non-Unix (where
  forking doesn't happen anyway).
- **`scoresDetect()`'s parallel path never actually got the progress-output
  fix above** -- `scoresDetectParallelChunks()`'s per-recording loop had
  its own plain `cat(filename, "\n")`, which -- being the same
  forked-`mclapply`-worker output that motivated the `/dev/stderr` trick
  for `birdsDetect()` in the first place -- silently produced no visible
  output at all under `numCores > 1` (confirmed live: only the final
  `reportDetectionSpeed()` summary showed up, nothing during the run).
  Replaced with the identical shared-ticker-file + `/dev/stderr` pattern
  used by `birdsDetect()`, so `numCores > 1` now behaves the same as that
  function. `numCores = 1` was never affected -- it already uses
  `utils::txtProgressBar()`, which runs in the main process and was never
  silenced.
- **Fixed a crash in both functions' parallel-worker-failure warning**
  (`... parallel worker(s) failed and were skipped: ...`), found while
  live-testing the fix above against a deliberately-broken S3 URL: a
  `try-error` returned by `mclapply()` on a failed worker is itself a
  character string (the printed `"Error in ... : message"`), not a
  condition object, so `conditionMessage(worker_out[failed])` -- called
  directly on it -- errored with `"no applicable method for
  'conditionMessage'"`, masking whatever the real per-worker failure was.
  Fixed by pulling the actual condition out of its `"condition"`
  attribute first: `conditionMessage(attr(x, "condition"))`. Verified live
  against both the real, transient failure that surfaced it (an S3
  download timeout) and a deliberately-unreachable URL, confirming the
  real underlying error now displays correctly in both functions instead
  of crashing.
- **`scoresDetect()`: fixed a second crash when *every* parallel worker
  fails** -- after the fix above filters all-failed workers out of
  `worker_out`, an empty `worker_out` made `do.call(rbind, ...)` return
  `NULL`, and the next line's `attr(scores, "total_duration_sec") <-`
  then failed with `"attempt to set an attribute on NULL"` -- instead of
  the warning above (which does still fire) actually explaining what went
  wrong. Added an explicit check that raises a clear
  `stop("All parallel workers failed; ...")` in that case.
  `birdsDetect()`'s analogous downstream code already tolerated an empty
  `worker_out` (`sum()` of an empty numeric vector is `0`, not an error,
  and its own `results`-is-NULL check already returns cleanly), so it
  didn't need the equivalent guard.
- **`scoresDetect()`: one bad recording no longer sacrifices every other
  recording around it.** The audio download/read step (`utils::download.file()`
  + `tuneR::readWave()`) was the one part of the per-recording loop *not*
  already wrapped in a `tryCatch` -- binary- and correlation-template
  matching each already had their own, per recording, independently. So a
  single failed download used to be fatal: in the sequential path
  (`numCores = 1`), it aborted the *entire* call immediately, losing every
  recording already scored before it too, since `dbInsert` only happens
  once at the very end on the accumulated results; in the parallel path,
  it took down that worker's *entire chunk* (~1/`numCores` of the request),
  not just the one bad file, since an uncaught error partway through
  `process_chunk()` means it never reaches its own `return`. This is
  exactly what surfaced live: a user's `numCores = 8` run of 95 recordings
  lost 2 whole chunks (~24 recordings) to 2 individual S3 download
  failures. Wrapped the download/read step in its own `tryCatch` in both
  the sequential loop and `process_chunk()`, matching the existing
  per-template-type pattern: on failure, `message()` which file and why,
  record it, and move on to the next recording (`next`) instead of
  aborting. Failures are collected into a `filename`/`stage`/`message`
  data.frame -- summarized in one `warning()` when the run finishes, and
  also attached as the `"failed_recordings"` attribute on the returned
  scores (`attr(scores, "failed_recordings")`) so exactly which recordings
  need reprocessing is directly queryable, no more reconstructing
  `parallel::splitIndices()` by hand to guess which chunk died. Verified
  with a battery of live tests against a scratch copy of a real database
  (two fake, guaranteed-to-404 recordings registered in `media` solely for
  the test) covering both `numCores = 1` and `numCores = 2` with failures
  interleaved *within* the same chunk as a good recording: in every case,
  the good recording(s) were still scored and the failure(s) reported by
  name and reason, with no crash and nothing silently lost.
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

## New package functions -- classifier threshold sensitivity

- **`R/testThresholdRec.R`** (originally `testThreshold()`, renamed once
  its detection-level sibling below was added) -- sweeps a range of
  `modeloutputs` score thresholds for a given `taxon_id`/`model_ids`, and
  at each one computes a **recording-level** confusion matrix (TP/FP/TN/FN
  -- a whole recording is one unit, regardless of how many individual
  detections it holds) plus precision, recall, F1, accuracy, and error
  rate, so a template/model's score threshold can be picked by seeing
  exactly where the precision/recall tradeoff lands across candidate
  cutoffs. Tagged `@family classifier` alongside `plotVerifications()`.
  Ground truth per recording, in priority order: `media.ManualDetx` (a
  manual detection-count override some projects add to their `media`
  table -- not part of the standard schema, so only used if
  `DBI::dbListFields()` shows the column actually exists in this database)
  if not NA; otherwise, for a recording with a qualifying modeloutput,
  whether any of them were accepted in `modelverifications` (unverified
  or verified valid, not all rejected); otherwise, for a recording with
  none, a manual `annotations` entry for `taxon_id` (a false negative) or
  `"no-species"` (a true negative). Recordings matching none of these are
  excluded from that threshold's confusion matrix. `thresholds` defaults
  to 17 evenly-spaced values spanning the observed score range for that
  taxon/model set, since score scales vary widely across model types
  (e.g. 0-1 BirdNET confidence vs. tens for monitoR binary-template
  scores) -- no single hardcoded default range would make sense across
  models. Originally written ad hoc for Wood Frog templates in a personal
  analysis script (`myfunctions.R`/`testThreshold()`, hardcoded to
  `taxon = "Wood Frog"`/`model_ids = c(4, 5)`, using dplyr); integrated
  here as a general package function with `model_ids`/`taxon_id` as
  required arguments, rewritten in base R (`merge`/`tapply`) since
  dplyr/tidyr aren't package dependencies (only ggplot2 already is).
  Verified against three real/demo databases (VPMon_AMM's Wood Frog
  templates, VCE_AMm_DB_trial's BirdNET-scored American Bittern, and the
  package's own `ammCreateMiniDemo()` fixture) to confirm correct behavior
  across different score scales and with/without the `ManualDetx` column
  present. The precision/recall/F1 lines use a fixed
  `scale_color_manual()` palette (`precision = "#F6511D"`,
  `recall = "#FFB400"`, `f1 = "#00A6ED"`) instead of ggplot2's default
  discrete hues, per request.
- **`R/testThresholdDetx.R`** -- same idea, but at the **detection**
  level: each individual `modeloutputs` row (one scored time window) is
  its own unit, rather than a whole recording. A qualifying detection
  (score >= threshold) is a TP if verified valid, an FP if verified
  invalid, and excluded entirely if unverified -- unlike
  `testThresholdRec`, an unverified individual detection is *not* assumed
  correct, since assuming every unverified call is right would inflate TP
  with no real evidence behind it. FN comes from a source independent of
  modeloutputs entirely: each `annotations` row of `taxon_id` is its own
  real event with its own time window, and counts as missed if no
  accepted modeloutput (score >= threshold, unverified or verified valid)
  in the same recording overlaps its `x_min`/`x_max`. No true negative or
  accuracy/error rate is reported here -- unlike a whole recording, an
  empty stretch of time with no detection and no annotation isn't a
  discrete, countable event, so TN has no natural detection-level
  definition. `media.ManualDetx` isn't used here either, for the same
  reason: it's a whole-recording count with no per-event time window, so
  it has no role at the detection level. Confirmed against VPMon_AMM's
  Wood Frog templates (recall is meaningfully lower than
  `testThresholdRec`'s at the same thresholds, exactly as expected: a
  recording can have an accepted detection *somewhere* in it without that
  detection actually overlapping a given real call, which
  `testThresholdRec`'s whole-recording ground truth can't distinguish but
  this can) and the package's own `ammCreateMiniDemo()` fixture (including
  the zero-detections-at-a-threshold edge case, which correctly returns
  `NaN` precision/recall rather than erroring).

## Bug fixes to existing package functions

- **`inst/shiny/modules/app_modules/registerVisitUpdateDB.R`** -- adding
  media to a *pre-existing* visit crashed immediately on `Add the new
  visit`: the status-message builder referenced `rs$message`, but `rs` is
  only ever assigned in the *other* branch (creating a brand-new visit).
  Selecting an existing visit hit an undefined-variable error before any
  media could be added. Removed the stray reference.
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
- On the Model Verifications page specifically, **Select Taxa is
  multi-select**, to filter to a species group (e.g. every owl species
  plus Eastern Whip-poor-will) in one go rather than one taxon at a time.
  Selectize's default search already fuzzy-matches typed text against
  every species' common name, so typing "owl" surfaces all owl species
  with no extra grouping data needed. Empty selection means no filter
  (same meaning the dedicated "all" choice has in the other, still
  single-select, modes). `qryModelOutputsMedia()`'s `taxonID` parameter
  now accepts a vector for this (builds a SQL `IN (...)` clause; a single
  value still works exactly as before). Also switched several `ifelse()`
  calls that read `input$filterTaxa` to plain `if`/`else`: `ifelse()`'s
  result length follows its *test* argument, not its "yes"/"no" arguments,
  so it would have silently kept only the first selected taxon once
  multi-select was possible.
- Default Spectrogram Frequency Range changed to 0-8 kHz; default
  Spectrogram Length changed to 30s (originally 20s).
- Player/spectrogram section widened to the full page width
  (`column(10, ...)` -> `column(12, ...)`). It sat directly below the
  "Recording Filters & Settings" box, which already spans the full
  `width = 12` row on its own -- as the next sibling in that same outer
  `fluidRow`, the player column wrapped to a fresh row of its own instead
  of sharing space with anything, so its old `width = 10` just left 2 of
  12 grid columns (about a sixth of the page) unused on the right for no
  reason. The spectrogram/waveform `plotOutput()`s were already
  `width = "100%"` of their container, so this widens them too.
- Spectrogram plots (`plot_bg`/`plotline`/`plotx`, the three overlaid
  `plotOutput()`s inside the `.large-plot` div -- background image, line
  overlay, interactive click/hover layer) made slightly taller, 300px ->
  350px. Updated all four `plotOutput(height = ...)` calls *and* the
  `.large-plot` CSS class's own `height: 300px` together -- the plots are
  `position: absolute` inside that div, so its height is what actually
  clips/sizes them on screen regardless of their own declared height;
  changing one without the other would have left them visually
  mismatched. Waveform plots (`.wave-plot`, 200px) left as-is.
- Spectrogram/waveform plot axis titles and tick text (`"Time (s)"`,
  `"Frequency (kHz)"`, `" Relative Amplitude"`, and their numbers) set
  to white (`axis.title`/`axis.text` in each `theme()`, across
  `plot_wave`, `plotline_wave`, `plot_bg`, `plotline`, and `plotx`) --
  needed once the dark theme (`ui.R`, above) made the page background dark:
  these are rendered server-side as ggplot images with `bg =
  "transparent"`, so the axis labels sit directly on the surrounding
  page (not a plot background), and ggplot's default axis text color
  (near-black) had become unreadable. Same change in `image_viewer.R`
  isn't needed -- photos have no axes to label.
- The spectrogram (`plotx`) can overlay faint boxes showing what the
  *other side* already found in this window -- existing model outputs on
  Tagger, existing manual annotations on Model Verifier -- each labelled
  just above the box (`"Wood Frog (13.42)"` on Tagger, the score; `"Wood
  Frog (person1)"` on Model Verifier, who annotated it, since a human
  annotation has no numeric confidence to show). Lets whoever's reviewing
  see at a glance that the other side already flagged/confirmed this
  call before adding a new one over the same detection, rather than
  double-counting it. Built from `reference_rects`/`reference_labels`,
  filtered by `fk_mediaid`/time/frequency from `metadata_cache$cache$
  modeloutputs` (Tagger) or `metadata_cache$cache$annotations`
  (Model Verifier, also excluding `is_delete == 1`), with the same
  shift-by-`startTime()` treatment as the manual-annotation `rects2`/
  pending `rects` boxes it's drawn alongside. Deliberately kept as its
  own `geom_rect`/`geom_text` layer rather than merged into
  `current_taxon_annotations()` -- read-only reference, not something
  meant to be selected/edited/deleted the way each mode's own primary
  boxes are.
  Toggled by the existing "Show Model Outputs" checkbox
  (`checkboxInput('viewModelOutputs', ...)`), previously only rendered
  for `viewer_mode == "viewer"` (where checking it does something
  different -- merges model outputs directly into
  `current_taxon_annotations()` for display in that mode's own table/
  plot) -- extended to also render for `"tagger"` and `"modelOutputs"`,
  relabelled `"Show Manual Annotations"` for the latter since checking it
  there does the opposite of what it does on Tagger. Defaults *on* for
  Tagger and Model Verifier (the useful case, since these are read-only
  reference boxes there); unchanged (off) for `"viewer"`, where checking
  it does the more involved annotations-merge behavior. Label text size
  doubled (`size = 6`, was `3`) for legibility.
  Fixed a crash ("replacement has 1 row, data has 0") that surfaced
  once this overlay defaulted to on: `reference_labels` was built with
  `paste0(reference_source$fk_taxonid[reference_mask], " (", ..., ")")`,
  and whenever `reference_mask` was empty (the ordinary case of a window
  with no existing model outputs/annotations to reference), `paste0()`
  doesn't return `character(0)` here the way it would with only
  zero-length inputs -- with literal length-1 strings like `" ("`/`")"`
  also in the call, it recycles the zero-length input up to length 1
  instead, silently producing one `" ()"` label rather than none. That
  length-1 result then failed to assign into the correctly-0-row
  `reference_rects$label` a few lines later. Guarded both branches with
  `if (length(reference_mask)) ... else character(0)` instead of relying
  on `paste0()` alone.
  The Tagger branch's label now also appends a verification-status
  indicator per box: `" ✓ verified"` if the modeloutput has a
  `modelverifications` row with `is_valid == 1`, `" ✗ rejected"` if any
  matching row has `is_valid == 0`, or nothing if it hasn't been
  verified at all yet (the common case, left unmarked so the majority
  of boxes stay uncluttered). Looked up per-box via `vapply()` over
  `metadata_cache$cache$modelverifications` keyed on
  `pk_modeloutputid`/`fk_modeloutputid`; a rejection takes priority over
  a valid verification if a given output somehow has both. Not added to
  the Model Verifier branch -- manual annotations there have no
  equivalent "verified/rejected" concept of their own.
- Both copies of the "Save Detections" button/`Saved hh:mm:ss` status
  text (Tagger's own box in `audio_annotator.R`, and the shared
  Player/Tagger/Verifier/Model-Outputs box in `audio_player.R`) only
  showed the "Saved ..." confirmation on every *second* click, not the
  first. Root cause: the observer that loads the current recording's
  `ManualDetx` value into the box also unconditionally cleared
  `output$manual_detx_save_status` on every run, and it re-runs not
  just on recording change but also right after a save -- because it
  reads `current_model_output_count()`/the cached `ManualDetx` value,
  both of which live under the same combined `metadata_cache$cache`
  field the save handler just wrote into (a `reactiveValues` field
  invalidates as a whole, not per nested sub-element, so writing
  `metadata_cache$cache$mediaMetaData$ManualDetx[...]` invalidates
  every other reader of `$cache` too). That re-run executed within the
  same reactive flush as the save, immediately blanking the "Saved
  hh:mm:ss" text the save handler had just set -- and only stopped
  happening on a second click because `reactiveValues`/`reactiveVal`
  skip invalidating dependents when the newly assigned value is
  `identical()` to the previous one (true on a second save of the same
  number, false on the first, real change). Fixed by splitting the
  status-clearing into its own observer keyed only on the recording
  identity (`audio_name()` in `audio_annotator.R`;
  `audio_avail()$pk_mediaid[i_audio()]` in `audio_player.R`), decoupled
  from the value-loading observer entirely, so a save no longer
  re-triggers it. Verified via temporary `cat()` instrumentation tracing
  observer execution order/timing plus DB reads confirming the write,
  live in the browser, before removing the instrumentation.
- The taxon-name label on each primary model-output/annotation box
  (`geom_label(data = rects2, ...)`, used by Tagger/Verifier/Model
  Verifier alike since `rects2` is just whatever
  `current_taxon_annotations()` holds for the active mode) had a fully
  opaque background, obscuring the spectrogram directly behind each
  label -- most noticeable in Model Verifier, where these are the model
  outputs being reviewed. First tried `alpha = 0.5` with the taxon-color
  `fill` mapping kept (translucent tint); per follow-up request this was
  replaced with `fill = "transparent"` (the taxon `fill` aesthetic
  mapping removed entirely) so the label is fully clear -- just the
  black outline and text, no tint at all, matching the plain
  `fill = "transparent"` style already used by this same layer's
  `geom_rect` box outline right above it. Confirmed via an isolated
  `ggplot`/`ggsave` reproduction (gridlines pass straight through the
  label, only the border+text show); live in-app confirmation wasn't
  possible this session -- every recording with model outputs in either
  trial database streams its audio from S3 (or the local recordings
  have no model outputs at all), and the spectrogram output never
  completed rendering for any recording this session (empty
  `<div class="shiny-plot-output">`, no server error, ~0% CPU), an
  audio I/O stall unrelated to this change. Also independently confirmed
  in this same session that `geom_label()`'s `alpha` parameter *does*
  genuinely affect the fill background in this project's ggplot2
  (3.5.1) -- ruled out as an explanation before moving to the
  `fill = "transparent"` approach instead.
- The Tagger's faint reference-overlay box (`reference_rects`, showing an
  existing model output for context) was still visibly filled despite
  `fill = "transparent"`, because that `geom_rect()` call *also* had a
  fixed `alpha = 0.4` on it. A fixed (non-`aes`) `alpha` parameter on
  `geom_rect()` is applied to `fill` and `colour` alike via
  `scales::alpha()`, and `scales::alpha("transparent", 0.4)` evaluates to
  `"#FFFFFF66"` -- a translucent *white*, not a transparent color:
  `scales::alpha()` discards whatever alpha a color already carries (here
  the literal color `"transparent"`, which is just white with alpha 0)
  and stamps the new alpha onto its RGB instead of multiplying the two.
  So the box was being washed with translucent white every time, not
  left clear. Fixed by dropping `alpha` from the call entirely and baking
  the intended faintness directly into the border instead --
  `colour = scales::alpha("orange", 0.4)` -- leaving `fill =
  "transparent"` alone so it's genuinely invisible. The other three
  `fill = "transparent"` layers in this same plot (`rects2`'s box and
  label, `rects`'s box) have no `alpha` parameter and were never affected
  by this. Confirmed with an isolated `ggplot`/`ggsave` reproduction over
  a colored raster background (a stand-in for the spectrogram): before
  the fix, the box interior visibly washed out to white; after, the
  background underneath it is pixel-identical to the surrounding area,
  with only a faint orange outline.
- The `"Warning: Un-applied filters selected..."` banner
  (`output$filters_applied`) has a hardcoded yellow background
  (`#filters_applied {background-color: yellow; ...}`); its text color
  was never set explicitly, so it inherited the dark theme's light body
  text color and became unreadable against that yellow. Added an
  explicit `color: black`. Same fix in `image_viewer.R`.
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
- Two new optional per-project settings files, read the same way as
  `cache_size.txt`/`autosave_rate.txt` (plain text, one number, in the
  project's `settings/` directory -- silently ignored if missing or
  unparseable):
    - `spec_length.txt` overrides the "Spectrogram Length (s):"
      `numericInput`'s default (still 30 if absent).
    - `spec_height.txt` overrides the default upper bound (kHz) of the
      per-recording "Spectrogram Frequency Range" slider (still 8 if
      absent). Unlike `specLength`, that slider is only ever built by a
      `renderUI` once a recording actually loads (its `max` depends on
      the recording's own sample rate) -- so the configured value is
      held in a `reactiveVal` (`default_spec_height`) read fresh each
      time that `renderUI` runs, rather than pushed once via
      `updateNumericInput`/`updateSliderInput` like the other settings
      files.
    Also added to `ammCreateDirectories()` (`R/ammCreateDirectories.R`)
    alongside `cache_size.txt`/`autosave_rate.txt`/`default_user.txt`, so
    every newly-created AMMonitor project gets both files (`30` and `8`)
    from the start, not just this one project.
- "Previous file"/"Next file" now reliably reset playback to 00:00 (and
  the spectrogram view back to its first window) on the newly-loaded
  recording, instead of sometimes carrying over wherever you'd scrubbed
  to on the previous one. The reset itself (`updateCurTime(0)`,
  `startTime(0)`) already existed, in an `observe()` keyed off the
  current recording changing -- but `output$player`'s `renderUI` (which
  rebuilds the `<audio>` tag and embeds `myAudio.currentTime = ...`)
  reads both via `isolate()`, specifically so scrubbing/spec-clicks don't
  rebuild the whole audio element on every move. That means the rebuild
  only picks up the reset if the reset observer has already run by the
  time the rebuild executes -- and since both are triggered by the same
  upstream change (the recording index) through separate reactive paths,
  Shiny doesn't otherwise guarantee which runs first in a given flush.
  Gave the reset observer `priority = 100` so it's guaranteed to run
  first, rather than leaving it to depend on unguaranteed flush ordering.

### `modules/media_tools/audio_annotator.R`

- Same Manual Detections box as above, added to the Tagger's annotation
  panel. Tracks its own just-saved state locally, since it receives the
  shared metadata cache as a read-only reactive getter rather than a
  `reactiveValues` object.
- Recording-level Tags' dropdown/bbox-list `selectizeInput()`s no longer
  show a label at all (`label = x` -> `label = NULL`). `x` here is the
  raw `pk_medialistid` value (e.g. `recording_non_taxa_bbox`), not a
  curated display name -- always a little rough as a label, and once
  the Tagger's left sidebar was narrowed (see `ui.R`) a long
  underscore-separated value like that has no spaces to wrap on, so it
  just overflowed the column instead of wrapping.

### `modules/media_tools/image_viewer.R`

- Same fix as `audio_player.R` above: `photos_avail` now only loads on
  "Apply Filters" (`ignoreInit = TRUE`) instead of also firing once on tab
  open with blank/default filters.

### `modules/media_tools/annotation_viewer_tables.R`

- "Taxon Model Outputs" table: `model_name` column no longer wraps;
  multi-select enabled for audio (was single-select), with a slightly
  darker highlight on selected rows; click-and-drag range selection across
  rows.
- Audio's "Taxon Tags" table (Tagger/Annotation Verifications/Player --
  `tagger-audio`/`verifier-audio`/`viewer-audio`) no longer groups rows
  by `pk_annotationid` with `defaultExpanded = TRUE`. That grouping made
  each annotation collapse to a single summary row (`"2636 (1)"`) that
  had to be expanded to see its own start/end time, frequency range, etc.
  on a second line -- removed `groupBy`/`defaultExpanded` so every
  annotation is just one row with all of its columns, matching how the
  "Taxon Model Outputs" table already displays. (The equivalent photo
  table -- `tagger-photo`/`verifier-photo`/`viewer-photo` -- still groups
  by annotation, since annotags genuinely nest under a photo annotation
  in a way that wasn't in scope here.)
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
- Fixed a regression (a side effect of the `audio_avail()`/`photos_avail()`
  fixes in `audio_player.R`/`image_viewer.R` above): the "Taxon Model
  Outputs" table stopped showing whether a model output had already been
  verified at all -- the `verified` column was missing from the table
  entirely, not just blank. `output$taxon_table`'s `reactable` locks in
  its column set at whatever data it's *first* rendered with; every
  update after that goes through `reactable::updateReactable(data =
  ...)`, which can refresh cell values but can't add a brand-new column
  to an already-built table. `verified` was only ever added to the
  `modelOutputs` data.frame *conditionally* (`if (nrow(modelOutputs) !=
  0) { modelOutputs <- cbind(modelOutputs, 'verified' = NA); ... }`) --
  fine as long as the very first render happened to already have data,
  which used to be guaranteed because reading an unfired
  `audio_avail()`/`photos_avail()` threw and blocked that first render
  until real data existed. Now that those always resolve immediately
  (even to an empty placeholder), the table's first-ever render routinely
  catches 0 rows, and `verified` never made it into that locked-in
  schema. Fixed by adding the `verified` column unconditionally (using
  `rep(NA, nrow(modelOutputs))`, not a bare `NA` -- `cbind` requires an
  added column's length to already match `nrow()`, and a bare `NA` errors
  "arguments imply differing number of rows" against a 0-row data.frame).
  The same fragile pattern existed for the Annotation Verifications
  page's own `verified` column (`if (nrow(annoTable) != 0 && viewer_mode
  == 'verifier')`); fixed there too by dropping the `nrow` guard, since
  `merge()` already returns the right columns (`verified`, after the
  rename) regardless of row count.

### `modules/app_modules/registerVisitUpdateDB.R`

- New audio recordings registered through Add Visit automatically get
  AudioMoth WAV comment metadata parsed and stored: `recorded_datetime_utc`,
  `recorded_datetime_local`, `device_serial`, `gain_setting`,
  `battery_voltage`, `temperature_c` (all new `media` columns). The parser
  handles the real variation across AudioMoth firmware comment formats
  (see [metamoth's firmware history](https://metamoth.readthedocs.io/en/latest/firmwares.html)):
  "gain" vs. "gain setting", "battery was" vs. "battery state was", an
  optional timezone offset in the UTC parenthetical, no temperature field
  on older firmware, and firmware 1.5.0+ replacing the "by AudioMoth
  &lt;serial&gt;" clause entirely with "during deployment &lt;id&gt;" when
  a deployment ID is configured rather than adding to it -- verified
  against real documented comment strings for each variant. The timezone
  lookup used to convert the parsed UTC timestamp to local time
  (`SELECT tz FROM locations WHERE pk_locationid = ...`) uses a
  parameterized query, so a location name containing an apostrophe (e.g.
  "Smith's Field") doesn't break the SQL.

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
- Left sidebar narrowed 25%, from AdminLTE's default 230px to 172.5px.
  shinydashboard doesn't expose a sidebar-width option -- AdminLTE
  hardcodes 230px in four places at once, all inside its own >=768px
  ("desktop") media query: the sidebar itself, the content area's left
  margin, the header logo width, and the header navbar's left margin.
  Overrode all four together (leaving the sub-768px mobile breakpoint,
  where the sidebar becomes an overlay instead, untouched). This is also
  what pushes the main content area -- including the audio player's
  spectrogram, which already fills the full content width -- further
  left; there's no separate spectrogram-specific position to adjust.
- Audio Tagger's left "annotator" sidebar (`audio_annotator_ui()`)
  narrowed from `column(2, ...)` to `column(1, ...)`, with the player
  column (containing the spectrogram) next to it widened from
  `column(10, ...)` to `column(11, ...)` to match (Bootstrap columns in
  a row need to add up to 12). Only the Tagger tab has this left/right
  split at the `ui.R` level -- Player, Annotation Verifications, and
  Model Verifications place `audio_player_ui()` directly, so its own
  internal `column(12, ...)` (see `audio_player.R` below) already gives
  it the full row on those tabs.
- App-wide dark theme, applied via a global CSS block (there's no
  toggle -- it's just always on). `dashboardPage(skin = "black")` gets
  most of the way there for the sidebar, but AdminLTE's "black" skin
  only ever darkens the sidebar by design -- the header/logo bar stays
  its own light color regardless of skin, so that needed its own
  override to look like one consistent theme rather than half-and-half.
  Most plain text (headings, labels, paragraphs) needed no explicit
  color rule at all: `color` is an inherited CSS property, so setting it
  once on `body` covers anything that doesn't set its own color, and
  only elements that set their *own* background (boxes, wellPanels,
  inputs, tables, modals) needed matching foreground colors added.
  Notable non-obvious fixes along the way:
    - shinydashboard's `.box-header`/`.box-title` default text color
      (a dark gray meant for a white box) was nearly unreadable against
      the new dark box background -- needed its own override, not just
      inheritance.
    - `reactable` ships its own stylesheet (loaded as an htmlwidgets
      dependency *after* this app's CSS), which sets `.rt-table`/`.rt-tr`
      to a white background at the same specificity -- without
      `!important` on this app's rules, reactable's own CSS wins on load
      order alone and every table (Taxon Model Outputs, File Tags, etc.)
      stayed white.
    - Missed on the first pass: `.Reactable` (capital R) is a *separate*,
      outer wrapper div, also hardcoded white by reactable's own CSS --
      distinct from `.rt-table` above. The pagination footer (page
      numbers, page-size selector) lives inside `.Reactable` but outside
      `.rt-table`, so it kept showing this wrapper's white background
      even once the table body itself was already dark. Its page-jump
      box and page-size `<select>` (`.rt-page-jump`/`.rt-page-size-select`)
      are separately hardcoded white too. All three needed their own
      `!important` overrides, same reasoning as `.rt-table` above.
    - A handful of modules (`my_home.R`, several `apps/*.R` and
      `app_modules/*.R` instructional banners) hardcode a light
      `wellPanel` background directly (`style = "background: skyblue"`,
      `lightblue`, `aquamarine`) -- inline styles beat any external
      stylesheet rule regardless of specificity, so fixing these needed
      `[style*='background: skyblue']`-style attribute-selector
      overrides with `!important`, added centrally here, rather than
      editing every one of those files individually.
  Didn't originally touch the spectrogram/waveform plots themselves
  (rendered server-side as ggplot images) -- see the follow-up fix in
  `audio_player.R` below for their axis labels. This pass doesn't reach
  into every module either -- some corner of the app not yet clicked
  through may still have an unstyled light element.

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
- A later, separate crash report (loud `Error in <Anonymous>: error in
  evaluating the argument 'x' in selecting a method for function 'nrow'`,
  from dozens of different outputs/observers) turned out to be the same
  underlying mechanism as above, just at different call sites: removing
  the one top-level `isolate(nrow(audio_avail()))` block fixed *that*
  call site, but `audio_avail()`/`photos_avail()` are read bare (no
  `req()` guard) all over both files -- every `renderText`, `renderPlot`,
  `renderUI`, and `observe` that touches `nrow(audio_avail())` or
  `audio_avail()$...` before "Apply Filters" is ever clicked hit the exact
  same problem independently. `nrow()` is an S4 generic; R wraps whatever
  error occurs while evaluating its argument in a *new* plain error,
  which strips the `shiny.silent.error` class Shiny's render/reactive
  framework checks for to decide "quietly suspend this output" vs. "print
  a loud warning" -- so an unfired `eventReactive` read through `nrow()`
  (or similar) always surfaces loudly, never silently, regardless of
  `req()` guards anywhere else in the app. Rather than hunting down and
  guarding every individual call site, fixed it at the source: removed
  `ignoreInit = TRUE` from both `eventReactive`s and added an explicit
  check inside the reactive body instead (`if (input$apply_filters == 0)
  return(<empty placeholder data.frame>)`, matching the real column
  shape). `audio_avail()`/`photos_avail()` now never throw -- before the
  button's first real click they just report zero rows, exactly like a
  filtered search that matched nothing, so every downstream `nrow()`/`$`
  read sees a valid (empty) value instead of an unfired reactive.
  Verified with the trial database: opening Model Verifications and
  leaving every filter at its default no longer logs any error at all.
- The above wasn't quite the whole story: `input$apply_filters` (the
  guard both fixes above rely on) can itself still be `NULL`, not `0`,
  for a brief window right after a session connects -- specifically
  whenever the button lives in UI that mounts a moment after the initial
  page load (e.g. behind a dynamically-rendered module/tab), the
  actionButton's own default value hasn't reached the server over the
  websocket yet. `eventReactive()`'s default `ignoreNULL = TRUE` means it
  simply does not fire at all while its trigger is `NULL` -- so during
  that window `audio_avail()`/`photos_avail()` have no cached value
  whatsoever (not even the empty placeholder), and reading them is back
  to the original unfired-reactive/`shiny.silent.error` problem, just
  narrowed from "until Apply Filters is clicked" down to "until the
  button's own value syncs." This reproduced reliably against
  `VPMon_AMM` (a much larger, slower-loading production project than the
  trial database this was first verified against) despite testing clean
  against the trial database beforehand -- a heavier session takes
  longer to reach a steady state, giving this narrow window more chances
  to actually get hit. Fixed by adding `ignoreNULL = FALSE` to both
  `eventReactive`s and updating their guards to
  `is.null(input$apply_filters) || input$apply_filters == 0`, so they
  produce the same empty placeholder on that first `NULL` too instead of
  staying unfired.
- Fixed the spectrogram view not resetting to 0 seconds in Model
  Verifications (and any other viewer mode) when moving to the next/previous
  file. A priority-100 observer already resets `startTime(0)` on every file
  change, but a second, default-priority "jump to selection" observer
  (triggered by `selected_rows()` *and* the same file-change event) could
  still overwrite it in the same flush: `selected_rows()` still held the
  previous file's selected row(s) at that point (its client-side clear via
  `updateReactable()` doesn't land until a later round trip), and if this
  observer ran before the also-default-priority observer that refreshes
  `current_taxon_annotations()` for the new file, it matched those stale
  selected IDs against the still-stale annotations and jumped `startTime()`
  to that old row's coordinates -- undoing the reset. Most visible in Model
  Verifications because its detection table allows multi-select, so users
  routinely have a row selected before paging. Fixed by giving the
  `current_taxon_annotations()`-refresh observer `priority = 50` (between
  the reset's 100 and the jump-to-selection observer's default 0), so the
  annotations are always refreshed for the new file before the
  jump-to-selection observer can act on them -- the stale IDs then match
  nothing, so no unwanted jump occurs.
- Fixed manual annotation boxes silently failing to render on the
  spectrogram in Audio Tagger (and, by the same code path, Player/Verifier)
  whenever an annotation's saved `y_min`/`y_max` ran past the currently
  displayed "Audio Frequency Range" filter (or past the recording's own
  Nyquist limit) -- e.g. a real annotation with `y_max = 9.82` kHz was
  completely invisible whenever the display range topped out at 8 kHz,
  even though it was otherwise fully inside the visible time window.
  `output$plotx`'s `rects2` (each mode's own primary/editable boxes, built
  from `current_taxon_annotations()`) filtered candidate rows with `(y_min
  >= spec_range[1]) %in% c(1, NA)` and `(y_max <= spec_range[2]) %in% c(1,
  NA)` alongside the x_min/x_max time-window check -- unlike the time
  check (deliberately hiding boxes outside the currently scrolled time
  window), excluding on the *frequency* filter this way made a box vanish
  outright instead of just clipping to the visible band, which is wrong
  for a display/zoom setting that has nothing to do with whether the
  annotation itself should be shown. Fixed by dropping the y_min/y_max
  conditions from both `which()` filters in `output$plotx` (the box-data
  and the box-label cbind use identical filters and had to be kept in
  sync) and clamping the resulting `y_min`/`y_max` to `spec_range` with
  `pmax()`/`pmin()` (alongside the existing NA-defaulting) instead, so a
  box is now clipped to the visible frequency band rather than dropped.
  Verified live: an `ammCreateMiniDemo()` recording with two manual
  annotations (one within range, one with `y_max` above the demo
  recording's ~8 kHz Nyquist limit) showed only the first box before the
  fix and both boxes (the second clipped at the top) after.

### `modules/my_home.R`

- Fixed `"Closing Database Connections"` printing once per browser
  reconnect instead of once per app run. `onStop()` is an app-level hook
  (fires once when the whole Shiny process stops), not a per-session one,
  but it was being registered inside `my_home_server()`, which runs once
  per session -- every reconnect (a page reload, a second tab, repeated
  testing) added another `onStop()` callback, and they all fired together
  whenever the app actually stopped. Guarded registration with a
  `getOption("ammonitor.onstop_registered")` flag so it only happens
  once per app run; the flag resets back to `FALSE` inside the callback
  itself so a subsequent `launchApp()` call in the same R session (stop,
  tweak something, relaunch, without restarting R) registers its own
  handler again instead of finding the flag still set from the previous
  run.

## Related database schema changes (not in this repo)

Live SQLite schema changes needed for the above, made via
`AMMonitor::dbAddUserCol()` and registered in `dbdictionary`:
`media.comments`, `media.ManualDetx`, `media.recorded_datetime_utc`,
`media.recorded_datetime_local`, `media.device_serial`,
`media.gain_setting`, `media.battery_voltage`, `media.temperature_c`. A
`BirdNET_v2.4` model row is added per-database via `registerBirdNETModel()`.
