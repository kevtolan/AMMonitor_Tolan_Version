# Local customizations

This branch/fork carries local changes on top of upstream AMMonitor
`AMMonitor2.2` (commit `585b4a79`), affecting only the Shiny app
(`inst/shiny/`) -- no changes to the package's R/ function source.

## `inst/shiny/modules/media_tools/audio_player.R`

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

## `inst/shiny/modules/media_tools/audio_annotator.R`

- Same Manual Detections box as above, added to the Tagger's annotation
  panel. Tracks its own just-saved state locally, since it receives the
  shared metadata cache as a read-only reactive getter rather than a
  `reactiveValues` object.

## `inst/shiny/modules/media_tools/annotation_viewer_tables.R`

- "Taxon Model Outputs" table: `model_name` column no longer wraps;
  multi-select enabled for audio (was single-select), with a slightly
  darker highlight on selected rows; click-and-drag range selection across
  rows.
- Flag shown above the table when the current recording has manual (human)
  annotations, so a model-output reviewer knows tagger activity exists on
  this file even though the table itself only shows model detections.

## `inst/shiny/modules/app_modules/registerVisitUpdateDB.R`

- New audio recordings registered through Add Visit automatically get
  AudioMoth WAV comment metadata parsed and stored: `recorded_datetime_utc`,
  `recorded_datetime_local`, `device_serial`, `gain_setting`,
  `battery_voltage`, `temperature_c` (all new `media` columns).

## `inst/shiny/ui.R`

- Wires `audio_comment_box_ui()` into all four Audio sub-tabs.

## Related database schema changes (not in this repo)

Live SQLite schema changes needed for the above, made via
`AMMonitor::dbAddUserCol()` and registered in `dbdictionary`:
`media.comments`, `media.ManualDetx`, `media.recorded_datetime_utc`,
`media.recorded_datetime_local`, `media.device_serial`,
`media.gain_setting`, `media.battery_voltage`, `media.temperature_c`. A
`BirdNET_v2.4` row (`pk_modelid = 6`) was also added to `models`.

## BirdNET integration (`birdnet/`)

Standalone scripts (not part of the package build -- source manually) that
add BirdNET as a detection model via the `birdnetR` package, replacing the
old Python + CSV-import workflow. See `birdnet/README.md` for usage.
