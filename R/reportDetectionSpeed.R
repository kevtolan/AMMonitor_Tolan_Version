#' @name reportDetectionSpeed
#' @title Report analysis throughput for a detection run
#' @description Internal helper shared by \code{\link{scoresDetect}} and
#' \code{\link{birdsDetect}}. Prints a one-line throughput summary (elapsed
#' time, recordings/minute) plus how many times faster than real-time the
#' analysis ran (total audio duration analyzed divided by wall-clock time
#' spent analyzing it), and returns the same figures invisibly for
#' programmatic use.
#' @param n_recordings Number of recordings processed.
#' @param elapsed_sec Wall-clock seconds spent processing them.
#' @param total_audio_sec Total duration, in seconds, of the audio actually
#' analyzed.
#' @return Invisibly, a list with n_recordings, elapsed_sec, total_audio_sec,
#' rate_per_minute, sec_per_recording, and realtime_multiplier.
#' @keywords internal
reportDetectionSpeed <- function(n_recordings, elapsed_sec, total_audio_sec) {
  elapsed_min <- elapsed_sec / 60
  rate_per_minute <- if (elapsed_min > 0) n_recordings / elapsed_min else NA_real_
  sec_per_recording <- if (n_recordings > 0) elapsed_sec / n_recordings else NA_real_
  realtime_multiplier <- if (elapsed_sec > 0) total_audio_sec / elapsed_sec else NA_real_

  audio_min <- round(total_audio_sec / 60, 1)
  elapsed_min_r <- round(elapsed_min, 2)
  sec_per_rec_r <- round(sec_per_recording, 2)
  rate_per_min_r <- round(rate_per_minute, 2)
  speed_r <- round(realtime_multiplier, 1)

  cli::cli_alert_success(
    "Processed {n_recordings} recording{?s} ({audio_min} min of audio) in {elapsed_min_r} min ({sec_per_rec_r} sec/recording, {rate_per_min_r} recordings/min)"
  )
  cli::cli_alert_info("Analysis speed: {.strong {speed_r}x} faster than real-time")

  invisible(list(
    n_recordings = n_recordings,
    elapsed_sec = elapsed_sec,
    total_audio_sec = total_audio_sec,
    rate_per_minute = rate_per_minute,
    sec_per_recording = sec_per_recording,
    realtime_multiplier = realtime_multiplier
  ))
}
