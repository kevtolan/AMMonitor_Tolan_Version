#' @name plotVerifications
#' @title Plot side-by-side spectrograms of verified data
#' @description Select a model, taxon, and score threshold, and compare validated modeloutputs (audio only) based on whether they were maked as valid or invalid. Side-by-side spectrograms are plotted so the user can make a visual comparison. The modeloutput ID's are returned.
#' @param con An open database connection
#' @param model_id pk_modelid.
#' @param taxon_id A valid primary key reference to the taxa table.
#' @param plot_modeloutput_id Whether to plot the modeloutputid on the spectrogram
#' @param min_value_num Mimimum value_num for filtering modeloutputs.
#' @param limit The maximum number of valid and invalid examples to plot (default = 10).
#' @param amm_project_path Filepath to the outermost directory for an AMMonitor project (defaults to current working directory)
#' @param spec_wl window length for the analysis (even number of points) (by default = 512).
#' @param spec_wn window name (by default "hanning").
#' @param spec_zp zero-padding (even number of points)
#' @param spec_ovlp overlap between two successive windows (in percent).
#' @param spec_freq_range Frequency range, in KHz (defaults to c(0, 10))
#' @param palette Seewave color palette for plotting spectorgrams (defaults to "reverse.gray.colors.1")
#' @param disconnect TRUE or FALSE. Should the database connection be severed
#' @return A ggplot object with two columns; the left containing valid (verified true) modeloutputs for the given taxon, and the right containing invalid (verified false) detections. \code{plotVerifications} also returns the pk_modeloutputid of each plotted verification from the modeloutputs table. 
#' @details Select a model, taxon, and score threshold, and compare validated modeloutputs based on whether they were maked as valid or invalid. Side-by-side spectrograms are plotted so the user can make a visual comparison. The modeloutput ID's are returned.
#' @family classifier
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom utils download.file
#' @importFrom tuneR readWave
#' @importFrom seewave spectro
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_gradientn theme_void theme element_blank scale_y_continuous
#' @importFrom grid grid.newpage pushViewport viewport grid.layout unit grid.text 
#' @export
#' @examples
#'\dontrun{
#'
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#'
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # Add some additional BirdNET verifications (let's make this more interesting)
#' new_modelverifications <- data.frame(
#'   fk_modeloutputid = c(45, 52, 46),
#'   fk_personid = c("sgamgee", "sgamgee", "sgamgee"),
#'   is_valid = c(1, 0, 1)
#' )
#' DBI::dbAppendTable(conx, "modelverifications", new_modelverifications)
#' 
#' # Plot the verifications (plot will be displayed)
#' modeloutput_ids <- AMMonitor::plotVerifications(
#'   con = conx, 
#'   model_id = 6, 
#'   taxon_id = "oven", 
#'   plot_modeloutput_id = TRUE, 
#'   amm_project_path = demo_fp
#' )
#' 
#' # View modeloutput_id's from the returned sample of verificatied modeloutputs
#' (modeloutput_ids)
#' 
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#'
#' }


plotVerifications <- function(con, model_id, taxon_id, plot_modeloutput_id = FALSE, min_value_num = 0, limit = 10, amm_project_path = getwd(), spec_wl = 512, spec_wn = "hanning", spec_zp = 0, spec_ovlp = 0, spec_freq_range = c(0, 10), palette = "reverse.gray.colors.1", disconnect = FALSE) {
  
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Set audio path
  if (file.exists(paste0(amm_project_path, '/settings/audio_path.txt'))) {
    AUDIO_PATH <- read.csv(paste0(amm_project_path, '/settings/audio_path.txt'), header = F)[,]
    if (!startsWith(AUDIO_PATH, "http") && !dir.exists(AUDIO_PATH)) {
      AUDIO_PATH <- gsub("//", "/", file.path(amm_project_path, AUDIO_PATH))
    }
  } else if (dir.exists(paste(amm_project_path, "recordings", sep = "/"))) {
    AUDIO_PATH <- paste(amm_project_path, "recordings", sep = "/")
  } else {
    AUDIO_PATH <- ""
  }
  
  # Get all verified observations ----------
  verified <- DBI::dbGetQuery(
    con, 
    statement = "SELECT media.filename, media.filepath, modeloutputs.fk_taxonid, modeloutputs.x_min, modeloutputs.x_max, modeloutputs.y_min, modeloutputs.y_max, modeloutputs.value_num, modelverifications.* FROM media INNER JOIN modeloutputs ON media.pk_mediaid = modeloutputs.fk_mediaid INNER JOIN modelverifications ON modeloutputs.pk_modeloutputid = modelverifications.fk_modeloutputid WHERE media_type = 'audio' AND fk_modelid = $model_id AND fk_taxonid = $taxon_id AND value_num > $min_value_num;",
    params = list(
      model_id = model_id,
      taxon_id = taxon_id,
      min_value_num = min_value_num
    )
  )
  
  # Check query results -------
  
  if (nrow(verified) == 0) {
    stop('No verifications to plot for this model_id, taxon_id, and min_value_num.')
  }
  
  num_valid <- sum(verified$is_valid == 1)
  num_invalid <- sum(verified$is_valid == 0)
  
  if (num_valid < limit) {
    warning(
      paste(
        "There are fewer than", 
        limit, 
        "valid modeloutputs meeting the specified criteria. Only",
        num_valid, 
        "will be displayed."
      )
    )
  }
  
  if (num_invalid < limit) {
    warning(
      paste(
        "There are fewer than", 
        limit, 
        "invalid modeloutputs meeting the specified criteria. Only",
        num_invalid, 
        "will be displayed."
      )
    )
  }
  
  # Get the samples of modeloutputs to display ------
  i_valid <- which(verified$is_valid == 1)
  if (length(i_valid) == 1) {
    sample_valid <- verified[i_valid,]
  } else {
    sample_valid <- verified[sample(i_valid, min(limit, num_valid)),]
  }
  
  i_invalid <- which(verified$is_valid == 0)
  if (length(i_invalid) == 1) {
    sample_invalid <- verified[i_invalid,]
  } else {
    sample_invalid <- verified[sample(i_invalid, min(limit, num_invalid)),]
  }

  the_sample <- rbind(sample_valid, sample_invalid)

  # Create the plots ---------
  plots <- list()
  for (i in seq_len(nrow(the_sample))) {
    if (!is.na(the_sample$filepath[i])) {
      audio_path <- the_sample$filepath[i]
    } else if (!is.na(amm_project_path)) {
      audio_path <- file.path(AUDIO_PATH, the_sample$filename[i])
    }
    
    if (grepl("^www.|^http:|^https:", audio_path)) {
      temp.file <- tempfile()
      utils::download.file(
        url = audio_path, 
        destfile = temp.file, 
        quiet = TRUE, 
        mode = "wb", 
        cacheOK = TRUE
      )
      if (!file.exists(temp.file)) stop("File couldn't be downloaded")
      mo_audio <- tuneR::readWave(
        temp.file, 
        from = the_sample$x_min[i], 
        to = the_sample$x_max[i], 
        units = "seconds"
      )
    } else {
      mo_audio <- tuneR::readWave(
        audio_path, 
        from = the_sample$x_min[i], 
        to = the_sample$x_max[i],
        units = "seconds"
      )
    }
    
    s <- seewave::spectro(
      mo_audio, 
      wl = spec_wl,
      wn = spec_wn,
      zp = spec_zp,
      ovlp = spec_ovlp,
      fastdisp = TRUE,
      plot = FALSE
    )
    
    s_amp <- s$amp
    rownames(s_amp) <- s$freq
    colnames(s_amp) <- s$time
    s_df <- data.frame(
      freq = rep(as.numeric(names(s_amp[,1])), ncol(s_amp)),
      time = as.vector(sapply(as.numeric(names(s_amp[1,])), function(x) {rep(x, nrow(s_amp))})),
      amp = as.vector(s_amp)
    )

    p <- ggplot2::ggplot(data = s_df, ggplot2::aes(x = time, y = freq, fill = amp)) +
      ggplot2::geom_raster(interpolate = TRUE, na.rm = TRUE) +
      ggplot2::scale_fill_gradientn(
        colours = eval(parse(text = paste0('seewave::', palette, '(255)'))) 
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "none",
        legend.background = ggplot2::element_blank(),
        legend.box.background = ggplot2::element_blank()
      ) +
      ggplot2::scale_y_continuous(name = "Frequency (kHz)", limits = c(max(0, spec_freq_range[1]), min(spec_freq_range[2], mo_audio@samp.rate/2)
      ), expand = c(0,0))
      
    plots[[i]] <- p
  }
  
  # Display the plots -------
  n_rows <- min(max(num_valid, num_invalid), limit)
  
  # Create the grid layout
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n_rows + 1, 2, heights = grid::unit(c(1, rep(8, n_rows)), "null"))))
  
  # Plot the headers
  grid::grid.text("is_valid == 1", vp = grid::viewport(
    layout.pos.row = 1, 
    layout.pos.col = 1
  ))
  grid::grid.text("is_valid == 0", vp = grid::viewport(
    layout.pos.row = 1, 
    layout.pos.col = 2
  ))
  
  # Plot each row of spectrograms
  for (i in seq_len(n_rows)) {
    # Plot the valid spectrogram for row i
    if (i <= num_valid) {
      vp <- grid::viewport(
        layout.pos.row = i + 1, 
        layout.pos.col = 1
      )
      print(
        plots[[i]], 
        vp = vp
      )
      if (plot_modeloutput_id) {
        grid::grid.text(
          the_sample$fk_modeloutputid[i], 
          x = grid::unit(5, "mm"),
          y = grid::unit(1, "npc") - grid::unit(3, "mm"),
          vp = vp,
          just = c("left", "top")
        )
      }
    }
    
    # Plot the invalid spectrogram for row i
    if (i <= num_invalid) {
      vp <- grid::viewport(
        layout.pos.row = i + 1, 
        layout.pos.col = 2
      )
      print(
        plots[[min(num_valid, limit)+i]], 
        vp = vp
      )
      if (plot_modeloutput_id) {
        grid::grid.text(
          the_sample$fk_modeloutputid[min(num_valid, limit)+i], 
          vp = vp,
          x = grid::unit(5, "mm"),
          y = grid::unit(1, "npc") - grid::unit(3, "mm"),
          just = c("left", "top")
        )
      }
    }
  }
  return(the_sample$fk_modeloutputid)
}
