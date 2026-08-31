#' @name dbPlotSummary
#' @aliases dbPlotSummary
#' @title Creates database summary figures 
#' @description Inputs result from \code{dbGetSummaryData()} and creates 
#' database 
#' summary figures for inspection of outliers and for use in reporting
#' @param summary_data A list of dataframes generated from the 
#' \code{dbGetSummaryData()} function
#' @param num_bins  Number of bins to break the date columns into; default is 
#' 15 for  plotting.  Also could  enter "month", "year", or "quarter".
#' @param map_database  Name of map database for plotting locations. Options 
#' are
#'  "world" or "usa" or "state" or "county"
#' @param regions  Vector of regions within the map_database for plotting 
#' (e.g., c("vermont")) 
#' @param key_species Vector of taxa ids to include for additional plots and
#'  maps
#' @param show_plots TRUE or FALSE. Should the plots be printed? 
#' Default is FALSE
#' @usage dbPlotSummary(
#'  summary_data,
#'  num_bins = 15,
#'  map_database = NA,
#'  regions = NA,
#'  key_species = NULL,
#'  show_plots = FALSE)
#' @export
#' @import ggplot2
#' @importFrom stats complete.cases
#' @importFrom utils  tail head
#' @importFrom maps map
#' @details  \code{dbPlotSummary()} provides graphical summaries of different 
#' elements 
#' of an AMMonitor database.  A primary input is a list generated from the 
#' function \code{dbGetSummaryData()}, which extracts portions of the database 
#' for
#' inspection and summary.   The primary output is a list of ggplot objects
#' created by the package, ggplot2.  Each output can be further modified by 
#' the user with ggplot2 code.
#' 
#' Map boundaries are produced from the package, maps.  The 'map_database' 
#' argument in \code{dbPlotSummary()} 
#' feeds the 'database' argument to the \code{maps::map()} function.  It is a 
#' character 
#' string naming a geographical database. The string choices include a world 
#' map, 
#' three USA databases (usa, state, county), and more.  The 'regions'  argument 
#' in \code{dbPlotSummary()} feeds the 'regions'
#'  argument from the \code{maps::map()} function.   
#' 
#' In the maps package, each database is 
#' composed of a collection of polygons, and each polygon has a unique name. 
#' When a region is composed of more than one polygon, the individual polygons 
#' have the name of the region, followed by a colon and a qualifier, as in 
#' michigan:north and michigan:south. Each element of regions is matched 
#' against 
#' the polygon names in the database and, according to exact, a subset is 
#' selected for drawing. The regions may also be defined using (perl) regular 
#' expressions. This makes it possible to use 'negative' expressions like 
#' "Norway(?!:Svalbard)", which means Norway and all islands except Svalbard. 
#' All entries are case insensitive. 
#'  
#'  Type \code{help(package = 'maps')} to see the maps package index page. 
#'  
#' See the "dbsummary" learnr tutorial for more details on the
#' dbPlotSummary() function. The tutorial can be launched with
#' \code{learnr::run_tutorial(name = "dbsummary", package = "AMMonitor")}.
#' 
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # the sample database is named "demo.sqlite" within the database directory
#' list.files(file.path(demo_fp, "database"))
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # get summary data and disconnect from database
#' results <- dbGetSummaryData(con = conx, disconnect = TRUE)
#'  
#' # look at the returned object; it is a list of dataframes
#' str(results, max.level = 1)
#' 
#' # load ggplot2
#' library(ggplot2)
#' 
#' # send the captured outputs to dbPlotSummary to generate summary plots
#' plot_results <- dbPlotSummary(
#'   summary_data = results,
#'   num_bins = 'month',
#'   map_database = "state", 
#'   regions = "vermont",
#'   key_species = c("moose", "black bear"),
#'   show_plots = FALSE
#' )
#'  
#' # the returned object is a list of ggplot objects
#' str(plot_results, max.level = 1)
#' 
#' # or just look at the names of the ggplot objects
#' names(plot_results)
#' 
#' #  see all results in plot window
#' plot_results
#' 
#' # or plot individually for inspection
#' plot_results[[1]]
#' 
#' # each element can be retrieved by name or index and edited
#' plot1 <- plot_results[[1]]
#' plot(plot1)
#' 
#' # customize plot1 to your liking
#' plot1 <- plot1 +
#'  scale_fill_viridis_d()
#' plot1
#'  
#' # make sure to update the list if you want to save plots
#' plot_results[['gg_media_month_year_hist']] <- plot1
#' 
#' # confirm the update
#' plot_results[[1]]
#' 
#' } 
#' 
#' 
dbPlotSummary <- function(summary_data, num_bins = 15, map_database = NA, regions = NA, key_species = NULL, show_plots = FALSE) {
  
  
  # check summary_data
  test_all <- all(
    class(summary_data) == 'list', 
    length(summary_data) >= 10 , 
    'field_visits' %in% names(summary_data)
  )
  
  if (test_all == FALSE) {
    stop('Please enter a valid summary_data results list. Use dbGetSummaryData() with 
         an open connection to your database.')
  }
  
  # check key species vector
  taxa <- summary_data$taxa
  
  if (!is.null(key_species)) {
    if (length(key_species) > 10) {
      stop("Please enter a vector of 10 taxa or less. The default for key_species includes all taxa.")
    }
    
    for (sp in tolower(key_species)) {
      if (!sp %in% tolower(taxa$pk_taxonid) & !sp %in% tolower(taxa$common_name)) {
        stop(paste0(sp, " is not a valid taxon id or common name."))
      } 
    }
  } # end checking key_species input
  
  
  # check num_bins input
  if (!is.numeric(num_bins) 
      & num_bins != 'year' 
      & num_bins != 'month' 
      & num_bins != 'quarter') {
    stop("Please enter a valid num_bins value: 'month', 'year', 'quarter', or an integer.")
  }
  
  # check that num_bins != year if only one year present
  field_visits <- summary_data$field_visits
  
  if (length(unique(field_visits$year)) == 1 & num_bins == 'year') {
    stop(paste0("The summary results provided only contain the year ", 
                unique(field_visits$year), ". Please enter another num_bins 
                value: 'month', 'quarter', or an integer."))
  }
  
  # find earliest start date in data and latest end date in data (media vs visits)
  media <- summary_data$media
  
  startDate_visits <- as.Date(
    format(min(field_visits$visit_date), "%Y-%m-01")
  )
  startDate_media <- as.Date(
    format(min(media$start_date), "%Y-%m-01")
  )
  
  startDate <- min(c(startDate_visits, startDate_media ))
  
  endDate_visits <- max(field_visits$visit_date) + 1
  endDate_media <- max(media$start_date) + 1
  
  endDate <- max(c(endDate_visits, endDate_media))
  
  
  # check that num_bins != quarter if only one quarter present
  if (num_bins == 'quarter') {
   quarters <- seq.Date(
                    startDate,
                    endDate,
                    by = num_bins
                  )
   
   if (length(quarters) == 1) {
     stop('Media and field visits in the summary data only occur in one quarter. Please choose a different num_bins value.')
   }
   
  }
  
  
  
  if (nrow(field_visits) == 0 ) {
    message('Your summary data contains media but no field visits.')
  }
  
 # create list to store results
  results <- list()
  
  # total media by month and year ================================
  media$month <- as.factor(media$month)
  media$media_type <- as.factor(media$media_type)

  results$gg_media_month_year_hist <- ggplot2::ggplot(
    media,
    aes(
      x = year,
      fill = month)) +
    geom_bar() +
    labs(
      title = "Total Media Files Collected by Year and Month",
      x = "Year",
      y = "Count") +
    theme_minimal() + 
    guides(fill = guide_legend(title = "Month"))
  
  if (show_plots == TRUE) results$gg_media_month_year_hist

  # total media by media type and year ================================
  results$gg_media_type_year_hist <- ggplot2::ggplot(
    media, 
    aes(
      x = year,
      fill = media_type)) +
    geom_bar() +
    labs(
      title = "Total Media Files Collected by Year and Media Type",
      x = "Year",
      y = "Count") +
    theme_minimal() + 
    guides(fill = guide_legend(title = "Media Type"))
  
  if (show_plots == TRUE) results$gg_media_type_year_hist

  # media by year and location ====================
  
   # get visits associated with media
  visits <- summary_data$media_visits
  
 
   # creating plot
   results$gg_media_year_location_hist <- ggplot2::ggplot(
     media,
     aes(
       x = year,
       fill = fk_locationid)) +
    geom_bar() +
    labs(
      title = "Total Media Files Collected by Year and Location",
      x = "Year",
      y = "Count") +
    theme_minimal() + 
    guides(fill = guide_legend(title = "Location ID"))
  
  if (show_plots == TRUE) results$gg_media_year_location_hist
  

  # visits with media by year and month ======================
  visits$visit_type[which(is.na(visits$visit_type))] <- "unknown/NA"
  visits$visit_type <- as.factor(visits$visit_type)
  
  results$gg_media_visits_year_month <- ggplot2::ggplot(
    visits,
    aes(
      x = year,
      fill = month)) +
    geom_bar() +
    labs(
      title = "Field Visits Associated with Collected Media by Year and Month",
      x = "Year",
      y = "Count") +
    theme_minimal() +
    guides(fill = guide_legend(title = "Visit Month"))
  
  if (show_plots == TRUE) results$gg_media_visits_year_month

  # get all visits, not just those associated with media
  field_visits <- summary_data$field_visits
  field_visits$visit_date <- as.Date(field_visits$visit_date)
  
  # field visits by year and visit_type =====================
  results$gg_field_visits_year_type <- ggplot2::ggplot(
    field_visits,
    aes(
      x = year,
      fill = visit_type)) +
    geom_bar() +
    labs(
      title = "Field Visits by Year and Type",
      x = "Year",
      y = "Count") +
    theme_minimal() +
    guides(fill = guide_legend(title = "Visit Type"))
  
  if (show_plots == TRUE) results$gg_field_visits_year_type
  
  
  # locations and date checked per year (julian dates) ==========
  julian_dates <- field_visits
  julian_dates$visit_date <- format(julian_dates$visit_date, '%j')
  
  results$gg_visits_julian_date_checked <- ggplot2::ggplot(
    julian_dates,
    aes(
      x = fk_locationid,
      y = as.numeric(visit_date),
      fill = year)) +
    geom_col(position = "dodge") +
    labs(
      title = "Locations and Date Checked per Year",
      x = "Location ID",
      y = "Date (Julian)") +
    theme_minimal() +
    guides(fill = guide_legend(title = "Year")) +
    theme(
      axis.text.x = element_text(angle = 90,
                                 vjust = 0.5,
                                 hjust = 1))
  
  if (show_plots == TRUE) results$gg_visits_julian_date_checked
    
  # field visits by date bins and visit_type ======================
  
  # if min and max field visit dates in given date range are equal
  # or if time difference between the two is smaller than number bins,
  # skip section
  
  if (nrow(field_visits) != 0) {

    # create cutoff dates for bins, by number or 'month' or 'year' etc.
    # (note number of date bin dates is 1 greater than inputed bins, for cutoff)
     if (is.numeric(num_bins)) {
      
      # add one to maximum date so latest visit is included in bins
       date_bins <- seq.Date(
         startDate,
         endDate,
         length.out = num_bins + 1)
      
    } else {  # if 'month' or 'year' or 'quarter'
      
       first_date_of_bin <- seq.Date(
        startDate,
        endDate,
        by = num_bins)
   
      # add one bin out for date cutoff
      date_bins <- c(first_date_of_bin,
        tail(first_date_of_bin, n = 1) 
          + difftime(
            first_date_of_bin[2],
            first_date_of_bin[1],
            units = "days") 
          + 3)
      
      
    }

    # applying date bin factors to visits by date
    # label factor by start date of bin only
    field_visits$date_bin_factor <- factor(
      cut(
        field_visits$visit_date,
        breaks = date_bins,
        labels = utils::head(date_bins,
        n = length(date_bins) - 1)
        ),
      levels = utils::head(date_bins,
                    n = length(date_bins) - 1)
      )
    
    
    
    # create plot
    results$gg_field_visits_type_binned <- ggplot2::ggplot(
      field_visits,
      aes(
        x = date_bin_factor,
        fill = visit_type)) + 
      geom_bar() +
      scale_x_discrete(drop = FALSE) +
      labs(
        title = "Field Visit Types by Date Bin",
        x = "Bin Start Date",
        y = "Count") +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))   +
      guides(fill = guide_legend(title = "Visit Type: "))
    
    if (show_plots == TRUE) results$gg_field_visits_type_binned
  
    
    # plot visits by date bin per location ===========
    results$gg_field_visits_location_binned <- ggplot2::ggplot(
      field_visits,
      aes(
        x = date_bin_factor,
        fill = fk_locationid)) + 
      geom_bar() +
      scale_x_discrete(drop = FALSE) +
      labs(
        title = "Field Visits per Location by Date Bin",
        x = "Bin Start Date",
        y = "Count") +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))   +
      guides(fill = guide_legend(title = "Location ID:"))
    
    if (show_plots == TRUE) results$gg_field_visits_location_binned
    
  } # end if field visits not zero
  
  
  ## media per location by date bin =================
  media$date_bin_factor <- factor(
    cut(
      media$start_date,
      breaks = date_bins,
      labels = utils::head(date_bins,
                           n = length(date_bins) - 1)
    ),
    levels = utils::head(date_bins,
                         n = length(date_bins) - 1)
  )
  
  
  
  # create plot
  results$gg_media_location_binned <- ggplot2::ggplot(
    media,
    aes(
      x = date_bin_factor,
      fill = fk_locationid)) + 
    geom_bar() +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = "Collected Media files per Location by Date Bin",
      x = "Bin Start Date",
      y = "Count") +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))   +
    guides(fill = guide_legend(title = "Visit Type: "))
  
  if (show_plots == TRUE) results$gg_media_location_binned
  
  
  
  # plot visits by person  =========================
  results$gg_person_visits <- ggplot2::ggplot(
    field_visits,
    aes(
      x = fk_personid,
      fill = visit_type)) +
    geom_bar(
      position = "stack",
      stat = "count") +
    labs(
      title = "Visits by Person and Type",
      x = "Person",
      y = "Count",
      fill = "Visit Type") +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme_minimal()
  
  if (show_plots == TRUE) results$gg_person_visits
  
  # get locations
  locations <- summary_data$locations  
  
  # bar chart of locations by type =================
  results$gg_locations_type_status <- ggplot2::ggplot(
    locations,
    aes(
      x = location_type,
      fill = location_status)) +
    geom_bar(
      position = "stack",
      stat = "count") +
    labs(
      title = "Total Monitoring Locations by Location Type and Status",
      x = "Location Type",
      y = "Count") +
    guides(
      fill = guide_legend(title = "Location Status")) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme_minimal() 
  
  if (show_plots == TRUE) results$gg_locations_type_status

  # count of visits by location ids by year =================
  
  results$gg_visits_by_location <- ggplot2::ggplot(
    field_visits,
    aes(
      x = fk_locationid,
      fill = year)) +
    geom_bar() +
    labs(
      title = "Field Visits by Location ID per Year",
      x = "Location ID",
      y = "Count") +
    theme_minimal() +
    guides(fill = guide_legend(title = "Year")) +
    theme(
      axis.text.x = element_text(angle = 90,
                                 vjust = 0.5,
                                 hjust = 1))
  
  if (show_plots == TRUE) results$gg_visits_by_location
  
  # locations map =====================
  
  locations_xy <- locations[stats::complete.cases(
    locations$lat, locations$long), ]
  
  if (nrow(locations_xy) > 0) {
    g <- ggplot2::ggplot(
      locations_xy,
      aes(
        x = long,
        y = lat))  +
      geom_point(
        data = locations, 
        aes(
          x = long,
          y = lat,
          color = location_status,
          shape = location_type), 
          size = 3) +
      coord_fixed() +
      labs(
        title = "Map of Locations",
        x = "Longitude",
        y = "Latitude") +
      guides(
        shape = guide_legend(title = "Location Type"),
        color = guide_legend(title = "Location Status")) +
      theme_minimal()
    
    if (!is.na(map_database)) {
      # map of locations by type
      borders <- maps::map(
        database = map_database, 
        regions = regions, 
        fill = TRUE, 
        plot = FALSE
      )
      g <- g + geom_polygon(
        data = borders, 
        aes(x = long, y = lat), 
        fill = NA,
        color = "black") 
    }
    
    
    # add to results
    results$gg_locations_map <- g
    
    if (show_plots == TRUE) results$gg_locations_map
 
  } # end of map


  
  # plot modeloutputs ==============================
  modeloutputs <-  summary_data$modeloutputs
  
  modeloutputs$fk_taxonid <- modeloutputs$`fk_taxonid:1`
  
  results$gg_media_model <- ggplot2::ggplot(
    modeloutputs,
    aes(
      x = model_name,
      fill = as.factor(model_name))) +
    geom_bar() +
    labs(
      title = "Summary Count of Model Names by Media ID",
      x = NULL,
      y = "Count") +
    theme_minimal() +
    guides(
      fill = guide_legend(title = "Model Name")) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
      legend.position = "top"
    )
  
   if (show_plots == TRUE) results$gg_media_model
  
  results$gg_tot_modeloutputs <- ggplot2::ggplot(
    modeloutputs,
    aes(
      y = fk_taxonid,
      fill = as.factor(model_name))) +
    geom_bar() +
    guides(fill = guide_legend(title = "Model ID")) +
    theme_minimal() +
    theme(
      legend.position = "top",
      text = element_text(size = 10),
      if (show_plots == TRUE) .margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
    labs(
      title = "Machine Tags by Taxa",
      y = "Taxa", 
      x = "Count of tags")
  
  if (show_plots == TRUE) results$gg_tot_modeloutputs
  

    # plots for annotations ==============================
  
  # total annotations by personid
  annotations <- summary_data$annotations
  
  results$gg_tot_annotations_personid <- ggplot2::ggplot(
    annotations,
    aes(x = fk_personid)) +
    geom_bar(stat = "count") +
    labs(
      title = "Annotations per Person",
      x = "Person ID",
      y = "Count") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90,
                                 vjust = 0.5,
                                 hjust = 1))
  
  if (show_plots == TRUE) (results$gg_tot_annotations_personid)
  
  # annotations per taxa by person id
  results$gg_annotations_personid <- ggplot2::ggplot(
    annotations,
    aes(
      y = fk_taxonid,
      fill = fk_personid)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Person ID")) +
    theme_minimal() +
    theme(
      legend.position = "top",
      text = element_text(size = 10),
      plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
    labs(
      title = "Annotations per Person by Taxa",
      y = "Taxa",
      x = "Count of tags") 
   
  
  if (show_plots == TRUE) (results$gg_annotations_personid)

  
    annotations$media_year <- annotations$year
    annotations$media_month <- annotations$month

 
  # reduce to unique media 
  
  annotations <- annotations[which(
    annotations$fk_mediaid %in% unique(annotations$fk_mediaid)),]
  
  annotations_key_sp <- data.frame(matrix(nrow = 0, ncol = ncol(annotations)))
  colnames(annotations_key_sp) <- colnames(annotations)
  
  # create annotations for only key species 
  
  if (length(key_species) > 0) {
 
   if (nrow(annotations) != 0) {
    
    for (i in 1:nrow(annotations)) {
      
      if (!tolower(annotations$fk_taxonid[i]) %in% tolower(key_species) 
          & !tolower(annotations$common_name[i]) %in% tolower(key_species)) next
      
      annotations_key_sp <- rbind(annotations_key_sp, annotations[i,])
      
    }
  
    if (nrow(annotations_key_sp) != 0) {
      
      
      annotations_key_sp$fk_taxonid = factor(
        annotations_key_sp$fk_taxonid,
        levels = unique(annotations_key_sp$fk_taxonid)
      )
      
      # annotations by month per key taxa, barplots dodge
      results$gg_annotations_month_keysp <- ggplot2::ggplot(
        annotations_key_sp,
        aes(
          x = as.factor(media_month),
          fill = fk_taxonid)) +
        geom_bar(position = position_dodge(
          preserve = "single")) +
        scale_x_discrete(drop = FALSE) +
        guides(fill = guide_legend(title = "Taxon ID")) +
        theme_minimal() +
        labs(
          title = "Annotated Media Files per Month by Key Taxa",
          y = "Count Tagged Media", 
          x = "Month")
      
      if (show_plots == TRUE) (results$gg_annotations_month_keysp)
      
      
      # annotations by year per key taxa, barplots dodge
      results$gg_annotations_year_keysp <- ggplot2::ggplot(
        annotations_key_sp,
        aes(
          x = media_year,
          fill = fk_taxonid)) +
        geom_bar(position = position_dodge(
          preserve = "single")) +
        guides(fill = guide_legend(title = "Taxon ID")) +
        scale_x_discrete(drop = FALSE) +
        theme_minimal() +
        labs(
          title = "Annotated Media Files per Year by Key Taxa",
          y = "Count Tagged Media", 
          x = "Year")
      
      if (show_plots == TRUE) (results$gg_annotations_year_keysp)
    } # end if there are key sp annotations
   } # end if there are annotations
    # maps of key species detections ============
    
    # Annotation and Model Verifications
    
    annotation_verif <- summary_data$annotationverifications
    
    # model verifications
    model_verif <- summary_data$modelverifications
    
    # loop through given key species
    for (sp in tolower(key_species)) {
      
      # create detection data
      locations_xy_key_sp <- locations_xy
      locations_xy_key_sp$annotated <- 0
      locations_xy_key_sp$ml_model_tag <- 0
      locations_xy_key_sp$verified <- 0
  
    
      # loop through locations_xy and record number of times key sp. recorded at that location
      if (nrow(annotations_key_sp) != 0) {
        
        for (i in 1:nrow(locations_xy_key_sp)) {
          
          # annotated 1 or 0 at that location
          if (length(
            which(
              tolower(annotations_key_sp$fk_taxonid) == sp 
              & annotations_key_sp$fk_locationid == locations_xy_key_sp$pk_locationid[i])
            )) {
            
            locations_xy_key_sp$annotated[i] <- 1  
          }
          
          # tagged by machine learning models at that location 
          if (length(
            which(
              tolower(modeloutputs$fk_taxonid) == sp 
              & modeloutputs$fk_locationid == locations_xy_key_sp$pk_locationid[i])
            )) {
            
            locations_xy_key_sp$ml_model_tag[i] <- 1
          } 
          
        
          # verified at location 1 or 0
          if (length(
            which(
              tolower(annotation_verif$fk_taxonid) == sp 
              & annotation_verif$fk_locationid == locations_xy_key_sp$pk_locationid[i])
            )) {
            
            locations_xy_key_sp$verified[i] <- 1
            
          } else if (length(
            which(
              tolower(model_verif$fk_taxonid) == sp 
              & model_verif$fk_locationid == locations_xy_key_sp$pk_locationid[i])
            )) {
            
            locations_xy_key_sp$verified[i] <- 1
          }
      
        } # end loop recording annotations and verifications 
        
        # add column for color factor
        locations_xy_key_sp$tag <- NA
        
        for (i in 1:nrow(locations_xy_key_sp)) {
          if (locations_xy_key_sp$verified[i] == 1) {
            locations_xy_key_sp$tag[i] <- "Verified"
          } else if (locations_xy_key_sp$annotated[i] == 1) {
            locations_xy_key_sp$tag[i] <- "Annotated"
          } else if (locations_xy_key_sp$ml_model_tag[i] == 1) {
            locations_xy_key_sp$tag[i] <- "ML Model Tag"
          } else {
            locations_xy_key_sp$tag[i] <- "Not Recorded"
          }
        }
  
        g <- ggplot2::ggplot(
          locations_xy_key_sp,
          aes(
            x = long,
            y = lat))  +
          geom_point(
            data = locations_xy_key_sp, 
            aes(
              x = long,
              y = lat,
              color = as.factor(tag))) +
          coord_fixed() +
          guides(color = guide_legend(title = "Status")) +
          labs(
            title = paste0('Locations where ', toupper(sp), ' was Recorded'),
            x = "Longitude",
            y = "Latitude") +
          theme_minimal()
        
        if (!is.na(map_database)) {
          # map of locations by type
          borders <- maps::map(
            database = map_database, 
            regions = regions, 
            fill = TRUE, 
            plot = FALSE
          )
          key_sp_map <- g + geom_polygon(
            data = borders, 
            aes(x = long, y = lat), 
            fill = NA, color = "black")
          
          # add key_sp_map to results
          results$test <- key_sp_map
          index <- which(names(results) == "test")
          names(results)[index] <- paste0("gg_", gsub(" ", "_", sp), "_map") 
          
        }
      }
      
    } # end loop through key species
  
  } else { # end key species if statement

    results$gg_annotations_year <- ggplot2::ggplot(
      annotations,
      aes(
        y = fk_taxonid,
        fill = media_year)) +
      geom_bar() +
      guides(fill = guide_legend(title = "Media Year")) +
      theme_minimal() +
      theme(
        legend.position = "top",
        text = element_text(size = 10),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      labs(title = "Annotated Media Files for All Taxa by Year",
           y = "Taxa",
           x = "Count Tagged Media") 
    
    if (show_plots == TRUE) results$gg_annotations_year
    
  } # end else (no key sp. vector entered)
  
  # annotation verifications =============
  
  annotation_verifications <- summary_data$annotationverifications
  
  if (nrow(annotation_verifications) != 0) {
    
    # replace tagger id col name
    names(annotation_verifications)[which(
      names(annotation_verifications) == "fk_personid")[1]] <- "tagger_id"
    
    # valid annotations by tagger id =============
    
    annotation_verifications2 <- annotation_verifications[, c('tagger_id',
                                                              'fk_taxonid',
                                                              'is_valid')]
    
    results$gg_valid_annotations_by_tagger <- ggplot2::ggplot(
      data = annotation_verifications2, 
      aes(
        x = tagger_id,
        fill = is_valid)) +
      geom_bar() +
      guides(fill = guide_legend(title = "Annotation is Valid")) +
      theme_minimal() +
      theme(
        legend.position = "top",
        text = element_text(size = 10),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      labs(title = "Valid Annotations by Tagger",
           y = "Count",
           x = "Tagger ID") 
    
    if (show_plots == TRUE) results$gg_valid_annotations_by_tagger

    colnames(annotation_verifications)[which(colnames(annotation_verifications) == 'fk_personid:1')] <- 'fk_personid'
    # annotation verifications by person id ========
    annotation_verifications3 <- annotation_verifications[ , c('fk_personid',
                                                              'fk_taxonid')]
      
    results$gg_annotation_verifications_person <- ggplot2::ggplot(
      annotation_verifications3,
      aes(
        y = fk_taxonid,
        fill = fk_personid)) +
      geom_bar() +
      guides(fill = guide_legend(title = "Verifier ID")) +
      theme_minimal() +
      theme(
        legend.position = "top",
        text = element_text(size = 10),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
      labs(title = "Verified Annotations by Tagger",
           y = "Taxa",
           x = "Count") 
      
    if (show_plots == TRUE) results$gg_annotation_verifications_person
    
    
    # key species plots for verified annotations
    if (!is.null(key_species)) {
      
      # get verifications for only key sp
      taxonids <- tolower(taxa$pk_taxonid[
            which(tolower(taxa$pk_taxonid) %in% tolower(key_species))
          ]
        )
      
      commons <- tolower(taxa$common_name[
            which(tolower(taxa$common_name) %in% tolower(key_species))
          ]
        )
      
      key_sp_all <- c(taxonids, commons)
      
      anno_verifications <- annotation_verifications[
          which(tolower(annotation_verifications$fk_taxonid) %in% key_sp_all)
        ,]
      
      # annotation verifications
      
      # get verifications for unique media
      unique_media_per_sp <- unique(
        anno_verifications[c('fk_taxonid', 'fk_mediaid')])
      
      anno_verifications <- anno_verifications[row.names(unique_media_per_sp),]
      
      # remove invalid annotations
      anno_verifications <- anno_verifications[
        which(anno_verifications$is_valid == 1)
        ,]
      
      # keep only necessary cols 
      anno_verifications <- anno_verifications[, c('fk_taxonid',
                                                   'fk_visitid',
                                                   'year',
                                                   'month')]
      
      
      # media with verified annotations for key taxa by year ===============
      
      results$gg_valid_annotations_media_year_keysp <- ggplot2::ggplot(
        anno_verifications,
        aes(
          x = as.factor(year),
          fill = fk_taxonid)) +
        geom_bar(position = position_dodge(
          preserve = "single")) +
        guides(fill = guide_legend(title = "Species")) +
        scale_x_discrete(drop = FALSE) +
        theme_minimal() +
        scale_x_discrete(drop = FALSE) +
        theme(
          legend.position = "top",
          text = element_text(size = 10),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
        labs(
          title = "Media Files with Verified Annotations of Key Species per Year",
          y = "Count",
          x = "Year") 
      
      if (show_plots == TRUE) results$gg_valid_annotations_media_year_keysp
      
      
      # media with verified annotations for key taxa by month ===============
      
      results$gg_valid_annotations_media_month_keysp <- ggplot2::ggplot(
        anno_verifications,
        aes(
          x = as.factor(month),
          fill = fk_taxonid)) +
        geom_bar(position = position_dodge(
          preserve = "single")) +
        guides(fill = guide_legend(title = "Species")) +
        scale_x_discrete(drop = FALSE) +
        theme_minimal() +
        theme(
          legend.position = "top",
          text = element_text(size = 10),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
        labs(
          title = "Media Files with Verified Annotations of Key Species per Month",
          y = "Count",
          x = "Month") 
      
      if (show_plots == TRUE) results$gg_valid_annotations_media_month_keysp
      
      
      
      # get verifications for unique visits
      unique_visits_per_sp <- unique(
        anno_verifications[c('fk_taxonid', 'fk_visitid')])
      
      anno_verifications <- anno_verifications[row.names(unique_visits_per_sp),]
      
      
      # media with verified annotations for key taxa by year ===============
      
      results$gg_valid_annotations_visits_year_keysp <- ggplot2::ggplot(
        anno_verifications,
        aes(
          x = as.factor(year),
          fill = fk_taxonid)) +
        geom_bar(position = position_dodge(
          preserve = "single")) +
        guides(fill = guide_legend(title = "Species")) +
        theme_minimal() +
        theme(
          legend.position = "top",
          text = element_text(size = 10),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
        labs(title = "Visits with Verified Annotations of Key Species per Year",
             y = "Count",
             x = "Year") 
      
      if (show_plots == TRUE) results$gg_valid_annotations_visits_year_keysp
      
    } # end if key_species is not null
    
    
  } # end if annotation verifications are present
  
  # send a message
  message(paste0("The output list contains ", length(results), " ggplot objects."))
  
  # return results
  return(results)

} # end of function
