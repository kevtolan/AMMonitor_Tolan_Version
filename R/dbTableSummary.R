#' @name dbTableSummary
#' @aliases dbTableSummary
#' @title Creates database summary figures 
#' @description Creates database summary tables
#' for inspection of outliers and for use in reporting
#' @param summary_data A list of dataframes generated from dbGetSummaryData()
#' @param key_species A vector of up to ten key taxa to include for annotations 
#' and verifications tables. Default includes all taxa.
#' @usage dbTableSummary(summary_data, key_species = NULL)
#' @export
#' @import gt
#' @importFrom stats aggregate
#' @importFrom data.table dcast data.table dcast.data.table
#' @details  dbTableSummary() provides table summaries of different elements 
#' of an AMMonitor database.  A primary input is a list generated from the 
#' function \code{dbGetsummaryData()}, which extracts portions of the database for
#' inspection and summary. 
#' The output is a list of gt table objects, which can be  
#' manipulated by the user using the gt package to create professional tables.
#' 
#' See the "dbsummary" learnr tutorial for more details on the
#' dbTableSummary() function.  The tutorial can be launched with
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
#' results <- dbGetSummaryData(con = conx, disconnect = FALSE)
#'  
#' # look at the returned object; it is a list of dataframes
#' str(results, max.level = 1)
#' 
#' # generate summary tables
#' table_results <- dbTableSummary(
#'  summary_data = results,
#'  key_species = c("moose", "black bear")
#'  )
#'  
#' str(table_results, max.level = 1)
#' 
#' # each output is saved in a list element that can be retrieved and edited
#' 
#' # let's take a look at the collected media files by year and location
#' collected_media <- table_results$media_collected_per_year_location
#' 
#' # look at this table
#' collected_media
#' 
#' # change column names
#' collected_media <- collected_media |> 
#'   gt::cols_label(
#'     year = "Year",
#'     fk_locationid = "Location ID",
#'     collected_media_files = "Collected Media Files") 
#'     
#' # add source note
#' collected_media <- collected_media |>
#'   gt::tab_source_note(
#'     source_note = "Source: AMMonitor Demo Dataset")
#'     
#' # look at the updated table
#' collected_media
#' 
#' # save the table back to the list if desired
#' table_results$media_collected_per_year_location <- collected_media
#' 
#' # refer to the gt package for more ideas on manipulating your results tables 
#' 
#' 
#' 
#' } 
#' 
#' 
dbTableSummary <- function(summary_data, key_species = NULL){

 # create list to store results
  results <- list()

  # get taxa table
  taxa <- summary_data$taxa
  
  # check key species vector
  if (!is.null(key_species)) {
    if (length(key_species) > 10) {
      stop("Please enter a vector of 10 taxa or less. The default for key_species includes all taxa.")
    }
    
    for (sp in tolower(key_species)) {
      if (!sp %in% tolower(taxa$pk_taxonid) & !sp %in% tolower(taxa$common_name)) {
        stop(paste0("'", sp, "' is not present in the annotations table. Please check this table for valid taxon ids and common names, or run the function without the key_species argument."))
      }
    }
  } # end checking key_species input
  
  # media ================================
  media <- summary_data$media
  media$count <- 1
  
  ## media by year and type ===========
  
  summarized_media <- stats::aggregate(
    x = count ~ year + media_type,
    data = media,
    FUN = sum)

  # order the table
   media_count_year_type <- gt::gt(
     data = summarized_media
   )
     
  # check it
   media_count_year_type <- media_count_year_type |>
     gt::tab_header(
       title = "Collected Media Files",
       subtitle = "by Year and Media Type"
     ) |>
     gt::cols_label(
       year = "Year",
       media_type = "Media Type",
       count = "Count"
     ) |>
     gt::cols_align(
       align = "center"
     )
  
   # add to results
  results$media_count_year_type <- media_count_year_type
  
  # visits associated with media ======================
  visits <- summary_data$media_visits
  
  visits$visit_type <- factor(visits$visit_type, levels = c("set", "check", "pull", "sync"))
  
  # indices <- which(is.na(visits$visit_type))
  # visits$visit_type[indices] <- "unknown/NA"
  visits$count <- 1
  
  ## media visits by year and location ===========

  media_visits_year_location <- stats::aggregate(
    x = count ~ year + fk_locationid,
    data = visits,
    FUN = sum)
  
  media_visits_year_location <- media_visits_year_location[
    order(
      media_visits_year_location$year,
      media_visits_year_location$fk_locationid)
    ,]

  
  media_visits_year_location <- gt::gt(media_visits_year_location) |>
    gt::tab_header(
      title = "Visits Associated with Collected Media",
      subtitle = "by Year and Location"
    ) |>
    gt::cols_label(
      fk_locationid = "Location ID",
      year = "Year",
      count = "Total Visits"
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$media_visits_year_location <- media_visits_year_location
  
  ## media visits by year only =============
  
  media_visits_year <- stats::aggregate(
    x = count ~ year,
    data = visits,
    FUN = sum)
  
  media_visits_year <- gt::gt(media_visits_year) |>
    gt::tab_header(
      title = "Visits Associated with Collected Media",
      subtitle = "by Year"
    ) |>
    gt::cols_label(
      year = "Year",
      count = "Total Visits"
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$media_visits_year <- media_visits_year
  
  ## media visits by visit type ==============

  visit_type_by_year <- t(sapply(
    unique(visits$year), 
    function(x) {
      table(visits$visit_type[visits$year == x])
    }
  ))
  visit_type_by_year <- cbind(
    year = row.names(visit_type_by_year), 
    data.frame(visit_type_by_year)
  )

  media_visits_visittype_year <- gt::gt(visit_type_by_year) |>
    gt::tab_header(
      title = "Total Visits Associated with Collected Media"
    ) |>
    gt::cols_label(
      year = "Year",
      set = "Set Visits",
      check = "Check Visits",
      pull = "Pull Visits",
      sync = "Sync Visits"
    ) |>
    gt::cols_align(
      align = "center"
    )

  results$media_visits_visittype_year <- media_visits_visittype_year
  
  ## media visits by visit type and location and year ==============
  
  locations <- sort(unique(visits$fk_locationid))
  
  # Set grid for all location/year combos
  media_visits_visittype_year_location <- expand.grid(locations, unique(visits$year))
  names(media_visits_visittype_year_location) <- c('location', 'year')
  
  # Add columns for each visit type
  for (x in levels(visits$visit_type)) {
    media_visits_visittype_year_location[x] <- NA
  }

  # Populate visit counts in each row
  for (i in seq_len(nrow(media_visits_visittype_year_location))) {
    media_visits_visittype_year_location[i,3:6] <- table(visits$visit_type[visits$year == media_visits_visittype_year_location$year[i] & visits$fk_locationid == media_visits_visittype_year_location$location[i]])
  }

  # Drop rows with no visits
  media_visits_visittype_year_location <- media_visits_visittype_year_location[
    rowSums(media_visits_visittype_year_location[,3:6]) != 0,
  ]

  media_visits_visittype_year_location <- gt::gt(
    as.data.frame(media_visits_visittype_year_location)) |>
    gt::tab_header(
      title = "Total Visits Associated with Collected Media",
      subtitle = "by Year and Location"
    ) |>
    gt::cols_label(
      location = "Location ID",
      year = "Year",
      set = "Set Visits",
      check = "Check Visits",
      pull = "Pull Visits",
      sync = "Sync Visits"
    ) |>
    gt::cols_align(
      align = "center"
    )

  results$media_visits_visittype_year_location <- media_visits_visittype_year_location
  
  ## collected media files per location and year ============
  
  # get count of media files and add to visits
  visits$collected_media_files <- NA
  
  for (i in 1:nrow(visits)) {
    visits$collected_media_files[i] <- length(
      which(media$fk_visitid == visits$pk_visitid[i])
      )
  }
  
  
  media_collected_per_year_location <- stats::aggregate(
    x = collected_media_files ~ year + fk_locationid,
    data = visits,
    FUN = sum)
  
  media_collected_per_year_location <- media_collected_per_year_location[
    order(
      media_collected_per_year_location$year,
      media_collected_per_year_location$fk_locationid)
    ,]
  
  
  media_collected_per_year_location <- gt::gt(
    media_collected_per_year_location) |>
    gt::tab_header(
      title = "Collected Media Files",
      subtitle = "by Year and Location"
    ) |>
    gt::cols_label(
      fk_locationid = "Location ID",
      year = "Year",
      collected_media_files = "Total Files"
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$media_collected_per_year_location <- media_collected_per_year_location
  
  
  # survey effort for visits ==============

  survey_effort <- summary_data$survey_effort 
  
  survey_effort$activeStartLB <- as.Date(survey_effort$activeStartLB)
  survey_effort$activeEndLB <- as.Date(survey_effort$activeEndLB)
  survey_effort$activeStartUB <- as.Date(survey_effort$activeStartUB)
  survey_effort$activeEndUB <- as.Date(survey_effort$activeEndUB)
  survey_effort$effortLB <- as.numeric(
    difftime(
      survey_effort$activeEndLB,
      survey_effort$activeStartLB,
      units = 'days'))
  
  survey_effort$effortUB <- as.numeric(
    difftime(
      survey_effort$activeEndUB,
      survey_effort$activeStartUB,
      units = 'days'))
  
  field_visits <- summary_data$field_visits
  all_field_visits <- summary_data$all_field_visits

  
  effort_table <- survey_effort
  
  effort_table$year <- format(effort_table$activeEndUB, '%Y')

  # get average survey effort per year
  effort_per_year <- data.frame() 
  
  for (year in unique(effort_table$year)) {
    
    visits_for_year <- effort_table[
      which(effort_table$year == year)
      ,]
    
    new_data <- data.frame(
      survey_year = year,
      effortLB = round(mean(visits_for_year$effortLB), 2),
      effortUB = round(mean(visits_for_year$effortUB), 2)
    )
    
    effort_per_year <- rbind(effort_per_year, new_data)
  } # end effort per year
  
  
  effort_per_year <- gt::gt(effort_per_year) |>
    gt::tab_header(
      title = "Average Survey Effort Across All Locations Per Year",
      subtitle = "Lower Bound and Upper Bound Estimates"
    ) |>
    gt::cols_label(
      survey_year = "Year",
      effortLB = html("Lower Bound Estimate<br>(Days)"),
      effortUB = html("Upper Bound Estimate<br>(Days)")
    ) |>
    gt::tab_source_note(
      "Lower bound estimates for each visit were calculated from the minimum and maximum media dates for that visit. The upper bound estimates for each visit were calculated as the difference between a visit's date and the date of the previous visit at that location."
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$survey_effort_year <- effort_per_year

  ## effort per location
  effort_per_location <- data.frame() 
  
  for (loc in unique(effort_table$fk_locationid)) {
    
    visits_for_location <- effort_table[which(
      effort_table$fk_locationid == loc),]
    
    new_data <- data.frame(
      survey_loc = loc,
      effortLB = round(mean(visits_for_location$effortLB, na.rm = TRUE), 2),
      effortUB = round(mean(visits_for_location$effortUB, na.rm = TRUE),2)
    )
    
    effort_per_location <- rbind(effort_per_location, new_data)
  } # end effort per location 
  
  effort_per_location <- effort_per_location[
    order(effort_per_location$survey_loc)
    ,]
  
  
  effort_per_loc <- gt::gt(effort_per_location) |>
    gt::tab_header(
      title = "Average Survey Effort Per Location",
      subtitle = "Lower Bound and Upper Bound Estimates"
    ) |>
    gt::cols_label(
      survey_loc = "Location ID",
      effortLB = html("Lower Bound Estimate<br>(Days)"),
      effortUB = html("Upper Bound Estimate<br>(Days)")
    ) |>
    gt::tab_source_note(
      "Lower bound estimates for each visit were calculated from the minimum and maximum media dates for that visit. The upper bound estimates for each visit were calculated as the difference between a visit's date and the date of the previous visit at that location."
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  
  results$survey_effort_location <- effort_per_loc

  # field visits ==================
  
  field_visits <- summary_data$field_visits
  field_visits$count <- 1
  
  ## field visits by location and person id ============
  
  summary_field_visits_total <- stats::aggregate(
    x = count ~ year + fk_locationid + fk_personid,
    data = field_visits,
    FUN = sum)
  
  summary_field_visits_total <- summary_field_visits_total[
    order(summary_field_visits_total$fk_locationid)
    ,]
  
  field_visits_count_by_location_person_total <- gt::gt(
    data = summary_field_visits_total
    ) |>
    gt::tab_header(
      title = "Total Field Visits",
      subtitle = "by Year, Location, and Person"
    ) |>
    gt::cols_label(
      year = "Year",
      fk_locationid = "Location ID",
      fk_personid = "Person ID",
      count = "Count"
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$field_visits_count_by_location_person_total <- field_visits_count_by_location_person_total
  
  
  ## field visits by type only =================
  summary_field_visits_type <- stats::aggregate(
    x = count ~ visit_type,
    data = field_visits,
    FUN = sum)
  
  field_visits_count_by_type <- gt::gt(data = summary_field_visits_type) |>
    gt::tab_header(
      title = "Total Field Visits by Type"
    ) |>
    gt::cols_label(
      visit_type = "Visit Type",
      count = "Count"
    ) |>
    gt::cols_align(
      align = "center"
    )

  
  results$field_visits_count_by_type <- field_visits_count_by_type 
  

  
  ## field visits by type, location, and person id ==================
  summary_field_visits <- stats::aggregate(
    x = count ~ year + fk_locationid + fk_personid + visit_type,
    data = field_visits,
    FUN = sum)
  
  summary_field_visits <- data.table::dcast.data.table(
    data = data.table::data.table(summary_field_visits),
    formula = year + fk_locationid + fk_personid ~ visit_type,
    value.var = "count")
  
  summary_field_visits[is.na(summary_field_visits)] <- 0
  
  visit_types_names <- colnames(
    summary_field_visits[, 4:ncol(summary_field_visits)])

  field_visits_count_by_type_location_person <- gt::gt(
    data = summary_field_visits
    ) |>
    gt::tab_header(
      title = "Total Field Visits",
      subtitle = "by Year, Location, Person, and Visit Type"
    ) |>
    gt::cols_label(
      year = "Year",
      fk_locationid = "Location ID",
      fk_personid = "Person ID"
    ) |>
    gt::tab_spanner(
      label = "Visit Type",
      columns = visit_types_names
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$field_visits_count_by_type_location_person <- field_visits_count_by_type_location_person

  
  ## field visits and date checked (check or pull) per location and year =======
  
  check_pull_visits <- field_visits[
    which(field_visits$visit_type == "check" 
          | field_visits$visit_type == "pull")
    ,]
  
  
  check_pull_visits_by_month <- stats::aggregate(
    x = count ~ fk_locationid + year + month + fk_personid,
    data = check_pull_visits,
    FUN = sum)
  
  check_pull_visits_by_month <- check_pull_visits_by_month[
    order(
      check_pull_visits_by_month$fk_locationid)
    ,]
  
  check_pull_visits_by_month <- gt::gt(check_pull_visits_by_month) |>
    gt::tab_header(
      title = "Check and Pull Visits by Month and Year"
    ) |>
    gt::cols_label(
      fk_locationid = "Location ID",
      year = "Year",
      month = "Month",
      fk_personid = "Person ID",
      count = "Count"
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$check_pull_visits_by_month <- check_pull_visits_by_month
  
  
  ## check and pull visits with julian dates ==================
  check_pull_julian <- check_pull_visits
  check_pull_julian$visit_date <-  format(check_pull_julian$visit_date, "%j")
  
  check_pull_visits_julian <- stats::aggregate(
    x = count ~ fk_locationid + year + visit_date + fk_personid,
    data = check_pull_julian,
    FUN = sum)
  
  check_pull_visits_julian <- check_pull_visits_julian[
    order(
      check_pull_visits_julian$year,
      check_pull_visits_julian$fk_locationid),
    c("fk_locationid",
      "year",
      "visit_date",
      "fk_personid" )]
  
  
  
  check_pull_visits_julian <- gt::gt(check_pull_visits_julian) |>
    gt::tab_header(
      title = "Check and Pull Visits by Julian Date per Year"
    ) |>
    gt::cols_label(
      fk_locationid = "Location ID",
      fk_personid = "Person ID",
      year = "Year",
      visit_date = "Date Checked (Julian)"
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$check_pull_visits_julian <- check_pull_visits_julian
  
  

  
  
  # locations ===============
  locations <- summary_data$locations
  
  ## locations by type and status ====================
  locations_trimmed <- locations[, c("pk_locationid",
                                     "location_type",
                                     "location_status")]
  
  locations_trimmed <- locations_trimmed[
    order(locations_trimmed$pk_locationid)
    ,]
  
  locations_status <- gt::gt(data = locations_trimmed) |>
    gt::tab_header(
      title = "Type and Status of All Locations"
    ) |>
    gt::cols_label(
      pk_locationid = "Location ID",
      location_type = "Type",
      location_status = "Status"
    ) |>
    gt::cols_align(
      align = "center"
    )
  
  results$locations_status <- locations_status
  
 # annotations  =================
  annotations <- summary_data$annotations
  if (nrow(annotations) != 0) {
    
    annotations$count <- 1
    
    annotations_people <- stats::aggregate(
      x = count ~ year + fk_personid,
      data = annotations,
      FUN = sum)
    
    annotations_people <- annotations_people[
      order(
        annotations_people$year,
        annotations_people$fk_personid)
      ,]
  
    annotations_people <- data.table::dcast.data.table(
      data = data.table::data.table(annotations_people),
      formula = fk_personid ~ year,
      value.var = "count")
    
    annotations_people[is.na(annotations_people)] <- 0
    
    annotations_people <- annotations_people[
      order(annotations_people$fk_personid)
      ,]
    
    
    annotations_people <- gt::gt(annotations_people) |>
      gt::tab_header(
        title = "Total Annotations per Person per Year"
      ) |>
      gt::cols_label(
        fk_personid = "Person ID"
      ) |>
      gt::cols_align(
        align = "center"
      )
    
    results$total_annotations_per_person <- annotations_people
    
   
    if (is.null(key_species)) {
      
      # produce tables for all species 
      
      # get only annotations for unique media
      annnotations <- annotations[which(
        annotations$fk_mediaid %in% unique(annotations$fk_mediaid)),]
  
        ## annotations by year, location, and species (all taxa) =============
      annotations_by_location_year_sp <- stats::aggregate(
        x = count ~ fk_taxonid + year + fk_locationid,
        data = annotations,
        FUN = sum)
      
      annotations_by_location_year_sp <- data.table::dcast(
        data = data.table::data.table(annotations_by_location_year_sp),
        formula = year + fk_locationid ~ fk_taxonid,
        value.var = "count")
      
      annotations_by_location_year_sp[is.na(annotations_by_location_year_sp)] <- 0
      
      annotations_by_location_year_sp <- annotations_by_location_year_sp[
        order(
          annotations_by_location_year_sp$year,
          annotations_by_location_year_sp$fk_locationid)
        ,]
      
      annotations_by_location_year_sp <- gt::gt(annotations_by_location_year_sp) |>
        gt::tab_header(
          title = "Annotated Media Files for All Taxa",
          subtitle = "by Year and Location"
        ) |>
        gt::cols_label(
          year = "Year",
          fk_locationid = "Location ID"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_location_year_sp <- annotations_by_location_year_sp
      
      ## annotations by location (all taxa) =====================
      annotations_by_location_sp <- stats::aggregate(
        x = count ~ fk_taxonid + fk_locationid,
        data = annotations,
        FUN = sum)
      
      annotations_by_location_sp <- data.table::dcast(
        data = data.table::data.table(annotations_by_location_sp),
        formula = fk_locationid ~ fk_taxonid,
        value.var = "count")
      
      annotations_by_location_sp[is.na(annotations_by_location_sp)] <- 0
      
      annotations_by_location_sp <- annotations_by_location_sp[
        order(annotations_by_location_sp$fk_locationid)
        ,]
      
      
      annotations_by_location_sp <- gt::gt(annotations_by_location_sp) |>
        gt::tab_header(
          title = "Annotated Media Files for All Taxa",
          subtitle = "by Location"
        ) |>
        gt::cols_label(
          fk_locationid = "Location ID"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_location_sp <- annotations_by_location_sp
      
      ## annotations per year (all taxa) ===========================
      annotations_by_year_sp <- stats::aggregate(
        x = count ~ fk_taxonid + year,
        data = annotations,
        FUN = sum)
      
      annotations_by_year_sp <- data.table::dcast(
        data = data.table::data.table(annotations_by_year_sp),
        formula = year ~ fk_taxonid,
        value.var = "count")
      
      annotations_by_year_sp[is.na(annotations_by_year_sp)] <- 0
      
      
      annotations_by_year_sp <- gt::gt(annotations_by_year_sp) |>
        gt::tab_header(
          title = "Annotated Media Files for All Taxa",
          subtitle = "by Year"
        ) |>
        gt::cols_label(
          year = "Year"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_year_sp <- annotations_by_year_sp
      
      
      ## annotations per year (all taxa) ===========================
      annotations_by_month_sp <- stats::aggregate(
        x = count ~ month + fk_taxonid ,
        data = annotations,
        FUN = sum)
      
      annotations_by_month_sp <- data.table::dcast(
        data = data.table::data.table(annotations_by_month_sp),
        formula = month ~ fk_taxonid,
        value.var = "count")
      
      annotations_by_month_sp[is.na(annotations_by_month_sp)] <- 0
      
      
      annotations_by_month_sp <- gt::gt(annotations_by_month_sp) |>
        gt::tab_header(
          title = "Annotated Media Files for All Taxa",
          subtitle = "by Month"
        ) |>
        gt::cols_label(
          month = "Month"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_month_sp <- annotations_by_month_sp
      
      
      
        # annotations by only species (all taxa) ==============
      
      annotations_by_sp <- stats::aggregate(
        x = count ~ fk_taxonid,
        data = annotations,
        FUN = sum)
      
      annotations_by_sp <- gt::gt(annotations_by_sp) |>
        gt::tab_header(
          title = "Annotations by Species"
        ) |>
        gt::cols_label(
          fk_taxonid = "Species",
          count = "Annotated Media Files"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_sp <- annotations_by_sp
      
    
      ## annotations per visit all taxa ===============
      
      # get annotations for unique visits per sp
      unique_visits_per_sp <- unique(
        annotations[c('fk_taxonid', 'fk_visitid')])
      
      annotations <- annotations[row.names(unique_visits_per_sp),]
       
      ## annotations by visit for year, location, and species (all taxa) =========
      annotations_by_location_year_sp <- stats::aggregate(
        x = count ~ fk_taxonid + year + fk_locationid,
        data = annotations,
        FUN = sum)
      
      annotations_by_location_year_sp <- data.table::dcast(
        data = data.table::data.table(annotations_by_location_year_sp),
        formula = year + fk_locationid ~ fk_taxonid,
        value.var = "count")
      
      annotations_by_location_year_sp[
          is.na(annotations_by_location_year_sp)
        ] <- 0
      
      annotations_by_location_year_sp <- annotations_by_location_year_sp[
        order(
          annotations_by_location_year_sp$year,
          annotations_by_location_year_sp$fk_locationid)
        ,]
      
      annotations_by_location_year_sp <- gt::gt(
        data = annotations_by_location_year_sp
        ) |>
        gt::tab_header(
          title = "Total Visits with Annotations for All Taxa",
          subtitle = "by Year and Location"
        ) |>
        gt::cols_label(
          year = "Year",
          fk_locationid = "Location ID"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_location_year_sp <- annotations_by_location_year_sp
      
      ## annotations by visit per year (all taxa) ===========================
      annotations_by_year_sp <- stats::aggregate(
        x = count ~ fk_taxonid + year,
        data = annotations,
        FUN = sum)
      
      annotations_by_year_sp <- data.table::dcast(
        data = data.table::data.table(annotations_by_year_sp),
        formula = year ~ fk_taxonid,
        value.var = "count")
      
      annotations_by_year_sp[is.na(annotations_by_year_sp)] <- 0
      
      
      annotations_by_year_sp <- gt::gt(annotations_by_year_sp) |>
        gt::tab_header(
          title = "Total Visits with Annotations for All Taxa",
          subtitle = "by Year"
        ) |>
        gt::cols_label(
          year = "Year"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_year_sp <- annotations_by_year_sp
      
      
      ## annotations by location and species (all taxa) =====================
      annotations_by_location_sp <- stats::aggregate(
        x = count ~ fk_taxonid + fk_locationid,
        data = annotations,
        FUN = sum)
      
      annotations_by_location_sp <- data.table::dcast(
        data = data.table::data.table(annotations_by_location_sp),
        formula = fk_locationid ~ fk_taxonid,
        value.var = "count")
      
      annotations_by_location_sp[is.na(annotations_by_location_sp)] <- 0
      
      annotations_by_location_sp <- annotations_by_location_sp[order(
        annotations_by_location_sp$fk_locationid),]
      
      annotations_by_location_sp <- gt::gt(annotations_by_location_sp) |>
        gt::tab_header(
          title = "Total Visits with Annotations for All Taxa",
          subtitle = "by Location"
        ) |>
        gt::cols_label(
          fk_locationid = "Location ID"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_location_sp <- annotations_by_location_sp
      
      
      # annotations by only species (all taxa) ==============
      
      annotations_by_sp <- stats::aggregate(
        x = count ~ fk_taxonid,
        data = annotations,
        FUN = sum)
      
      annotations_by_sp <- gt::gt(annotations_by_sp) |>
        gt::tab_header(
          title = "Total Visits with Annotations by Species"
        ) |>
        gt::cols_label(
          fk_taxonid = "Species",
          count = "Total Visits"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_sp <- annotations_by_sp
      
      
      
      
      
      
      ## annotation verifications ====================

      anno_verifications <- summary_data$annotationverifications
      
      if (nrow(anno_verifications) > 0) {
        
        # update tagger id column name to avoid conflicts between two fk_personid cols
        names(anno_verifications)[which(
          names(anno_verifications) == "fk_personid:1")] <- "tagger_id"
        
        valid_annoverifications_by_people <- stats::aggregate(
          x = is_valid ~ fk_taxonid + tagger_id + fk_personid,
          data = anno_verifications,
          FUN = sum)
        
        valid_annoverifications_by_people <- data.table::dcast(
          data = data.table::data.table(valid_annoverifications_by_people),
          formula = tagger_id + fk_personid ~ fk_taxonid,
          value.var = "is_valid")
        
        valid_annoverifications_by_people[is.na(
          valid_annoverifications_by_people)] <- 0
        
        valid_annoverifications_by_people <- gt::gt(
          valid_annoverifications_by_people) |>
          gt::tab_header(
            title = "Verified Annotations"
          ) |>
          gt::cols_align(
            align = "center"
          ) |> 
          gt::cols_label(
            tagger_id = "Tagger ID",
            fk_personid = "Verifier ID",
          )
        
        results$valid_annotations_by_people <- valid_annoverifications_by_people
        
        
        ## invalid annotation verifications by people
        
        anno_verifications$count_invalid <- 0
        
        for (i in 1:nrow(anno_verifications)) {
          
          if (anno_verifications$is_valid[i] == 0) {
            anno_verifications$count_invalid[i] <- 1
          }
          
        } # end adding invalid count
        
        invalid_annoverifications_by_people <- stats::aggregate(
          x = count_invalid ~ fk_taxonid + tagger_id + fk_personid,
          data = anno_verifications,
          FUN = sum)
        
        invalid_annoverifications_by_people <- data.table::dcast(
          data = data.table::data.table(invalid_annoverifications_by_people),
          formula = tagger_id + fk_personid ~ fk_taxonid,
          value.var = "count_invalid")
        
        invalid_annoverifications_by_people[is.na(
          invalid_annoverifications_by_people)] <- 0
        
        invalid_annoverifications_by_people <- gt::gt(
          invalid_annoverifications_by_people) |>
          gt::tab_header(
            title = "Invalid Annotations"
          ) |>
          gt::cols_align(
            align = "center"
          ) |>
          gt::cols_label(
            tagger_id = "Tagger ID",
            fk_personid = "Verifier ID"
          )
        
        
        results$invalid_annotations_by_people <- invalid_annoverifications_by_people
        
        
        ## valid annotations from verifications ==============

        # remove invalid annotations from verifications
        anno_verifications <- anno_verifications[
          which(anno_verifications$is_valid == 1),]
        
        # get unique media
        unique_media_per_sp <- unique(
          anno_verifications[c('fk_taxonid', 'fk_mediaid')])
        
        anno_verifications <- anno_verifications[row.names(unique_media_per_sp),]
        
        # Verified annotations for all sp., location + year
        
        valid_annotations_location_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_year_sp),
          formula = year + fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_year_sp[
          is.na(valid_annotations_location_year_sp)
        ] <- 0
        
        valid_annotations_location_year_sp <- valid_annotations_location_year_sp[
          order(
            valid_annotations_location_year_sp$year,
            valid_annotations_location_year_sp$fk_locationid
          )
        ]
        
        valid_annotations_location_year_sp <- gt::gt(
          data = valid_annotations_location_year_sp
        ) |>
          gt::tab_header(
            title = "Media Files with Verified Annotations for All Taxa",
            subtitle = "by Year and Location"
          ) |>
          gt::cols_label(
            year = "Year",
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_media_loc_year <- valid_annotations_location_year_sp
        
        # Valid annotations by location
        
        valid_annotations_location_sp <- stats::aggregate(
          x = is_valid ~ fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_sp),
          formula = fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_sp[
          is.na(valid_annotations_location_sp)
        ] <- 0
        
        valid_annotations_location_sp <- valid_annotations_location_sp[
          order(valid_annotations_location_sp$fk_locationid)
        ]
        
        valid_annotations_location_sp <- gt::gt(
          data = valid_annotations_location_sp
        ) |>
          gt::tab_header(
            title = "Media Files with Verified Annotations for All Taxa",
            subtitle = "by Location"
          ) |>
          gt::cols_label(
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_media_loc <- valid_annotations_location_sp
        
        
        # valid annotations by year for all taxa
        
        valid_annotations_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_year_sp),
          formula = year ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_year_sp[
          is.na(valid_annotations_year_sp)
        ] <- 0
        
        valid_annotations_year_sp <- valid_annotations_year_sp[
          order(valid_annotations_year_sp$year)
        ]
        
        valid_annotations_year_sp <- gt::gt(
          data = valid_annotations_year_sp
        ) |>
          gt::tab_header(
          title = "Media Files with Verified Annotations for All Taxa",
          subtitle = "by Year"
        ) |>
          gt::cols_label(
            year = "Year"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        
        results$valid_annotations_media_year <- valid_annotations_year_sp
        
        
        # valid annotations by month for all taxa 
        
        valid_annotations_month_sp <- stats::aggregate(
          x = is_valid ~ month + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_month_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_month_sp),
          formula = month ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_month_sp <- valid_annotations_month_sp[
          order(valid_annotations_month_sp$month)
        ]
        
        valid_annotations_month_sp[is.na(valid_annotations_month_sp)] <- 0
        
        valid_annotations_month_sp <- gt::gt(
          data = valid_annotations_month_sp
        ) |>
          gt::tab_header(
            title = "Media Files with Verified Annotations for All Taxa",
            subtitle = "by Month"
          ) |>
          gt::cols_label(
            month = "Month"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        
        results$valid_annotations_media_month <- valid_annotations_month_sp
        
        
        ## valid annotations, media files per sp 
        
        valid_annotations_sp <- aggregate(
          x = is_valid ~ fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_sp <- valid_annotations_sp[
          order(valid_annotations_sp$fk_taxonid),
        ]
        
        valid_annotations_sp <- gt::gt(
          data = valid_annotations_sp
        ) |>
          gt::tab_header(
            title = "Total Media Files with Verified Annotations for All Taxa"
          ) |>
          gt::cols_label(
            fk_taxonid = "Species",
            is_valid = "Annotated Media"
          ) |>
          gt::cols_align(
            align = "center"
          )
          
        results$valid_annotations_media_total_sp <- valid_annotations_sp
        
        ## verifications by visit for all taxa ================
        
        # get unique visits per sp
        unique_visits_per_sp <- unique(
          anno_verifications[c('fk_taxonid', 'fk_visitid')])
        
        anno_verifications <- anno_verifications[row.names(unique_visits_per_sp),]
        
        
        # Verified annotations by visit for all sp., location + year 
        
        valid_annotations_location_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_year_sp),
          formula = year + fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_year_sp[
          is.na(valid_annotations_location_year_sp)
        ] <- 0
        
        valid_annotations_location_year_sp <- valid_annotations_location_year_sp[
          order(
            valid_annotations_location_year_sp$year,
            valid_annotations_location_year_sp$fk_locationid
          )
        ]
        
        valid_annotations_location_year_sp <- gt::gt(
          data = valid_annotations_location_year_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for All Taxa",
            subtitle = "by Year and Location"
          ) |>
          gt::cols_label(
            year = "Year",
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_visits_loc_year <- valid_annotations_location_year_sp
        
        # Valid annotations by visit by location
        
        valid_annotations_location_sp <- stats::aggregate(
          x = is_valid ~ fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_sp),
          formula = fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_sp[
          is.na(valid_annotations_location_sp)
        ] <- 0
        
        valid_annotations_location_sp <- valid_annotations_location_sp[
          order(valid_annotations_location_sp$fk_locationid)
        ]
        
        valid_annotations_location_sp <- gt::gt(
          data = valid_annotations_location_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for All Taxa",
            subtitle = "by Location"
          ) |>
          gt::cols_label(
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_visits_loc <- valid_annotations_location_sp
       
        # valid annotations by year for all taxa 
        
        valid_annotations_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_year_sp),
          formula = year ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_year_sp[
          is.na(valid_annotations_year_sp)
        ] <- 0
        
        valid_annotations_year_sp <- valid_annotations_year_sp[
          order(valid_annotations_year_sp$year)
        ]
        
        valid_annotations_year_sp <- gt::gt(
          data = valid_annotations_year_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for All Taxa",
            subtitle = "by Year"
          ) |>
          gt::cols_label(
            year = "Year"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        
        results$valid_annotations_visits_year <- valid_annotations_year_sp
       
        ## valid annotations by visit per sp 
        
        valid_annotations_sp <- aggregate(
          x = is_valid ~ fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_sp <- valid_annotations_sp[
          order(valid_annotations_sp$fk_taxonid),
        ]
        
        valid_annotations_sp <- gt::gt(
          data = valid_annotations_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for All Taxa"
          ) |>
          gt::cols_label(
            fk_taxonid = "Species",
            is_valid = "Total Visits"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_visits_total_sp <- valid_annotations_sp
        
      } # end annotations verifications for all species (if nrow != 0)
      
    } else {
        
        # get key species annotations only
        annotations_key <- annotations[which(
          tolower(annotations$fk_taxonid) %in% tolower(key_species) 
          | tolower(annotations$common_name) %in% tolower(key_species)),]
        
        
        ## annotations per person per year ================
        annotations_people_key_sp <- stats::aggregate(
          x = count ~ fk_taxonid + year + fk_personid,
          data = annotations_key,
          FUN = sum)
        
        annotations_people_key_sp <- data.table::dcast(
          data = data.table::data.table(annotations_people_key_sp),
          formula = year + fk_personid ~ fk_taxonid,
          value.var = "count")
        
        annotations_people_key_sp[is.na(annotations_people_key_sp)] <- 0
        
        annotations_people_key_sp <- annotations_people_key_sp[order(
          annotations_people_key_sp$year,
          annotations_people_key_sp$fk_personid),]
        
        annotations_people_key_sp <- gt::gt(annotations_people_key_sp) |>
          gt::tab_header(
            title = "Key Species Annotations",
            subtitle = "by Person and Year"
          ) |>
          gt::cols_label(
            year = "Year",
            fk_personid = "Person ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$annotations_people_key_sp <- annotations_people_key_sp
        
        ## annotations by unique media ==================
        # get unique media ids only
        
        unique_media_per_sp <- unique(
          annotations_key[c('fk_taxonid', 'fk_mediaid')])
        
        annotations_key <- annotations_key[row.names(unique_media_per_sp),]
      
      # media with annotations per year and location (key taxa) ===============
      key_annotations_full <- stats::aggregate(
        x = count ~ fk_taxonid + year + fk_locationid,
        data = annotations_key,
        FUN = sum)
        
      media_w_key_annotations_location_year <- data.table::dcast(
        data = data.table::data.table(key_annotations_full),
        formula = year + fk_locationid ~ fk_taxonid,
        value.var = "count")
    
      media_w_key_annotations_location_year[is.na(
        media_w_key_annotations_location_year)] <- 0
      
      
      media_w_key_annotations_location_year <- media_w_key_annotations_location_year[
        order(
          media_w_key_annotations_location_year$year,
          media_w_key_annotations_location_year$fk_locationid
          )
        ,]
      
      media_w_key_annotations_location_year <- gt::gt(
        media_w_key_annotations_location_year) |>
        gt::tab_header(
          title = "Media files with Annotations of Key Species",
          subtitle = "by Year and Location"
        ) |>
        gt::cols_label(
          fk_locationid = "Location ID",
          year = "Year"
        ) |>
        gt::cols_align(
          align = "center"
        )
        
      results$annotations_media_location_year_keysp <- media_w_key_annotations_location_year
      
      # annotations per location (key taxa) ======================
      media_w_key_annotations_location <- stats::aggregate(
        x = count ~ fk_taxonid + fk_locationid,
        data = annotations_key,
        FUN = sum)
      
      media_w_key_annotations_location <- data.table::dcast(
        data = data.table::data.table(media_w_key_annotations_location),
        formula = fk_locationid ~ fk_taxonid,
        value.var = "count")
      
      media_w_key_annotations_location[is.na(
        media_w_key_annotations_location)] <- 0
      
      media_w_key_annotations_location <- media_w_key_annotations_location[order(
        media_w_key_annotations_location$fk_locationid),]
      
      media_w_key_annotations_location <- gt::gt(media_w_key_annotations_location) |>
        gt::tab_header(
          title = "Media files with Annotations of Key Species",
          subtitle = "by Location"
        ) |>
        gt::cols_label(
          fk_locationid = "Location ID"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_location_keysp <- media_w_key_annotations_location
      
      # annotations per year and location (key taxa) ================
      media_w_key_annotations_year <- stats::aggregate(
        x = count ~ fk_taxonid + year,
        data = annotations_key,
        FUN = sum)
      
      media_w_key_annotations_year <- data.table::dcast(
        data = data.table::data.table(media_w_key_annotations_year),
        formula = year ~ fk_taxonid,
        value.var = "count")
      
      media_w_key_annotations_year <- gt::gt(media_w_key_annotations_year) |>
        gt::tab_header(
          title = "Media files with Annotations of Key Species",
          subtitle = "by Year"
        ) |>
        gt::cols_label(
          year = "Year"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_year_keysp <-  media_w_key_annotations_year
      
      # annotations, counts only (key taxa) =====================
      media_w_key_annotations <- stats::aggregate(
        x = count ~ fk_taxonid,
        data = annotations_key,
        FUN = sum)
      
      media_w_key_annotations <- media_w_key_annotations[order(
        media_w_key_annotations$fk_taxonid),]
      
      
      media_w_key_annotations <- gt::gt(media_w_key_annotations) |>
        gt::tab_header(
          title = "Total Media files with Annotations of Key Species"
        ) |>
        gt::cols_label(
          fk_taxonid = "Species",
          count = "Media Files"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_media_keysp <- media_w_key_annotations
      
  ## visits with annotations (key taxa) ==================
      
      # get only annotations for unique visits
      unique_visits_per_sp <- unique(
        annotations_key[c('fk_taxonid', 'fk_visitid')])
      
      annotations_key <- annotations_key[row.names(unique_visits_per_sp),]
      
      key_annotations_full <- stats::aggregate(
        x = count ~ fk_taxonid + year + fk_locationid,
        data = annotations_key,
        FUN = sum)
      
      
      ## visits with annotations per location and year
      visits_w_key_annotations_location_year <- data.table::dcast(
        data = data.table::data.table(key_annotations_full),
        formula = year + fk_locationid ~ fk_taxonid,
        value.var = "count")
      
      visits_w_key_annotations_location_year[is.na(
        visits_w_key_annotations_location_year)] <- 0
      
      
      visits_w_key_annotations_location_year <- visits_w_key_annotations_location_year[
        order(
          visits_w_key_annotations_location_year$fk_locationid,
          visits_w_key_annotations_location_year$year),]
      
      visits_w_key_annotations_location_year <- gt::gt(
        visits_w_key_annotations_location_year) |>
        gt::tab_header(
          title = "Total Visits with Annotations of Key Species",
          subtitle = "by Year and Location"
        ) |>
        gt::cols_label(
          fk_locationid = "Location ID",
          year = "Year"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_location_year_keysp <- visits_w_key_annotations_location_year
      
      # visits with annotations per location (key taxa) ===============
      visits_w_key_annotations_location <- stats::aggregate(
        x = count ~ fk_taxonid + fk_locationid,
        data = annotations_key,
        FUN = sum)
      
      visits_w_key_annotations_location <- data.table::dcast(
        data = data.table::data.table(visits_w_key_annotations_location),
        formula = fk_locationid ~ fk_taxonid,
        value.var = "count")
      
      visits_w_key_annotations_location[is.na(
        visits_w_key_annotations_location)] <- 0
      
      visits_w_key_annotations_location <- visits_w_key_annotations_location[
        order(
          visits_w_key_annotations_location$fk_locationid),]
      
      visits_w_key_annotations_location <- gt::gt(visits_w_key_annotations_location) |>
        gt::tab_header(
          title = "Total Visits with Annotations of Key Species",
          subtitle = "by Location"
        ) |>
        gt::cols_label(
          fk_locationid = "Location ID"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_location_keysp <- visits_w_key_annotations_location
      
      # visits with annotations per year and location (key taxa only) =======
      visits_w_key_annotations_year <- stats::aggregate(
        x = count ~ fk_taxonid + year,
        data = annotations_key,
        FUN = sum)
      
      visits_w_key_annotations_year <- data.table::dcast(
        data = data.table::data.table(visits_w_key_annotations_year),
        formula = year ~ fk_taxonid,
        value.var = "count")
      
      visits_w_key_annotations_year[
        is.na(visits_w_key_annotations_year)
      ] <- 0
      
      visits_w_key_annotations_year <- gt::gt(visits_w_key_annotations_year) |>
        gt::tab_header(
          title = "Total Visits with Annotations of Key Species",
          subtitle = "by Year"
        ) |>
        gt::cols_label(
          year = "Year"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_year_keysp <-  visits_w_key_annotations_year
      
      # visits with annotations, counts only (key taxa) ============
      visits_w_key_annotations <- stats::aggregate(
        x = count ~ fk_taxonid,
        data = annotations_key,
        FUN = sum)
      
      visits_w_key_annotations <- visits_w_key_annotations[
        order(visits_w_key_annotations$fk_taxonid),]
      
      
      visits_w_key_annotations <- gt::gt(visits_w_key_annotations) |>
        gt::tab_header(
          title = "Total Visits with Annotations of Key Species"
        ) |>
        gt::cols_label(
          fk_taxonid = "Species",
          count = "Visits"
        ) |>
        gt::cols_align(
          align = "center"
        )
      
      results$annotations_visits_keysp <- visits_w_key_annotations
      
      ## annotation verifications (key taxa)====================
      
      anno_verifications <- summary_data$annotationverifications
      
      if (nrow(anno_verifications) > 0 ) {
  
        # get key species annotation verifications only
        
        taxonids <- tolower(taxa$pk_taxonid[
          which(
            tolower(taxa$pk_taxonid) %in% tolower(key_species))])
        
        commons <- tolower(taxa$common_name[
          which(
            tolower(taxa$common_name) %in% tolower(key_species))])
        
        key_sp_all <- c(taxonids, commons)
          
        anno_verifications <- anno_verifications[
          which(
            tolower(anno_verifications$fk_taxonid) %in% key_sp_all),]
  
        
        ## valid annotations by people (key taxa) ====================
        
        # update tagger id column name to avoid conflicts between two fk_personid cols
        names(anno_verifications)[which(names(anno_verifications) == "fk_personid:1")] <- "tagger_id"
        
        
        valid_annoverifications_by_people <- stats::aggregate(
          x = is_valid ~ fk_taxonid + tagger_id + fk_personid,
          data = anno_verifications,
          FUN = sum)
        
        valid_annoverifications_by_people <- data.table::dcast(
          data = data.table::data.table(valid_annoverifications_by_people),
          formula = tagger_id + fk_personid ~ fk_taxonid,
          value.var = "is_valid")
        
        valid_annoverifications_by_people[
          is.na(valid_annoverifications_by_people)] <- 0
        
        valid_annoverifications_by_people <- gt::gt(
          valid_annoverifications_by_people) |>
          gt::tab_header(
            title = "Valid Annotation Verifications"
          ) |>
          gt::cols_label(
            tagger_id = "Tagger ID",
            fk_personid = "Verifier ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_by_people <- valid_annoverifications_by_people
        
        ## invalid annotation verifications by people
        
        anno_verifications$count_invalid <- 0
        
        for (i in 1:nrow(anno_verifications)) {
          
          if (anno_verifications$is_valid[i] == 0) {
            anno_verifications$count_invalid[i] <- 1
          }
          
        } # end adding invalid count
        
        invalid_annoverifications_by_people <- stats::aggregate(
          x = count_invalid ~ fk_taxonid + tagger_id + fk_personid,
          data = anno_verifications,
          FUN = sum)
        
        invalid_annoverifications_by_people <- data.table::dcast(
          data = data.table::data.table(invalid_annoverifications_by_people),
          formula = tagger_id + fk_personid ~ fk_taxonid,
          value.var = "count_invalid")
        
        invalid_annoverifications_by_people[
          is.na(invalid_annoverifications_by_people)] <- 0
        
        invalid_annoverifications_by_people <- gt::gt(
          invalid_annoverifications_by_people) |>
          gt::tab_header(
            title = "Invalid Annotation Verifications"
          ) |>
          gt::cols_label(
            tagger_id = "Tagger ID",
            fk_personid = "Verifier ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        
        results$invalid_annotations_by_people <- invalid_annoverifications_by_people
        
        ## valid annotations from verifications (key taxa) ============
        
        # remove invalid annotations from verifications
        anno_verifications <- anno_verifications[
          which(anno_verifications$is_valid == 1),]
        
        
        # get unique media
        unique_media_per_sp <- unique(
          anno_verifications[c('fk_taxonid', 'fk_mediaid')])
        
        anno_verifications <- anno_verifications[row.names(unique_media_per_sp),]
        
        
        # Verified annotations for all sp., location + year
        
        valid_annotations_location_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_year_sp),
          formula = year + fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_year_sp[
          is.na(valid_annotations_location_year_sp)
        ] <- 0
        
        valid_annotations_location_year_sp <- valid_annotations_location_year_sp[
          order(
            valid_annotations_location_year_sp$year,
            valid_annotations_location_year_sp$fk_locationid
          )
        ]
        
        valid_annotations_location_year_sp <- gt::gt(
          data = valid_annotations_location_year_sp
        ) |>
          gt::tab_header(
            title = "Media Files with Verified Annotations for Key Taxa",
            subtitle = "by Year and Location"
          ) |>
          gt::cols_label(
            year = "Year",
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_media_loc_year <- valid_annotations_location_year_sp
        
        # Valid annotations by location
        
        valid_annotations_location_sp <- stats::aggregate(
          x = is_valid ~ fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_sp),
          formula = fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_sp[
          is.na(valid_annotations_location_sp)
        ] <- 0
        
        valid_annotations_location_sp <- valid_annotations_location_sp[
          order(valid_annotations_location_sp$fk_locationid)
        ]
        
        valid_annotations_location_sp <- gt::gt(
          data = valid_annotations_location_sp
        ) |>
          gt::tab_header(
            title = "Media Files with Verified Annotations for Key Taxa",
            subtitle = "by Location"
          ) |>
          gt::cols_label(
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_media_loc <- valid_annotations_location_sp
        
        # valid annotations by year for all taxa
        
        valid_annotations_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_year_sp),
          formula = year ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_year_sp[
          is.na(valid_annotations_year_sp)
        ] <- 0
        
        valid_annotations_year_sp <- valid_annotations_year_sp[
          order(valid_annotations_year_sp$year)
        ]
        
        valid_annotations_year_sp <- gt::gt(
          data = valid_annotations_year_sp
        ) |>
          gt::tab_header(
            title = "Media Files with Verified Annotations for Key Taxa",
            subtitle = "by Year"
          ) |>
          gt::cols_label(
            year = "Year"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        
        results$valid_annotations_media_year <- valid_annotations_year_sp
        
        # valid annotations by month for all taxa 
        
        valid_annotations_month_sp <- stats::aggregate(
          x = is_valid ~ month + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_month_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_month_sp),
          formula = month ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_month_sp <- valid_annotations_month_sp[
          order(valid_annotations_month_sp$month)
        ]
        
        valid_annotations_month_sp[is.na(valid_annotations_month_sp)] <- 0
        
        valid_annotations_month_sp <- gt::gt(
          data = valid_annotations_month_sp
        ) |>
          gt::tab_header(
            title = "Media Files with Verified Annotations for Key Taxa",
            subtitle = "by Month"
          ) |>
          gt::cols_label(
            month = "Month"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        
        results$valid_annotations_media_month <- valid_annotations_month_sp
        
        ## valid annotations, media files per sp 
        
        valid_annotations_sp <- aggregate(
          x = is_valid ~ fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_sp <- valid_annotations_sp[
          order(valid_annotations_sp$fk_taxonid),
        ]
        
        valid_annotations_sp <- gt::gt(
          data = valid_annotations_sp
        ) |>
          gt::tab_header(
            title = "Total Media Files with Verified Annotations for Key Taxa"
          ) |>
          gt::cols_label(
            fk_taxonid = "Species",
            is_valid = "Annotated Media"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_media_total_sp <- valid_annotations_sp
        
        ## verifications by visit (all taxa) ================
        
        # get unique visits per sp
        unique_visits_per_sp <- unique(
          anno_verifications[c('fk_taxonid', 'fk_visitid')])
        
        anno_verifications <- anno_verifications[row.names(unique_visits_per_sp),]
        
        
        # Verified annotations by visit for all sp., location + year 
        
        valid_annotations_location_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_year_sp),
          formula = year + fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_year_sp[
          is.na(valid_annotations_location_year_sp)
        ] <- 0
        
        valid_annotations_location_year_sp <- valid_annotations_location_year_sp[
          order(
            valid_annotations_location_year_sp$year,
            valid_annotations_location_year_sp$fk_locationid
          )
        ]
        
        valid_annotations_location_year_sp <- gt::gt(
          data = valid_annotations_location_year_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for Key Taxa",
            subtitle = "by Year and Location"
          ) |>
          gt::cols_label(
            year = "Year",
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_visits_loc_year <- valid_annotations_location_year_sp
        
        # Valid annotations by visit by location
        
        valid_annotations_location_sp <- stats::aggregate(
          x = is_valid ~ fk_locationid + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_location_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_location_sp),
          formula = fk_locationid ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_location_sp[
          is.na(valid_annotations_location_sp)
        ] <- 0
        
        valid_annotations_location_sp <- valid_annotations_location_sp[
          order(valid_annotations_location_sp$fk_locationid)
        ]
        
        valid_annotations_location_sp <- gt::gt(
          data = valid_annotations_location_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for Key Taxa",
            subtitle = "by Location"
          ) |>
          gt::cols_label(
            fk_locationid = "Location ID"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_visits_loc <- valid_annotations_location_sp
        
        # valid annotations by year for all taxa 
        
        valid_annotations_year_sp <- stats::aggregate(
          x = is_valid ~ year + fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_year_sp <- data.table::dcast.data.table(
          data = data.table::data.table(valid_annotations_year_sp),
          formula = year ~ fk_taxonid,
          value.var = "is_valid"
        )
        
        valid_annotations_year_sp[
          is.na(valid_annotations_year_sp)
        ] <- 0
        
        valid_annotations_year_sp <- valid_annotations_year_sp[
          order(valid_annotations_year_sp$year)
        ]
        
        valid_annotations_year_sp <- gt::gt(
          data = valid_annotations_year_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for Key Taxa",
            subtitle = "by Year"
          ) |>
          gt::cols_label(
            year = "Year"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        
        results$valid_annotations_visits_year <- valid_annotations_year_sp
        
        ## valid annotations by visit per sp 
        
        valid_annotations_sp <- aggregate(
          x = is_valid ~ fk_taxonid,
          data = anno_verifications,
          FUN = sum
        )
        
        valid_annotations_sp <- valid_annotations_sp[
          order(valid_annotations_sp$fk_taxonid),
        ]
        
        valid_annotations_sp <- gt::gt(
          data = valid_annotations_sp
        ) |>
          gt::tab_header(
            title = "Total Visits with Verified Annotations for Key Taxa"
          ) |>
          gt::cols_label(
            fk_taxonid = "Species",
            is_valid = "Total Visits"
          ) |>
          gt::cols_align(
            align = "center"
          )
        
        results$valid_annotations_visits_total_sp <- valid_annotations_sp
        
      } # end annotation verifications for key species
      
    } # end if key_species list entered
    
  } # end if nrow(annotations) != 0

  # modeloutputs ==============================
  modeloutputs <-  summary_data$modeloutputs
  modeloutputs$fk_taxonid <- modeloutputs$`fk_taxonid:1`
  
  if (nrow(modeloutputs) != 0) {
    modeloutputs$count <- 1
    
    
     ## model outputs per location and year ==================
     modeloutputs_location_year <- stats::aggregate(
       x = count~fk_taxonid + model_name + year + fk_locationid,
       data = modeloutputs,
       FUN = sum)
     
    modeloutputs_location_year <- data.table::dcast(
      data = data.table::data.table(modeloutputs_location_year),
      formula = model_name + year + fk_locationid ~ fk_taxonid,
      value.var = "count")
      
     modeloutputs_location_year[is.na(modeloutputs_location_year)] <- 0
     
     modeloutputs_location_year <- modeloutputs_location_year[
       order(
         modeloutputs_location_year$model_name),]
    
     modeloutputs_location_year <- gt::gt(modeloutputs_location_year) |>
       gt::tab_header(
         title = "Model Outputs by Year and Location"
       ) |>
       gt::cols_label(
         model_name = "Model",
         fk_locationid = "Location ID",
         year = "Year"
       ) |>
       gt::cols_align(
         align = "center"
       )
    
     results$modeloutputs_location_year <- modeloutputs_location_year
     
     ## modeloutputs per year =================
     modeloutputs_year <- stats::aggregate(
       x = count~fk_taxonid + model_name + year,
       data = modeloutputs,
       FUN = sum)
     
     modeloutputs_year <- data.table::dcast(
       data = data.table::data.table(modeloutputs_year),
       formula = model_name + year ~ fk_taxonid,
       value.var = "count")
     
     modeloutputs_year[is.na(modeloutputs_year)] <- 0
     
     modeloutputs_year <- modeloutputs_year[
       order(modeloutputs_year$model_name),]
     
     modeloutputs_year <- gt::gt(modeloutputs_year) |>
       gt::tab_header(
         title = "Model Outputs by Year"
       ) |>
       gt::cols_label(
         model_name = "Model",
         year = "Year"
       ) |>
       gt::cols_align(
         align = "center"
       )
     
     results$modeloutputs_year <- modeloutputs_year
     
   ## modeloutputs per location =============
   modeloutputs_location <- stats::aggregate(
     x = count~fk_taxonid + model_name + fk_locationid,
     data = modeloutputs,
     FUN = sum)
   
   modeloutputs_location <- data.table::dcast(
     data = data.table::data.table(modeloutputs_location),
     formula = model_name + fk_locationid ~ fk_taxonid,
     value.var = "count")
     
   modeloutputs_location[is.na(modeloutputs_location)] <- 0
   
   modeloutputs_location <- modeloutputs_location[
     order(modeloutputs_location$model_name),]
   
   modeloutputs_location <- gt::gt(modeloutputs_location) |>
     gt::tab_header(
       title = "Model Outputs by Location"
     ) |>
     gt::cols_label(
       model_name = "Model",
       fk_locationid = "Location ID"
     ) |>
     gt::cols_align(
       align = "center"
     )
   
   results$modeloutputs_location <- modeloutputs_location
   
   } #end model outputs
  
   # model verifications ===================
   mverifications <- summary_data$modelverifications
  
  if (nrow(mverifications) != 0 ) {
    
    mverifications$invalid <- 0
    
    for (i in 1:nrow(mverifications)) {
     
      if (mverifications$is_valid[i] == 0) {
        mverifications$invalid[i] <- 1
      }
    } # end adding to mverifications
    
    ## invalid model verifications ===========
    
    invalid_model_verifications <- stats::aggregate(
      x = invalid ~ fk_taxonid + fk_personid + model_name,
      data = mverifications,
      FUN = sum)
    
    invalid_model_verifications <- data.table::dcast(
      data.table::data.table(invalid_model_verifications),
      formula = model_name + fk_personid ~ fk_taxonid,
      value.var = "invalid")
    
    invalid_model_verifications[is.na(invalid_model_verifications)] <- 0
    
    invalid_model_output_verifications <- gt::gt(invalid_model_verifications) |>
      gt::tab_header(
        title = "Invalid Model Outputs"
      ) |>
      gt::cols_align(
        align = "center"
      ) |>
      gt::cols_label(
        model_name = "Model",
        fk_personid = "Person ID"
      )
    
    results$invalid_model_output_verifications <- invalid_model_output_verifications
    
  } # end model verifications
  
  # return results
  return(results)

} # end of function
