#' @name ammCreateMiniDemo
#' @aliases ammCreateMiniDemo
#' @title Create a lightweight AMMonitor project for demonstration
#' @description Create a lightweight AMMonitor project for demonstration. An
#' AMMonitor project consists of several directories and an AMMonitor SQLite 
#' database that tracks all components of a monitoring program.
#' @param filepath  File path to the directory that will house the database.
#' Default is tempdir().
#' @export
#' @importFrom DBI dbDisconnect dbReadTable dbAppendTable
#' @import monitoR
#' @importFrom tuneR writeWave
#' @importFrom AMModels amModel amModelLib
#' @importFrom utils data
#' @usage ammCreateMiniDemo(filepath = tempdir())
#' @details Each 
#' AMMonitor project consists of a set of directories that store specific types 
#' of information, as detailed below:
#' \itemize{
#'   \item \strong{ammls}: Stores AMModel libraries.  A demo library is named
#'   "templates.RDS", which stores 4 templates created with the monitoR package.
#'   \item \strong{database}: Stores the SQLite database. The example database
#'   is named "demo.sqlite".
#'   \item \strong{logs} and \strong{log_drop}: Stores archived logs that 
#'   provide information about
#'   an analysis or an equipment performance.  
#'   \item \strong{videos} and \strong{video_drop}: Stores archived videos. 
#'   (Video functions are yet to be developed). 
#'   \item \strong{photos} and \strong{photos_drop}: Stores archived photos. The
#'   example photos directory consists of 25 photos.
#'   \item \strong{recordings} and \strong{recordings_drop}: Stores archived 
#'   audio recording files (e.g., .wav) 
#'   captured in acoustic monitoring programs.  The example recordings directory
#'   stores one recording named "1.wav".
#'   \item \strong{scripts}: Stores R scripts that can be sourced to automatically 
#'   process new data.  The example script is named "dbCheckAll.R".
#'   \item \strong{settings}: Stores files needed to access accounts through R.
#'   The demo settings include "default_user.txt",  "audio_path.txt",
#'    "image_path.txt", and  "noaa_token.txt".
#'   \item \strong{spatials}: Stores spatial layers associated with locations 
#'   in a monitoring program (rasters and/or shapefiles) as raw files or RDS files.
#' }
#' 
#' 
#' See the "getting_started" learnr tutorial for more details on what an 
#' AMMonitor project is and its setup.  
#' The tutorial can be launched with \code{learnr::run_tutorial(name = 
#' "getting_started", package = "AMMonitor")}.

#' @return Returns the filepath of the mini AMMonitor project.
#' @export
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor project in a temporary directory
#' # (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # look at the folders within an AMMonitor project
#' list.files(demo_fp, recursive = FALSE)
#' 
#' # look at the sample photos in the photos directory
#' list.files(file.path(demo_fp, "photos"), recursive = FALSE)
#' 
#' # look at the sample recording in the recordings directory
#' list.files(file.path(demo_fp, "recordings"), recursive = FALSE)
#' 
#' # look at the sample ammodels library that stores templates in the amml directory
#' list.files(file.path(demo_fp, "ammls"), recursive = FALSE)
#' 
#' # read in the "templates" model library; note there are 4 templates
#' templates <- readRDS(file.path(demo_fp, "ammls", "templates.RDS"))
#' templates
#' 
#' # look at the sample settings in the settings directory
#' list.files(file.path(demo_fp, "settings"))
#' 
#' # see who the default user is for the database
#' readLines(file.path(demo_fp, "settings", "default_user.txt"))
#' 
#' # look at the sample scripts in the scripts directory
#' list.files(file.path(demo_fp, "scripts"))
#' 
#' # look at the sample survey123 template in the mobile_apps directory
#' list.files(file.path(demo_fp, "mobile_apps"))
#' 
#' # look at the sample database in the database directory
#' list.files(file.path(demo_fp, "database"))
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # list the tables in the AMMonitor database
#' DBI::dbListTables(conx)
#' 
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#' 
#' # to work with the shiny app, just point to the AMMonitor project directory
#' # the app will set the database connection and disconnect
#' launchApp(amm_project_path = demo_fp)
#' 
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' 
#' }


ammCreateMiniDemo <- function(filepath = tempdir()){
  
  # get the full path
  demo_name <- paste0("demo_", round(runif(1)*1000000000))
  full.path <- paste0(filepath, "/", demo_name)

  # create the main directories ---------------
  ammCreateDirectories(amm_dirname = demo_name, filepath = filepath)
  
  # add some media -------------
  utils::data(survey, package = "monitoR", envir = environment())
  tuneR::writeWave(survey, file.path(paste0(full.path, "/recordings/1.wav")))
  
  message("\nThe following audio files have been added to recordings folder:\n")
  print(list.files(file.path(full.path, "recordings")))
  
  # add some photos
  source_directory <- paste0(find.package("AMMonitor", lib.loc = .libPaths()), "/extdata/demo_photos/")
  destination_directory <- paste0(full.path, "/photos")
  
  # list all files in the source directory
  files_to_copy <- list.files(source_directory, full.names = TRUE)
  
  for (i in 1:length(files_to_copy)) {
    file.copy(
      from = files_to_copy[i], 
      to = destination_directory, 
      overwrite = TRUE)
  }
  
  message("\nThe following photos have been added to photos folder:\n")
  print(list.files(paste0(full.path, "/photos/")))
  
  # make templates to AMModels library ---------------
  message("\nMaking demo templates . . .\n")
  utils::data(oven, package = "monitoR", envir = environment())
  utils::data(btnw, package = "monitoR", envir = environment())

  # btnw ----------------
  
  if (file.exists("btnw.wav") == TRUE) unlink("btnw.wav")
  
  btnw_ct <- makeCorTemplate(
    clip = btnw, 
    t.lim = c(0.7, 2.4), 
    frq.lim = c(3.3, 6.6), 
    name = "btnw_ct",
    write.wav = TRUE)
  
  unlink("btnw.wav")
  
  
  btnw_ct <- AMModels::amModel(
    model = btnw_ct,
    comment = list(
      model_type = "correlation template",
      taxa = "btnw",
      original_file = "data(btnw)")
  )
  
  if (file.exists("btnw.wav") == TRUE) unlink("btnw.wav")
  
  btnw_bt <- monitoR::makeBinTemplate(
    clip = btnw, 
    amp.cutoff = -40, 
    name = "btnw_bt",
    write.wav = TRUE)
  
  unlink("btnw.wav")
  
  btnw_bt  <- AMModels::amModel(
    model = btnw_bt , 
    comment = list(
      model_type = "binary template",
      taxa = "btnw",
      original_file = "data(btnw)"
    )
  )
  
  # ovenbird -------------
  
  if (file.exists("oven.wav") == TRUE) unlink("oven.wav")
  
  oven_ct <- monitoR::makeCorTemplate(
    clip = oven, 
    name = "oven_ct", 
    t.lim = c(1, 4), 
    frq.lim = c(3, 10),
    write.wav = TRUE)
  
  unlink("oven.wav")
  
  oven_ct <- AMModels::amModel(
    model = oven_ct, 
    comment = list(
      model_type = "correlation template",
      taxa = "oven",
      original_file = "data(oven)"
      )
  )
  
 if (file.exists("oven.wav") == TRUE) unlink("oven.wav")
  
 oven_bt <- monitoR::makeBinTemplate(
    clip = oven, 
    amp.cutoff = -40, 
    name = "oven_bt", 
    write.wav = TRUE)
 
 unlink("oven.wav")
 
 oven_bt  <- AMModels::amModel(
   model = oven_bt , 
   comment = list(
     model_type = "binary template",
     taxa = "oven",
     original_file = "data(oven)"
   )
  )


  # add templates to ammodels library ---------------
  amml <- AMModels::amModelLib(
    models = list(
      btnw_ct = btnw_ct,
      btnw_bt = btnw_bt,
      oven_ct = oven_ct,
      oven_bt = oven_bt
     ),
    info = list(
      owner = "gandalf",
      purpose = "Demonstration"), 
    description = "This library stores templates of target species.")
 
  message("\nAn AMModels library has been created to store audio templates.
      Here is the summary:\n")
  print(amml)
 
  saveRDS(amml, file = paste0(full.path, "/ammls/templates.RDS"))
  
  # create the database ---------------
  dbCreate(
    new_db_name = "demo.sqlite", 
    new_db_filepath = paste0(full.path, "/database"), 
    db_source = "default")
  
  # connect to db
  con_demo <- dbSetCon(paste0(full.path, "/database/demo.sqlite"))
  
  # add people ---------
  
  pk_personid <- c("bbaggins", "fbaggins", "sgamgee", "gandalf")
  
  new_people <- data.frame(
    pk_personid = pk_personid,
    first_name  = c("Bilbo", "Frodo", "Samwise", "Gandalf"),
    last_name = c("Baggins", "Baggins", "Gamgeee", "The_Gray"),
    project_role = c("Tagger", "Field technician", "Database Manager", "PI"),
    email = NA,
    phone = NA, 
    sb_login = NA, 
    display_name = c("blue", "red", "green", "gray")
  )
  
  DBI::dbAppendTable(con_demo, name = "people", value = new_people)
  
  # add taxa ------------------------
  utils::data(taxa, package = "AMMonitor", envir = environment())
  indices <- which(taxa$pk_taxonid == 'no-species' | taxa$pk_taxonid == 'Human' |
    taxa$pk_taxonid == 'animal sp.')

  taxa <- taxa[-indices,]
  
  DBI::dbAppendTable(con_demo, name = "taxa", value = taxa)
  
  # confirm
  taxa_n <- dbGetQuery(con_demo, "SELECT COUNT (*) FROM taxa;")
  message(paste0(
    "\nThe taxa table has been populated with ",
    taxa_n[1,1, drop = TRUE],
    " records from the itis database.")
  )
  # add objectives ---------------
  new_objectives <- data.frame(
    pk_objectiveid = c("Ovenbird objective", "Black bear objective"),
    fk_taxonid = c("oven", "bear"),   
    fk_librarylistid = NA,
    objective = c(
      "Maximize occupancy rate of ovenbirds.",
      "Maintain occupancy rate of black bears at 0.4"
    ),
    indicator = "occupancy rate", 
    units = "probability",
    direction = c("maximize", "maintain"),   
    min = c(0.5, 1),          
    max = c(0.3, 0.5),           
    standard = c(NA, 0.4),        
    narrative = c(
      "Narration for this objective -- possibly report ready.",
      "Narration for this objective -- possibly report ready."
    )  
  )
  
  # locations ----------
  pk_locationid <- c("locationA", "locationB", "locationC")
  
  new_locations <- data.frame(
    pk_locationid = pk_locationid,
    spatial_geometry = "point", 
    location_type = "monitoring_station", 
    fk_spatialid  = NA,
    lat = c(43.2, 43.3, 43.4),
    long = c(-72.7, -72.8, -72.9),
    datum = "WGS84",
    description = paste("Description of site", pk_locationid), 
    x  = NA,
    y = NA,
    epsg = NA,
    tz = "America/New_York",
    sensitive = FALSE,
    long_min = NA,
    long_max = NA,
    lat_min = NA,
    lat_max = NA,
    location_status = "active"
  )
  
  DBI::dbAppendTable(con_demo, name = "locations", value = new_locations)
  
  # blurr locations
  locationsObscure(
    con = con_demo,
    bbLength = 0.02,
    overwrite = TRUE,
    disconnect = FALSE)
  
  # add equipment models
  new_equipmodels <- data.frame(
    pk_equipmodelid = c("recorder_model_x", "camera_model_y"),
    equip_type = c("recorder", "camera"),
    manufacturer = NA,
    user_manual = NA
  )
  
  DBI::dbAppendTable(con_demo, name = "equipmodels", value = new_equipmodels)
  
  # add equipment configurations -------------
  
  new_esettingnames <- data.frame(
    pk_esettingnameid = NA,
    setting_name = c("sample rate", "max recording length", "gain", 
      "image size", "image format", "illumination mode"),
    fk_equipmodelid = c(rep("recorder_model_x", 3), rep("camera_model_y", 3)),
    description = c(
      "Sets the  number of samples per second used to make a
         recording during a recording period. Higher sample rates record
         higher frequencies, but use more card space",
      "Sets the maximum length of recordings within a schedule. For
        example, if a recording schedule is set to record always and the
        maximum record length is set to 60 minutes, the recorder will
        create 24 60-minute files per day.",
      "Sets the gain of the microphone signal to increase/decrease the recorded 
        signal's amplitute",
      "Sets the image resolution and size. Note: Larger sizes mean higher
         image resolutions that take up more
         space on the SD card. Medium is the recommended default for good 
         resolution and smaller size",
      "Sets the aspect ratio of your photos.",
      "Sets the shutter speed."
    )
  )
  
  DBI::dbAppendTable(con_demo, name = "esettingnames", value = new_esettingnames)
  
  new_settingoptions <- data.frame(
    pk_esettingoptionid = NA,
    fk_esettingnameid = c(
      rep(1, 4), 
      rep(3, 4),
      rep(4, 3),
      rep(5, 2),
      rep(6, 3)),
    option_name = c(
      "8000Hz", "12000Hz", "16000Hz", "24000Hz",
      "6dB", "12dB", "18dB", "24dB",
      "low", "medium", "high",
      "4:3", "16:9",
      "low", "fast motion", "long range"
    ),
    description = c(
      "8000Hz recording sample rate.",
      "12000Hz recording sample rate.",
      "16000Hz recording sample rate.",
      "24000Hz recording sample rate.",
      "6dB gain setting.",
      "12dB gain setting.",
      "18dB gain setting.",
      "24dB gain setting.",
      "Low is low image resolution",
      "Medium is the recommended default for good resolution and smaller size",
      "High is high quality resolution that take up more
          space on the SD card.",
      "4:3 image aspect ratio", 
      "16:9 image aspect ratio",
      "Recommended for subjects at less than 60'.", 
      "Recommended for subjects moving quickly through the frame.",
      "Recommended for maximum illumination range and field of view."
    )
  )
    
  DBI::dbAppendTable(con_demo, name = "esettingoptions", value = new_settingoptions)
  
  new_econfignames <- data.frame(
    pk_econfignameid = c("default recorder settings", "default camera settings"),
    fk_equipmodelid = c("recorder_model_x", "camera_model_y"),
    description = c("This configuration is the default recorder configuration
                    used in monitoring Middle Earth",
                    "This configuration is the default camera configuration
                    used in monitoring Middle Earth"),
    filename = NA
  )
  
  DBI::dbAppendTable(con_demo, name = "econfignames", value = new_econfignames)
  
  new_configvalues <- data.frame(
    pk_econfigvalueid = NA,
    fk_econfignameid = c(
      rep("default recorder settings", times = 3),
      rep("default camera settings", times = 3)),
    fk_esettingnameid   =  1:6,
    fk_esettingoptionid = c(4, NA, 7, 10, 13, 15),
    value_num = c(NA, 1, rep(NA, 4))
  )
  
  DBI::dbAppendTable(con_demo, name = "econfigvalues", value = new_configvalues)
  
  # add equipment  ----------
  units <- 1:3
  pk_equipmentid <- c(paste0("recorder", units), paste0("camera", units))
  
  new_equipment <- data.frame(
    pk_equipmentid = pk_equipmentid,
    equip_type = c(rep("recorder", 3), rep("camera", 3)),
    fk_accountid = NA,
    fk_equipmodelid = c(rep("recorder_model_x", 3), c(rep("camera_model_y", 3))),
    serial_number = NA,
    equip_status = "operational",
    year_purchased = 2023,
    notes = "Notes about this unit."
  )
  
  DBI::dbAppendTable(con_demo, "equipment", value = new_equipment)

  # add visits ----------
  new_visits <- data.frame(
    pk_visitid = NA,
    fk_personid = rep("fbaggins", 18),    
    fk_locationid = c(pk_locationid,  pk_locationid, pk_locationid),
    fk_equipmentid = rep(pk_equipmentid, 3), 
    visit_type = c(rep("set", 6), rep("check", 6), rep("pull", 6)),
    visit_date = c(rep("2023-01-01", 6), rep("2023-09-30", 6), rep("2023-12-31", 6)),
    visit_time = rep("12:00:00", 18),
    visit_notes = rep("Notes associated with this visit.", 18),
    fk_econfigid =  c(
      rep("default recorder settings", 3), 
      rep("default camera settings", 3), 
      rep(NA,12))
  )
  
  DBI::dbAppendTable(con_demo, name = "visits", value = new_visits)
  
  # add media to the database -----------------
  photos <- list.files(paste0(full.path, "/photos"))
  
  result_df <- data.frame()
  
  for (photo in photos) {
    split_result <- unlist(strsplit(photo, "_"))
    
    # Append the split record to the data frame
    result_df <- rbind(
      result_df, 
      data.frame(
        media_type = "photo",
        filename = photo,
        fk_locationid = split_result[1] , 
        fk_equipmentid = split_result[2], 
        start_date = as.character(as.Date(split_result[3], format = "%Y%m%d")), 
        start_time = as.character(format(as.POSIXct(split_result[4], format = "%H%M%S"), "%H:%M:%S")),
        timestamp = "2024-01-03 13:58:07",
        stringsAsFactors = FALSE)
    )
  }
  
  result_df$start_date <- as.Date(result_df$start_date)
  
  db_visits <- DBI::dbReadTable(con_demo, name = "visits")
  db_visits$visit_date <- as.Date(db_visits$visit_date)
  
  pull_visits <- db_visits[which(db_visits$visit_type == "pull"), ]
  pull_media <- result_df[which(result_df$start_date > as.Date("2023-09-30")), ] 
  check_visits <- db_visits[which(db_visits$visit_type == "check"), ]
  check_media <- result_df[which(result_df$start_date <= as.Date("2023-09-30")), ]
     
  pull <- merge(pull_visits, pull_media)
  check <- merge(check_visits, check_media)
  
  new_media <- data.frame(
    pk_mediaid = NA,
    media_type  = "photo",
    filename  = c(check$filename, pull$filename),
    fk_visitid = c(check$pk_visitid, pull$pk_visitid),
    start_date = c(as.character(check$start_date), as.character(pull$start_date)),
    start_time  = c(check$start_time, pull$start_time),
    filepath = NA,
    sb_exclude = 0 ,
    fk_sciencebaseid = NA,
    filesize = NA,
    timestamp = "2024-01-04 08:32:45"
  )
  
  new_media <- rbind(
    new_media,
    data.frame(
      pk_mediaid = NA,
      media_type  = "audio",
      filename  = "1.wav",
      fk_visitid = new_media[1, "fk_visitid", drop = TRUE],
      start_date = new_media[1, "start_date", drop = TRUE],
      start_time  = "06:02:49",
      filepath = NA,
      sb_exclude = 0 ,
      fk_sciencebaseid = NA,
      filesize = NA,
      timestamp = "2024-01-03 13:58:07"
    )
  )

  
  DBI::dbAppendTable(con_demo, name = "media", value = new_media)

  
  # add librarylists -------------------------
  liblists <- dbReadTable(con_demo, name = 'librarylists')
  
  new_liblist <- data.frame(
    pk_librarylistid = c("fur_color_taxa_list", "md_list"),
    core_list = 0,
    list_type = "dropdown_list",
    description = 
      c("List of taxa that can be tagged for fur_color.",
        "List of taxa (and vehicles) that can be tagged by MegaDetector."),
    photos = 1,
    recordings = 0,
    videos = 0,
    fk_taxonid = NA,
    taxa_list = 1,
    fk_child_librarylistid = NA
  )
  
  DBI::dbAppendTable(con_demo, name = "librarylists", value = new_liblist)
  
  new_liblist2 <- data.frame(
    pk_librarylistid = c("fur_condition", "fur_color"),
    core_list = 0,
    list_type = "dropdown_list",
    description = c(
      "Fur condition options for moose photos.",
      "Fur color options for taxa in the fur_color_taxa_list."),
    photos = 1,
    recordings = 0,
    videos = 0,
    fk_taxonid = c("moose", NA),
    taxa_list = 0,
    fk_child_librarylistid = c(NA, "fur_color_taxa_list")
  )
  
  DBI::dbAppendTable(con_demo, name = "librarylists", value = new_liblist2)
  
  
  # add librarylistitems ---------------
  new_listitems <- data.frame(
    pk_librarylistitemid = NA,
    fk_librarylistid = c(
      rep("fur_color_taxa_list", 2),
      rep("fur_condition", 5),
      rep("fur_color", 3),
      rep("md_list", 3)),
    item = c(
      "hare",
      "weasel sp.",
      "Category 0",
      "Category 1",
      "Category 2",
      "Category 3",
      "Category 4",
      "brown",
      "molting",
      "white",
      "Human",
      "animal sp.",
      "vehicle"
    ),
    description = c(
      "Snowshoe Hare  primary key in the taxa table.",
      "Weasel sp.  primary key in the taxa table.",
      "No fur loss.",
      "Some hair loss on shoulders.",
      "Substantial hair rubbed off shoulders, chest and back.",
      "Most protective hairs gone from moose's front.",
      "Ghost moose - 80% or more of hair gone from its body.",
      "< 10% of the body (excluding belly and feet) is white.",
      ">10% and <90% of body (excluding belly and feet) is white.",
      "> 90% of the body (excluding belly and feet) is white.",
      "The Human primary key in the taxa table.",
      "The animal sp. primary key in the taxa table.",
      "The vehicle tag in the photo_non-taxa_bbox medialist."
    ),
    sort_order = NA
  )
  
  
  DBI::dbAppendTable(con_demo, name = "librarylistitems", value = new_listitems)
  
  
  
  # add annotations  -----------------
  
  taxa_opts = c("oven", "btnw", "bear", "deer", "raccoon", "moose", "coyote")
  annos <- c("deer", "bear", "bear", 
              "raccoon", "bear", "bear",
              "coyote", "deer", 
             "deer", "deer", "deer", "hare",  
             "no-species", "bear", "deer", "deer", 
             "deer", "no-species", "deer", 
             "deer", "deer", "moose", 
             "deer", "deer", "deer",
             "btnw", "oven")
  x_min <- c(rep(NA, 25), 1.11, 4.24)
  x_max <- c(rep(NA, 25), 2.58, 7.29)
  y_min <- c(rep(NA, 25), 3.53, 2.34)
  y_max <- c(rep(NA, 25), 6.63, 9.82)

  set.seed(11) # For repeatability
  timestamp_deltas <- cumsum(round((runif(27)*10)+3))
  annotation_timestamps <- paste(
    "2024-01-14",
    format(as.POSIXct('2023-01-01 21:29:31')+timestamp_deltas, '%H:%M:%S')
  )
  
  new_annotations <- data.frame(
    pk_annotationid = NA,
    fk_personid = "bbaggins",
    fk_mediaid  = c(1:25, 26, 26),  
    fk_searchlistid = NA, 
    fk_taxonid = annos,
    x_max = x_max, 
    x_min = x_min,
    y_max = y_max,
    y_min = y_min,
    notes = "Notes about this annotation.",
    timestamp = annotation_timestamps
  )
  
  DBI::dbAppendTable(con_demo, name = "annotations", value = new_annotations)
  
  db_annos <- DBI::dbReadTable(con_demo, "annotations")
  
  # add annotags ------------------
  dbAppendTable(
    con_demo,
    "annotags",
    data.frame(
      fk_annotationid = c(26, 26, 27, 27),
      fk_librarylistitemid = c(16, 22, 16, 22)
    )
  )
  
  # add verifications ------------------
  
  timestamp_deltas <- cumsum(round((runif(25)*13)+1))
  verification_timestamps <- paste(
    "2024-01-16",
    format(as.POSIXct('2023-01-01 08:32:31')+timestamp_deltas, '%H:%M:%S')
  )

  new_verifications <- data.frame(
    pk_annoverificationid = NA ,
    is_valid = TRUE,
    fk_personid = "sgamgee",
    fk_annotationid = 1:25,
    timestamp = verification_timestamps
  )
  
  dbAppendTable(
    con_demo,
    'annotationverifications',
    new_verifications
  )
  
  # add models (templates, megadetector, birdnet)  -----------------
  
  new_models <- data.frame(
    pk_modelid = NA,
    model_name = c("btnw_ct", "btnw_bt", "oven_ct", "oven_bt",  "MegaDetector", "BirdNET24"),
    model_url = c(
      rep(NA, 4), 
      "https://github.com/microsoft/CameraTraps", 
      "https://github.com/kahst/BirdNET-Analyzer"),
    amml = c(rep("templates.RDS", 4), NA, NA),
    model_type = c(
      rep(c("correlation template", "binary template"), 2),
      "CNN", "CNN"),
    model_description = c(
      rep("A template create with the monitoR package", 4),
      "An object detection model that identifies animals, people, and vehicles 
        in camera trap images.",
      "Detect avian vocalizations in soundscape recordings."),
    model_citation = c(
      rep(NA, 4),
      "Beery, S., Morris, D., & Yang, S. (2019). Efficient pipeline for camera trap
         image review. arXiv:1907.06772.",
      "Kahl, S., Wood, C. M., Eibl, M., & Klinck, H. (2021). BirdNET: A deep learning
         solution for avian diversity monitoring. Ecological Informatics, 61, 101236."),
    fk_taxonid = c("btnw", "btnw", "oven", "oven", NA, NA),
    fk_librarylistid = c(rep(NA,4), "md_list", NA),
    fk_parentid = NA
  )
  
  DBI::dbAppendTable(con_demo, name = "models", value = new_models)
  
  # add model settings -----------------------
  
  new_msettingnames <- data.frame(
    pk_msettingnameid = NA,
    setting_name  = c("confidence threshold", "lat", "long", "week", "overlap",
                      "sensitivity", "min_conf"),
    fk_modelid  = c(5, rep(6,6)),  
    description = c(
    "Minimum confidence score from a MegaDector model output 
      that will be returned.",
    "General latitude of the focal bird community.  Set -1 to ignore.",
    "General longitude of the focal bird community. Set -1 to ignore.",
    "Week of the year when the recording was made. Values in [1, 48] (4 weeks per month). Set -1 for year-round species list.",
    "Overlap of prediction segments. Values in [0.0, 2.9]. Defaults to 0.0.",
    "Detection sensitivity; Higher values result in higher sensitivity. Values in [0.5, 1.5]. Defaults to 1.0.",
    "Minimum confidence threshold. Values in [0.01, 0.99]. Defaults to 0.1.")
  )
  
  DBI::dbAppendTable(con_demo, name = "msettingnames", value = new_msettingnames)
  
 new_mconfignames <- data.frame(
   pk_mconfignameid = NA,
   mconfigname  = c("MegaDetector default settings", "BirdNet default settings"),
   fk_modelid = c(5, 6),    
   description = c("Default settings for MegaDetector.", "Default settings for BirdNet."),
   filename = NA,     
   is_default  = 1
 )
 
 DBI::dbAppendTable(con_demo, name = "mconfignames", new_mconfignames)
 
 new_mconfigvalues <- data.frame(
   pk_mconfigvalueid = NA,
   fk_mconfignameid = c(1, rep(2, 6)),
   fk_msettingnameid  = c(1, 2, 3, 4, 5, 6, 7),
   fk_msettingoptionid = NA,
   value_num = c(0.1, 43.6, -72.7, -1, 0, 1, 0.1)
 )
  
 DBI::dbAppendTable(con_demo, name = "mconfigvalues", new_mconfigvalues)

  # add modeloutputs  -----------------
  suppressWarnings(scoresDetect(
    con = con_demo,
    recordingNames = "1.wav",
    templateNames = "all",
    scoreThresholds = NA,
    recordingRootPath = paste0(full.path, "/recordings"),
    ammlPath = paste0(full.path, "/ammls"),
    dbInsert = TRUE
   )
  )
 
 
 # read in the megadector data
 utils::data(md_outputs, package = "AMMonitor", envir = environment())
 DBI::dbAppendTable(con_demo, name = "modeloutputs", value = md_outputs)
 
 utils::data(bn_outputs, package = "AMMonitor", envir = environment())
 DBI::dbAppendTable(con_demo, name = "modeloutputs", value = bn_outputs)
 
 # add modelverifications
 timestamp_deltas <- cumsum(round((runif(31)*15)+0.5))
 modelverification_timestamps <- paste(
   "2024-01-19",
   format(as.POSIXct('2023-01-01 12:46:44')+timestamp_deltas, '%H:%M:%S')
 )

 new_modelverifications <- data.frame(
   fk_modeloutputid = c(2, 8, 9, 50, 48, 30, 31, 11, 12, 13, 14, 15, 16, 36, 37, 38, 17, 32, 39, 33, 34, 35, 40, 18, 19, 20, 21, 22, 41, 42, 23),
   fk_personid = "sgamgee",
   is_valid = c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
   timestamp = modelverification_timestamps
 )
 
 DBI::dbAppendTable(con_demo, "modelverifications", new_modelverifications)
 
 # add modellabels
 new_modellabels <- data.frame(
   fk_modelid = c(6, 6, 6, 6, 6, 1, 2, 3, 4),
   fk_taxonid = c("btnw", "eato", "etti", "gcfl", "oven", "btnw", "btnw", "oven", "oven"),
   fk_librarylistitemid = rep(NA, 9),
   fk_medialistitemid = rep(NA, 9),
   original_label = c("Black-throated Green Warbler", "Eastern Towhee",
                      "Tufted Titmouse", "Great Crested Flycatcher", "Ovenbird",
                      "btnw", "btnw", "oven", "oven")
 )
 
 DBI::dbAppendTable(con_demo, "modellabels", new_modellabels)
  
  # add script ===================
  script_text <- "
  
    # load AMMonitor
    library('AMMonitor')
  
    # set a filepath to the AMMonitor database
    dbPath <- file.choose()
    
    # set the connection to database
    con <- dbSetCon(dbPath)
    
    # run dbCheckup to ensure dbdictionary actually matches the database schema
    checkup <- dbCheckup(con)
    
    lapply(checkup, FUN = head)
    
    # check the dictionary core columns against the default AMMonitor database
    core_test <- dbCheckCore(con)
    
    lappy(core_test, FUN = head)
    
    # check to see if problems may exist with data
    data_check <- dbCheckData(con)
    
    lapply(data_check, FUN = head)
    
    # get a summary of the database
    summary_data <- dbGetSummaryData(con)
    
    # obtain plots that summarize the data
    plot_results <- dbPlotSummary(summary_data)
    
    # obtain tables that summarize the data
    table_summary <- dbTableSummary(summary_data)
    "
  
  
  writeLines(
    text = script_text, 
    con = file(paste0(full.path, "/scripts/dbCheckAll.R"))
  )
 
 file.copy(
   from = paste0(find.package("AMMonitor", lib.loc = .libPaths()), "/extdata/annualreport.qmd"), 
   to = paste0(full.path, "/scripts/annualreport.qmd"), 
   overwrite = TRUE)
  
  message("\nA sample script has been written to the scripts directory.\n")
  print(list.files(paste0(full.path, "/scripts")))
  
  # add settings ---------------
  
  # noaa_token.txt
  writeLines(
    text = "FKwHpdWRbBJLxgZUguDexFCyJdbkKJoA", 
    con = file(paste0(full.path, "/settings/noaa_token.txt"))
  )
  
  # audio_path.txt
  writeLines(
    text = paste0(full.path, "/recordings/"),
    con = file(paste0(full.path, "/settings/audio_path.txt"))
  )
  
  # image_path.txt
  writeLines(
    text = paste0(full.path, "/photos/"), 
    con = file(paste0(full.path, "/settings/image_path.txt"))
  )
  
  # default_user.txt
  writeLines(
    text = 'sgamgee', 
    con = file(paste0(full.path, "/settings/default_user.txt"))
  )
  
  message("\nSample settings have been written to the settings directory.\n")
  print(list.files(paste0(full.path, "/settings")))
  
  # add survey123 ----------------------
  file.copy(
    from = paste0(
      find.package("AMMonitor", lib.loc = .libPaths()), 
      "/extdata/Survey123_Template.xlsx"),
    to = paste0(full.path, "/mobile_apps"), 
    overwrite = TRUE)

  # Return user feedback
  message(paste0("\nAn AMMonitor project directory has been created with the name ", demo_name, ". This directory consists of the following subdirectories (folders):  \n\n"))

  print(list.files(path = full.path))
  
  return(full.path)
  
  # check if directory already exists. If so, unlink
  if (dir.exists(full.path) == TRUE) {
    
    if (exists("con_demo") && inherits(con_demo, "SQLiteConnection")) {
      if (dbIsValid(con_demo)) {
        cat("Disconnecting SQLite connection.\n")
        dbDisconnect(con_demo)
      } 
    }
    Sys.sleep(5) 
    tryCatch( 
      expr = {unlink(filepath, recursive = TRUE, force = TRUE)},
      error = function(e) {cat("Error: ", conditionMessage(e), "\n")}
    )
  }
  
  # disconnect on exit if requested
  on.exit(expr = {
    DBI::dbDisconnect(con_demo)
  }) 

}
