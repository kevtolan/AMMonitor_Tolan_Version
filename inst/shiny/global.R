# source in all modules
mods <- list.files(
  paste(find.package('AMMonitor', lib.loc = .libPaths()), 'shiny/modules', sep = '/'),
  full.names = TRUE, 
  recursive = TRUE
)
sapply(mods, FUN = source)

# Get setting options and set global image/audio paths
IMG_PATH <<- reactiveVal({
  if (file.exists(paste0(ammPath, '/settings/image_path.txt'))) {
    IMG_PATH <- read.csv(paste0(ammPath, '/settings/image_path.txt'), header = F)[,]
    if (startsWith(IMG_PATH, "http") || dir.exists(IMG_PATH)) {
      IMG_PATH
    } else {
      gsub("//", "/", file.path(ammPath, IMG_PATH))
    }
  } else if (dir.exists(paste(ammPath, "photos", sep = "/"))) {
    paste(ammPath, "photos", sep = "/")
  } else {
    ""
  }
})

AUDIO_PATH <<- reactiveVal({
  if (file.exists(paste0(ammPath, '/settings/audio_path.txt'))) {
    AUDIO_PATH <- read.csv(paste0(ammPath, '/settings/audio_path.txt'), header = F)[,]
    if (startsWith(AUDIO_PATH, "http") || dir.exists(AUDIO_PATH)) {
      AUDIO_PATH
    } else {
      gsub("//", "/", file.path(ammPath, AUDIO_PATH))
    }
  } else if (dir.exists(paste(ammPath, "recordings", sep = "/"))) {
    paste(ammPath, "recordings", sep = "/")
  } else {
    ""
  }
})

#connect to database
dbPath <- paste(ammPath, 'database', dir(paste(ammPath, 'database', sep = '/')), sep = '/')
dbPath <- dbPath[endsWith(dbPath, '.sqlite')]

if (length(dbPath) != 1) {
  stop("Could not specify database file, check AMMonitor project's database directory.")
}

onStop(function() {
  non_modules <- c('get_new_annotations.R', 'getModalUI.R', 'save_metadata_cache.R', 'screenNewVisit.R')
  fcts <- as.vector(sapply(
    mods[!basename(mods) %in% non_modules],
    function(x) {
      paste0(gsub("\\.[^.]*$","", basename(x)), c('_server', '_ui'))
    }
  ))
  rm(
    list = c(
      'AUDIO_PATH', 'dbPath', 'IMG_PATH', 'jscode', 'mods', 'css',
      'fetch_page', 'get_new_annotations', 'getModalUI', 'save_metadata_cache', 
      'screenNewVisit',
      fcts
    ), 
    envir = .GlobalEnv
  )
})