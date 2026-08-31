#!! ModName = registerVisitSetSourceDest
#!! ModDisplayName = Select source and destination for media files to register.
#!! ModDescription = The user selects source and destination directories.
#!! ModCitation = Laurence Clarfeld.  (2023). registerVisitSetSourceDest. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = mediaType !! Media type (photos/recordings/videos) !! Character
#!! FunctionReturn = selectedFiles !! File paths for media to be registered !! Character
#!! FunctionReturn = dirDest !! File path to destination directory !! Character
#!! FunctionReturn = storage_type !! What is the storage type (e.g., local, googledrive) || Character
#!! FunctionReturn = audio_fn_format !! File naming format for audio files !! Character


# the ui function
registerVisitSetSourceDest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('audio_filename_info')),
    wellPanel(
      tags$h2('Source'),
      shinyFiles::shinyDirButton(
        ns('source_dir'), 
        'Select a directory', 
        title='Select a SOURCE directory'
      ),
      verbatimTextOutput(ns('source_dir_name'))
    ),
    wellPanel(
      tags$h2('Destination'),
      tags$p(
        'The default destination is the media folder (photos/recoridngs/videos) 
        associated with the active AMMonitor project. If no such directory exists, 
        you can create the missing directory or select an alternative.'
      ),
      
      
      radioButtons(
        ns('storage_type'),
        'Storage Type',
        choices = c('local', 'Google Drive', 'AWS S3', 'SharePoint')
      ),
      
      uiOutput(ns('folder_select_ui')),
      
      tags$br(),
      
      verbatimTextOutput(ns('dest_dir_name'))
    ),
    wellPanel(
      htmlOutput(ns('sourceDestStatus'))
    )
  )
}


# the server function
registerVisitSetSourceDest_server <- function(id, mediaType) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dest_dir <- reactiveVal("")
    
    output$audio_filename_info <- renderUI({
      if (mediaType() == "audio") {
        tagList(
          wellPanel(
            tags$h2('File naming convention'),
            tags$p(paste('For audio data, there is no standard EXIF data that allows AMMontior to scrape the date/time of the recording. However, most ARU\'s use a standard file naming convention that embeds the date/time into the file name. Please select a file naming convention from the list below and AMMontior will parse the filename to extract date/time info.')),
            selectInput(
              ns('audio_fn_format'),
              'Audio file name format',
              choices = c('*YYYYMMDD_HHMMSS.*')
            )
          )
        )
      }
    })
    
    output$folder_select_ui <- renderUI({
      switch(
        input$storage_type,
        "local" = shinyFiles::shinyDirButton(
          ns('dest_localdir'), 
          'Select a directory', 
          title='Select a DESTINATION directory'
        ),
        "Google Drive" = textInput(
          ns('dest_googledrive'),
          'Google drive folder path', 
          placeholder = "e.g., trailcam/photos/"
        ),
        "AWS S3" = tagList(
          textInput(
            ns('s3_bucket_name'),
            'AWS S3 Bucket Name'
          ),
          textInput(
            ns('dest_s3'),
            'AWS S3 folder name', 
            placeholder = "e.g., trailcam/photos/"
          )
        ),
        "SharePoint" = tagList(
          textInput(
            ns('sharepoint_site'),
            'SharePoint Site URL',
            placeholder = "e.g., https://TENANT-ID.sharepoint.com/sites/"
          ),
          textInput(
            ns('sharepoint_folder'),
            'SharePoint Folder',
            placeholder = "e.g., trailcam/photos"
          )
        )
      )
    })
    
    dest_googledrive_temp <- reactive(input$dest_googledrive)
    dest_googledrive <- debounce(dest_googledrive_temp, 2000)
    
    observe({
      req(input$s3_bucket_name)
      if (suppressMessages(aws.s3::bucket_exists(input$s3_bucket_name))) {
        dest_dir(paste0(
          "https://", 
          input$s3_bucket_name, 
          ".s3.amazonaws.com/", 
          gsub('^/|/$', "", input$dest_s3)
        ))
        showModal(modalDialog(
          title = 'AWS S3 bucket found',
          paste('Setting destination folder in AWS S3 to:', dest_dir()),
          easyClose = TRUE
        ))
      } else {
        dest_dir("")
        showModal(modalDialog(
          title = 'AWS S3 bucket not found',
          paste('Either the specified bucket does no exist or you have not properly authenticated to access the bucket.'),
          easyClose = TRUE
        ))
      }
    }) |> bindEvent(input$s3_bucket_name, input$dest_s3)
    
    observeEvent(dest_googledrive(), {
      if (dest_googledrive() != "") {
        drive_item <- googledrive::with_drive_quiet(googledrive::drive_get(path = dest_googledrive()))
        if (nrow(drive_item) == 1 && googledrive::is_folder(drive_item)) {
          dest_dir(dest_googledrive())
          showModal(modalDialog(
            title = 'Google folder found',
            paste('Setting destination folder in Google Drive to:', dest_googledrive()),
            easyClose = TRUE
          ))
        } else {
          dest_dir("")
          showModal(modalDialog(
            title = 'Google folder not found',
            ifelse(
              nrow(drive_item) != 1,
              paste('Total of ', nrow(drive_item), 'items found. Specified folder must return a single, valid Google Drive folder path.'),
              'Specified Google Drive item was not a folder.'
            ),
            easyClose = TRUE
          ))
        }
      } else {
        dest_dir("")
      }
    })
    
    observe({
      req(input$sharepoint_site)
      
      dest_dir("")
      
      rs <- tryCatch(
        {
          site <- Microsoft365R::get_sharepoint_site(
            site_url = input$sharepoint_site,
            tenant = gsub("^.*https://([^//.]+).*", "\\1", input$sharepoint_site)
          )
          drv <- site$get_drive()
          if (input$sharepoint_folder != "") {
            sp_item <- drv$get_item(input$sharepoint_folder)
            dest_dir(sp_item$properties$webUrl)
          } else {
            dest_dir(drv$properties$webUrl)
          }
        },
        error = function(cond) {
          cond
        }
      )
      
      if ('error' %in% class(rs)) {
        showModal(modalDialog(
          title = 'Failed to connect to SharePoint',
          HTML(rs$message),
          easyClose = TRUE
        ))
      }
      
    }) |> bindEvent(input$sharepoint_site, input$sharepoint_folder)
    
    sourceDirname <- reactive({shinyFiles::parseDirPath(volumes, input$source_dir)})
    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, 'source_dir', roots=volumes, session=session)
    
    observe({
      req(mediaType())
      shinyFiles::shinyDirChoose(input, 'dest_localdir', roots=volumes, session=session)
      dest_dir({
        selectedDir <- shinyFiles::parseDirPath(volumes, input$dest_localdir)
        if (length(selectedDir) == 0) {
          defaultFolderName <- switch(
            mediaType(),
            'photo' = 'photos',
            'audio' = 'recordings'
          )
          defaultDestFolder <- paste(ammPath, defaultFolderName, sep = '/')
          if (dir.exists(defaultDestFolder)) {
            selectedDir <- defaultDestFolder
          }
        }
        selectedDir
      })
    }) |> bindEvent(input$dest_localdir, mediaType())
    
    
    selectedFiles <- reactiveVal()
    n_files <- reactiveVal(0)
    
    ## Observe input dir. changes
    observe({
      req(mediaType())
      if(!is.null(sourceDirname)){
        if (length(sourceDirname()) != 0) {
          allFiles <- list.files(sourceDirname(), recursive = TRUE)
          
          if (mediaType() == 'photo') {
            valid_mask <- tolower(tools::file_ext(allFiles)) %in% c('jpg', 'jpeg', 'png', 'tiff')
          } else {
            valid_mask <- tolower(tools::file_ext(allFiles)) %in% c('wav', 'mp3')
          }
          
          selectedFiles(paste(sourceDirname(), allFiles, sep = '/')[valid_mask])
          n_files(length(selectedFiles()))
        }
        
        output$source_dir_name <- renderText(paste0(
          'Selected SOURCE folder: ', sourceDirname(),
          '\nNumber of ',
          ifelse(mediaType() == "photo", 'image', 'audio'),
          ' files found: ', n_files()
        ))
      }
    })
    
    observe({
      req(mediaType())
      if(length(dest_dir()) != 0){
        message <- paste0(
          'Selected DESTINATION folder: ', 
          dest_dir()
        )
      } else {
        message <- paste(
          'Default DESTINATION folder, ',
          paste(ammPath, mediaType(), sep = '/'),
          'DOES NOT exists. Either create this directory or select a different directory above.'
        )
      }
      # output$dest_dir_name <- renderText(message)
    })
    
    output$dest_dir_name <- renderText({
      if (dest_dir() == "") {
        "Select a valid local or web destination for media files"
      } else {
        paste('Selected DESTINATION folder:', dest_dir())
      }
    })
    
    observe({
      output$sourceDestStatus <- renderUI({
        if (isTruthy(sourceDirname()) && isTruthy(dest_dir())) {
          div(
            icon("check", style = "color:green;"),
            "SOURCE and DESTINATION selected."
          )
        } else {
          div(
            icon("xmark", style = "color:red;"), 
            "Select both a SOURCE and DESTINATION."
          )
        }
      })
    })
    
    return(
      reactiveValues(
        selectedFiles = reactive(selectedFiles()),
        dirDest = reactive(dest_dir()),
        storage_type = reactive(input$storage_type),
        audio_fn_format = reactive(input$audio_fn_format)
      )
    )
  })
}
