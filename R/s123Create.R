#' @name s123Create
#' @title Creates a Survey123 spreadsheet from an AMMonitor database
#' @description Populates the Survey123 Excel 
#' spreadsheet to enable users to collect visit data from a Survey123
#' cell phone app. The function assumes there are entries in the people, 
#' locations, and equipment tables of the database.
#'  
#' See the 'mobile_apps' learnr tutorial for instructions on creating the survey 
#' in  ArcGIS Survey123 Connect.
#' @param con Open connection to an AMMonitor database 
#' @param equip_type Type of equipment to include from 
#' the database. Options are 'camera', 'recorder', default is 'all'. 
#' Any equipment in the database with equip_type = NA will be included.
#' @param reminders An optional vector of 'checkbox'
#' reminders to add to the end of the survey form
#' @param notes An optional vector of notes to add to 
#' the end of the survey form
#' @param disconnect TRUE or FALSE. Should the 
#' database connection be severed on exit? 
#' Default is FALSE
#' @export
#' @details This function creates an Excel workbook from a 
#' given AMMonitor database that can be used to generate a 
#' cell app via ArcGIS Survey123 Connect. After running 
#' the function, the Excel file will be written to the user's 
#' working directory. 
#' 
#' 
#' This function should be used after updating the database 
#' with any new locations, people, equipment, user fields, etc. 
#' and the .xlsx file in Survey123 Connect should be 
#' updated accordingly. This will prevent data entry errors 
#' where a new location, person, etc. is not available for 
#' survey users. 
#' 
#' Please see the "mobile_apps" tutorial by running 
#' \code{learnr::run_tutorial("mobile_apps", package = "AMMonitor")}
#' 
#' @importFrom DBI dbIsValid dbDisconnect dbReadTable
#' @importFrom writexl write_xlsx
#' @usage s123Create(con, equip_type = 'all',
#'  reminders = NULL, notes = NULL, disconnect = FALSE)
#' @return filepath to the data source .xlsx file
#' @export
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor file structure in a temporary directory
#' # (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # Set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # Create the Survey123 spreadsheet
#' filepath <- s123Create(
#'   con = conx, 
#'   reminders = c(
#'     'Is the case secured?',
#'     'Is the memory card secured?'),
#'   notes = c('Remember to label your SD card slip!'),
#'   disconnect = TRUE
#' )
#' 
#' # look at the filepath of the Excel file
#' filepath
#' 
#' # remove the demo
#' unlink(demo_fp)
#'   
#' }
s123Create <- function(con, equip_type = 'all', reminders = NULL, notes = NULL, disconnect = FALSE) {
  
  # Initial setup ----------------------------
  
  # test arguments 
  
  if (equip_type != 'all' & 
      equip_type != 'camera' &
      equip_type != 'recorder') {
    stop('Please enter a valid equip_type: "camera", "recorder", or "all".')
  }
  
  # check connection
  if (!DBI::dbIsValid(con)) {
    stop('The database connection is invalid.')
  }
  
  # disconnect on exit
  if (disconnect) {
    on.exit(
      DBI::dbDisconnect(con)
    )
  }
  
  # read in dictionary 
  dictionary <- DBI::dbReadTable(conn = con,
                                 name = 'dbdictionary')
  
  # get dictionary entries for the visits table 
  dict_visits <- dictionary[
    which(dictionary$pk_tablename == 'visits')
    ,]
  
  # remove autonumber for visit id
  index <- which(dict_visits$shiny_input == 'locked')
  
  if (index > 0) {
    dict_visits <- dict_visits[-index,]
  }
  
  # Choices Tab --------------------------
  # column names in 'choices' tab
  choices_names <- c('list_name', 'name', 'label',
                     'first_name', 'email', 'fk_equipmodelid')
  
  # create empty choices dataframe
  choices <- data.frame(matrix(nrow = 0,
                               ncol = length(choices_names)))
  
  colnames(choices) <- choices_names
  
  # populate choices, listitems -------------
  
  lists <- dict_visits$fk_listid
  lists <- lists[!is.na(lists)]
  
  listitems <- DBI::dbReadTable(conn = con,
                                name = 'listitems')
  
  
  for (list in lists) {

    list_listitems <- listitems[
      which(listitems$fk_listid == list),
    ]
    
    if (nrow(list_listitems) == 0) {
      
      warning(paste0('The list "',
                     list,
                     '" has no options in the listitems table of the database. It was excluded from the survey. It is advised that you add to the listitems table in the database and re-run this function.'))
      
      next
    } # end if empty listitems 
    
    choices <- rbind(choices,
                     data.frame(
                       'list_name' = list_listitems$fk_listid,
                       'name' = list_listitems$item,
                       'label' = paste0(
                         toupper(list_listitems$item),
                         ': ',
                         list_listitems$description
                       ),
                       'first_name' = '',
                       'email' = '',
                       'fk_equipmodelid' = ''
                     ))
    
  } # end for list in lists
  
  # populate choices, tables connected through foreign keys -----
  
  # add list_equipment for relevant data_types
  equipment <- DBI::dbReadTable(conn = con, 
                                name = 'equipment')
  
  equipment <- equipment[order(equipment$pk_equipmentid),]
  
  
  if (equip_type == 'camera') {
    
    equipment <- equipment[
      which(equipment$equip_type == 'camera' | 
              is.na(equipment$equip_type))
      ,]
    
  } else if (equip_type == 'recorder') {
    
    equipment <- equipment[
      which(equipment$equip_type == 'recorder' | 
              is.na(equipment$equip_type))
      ,]
  }
  
  # add equipment to choices
  equipment_list <- dict_visits$pk_fieldname[
    which(dict_visits$foreign_key_table == 'equipment')
  ]
  
  
  
  choices <- rbind(choices,
                   data.frame(
                     'list_name' = equipment_list,
                     'name' = equipment$pk_equipmentid,
                     'label' = equipment$pk_equipmentid,
                     'first_name' = '',
                     'email' = '',
                     'fk_equipmodelid' = equipment$fk_equipmodelid
                   ))
  
  # add people to choices
  
  people <- DBI::dbReadTable(conn = con,
                             name = 'people')
  
  people <- people[order(people$pk_personid),]
  
  people_list <- dict_visits$pk_fieldname[
    which(dict_visits$foreign_key_table == 'people')
  ]
  
  
  
  choices <- rbind(choices,
                   data.frame(
                     'list_name' = people_list,
                     'name' = people$pk_personid,
                     'label' = people$pk_personid,
                     'first_name' = people$first_name,
                     'email' = people$email,
                     'fk_equipmodelid' = ''
                   ))
  
  
  # add econfignames to choices
  
  econfignames <- DBI::dbReadTable(conn = con,
                                   name = 'econfignames')
  
  if (nrow(econfignames) != 0) {
    
    econf_list <- dict_visits$pk_fieldname[
      which(dict_visits$foreign_key_table == 'econfignames')
    ]
    
    choices <- rbind(choices,
                     data.frame(
                       'list_name' = econf_list,
                       'name' = econfignames$pk_econfignameid,
                       'label' = paste0(
                         econfignames$pk_econfignameid,
                         ': ',
                         econfignames$description
                       ),
                       'first_name' = '',
                       'email' = '',
                       'fk_equipmodelid' = ''
                     ))
  } # end adding econfignames if not empty
  
  # add other foreign key tables to choices
  fk_tables <- dict_visits$foreign_key_table
  fk_tables <- fk_tables[!is.na(fk_tables)]
  fk_tables <- fk_tables[which(fk_tables != 'econfignames')]
  
  for (table in fk_tables) {
    
    choice_list_name <- dict_visits$pk_fieldname[
      which(dict_visits$foreign_key_table == table)
    ]
    # if already added, ex people:
    if (choice_list_name %in% choices$list_name) next
    
    # add to choices
    
    table_data <- DBI::dbReadTable(conn = con,
                                   name = table)
    
    table_data <- table_data[order(table_data[,1]),]
    
    choices <- rbind(choices,
                     data.frame(
                       'list_name' = choice_list_name,
                       'name' = table_data[,1],
                       'label' = table_data[,1],
                       'first_name' = '',
                       'email' = '',
                       'fk_equipmodelid' = ''
                     ))
  }
  
  # Survey Tab ----------------------------------
  
  # create survey dataframe
  survey_names <- c('type',	'name',	'label',
                    'hint',	'guidance_hint',	'appearance',
                    'required',	'calculation',	'constraint',
                    'constraint_message',	'relevant',
                    'choice_filter',	'repeat_count',
                    'media::audio',	'media::image',
                    'bind::type',	'bind::esri:fieldType',
                    'bind::esri:fieldLength',
                    'bind::esri:fieldAlias',
                    'body::esri:style',	'bind::esri:parameters',
                    'parameters',	'body::accept')
  
  survey <- data.frame(matrix(nrow = 0,
                              ncol = length(survey_names)))
  
  colnames(survey) <- survey_names
  
  # add core fields to survey - make required
  # - use description for hint only not for cascade
  # - get data type for field and adjust field length
  # - if dropdown, add select_one to type, if not, add proper integer or text etc.
  # see skip if dropdown or foreign key not in choices
  
  core_fields <- dict_visits[
    which(dict_visits$core == 1)
    ,]
  
  for (i in 1:nrow(core_fields)) {
    
    field_name <- core_fields$pk_fieldname[i]

    if (field_name == 'visit_time') next
    
    append_data <- data.frame(matrix(NA, nrow = 1,
                                     ncol = ncol(survey)))
    
    colnames(append_data) <- colnames(survey)
    
    label_ <- switch(field_name,
                     'fk_personid' = 'Person ID',
                     'fk_locationid' = 'Location ID',
                     'fk_equipmentid' = 'Equipment ID',
                     'visit_type' = 'Visit Type',
                     'visit_date' = 'Visit Date',
                     'visit_notes' = 'Visit Notes',
                     'fk_econfigid' = 'Econfiguration ID')
    
    hint <- switch(field_name,
                   'fk_personid' = 'Who are you?',
                   'fk_locationid' = '',
                   'fk_equipmentid' = '',
                   'visit_type' = 'Please choose the visit type (Set/Check/Pull)',
                   'visit_date' = 'When was the visit?',
                   'visit_notes' = '',
                   'fk_econfigid' = '')
    
    append_data$label[1] <- label_
    append_data$hint[1] <- hint
    
    # if foreign key
    if (core_fields$shiny_input[i] == 'foreignKey') {
      
      append_data$type[1] <- paste0(
        'select_one ',
        field_name
      )
      
      append_data$name[1] <- field_name
      
      append_data$appearance[1] <- 'autocomplete'
      
      append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeString'
      append_data$`bind::esri:fieldLength`[1] <- 50
      
    } else if (core_fields$shiny_input[i] == 'dropdown') { 
      
      # end if foreign key, begin if dropdown
      
      append_data$type[1] <- paste0(
        'select_one ',
        core_fields$fk_listid[i]
      )
      
      append_data$name[1] <- field_name
      
      append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeString'
      append_data$`bind::esri:fieldLength`[1] <- 50
      
    } else if (core_fields$shiny_input[i] == 'date') {
      
      # if shiny input is date
      append_data$type[1] <- 'dateTime'
      
      append_data$name[1] <- 'visitDateTime'
      
      append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeDate'
      
    } else if (core_fields$shiny_input[i] == 'time') {
      
      next
      
    } else if (core_fields$shiny_input[i] == 'longtext') {
      
      # if shiny input is longtext
      append_data$type[1] <- 'text'
      
      append_data$name[1] <- field_name
      
      append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeString'
      append_data$`bind::esri:fieldLength`[1] <- 255
    } 
    
    
    if (core_fields$shiny_input[i] != 'longtext') {
      append_data$required[1] <- 'yes'
    }
    
    # add append_data to survey dataframe, check if select_one is in choices first
    if (core_fields$shiny_input[i] != 'date') {
      append_data$`bind::esri:fieldAlias` <- field_name
    }
    
    
    if (grepl('select_one', append_data$type) &
        !append_data$name %in% choices$list_name) next
    
    survey <- rbind(survey,
                    append_data)
    
  } # end loop through core fields
  
  # Non-core fields ----------------
  noncore_fields <- dict_visits[
    which(dict_visits$core == 0)
    ,]
  
  if (nrow(noncore_fields) != 0) {
    for (i in 1:nrow(noncore_fields)) {
      
      field_name <- noncore_fields$pk_fieldname[i]
      
      append_data <- data.frame(matrix(NA, nrow = 1,
                                       ncol = ncol(survey)))
      
      colnames(append_data) <- colnames(survey)
      
      # if foreign key
      if (noncore_fields$shiny_input[i] == 'foreignKey') {
        
        append_data$type[1] <- paste0(
          'select_one ',
          field_name
        )
        
        append_data$name[1] <- field_name
        append_data$label[1] <- field_name
        
        append_data$appearance[1] <- 'autocomplete'
        
        append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeString'
        append_data$`bind::esri:fieldLength`[1] <- 50
        
      } else if (noncore_fields$shiny_input[i] == 'dropdown') { 
        
        # end if foreign key, begin if dropdown
        
        append_data$type[1] <- paste0(
          'select_one ',
          noncore_fields$fk_listid[i]
        )
        
        append_data$name[1] <- field_name
        if (is.na(noncore_fields$description[i])) {
          append_data$label[1] <- field_name
        } else {
          append_data$label[1] <- paste0(field_name,
                                         ': ',
                                         noncore_fields$description[i])
        }
        
        append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeString'
        append_data$`bind::esri:fieldLength`[1] <- 50
        
      } else if (noncore_fields$shiny_input[i] == 'date') {
        
        # if shiny input is date
        append_data$type[1] <- 'date'
        
        append_data$name[1] <- field_name
        append_data$label[1] <- field_name
        
        append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeDate'
        
      } else if (noncore_fields$shiny_input[i] == 'time') {
        
        # if shiny input is time
        append_data$type[1] <- 'time'
        
        append_data$name[1] <- field_name
        append_data$label[1] <- field_name
        
        append_data$`bind::type`[1] <- 'time'
        
      } else if (noncore_fields$shiny_input[i] == 'longtext') {
        
        # if shiny input is longtext
        append_data$type[1] <- 'text'
        
        append_data$name[1] <- field_name
        
        if (is.na(noncore_fields$description[i])) {
          append_data$label[1] <- field_name
        } else {
          append_data$label[1] <- paste0(field_name,
                                         ': ',
                                         noncore_fields$description[i])
        }
        
        append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeString'
        append_data$`bind::esri:fieldLength`[1] <- 255
        
      } else if (noncore_fields$shiny_input[i] == 'text') {
        
        # if shiny input is longtext
        append_data$type[1] <- 'text'
        
        append_data$name[1] <- field_name
        
        if (is.na(noncore_fields$description[i])) {
          append_data$label[1] <- field_name
        } else {
          append_data$label[1] <- paste0(field_name,
                                         ': ',
                                         noncore_fields$description[i])
        }
        
        append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeString'
        append_data$`bind::esri:fieldLength`[1] <- 50
        
      } else if (noncore_fields$shiny_input[i] == 'numeric') {
        
        # if shiny input is longtext
        append_data$type[1] <- 'integer'
        
        append_data$name[1] <- field_name
        
        if (is.na(noncore_fields$description[i])) {
          append_data$label[1] <- field_name
        } else {
          append_data$label[1] <- paste0(field_name,
                                         ': ',
                                         noncore_fields$description[i])
        }
        
        append_data$`bind::esri:fieldType`[1] <- 'esriFieldTypeInteger'
        append_data$`bind::esri:fieldLength`[1] <- 10
      } 
      
      # add append_data to survey dataframe, check if select_one is in choices first
      
      append_data$`bind::esri:fieldAlias` <- field_name
      
      
      if (grepl('select_one', append_data$type) &
          !any(grepl(append_data$name, choices$list_name))) next
      
      survey <- rbind(survey,
                      append_data)
      
    } # end loop through non-core fields
    
  } # end if there are non-core fields
  
  # Add geo point location ------------------
  
  # new empty append_data
  append_data <- data.frame(matrix(NA, nrow = 4,
                                   ncol = ncol(survey)))
  
  colnames(append_data) <- colnames(survey)
  
  # hard code geo-point rows
  append_data$type[1:4] <- c('geopoint', 'decimal',
                             'decimal', 'decimal')
  append_data$name[1:4] <- c('geo_id',
                             'phone_latitude',
                             'phone_longitude',
                             'horizontal_accuracy')
  
  append_data$`bind::esri:fieldAlias`[1:4] <- c('geo_id',
                             'phone_latitude',
                             'phone_longitude',
                             'horizontal_accuracy')
  
  append_data$label[1:4] <- c('Coordinates',
                              'Phone Latitude',
                              'Phone Longitude',
                              'Accuracy')
  append_data$hint[1] <- "Press the bullseye to register the location's lat and long."
  
  append_data$appearance[2:4] <- 'hidden'
  
  append_data$required[1] <- 'yes'
  
  append_data$calculation[2:4] <- c('pulldata("@geopoint", ${geo_id}, "y")',
                                    'pulldata("@geopoint", ${geo_id}, "x")',
                                    'pulldata("@geopoint", ${geo_id}, "horizontalAccuracy")')
  
  append_data$`bind::esri:fieldType`[2:4] <- 'esriFieldTypeDouble'
  
  
  # add geopoint info to survey
  survey <- rbind(survey, append_data)
  
  # Add iso date, formated date ===========
  
  # new empty append_data
  append_data <- data.frame(matrix(NA, nrow = 2,
                                   ncol = ncol(survey)))
  
  colnames(append_data) <- colnames(survey)
  
  
  append_data[,1] <- 'text'
  append_data[,2:3] <- c('visit_date',
                         'visit_time')
  append_data[,6] <- 'hidden'
  
  
  append_data[1, 8] <- "format-date(${visitDateTime}, '%Y-%m-%d')"
  append_data[2, 8] <- "format-date(${visitDateTime}, '%H:%M:%S')"
  
  append_data$`bind::esri:fieldType` <- 'esriFieldTypeString'
  append_data$`bind::esri:fieldLength` <- 100
  
  survey <- rbind(survey, append_data)
  
  
  # add reminders, if present
  
  if (!is.null(reminders)) {
    
    append_data_choices <- data.frame(
      matrix(
        NA,
        nrow = length(reminders),
        ncol = ncol(choices)))
    
    colnames(append_data_choices) <- colnames(choices)
    
    append_data_choices$list_name <- 'list_reminders'
    
    for (r in 1:length(reminders)) {
      
      append_data_choices$name[r] <- paste0('reminder_', r)
      append_data_choices$label[r] <- reminders[r]
    }
    
    
    append_data_survey <- data.frame(
      matrix(NA,
             nrow = 1,
             ncol = ncol(survey)))
    
    colnames(append_data_survey) <- colnames(survey)
    
    append_data_survey$type <- 'select_multiple list_reminders'
    append_data_survey$name <- 'reminders'
    append_data_survey$label <- 'Reminders'
    append_data_survey$hint <- "You're almost done! Here are a few reminders before submitting your response."
    append_data_survey$`bind::esri:fieldType` <- 'esriFieldTypeString'
    append_data_survey$`bind::esri:fieldLength` <- 100
    
    # append choices and survey
    choices <- rbind(choices,
                     append_data_choices)
    
    survey <- rbind(survey,
                    append_data_survey)
    
  } # end if !is.null(reminders)
  
  # add notes ----------------
  if (!is.null(notes)) {
    
    append_data_survey <- data.frame(
      matrix(NA,
             nrow = length(notes),
             ncol = ncol(survey)
      )
    )
    
    colnames(append_data_survey) <- colnames(survey)
    
    append_data_survey$type <- 'note'
    append_data_survey$`bind::esri:fieldType` <- 'esriFieldTypeString'
    append_data_survey$`bind::esri:fieldLength` <- 100
    
    for (n in 1:length(notes)) {
      
      append_data_survey$name <- paste0("note_", n)
      append_data_survey$label <- notes[n]
    }
    
    survey <- rbind(survey,
                    append_data_survey)
  }
  
  
  # write xlsx file to working directory
  writexl::write_xlsx(x = list('survey' = survey,
                               'choices' = choices),
                      path = 's123_data.xlsx',
                      col_names = TRUE
  )
  
  # user message
  message(paste0('A file named "s123_data.xlsx" has been written to your working directory. You may now create a new survey in ArcGIS Survey123 Connect with this file. If any changes to the database occur (new people, locations, equipment, user fields, etc.), please update the database and rerun s123Create to update your survey.'))
  
  return(paste0(getwd(), "/s123_data.xlsx"))
  
} # end function
