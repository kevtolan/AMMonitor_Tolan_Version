#' @name dbCheckCore
#' @title Compares the core fields and lists in an AMMonitor  database with 
#' the built-in AMMonitor default dictionary
#' @description Checks a database to find mismatches between it and the core columns of the default AMMonitor database
#' @param con  An open connection to a database
#' @param disconnect  TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @export
#' @importFrom DBI dbIsValid dbDisconnect dbReadTable
#' @importFrom  readxl read_excel
#' @family database
#' @usage dbCheckCore(con, disconnect = FALSE)
#' @details This function should be used occasionally to ensure that the a 
#' given AMMonitor database that is in use has the "core" tables, "core" fields,
#' and "core" lists that come with a new, default AMMonitor database.  Most 
#' AMMonitor functions rely on this "core" setup; this function checks for 
#' any inconsistencies and provides output that points to them.  This function
#' will NOT fix the inconsistencies; rather it is up to the user to fix them
#' in their database.
#' 
#' The output is a list with seven sections. One is a list "description" that 
#' briefly explains errors that were found. Each of the other six are 
#' data.frames containing mappings to any mismatches between the input database 
#' and the default AMMonitor 
#' database. The "dictionary_check" data.frame shows discrepancies between the 
#' database and the default within core fields of the "dbdictionary" table. 
#' Similarly, each of the other data.frames returned in the output will reveal 
#' mismatches for the "lists" and "listitems" tables, the "librarylists" table, 
#' the "librarylistitems" table, the "medialists" table, and the 
#' "medialistitems" table. In each of these data.frames, the prefix "db_" 
#' describes information from the database passed into the dbCheckCore 
#' function, and the prefix "default_" describes information pertaining to the 
#' default AMMonitor database. 
#' 
#' #' See the "database" learnr tutorial for more details on the
#' database. The tutorial can be launched with
#' \code{learnr::run_tutorial(name = "database", package = "AMMonitor")}.
#' 
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
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#' 
#' # list the tables in the AMMonitor database
#' DBI::dbListTables(conx)
#' 
#' # check the database core columns against the AMMonitor default
#' results <- dbCheckCore(con = conx, disconnect = FALSE)
#'  
#' # the errors, if present, will be given in the captured output
#' str(results)
#' 
#' # -----------------------------------------------------
#' # the demo has no errors
#' # let's change the demo dictionary so you can see the errors returned
#' 
#' # adding dictionary mismatch
#' DBI::dbExecute(
#'  conx, 
#'  statement = "UPDATE dbdictionary SET pk_fieldname = 'primary' 
#'    WHERE pk_fieldname = 'primary_account';"
#'  )
#' 
#' # adding librarylist mismatch
#' DBI::dbExecute(
#'  conx, 
#'  statement = "UPDATE librarylists 
#'    SET pk_librarylistid = 'ageClass' 
#'    WHERE pk_librarylistid = 'age_class';"
#'  )
#' 
#' 
#' # check the database core columns against the AMMonitor default
#' results <- dbCheckCore(con = conx, disconnect = TRUE)
#' 
#' # look at results
#' lapply(results,  FUN = head)
#' 
#' # -----------------------------------------------------
#' 
#' # remove the demo AMMonitor project
#' unlink(demo_fp, recursive = TRUE)
#' 
#' }

dbCheckCore <- function(con, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    }) 
  }
  
  # retrieve base dictionary
  basePath <- paste0(
    find.package("AMMonitor", lib.loc = .libPaths()), 
    "/extdata/dbCreate.xlsx"
  )
  
  # read in the con dictionary, trim to core fields, and order
  df1 <- DBI::dbReadTable(con, "dbdictionary")
  df1 <- df1[df1$core == 1, ] 
  indices <- order(df1$pk_tablename, df1$sort_order)
  df1 <- df1[indices,]
  
  # read in the package excel file and order
  df2 <- as.data.frame(readxl::read_excel(basePath, sheet = "dbdictionary"))
  indices <- order(df2$pk_tablename, df2$sort_order)
  df2 <- df2[indices,]
  
  # set up df to hold results
  dictionary_check <- data.frame(
    db_row = integer(), 
    db_table = character(),
    db_field = character(),
    db_column = character(), 
    db_value = character(), 
    default_value = character(), 
    stringsAsFactors = FALSE)
  
  # loop through dfs and compare
  for (i in 1:nrow(df1)) {
    
    # identify if columns are different
    differing_columns <- which(df1[i, ] != df2[i, ])
    
    # identify mismatches
    if (length(differing_columns) > 0) {

      for (col in differing_columns) {
        dictionary_check  <- rbind(
          dictionary_check, 
          data.frame(
            db_row = i,
            db_table = as.character(df1[i, "pk_tablename"]),
            db_field = as.character(df1[i, "pk_fieldname"]),
            db_column = names(df1)[col], 
            db_value = as.character(df1[i, col]), 
            default_value = as.character(df2[i, col]), 
            stringsAsFactors = FALSE
          )
        )
      } # end of column j
    } # end of if column mismatches
  } # end of row i
  
  # check core listitems ------
  listitems_check <- data.frame(
    default_list  = character(),
    default_item = character(),
    db_list = character(), 
    db_item = character()
  )
  
  # read in the listitems
  listitems <- as.data.frame(readxl::read_excel(basePath, sheet = "listitems"))
  db_listitems <- DBI::dbReadTable(con, "listitems")
  
  # check each core listitem: fk_listid and item
  for (i in nrow(listitems)) {
    
    # extract the list and item
    item_i_base <- listitems[i, c("fk_listid", "item")]
    
    # merge the two
    merged_i <- merge(item_i_base, db_listitems)
    
    # log if mismatch
    if (any(is.na(merged_i$fk_listid), is.na(merged_i$item))) {
      listitems_check <- rbind(
        listitems_check,
        data.frame(
          default_list  = item_i_base$fk_listid,
          default_item = item_i_base$item,
          db_list = merged_i$fk_listid, 
          db_item = merged_i$item)
      )
    } # end of mismatch
  } # end of listitem i
  
  # check the librarylists ----------
  
  librarylists <- as.data.frame(
    readxl::read_excel(
      path = basePath,
      sheet = "librarylists", 
      col_types = c("text", "numeric", "text", "text", "numeric", "numeric", 
        "numeric", "text", "text", "text")
    )
  )
  
  # fix imported datatypes
  librarylists$core_list <- as.integer(librarylists$core_list)
  librarylists$photos <- as.integer(librarylists$photos)
  librarylists$recordings <- as.integer(librarylists$recordings)
  librarylists$videos <- as.integer(librarylists$videos)
                                                   
  db_librarylists <- DBI::dbReadTable(con, "librarylists")
  
  # set up dataframe to hold results
  librarylist_check <- data.frame(
    "default_librarylist" = character(0),
    "default_col" = character(0),
    "default_value" = character(0),
    "default_type" = character(0),
    "db_list" = character(0),
    "db_value" = character(0),
    "db_type" = character(0)
  )
  
  for (i in 1:nrow(librarylists)) {
    
    # extract the base librarylist 
    item_i_base <- librarylists[i, ]
    item_i_name <- item_i_base[1, "pk_librarylistid", drop = TRUE]
    
    # extract the database librarylist
    index <- which(db_librarylists$pk_librarylistid == item_i_name)
    item_i_db <- db_librarylists[index, ]
    
    # if the list is missing, log it
    if (nrow(item_i_db) == 0) {
      
      data_i <- data.frame(
        "default_librarylist" = item_i_base[1, "pk_librarylistid", drop = TRUE],
        "default_col" = "MISSING",
        "default_value" = "MISSING",
        "default_type" = "MISSING",
        "db_list" = "MISSING",
        "db_value" = "MISSING",
        "db_type" = "MISSING"
      )
      
      librarylist_check <- rbind(librarylist_check, data_i)
      
      next
    }
    
    differences <- item_i_base != item_i_db
    
    if (sum(differences, na.rm = TRUE) > 0) {
      for (j in 1:ncol(differences)) {
        if (differences[1,j] != TRUE | is.na(differences[1,j])) next
        
        data_i <- data.frame(
          "default_librarylist" = item_i_base[1, "pk_librarylistid", drop = TRUE],
          "default_col" = names(item_i_base[j]),
          "default_value" = item_i_base[1, j],
          "default_type" = typeof(item_i_base[1, j]),
          "db_list" = item_i_db[1, "pk_librarylistid", drop = TRUE],
          "db_value" = item_i_db[1, j, drop = TRUE],
          "db_type" = typeof(item_i_db[1, j])
        )
        
        librarylist_check <- rbind(librarylist_check, data_i)
        
      } # end of differences
    } # end of bind differences
  } # end of core librarylist i
  
  rownames(librarylist_check) <- NULL
  
  # check librarylistitems ------------
  
  librarylistitems_check <- data.frame(
    default_list  = character(),
    default_item = character(),
    db_list = character(), 
    db_item = character()
  )
  
  # read in the librarylistitems
  librarylistitems <- as.data.frame(readxl::read_excel(basePath, sheet = "librarylistitems"))
  db_librarylistitems <- DBI::dbReadTable(con, "librarylistitems")
  
  # check each core librarylistitem
  for (i in nrow(librarylistitems)) {
    
    # extract the list and item
    item_i_base <- librarylistitems[i, c("fk_librarylistid", "item")]
    
    # merge the two
    merged_i <- merge(item_i_base, db_librarylistitems)
    merged_i <- merged_i[, names(item_i_base)]
    
    # log if mismatch
    if (any(is.na(merged_i$fk_librarylistid), is.na(merged_i$item))) {
      librarylistitems_check <- rbind(
        librarylistitems_check,
        data.frame(
          default_list  = item_i_base$fk_librarylistid,
          default_item = item_i_base$item,
          db_list = merged_i$fk_librarylistid, 
          db_item = merged_i$item)
      )
    } # end of mismatch
  } # end of listitem i
  
  
  # check the medialists ----------
  
  medialists <- as.data.frame(
    read_excel(
      path = basePath, 
      sheet = "medialists", 
      col_types = c("text", "numeric", "text", "text", "numeric", "numeric", "numeric"))
  )
  
  # remove description
  medialists$description <- NULL
  
  # fix imported datatypes
  medialists$core_list <- as.integer(medialists$core_list)
  medialists$photos <- as.integer(medialists$photos)
  medialists$recordings <- as.integer(medialists$recordings)
  medialists$videos <- as.integer(medialists$videos)
  
  db_medialists <- DBI::dbReadTable(con, "medialists")
  db_medialists$description <- NULL
  
  # set up dataframe to hold results
  medialists_check <- data.frame(
    "default_medialist" = character(0),
    "default_col" = character(0),
    "default_value" = character(0),
    "default_type" = character(0),
    "db_list" = character(0),
    "db_value" = character(0),
    "db_type" = character(0)
  )
  
  for (i in 1:nrow(medialists)) {
    
    # extract the base medialist 
    item_i_base <- medialists[i, ]
    item_i_name <- item_i_base[1, "pk_medialistid", drop = TRUE]
    
    # extract the database medialist
    index <- which(db_medialists$pk_medialistid == item_i_name)
    item_i_db <- db_medialists[index, ]
    
    # if the list is missing, log it
    if (nrow(item_i_db) == 0) {
      
      data_i <- data.frame(
        "default_medialist" = item_i_base[1, "pk_medialistid", drop = TRUE],
        "default_col" = "MISSING",
        "default_value" = "MISSING",
        "default_type" = "MISSING",
        "db_list" = "MISSING",
        "db_value" = "MISSING",
        "db_type" = "MISSING"
      )
      
      medialists_check <- rbind(medialists_check, data_i)
      
      next
    }
    
    differences <- item_i_base != item_i_db
    
    if (sum(differences, na.rm = TRUE) > 0) {
      for (j in 1:ncol(differences)) {
        if (differences[1,j] != TRUE | is.na(differences[1,j])) next
        
        data_i <- data.frame(
          "default_medialist" = item_i_base[1, "pk_medialistid", drop = TRUE],
          "default_col" = names(item_i_base[j]),
          "default_value" = item_i_base[1, j],
          "default_type" = typeof(item_i_base[1, j]),
          "db_list" = item_i_db[1, "pk_medialistid", drop = TRUE],
          "db_value" = item_i_db[1, j, drop = TRUE],
          "db_type" = typeof(item_i_db[1, j])
        )
        
        medialists_check <- rbind(medialists_check, data_i)
        
      } # end of differences
    } # end of bind differences
  } # end of core medialist i
  
  rownames(medialists_check) <- NULL
  
  # medialistitems check ------------------
    medialistitems_check <- data.frame(
    default_list  = character(),
    default_item = character(),
    db_list = character(), 
    db_item = character()
  )
  
  # read in the medialistitems
  medialistitems <- as.data.frame(readxl::read_excel(basePath, sheet = "medialistitems"))
  db_medialistitems <- DBI::dbReadTable(con, "medialistitems")
  
  # check each core medialistitem
  for (i in nrow(medialistitems)) {
    
    # extract the medialist and item
    item_i_base <- medialistitems[i, c("fk_medialistid", "item")]
    
    # merge the two
    merged_i <- merge(item_i_base, db_medialistitems)
    
    # log if mismatch
    if (any(is.na(merged_i$fk_medialistid), is.na(merged_i$item))) {
      medialistitems_check <- rbind(
        medialistitems_check,
        data.frame(
          default_list  = item_i_base$fk_medialistid,
          default_item = item_i_base$item,
          db_list = merged_i$fk_medialistid, 
          db_item = merged_i$item)
      )
    } # end of mismatch
  } # end of listitem i
  
  description_message <- c()
  
  if (nrow(dictionary_check > 0)) {
    for (i in 1:nrow(dictionary_check)) {
      description_message <- c(description_message, 
                               paste0("The database value for the column '", 
                                      dictionary_check$db_column[i], 
                                      "' in table '",
                                      dictionary_check$db_table[i], 
                                      "' is '",
                                      dictionary_check$db_value[i], 
                                      "', but in the default dictionary the value is '",
                                      dictionary_check$default_value[i],
                                      "'."))
    }
  }
  
  if (nrow(listitems_check) > 0 ) {
    for (i in 1:nrow(listitems_check)) {
      description_message <- c(description_message, 
                               paste0("The default item '", 
                                      listitems_check$default_item[i],
                                      "' in the list '", 
                                      listitems_check$default_list[i], 
                                      "' does not match the database's item '",
                                      listitems_check$db_item[i],
                                      "'."))
    }
  }
  
  if (nrow(librarylist_check) > 0 ) {
    for (i in 1:nrow(librarylist_check)) {
      description_message <- c(description_message, 
                               paste0("The default librarylist '",
                                      librarylist_check$default_librarylist[i],
                                      "' is ",
                                      librarylist_check$default_value[i],
                                      "."
                                      ))
    }
  }

  
  if (nrow(librarylistitems_check) > 0 ) {
    for (i in 1:nrow(librarylistitems_check)) {
      description_message <- c(description_message, 
                               paste0("The database librarylist item '",
                                      librarylistitems_check$db_item[i],
                                      "' in the list '",
                                      librarylistitems_check$db_list[i],
                                      "' does not match the default item '",
                                      librarylistitems_check$default_item[i],
                                      "'."
                               ))
    }
  }
  
  if (nrow(medialists_check) > 0 ) {
    for (i in 1:nrow(medialists_check)) {
      description_message <- c(description_message, 
                               paste0("The default medialist '",
                                      medialists_check$default_medialist[i],
                                      "' is ",
                                      medialists_check$default_value[i],
                                      "."
                               ))
    }
  }
  
  
  if (nrow(medialistitems_check) > 0 ) {
    for (i in 1:nrow(medialistitems_check)) {
      description_message <- c(description_message, 
                               paste0("The database medialist item '",
                                      medialistitems_check$db_item[i],
                                      "' in the list '",
                                      medialistitems_check$db_list[i],
                                      "' does not match the default item '",
                                      medialistitems_check$default_item[i],
                                      "'."
                               ))
    }
  }
  
  
  if (length(description_message) == 0) {
    description_message <- c(paste0("Congratulations! The core fields in your ",
                                    "database match those in the default ",
                                    "AMMonitor database."
                                    ))
  }
  # send messages
  print(paste0("There are ", nrow(dictionary_check),
               " dictionary mismatches in your database."))
  
  print(paste0("There are ", nrow(listitems_check),
               " lists/listitems mismatches in your database."))
  
  print(paste0("There are ", nrow(librarylist_check),
               " librarylists mismatches in your database."))

  print(paste0("There are ", nrow(librarylistitems_check),
               " librarylistitems mismatches in your database."))
  
  print(paste0("There are ", nrow(medialists_check),
               " medialists mismatches in your database."))
  
  print(paste0("There are ", nrow(medialistitems_check),
               " medialistitems mismatches in your database."))

  return(list(
    dictionary_check = dictionary_check, 
    listitems_check = listitems_check, 
    librarylist_check = librarylist_check,
    librarylistitems_check = librarylistitems_check,
    medialists_check = medialists_check,
    medialistitems_check = medialistitems_check, 
    description = description_message)
  )

} # End of function
