#' @name dbAddUserCol
#' @aliases dbAddUserCol
#' @title Add a user column to an AMMonitor database
#' @description Add a unique user-defined (non-core) column to an 
#' AMMonitor database, and update the 
#' dbdictionary to reflect changes in the database schema. 
#' @param con An open connection to an AMMonitor database
#' @param col_info  A dataframe with new column information. Use this option only
#' if your new column is not a drop-down. If it is a drop-down, enter the 
#' information in an Excel file. See Details.
#' @param excel_fp  Filepath to an Excel file with the new column information  
#' @param disconnect TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @family database
#' @usage dbAddUserCol(con, col_info = NULL, excel_fp = NA, disconnect = FALSE)
#' @seealso \code{\link{dbCreate}}, \code{\link{dbAddUserTable}}
#' @importFrom DBI dbIsValid dbExecute dbAppendTable dbReadTable dbDisconnect
#' @export
#' @return An updated database structure with updated dictionary table
#' @details If your new column includes drop-downs, use the template Excel file 
#' to store information about the new 
#' database column(s) and associated information.  The template Excel file comes 
#' with AMMonitor and is named "dbUserCol.xlsx".  This template can be copied
#' to your working directory with the following code:
#' 
#'  \code{fp <- system.file("extdata/dbUserCol.xlsx", package = "AMMonitor")}
#'  
#'  \code{file.copy(from = fp, to = getwd(), overwrite = FALSE)}
#'  
#'  From there, open the Excel file and fill it in. Note there are multiple 
#'  sheets in the Excel file; see below.
#'  
#'  The template contains a 
#'  sample userColumn named "card_size" as an example, which is a column
#'  whose entries will be restricted to a list named "list_card_size", with 
#'  options of '64GB', 128GB', and '256GB'.
#'  
#'  Once the Excel file is filled out with new column information,  point to this 
#'  new file to run the dbAddUserCol() function. 
#'  
#'  The Excel file has three sheets:
#'  
#'  Sheet 1: dbUserColInfo:
#' \itemize{
#'   \item \strong{table_name} = a valid database table name.  The list of tables 
#'   in a database
#'    can be obtained with \code{DBI::dbListTables(con)}
#'  \item \strong{field_name} = the name of the new user-column to be created.  
#'  Lower case will
#'  be enforced. Avoid spaces if possible.  E.g., my_new_column
#'  \item \strong{var_type} = the type of variable the column will store.  
#'  Options include
#'    "VARCHAR(255)", "TEXT", "INTEGER", "REAL".  For characters, VARCHAR(255) 
#'    will store up to 255 characters, while TEXT stores more than that.  For 
#'    columns that store numeric values, INTEGER will store integers while 
#'    REAL will store decimals.  
#'  \item \strong{not_null_clause} = clause to enforce required fields. Options
#'   include "NOT
#'     NULL", "UNIQUE NOT NULL".   If a column can contain null values, leave
#'     this blank.
#'  \item \strong{default_value} = column's default value, if applicable. If 
#'  there is no
#'     default value, leave this blank.
#'  \item \strong{fk_listid} = name of a list that controls the options for 
#'  the column (e.g., 
#'     if the column will hold a list of acceptable entries in a dropdown, 
#'     name that list here and create the list and list items on the next 
#'     two sheets).
#'  \item \strong{max} = maximum allowable value for numeric datatypes
#'  \item \strong{min} = minimum allowable value for numeric datatypes
#'  \item \strong{description} = description of the field's purpose.
#'  }
#'
#' Sheet 2: lists:  
#'  If the new database user column will contain entries limited 
#' to a list, create the list name on this sheet.  Use lower case for list 
#' names.
#' 
#' Sheet 3: listitems: 
#' If the new database user column will contain entries limited to a list,
#' create the acceptable list entries on this sheet. You must identify
#' the list name (fk_listid), the item, and a description of the item.
#' 
#' See the "database" learnr tutorial for more details on the
#' database. The tutorial can be launched with
#' \code{learnr::run_tutorial(name = "database", package = "AMMonitor")}.
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
#' # to work with the database, set a connection
#' conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))
#'
#' # look at the fields in visits table (these are AMMonitor core-fields)
#' DBI::dbListFields(conx, name = "visits")
#' 
#' # add a new column called "air_temperature"; does not have a dropdown
#' new_column <-  data.frame(
#'  pk_tablename = "visits",
#'  pk_fieldname = "air_temperature",
#'  core = 0,
#'  var_type = "REAL",
#'  not_null_clause = NA_character_,
#'  default_value = NA_character_,
#'  max = NA,
#'  min = NA,
#'  fk_listid = NA_character_,
#'  description = "Air temperature at time of visit")
#'  
#' # add the new column to the visits table
#' AMMonitor::dbAddUserCol(
#'  conx, 
#'  col_info = new_column,
#'  disconnect = FALSE
#'  )
#' 
#' # ------------------------------------------------------------
#' # if user columns include a dropdown, use the Excel template.
#' # add the example user column that comes with the AMMonitor package
#' # here, we will be adding a user column named "card_size" to
#' # the visits table, where entries are limited to items in the "list_card_size" 
#' # list.
#' # ------------------------------------------------------------
#' 
#' # set the filepath to the Excel file that has the required information
#' fp <- system.file("extdata/dbUserCol.xlsx", package = "AMMonitor")
#' 
#' # glimpse (but don't load) at sheets in the dbUserCol spreadsheet 
#' 
#' # sheet = dbUserColInfo
#' readxl::read_excel(
#'  path = fp,
#'  sheet = "dbUserColInfo", 
#'  col_types = c(
#'   "text", "text", "text", "text", "text", "text", "numeric", "numeric", "text")
#' )
#'  
#' # sheet = lists 
#' readxl::read_excel(path= fp, sheet = "lists")
#'   
#' # sheet = listitems
#' readxl::read_excel(path = fp, sheet = "listitems")
#' 
#' # --------------------------------------------------------
#'
#' # add the template user column to the database
#' dbAddUserCol(
#'   con = conx,
#'   col_info = NULL,
#'   excel_fp = fp,
#'   disconnect = FALSE)
#'
#' # list the fields in the visits table and note the new user column 
#' DBI::dbListFields(conx, name = "visits")
#'
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' 
#' 
#'}

dbAddUserCol <- function(con, col_info = NULL, excel_fp = NA, disconnect = FALSE) {
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    }) 
  }
  
  # check that either col_info is NULL or excel_fp is NA; not both
  if (!xor(is.null(col_info), is.na(excel_fp))) {
    stop("Please provide EITHER col_info OR excel_fp.")
  }

  # if new info is in spreadsheet -------------------

  if (!is.na(excel_fp)) {
    
    lists <- readxl::read_excel(excel_fp, sheet = "lists")
    lists$core_list <- 0
    
    DBI::dbAppendTable(conn = con, name = "lists", value = lists)
    
    # append new listitems ------------------------
    listitems <- readxl::read_excel(excel_fp, sheet = "listitems")
    DBI::dbAppendTable(conn = con, name = "listitems", value = listitems)
    
    # add new user columns  -----------------------
    col_info <- readxl::read_excel(
      path = excel_fp,
      sheet = "dbUserColInfo", 
      col_types = c(
        "text", "text", "text", "text", "text", "text", "numeric", "numeric", "text")
    )

  # append new listitems ------------------------
  listitems <- readxl::read_excel(excel_fp, sheet = "listitems")
  DBI::dbAppendTable(conn = con, name = "listitems", value = listitems)
  
  # add new user columns  -----------------------
  col_info <- readxl::read_excel(
    path = excel_fp,
    sheet = "dbUserColInfo", 
    col_types = c(
     "text", "text", "text", "text", "text", "text", "numeric", "numeric", "text")
    )

  } # end of excel file
  
  # loop through user columns
  for (i in 1:nrow(col_info)) {

    # Add Field name and data type
    stmnt <- paste0(
      "ALTER TABLE ",
      col_info$pk_tablename[i],
      " ADD '",
      col_info$pk_fieldname[i],
      "'  ",
      col_info$var_type[i]
    )

    # add default value
    if (!is.na(col_info[i, 'default_value'])) {
      stmnt <- paste0(stmnt, " DEFAULT ", col_info$default_value[i])
    }

    # add not null
    if (!is.na(col_info[i, 'not_null_clause'])) {
      stmnt <- paste0(stmnt, " ", col_info[i, 'not_null_clause'])
    }

    # close off the query statement
    stmnt <- paste0(stmnt, ";")
  
    # execute the query
    DBI::dbExecute(
        conn = con,
        statement = stmnt
    )

    # Populate the "dbdictionary" table
    dictionary <- DBI::dbReadTable(con, "dbdictionary")
    dictionary_i <- dictionary[which(dictionary$pk_tablename == col_info[i, "pk_tablename", drop = TRUE]),]
    
    # Add in sort order
    new_data <- col_info[i,]
    new_data$sort_order <- max(dictionary_i$sort_order) + 1
    new_data$core <- 0
    new_data$pk <- 0

    # Add in shiny default
    shiny_default <- switch(
      EXPR = new_data$var_type,
      "VARCHAR(255)" = "text",
      "TEXT" = "longtext",
      "INTEGER" = "numeric",
      "REAL" = "numeric"
      )
  
    if (!is.na(new_data$fk_listid)) shiny_default = "dropdown"
    new_data$shiny_input <- shiny_default

    # append to database dictionary table
    rs <- DBI::dbAppendTable(con, "dbdictionary", new_data)
    
    # add message if successful
    if (rs == 1) {
      cat(paste0("A new column named ", col_info[i, "pk_fieldname", drop = TRUE],
        " was added to the ", col_info[i, "pk_tablename", drop = TRUE]), " table in 
        your AMMonitor database.")} else {
          
       cat("The new user column was not added.")
        }

  } # end of column i

} # end of function
