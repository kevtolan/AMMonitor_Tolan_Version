#' @name dbAddUserTable
#' @aliases dbAddUserTable
#' @title Add a user table to an AMMonitor database
#' @description Add a unique user-defined table to an AMMonitor database and 
#' update the dbdictionary and shinytables table to reflect changes in the 
#' database schema. 
#' @param con An open connection to an AMMonitor database
#' @param table_name The name of the new table to be added
#' @param table_info A dataframe with new table information. Use this option only
#' if your new table does not contain columns that reference lists. If 
#' your table does contain a column referencing a list, enter the information 
#' based on the Excel template (See Details).
#' @param excel_fp  Filepath to an Excel file with the new table information
#' @param primary_tab Primary tab under which the table should appear in shiny
#' @param subtable Table under which records should appear as a subtable in shiny
#' @param disconnect TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @family database
#' @usage dbAddUserTable(con, table_name, table_info = NULL, 
#' excel_fp = NA, primary_tab = NA, subtable = NA, disconnect = FALSE)
#' @seealso \code{\link{dbCreate}}, \code{\link{dbAddUserCol}}
#' @importFrom DBI dbIsValid dbAppendTable dbDisconnect dbListTables dbSendQuery 
#' dbClearResult dbGetQuery
#' @importFrom readxl read_excel
#' @export
#' @return Returns 1 if successful
#' @details Adding new tables to the AMMonitor database requires a detailed 
#' knowledge of database structure and design. Any added user tables will be 
#' supported by the Shiny app's database front-end. New tables can be specified 
#' by completing the template Excel file that comes with AMMonitor and is named
#' "dbUserTable.xlsx".  This template can be copied to your working directory 
#' with the following code:
#' 
#'  \code{fp <- system.file("extdata/dbUserTable.xlsx", package = "AMMonitor")}
#'  
#'  \code{file.copy(from = fp, to = getwd(), overwrite = FALSE)}
#'  
#'  From there, open the Excel file and fill it in. Note that the fields here come 
#'  from the dbdictionary table, and review of both the "database" and "dbdictionary"
#'  tutorials is recommended before adding a user table. 
#'  
#'  The template contains a sample user table named "vegsamples" as an example, 
#'  which is a table for storing vegetation sampling results for a given location.
#'  
#'  Once the Excel file is filled out with new table information, point to this 
#'  new file to run the dbAddUserTable() function. 
#'  
#'  The Excel file has three sheets:
#'  
#'  Sheet 1: dbUserTableInfo:
#'   Columns in this sheet are defined in the dbdictionary table and can be returned  
#'   with the following query after a connection to the database is made:
#'   
#'  \code{DBI::dbListFields(conx, table = "dbdictionary")}
#'
#' Sheet 2: lists:  
#'  If the new database user table will contain a column with entries limited 
#' to a list, create the list name on this sheet.  Use lower case for list 
#' names.
#' 
#' Sheet 3: listitems: 
#'  If the new database user table will contain a column with entries limited 
#' to a list, create the acceptable list entries on this sheet. You must identify
#' the list name (fk_listid), the item, and a description of the item.
#' 
#' Table info can be passed to the \code{dbAddUserTable()} function
#' excel template or as a data.frame. Additionally, the function will accept a
#' "primary_tab" argument that defines which tab the table will appear under in 
#' the AMMonitor Shiny app's database front-end, as well as a subtable to include 
#' the new user table under, if applicable.
#' 
#' See the "database" and "dbdictionary" learnr tutorials for more details on the
#' database. The tutorials can be launched with:
#' 
#' \code{learnr::run_tutorial(name = "database", package = "AMMonitor")}.
#' \code{learnr::run_tutorial(name = "dbdictionary", package = "AMMonitor")}.
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
#' conx <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
#'
#' # look at the tables in the demo database (these are AMMonitor core-tables)
#' DBI::dbListTables(conx)
#' 
#' # New table name
#' table_name <- "grants"
#' 
#' # Set the primary tab under which the new table will appear in the Shiny app
#' primary_tab <- "Program Mgt"
#' 
#' # Create the new "grants" table; does not have any columns with dropdowns
#' new_table <-  data.frame(
#'   pk_fieldname = c("pk_grantid", "grantor", "award_date", "total_award", "notes"),
#'   var_type = c("INTEGER", "VARCHAR(255)", "VARCHAR(255)", "REAL", "TEXT"),
#'   not_null_clause = NA,
#'   pk = c(1, 0, 0, 0, 0),
#'   foreign_key_table = NA,
#'   foreign_key_field = NA,
#'   on_update = NA,
#'   on_delete = NA,
#'   shiny_placeholder = NA,
#'   shiny_input = c("locked", "text", "date", "numeric", "longtext"),
#'   default_value = NA,
#'   fk_listid = NA,
#'   sb_include = 0,
#'   min = NA,
#'   max = NA,
#'   description = c(
#'     "Grant ID (auto-number)",
#'     "Granting agency (i.e., NSF, NIH, etc.)",
#'     "Date the grant was awarded",
#'     "Value of grant (in dollars)",
#'     "Notes about the grant"
#'     ),
#'  sort_order = 1:5
#'  )
#'  
#' # add the new table to the database
#' AMMonitor::dbAddUserTable(
#'  con = conx, 
#'  table_name = table_name,
#'  table_info = new_table,
#'  primary_tab = primary_tab,
#'  disconnect = FALSE
#'  )
#'  
#'  # confirm the new table is added
#'  DBI::dbListTables(conx)
#'  DBI::dbListFields(conx, name = "grants")
#'  
#'   
#'  # check the dbdictionary table for the new table
#'  DBI::dbGetQuery(
#'   con = conx,
#'   statement = "SELECT * from dbdictionary WHERE pk_tablename = 'grants';")
#'   
#'  # double check using SQLite table information
#'  DBI::dbGetQuery(
#'   con = conx,
#'   statement = "PRAGMA table_info(grants)"
#'   )
#'  
#'  # add a record to the new table
#'  DBI::dbAppendTable(conx,
#'     name = "grants",
#'     value = data.frame(
#'       pk_grantid = 1,
#'       grantor = "Agency XYZ",
#'       award_date = "2022-01-01",
#'       total_award = 10000,
#'       notes = "My award notes.")
#'   )
#'   
#'  # confirm the record
#'  DBI::dbReadTable(conx, name = "grants")
#'  
#'  # look at the table through the shiny app (click on database tab)
#'  launchApp(demo_fp)
#' 
#' # ------------------------------------------------------------
#' # if user tables include columns with a dropdown, use the Excel template.
#' # Add the example user table that comes with the AMMonitor package
#' # Here, we will be adding a user table named "vegsamples" to
#' # the database, containing a foreign key reference to the "locations" table
#' # and several columns with possible values restricted to a list.
#' # ------------------------------------------------------------
#' 
#' 
#' # Set the primary tab under which the new table will appear in the Shiny app
#' primary_tab <- "Location Info"
#' 
#' # set the filepath to the Excel file that has the required information
#' fp <- system.file("extdata/dbUserTable.xlsx", package = "AMMonitor")
#' 
#' # glimpse (but don't load) at sheets in the dbUserTable spreadsheet 
#' readxl::read_excel(
#'  path = fp,
#'  sheet = "dbUserTableInfo", 
#'  col_types = c(
#'  "text", "text", "text", "numeric", "text", "text", "text", "text", "text", 
#'  "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")
#'  )
#'  
#' # glimpse at the lists
#' readxl::read_excel(path= fp, sheet = "lists")
#'   
#' # glimpse at the listitems
#' readxl::read_excel(path = fp, sheet = "listitems")
#'
#' # add the template user table to the database
#' dbAddUserTable(
#'   con = conx,
#'   table_name = "vegsamples",
#'   excel_fp = fp,
#'   primary_tab = primary_tab,
#'   disconnect = FALSE)
#'
#' # list the fields in the new tables and note the new user columns
#' DBI::dbListFields(conx, name = "vegsamples")
#'
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' 
#' 
#'}

dbAddUserTable <- function(con, table_name, table_info = NULL, excel_fp = NA, primary_tab = NA, subtable = NA, disconnect = FALSE) {
  
  # table_name <- "vegsamples"
  # excel_fp <- "C:/ammonitor/user_table.xlsx"
  
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    }) 
  }
  
  # check that either table_info or excel_fp is NA; not both
  if (!xor(is.null(table_info), is.na(excel_fp))) {
    stop("Either table_info or excel_fp must have non-NA values.")
  }
  
  # Make sure the table doesn't already exist
  if (table_name %in% DBI::dbListTables(con)) {
    stop(paste("Table", table_name, "already exists."))
  }

  # if new info is in spreadsheet -------------------
  
  if (!is.na(excel_fp)) {
    
    lists <- readxl::read_excel(excel_fp, sheet = "lists")
    lists$core_list <- 0

    DBI::dbAppendTable(conn = con, name = "lists", value = lists)

    # append new listitems ------------------------
    listitems <- readxl::read_excel(excel_fp, sheet = "listitems")
    DBI::dbAppendTable(conn = con, name = "listitems", value = listitems)
    
    # get table metadata  -----------------------
    table_info <- readxl::read_excel(
      path = excel_fp,
      sheet = "dbUserTableInfo", 
      col_types = c(
        "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")
    )
  } # end of excel file
  
  # Make sure a primary key has been specified
  if (!any(table_info$pk == 1)) {
    stop("You must specify at least one primary key.")
  }
  
  # order by sort order
  indices <- order(table_info$sort_order)
  table_info <- table_info[indices,]
  
  # get the foreign keys
  fk_info <- subset(table_info, !is.na(table_info$foreign_key_table) & table_info$foreign_key_table != "")
  
  # Logicals indicating whether the table has a foreign key or compound primary key
  has_compound_pk <- sum(table_info$pk != 0) > 1
  has_fk <- nrow(fk_info) != 0
  
  # Begin the query statement (stmnt will be concatenated to to build the query)
  stmnt <- paste("CREATE TABLE", table_name, "(\n")
  
  # Loop through each field in the table
  for (j in 1:nrow(table_info)) {
    
    # Add Field name and data type
    stmnt <- paste(
      stmnt,
      table_info$pk_fieldname[j],
      table_info$var_type[j]
    )
    
    # Add NOT NULL clause
    if (!is.na(table_info$not_null_clause[j])) {
      stmnt <- paste(stmnt, table_info$not_null_clause[j])
    }
    
    # Add default value
    if (!is.na(table_info$default_value[j])) {
      stmnt <- paste(stmnt, "DEFAULT", table_info$default_value[j])
    }
    
    # add primary key if not compound 
    if (!has_compound_pk && table_info[j, "pk"] == TRUE) {
      stmnt <- paste(stmnt, "PRIMARY KEY")
    }
    
    if (j != nrow(table_info) || has_compound_pk) {
      stmnt <- paste0(stmnt, ",")
    }
    
    # Add the field description (as a comment)
    if (!is.na(table_info$description[j])) {
      stmnt <- paste(stmnt, "--", table_info$description[j], "\n")
    }
  }
  
  # Add compound primary key
  if (has_compound_pk) {
    stmnt <- paste(
      stmnt,
      "PRIMARY KEY (",
      paste(
        table_info$pk_fieldname[table_info$pk != 0],
        collapse = ", "
      ),
      ")"
    )
  }
  
  # Add foreign keys
  if (has_fk) {
    stmnt <- paste0(stmnt, ",\n")
    for (j in 1:nrow(fk_info)) {
      stmnt <- paste0(
        stmnt,
        " FOREIGN KEY (",
        fk_info$pk_fieldname[j],
        ") REFERENCES ",
        fk_info$foreign_key_table[j], "(",
        fk_info$foreign_key_field[j],
        ") ON UPDATE ",
        fk_info$on_update[j],
        " ON DELETE ",
        fk_info$on_delete[j]
      )
    }
  }
  
  # Close off the query statement
  stmnt <- paste0(stmnt, "\n)")

  # execute the query
  rs <- tryCatch(
    {DBI::dbSendQuery(
      conn = con,
      statement = stmnt
    )},
    error = function(cond) {
      stop(paste("Unable to create table:", cond))
    },
    warning = function(cond) {
      stop(paste("Unable to create table:", cond))
    }
  )
  
  # clear the result
  DBI::dbClearResult(rs)
  
  # Add new table to the tables list
  DBI::dbAppendTable(
    con,
    "listitems",
    data.frame(
      fk_listid = "tables_list",
      item = table_name,
      description = paste(
        "The",
        table_name,
        "table in the database. See the dictionary table for a full description of this table's structure."
      )
    )
  )
  
  # Add records the "dbdictionary" table for each new column
  DBI::dbAppendTable(
    con, 
    'dbdictionary', 
    cbind(
      table_info,
      pk_tablename = table_name,
      core = 0
    )
  )
  
  # Update the shinytable table, if specified
  if (any(!is.na(c(primary_tab, subtable)))) {
    primary_tab_exists <- any(DBI::dbGetQuery(con, paste0("SELECT (1) FROM listitems WHERE fk_listid = 'primary_tab' AND item = '", primary_tab, "';")))
    
    # Add the new primary tab (if needed)
    if (!is.na(primary_tab) && !primary_tab_exists) {
      DBI::dbAppendTable(
        con,
        "listitems",
        data.frame(
          fk_listid = "primary_tab",
          item = primary_tab,
          description = paste(
            "Primary tab name for displaying database tables in Shiny:",
            primary_tab
          ),
          sort_order = DBI::dbGetQuery(con, "SELECT MAX(sort_order)+1 FROM listitems WHERE fk_listid = 'primary_tab';")[,]
        )
      )
    }
    
    # Add the new record to the shinytable table
    DBI::dbAppendTable(
      con,
      "shinytable",
      data.frame(
        primary_tab = primary_tab,
        fk_tablename = table_name,
        sort_order = max(
          1,
          DBI::dbGetQuery(
            con,
            paste0(
              "SELECT MAX(sort_order)+1 FROM shinytable WHERE primary_tab = '",
              primary_tab,
              "';"
            )
          )[,],
          na.rm = TRUE
        ),
        subtable = subtable
      )
    )
  }
  return(1)
} # end of function
