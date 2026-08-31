test_that("error if open connection not passed", {
  
  # create empty con object
  con <- ""
  
  # expect error
  expect_error(dbAddUserTable(con = con))
  
})

test_that("error if col_info contains incorrect info", {
  

  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # set wrong table_info 
  new_table <-  data.frame(
    pk_fielname = c("pk_grantid", "grantor", "award_date", "value", "notes"),
    var_type = c("INTEGER", "VARCHAR(255)", "VARCHAR(255)", "REAL", "TEXT"),
    not_null_clause = NA,
    pk = c(1, 0, 0, 0, 0),
    foreign_key_table = NA,
    foreign_key_field = NA,
    on_update = NA,
    on_delete = NA,
    shiny_placeholder = NA,
    shiny_input = c("locked", "text", "date", "numeric", "longtext"),
    default_value = NA,
    fk_listid = NA,
    sb_include = 0,
    min = NA,
    max = NA,
    description = c(
      "Grant ID (auto-number)",
      "Granting agency (i.e., NSF, NIH, etc.)",
      "Date the grant was awarded",
      "Value of grant (in dollars)",
      "Notes about the grant"
    ),
    sort_order = 1:5
  )
  
  # expect error
  expect_error(dbAddUserTable(con = con,
               table_info = new_table))
  
  # disconnect
  DBI::dbDisconnect(con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("user table added via excel", {
  
 
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir(), overwrite = TRUE)
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # set excel filepath to template with example
  fp <- system.file("extdata/dbUserTable.xlsx", package = "AMMonitor")
  
  # add user col to demo database
  dbAddUserTable(
    con,
    table_name = "vegsamples",
    excel_fp = fp,
    primary_tab = "Location Info",
    disconnect = FALSE
  )
  
  # read in dictionary
  dictionary <- DBI::dbReadTable(conn = con,
                                 name = 'dbdictionary')
  
  # expect table in pk_tablename
  expect_true("vegsamples" %in% dictionary$pk_tablename)
  
  # expect leaf_phenology in fk_listid
  expect_true('leaf_phenology' %in% dictionary$fk_listid)
  
  # expect all columns in new table have core field equal to 0
  expect_true(all(
    dictionary$core[which(dictionary$tablename == 'vegsamples')] == 0
    ))
  
  # expect shinytable updated
  shinytable <-  DBI::dbReadTable(conn = con,
                                  name = 'shinytable')
  expect_true('vegsamples' %in% shinytable$fk_tablename)
  
  # read in lists
  lists <- DBI::dbReadTable(conn = con,
                            name = 'lists')
  
  # expect leaf_phenology in pk_listid
  expect_true('leaf_phenology' %in% lists$pk_listid)
  
  # read in listitems
  listitems <-  DBI::dbReadTable(conn = con,
                                 name = 'listitems')
  
  # expect 4 entries for list_card_size
  num_new_listitems <- length(
    which(listitems$fk_listid == 'leaf_phenology'))
  
  expect_equal(num_new_listitems, 4)
  
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("user table added via table_info argument", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # new table info
  new_table <-  data.frame(
    pk_fieldname = c("pk_grantid", "grantor", "award_date", "value", "notes"),
    var_type = c("INTEGER", "VARCHAR(255)", "VARCHAR(255)", "REAL", "TEXT"),
    not_null_clause = NA,
    pk = c(1, 0, 0, 0, 0),
    foreign_key_table = NA,
    foreign_key_field = NA,
    on_update = NA,
    on_delete = NA,
    shiny_placeholder = NA,
    shiny_input = c("locked", "text", "date", "numeric", "longtext"),
    default_value = NA,
    fk_listid = NA,
    sb_include = 0,
    min = NA,
    max = NA,
    description = c(
      "Grant ID (auto-number)",
      "Granting agency (i.e., NSF, NIH, etc.)",
      "Date the grant was awarded",
      "Value of grant (in dollars)",
      "Notes about the grant"
    ),
    sort_order = 1:5
  )
  
  # add user table to demo database
  AMMonitor::dbAddUserTable(
    con,
    table_name = "grants",
    table_info = new_table,
    primary_tab = "Program Mgt",
    disconnect = FALSE
  )
  
  # read in dictionary
  dictionary <- DBI::dbReadTable(conn = con,
                                 name = 'dbdictionary')
  
  # expect grants in pk_tablename
  expect_true('grants' %in% dictionary$pk_tablename)
  
  # expect core field is 0
  expect_true(all(
    dictionary$core[
      which(dictionary$pk_tablename == 'grants')] == 0
    ))
  
  # expect shinytable updated
  shinytable <-  DBI::dbReadTable(conn = con,
                                  name = 'shinytable')
  expect_true('grants' %in% shinytable$fk_tablename)
  
  # disconnect from database
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

