test_that("error if open connection not passed", {
  
  # create empty con object
  con <- ""
  
  # expect error
  expect_error(dbAddUserCol(con = con))
  
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
  
  # set wrong col_info 
  new_column <-  data.frame(
    pk_tablename = 'visits',
    fielname = 'field',
    core = 0,
    var_type = "REAL",
    not_null_clause = NA_character_,
    default_value = NA_character_,
    max = NA,
    min = NA,
    fk_listid = NA_character_,
    description = "Air temperature at time of visit")
  
  
  # expect error
  expect_error(dbAddUserCol(con = con,
               col_info = new_column))
  
  
  # disconnect
  DBI::dbDisconnect(con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("user col added via excel", {
  
 
  
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
  fp <- system.file("extdata/dbUserCol.xlsx", package = "AMMonitor")
  
  # add user col to demo database
  dbAddUserCol(con,
               col_info = NULL,
               excel_fp = fp,
               disconnect = FALSE)
  
  # read in dictionary
  dictionary <- DBI::dbReadTable(conn = con,
                                 name = 'dbdictionary')
  
  # expect card_size in pk_fieldname
  expect_true('card_size' %in% dictionary$pk_fieldname)
  
  # expect list_card_size in fk_listid
  expect_true('list_card_size' %in% dictionary$fk_listid)
  
  # expect core field is 0
  expect_equal(
    dictionary$core[which(dictionary$pk_fieldname == 'card_size')],
    0)
  
  # read in lists
  lists <- DBI::dbReadTable(conn = con,
                            name = 'lists')
  
  # expect list_card_size in pk_listid
  expect_true('list_card_size' %in% lists$pk_listid)
  
  # read in listitems
  listitems <-  DBI::dbReadTable(conn = con,
                                 name = 'listitems')
  
  # expect 6 entries for list_card_size
  num_new_listitems <- length(
    which(listitems$fk_listid == 'list_card_size'))
  
  expect_equal(num_new_listitems, 6)
  
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("user col added via col_info argument", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # new col info
  new_column <-  data.frame(
    pk_tablename = "visits",
    pk_fieldname = "new_field",
    core = 0,
    var_type = "REAL",
    not_null_clause = NA_character_,
    default_value = NA_character_,
    max = NA,
    min = NA,
    fk_listid = NA_character_,
    description = "Air temperature at time of visit")
  
  # add user col to demo database
  dbAddUserCol(con,
               col_info = new_column,
               excel_fp = NA,
               disconnect = FALSE)
  
  # read in dictionary
  dictionary <- DBI::dbReadTable(conn = con,
                                 name = 'dbdictionary')
  
  # expect card_size in pk_fieldname
  expect_true('new_field' %in% dictionary$pk_fieldname)
  
  # expect core field is 0
  expect_equal(
    dictionary$core[
      which(dictionary$pk_fieldname == 'new_field')],
    0)
  
  # disconnect from database
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

