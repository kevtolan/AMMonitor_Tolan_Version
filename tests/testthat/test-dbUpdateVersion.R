test_that("database version updated successfully", {
  
  # Testing incompatible conversion ----------
  expect_error(
    AMMonitor::dbUpdateVersion(
      db_path = "fake/path", 
      from_version = "-99", 
      to_version = "-99", 
      dest = tempdir()
    )
  )
  
  # Testing 2.1->2.2 conversion --------
  
  # Download database verson 2.1
  db_path_old <- file.path(tempdir(), "demo_21.sqlite")
  download.file("https://code.usgs.gov/vtcfwru/ammonitor/-/raw/AMMonitor2.1/inst/extdata/demoAMM/database/demo.sqlite", db_path_old, mode = "wb", quiet = TRUE)

  # Convert the database
  db_path_new <- AMMonitor::dbUpdateVersion(
    db_path = db_path_old,
    from_version = "2.1",
    to_version = "2.2"
  )
  
  # Make sure database file exists
  expect_true(file.exists(db_path_new))
  
  # Expect can make a valid connection to db
  expect_no_error(con <- dbSetCon(db_path_new))
  
  # Ensure the modellabels table exists
  expect_true("modellabels" %in% DBI::dbListTables(con))
  
  # Check that modellabels is in the dictionary
  expect_true(nrow(DBI::dbGetQuery(con, "SELECT * FROM shinytable WHERE fk_tablename = 'modellabels';")) == 1)
  
  # Check that modellabels is in the shinytable
  expect_true(nrow(DBI::dbGetQuery(con, "SELECT * FROM dbdictionary WHERE pk_tablename = 'modellabels';")) == 6)
  
  # Are all non-integer primary keys NOT NULL?
  expect_true(all(DBI::dbGetQuery(con, "SELECT not_null_clause FROM dbdictionary WHERE pk = 1 AND var_type <> 'INTEGER';") == "NOT NULL"))
  
  # Is fk_equipmodelid column in the econfignames table?
  expect_true("fk_equipmodelid" %in% DBI::dbListFields(con, "econfignames"))
  
  # Is fk_equipmodelid column in the econfignames table in the dictionary?
  expect_true(nrow(DBI::dbGetQuery(con, "SELECT * FROM dbdictionary WHERE pk_tablename = 'econfignames' AND pk_fieldname = 'fk_equipmodelid';")) == 1)

  # Clean-up
  DBI::dbDisconnect(con)
  unlink(db_path_old)
  unlink(db_path_new)
})
