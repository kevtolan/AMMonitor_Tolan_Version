test_that("error if open connection not passed", {
  
  # create empty con object
  con <- ""
  
  # expect error
  expect_error(dbCheckup(con = con))
  
})

test_that("correct output for demo w/o errors", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # run dbCheckup
  results <- dbCheckup(con = con, disconnect = TRUE)
  
  # expect list of 4 elements
  expect_equal(length(results), 4)
  
  # expect these names
  expected_names <- c("violations",
                      "tables",
                      "foreign_keys",
                      "description")
  
  expect_equal(names(results), expected_names)
  
  
  # expect empty results dataframes, except description
  expect_equal(nrow(results[1]), NULL)
  expect_equal(nrow(results[2]), NULL)
  expect_equal(nrow(results[3]), NULL)
  
  expect_match(results$description, "Congratulations")
  
})

test_that("correct outputs for demo w/ errors", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # introduce column mismatch
   DBI::dbExecute(
    con, 
    statement = "UPDATE dbdictionary SET pk_fieldname = 'primary' 
      WHERE pk_fieldname = 'primary_account';"
    ) 
  
  # run dbCheckup
  results <- dbCheckup(con = con, disconnect = TRUE) 
  
  # expect column mismatch error
  expect_equal(nrow(results$tables), 1)
  
  tables <- results$tables
  expect_equal(tables$dict_tablename, 'accounts')
  expect_equal(tables$db_fieldname, 'primary')
  
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})