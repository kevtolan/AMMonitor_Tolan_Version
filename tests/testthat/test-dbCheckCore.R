test_that("error if open connection not passed", {
  
  # create empty con object
  con <- ""
  
  # expect error
  expect_error(dbCheckCore(con = con))
  
})

test_that("correct output for demo w/o errors", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # run dbCheckCore
  results <- dbCheckCore(con = con, disconnect = TRUE)
  
  # expect list of 7 elements
  expect_equal(length(results), 7)
  
  # expect these names
  expected_names <- c("dictionary_check", "listitems_check", 
                      "librarylist_check", "librarylistitems_check",
                      "medialists_check", "medialistitems_check",
                      "description" )
  
  expect_equal(names(results), expected_names)
  
  
  # expect empty results dataframes, except description results[7]
  expect_equal(nrow(results[1]), NULL)
  expect_equal(nrow(results[2]), NULL)
  expect_equal(nrow(results[3]), NULL)
  expect_equal(nrow(results[4]), NULL)
  expect_equal(nrow(results[5]), NULL)
  expect_equal(nrow(results[6]), NULL)
  
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
  
  # create mismatches
  # Adding dictionary mismatch
  DBI::dbExecute(
    con, 
    statement = "UPDATE dbdictionary SET pk_fieldname = 'primary' 
    WHERE pk_fieldname = 'primary_account';"
  )
  
  # Adding librarylist mismatch
  DBI::dbExecute(
    con, 
    statement = "UPDATE librarylists 
    SET pk_librarylistid = 'ageClass' 
    WHERE pk_librarylistid = 'age_class';"
    )
  
  # run dbCheckCore
  results <- dbCheckCore(con = con, disconnect = TRUE)
  
  # check description
  expect_equal(results$description[1], "The database value for the column 'pk_fieldname' in table 'accounts' is 'primary', but in the default dictionary the value is 'primary_account'.")
  
  expect_equal(results$description[2], "The default librarylist 'age_class' is MISSING." )
  
  # check dictionary
  dictionary <- results$dictionary_check
  
  expect_equal(nrow(dictionary), 1)
  expect_equal(dictionary$db_value, 'primary')
  
  # check librarylists
  library_lists <- results$librarylist_check
  
  expect_equal(nrow(library_lists), 1)
  expect_equal(library_lists$default_librarylist, 'age_class')
  
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})