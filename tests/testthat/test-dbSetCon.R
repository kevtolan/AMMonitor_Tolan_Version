test_that("error thrown if empty filepath", {
  
  # set empty filepath
  fp <- ""
  
  # check error thrown if no path to database provided
  expect_error(dbSetCon(fp))
  
})

test_that("creates SQLITE connection", {
  
  # copy the demo database
  fp <- paste0(find.package("AMMonitor",
                            lib.loc = .libPaths()),
               "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(fp)
  
  # expect class is class SQlite con
  expect_s4_class(con, class = "SQLiteConnection")
  
})