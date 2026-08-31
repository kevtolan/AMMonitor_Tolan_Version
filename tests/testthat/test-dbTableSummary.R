test_that("error if valid summary results not passed in", {
  
  results <- c()
  
  expect_error(dbTableSummary(summary_data = results))
  
})

test_that("gt table results returned", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # get summary results
  results <- dbGetSummaryData(con = con)
  
  DBI::dbDisconnect(con)
  
  # run table summary with defaults
  table_results <- dbTableSummary(summary_data = results)
  
  # expect results class list
  expect_equal(class(table_results), 'list')
  
  # expect all items in results are gt table objects
  for (i in 1:length(table_results)) {
    expect_equal(class(table_results[[i]]), c('gt_tbl', 'list'))
  }
  
  
  # try with key_taxa entered
  table_results <- dbTableSummary(summary_data = results,
                                key_species = c('moose', 'black bear'))
  
  # expect results class list
  expect_equal(class(table_results), 'list')
  
  # expect all items in results are gt table objects
  for (i in 1:length(table_results)) {
    expect_equal(class(table_results[[i]]), c('gt_tbl', 'list'))
  }
  
})

test_that("error if invalid key_species given", {

  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # get summary results
  results <- dbGetSummaryData(con = con)
  
  DBI::dbDisconnect(con)
  
  # give invalid key_species
  expect_error(
    results <- dbTableSummary(
      summary_data = results,
      key_species = c('moos', 'black bear')
    )
  )
  
})


test_that("no error if no annotations or modeloutputs", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set connection
  con <- dbSetCon(new_demo_fp)
  
  # delete annotations table
  stmnt <- "DELETE FROM annotations;"
  DBI::dbExecute(con, stmnt)
  
  # get summary results
  results <- dbGetSummaryData(con = con)
  
  # give invalid key_species
  expect_no_error(
    results <- dbTableSummary(
      summary_data = results
    )
  )
  
  
  # delete modeloutputs table
  stmnt <- "DELETE FROM modeloutputs;"
  dbExecute(con, stmnt)
  
  # get summary results
  results <- dbGetSummaryData(con = con)
  
  # give invalid key_species
  expect_no_error(
    results <- dbTableSummary(
      summary_data = results
    )
  )
  
  DBI::dbDisconnect(con)
  
  # clear database from tempdir
  unlink(paste0(tempdir(), '/demo.sqlite')) 
  
})
