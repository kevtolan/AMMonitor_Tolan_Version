test_that("summary data results returned with defaults", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # get summary data and disconnect from database
  expect_no_error(
    results <- dbGetSummaryData(con = con, disconnect = TRUE)
  )
  
  # check results
  expect_equal(class(results), 'list')
  
  expect_true(length(results) >= 10)
  
  # check no results items are empty
  for (i in 1:length(results)) {
    expect_false(is.null(results[i]))
  }
  
  
})

test_that("summary data results returned with date range", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # get summary data and disconnect from database
  expect_no_error(
  results <- dbGetSummaryData(con = con,
                              m_dates = c("2023-05-01", "2023-12-31"),
                              disconnect = TRUE)
  )
  
  # check results
  expect_equal(class(results), 'list')
  
  expect_true(length(results) >= 10)
  
  # check no results items are empty
  for (i in 1:length(results)) {
    expect_false(is.null(results[i]))
  }
  
})

test_that("error with invalid dates", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # check error if invalid dates
  expect_error(
   results <- dbGetSummaryData(con = con,
                     m_dates = c("2023-05-01", "20240101"),
                     disconnect = FALSE)
  )
  
  # dates not in database
  expect_error(
    results <- dbGetSummaryData(con = con,
                                m_dates = c('1990-01-01', '2000-01-01'),
                                disconnect = TRUE)
  )
  
})
