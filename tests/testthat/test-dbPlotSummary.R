test_that("error if valid summary results not passed in", {

  results <- c()
  
  expect_error(dbPlotSummary(summary_data = results))
  
})

test_that("ggplot results returned", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # get summary results
  results <- dbGetSummaryData(con = con)
  
  DBI::dbDisconnect(con)
  
  # run plot summary with defaults
  expect_no_error(
    plot_results <- dbPlotSummary(summary_data = results)
  )
  
  # expect results class list
  expect_equal(class(plot_results), 'list')
  
  # expect all items in results are ggplot objects
  for (i in 1:length(plot_results)) {
    expect_equal(class(plot_results[[i]]), c('gg', 'ggplot'))
  }
  
  
  # try with key_taxa entered
  plot_results <- dbPlotSummary(
    summary_data = results,
    key_species = c('moose', 'black bear')
  )
  
  # expect results class list
  expect_equal(class(plot_results), 'list')
  
  # expect all items in results are ggplot objects
  for (i in 1:length(plot_results)) {
    expect_equal(class(plot_results[[i]]), c('gg', 'ggplot'))
  }
  
  
  # try with map data entered
  plot_results <- dbPlotSummary(
    summary_data = results,
    map_database = "state", 
    regions = "vermont")
  
  # expect results class list
  expect_equal(class(plot_results), 'list')
  
  # expect all items in results are ggplot objects
  for (i in 1:length(plot_results)) {
    expect_equal(class(plot_results[[i]]), c('gg', 'ggplot'))
  }
  
  
})

test_that("error if invalid arguments given", {
  
  # get the demo database
  demo_fp <- paste0(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # get summary results
  results <- dbGetSummaryData(con = con)
  
  DBI::dbDisconnect(con)
  
  # give invalid key_species
  expect_error(
    plot_results <- dbPlotSummary(
      summary_data = results,
      num_bins = 15,
      map_database = "state", 
      regions = "vermont",
      key_species = c("moos", "black bear")
    )
  )
  
  # give invalid mapping data
  expect_error(
    plot_results <- dbPlotSummary(
      summary_data = results,
      num_bins = 15,
      map_database = "state", 
      regions = "vt",
      key_species = c("moose", "black bear")
    )
  )
  
  # give invalid num_bins
  expect_error(
    plot_results <- dbPlotSummary(
      summary_data = results,
      num_bins = "yearly",
      map_database = "state", 
      regions = "vermont",
      key_species = c("moose", "black bear")
    )
  )

})
