test_that("error thrown with invalid connection", {

  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection then disconnect
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  DBI::dbDisconnect(con)
  
  expect_error(
    stations <- locationsGetStations(
         amm_fp = demo_fp,
         con,
         noaa_token = "settings",
         startDate = NULL,
         endDate = NULL,
         bbox =  c(43.15, -73, 43.5, -72.6), 
         dbInsert = FALSE,
         disconnect = FALSE)
  )
  
})
