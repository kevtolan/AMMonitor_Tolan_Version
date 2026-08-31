test_that("error thrown if invalid connection", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection then disconnect
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  DBI::dbDisconnect(con)
  
  expect_error(
    temporal_data <- temporalsGet(
         amm_fp  = demo_fp,
         con = con, 
         startDate = "2023-01-01",
         endDate = "2023-12-31",
         token = "settings",
         temporalSource = "noaa",
         stationCodes = stations$pk_locationid,
         type = "historical",
         keys = c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"),
         dbInsert = FALSE,
         disconnect = FALSE
        )
  ) 
  

})
