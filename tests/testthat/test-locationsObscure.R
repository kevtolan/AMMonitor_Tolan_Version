test_that("location bounding box coordinates updated in db", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # get original bounding boxes
  original_locations <- DBI::dbReadTable(con, "locations")
  
  # no changes in demo for overwrite = FALSE because only NA is unknown location
  locationsObscure(
       con = con,
       bbLength = 0.02,
       overwrite = FALSE,
       disconnect = FALSE
     )
  
  expect_equal(original_locations, DBI::dbReadTable(con, "locations"))
  
  # expect changes for overwrite = TRUE
  locationsObscure(
    con = con,
    bbLength = 0.02,
    overwrite = TRUE,
    disconnect = FALSE
  )
  
  new_locations <- DBI::dbReadTable(con, 'locations')
  
  # expect changes to bbx coordinates
  expect_false(FALSE %in% (original_locations$long_min != new_locations$long_min))
  expect_false(FALSE %in% (original_locations$long_max != new_locations$long_max))
  expect_false(FALSE %in% (original_locations$lat_min != new_locations$lat_min))
  expect_false(FALSE %in% (original_locations$lat_max != new_locations$lat_max))
  
  
  DBI::dbDisconnect(con)
  unlink(new_demo_fp)
  
})
