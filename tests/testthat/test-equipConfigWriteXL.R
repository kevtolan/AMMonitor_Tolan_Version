test_that("Error if database connection isn't valid", {
  
  # Create an empty con object
  con <- ""
  
  # Try to write a spreadsheet with a bad connection
  expect_error(
    equipConfigWriteXL(
      con = con,
      equipmodelid = "recorder_model_x",
      output_dir = tempdir(),
      disconnect = FALSE
    )
  )
  
})

test_that("Excel sheet is successfully created", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Attempt to write to the tempdir()
  expect_no_error(
    equipConfigWriteXL(
      con = con,
      equipmodelid = "recorder_model_x",
      output_dir = tempdir(),
      disconnect = FALSE
    )
  )
  
  # Expect the output file to exist
  expect_true(
    file.exists(file.path(tempdir(), "equipAddConfig.xlsx"))
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipAddConfig.xlsx"))
  
})