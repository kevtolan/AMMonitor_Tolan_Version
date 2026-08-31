test_that("Error if database connection isn't valid", {
  
  # Create an empty connection object
  con <- ""
  
  # Expect error from bad database connection
  expect_error(
    modelConfigWriteXL(
      con = con,
      modelid = 6,
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
    modelConfigWriteXL(
      con = con,
      modelid = 6,
      output_dir = tempdir(),
      disconnect = FALSE
    )
  )
  
  # Expect the output file to exist
  expect_true(
    file.exists(file.path(tempdir(), "modelAddConfig.xlsx"))
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAddConfig.xlsx"))
  
})