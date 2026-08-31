test_that("Error if database connection is not valid", {
  
  # Create an empty con object
  con <- ""
  
  expect_error(
    verifyExcelCreate(
      con = con,
      media_ids = 1:26,
      verify_what = "annotations",
      media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
      outfile_path = file.path(tempdir(), "validate_me.xlsx"),
      disconnect = FALSE
    )
  )
  
})

test_that("Error if verify_what is an invalid entry", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Run the function with both annotations and modeloutputs
  expect_error(
    verifyExcelCreate(
      con = con,
      media_ids = 1:26,
      verify_what = c("annotations", "modeloutputs"),
      media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
      outfile_path = paste0(tempdir(), "/validate_me.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Run the function with an incorrect verify_what string
  expect_error(
    verifyExcelCreate(
      con = con,
      media_ids = 1:26,
      verify_what = "verifications",
      media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
      outfile_path = paste0(tempdir(), "/validate_me.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("Spreadsheet created for annotations", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Get the media IDs
  media_ids <- DBI::dbGetQuery(
    conn = con,
    statement = "SELECT pk_mediaid FROM media;"
  )[,,drop = TRUE]
  
  # Set the output filepath
  outfile_path <- paste0(tempdir(), "/validate_me.xlsx")
  
  # Run the function
  expect_no_error(
    verifyExcelCreate(
      con = con,
      media_ids = media_ids,
      verify_what = "annotations",
      media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
      outfile_path = outfile_path,
      disconnect = FALSE
    )
  )
  
  # Expect the file to exist
  expect_true(file.exists(outfile_path))
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(outfile_path)
  
})

test_that("Spreadsheet created for modeloutputs", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Get the media IDs
  media_ids <- DBI::dbGetQuery(
    conn = con,
    statement = "SELECT pk_mediaid FROM media;"
  )[,,drop = TRUE]
  
  # Set the output filepath
  outfile_path <- paste0(tempdir(), "/validate_me.xlsx")
  
  # Run the function
  expect_no_error(
    verifyExcelCreate(
      con = con,
      media_ids = media_ids,
      verify_what = "modeloutputs",
      media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
      outfile_path = outfile_path,
      disconnect = FALSE
    )
  )
  
  # Expect the file to exist
  expect_true(file.exists(outfile_path))
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(outfile_path)
  
})