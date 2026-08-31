test_that("Error if database connection isn't valid", {
  
  # Create an empty connection object
  con <- ""
  
  # Check for an error
  expect_error(
    verifyExcelImport(
      con = con,
      excel_file = file.path(tempdir(), "validate_me.xlsx"),
      verified_what = "annotations",
      person_id = "gandalf",
      disconnect = FALSE
    )
  )
  
})

test_that("Error if verified_what is an invalid entry", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a spreadsheet in the tempdir
  verifyExcelCreate(
    con = con,
    media_ids = 1:26,
    verify_what = "annotations",
    media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
    outfile_path = paste0(tempdir(), "/validate_me.xlsx"),
    disconnect = FALSE
  )
  
  # Try to import with both modeloutputs and annotations
  expect_error(
    verifyExcelImport(
      con = con,
      excel_file = paste0(tempdir(), "/validate_me.xlsx"),
      verified_what = c("annotations", "modeloutputs"),
      person_id = "gandalf",
      disconnect = FALSE
    )
  )
  
  # Try to import with an invalid entry
  expect_error(
    verifyExcelImport(
      con = con,
      excel_file = paste0(tempdir(), "/validate_me.xlsx"),
      verified_what = "verifications",
      person_id = "gandalf",
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/validate_me.xlsx"))
  
})

test_that("Error if person_id isn't in the database", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a spreadsheet in the tempdir
  verifyExcelCreate(
    con = con,
    media_ids = 1:26,
    verify_what = "annotations",
    media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
    outfile_path = paste0(tempdir(), "/validate_me.xlsx"),
    disconnect = FALSE
  )
  
  # Expect an error from a missing person id
  expect_error(
    verifyExcelImport(
      con = con,
      excel_file = paste0(tempdir(), "/validate_me.xlsx"),
      verified_what = "annotations",
      person_id = "galadriel",
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/validate_me.xlsx"))
  
})

test_that("Import verifications for annotations",{
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a spreadsheet in the tempdir
  verifyExcelCreate(
    con = con,
    media_ids = 1:26,
    verify_what = "annotations",
    media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
    outfile_path = paste0(tempdir(), "/validate_me.xlsx"),
    disconnect = FALSE
  )
  
  # Read in the excel sheet
  excel <- readxl::read_excel(paste0(tempdir(), "/validate_me.xlsx"))
  
  # Set all of the verified column to verified
  excel$verified <- TRUE
  
  # Write out a new spreadsheet that's verified
  writexl::write_xlsx(
    x = excel,
    path = paste0(tempdir(), "/validated.xlsx")
  )
  
  # Get the current number of annotation verifications
  vers_pre_import <- nrow(DBI::dbReadTable(con, "annotationverifications"))
  
  # Import the verifications
  expect_no_error(
    verifyExcelImport(
      con = con,
      excel_file = paste0(tempdir(), "/validated.xlsx"),
      verified_what = "annotations",
      person_id = "gandalf",
      disconnect = FALSE
    )
  )
  
  # Get the number of verifications now
  vers_post_import <- nrow(DBI::dbReadTable(con, "annotationverifications"))
  
  # Expect more verifications in the table post import
  expect_true(vers_post_import > vers_pre_import)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/validate_me.xlsx"))
  unlink(paste0(tempdir(), "/validated.xlsx"))
  
})

test_that("Import verifications for modeloutputs",{
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a spreadsheet in the tempdir
  verifyExcelCreate(
    con = con,
    media_ids = 1:26,
    verify_what = "modeloutputs",
    media_root_path = "https://cruvt-external-share.s3.amazonaws.com/demo",
    outfile_path = paste0(tempdir(), "/validate_me.xlsx"),
    disconnect = FALSE
  )
  
  # Read in the excel sheet
  excel <- readxl::read_excel(paste0(tempdir(), "/validate_me.xlsx"))
  
  # Set all of the verified column to verified
  excel$verified <- TRUE
  
  # Write out a new spreadsheet that's verified
  writexl::write_xlsx(
    x = excel,
    path = paste0(tempdir(), "/validated.xlsx")
  )
  
  # Get the current number of annotation verifications
  vers_pre_import <- nrow(DBI::dbReadTable(con, "modelverifications"))
  
  # Import the verifications
  expect_no_error(
    verifyExcelImport(
      con = con,
      excel_file = paste0(tempdir(), "/validated.xlsx"),
      verified_what = "modeloutputs",
      person_id = "gandalf",
      disconnect = FALSE
    )
  )
  
  # Get the number of verifications now
  vers_post_import <- nrow(DBI::dbReadTable(con, "modelverifications"))
  
  # Expect more verifications in the table post import
  expect_true(vers_post_import > vers_pre_import)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/validate_me.xlsx"))
  unlink(paste0(tempdir(), "/validated.xlsx"))
  
})