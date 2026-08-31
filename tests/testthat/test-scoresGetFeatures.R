test_that("Successfully get amplitude matrix, pk_id, value_num, and is_valid status", {

  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # copy over recordings
  dir.create(paste0(tempdir(), "/recordings"))
  
  # recording root path
  recording_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/recordings")
  
  files <- list.files(recording_fp,
                      full.names = TRUE,
                      recursive = TRUE)
  
  file.copy(from = files,
            to = paste0(tempdir(), "/recordings"),
            overwrite = TRUE, recursive = FALSE)

  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # get features
  expect_no_error(
    spectrograms <- scoresGetFeatures(
        con = con, 
        templateName = "oven_ct", 
        recordingRootPath = paste0(tempdir(), "/recordings/"),
        disconnect = FALSE)
  )
  
  # expect no nas in these columns
  expect_true(
    all(
      !is.na(spectrograms$pk_modeloutputid) &
      !is.na(spectrograms$value_num) &
      !is.na(spectrograms$is_valid))
  )
 
  # expect at least 18kcols and no more than 30k.
  expect_true(
    18000 < ncol(spectrograms) & ncol(spectrograms) < 30000
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), '/recordings', recursive = TRUE))

})

test_that("Error if template name not in database", {
  
  # find demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # get features
  expect_error(
    spectrograms <- scoresGetFeatures(
      con = con, 
      templateName = "not_in_db", 
      recordingRootPath = paste0(demo_fp, "/recordings/"),
      disconnect = FALSE)
  )
  
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), '/recordings', recursive = TRUE))
  
})