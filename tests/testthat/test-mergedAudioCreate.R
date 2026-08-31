test_that("Merged audio file creation successful", {
  # get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  new_demo_fp <- paste0(tempdir(), '/demoAMM/database/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Define input parameters
  modeloutput_ids <- 50:52
  annotation_ids <- 26:27
  
  # Add a merged audio file to the database with the specified
  # annotations and model outputs
  mergedAudioCreate(
    con = con,
    annotation_ids = annotation_ids,
    modeloutput_ids = modeloutput_ids,
    ammPath = demo_fp
  )
  
  # Get the visit for the merged audio
  new_visit <- DBI::dbGetQuery(
    con,
    "SELECT * FROM visits WHERE visit_type = 'merged-verification';"
  )
  
  # Make sure the visit was added to the visits table
  expect_true(nrow(new_visit) == 1)
  
  merged_audio <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT * FROM media WHERE fk_visitid = ",
      new_visit$pk_visitid,
      ";"
    )
  )
  
  # Make sure the merged audio was added to the media table
  expect_true(nrow(merged_audio) == 1)
  
  # Make sure the merged audio file exists
  expect_true(file.exists(merged_audio$filepath))
  
  # Make sure the merged audio has two annotations and three model outputs
  expect_equal(
    object = DBI::dbGetQuery(
      con,
      paste(
        "SELECT COUNT(*) FROM annotations WHERE fk_mediaid = ",
        merged_audio$pk_mediaid,
        ";"
      )
    )[,],
    expected = 2
  )
  
  expect_equal(
    object = DBI::dbGetQuery(
      con,
      paste(
        "SELECT COUNT(*) FROM modeloutputs WHERE fk_mediaid = ",
        merged_audio$pk_mediaid,
        ";"
      )
    )[,],
    expected = 3
  )
  
  # Disconnect and clean up
  DBI::dbDisconnect(con)
  unlink(paste0(tempdir(), '/demoAMM'), recursive = TRUE)
  
})
