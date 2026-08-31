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
  
  # Add some verifications to the audio file as an example
  # Model output verifications
  DBI::dbAppendTable(
    conn = con,
    name = "modelverifications",
    value = data.frame(
      pk_modelverificationid = NA,
      fk_modeloutputid = 56:58,
      fk_personid = "gandalf",
      is_valid = 1,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  
  # Annotation Verifications
  DBI::dbAppendTable(
    conn = con,
    name = "annotationverifications",
    value = data.frame(
      pk_annoverificationid = NA,
      is_valid = 1,
      fk_personid = "gandalf",
      fk_annotationid = 28:29,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  
  # remove the merged media file and move verifications to the original media
  mergedAudioRemove(
    con = con,
    mediaID = 27,
    ammPath = demo_fp,
    disconnect = FALSE
  )
  
  # Make sure the merged audio's visit was removed from the visits table
  expect_equal(
    DBI::dbGetQuery(
      con, 
      "SELECT COUNT(*) FROM visits WHERE visit_type = 'merged-verification';"
    )[,],
    0
  )
  
  # Make sure the merged audio file was removed from the recordings directory
  expect_equal(
    length(list.files(
      file.path(tempdir(), "demoAMM/recordings"), 
      pattern = "merged_validation"
    )),
    0
  )
  
  # Make sure verifications were correctly transfered to the originating 
  # annotations and modeloutputs.
  
  anno_verifications <- dbReadTable(con, "annotationverifications")
  model_verifications <- dbReadTable(con, "modelverifications")
  
  expect_true(
    all(
      modeloutput_ids %in% model_verifications$fk_modeloutputid[
        model_verifications$fk_personid == "gandalf"
      ]
    )
  )
  
  expect_true(
    all(
      annotation_ids %in% anno_verifications$fk_annotationid[
        anno_verifications$fk_personid == "gandalf"
      ]
    )
  )
  
  # Disconnect and clean up
  DBI::dbDisconnect(con)
  unlink(paste0(tempdir(), '/demoAMM'), recursive = TRUE)
  
})