test_that("Error is connections are invalid", {
  
  # Create two empty connections
  master_con <- ""
  dis_con <- ""
  
  # Expect an error with no valid connections
  expect_error(
    dbDistributedImport(
      con = master_con,
      con_distributed = dis_con,
      mode = "verify", 
      disconnect = FALSE
    )
  )
  
})

test_that("Successfully imports all annotations", {
  
  # get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  # set the connection
  conx <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
  
  # Update the audio_path and photo_path settings appropriately
  writeLines(
    text = file.path(demo_fp, "photos"), 
    con = file(paste0(demo_fp, "/settings/image_path.txt"))
  )
  writeLines(
    text = file.path(demo_fp, "recordings"), 
    con = file(paste0(demo_fp, "/settings/audio_path.txt"))
  )
  
  # Create the distributed database
  distributed_fp <- dbDistributed(
    con = conx,
    out_filepath = tempdir(),
    person_id = "gandalf",
    media_ids = 1:15,
    mode = "annotate",
    use_renv = FALSE,
    disconnect = FALSE
  )
  
  # Connect to the distributed database
  cony <- dbSetCon(file.path(distributed_fp, "/database/demo.sqlite"))
  
  # Create a few mediatags to append to the database
  mediatags <- data.frame(
    fk_mediaid = c(2, 3),
    fk_medialistitemid = c(1, 2),
    fk_personid = rep("gandalf", 2),
    x_max = rep(NA, 2),
    x_min = rep(NA, 2),
    y_max = rep(NA, 2),
    y_min = rep(NA, 2),
    value_num = rep(NA, 2),
    notes = rep(NA, 2),
    timestamp = rep(strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), 2)
  )
  
  # Append to the distributed database
  DBI::dbAppendTable(
    conn = cony,
    name = "mediatags",
    value = mediatags
  )
  
  # Create an annotation to append to the database
  annotations <- data.frame(
    fk_personid = "gandalf",
    fk_mediaid = 12,
    fk_searchlistid = NA,
    fk_taxonid = "hare",
    x_max = NA,
    x_min = NA,
    y_max = NA,
    y_min = NA,
    notes = NA,
    timestamp = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # Append to the distributed database
  DBI::dbAppendTable(
    conn = cony,
    name = "annotations",
    value = annotations
  )
  
  # Add an annotag
  annotags <- data.frame(
    fk_annotationid = 1,
    fk_librarylistitemid = 33,
    value_num = NA
  )
  
  # Append to the distributed database
  DBI::dbAppendTable(
    conn = cony,
    name = "annotags",
    value = annotags
  )
  
  # Read in the number of annotations, annotags, and mediatags prior to import
  old_mediatags <- nrow(DBI::dbReadTable(conx, "mediatags"))
  old_annotations <- nrow(DBI::dbReadTable(conx, "annotations"))
  old_annotags <- nrow(DBI::dbReadTable(conx, "annotags"))
  
  # Expect no error importing the distributed database
  expect_no_error(
    dbDistributedImport(
      con = conx,
      con_distributed = cony,
      mode = "annotate",
      disconnect = FALSE
    )
  )
  
  # Expect that all tables are larger after the additions
  new_mediatags <- nrow(DBI::dbReadTable(conx, "mediatags"))
  new_annotations <- nrow(DBI::dbReadTable(conx, "annotations"))
  new_annotags <- nrow(DBI::dbReadTable(conx, "annotags"))
  
  expect_true(new_mediatags > old_mediatags)
  expect_true(new_annotations > old_annotations)
  expect_true(new_annotags > old_annotags)
  
  # Clean up
  DBI::dbDisconnect(conx)
  unlink(demo_fp, recursive = TRUE)
  unlink(distributed_fp, recursive = TRUE)
  
})

test_that("Successfully imports all verifications", {
  
  # get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  # set the connection
  conx <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
  
  # Update the audio_path and photo_path settings appropriately
  writeLines(
    text = file.path(demo_fp, "photos"), 
    con = file(paste0(demo_fp, "/settings/image_path.txt"))
  )
  writeLines(
    text = file.path(demo_fp, "recordings"), 
    con = file(paste0(demo_fp, "/settings/audio_path.txt"))
  )
  
  # Create the distributed database
  distributed_fp <- dbDistributed(
    con = conx,
    out_filepath = tempdir(),
    person_id = "gandalf",
    media_ids = 1:15,
    mode = "verify",
    use_renv = FALSE,
    disconnect = FALSE
  )
  
  # Connect to the distributed database
  cony <- dbSetCon(file.path(distributed_fp, "/database/demo.sqlite"))
  
  # Add annotation verifications to the distributed database
  annovers <- data.frame(
    is_valid = rep(1, 2),
    fk_personid = rep("gandalf", 2),
    fk_annotationid = c(1, 2),
    timestamp = rep(strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), 2)
  )
  
  # Append to the annotation verifications table
  DBI::dbAppendTable(
    conn = cony,
    name = "annotationverifications",
    value = annovers
  )
  
  # Add annotag verifications
  #annotagvers <- data.frame(
   # fk_annotagid = 1,
   # fk_personid = "gandalf",
   # is_valid = 1,
   # timestamp = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  #)
  
  # Append to the database
  #DBI::dbAppendTable(
   # conn = cony,
   # name = "annotagverifications",
   # value = annotagvers
  #)
  
  # Add modeloutput verifications
  modelvers <- data.frame(
    fk_modeloutputid = 10,
    fk_personid = "gandalf",
    is_valid = 1,
    timestamp = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # Append to the database
  DBI::dbAppendTable(
    conn = cony,
    name = "modelverifications",
    value = modelvers
  )
  
  # Read in the old anno vers, annotag vers, and model vers table lengths
  old_annovers <- nrow(DBI::dbReadTable(conx, "annotationverifications"))
  # old_annotagvers <- nrow(DBI::dbReadTable(conx, "annotagverifications"))
  old_modelvers <- nrow(DBI::dbReadTable(conx, "modelverifications"))
  
  # Expect no error importing
  expect_no_error(
    dbDistributedImport(
      con = conx,
      con_distributed = cony,
      mode = "verify",
      disconnect = FALSE
    )
  )
  
  # Expect longer tables after import
  new_annovers <- nrow(DBI::dbReadTable(conx, "annotationverifications"))
  new_modelvers <- nrow(DBI::dbReadTable(conx, "modelverifications"))
  
  expect_true(new_annovers > old_annovers)
  expect_true(new_modelvers > old_modelvers)
  
  # Clean up
  DBI::dbDisconnect(conx)
  unlink(demo_fp, recursive = TRUE)
  unlink(distributed_fp, recursive = TRUE)
  
})