test_that("Error if database connection is invalid", {
  
  # Create an empty con
  con <- ""
  
  # Expect an error when running the function
  expect_error(
    distributed_fp <- dbDistributed(
      con = con,
      out_filepath = tempdir(),
      person_id = "fbaggins",
      media_ids = 1:15,
      mode = "verify",
      use_renv = FALSE,
      disconnect = FALSE
    )
  )
  
})

test_that("Error from unimplemented renv functionality", {
  
  # get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  # set the connection
  con <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
  
  # Update the audio_path and photo_path settings appropriately
  writeLines(
    text = file.path(demo_fp, "photos"), 
    con = file(paste0(demo_fp, "/settings/image_path.txt"))
  )
  writeLines(
    text = file.path(demo_fp, "recordings"), 
    con = file(paste0(demo_fp, "/settings/audio_path.txt"))
  )
  
  # Expect an error if you try to use renv
  expect_error(
    distributed_fp <- dbDistributed(
      con = con,
      out_filepath = tempdir(),
      person_id = "fbaggins",
      media_ids = 1:15,
      mode = "verify",
      use_renv = TRUE,
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(demo_fp, recursive = TRUE)
  
})

test_that("Error for invalid mode", {
  
  # get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  # set the connection
  con <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
  
  # Update the audio_path and photo_path settings appropriately
  writeLines(
    text = file.path(demo_fp, "photos"), 
    con = file(paste0(demo_fp, "/settings/image_path.txt"))
  )
  writeLines(
    text = file.path(demo_fp, "recordings"), 
    con = file(paste0(demo_fp, "/settings/audio_path.txt"))
  )
  
  # Expect an error for an invalid mode
  expect_error(
    distributed_fp <- dbDistributed(
      con = con,
      out_filepath = tempdir(),
      person_id = "fbaggins",
      media_ids = 1:15,
      mode = "model",
      use_renv = FALSE,
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(demo_fp, recursive = TRUE)
  
})

test_that("Error if person_id is not in the database", {
  
  ## get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  # set the connection
  con <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
  
  # Update the audio_path and photo_path settings appropriately
  writeLines(
    text = file.path(demo_fp, "photos"), 
    con = file(paste0(demo_fp, "/settings/image_path.txt"))
  )
  writeLines(
    text = file.path(demo_fp, "recordings"), 
    con = file(paste0(demo_fp, "/settings/audio_path.txt"))
  )
  
  # Expect an error for an invalid person ID
  expect_error(
    distributed_fp <- dbDistributed(
      con = con,
      out_filepath = tempdir(),
      person_id = "pippin",
      media_ids = 1:15,
      mode = "verify",
      use_renv = FALSE,
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(demo_fp, recursive = TRUE)
  
})

test_that("New distributed DB and mini project are created in verify mode", {
  
  # get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  # set the connection
  con <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
  
  # Update the audio_path and photo_path settings appropriately
  writeLines(
    text = file.path(demo_fp, "photos"), 
    con = file(paste0(demo_fp, "/settings/image_path.txt"))
  )
  writeLines(
    text = file.path(demo_fp, "recordings"), 
    con = file(paste0(demo_fp, "/settings/audio_path.txt"))
  )
  
  # Expect no error creating the database
  expect_no_error(
    distributed_fp <- dbDistributed(
      con = con,
      out_filepath = tempdir(),
      person_id = "fbaggins",
      media_ids = 1:15,
      mode = "verify",
      use_renv = FALSE,
      disconnect = FALSE
    )
  )
  
  # Expect the correct list of directories in the new mini project
  expect_true(all(basename(list.dirs(distributed_fp)) %in% 
                c(basename(distributed_fp), "database", "photos", "recordings", 
                  "settings")))
  
  # Expect that the photos folder contains 15 files
  expect_equal(
    length(list.files(file.path(distributed_fp, "photos"))),
    15
  )
  
  # Expect to be able to connect to the new DB
  expect_no_error(
    new_con <- dbSetCon(file.path(distributed_fp, "/database/demo.sqlite"))
  )
  
  # Expect the new media table to have 15 entries
  expect_equal(
    nrow(DBI::dbReadTable(new_con, "media")), 15
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  DBI::dbDisconnect(conn = new_con)
  unlink(demo_fp, recursive = TRUE)
  unlink(distributed_fp, recursive = TRUE)
  
})

test_that("New distributed DB and mini project are created in annotate mode", {
  
  # get demo
  demo_fp <- file.path(tempdir(), 'demoAMM/')
  
  # copy to temp dir to edit
  file.copy(from = file.path(
    find.package("AMMonitor", lib.loc = .libPaths()),
    "extdata/demoAMM/"
  ), to = tempdir(), recursive = TRUE)
  
  # set the connection
  con <- dbSetCon(file.path(demo_fp, "database/demo.sqlite"))
  
  # Update the audio_path and photo_path settings appropriately
  writeLines(
    text = file.path(demo_fp, "photos"), 
    con = file(paste0(demo_fp, "/settings/image_path.txt"))
  )
  writeLines(
    text = file.path(demo_fp, "recordings"), 
    con = file(paste0(demo_fp, "/settings/audio_path.txt"))
  )
  
  # Expect no error creating the database
  expect_no_error(
    distributed_fp <- dbDistributed(
      con = con,
      out_filepath = tempdir(),
      person_id = "fbaggins",
      media_ids = 1:15,
      mode = "annotate",
      use_renv = FALSE,
      disconnect = FALSE
    )
  )
  
  # Expect the correct list of directories in the new mini project
  expect_true(all(basename(list.dirs(distributed_fp)) %in% 
                    c(basename(distributed_fp), "database", "photos", "recordings", 
                      "settings")))
  
  # Expect that the photos folder contains 15 files
  expect_equal(
    length(list.files(file.path(distributed_fp, "photos"))),
    15
  )
  
  # Expect to be able to connect to the new DB
  expect_no_error(
    new_con <- dbSetCon(file.path(distributed_fp, "/database/demo.sqlite"))
  )
  
  # Expect the new media table to have 15 entries
  expect_equal(
    nrow(DBI::dbReadTable(new_con, "media")), 15
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  DBI::dbDisconnect(conn = new_con)
  unlink(demo_fp, recursive = TRUE)
  unlink(distributed_fp, recursive = TRUE)
  
})