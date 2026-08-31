test_that("Successfully add record- no file", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # new record dataframe
  new_person <- data.frame(
    pk_personid = 'test_person',
    display_name = 'test')
  
  expect_no_error(
     rs <- addRecord(
        con = con,
        table_name = 'people',
        new_record = new_person)
  )
  
  # true status
  expect_true(rs$status)
  
  # check for record in database
  people <- DBI::dbReadTable(con, 'people')
  
  expect_true(
    new_person$pk_personid %in% people$pk_personid
  )
 
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
})

test_that("Successfully add record- media file local ", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # photo to add to the database (use one of the demo photos)
  file_source <- dir(
     file.path(
       find.package("AMMonitor", lib.loc = .libPaths()), 
       "extdata/demoAMM/photos"
     ), 
     full.names = T
   )[1]
   
   # where the new photo should be saved 
   file_destination <- file.path(tempdir(), 'test.JPG')
   
   # create dataframe to be sent to the addRecord function
   new_media <- data.frame(
     media_type = "photo",
     filename = "test.JPG",
     fk_visitid = 1,
     start_date = "1900-01-01",
     start_time = "00:00:00",
     filepath = file_destination
   )
   
   # add the record with media
   expect_no_error(
     rs <- addRecord(
       con = con,
       table_name = "media",
       new_record = new_media,
       add_file = TRUE,
       storage_type = "local",
       file_path = file_source
    )
   )
   
  
  # true status
  expect_true(rs$status)
  
  # check for record in database
  media <- DBI::dbReadTable(con, 'media')
  
  expect_true(
    new_media$filename %in% media$filename
  )
  
  
  # file copied to correct destination
  expect_true(
    file.exists(file_destination)
  )
  
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(file_destination)
})

# test_that("Successfully add record- google drive", {
#   testing will require google drive authentication
# })

# test_that("Successfully add record- sharepoint", {
#   testing will require sharepoint authentication
# })

# test_that("Successfully add record- aws s3", {
#   testing will require s3 access
# })

test_that("False rs if cannot find file (no error, not added to db)", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # photo to add to the database (use one of the demo photos)
  file_source <- "empty_filepath.JPG"
  
  # where the new photo should be saved 
  file_destination <- file.path(tempdir(), 'filepath.JPG')
  
  # create dataframe to be sent to the addRecord function
  new_media <- data.frame(
    media_type = "photo",
    filename = "empty_filepath.JPG",
    fk_visitid = 1,
    start_date = "1900-01-01",
    start_time = "00:00:00",
    filepath = file_destination
  )
  
  # add the record with media
  expect_no_error(
    rs <- addRecord(
      con = con,
      table_name = "media",
      new_record = new_media,
      add_file = TRUE,
      storage_type = "local",
      file_path = file_source
    )
  )
  
  
  # false status
  expect_false(rs$status)
  
  # expect not in database
  media  <- DBI::dbReadTable(con, 'media')
  
  expect_false(
    new_media$filename %in% media$filename
  )
  
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
  
})

test_that("False rs if wrong tablename", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # photo to add to the database (use one of the demo photos)
  file_source <- dir(
    file.path(
      find.package("AMMonitor", lib.loc = .libPaths()), 
      "extdata/demoAMM/photos"
    ), 
    full.names = T
  )[1]
  
  # where the new photo should be saved 
  file_destination <- file.path(tempdir(), 'test.JPG')
  
  # create dataframe to be sent to the addRecord function
  new_media <- data.frame(
    media_type = "photo",
    filename = "test.JPG",
    fk_visitid = 1,
    start_date = "1900-01-01",
    start_time = "00:00:00",
    filepath = file_destination
  )
  
  # add the record with media
  expect_no_error(
    rs <- addRecord(
      con = con,
      table_name = "models",
      new_record = new_media,
      add_file = TRUE,
      storage_type = "local",
      file_path = file_source
    )
  )
  
  
  # false status
  expect_false(rs$status)
  
  # make sure file not copied
  expect_false(
    file.exists(file_destination)
  )
  
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

