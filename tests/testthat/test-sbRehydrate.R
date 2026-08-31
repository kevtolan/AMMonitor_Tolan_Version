test_that("correct AMMonitor project created", {
  
  # Test the API and skip if it fails
  test <- try(suppressWarnings(sbtools::item_file_download(
    sb_id = "654a576bd34ee4b6e05c24d6",
    dest_dir = tempdir()
  )))
  
  if ("try-error" %in% class(test)) {
    skip("Sciencebase API failure, skipping test.")
  }
  
  project_fp <- sbRehydrate(
    ammPath = tempdir(), 
    sbItemNum = "654a576bd34ee4b6e05c24d6",
    projectName = "MiddleEarth3"
  )
  
  # correct files in project
  expect_true(unique(list.files(project_fp) %in% c("ammls", "database",
                                                   "logs", "logs_drop",
                                                   "Middle Earth Wildlife Study Volume 1 (2023 - 2023).xml",
                                                   "ml_drop", "mobile_apps", "photos",
                                                   "photos_drop", "recordings", "recordings_drop", 
                                                   "scripts", "settings", "spatials", "tags_drop",
                                                   "videos", "videos_drop")))
  
  expect_equal(list.files(paste0(project_fp, '/database/')), "MiddleEarth3.sqlite")
  
  # expect can make connection to database
  expect_no_error(
    con <- dbSetCon(paste0(project_fp, '/database/MiddleEarth3.sqlite'))
  )
  
  expect_s4_class(con, class = "SQLiteConnection")
  
  # inspect database structure
  expect_true(length(DBI::dbListTables(con)) == 45)
  
  # inspect some tables
  expect_true(nrow(DBI::dbReadTable(con, name = 'visits')) >= 15)
  expect_true(nrow(DBI::dbReadTable(con, name = 'dbdictionary')) == 391)
  expect_true(nrow(DBI::dbReadTable(con, name = 'lists')) == 26)
  expect_true(nrow(DBI::dbReadTable(con, name = 'listitems')) == 154)
  expect_true(nrow(DBI::dbReadTable(con, name = 'media')) >= 26)
  expect_true(nrow(DBI::dbReadTable(con, name = 'taxa')) >= 15)
  
  # check all media updated with sb id == 1
  media <- DBI::dbReadTable(con, name = 'media')
  expect_equal(unique(media$fk_sciencebaseid), 1)
  
  # check photos and audio not downloaded
  photos_path <- paste0(project_fp, '/photos/')
  expect_equal(length(list.files(photos_path)), 0)
  
  recordings_path <- paste0(project_fp, '/recordings/')
  expect_equal(length(list.files(recordings_path)), 0)
  
  
  DBI::dbDisconnect(con)
  
})
