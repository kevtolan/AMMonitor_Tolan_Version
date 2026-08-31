test_that("mini demo created with correct files", {
  
  # set filepath to temp directory
  fp <- tempdir()
  
  # run ammCreateMiniDemo
  demo_fp <- ammCreateMiniDemo(fp)
  
  # file exists
  expect_true(file.exists(demo_fp))
  
  # expected files
  exp_files <- c("ammls", "database",
                 "logs", "logs_drop",
                 "ml_drop", "mobile_apps",
                 "photos", "photos_drop", 
                 "recordings", "recordings_drop",
                 "scripts","settings", 
                 "spatials", "tags_drop", 
                 "videos", "videos_drop")
  
  # check files in demo are correct
  expect_equal(list.files(demo_fp), exp_files)

  # get database from demo
  demo_database <- list.files(file.path(demo_fp, "database"))
  
  # expect demo database filename contains .sqlite 
  expect_match(demo_database, '.sqlite')
  
  # get photos files from demo
  photos <- list.files(file.path(demo_fp, "photos"),
                       recursive = FALSE)
  
  expect_true(length(photos) >= 25)
  
  # get recordings files from demo
  recordings <- list.files(file.path(demo_fp, "recordings"),
                           recursive = FALSE)
  
  expect_true(length(recordings) != 0)
  
  
  # samples
  samples <- list.files(file.path(demo_fp, "settings"))
  
  expect_true(length(samples) >= 4)
  
  # default user in settings
  def_user <- readLines(file.path(demo_fp,
                                  "settings",
                                  "default_user.txt"))
  
  expect_equal(def_user, 'sgamgee')
  
})
