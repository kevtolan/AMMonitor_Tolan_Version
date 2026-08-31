test_that("directories created", {
  
  parent_dir <- paste0(tempdir(), "/parent")
  
  if (dir.exists(parent_dir) == TRUE) {
    unlink(parent_dir, recursive = TRUE)} else {
      dir.create(parent_dir)
    }
  
  fp <- AMMonitor::ammCreateDirectories(
    amm_dirname = "testAMMproject",
    filepath = parent_dir
  )
  
  # test that a message is sent
  # expect_output()
  
  # test that the project directory contains all subdirectories
  folders <- sort(c(
    "database", "recordings", "spatials", "logs_drop", "logs",
    "settings", "photos", "videos", "ammls", "photos_drop",
    "recordings_drop", "videos_drop", "scripts", "tags_drop",
    "ml_drop", "mobile_apps")
  )
  
  dirs <- list.files(paste0(parent_dir, "/testAMMproject"))
  
  # expect the directory names to match
  expect_equal(dirs, folders)
  
  # expect that exactly 16 directories were created
  expect_length(dirs, n = 16)
  
  # remove testing folder
  unlink(parent_dir, recursive = TRUE)
  
}
)
