test_that("launchApp fails with incorrect path", {

  expect_error(
    launchApp(tempdir())
  )
  
  # with correct file structure just no database
  ammCreateDirectories(amm_dirname = 'launchApptest',
                       filepath = tempdir())
  
  expect_error(
    launchApp(paste0(tempdir(), '/launchApptest'))
    )
  
})
