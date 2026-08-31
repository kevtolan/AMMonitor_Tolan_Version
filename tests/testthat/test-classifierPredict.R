test_that("classifierPredict works", {
 
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set connection 
  con <- dbSetCon(paste0(new_demo_fp, ''))
  
  # template not in database
  expect_error(
    classifierPredict(
         con, 
         classifierID = "template_AUDB02_20190513_001500_rf",
         recordingRootPath = 'https://cruvt-external-share.s3.amazonaws.com/ctfif/recdings/',
         ammlPath = '~/R_Packages/AMMonitor/testproj_frogs/ammls',
         showProgress = TRUE,
       )
  )
})
