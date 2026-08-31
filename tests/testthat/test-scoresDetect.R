test_that("Expected scores returned, and modeloutputs table updated", {
 
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
  
  # read in templates
  templates <- readRDS(paste0(demo_fp, "/ammls/templates.RDS"))
  
  # clear out modeloutputs table 
  DBI::dbExecute(con, statement = "DELETE FROM modeloutputs;")
  
  # run with dbInsert = FALSE to start to inspect scores
  suppressWarnings( # warnings from monitor are expected
     expect_no_error(
          scores <- scoresDetect(
               con = con,
               recordingNames = "all",
               recordingRootPath = paste0(demo_fp, "/recordings"),
               ammlPath = paste0(demo_fp, "/ammls"),
               dbInsert = FALSE
              )
      )
    )
  
  expect_equal(names(scores), c('fk_mediaid',
                                  'fk_modelid',
                                  'fk_taxonid',
                                  'x_min', 'x_max',
                                  'y_min', 'y_max',
                                  'value_num'))
  expect_true(nrow(scores) >= 1)
    
    
  # run with dbInsert = TRUE to start to check scores added to modeloutputs
  suppressWarnings( # warnings from monitor are expected
    expect_no_error(
        scores2 <- scoresDetect(
          con = con,
          recordingNames = "all",
          recordingRootPath = paste0(demo_fp, "/recordings"),
          ammlPath = paste0(demo_fp, "/ammls"),
          dbInsert = TRUE
        )
    )
  )
  
  new_model_outputs <- DBI::dbReadTable(con, name = 'modeloutputs')
    
  expect_equal(nrow(scores), nrow(new_model_outputs))
    

  DBI::dbDisconnect(con)
  unlink(demo_fp)
  
})

test_that("Sucessful run when audio is in different sample rate", {
  
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
  
  # read in templates
  templates <- readRDS(paste0(demo_fp, "/ammls/templates.RDS"))
  
  # clear out modeloutputs table 
  DBI::dbExecute(con, statement = "DELETE FROM modeloutputs;")
  
  # resample audio file to different sample rate
  
  audio <- tuneR::readWave(paste0(demo_fp, "/recordings/1.wav"))
  
  audio <- seewave::resamp(audio,
                           f = audio@samp.rate,
                           g = 48000,
                           output = "Wave")
  
  suppressWarnings(
    tuneR::writeWave(audio,
                   paste0(demo_fp, "/recordings/1.wav"))
  )
  
  # run with dbInsert = FALSE to start to inspect scores
  suppressWarnings( # warnings from monitor are expected
    expect_no_error(
      scores <- scoresDetect(
        con = con,
        recordingNames = "all",
        recordingRootPath = paste0(demo_fp, "/recordings"),
        ammlPath = paste0(demo_fp, "/ammls"),
        dbInsert = FALSE
      )
    )
  )
  
  expect_equal(names(scores), c('fk_mediaid',
                                'fk_modelid',
                                'fk_taxonid',
                                'x_min', 'x_max',
                                'y_min', 'y_max',
                                'value_num'))
  expect_true(nrow(scores) >= 1)
  
  
  # run with dbInsert = TRUE to start to check scores added to modeloutputs
  suppressWarnings( # warnings from monitor are expected
    expect_no_error(
      scores2 <- scoresDetect(
        con = con,
        recordingNames = "all",
        recordingRootPath = paste0(demo_fp, "/recordings"),
        ammlPath = paste0(demo_fp, "/ammls"),
        dbInsert = TRUE
      )
    )
  )
  
  new_model_outputs <- DBI::dbReadTable(con, name = 'modeloutputs')
  
  expect_equal(nrow(scores), nrow(new_model_outputs))
  
  
  DBI::dbDisconnect(con)
  unlink(demo_fp)
  
})


test_that("Error if no templates names match", {
  
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
    
    # read in templates
    templates <- readRDS(paste0(demo_fp, "/ammls/templates.RDS"))
    
    # update template names
    DBI::dbExecute(con,
                   statement = "UPDATE models SET model_name = 'test_failure1' WHERE pk_modelid = 1;")
    DBI::dbExecute(con,
                   statement = "UPDATE models SET model_name = 'test_failure2' WHERE pk_modelid = 2;")
    DBI::dbExecute(con,
                   statement = "UPDATE models SET model_name = 'test_failure3' WHERE pk_modelid = 3;")
    DBI::dbExecute(con,
                   statement = "UPDATE models SET model_name = 'test_failure4' WHERE pk_modelid = 4;")
    
    
    
    # run with dbInsert = FALSE to start to inspect scores
    suppressWarnings( # warnings from monitor are expected
      expect_error(
        scores <- scoresDetect(
          con = con,
          recordingNames = "all",
          recordingRootPath = paste0(demo_fp, "/recordings"),
          ammlPath = paste0(demo_fp, "/ammls"),
          dbInsert = FALSE
        )
      )
    )
    
    DBI::dbDisconnect(con)
    unlink(demo_fp)
})