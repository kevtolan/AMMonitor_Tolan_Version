test_that("error if open connection not passed", {
  
  # create empty con object
  con <- ""
  
  # expect error
  expect_error(dbCheckData(con = con))
  
})

test_that("correct output for demo w/o errors", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # set the connection
  con <- dbSetCon(demo_fp)
  
  # run dbCheckData
  results <- dbCheckData(con = con, disconnect = TRUE)
  
  # expect list of 3 elements
  expect_equal(length(results), 3)
  
  # expect these names
  expected_names <- c("name", "description", "advice" )
  
  expect_equal(names(results), expected_names)
  
  
  # expect empty results dataframes
  expect_equal(nrow(results[1]), 0)
  expect_equal(nrow(results[2]), 0)
  expect_equal(nrow(results[3]), 0)

})

test_that("correct outputs for demo w/ errors", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # introduce errors
  
  # introducing an out-of-sequence pull visit before a new set at this location
  DBI::dbAppendTable(
       con,
       name = 'visits',
       value = data.frame(
         pk_visitid = 100,
         fk_personid = "fbaggins",
         fk_locationid = "locationA",
         fk_equipmentid = "camera1",
         visit_type = "pull", 
         visit_date = "2024-01-01",
         visit_time = "12:00:00",
         visit_notes = NA,
         fk_econfigid = NA
       ))
  
  # Adding a visit with invalid visit time
   DBI::dbAppendTable(
       con,
       name = 'visits',
       value = data.frame(
         pk_visitid = 101,
         fk_personid = "fbaggins",
         fk_locationid = "locationB",
         fk_equipmentid = "camera2",
         visit_type = "set", 
         visit_date = "2024-01-01",
         visit_time = "2:00",
         visit_notes = NA,
         fk_econfigid = NA
       ))
  
  # run dbCheckData
  results <- dbCheckData(con = con, disconnect = TRUE)
  
  # expect names has new entries
  expect_equal(length(results$name), 4)
  
  expect_equal(results$name, c("invalid_visit_times",
                               "visit_type_mismatch",
                               "out_of_bound_startDate", 
                               "out_of_bound_startDate"))

  # expect description and advice have new entries
  expect_equal(length(results$description), 4)
  expect_equal(length(results$advice), 4)
  
  unlink(paste0(tempdir(), '/demo.sqlite')) 
  
})