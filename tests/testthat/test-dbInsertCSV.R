test_that("Fail to add if unique constraint failed", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  
  csv_path <- file.path(tempdir(), 'test.csv')
   
  utils::write.csv(
    x = data.frame(
    pk_personid = c("bbaggins", "fbaggins"),
    first_name = c("Bilbo", "Frodo"),
    last_name = c("Baggins", "Baggins"),
    project_role = c("Tagger", "Field technician"),
    display_name = c("blue", "red")), 
    file = csv_path
  )
   
  # try to import records from the CSV file into the people table
  expect_no_error(
    rs <- dbImportCSV(
     con = con,
     csv_path = csv_path,
     table_name = "people",
     disconnect = FALSE
   )
  )
  
  expect_true(
    all(rs$status == 0)
  )
  
  # Clean up
  rm(rs)
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
})

test_that("Successfully add new record", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  
  csv_path <- file.path(tempdir(), 'test.csv')
  
  new_dataframe <- data.frame(
    pk_personid = c("radagast", "bill"),
    first_name = c("Radagast", "Bill"),
    last_name = c("TheBrown", "Pony"),
    project_role = c("Tagger", "Field technician"),
    display_name = c("wizard", "pony"))
  
  utils::write.csv(
    x = new_dataframe, 
    file = csv_path
  )
  
  # try to import records from the CSV file into the people table
  expect_no_error(
    result <- dbImportCSV(
      con = con,
      csv_path = csv_path,
      table_name = "people",
      disconnect = FALSE
    )
  )
  
  expect_true(
    all(result$status == 1)
  )
  
  # find both new records in the database
  people <- DBI::dbReadTable(con, 'people')
  
  expect_true(
    all(new_dataframe$pk_personid %in% people$pk_personid)
  )
  
  # Clean up
  rm(result)
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

# test_that("", {
#   
# })
# 
# test_that("", {
#   
# })
# 
# test_that("", {
#   
# })