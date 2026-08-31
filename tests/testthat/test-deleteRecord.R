test_that("row deleted from table", {

  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # add test record to later delete
  new_person <- data.frame(pk_personid = 'test_person', display_name = 'test')
  dbAppendTable(con, 'people', new_person)
  
  # check error thrown if necessary arguments not given
  expect_error(
    deleteRecord(con = con,
               selected_row = new_person)
  )
  
  # delete row
  deleteRecord(con, 'people', new_person)
  
  # check deleted
  people <- DBI::dbReadTable(con, name = 'people')
  
  expect_false('test_person' %in% people$pk_personid)
  
  
  DBI::dbDisconnect(con)
  unlink(new_demo_fp)
  
})
