test_that("new database correctly created", {
  
  dbCreate(new_db_name = 'test_db.sqlite',
           new_db_filepath = tempdir(),
           db_source = 'default')
  
  test_db_path <- paste0(tempdir(), '/test_db.sqlite')
  
  # check file exists
  expect_true(file.exists(test_db_path))
  
  # expect can make a valid connection to db
  expect_no_error(con <- dbSetCon(test_db_path))
  
  # check tables
  expected_tables <- c("accounts", "analyses",
                       "analysisoutputs", "annotags",
                       "annotagverifications", "annotations",
                       "annotationverifications", "dbdictionary",
                       "econfignames", "econfigvalues",
                       "equipment", "equipmodels",
                       "esettingnames", "esettingoptions",
                       "languages", "librarylistitems",
                       "librarylists", "listitems",
                       "lists", "locations", "logs",
                       "mconfignames", "mconfigvalues", 
                       "media", "medialistitems", "medialists",
                       "mediatags", "mediatagverifications",
                       "modellabels",
                       "modeloutputs", "models",
                       "modelverifications", "msettingnames",
                       "msettingoptions", "objectives", 
                       "people", "priorities", "sciencebase",
                       "shinytable", "spatials", "taxa",
                       "taxonlanguage", "temporallistitems",
                       "temporallists", "temporals", "visits")
  
  actual_tables <- DBI::dbListTables(con)
  
  expect_equal(actual_tables, expected_tables)
  
  # check dictionary
  expect_true(nrow(DBI::dbReadTable(con, name = 'dbdictionary')) >= 391)
  
  # check lists and listitems
  expect_true(nrow(DBI::dbReadTable(con, name = 'lists')) >= 26)
  expect_true(nrow(DBI::dbReadTable(con, name = 'listitems')) >=  154)
  
  
  DBI::dbDisconnect(con)
  
  unlink(test_db_path)
  
})
