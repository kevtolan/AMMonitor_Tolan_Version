test_that("Error on invalid database connection", {
  
  # Create an empty connection object
  con <- ""
  
  # Expect error on bad connection
  expect_error(
    results <- taxaAdd(
      con = con,
      tsns = 179021,
      common_names = "Red-eyed Vireo",
      pk_taxonids = "revi",
      overwrite = FALSE,
      disconnect = FALSE
    )
  )
  
})

test_that("taxa added if API is working", {
  
  # Test the API first
  test <- try(ritis::full_record(179021))
  
  # Skip if there's an error
  if ("try-error" %in% class(test)) {
    skip("ITIS not available, test skipped.")
  }
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Get the number of taxa records before adding
  taxa_pre_add <- nrow(DBI::dbReadTable(con, "taxa"))
  
  # Attempt to run the function
  expect_no_error(
    results <- taxaAdd(
      con = con,
      tsns = 179021,
      common_names = "Red-eyed Vireo",
      pk_taxonids = "revi",
      overwrite = FALSE,
      disconnect = FALSE
    )
  )
  
  # Get the number of taxa in the table now
  taxa_post_add <- nrow(DBI::dbReadTable(con, "taxa"))
  
  # Expect more rows in the table
  expect_true(taxa_post_add > taxa_pre_add)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("pk_taxonids are auto-assigned if not provided", {
  
  # Test the API first
  test <- try(ritis::full_record(179021))
  
  # Skip if there's an error
  if ("try-error" %in% class(test)) {
    skip("ITIS not available, test skipped.")
  }
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Get the number of taxa records before adding
  taxa_pre_add <- nrow(DBI::dbReadTable(con, "taxa"))
  
  # Attempt to run the function
  expect_no_error(
    results <- taxaAdd(
      con = con,
      tsns = c(179021, 179010),
      common_names = c("Red-eyed Vireo", "Blue-headed Vireo"),
      pk_taxonids = NA,
      overwrite = FALSE,
      disconnect = FALSE
    )
  )
  
  # Read in the updated taxa table
  taxa_tbl <- DBI::dbReadTable(con, "taxa")
  
  # Get the number of taxa rows after adding
  taxa_post_add <- nrow(taxa_tbl)
  
  # Expect more rows after the function is run
  expect_true(taxa_post_add > taxa_pre_add)
  
  # Expect no NAs in the taxon IDs
  # Including this due to a previous error that should be fixed
  expect_true(all(!(is.na(taxa_tbl$pk_taxonid))))
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("Make sure overwrite argument works", {
  
  # Test the API first
  test <- try(ritis::full_record(179021))
  
  # Skip if there's an error
  if ("try-error" %in% class(test)) {
    skip("ITIS not available, test skipped.")
  }
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Get the number of taxa records before adding
  taxa_pre_add <- nrow(DBI::dbReadTable(con, "taxa"))
  
  # Attempt to run the function, overwriting ovenbird
  expect_no_error(
    results <- taxaAdd(
     con = con,
     tsns = 726205,
     common_names = "Ovenbird",
     pk_taxonids = "Ovenbird",
     overwrite = TRUE,
     disconnect = FALSE
  ))
  
  # Read in the taxa table post-add
  taxa_post_add <- nrow(DBI::dbReadTable(con, "taxa"))
  
  # Expect the same number of rows since one has been overwritten
  expect_true(taxa_post_add == taxa_pre_add)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})