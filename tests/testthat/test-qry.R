test_that("qry_row works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run qry_row
  expect_no_error(
    visits <- qry_row(con = con,
                      tableName = 'visits',
                      rowConditions = NA,
                      colConditions = '*',
                      disconnect = FALSE)
  )
  
  expect_no_error(
    locations <- qry_row(con = con,
                      tableName = 'locations',
                      rowConditions = list(pk_locationid = 'locationA'),
                      colConditions = '*',
                      disconnect = FALSE)
  )
  
  expect_no_error(
    taxa <- qry_row(con = con,
                         tableName = 'taxa',
                         rowConditions = NA,
                         colConditions = 'pk_taxonid',
                         disconnect = FALSE)
  )
  
  DBI::dbDisconnect(con)
  
})

test_that("qryTags works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))

  # Retrieve library tags only (tags applied to taxa)
  expect_no_error(
    tags <- qryTags(con = con, media = FALSE)
  )
  
  # Retrieve library and media tags
  expect_no_error(
    tags <- qryTags(con = con, media = TRUE)
  )
  
  DBI::dbDisconnect(con)
  
})

test_that("qryItems works", { 
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  expect_no_error(
    results <-  qryItems(
               con = con, 
               table = "lists", 
               listname = "datum", 
               item = "WGS84"
     )
  )
  
  expect_equal(results$fk_listid, 'datum')
  expect_equal(results$item, 'WGS84')
   
  # Retrieve all items from the media_type list
  expect_no_error( 
   results <-  qryItems(
       con = con, 
       table = "lists", 
       listname = "media_type"
      )
  )
  
  expect_equal(unique(results$fk_listid), 'media_type')
  
  DBI::dbDisconnect(con)
  
})

test_that("qryMediaLocations works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  expect_no_error(
    results <- qryMediaLocations(con = con,
                                 mediaType = 'photo')
  )
  
  expect_equal(results, c('locationA',
                          'locationB',
                          'locationC'))
  
  expect_no_error(
    results <- qryMediaLocations(con = con,
                                 mediaType = 'audio')
  )
  
  expect_equal(results, 'locationA')
  
  expect_no_error(
    results <- qryMediaLocations(con = con,
                                 mediaType = 'video')
  )
  
  expect_equal(length(results), 0)
  
  DBI::dbDisconnect(con)
  
})

test_that("qryVisitTable works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  expect_no_error(
    results <- qryVisitTable(con,
                             mediaType = 'photo',
                             location = 'locationA',
                             personid = NA)
  )
  
  # try location where no audio files
  expect_no_error(
    results <- qryVisitTable(con,
                             mediaType = 'audio',
                             location = 'locationC',
                             personid = NA)
  )
  
  expect_equal(nrow(results), 0)
  
  # by personid
  expect_no_error(
    results <- qryVisitTable(con,
                             mediaType = 'photo',
                             location = 'locationC',
                             personid = 'fbaggins')
  )
  
  DBI::dbDisconnect(con)
  
})

test_that("qryMedia works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  expect_no_error(
    results <- qryMedia(con)
  )
  
  expect_no_error(
    results <- qryMedia(con,
                        locationID = 'locationA')
  )
  
  expect_match(results$filename, 'locationA')
  expect_no_match(results$filename, 'locationB')
  expect_no_match(results$filename, 'locationC')
  
  expect_no_error(
    results <- qryMedia(con,
                        taxonID = 'bear')
  )
  
  expect_no_error(
    results_all <- qryMedia(con)
  )
  
  expect_no_error(
    results_excluded <- qryMedia(con,
                                  excludeAnnotated = 'Me',
                                  selectedUser = 'bbaggins')
  )
  
  expect_false(nrow(results_all) == nrow(results_excluded))
  
  DBI::dbDisconnect(con)
  
})

test_that("qryModelOutputsMedia works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))

  expect_no_error(
    results <- qryModelOutputsMedia(con)
  ) # throws error at qry.R#629

  DBI::dbDisconnect(con)
  
})

test_that("qryModelVerificationConsensus works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryModelVerificationConsensus(con,
                                             modelid = 4)
  )
  
  DBI::dbDisconnect(con)
  
})

test_that("qryCheckMediaFilePaths works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  expect_no_error(
    results <- qryCheckMediaFilePaths(con)
  ) # throws error at qry.R#759
  
  DBI::dbDisconnect(con)
  
})

test_that("qryCheckTags works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryCheckTags(
             con, 
             mediaid = 10, 
             personid = NA, 
             excludeperson = FALSE, 
             mediatags = FALSE, 
             exists = FALSE, 
             disconnect = FALSE
       )
  )
  
  expect_no_error(
    results <- qryCheckTags(
      con, 
      mediaid = 10, 
      personid = NA, 
      excludeperson = FALSE, 
      mediatags = FALSE, 
      exists = TRUE, 
      disconnect = FALSE
    )
  )
  
  expect_no_error(
    results <- qryCheckTags(
      con, 
      mediaid = 10, 
      personid = 'fbaggins', 
      excludeperson = FALSE, 
      mediatags = FALSE, 
      exists = FALSE, 
      disconnect = FALSE
    )
  )
  
  expect_equal(nrow(results), 0)
  
  expect_no_error(
    results <- qryCheckTags(
      con, 
      mediaid = 10, 
      personid = 'bbaggins', 
      excludeperson = TRUE, 
      mediatags = FALSE, 
      exists = FALSE, 
      disconnect = FALSE
    )
  )
  
  expect_equal(nrow(results), 0)
  
  DBI::dbDisconnect(con)
  
})

test_that("qryMediaDateRange works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryMediaDateRange(con = con,
                                 mediaType = 'photo')
  )
  
  expect_no_error(
    results <- qryMediaDateRange(con = con,
                                 mediaType = 'audio')
  )
  
  # because only 1 audio record in demo:
  expect_equal(results$startdate, results$enddate)
  
  DBI::dbDisconnect(con)
  
})

test_that("qryVerifications works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryVerifications(con, 
                                table = 'annotations')
  )
  
  expect_true(nrow(results) != 0)
  
  expect_no_error(
    results <- qryVerifications(con, 
                                table = 'modeloutputs')
  )
  
  expect_true(nrow(results) != 0)
  
  DBI::dbDisconnect(con)
  
})

test_that("qryModelOutputs works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryModelOutputs(con)
  )
  
  modeloutputs <- DBI::dbReadTable(con, 'modeloutputs')
  
  # get example from 1st row modelouputs
  mediaid_1 <- modeloutputs$fk_mediaid[1]
  
  expect_no_error(
    results <- qryModelOutputs(con, 
                               mediaIDs = mediaid_1)
  )
  
  expect_true(nrow(results) > 0)
  
  # get example from 1st row modelouputs
  modelid_1 <- modeloutputs$fk_modelid[1]
  
  expect_no_error(
    results <- qryModelOutputs(con, 
                               modelIDs = modelid_1)
  )
  
  expect_true(nrow(results) > 0)
  
  DBI::dbDisconnect(con)
  
})

test_that("qryPeopleActivity works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryPeopleActivity(con)
  )
  
  expect_true(nrow(results$visits) > 0)
  expect_true(nrow(results$annotations) > 0)
  expect_true(nrow(results$annotation_verifications) > 0)
  expect_true(nrow(results$model_verifications) > 0)
  
  expect_no_error(
    results <- qryPeopleActivity(con, m_dates = c('2023-01-01', '2023-12-01'))
  )
  
  DBI::dbDisconnect(con)
  
})

test_that("qryLocationCounts works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryLocationCounts(con)
  )
  
  expect_true(nrow(results) > 0)
  
  DBI::dbDisconnect(con)
  
})

test_that("qryEconfiguration works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # run
  expect_no_error(
    results <- qryEconfiguration(con, 
                                 econfigname = "default recorder settings")
  )
  
  expect_true(nrow(results) > 0)
  
  DBI::dbDisconnect(con)
  
})

test_that("qryAnnotations works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # Run the query
  expect_no_error(
    results <- qryAnnotations(
      con = con,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
})

test_that("qryMconfiguration works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # Run the query
  expect_no_error(
    results <- qryMconfiguration(
      con = con,
      mconfigid = 2,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Disconnect from the database
  DBI::dbDisconnect(con)
  
})

test_that("qryUnannotated works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # Run the query
  expect_no_error(
    results <- qryUnannotated(
      con = con,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) == 0)
  
})

test_that("qryEffort works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # Run the query
  expect_no_error(
    results <- qryEffort(
      con = con,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
})

test_that("qryModelSettings works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # Run the query
  expect_no_error(
    results <- qryModelSettings(
      con = con,
      modelid = 6,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Disconnect from the database
  DBI::dbDisconnect(con)
  
})

test_that("qryEquipModelSettings works", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # Run the query
  expect_no_error(
    results <- qryEquipModelSettings(
      con = con,
      equipmodelid = "recorder_model_x",
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Disconnect from the database
  DBI::dbDisconnect(con)
  
})

test_that("qryModelOutputsFull works with all arguments", {
  
  # get the demo database
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM")
  
  # set connection 
  con <- dbSetCon(paste0(demo_fp, '/database/demo.sqlite'))
  
  # Run the query for media IDs
  expect_no_error(
    results <- qryModelOutputsFull(
      con = con,
      mediaIDs = 1:10,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Run the query for a model ID
  expect_no_error(
    results <- qryModelOutputsFull(
      con = con,
      modelIDs = 6,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Run the query for a locationID
  expect_no_error(
    results <- qryModelOutputsFull(
      con = con,
      locationIDs = "locationA",
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Run the query for a date range
  expect_no_error(
    results <- qryModelOutputsFull(
      con = con,
      dateRange = list(c("2023-05-01", "2023-05-31")),
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Run the query for a taxon ID
  expect_no_error(
    results <- qryModelOutputsFull(
      con = con,
      taxonIDs = "oven",
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Run the query for a value_num 
  expect_no_error(
    results <- qryModelOutputsFull(
      con = con,
      value_num = 0.9,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
  # Run a value_num query with the lessThan argument
  expect_no_error(
    results <- qryModelOutputsFull(
      con = con,
      value_num = 0.5,
      lessThan = TRUE,
      disconnect = FALSE
    )
  )
  
  expect_true(nrow(results) > 0)
  
})