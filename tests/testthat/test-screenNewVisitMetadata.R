test_that("Expect error if database connection is invalid", {
  
  # Create an empty connection object
  con <- ""
  
  # Try to run function with no connection
  expect_error(
    screenNewVisitMetadata(
      con = con,
      visitMetadata = NA,
      disconnect = FALSE
    )
  )
  
})

test_that("Check finds no errors with valid visit", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a data frame with some example visit metadata
  visit <- data.frame(
    pk_visitID = NA,
    fk_personid = "fbaggins",
    fk_locationid = "locationA",
    fk_equipmentid = "camera1",
    visit_type = "set",
    visit_date = "2024-01-01",
    visit_time = "12:00:00"
  )
  
  # Screen the new visit's metadata
  visit_check <- expect_no_error(
    screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE))
  
  # Expect the status to still be 1
  expect_equal(visit_check$status, 1)
  
  # Expect an empty data frame in warnings
  warnings <- data.frame(
    warning = character(0),
    description = character(0),
    solution = character(0),
    severity = integer(0)
  )
  
  expect_identical(visit_check$warnings, warnings)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("Expect warnings if essential visit metadata is missing", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a visit with a missing location
  visit <- data.frame(
    pk_visitID = NA,
    fk_personid = "fbaggins",
    fk_locationid = NA,
    fk_equipmentid = "camera1",
    visit_type = "set",
    visit_date = "2024-01-01",
    visit_time = "12:00:00"
  )
  
  # Run the function
  visit_check <- screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE
  )
  
  # Expect status set to 0
  expect_equal(visit_check$status, 0)
  
  # Expect a warning in the dataframe for a missing locationid
  warnings <- data.frame(
    warning = "missing_fk_locationid",
    description = "Field fk_locationid is missing",
    solution = "Go back and add fk_locationid to visit metadata",
    severity = 3
  )
  
  expect_identical(visit_check$warnings, warnings)
  
  # Create a visit with a missing person
  visit <- data.frame(
    pk_visitID = NA,
    fk_personid = NA,
    fk_locationid = "locationA",
    fk_equipmentid = "camera1",
    visit_type = "set",
    visit_date = "2024-01-01",
    visit_time = "12:00:00"
  )
  
  # Run the function
  visit_check <- screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE
  )
  
  # Expect status set to 0
  expect_equal(visit_check$status, 0)
  
  # Expect a warning in the dataframe for a missing personid
  warnings <- data.frame(
    warning = "missing_fk_personid",
    description = "Field fk_personid is missing",
    solution = "Go back and add fk_personid to visit metadata",
    severity = 3
  )
  
  expect_identical(visit_check$warnings, warnings)
  
  # Create a visit with a missing equipmentID
  visit <- data.frame(
    pk_visitID = NA,
    fk_personid = "fbaggins",
    fk_locationid = "locationA",
    fk_equipmentid = NA,
    visit_type = "set",
    visit_date = "2024-01-01",
    visit_time = "12:00:00"
  )
  
  # Run the function
  visit_check <- screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE
  )
  
  # Expect status set to 0
  expect_equal(visit_check$status, 0)
  
  # Expect a warning in the dataframe for a missing equipment
  warnings <- data.frame(
    warning = "missing_fk_equipmentid",
    description = "Field fk_equipmentid is missing",
    solution = "Go back and add fk_equipmentid to visit metadata",
    severity = 3
  )
  
  expect_identical(visit_check$warnings, warnings)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("Warning if duplicate visit is already in the db", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a duplicate visit dataframe
  visit <- data.frame(
    fk_personid = "fbaggins",
    fk_locationid = "locationA",
    fk_equipmentid = "recorder1",
    visit_type = "set",
    visit_date = "2023-01-01",
    visit_time = "12:00:00"
  )
  
  # Run the function
  visit_check <- screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE
  )
  
  # Create a dataframe with the expected duplicate visit error
  warnings <- data.frame(
    warning = "duplicateVisit",
    description = "A visit with matching location, equipment, person, and date already exists in the database.",
    solution = 'Check to make sure this visit was not already added to the database.',
    severity = 3
  )
  
  # Expect that the warning is present in the dataframe
  expect_identical(visit_check$warnings[1, ], warnings)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("Warning from missing visit type", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a visit with a visit type
  visit <- data.frame(
    pk_visitID = NA,
    fk_personid = "fbaggins",
    fk_locationid = "locationA",
    fk_equipmentid = "camera1",
    visit_type = NA,
    visit_date = "2024-01-01",
    visit_time = "12:00:00"
  )
  
  # Run the function
  visit_check <- screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE
  )
  
  # Expect two warning messages
  warnings <- data.frame(
    warning = c("missing_visit_type", "noVisitType"),
    description = c("Field visit_type is missing", "No visit type was provided."),
    solution = c("Go back and add visit_type to visit metadata",
                 "Add a visit type to the visit metadata, then rerun this function to complete checks."),
    severity = c(3, 3)
  )
  
  expect_identical(visit_check$warnings, warnings)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("Warning if visit includes an equipment set that's already deployed", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a visit that redeploys an equipment that is deployed
  visit <- data.frame(
    pk_visitID = NA,
    fk_personid = "fbaggins",
    fk_locationid = "locationA",
    fk_equipmentid = "camera1",
    visit_type = "set",
    visit_date = "2023-06-01",
    visit_time = "12:00:00"
  )
  
  # Run the function
  visit_check <- screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE
  )
  
  # Expect a deployed equipment error
  warnings <- data.frame(
    warning = "setDeployedEquip",
    description = "Equipment camera1 is already deployed at location locationA",
    solution = 'Be sure a "pull" visit for the given equipment is registered before it is set in a new location.',
    severity = 2
  )
  
  expect_identical(visit_check$warnings, warnings)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

test_that("Warning if a check or pull is registered without a corresponding set", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create a pull visit with no set
  visit <- data.frame(
    pk_visitID = NA,
    fk_personid = "fbaggins",
    fk_locationid = "locationA",
    fk_equipmentid = "camera1",
    visit_type = "pull",
    visit_date = "2024-12-31",
    visit_time = "12:00:00"
  )
  
  # Run the function
  visit_check <- screenNewVisitMetadata(
    con = con,
    visitMetadata = visit,
    disconnect = FALSE
  )
  
  # Expect matching visit warning
  warnings <- data.frame(
    warning = "matchingVisit",
    description = "Visit of type pull with no matching set/check visit.",
    solution = "Make sure to enter a matching set/check for the visit.",
    severity = 2
  )
  
  expect_identical(visit_check$warnings, warnings)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})