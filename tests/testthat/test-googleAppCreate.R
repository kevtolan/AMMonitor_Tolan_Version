test_that("Error if database connection is invalid", {
  
  # Create an empty database object
  con <- ""
  
  # Expect error with bad connection
  expect_error(
    googleAppCreate(
      con = con,
      disconnect = FALSE
    )
  )
  
})

test_that("Sucessful spreadsheet per equip_type, including all.", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # camera ====
  
  # write spreadsheet
  
  expect_no_error(
    fp <- googleAppCreate(
      con = con,
      equip_type = 'camera'
    )
  )
  
  # check spreadsheet
  personids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_personid"
  )
  
  locationids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_locationid"
  )
  
  equipmentids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_equipmentid"
  )
  
  visit_types <- readxl::read_xlsx(
    fp, 
    sheet = "visit_type"
  )
  
  econconfigids <- readxl::read_xlsx(
    fp,
    sheet = "fk_econfigid"
  )
  
  people <- DBI::dbReadTable(con, 'people')
  locations <- DBI::dbReadTable(con, 'locations')
  equipment <- DBI::dbReadTable(con, 'equipment')
  
  
  expect_true(
    all(people$pk_personid %in% personids$choices)
  )
  
  expect_true(
    all(locations$pk_locationid %in% locationids$choices)
  )
  
  expect_true(
    all(equipment$pk_equipmentid[
      which(equipment$equip_type == 'camera')
    ] %in% equipmentids$choices)
  )
  
  expect_false(
    any(equipment$pk_equipmentid[
      which(equipment$equip_type == 'recorder')
    ] %in% equipmentids$choices)
  )
  
  unlink(fp)
  
  # recorder ====
  
  # write spreadsheet
  
  expect_no_error(
    fp <- googleAppCreate(
      con = con,
      equip_type = 'recorder'
    )
  )
  
  # check spreadsheet
  personids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_personid"
  )
  
  locationids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_locationid"
  )
  
  equipmentids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_equipmentid"
  )
  
  visit_types <- readxl::read_xlsx(
    fp, 
    sheet = "visit_type"
  )
  
  econconfigids <- readxl::read_xlsx(
    fp,
    sheet = "fk_econfigid"
  )
  
  people <- DBI::dbReadTable(con, 'people')
  locations <- DBI::dbReadTable(con, 'locations')
  equipment <- DBI::dbReadTable(con, 'equipment')
  
  
  expect_true(
    all(people$pk_personid %in% personids$choices)
  )
  
  expect_true(
    all(locations$pk_locationid %in% locationids$choices)
  )
  
  expect_true(
    all(equipment$pk_equipmentid[
      which(equipment$equip_type == 'recorder')
    ] %in% equipmentids$choices)
  )
  
  expect_false(
    any(equipment$pk_equipmentid[
      which(equipment$equip_type == 'camera')
    ] %in% equipmentids$choices)
  )
  unlink(fp)
  
  
  # recorder ====
  
  # write spreadsheet
  
  expect_no_error(
    fp <- googleAppCreate(
      con = con,
      equip_type = 'all'
    )
  )
  
  # check spreadsheet
  personids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_personid"
  )
  
  locationids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_locationid"
  )
  
  equipmentids <- readxl::read_xlsx(
    fp, 
    sheet = "fk_equipmentid"
  )
  
  visit_types <- readxl::read_xlsx(
    fp, 
    sheet = "visit_type"
  )
  
  econconfigids <- readxl::read_xlsx(
    fp,
    sheet = "fk_econfigid"
  )
  
  people <- DBI::dbReadTable(con, 'people')
  locations <- DBI::dbReadTable(con, 'locations')
  equipment <- DBI::dbReadTable(con, 'equipment')
  
  
  expect_true(
    all(people$pk_personid %in% personids$choices)
  )
  
  expect_true(
    all(locations$pk_locationid %in% locationids$choices)
  )
  
  expect_true(
    all(equipment$pk_equipmentid %in% equipmentids$choices)
  )

  
  unlink(fp)
  
  # clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
})

test_that("Sucessful spreadsheet per equip_type, including all.", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # delete equip_type from equipment table
  DBI::dbExecute(con,
                 statement = "UPDATE equipment SET equip_type = NULL;")
  
  
  # write spreadsheet
  
  expect_error(
    fp <- googleAppCreate(
      con = con,
      equip_type = 'camera'
    )
  )
  
  expect_no_error(
    fp <- googleAppCreate(
      con = con,
      equip_type = 'all'
    )
  )
  
  unlink(fp)

  # clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
})