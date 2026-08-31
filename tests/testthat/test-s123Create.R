test_that("Error if database connection is invalid", {
  
  # Create an empty database object
  con <- ""
  
  # Expect error with bad connection
  expect_error(
    s123Create(
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
    fp <- s123Create(
      con = con,
      equip_type = 'camera'
    )
  )
  
  # check spreadsheet
  
  survey <- readxl::read_xlsx(fp,
                              sheet = 'survey')
  
  choices <- readxl::read_xlsx(fp,
                               sheet = 'choices')
  
  equipment <- DBI::dbReadTable(con, 'equipment')
  people <- DBI::dbReadTable(con, 'people')
  locations <- DBI::dbReadTable(con, 'locations')
  
  # expect all camera choices, people, and locations are in sheet
  expect_true(
    all(people$pk_personid %in% choices$name)
  )
  
  expect_true(
    all(locations$pk_locationid %in% choices$name)
  )
  
  expect_true(
    all(
      equipment$pk_equipmentid[
        which(equipment$equip_type == 'camera')
      ]
      %in% choices$name)
  )
  
  expect_false(
    any(
      equipment$pk_equipmentid[
        which(equipment$equip_type == 'recorder')
      ]
      %in% choices$name)
  )
  
  unlink(fp)
  
  
  
  # recorder ====
  
  # write spreadsheet
  
  expect_no_error(
    fp <- s123Create(
      con = con,
      equip_type = 'recorder'
    )
  )
  
  # check spreadsheet
  
  survey <- readxl::read_xlsx(fp,
                              sheet = 'survey')
  
  choices <- readxl::read_xlsx(fp,
                               sheet = 'choices')
  
  equipment <- DBI::dbReadTable(con, 'equipment')
  people <- DBI::dbReadTable(con, 'people')
  locations <- DBI::dbReadTable(con, 'locations')
  
  # expect all camera choices, people, and locations are in sheet
  expect_true(
    all(people$pk_personid %in% choices$name)
  )
  
  expect_true(
    all(locations$pk_locationid %in% choices$name)
  )
  
  expect_true(
    all(
      equipment$pk_equipmentid[
        which(equipment$equip_type == 'recorder')
      ]
      %in% choices$name)
  )
  
  expect_false(
    any(
      equipment$pk_equipmentid[
        which(equipment$equip_type == 'camera')
      ]
      %in% choices$name)
  )
  
  unlink(fp)
  
  # all ====
  
  # write spreadsheet
  
  expect_no_error(
    fp <- s123Create(
      con = con,
      equip_type = 'all'
    )
  )
  
  # check spreadsheet
  
  survey <- readxl::read_xlsx(fp,
                              sheet = 'survey')
  
  choices <- readxl::read_xlsx(fp,
                               sheet = 'choices')
  
  equipment <- DBI::dbReadTable(con, 'equipment')
  people <- DBI::dbReadTable(con, 'people')
  locations <- DBI::dbReadTable(con, 'locations')
  
  # expect all camera choices, people, and locations are in sheet
  expect_true(
    all(people$pk_personid %in% choices$name)
  )
  
  expect_true(
    all(locations$pk_locationid %in% choices$name)
  )
  
  expect_true(
    all(
      equipment$pk_equipmentid %in% choices$name)
  )
  
  
  unlink(fp)
  
  # clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})


test_that("Sucessful spreadsheet writing with notes and reminders", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  
  expect_no_error(
    fp <- s123Create(
      con = con,
      equip_type = 'all',
      reminders = "Remember to check the batteries!",
      notes = "Example note"
    )
  )
  
  # check spreadsheet
  
  survey <- readxl::read_xlsx(fp,
                              sheet = 'survey')
  
  choices <- readxl::read_xlsx(fp,
                               sheet = 'choices')
  
  equipment <- DBI::dbReadTable(con, 'equipment')
  people <- DBI::dbReadTable(con, 'people')
  locations <- DBI::dbReadTable(con, 'locations')
  
  # expect all camera choices, people, and locations are in sheet
  expect_true(
    all(people$pk_personid %in% choices$name)
  )
  
  expect_true(
    all(locations$pk_locationid %in% choices$name)
  )
  
  expect_true(
    all(
      equipment$pk_equipmentid %in% choices$name)
  )
  
  expect_true(
    survey$label[
      which(survey$type == 'note')
    ] == "Example note"
  )
  
  expect_true(
    choices$label[
      which(choices$list_name == 'list_reminders')
    ] == "Remember to check the batteries!"
  )
  
  
  
  unlink(fp)
  
  
  # try multiple reminders
  expect_no_error(
    fp <- s123Create(
      con = con,
      equip_type = 'all',
      reminders = c("1", "2", "3" ,"4", "5"),
      notes = "Example note"
    )
  )
  
  
  choices <- readxl::read_xlsx(fp,
                               sheet = 'choices')
  
  expect_true(
    nrow(
      choices[
        which(choices$list_name == 'list_reminders'),
      ]
    ) == 5
  )
  
  unlink(fp)
  
  # clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  
})

