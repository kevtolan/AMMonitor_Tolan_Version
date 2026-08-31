test_that("Error if database connection is invalid", {
  
  # Create an empty database object
  con <- ""
  
  # Expect error with bad connection
  expect_error(
    equipModelAdd(
      con = con,
      excel_fp = file.path(tempdir(), "equipModelAdd.xlsx"),
      disconnect = FALSE
    )
  )
  
})

test_that("Expect error if equipment model ID is already in use", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an "excel sheet" with a duplicate name
  excel_sheet <- data.frame(
    A = c("Equipment_Model_ID", "recorder_model_x", NA, "Setting_Name", "gain",
          "sample_rate"),
    B = c("Equip_Type", "recorder", NA, "Setting_Description", "Set gain",
          "Set sample rate"),
    C = c("Manufacturer", "Company", NA, NA, NA, NA),
    D = c("User_Manual", "Example URL", NA, "Setting_Name", "gain", "gain"),
    E = c(NA, NA, NA, "Setting_Option", "12dB", "18dB"),
    F = c(NA, NA, NA, "Option_Description", "gain of 12", "gain of 18")
  )
  
  # Write out the "example spreadsheet"
  writexl::write_xlsx(
    x = excel_sheet,
    path = file.path(tempdir(), "equipModelAdd.xlsx"),
    col_names = FALSE
  )
  
  # Expect error with duplicate name
  expect_error(
    equipModelAdd(
      con = con,
      excel_fp = file.path(tempdir(), "equipModelAdd.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipModelAdd.xlsx"))
  
})

test_that("Expect error if equipment type is invalid", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an "excel sheet" with an invalid equip type
  excel_sheet <- data.frame(
    A = c("Equipment_Model_ID", "recorder_model_z", NA, "Setting_Name", "gain",
          "sample_rate"),
    B = c("Equip_Type", "ARU", NA, "Setting_Description", "Set gain",
          "Set sample rate"),
    C = c("Manufacturer", "Company", NA, NA, NA, NA),
    D = c("User_Manual", "Example URL", NA, "Setting_Name", "gain", "gain"),
    E = c(NA, NA, NA, "Setting_Option", "12dB", "18dB"),
    F = c(NA, NA, NA, "Option_Description", "gain of 12", "gain of 18")
  )
  
  # Write out the "example spreadsheet"
  writexl::write_xlsx(
    x = excel_sheet,
    path = file.path(tempdir(), "equipModelAdd.xlsx"),
    col_names = FALSE
  )
  
  # Expect error with equipment type
  expect_error(
    equipModelAdd(
      con = con,
      excel_fp = file.path(tempdir(), "equipModelAdd.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipModelAdd.xlsx"))
  
})

test_that("Successfully add equipment model", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an "excel sheet"
  excel_sheet <- data.frame(
    A = c("Equipment_Model_ID", "recorder_model_z", NA, "Setting_Name", "gain",
          "sample_rate"),
    B = c("Equip_Type", "recorder", NA, "Setting_Description", "Set gain",
          "Set sample rate"),
    C = c("Manufacturer", "Company", NA, NA, NA, NA),
    D = c("User_Manual", "Example URL", NA, "Setting_Name", "gain", "gain"),
    E = c(NA, NA, NA, "Setting_Option", "12dB", "18dB"),
    F = c(NA, NA, NA, "Option_Description", "gain of 12", "gain of 18")
  )
  
  # Write out the "example spreadsheet"
  writexl::write_xlsx(
    x = excel_sheet,
    path = file.path(tempdir(), "equipModelAdd.xlsx"),
    col_names = FALSE
  )
  
  # Expect no error and add equipment model
  expect_no_error(
    equipModelAdd(
      con = con,
      excel_fp = file.path(tempdir(), "equipModelAdd.xlsx"),
      disconnect = FALSE
    )
  )
  
  # check that database tables are updated successfully
  # Read in the tables from the Excel spreadsheet
  equip_info <- readxl::read_excel(
    path = file.path(tempdir(), "equipModelAdd.xlsx"),
    range = "A1:D2"
  )
  
  setting_info <- readxl::read_excel(
    path = file.path(tempdir(), "equipModelAdd.xlsx"),
    range = "A4:B19"
  )
  
  setting_info <- na.omit(setting_info)
  
  option_info <- readxl::read_excel(
    path = file.path(tempdir(), "equipModelAdd.xlsx"),
    range = "D4:F19"
  )
  
  option_info <- na.omit(option_info)
  
  
  # get db tables
  equipmodels <- DBI::dbReadTable(con, 'equipmodels')
  esettingnames <- DBI::dbReadTable(con, 'esettingnames')
  esettingoptions <- DBI::dbReadTable(con, 'esettingoptions')
  
  expect_true(
    all(equip_info$Equipment_Model_ID %in% equipmodels$pk_equipmodelid)
  )
  
  expect_true(
    all(setting_info$Setting_Name %in% esettingnames$setting_name)
  )
  
  # get setting id for new option
  setting_id <- esettingnames$pk_esettingnameid[
    which(esettingnames$setting_name %in% option_info$Setting_Name &
            esettingnames$fk_equipmodelid %in% equip_info$Equipment_Model_ID)
  ]
  
  expect_true(
    all(setting_id %in% esettingoptions$fk_esettingnameid)
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipModelAdd.xlsx"))
  
})