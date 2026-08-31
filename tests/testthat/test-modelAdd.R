test_that("Error if database connection isn't valid", {
  
  # Create an empty con object
  con <- ""
  
  # Expect an error with no connection
  expect_error(
    modelAdd(
      con = con,
      excel_fp = file.path(tempdir(), "modelAdd.xlsx"),
      disconnect = FALSE
    )
  )
  
})

test_that("Error if duplicate model name", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an "excel sheet" to write out with a duplicate model name
  excel_sheet <- data.frame(
    A = c("Model_Name", "MegaDetector", NA, "Setting_Name", "min_conf", "timelapse"),
    B = c("Description", "trail camera model", NA, "Setting_Description", 
          "minimum confidence", "are timelapse photos taken"),
    C = c("URL", "example URL", NA, NA, NA, NA),
    D = c("Citation", "example citation", NA, "Setting_Name", "timelapse", "timelapse"),
    E = c("Model_Type", "CNN", NA, "Setting_Option", "True", "False"),
    F = c("Model_Library", NA, NA, "Option_Description", "timelapse photos are taken",
          "timelapse photos are not taken")
  )
  
  # Write out the "example spreadsheet"
  writexl::write_xlsx(
    x = excel_sheet,
    path = file.path(tempdir(), "modelAdd.xlsx"),
    col_names = FALSE
  )
  
  # Expect error with duplicate name
  expect_error(
    modelAdd(
      con = con,
      excel_fp = file.path(tempdir(), "modelAdd.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAdd.xlsx"))
  
})

test_that("Successfully add model to the database", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an "excel sheet" to write out
  excel_sheet <- data.frame(
    A = c("Model_Name", "DeepFaune NE", NA, "Setting_Name", "min_conf", "timelapse"),
    B = c("Description", "trail camera model", NA, "Setting_Description", 
          "minimum confidence", "are timelapse photos taken"),
    C = c("URL", "example URL", NA, NA, NA, NA),
    D = c("Citation", "example citation", NA, "Setting_Name", "timelapse", "timelapse"),
    E = c("Model_Type", "CNN", NA, "Setting_Option", "True", "False"),
    F = c("Model_Library", NA, NA, "Option_Description", "timelapse photos are taken",
          "timelapse photos are not taken")
  )
  
  # Write out the "example spreadsheet"
  writexl::write_xlsx(
    x = excel_sheet,
    path = file.path(tempdir(), "modelAdd.xlsx"),
    col_names = FALSE
  )
  
  # Expect no error
  expect_no_error(
    modelAdd(
      con = con,
      excel_fp = file.path(tempdir(), "modelAdd.xlsx"),
      disconnect = FALSE
    )
  )
  
  # expect model from spreadsheet is added to the database
  
  # read in spreadsheet
  model_info <- readxl::read_excel(
    path = file.path(tempdir(), "modelAdd.xlsx"),
    range = "A1:F2"
  )
  
  setting_info <- readxl::read_excel(
    path = file.path(tempdir(), "modelAdd.xlsx"),
    range = "A4:B19"
  )
  
  setting_info <- na.omit(setting_info)
  
  # query database after updates
  models_table <- DBI::dbReadTable(con, 'models')
  msettingnames <- DBI::dbReadTable(con, 'msettingnames')
  msettingoptions <- DBI::dbReadTable(con, 'msettingoptions')
  
  expect_true(
    all(model_info$Model_Name %in% models_table$model_name)
  )
  
  expect_true(
    all(setting_info$Setting_Name %in% msettingnames$setting_name)
  )
  
  setting_ids <- msettingnames$pk_msettingnameid[
    which(msettingnames$setting_name %in% setting_info$Setting_Name) &
      msettingnames$fk_modelid == models_table$pk_modelid[
        which(models_table$model_name == model_info$Model_Name)
      ]
  ]
  
  # for this example, expect some options added
  expect_true(
    any(setting_ids %in% msettingoptions$fk_msettingnameid)
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAdd.xlsx"))
  
})