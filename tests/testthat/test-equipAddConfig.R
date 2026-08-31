test_that("Error if database connection isn't valid", {
  
  # Create an empty connection
  con <- ""
  
  # Try to add a config with an empty connection
  expect_error(
    equipAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "equipAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
})

test_that("Error if configuration name is already in use", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an econfigname dataframe with a name in use
  econfigname <- data.frame(
    econfigname = "default recorder settings",
    fk_equipmodelid = "recorder_model_x",
    description = "Description",
    filename = NA
  )
  
  # Create a valid configvalues sheet
  configvalues <- data.frame(
    Setting_Name = c("sample rate", "max recording length", "gain"),
    Description = rep("Description", 3),
    Option_Name = c("24000Hz", NA, "18dB"),
    Value = c(NA, 60, NA),
    Valid_Options = rep(NA, 3)
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = econfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "equipAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with duplicate name
  expect_error(
    equipAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "equipAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipAddConfig.xlsx"))
  
})

test_that("Error if setting name doesn't match", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an econfigname dataframe
  econfigname <- data.frame(
    econfigname = "test settings",
    fk_equipmodelid = "recorder_model_x",
    description = "Description",
    filename = NA
  )
  
  # Create a configvalues sheet with an incorrect setting
  configvalues <- data.frame(
    Setting_Name = c("sample rate", "max_recording_length", "gain"),
    Description = rep("Description", 3),
    Option_Name = c("24000Hz", NA, "18dB"),
    Value = c(NA, 60, NA),
    Valid_Options = rep(NA, 3)
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = econfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "equipAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with duplicate name
  expect_error(
    equipAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "equipAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipAddConfig.xlsx"))
  
})

test_that("Error if option name doesn't match", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an econfigname dataframe
  econfigname <- data.frame(
    econfigname = "test settings",
    fk_equipmodelid = "recorder_model_x",
    description = "Description",
    filename = NA
  )
  
  # Create a configvalues sheet with an incorrect option
  configvalues <- data.frame(
    Setting_Name = c("sample rate", "max recording length", "gain"),
    Description = rep("Description", 3),
    Option_Name = c("24000", NA, "18dB"),
    Value = c(NA, 60, NA),
    Valid_Options = rep(NA, 3)
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = econfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "equipAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with duplicate name
  expect_error(
    equipAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "equipAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipAddConfig.xlsx"))
  
})

test_that("Equipment configuration added to database", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an econfigname dataframe
  econfigname <- data.frame(
    econfigname = "test settings",
    fk_equipmodelid = "recorder_model_x",
    description = "Description",
    filename = NA
  )
  
  # Create a valid configvalues sheet
  configvalues <- data.frame(
    Setting_Name = c("sample rate", "max recording length", "gain"),
    Description = rep("Description", 3),
    Option_Name = c("24000Hz", NA, "18dB"),
    Value = c(NA, 60, NA),
    Valid_Options = rep(NA, 3)
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = econfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "equipAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Check length of the econfignames table
  configs_pre_add <- nrow(DBI::dbReadTable(con, "econfignames"))
  
  # Attempt to add
  expect_no_error(
    equipAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "equipAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Get the number post adding the configuration
  configs_post_add <- nrow(DBI::dbReadTable(con, "econfignames"))
  
  # Expect a longer table
  expect_true(configs_post_add > configs_pre_add)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/equipAddConfig.xlsx"))
  
})