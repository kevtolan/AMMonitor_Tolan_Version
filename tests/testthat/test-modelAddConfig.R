test_that("Error if database connection isn't valid", {
  
  # Create an empty connection
  con <- ""
  
  # Try to add a config with an empty connection
  expect_error(
    modelAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "modelAddConfig.xlsx"),
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
  mconfigname <- data.frame(
    mconfigname = "MegaDetector default settings",
    fk_modelid = 5,
    description = "Description",
    filename = NA
  )
  
  # Create a valid configvalues sheet
  configvalues <- data.frame(
    Setting_Name = "confidence threshold",
    Description = "Minimum confidence",
    Option_Name = NA,
    Value = 0.1
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = mconfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "modelAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with duplicate name
  expect_error(
    modelAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "modelAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAddConfig.xlsx"))
  
})

test_that("Error if there's already a default config", {
  
  # get demo
  demo_fp <- paste0(find.package("AMMonitor",
                                 lib.loc = .libPaths()),
                    "/extdata/demoAMM/database/demo.sqlite")
  
  # copy to temp dir to edit
  file.copy(demo_fp, tempdir())
  
  new_demo_fp <- paste0(tempdir(), '/demo.sqlite')
  
  # set the connection
  con <- dbSetCon(new_demo_fp)
  
  # Create an econfigname dataframe as a default with an existing default
  mconfigname <- data.frame(
    mconfigname = "MegaDetector new default settings",
    fk_modelid = 5,
    description = "Description",
    filename = NA,
    is_default = 1
  )
  
  # Create a valid configvalues sheet
  configvalues <- data.frame(
    Setting_Name = "confidence threshold",
    Description = "Minimum confidence",
    Option_Name = NA,
    Value = 0.1
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = mconfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "modelAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with duplicated default
  expect_error(
    modelAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "modelAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAddConfig.xlsx"))
  
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
  mconfigname <- data.frame(
    mconfigname = "MegaDetector default settings",
    fk_modelid = 5,
    description = "Description",
    filename = NA
  )
  
  # Create a valid configvalues sheet
  configvalues <- data.frame(
    Setting_Name = "confidence threshold",
    Description = "Minimum confidence",
    Option_Name = NA,
    Value = 0.1
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = mconfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "modelAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with duplicate name
  expect_error(
    modelAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "modelAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAddConfig.xlsx"))
  
})

test_that("Error if setting name isn't listed for model", {
  
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
  mconfigname <- data.frame(
    mconfigname = "MegaDetector settings",
    fk_modelid = 5,
    description = "Description",
    filename = NA,
    is_default = 0
  )
  
  # Create a valid configvalues sheet with incorrect setting name
  configvalues <- data.frame(
    Setting_Name = "confidence_threshold",
    Description = "Minimum confidence",
    Option_Name = NA,
    Value = 0.1
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = mconfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "modelAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with incorrect setting
  expect_error(
    modelAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "modelAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAddConfig.xlsx"))
  
})

test_that("Error if option name isn't listed for model", {
  
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
  mconfigname <- data.frame(
    mconfigname = "MegaDetector settings",
    fk_modelid = 5,
    description = "Description",
    filename = NA,
    is_default = 0
  )
  
  # Create a valid configvalues sheet with incorrect option
  configvalues <- data.frame(
    Setting_Name = "confidence_threshold",
    Description = "Minimum confidence",
    Option_Name = "low",
    Value = NA
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = mconfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "modelAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Attempt to add with incorrect option
  expect_error(
    modelAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "modelAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAddConfig.xlsx"))
  
})

test_that("Successfully add configuration to the database", {
  
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
  mconfigname <- data.frame(
    mconfigname = "MegaDetector settings",
    fk_modelid = 5,
    description = "Description",
    filename = NA,
    is_default = 0
  )
  
  # Create a valid configvalues sheet with incorrect setting name
  configvalues <- data.frame(
    Setting_Name = "confidence threshold",
    Description = "Minimum confidence",
    Option_Name = NA,
    Value = 0.1
  )
  
  # Turn into a named list
  named_list <- list(
    Configuration_Name = mconfigname,
    Configuration_Values = configvalues
  )
  
  # Write out an excel file with each data frame as a sheet
  writexl::write_xlsx(
    x = named_list,
    path = file.path(tempdir(), "modelAddConfig.xlsx"),
    col_names = TRUE,
    format_headers = TRUE
  )
  
  # Get the number of configuration prior to adding the config
  configs_pre_add <- nrow(DBI::dbReadTable(con, "mconfignames"))
  
  # Attempt to add 
  expect_no_error(
    modelAddConfig(
      con = con,
      excel_fp = file.path(tempdir(), "modelAddConfig.xlsx"),
      disconnect = FALSE
    )
  )
  
  # Get the number post adding the configuration
  configs_post_add <- nrow(DBI::dbReadTable(con, "mconfignames"))
  
  # Expect a longer table
  expect_true(configs_post_add > configs_pre_add)
  
  # Clean up
  DBI::dbDisconnect(conn = con)
  unlink(paste0(tempdir(), '/demo.sqlite'))
  unlink(paste0(tempdir(), "/modelAddConfig.xlsx"))
  
})