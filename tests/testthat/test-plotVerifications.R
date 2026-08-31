test_that("plotVerifications returns expected modeloutputids", {
  
  # Set random seed
  set.seed(42)
  
  # copy the demo database
  demo_fp <- tempdir()
  file.copy(
    from = paste0(
      find.package("AMMonitor", lib.loc = .libPaths()),
      "/extdata/demoAMM/"
    ),
    to = demo_fp, 
    recursive = TRUE
  )
  
  # set connection then disconnect
  conx <- AMMonitor::dbSetCon(file.path(demo_fp, "demoAMM/database/demo.sqlite"))
  
  # Add some additional BirdNET verifications (let's make this more interesting)
  new_modelverifications <- data.frame(
    fk_modeloutputid = c(45, 52, 46),
    fk_personid = c("sgamgee", "sgamgee", "sgamgee"),
    is_valid = c(1, 0, 1)
  )
  DBI::dbAppendTable(conx, "modelverifications", new_modelverifications)
  
  # Plot the verifications (plot will be displayed)
  modeloutput_ids <- AMMonitor::plotVerifications(
    con = conx,
    model_id = 6,
    taxon_id = "oven",
    plot_modeloutput_id = TRUE,
    amm_project_path = file.path(demo_fp, "demoAMM")
  )
  
  # Verify the expected modeloutputs are returned
  expect_true(all(sort(modeloutput_ids) == c(45, 46, 48, 52)))
  
  # disconnect from the database when finished
  DBI::dbDisconnect(conx)
  
  # remove the demo AMMonitor file structure
  unlink(file.path(demo_fp, "demoAMM"), recursive = TRUE)
})
