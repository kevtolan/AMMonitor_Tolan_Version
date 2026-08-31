
  
    # load AMMonitor
    library('AMMonitor')
  
    # set a filepath to the AMMonitor database
    dbPath <- file.choose()
    
    # set the connection to database
    con <- dbSetCon(dbPath)
    
    # run dbCheckup to ensure dbdictionary actually matches the database schema
    checkup <- dbCheckup(con)
    
    lapply(checkup, FUN = head)
    
    # check the dictionary core columns against the default AMMonitor database
    core_test <- dbCheckCore(con)
    
    lappy(core_test, FUN = head)
    
    # check to see if problems may exist with data
    data_check <- dbCheckData(con)
    
    lapply(data_check, FUN = head)
    
    # get a summary of the database
    summary_data <- dbGetSummaryData(con)
    
    # obtain plots that summarize the data
    plot_results <- dbPlotSummary(summary_data)
    
    # obtain tables that summarize the data
    table_summary <- dbTableSummary(summary_data)
    
