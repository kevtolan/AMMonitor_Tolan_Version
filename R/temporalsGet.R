#' @name temporalsGet
#' @aliases temporalsGet
#' @title Retrieves and formats temporal data
#' @description This function retrieves temporal data from the National Climate 
#' Data Center (NCDC) or other specified sources.
#' @param amm_fp  Filepath to the AMMonitor project directory.  Default is getwd().
#' @param con A connection to the database
#' @param startDate Temporal data retrieved includes this date, must be a string 
#' in yyyy-mm-dd format
#' @param endDate Temporal data retrieved includes this date, must be a string in 
#' yyyy-mm-dd format
#' @param token Token obtained from 
#'   \url{https://www.ncdc.noaa.gov/cdo-web/token}.  Options are "settings" or
#'   "prompt".  Default value is "prompt", which prompts a token entry in the 
#'   R console. If the token is stored
#'   in the "settings" directory of your AMMonitor project as a text file,
#'   enter "settings" for this argument. See details below.
#' @param temporalSource Source of temporal data, currently only supports "noaa"
#' @param stationCodes List of noaa weather station codes to retrieve temporal 
#' data for, if null, will retrieve stations from the database
#' @param type Type of temporal data to retrieve, either "historical" or "forecast". 
#' NOAA only provides historical data
#' @param keys Vector of variable names indicating which types of data to retrieve.
#'  e.g. "TMAX" for maximum temperature, "TMIN" for minimum temperature, "PRCP" 
#'  for precipitation, etc
#' @param dbInsert Whether or not to write these locations to the database
#' @param disconnect  TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @details This function can retrieve daily weather data from the
#' Global Historical Climatology Network (GHCN) using the REST API provided by NOAA.
#' Due to restrictions in the API, data can only be retrieved from a maximum time 
#' frame of one (1) year,
#' though we recommend breaking up time frames into shorter periods. There are no 
#' restrictions
#' on the number of stations that can be entered at once. Weather data from stations
#' can be retrieved by their station codes only. Please run \code{locationsGetStations()}
#' to retrieve station codes, and insert them in the database if planning to 
#' insert weather data into the database.
#'
#' Different types of weather data can also be specified through the weatherType
#' argument by their abbreviated names (e.g. "TMAX" for maximum temperature,
#' "TMIN" for minimum temperature, etc.). By default, maximum temperature,
#'  minimum
#' temperature, precipitation, snowfall, and snow depth data are retrieved.
#' A complete list of weather data types, their abbreviations, and units can be 
#' found here:
#'
#' \url{https://docs.opendata.aws/noaa-ghcn-pds/readme.html}
#' 
#' Please see the "temporals" tutorial for more information:
#' 
#' \code{learnr::run_tutorial(name = "temporals", package = "AMMonitor")}
#' @importFrom DBI dbIsValid dbConnect dbSendQuery dbClearResult dbGetQuery 
#' dbDisconnect dbAppendTable
#' @importFrom httr2 request req_headers req_dry_run req_perform resp_status 
#' resp_body_json req_throttle
#' @importFrom data.table rbindlist
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @usage temporalsGet(amm_fp, con, startDate, endDate, token,
#'    temporalSource = "noaa", stationCodes = NULL, type = "historical",
#'    keys = c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"), dbInsert = FALSE,
#'    disconnect = FALSE)
#' @family data
#' @keywords data
#' @export
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # look at the locations table; we are going to add weather stations
#' DBI::dbReadTable(conx, "locations")
#'   
#' # get the weather stations with dbInsert = TRUE
#' stations <- locationsGetStations(
#'   amm_fp = demo_fp,
#'   conx,
#'   noaa_token = "prompt",
#'   startDate = NULL,
#'   endDate = NULL,
#'   bbox =  c(43.15, -73, 43.5, -72.6),  # minlat, minlong, maxlat, maxlong
#'   dbInsert = TRUE,
#'   disconnect = FALSE)
#'  
#' # look at the stations
#' str(stations)
#'  
#' # get weather associated with stations
#' temporal_data <- temporalsGet(
#'   amm_fp  = demo_fp,
#'   con = conx, 
#'   startDate = "2023-01-01",
#'   endDate = "2023-12-31",
#'   token = "prompt",
#'   temporalSource = "noaa",
#'   stationCodes = stations$pk_locationid,
#'   type = "historical",
#'   keys = c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"),
#'   dbInsert = FALSE,
#'   disconnect = FALSE
#'  )
#'  
#' # look at the retrieved data
#' head(temporal_data)
#'  
#' # insert to database if appropriate
#' DBI::dbAppendTable(conx, name = "temporals", value = temporal_data)
#'  
#' # look at the data in the temporals table
#' DBI::dbReadTable(conx, name = "temporals")
#'  
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'  
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' 
#'}

temporalsGet <- function(
    amm_fp = getwd(),
    con = NULL,
    startDate,
    endDate,
    token = "prompt",
    temporalSource = "noaa",
    stationCodes = NULL,
    type = "historical",
    keys = c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"),
    dbInsert = FALSE,
    disconnect = FALSE
) {

  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # check if not noaa
  if (temporalSource != "noaa") stop("Only the NOAA weather source is implemented
      at this time.")
  
  # get station codes if not provided
  if (is.null(stationCodes)) {
    stationCodes <- DBI::dbGetQuery(
      con, 
      statement = "SELECT pk_locationid from locations WHERE location_type = 'weather_station';")[,1, drop = TRUE]
    
  } else {
    # check that station codes provided are in the database
    if (dbInsert == TRUE) {
      db_locs <- DBI::dbGetQuery(
        con, 
        "SELECT pk_locationid from locations WHERE location_type = 'weather_station';")[,1, drop = TRUE]
      if (!all(stationCodes %in% db_locs)) stop("The station codes provided are not all 
          present in the database.")
    }
  }
  
  # Ensure date range does not exceed 365
  if (as.Date(endDate) - as.Date(startDate) > 365) {
    stop("The time frame cannot be greater than 1 year. Please select a shorter time frame.")
  }

  # get NOAA token 
  if (token == "settings")  {
    token = readLines(paste0(amm_fp, "/settings/noaa_token.txt"))
  } else {
    token <- readline("Enter your token here: ")  
  }

  # Get temporal data (progress bar bc long)
  cat("Gathering temporal data\n")
  # pb <- utils::txtProgressBar(min = 0, max = length(stationCodes))
  
  # create list to store results
  station_weather <- data.frame()
  
  # loop through stationCodes
  for (i in 1:length(stationCodes)) {
  
    message(paste("Extracting data for station ", i, " = ", stationCodes[i]))
    
    # Create URL for request
    url_string <- paste0(
      "https://www.ncei.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND",
      "&startdate=", startDate,
      "&enddate=", endDate,
      "&stationid=", stationCodes[i],
      "&", paste("datatypeid=", keys, collapse = "&", sep = ""),
      "&limit=1000"
    )
    
    # make the request - don't send more than 30 requests per minute
    req <- httr2::request(url_string) |> httr2::req_throttle(rate = 30 / 60)
    
    # add the headers
    req <- req |> httr2::req_headers(token = token)
    
    # test it out
    req |> httr2::req_dry_run()
    
    # perform the request
    response <-  httr2::req_perform(req)
    
    # retrieve data
    if (httr2::resp_status(response) != 200) {
    
      # could do more here
      print(paste0("Error retrieving weather data. Error code: ", resp_status(response)))
      next
    } else {
      
      # get the response contents
      rs_content <- response |> resp_body_json()
      
      if (length(rs_content) != 2) {
        message(paste0("No weather data found for station ",  stationCodes[i]))
        next
        
      } else {
        # extract the data
        weather <- rs_content[[2]]
        # date datatype station attributes value
        weather_df <- as.data.frame(data.table::rbindlist(weather, fill = TRUE))
        
        # update results
        station_weather <- rbind(station_weather, weather_df)
        # utils::setTxtProgressBar(pb, value = i)
      }  # end of weather data i
        
    } # end of non-error
    
  } # end of location i
  
  
  # close(pb)
  
  #sw_df <- as.data.frame(data.table::rbindlist(station_weather))
  sw_df <- station_weather
  sw_df$temporalTime <- gsub("(.*)T(\\d+:\\d+:\\d+)", "\\2", sw_df$date)
  sw_df$temporalDate <- gsub("(.*)T(\\d+:\\d+:\\d+)", "\\1", sw_df$date)
  
  # get temporalistitems
  listitems <- DBI::dbGetQuery(
    con,
    "SELECT * from temporallistitems 
    WHERE fk_temporallistid = 'noaa';"
  )
  
  merged_df <- merge(
    x = sw_df,
    y = listitems,
    by.x = "datatype",
    by.y = "item"
  )
  
  result <- data.frame(
    pk_temporalid = NA,
    fk_locationid = merged_df$station, 
    temporal_type = type, 
    temporal_date = merged_df$temporalDate, 
    temporal_time = merged_df$temporalTime, 
    fk_temporallistitemid = merged_df$pk_temporallistitemid,
    temporal_value_num = merged_df$value
  )
  
  if (dbInsert == TRUE) {
    DBI::dbAppendTable(con, 'temporals', result)
    message(paste(nrow(result), " rows of temporal data added to the database."))
  }

  return(result)
}
