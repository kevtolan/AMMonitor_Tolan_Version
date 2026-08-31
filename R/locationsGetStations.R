#' @name locationsGetStations
#' @aliases locationsGetStations
#' @title Retrieves GHCN-D weather station codes based on search criteria
#' @description This function retrieves weather station codes from the NOAA API
#' based on user entered search criteria.
#' @details Weather stations can be searched using either a bounding box or 
#' based on locations currently in the database.
#' @param amm_fp  Filepath to the AMMonitor project directory.  Default is 
#' getwd().
#' @param con An open connection to the AMMonitor database
#' @param noaa_token Token obtained from 
#'   \url{https://www.ncdc.noaa.gov/cdo-web/token}.  Options are "settings" or
#'   "prompt".  Default value is "prompt", which prompts a token entry in the 
#'   R console. If the token is stored
#'   in the "settings" directory of your AMMonitor project as a text file,
#'   enter "settings" for this argument. See details below.
#' @param startDate Stations with data after this date will be retrieved,
#' must be a string in yyyy-mm-dd format
#' @param endDate Stations with data before this date will be retrieved, must
#' be a string in yyyy-mm-dd format
#' @param bbox NULL or a vector of numeric values representing a bounding box
#' to search for weather stations  
#' in the order east, north, south, west:  minimum longitude, maximum latitude,
#' minimum latitude,  maximum longitude.
#' If NULL, a bounding box inferred from the locations table of the database
#' will be used.
#' @param dbInsert TRUE or FALSE, indicating whether or not to write these 
#' locations to the database. Default is FALSE
#' @param disconnect  TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @return A dataframe containing weather stations contained within the bbox
#' @importFrom DBI dbIsValid dbAppendTable dbGetQuery dbDisconnect
#' @importFrom httr2 request req_headers req_dry_run req_perform resp_status 
#' resp_body_json
#' @importFrom data.table rbindlist
#' @details  Obtain a token from https://www.ncdc.noaa.gov/cdo-web/token.
#' Then, if desired, save this token string in a text file
#' named 'noaa_token.txt', within the "settings" directory of your AMMonitor
#' project.  Then, run \code{locationsGetStations()} and set the noaa_token 
#' argument to "settings"; the function will look for the token there.  If the 
#' noaa_token argument is "prompt", the function will prompt you
#' to paste in the token in your console.
#' 
#' This function will retrieve up to 1000 weather station locations at a 
#' maximum.  If the bbox area of interest is very large (containing more
#' than 1000 stations), set the bbox to a smaller area and run the function
#' again.  Some weather stations operate for a short time and are no longer in 
#' operation. Make sure to set the startDate and endDate to eliminate stations
#' no longer in operation, if desired.
#' 
#' See the temporals tutorial by running the code:
#' \code{learnr::run_tutorial(name = "temporals", package = "AMMonitor")}
#' @usage locationsGetStations(amm_fp = getwd(), con, noaa_token = "prompt", startDate = NULL,
#'    endDate = NULL, bbox = NULL, dbInsert = FALSE, disconnect = FALSE)
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
#' # get the weather stations with dbInsert = FALSE - add results to db later
#' stations <- locationsGetStations(
#'   amm_fp = demo_fp,
#'   conx,
#'   noaa_token = "prompt",
#'   startDate = NULL,
#'   endDate = NULL,
#'   bbox =  c(43.15, -73, 43.5, -72.6),  # minlat, minlong, maxlat, maxlong
#'   dbInsert = FALSE,
#'   disconnect = FALSE)
#'  
#' # look at returned object
#' str(stations)
#' head(stations)
#'  
#' # append data if dbInsert was set to false
#' DBI::dbAppendTable(conx, name = "locations", value = stations)
#'  
#' # retrieve the locations table; look for new entries
#' locations <- DBI::dbReadTable(conx, name = "locations")
#' 
#' # prep data for plotting ---------------
#' indices <- stats::complete.cases(locations$lat, locations$long, locations$location_type)
#' locations_xy <- locations[indices, ]
#' 
#' # plot with ggplot2
#' ggplot2::ggplot(locations_xy,  aes(x = long, y = lat))  +
#'   geom_point(
#'     data = locations_xy, 
#'     aes(x = long, y = lat, shape = location_type, color = location_type), 
#'     size = 3) +
#'  coord_fixed() +
#'  labs(
#'    title = "Map of Locations",
#'    x = "Longitude",
#'    y = "Latitude") +
#'    theme_minimal()
#'    
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'  
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#' 
#' }
#' 

locationsGetStations <- function(amm_fp = getwd(), con, noaa_token = "prompt", startDate = NULL, endDate = NULL, bbox = NULL, dbInsert = FALSE, disconnect = FALSE) {
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }

  # get bounding box in which to look for stations
  if (is.null(bbox)) {
    # minlat, minlong, maxlat, maxlong
    bbox <- DBI::dbGetQuery(
      con,
      statement = "SELECT MIN(lat), MIN(long) , MAX(lat),  MAX(long) FROM locations;")
    bbox <- as.vector(bbox)
  } else {
    
    # add test to make sure bbox entries are in order
    # to do
  
  }
  
  # get NOAA token 
  if (noaa_token == "settings")  {
      noaa_token = readLines(paste0(amm_fp, "/settings/noaa_token.txt"))
    } else {
      noaa_token <- readline("Enter your token here: ")  
    }

  # create URL for request
  url_string <- paste0("https://www.ncei.noaa.gov/cdo-web/api/v2/stations?",
  "datasetid=GHCND",
  "&limit=1000",
  "&extent=", bbox[1], ",", bbox[2], ",", bbox[3], ",", bbox[4]
  )
  
  if (!is.null(startDate)) {url_string <- paste0(url_string, "&startdate=", startDate)}
  if (!is.null(endDate)) {url_string <- paste0(url_string, "&enddate=", endDate)}
  
  # make the request
  req <- httr2::request(url_string)
  
  # add the headers
  req <- req |> httr2::req_headers(token = noaa_token)
  
  # test it out
  req |> httr2::req_dry_run()
  
  # make the request
  response <- httr2::req_perform(req)
  
  # retrieve data
  if (httr2::resp_status(response) == 200) {
    
    rs_content <- response |> resp_body_json()
    
    if (length(rs_content) != 2) {
      stop(paste("No weather stations are in the selected area."))
    }
    
    # extract the data
    stations <- rs_content[[2]]
    stations_df <- as.data.frame(data.table::rbindlist(stations, fill = TRUE))
    message(paste0(nrow(stations_df), " stations have been found."))
    
  } else {
    print(paste0("Error retrieving weather stations. Error code: ", resp_status(response)))
  }

  # create a dataframe to append to locations table
  stations_df_final <- data.frame(
    pk_locationid = stations_df$id ,
    spatial_geometry = "point",
    location_type = "weather_station",
    fk_spatialid = NA,
    lat = stations_df$latitude,
    long = stations_df$longitude,
    datum = NA,
    description = paste0(
      "Data harvested from https://www.ncei.noaa.gov/cdo-web/api/v2/stations.  Name = ",
      stations_df$name, 
      ".  Elevation = " ,
      stations_df$elevation, " ", stations_df$elevationUnit,
      " Date range = ", stations_df$mindate, "-", stations_df$maxdate, "."
      ),
    x = NA,
    y = NA,
    epsg = NA,
    tz = NA,
    sensitive = 0,
    long_min = NA,
    long_max = NA,
    lat_min = NA,
    lat_max = NA,
    location_status = NA)


  # insert into database if specified
  if (dbInsert == TRUE) {
  
    # retrieve current stations
    current_stations <- dbGetQuery(con, statement = "SELECT pk_locationid FROM locations;")
    
    # remove duplicates
    indices <- which(stations_df_final$pk_locationid %in% current_stations$pk_locationid)
    if (length(indices) != 0) {
      stations_df_append <- stations_df_final[-indices,]
    } else {
      stations_df_append <- stations_df_final
    }
    
    new_stations <- DBI::dbAppendTable(con, name = 'locations', value = stations_df_append)

    print(paste(new_stations, "new weather stations were added to the database."))
  }

  return(stations_df_final)
}
