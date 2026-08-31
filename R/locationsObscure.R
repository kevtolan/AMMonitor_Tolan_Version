#' @name locationsObscure
#' @title Add a random perturbance to all locations for public display
#' @description For any location in the locations table with spatial_geometry of
#' "point", the function will jiggle the point location by a random x,y 
#' coordinate and will center a bounding box around the jiggled point.
#' @param con An open database connection
#' @param bbLength Length of each edge of the bounding box (in Decimal Degrees).
#' Default value of 0.02 equates to a bounding box with length ~2.2 km
#' @param overwrite TRUE or FALSE. Whether or not to overwrite the previous 
#' bounding box coordinates. Default is FALSE
#' @param disconnect  TRUE or FALSE. Should the database connection be severed
#' on exit? Default is FALSE
#' @usage locationsObscure(con, bbLength = 0.02, overwrite = FALSE, 
#' disconnect = FALSE)
#' @importFrom DBI dbGetQuery dbExecute dbDisconnect dbIsValid
#' @importFrom stats runif
#' @return Returns the number of rows from the locations table that have been 
#' updated
#' @export
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor project in a temporary directory
#' # (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # look at the folders within an AMMonitor project
#' list.files(demo_fp, recursive = FALSE)
#' 
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#' 
#' # look at the locations table; note the missing bounding boxes (long/lat min/max)
#' DBI::dbReadTable(conx, "locations")
#' 
#' # add obscured locations for those that require, with default size bounding box
#' locationsObscure(
#'   con = conx,
#'   bbLength = 0.02,
#'   overwrite = TRUE,
#'   disconnect = FALSE
#' )
#' 
#' # look again at the locations table to see the obscured bboxes
#' locs <- DBI::dbReadTable(conx, "locations")
#' 
#' locs
#' 
#' # remove locations that don't have bboxes (e.g. unknownLocation)
#' locs <- locs[-which(is.na(locs$lat) | is.na(locs$lat)), ]
#'  
#' # plot the location bounding boxes; any messages pertain
#' ggplot2::ggplot(locs) +
#'   geom_rect(
#'      aes(
#'        xmin = long_min, 
#'        xmax = long_max, 
#'        ymin = lat_min, 
#'        ymax = lat_max), 
#'      color = "blue", 
#'      fill = NA) +
#'   theme_minimal()
#'   
#' }  

locationsObscure <- function(con, bbLength = 0.02, overwrite = FALSE, disconnect = FALSE){
  
  # check if valid connection
  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")
  
  # disconnect on exit if requested
  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }

  # Get locations that need obscuring (select all if overwrite == TRUE)
  if (overwrite) {
      locs_to_obscure <- DBI::dbGetQuery(con, 'SELECT * FROM locations')
    } else { 
      locs_to_obscure <- DBI::dbGetQuery(
       con,
       statement = "SELECT * FROM locations WHERE long_min IS NULL;")
  }
  
  # Can't obscure if no lat/long
  locs_to_obscure <- locs_to_obscure[!is.na(locs_to_obscure$lat) & !is.na(locs_to_obscure$long),]
  
  # Create and add the bounding boxes for the obscured location
  if (nrow(locs_to_obscure) != 0) {
    # Make a "centroid" for the bounding box
    perturbed_lat <- locs_to_obscure$lat +
      stats::runif(nrow(locs_to_obscure), min = -bbLength, max = bbLength)
    perturbed_long <- locs_to_obscure$long +
      stats::runif(nrow(locs_to_obscure), min = -bbLength, max = bbLength)
    
    # Get the corners of the bounding box, based on the "centroid"
    locs_to_obscure$long_min <- perturbed_long - bbLength
    locs_to_obscure$long_max <- perturbed_long + bbLength
    locs_to_obscure$lat_min <- perturbed_lat - bbLength
    locs_to_obscure$lat_max <- perturbed_lat + bbLength
    
    # Add each bounding box to the database
    for (i in 1:nrow(locs_to_obscure)) {
      rs <- DBI::dbExecute(
        con,
        statement = paste0(
          "UPDATE locations SET ",
          "long_min = ", locs_to_obscure$long_min[i], ", ",
          "long_max = ", locs_to_obscure$long_max[i], ", ",
          "lat_min = ", locs_to_obscure$lat_min[i], ", ",
          "lat_max = ", locs_to_obscure$lat_max[i], " ",
          'WHERE pk_locationID = "', locs_to_obscure$pk_locationid[i], '";'
        )
      )
    }
  }
  
  # return result
  message(paste0("Bounding boxes around perturbed location coordinates have 
                 been added to ", nrow(locs_to_obscure), " locations."))
  return(nrow(locs_to_obscure))
}
