#' @name md_outputs
#' @title Sample outputs from MegaDector model run
#' @docType data
#' @description Model outputs from an external analysis of photos by MegaDector
#' \itemize{
#'   \item pk_modeloutputid Default is NA (will be given number when imported)
#'   \item fk_mediaid References pk_mediaid in media table
#'   \item fk_modelid References pk_modelid in the models table
#'   \item fk_parentid References a parent modeloutput id from this table.
#'   \item fk_mconfignameid The model configuration used to obtain the output.
#'   \item fk_taxonid References a primary key  from the taxa table.
#'   \item fk_librarylistitemid References a primary key in the librarylistitem
#'     table.
#'   \item fk_medialistitemid References a primary key in the medialistitems 
#'     table.
#'   \item x_min Bounding box x minimum
#'   \item x_max Bounding box x maximum
#'   \item y_min Bounding box y minimum
#'   \item y_max Bounding box y maximum
#'   \item value_num Value returned by megadetector (closeness score)
#'   \item timestamp Timestamp of record entry into the database
#' }
#' @keywords datasets
#' @usage data(md_outputs)
#' @format A data frame with 33 rows and 14 variables
NULL
#' @name bn_outputs
#' @title Sample outputs from a BirdNet model run
#' @docType data
#' @description Model outputs from an external analysis of recordings by BirdNet
#' \itemize{
#'   \item pk_modeloutputid Default is NA (will be given number when imported)
#'   \item fk_mediaid References pk_mediaid in media table
#'   \item fk_modelid References pk_modelid in the models table
#'   \item fk_parentid References a parent modeloutput id from this table.
#'   \item fk_mconfignameid The model configuration used to obtain the output.
#'   \item fk_taxonid References a primary key  from the taxa table.
#'   \item fk_librarylistitemid References a primary key in the librarylistitem
#'     table.
#'   \item fk_medialistitemid References a primary key in the medialistitems 
#'     table.
#'   \item x_min Bounding box x minimum
#'   \item x_max Bounding box x maximum
#'   \item y_min Bounding box y minimum
#'   \item y_max Bounding box y maximum
#'   \item value_num Value returned by BirdNet (closeness score)
#'   \item timestamp Timestamp of record entry into the database
#' }
#' @keywords datasets
#' @usage data(bn_outputs)
#' @format A data frame with 13 rows and 14 variables
NULL
#' @name taxa
#' @title Sample taxa table for ammCreateMiniDemo function
#' @docType data
#' @description Sample taxa table for ammCreateMiniDemo function
#' \itemize{
#'   \item pk_taxonid Default is NA (will be given number when imported)
#'   \item common_name References pk_mediaid in media table
#'   \item tsn xx
#'   \item taxon_rank xx.
#'   \item rank_subspecies xx.
#'   \item rank_species xx.
#'   \item rank_genus x
#'   \item rank_family x
#'   \item rank_order x
#'   \item rank_class x
#'   \item rank_phylum x
#'   \item rank_kingdom x
#'   \item notes x
#'   \item sensitive x
#'   \item taxa_image_url x
#'   \item taxa_url x
#' }
#' @keywords datasets
#' @usage data(taxa)
#' @format A data frame with 15 rows and 16 variables
NULL
#' @name stations
#' @title Sample weather stations for ammCreateMiniDemo
#' @docType data
#' @description Sample weather stations for ammCreateMiniDemo.
#' These stations were obtained with the `locationsGetStations()`
#' function for the demo.  The stations can be inserted into the 
#' locations table for illustration.  
#' \itemize{
#'   \item pk_locationid Unique name of weather station, no spaces.
#'   \item spatial_geometry Type of spatial geometry (point, line, polygon).  
#'   Acceptable entries are stored in the spatial_geometry list. 
#'   \item location_type Type of location (e.g., study area, monitoring station, 
#'   weather station, town). Here, "weather_station".
#'   \item fk_spatialid Foreign key to a spatial layer if the location is 
#'   represented by that layer (appropriate for study areas).
#'   \item lat Latitude (required for point geometries).
#'   \item long Longitude (required for point geometries).
#'   \item datum Datum - the model that represents the Earth. E.g. WGS84.  
#'   Acceptable entries are stored in the datum list (which can be added to).
#'   \item description Description of the location.
#'   \item x X coordinate if longitude is not provided for point locations.
#'   \item y Y coordinate if latitude is not provided for point locations.
#'   \item epsg EPSG code identifying the spatial attributes of the x and y 
#'   columns for point locations.  See https://epsg.org/home.html for 
#'   additional information.
#'   \item tz Olsen formatted timezone. See https://en.wikipedia.org/wiki
#'   /List_of_tz_database_time_zones.
#'   \item sensitive Is the location sensitive, such that it should not 
#'   included in data releases?
#'   \item long_min Minimum longitude for a bounding box, to be used for 
#'   obscuring the exact location of a monitoring station for publicly released 
#'   data.
#'   \item long_max Maximum longitude for a bounding box, to be used for 
#'   obscuring the exact location of a monitoring station for publicly released 
#'   data.
#'   \item lat_min Minimum latitude for a bounding box, to be used for obscuring 
#'   the exact location of a monitoring station for publicly released data.
#'   \item lat_max Maximum latitude for a bounding box, to be used for obscuring 
#'   the exact location of a monitoring station for publicly released data.
#'   \item location_status Status of location (e.g., active, retired, planned). 
#'    Acceptable entries are stored in the location_status list (which can be 
#'    modified but not deleted).
#' }
#' @keywords datasets
#' @usage data(stations)
#' @format A data frame with 14 rows and 18 variables
NULL
#' @name temporals
#' @title Sample weather data for ammCreateMiniDemo function
#' @docType data
#' @description Sample weather data for ammCreateMiniDemo function.
#' These data were obtained with the `temporalsGet()`
#' function for the demo.  
#' \itemize{
#'   \item pk_temporalid Autonumber identifying a given temporal record.
#'   \item fk_locationid Maps to a given pk_locationid from the locations table.
#'   \item temporal_type Type of temporal record; e.g, historic or forecast.  
#'   Acceptable entries are stored in the list named temporal_type.
#'   \item temporal_date Date of the the temporal record.
#'   \item temporal_time Time of the the temporal record.
#'   \item k_temporallistitemid Maps to a given pk_temporallistitem in the 
#'   temporallistitems table.  Typically provides the name of a temporal variable.
#'   \item temporal_value_num Numeric value associated with the referenced 
#'   fk_temporallistitemid (if applicable).
#' }
#' @keywords datasets
#' @usage data(stations)
#' @format A data frame with 4226 rows and 7 variables
NULL


