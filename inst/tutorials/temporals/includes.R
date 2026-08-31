# load required packages
library(learnr)
library(AMMonitor)
library(DBI)
library(ggplot2)

knitr::opts_chunk$set(echo = TRUE, class.source = "bg-success", comment = '##')

temp_fp <- tempdir()

file.copy(from = paste0(find.package("AMMonitor"), "/extdata/demoAMM"), to = temp_fp, recursive = TRUE, overwrite = TRUE)

# audio_path.txt
writeLines(
  text = file.path(temp_fp, "demoAMM/recordings/"),
  con = file(file.path(temp_fp, "demoAMM/settings/audio_path.txt"))
)

# image_path.txt
writeLines(
  text = file.path(temp_fp, "demoAMM/photos/"), 
  con = file(file.path(temp_fp, "demoAMM/settings/image_path.txt"))
)

demo_fp <- paste0(temp_fp, "/demoAMM")

conx <- dbSetCon(paste0(demo_fp, "/database/demo.sqlite"))

dictionary <- DBI::dbGetQuery(
  conx, 
  "SELECT * from dbdictionary 
  WHERE pk_tablename = 'temporals'
  ORDER by sort_order;")

lists <- DBI::dbReadTable(conx, "temporallists")

listitems <- DBI::dbReadTable(conx, "temporallistitems")

locations <- DBI::dbReadTable(conx, "locations")

test <- DBI::dbReadTable(conx, "temporals")

# stations <- locationsGetStations(
#   amm_fp = demo_fp,
#   conx,
#   noaa_token = "settings",
#   startDate = NULL,
#   endDate = NULL,
#   # minlat, minlong, maxlat, maxlong
#   bbox =  c(43.15, -73, 43.5, -72.6),  
#   dbInsert = TRUE,
#   disconnect = FALSE
# )

data(stations)
DBI::dbAppendTable(conx, name = "locations", value = stations)



# get weather associated with stations
# temporal_data <- temporalsGet(
#   amm_fp = demo_fp,
#   con = conx,
#   startDate = "2023-01-01",
#   endDate = "2023-12-31",
#   token = "settings",
#   temporalSource = "noaa",
#   stationCodes = stations$pk_locationid,
#   type = "historical",
#   keys = c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"),
#   dbInsert = FALSE,
#   disconnect = FALSE
# )

data(temporals)
temporal_data <- temporals

DBI::dbAppendTable(conx, name = "temporals", value = temporal_data)


#on.exit(DBI::dbDisconnect(conx))

