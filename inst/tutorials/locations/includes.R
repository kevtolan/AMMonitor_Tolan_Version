# load required packages
library(learnr)
library(AMMonitor)
library(DBI)
library(leaflet)

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

dictionary <- DBI::dbGetQuery(conx, "SELECT * from dbdictionary 
                              WHERE pk_tablename = 'locations'
                              ORDER by sort_order;")

listitems <- DBI::dbGetQuery(conx, "SELECT * FROM listitems
 WHERE (listitems.fk_listid='location_type' OR listitems.fk_listid='spatial_geometry' OR listitems.fk_listid='location_status' OR listitems.fk_listid='datum');
")


locations <- DBI::dbReadTable(conx, "locations")

#on.exit(DBI::dbDisconnect(conx))

