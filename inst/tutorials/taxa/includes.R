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
  WHERE pk_tablename = 'visits'
  ORDER by sort_order;")

visits <- DBI::dbReadTable(conx, "visits")

new_column <-  data.frame(
  pk_tablename = "visits",
  pk_fieldname = "air_temperature",
  core = 0,
  var_type = "REAL",
  not_null_clause = NA_character_,
  default_value = NA_character_,
  max = -10,
  min = 40,
  fk_listid = NA_character_,
  description = "Air temperature at time of visit in Celsius")



