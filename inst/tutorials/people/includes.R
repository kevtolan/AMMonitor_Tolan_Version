# load required packages
library(learnr)
library(AMMonitor)
library(DBI)

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

dictionary <- DBI::dbGetQuery(conx, "SELECT * from dbdictionary WHERE pk_tablename = 'people';")

listitems <- DBI::dbReadTable(conx, "listitems")

people <- DBI::dbReadTable(conx, "people")

pippin <- data.frame(
  pk_personid = "pippin",
  first_name = "Peregrin",
  last_name = "Took",
  display_name = "breakfast"
)




#on.exit(DBI::dbDisconnect(conx))

