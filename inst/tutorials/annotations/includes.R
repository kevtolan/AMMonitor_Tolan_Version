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

annotations <- DBI::dbReadTable(conx, "annotations")
media <- DBI::dbReadTable(conx, "media")
medialists <- DBI::dbReadTable(conx, "medialists")
librarylists <- DBI::dbReadTable(conx, "librarylists")
librarylistitems <- DBI::dbReadTable(conx, "librarylistitems")

annotation_ids <- 26:27
