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

results <- dbGetSummaryData(
  con = conx,
  m_dates = c("2023-05-01", "2023-12-31"),
  disconnect = FALSE
)

plot_results <- dbPlotSummary(
  summary_data = results,
  num_bins = 15,
  map_database = "state", 
  regions = "vermont",
  key_species = c("moose", "black bear")
)

table_results <- dbTableSummary(
  summary_data = results,
  key_species = c("moose", "black bear")
)

