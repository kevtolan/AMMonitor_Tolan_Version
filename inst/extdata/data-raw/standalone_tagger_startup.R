## ==================================================================
## if you do not have AMMonitor installed on your machine, run the
## following code in lines 5-11. You should only have to do this once.

if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_gitlab(
  repo = "vtcfwru/ammonitor",
  auth_token = Sys.getenv(
    "GITLAB_PAT"),
    host = "code.usgs.gov",
    build_vignettes = FALSE)
## ==================================================================



# Once AMMonitor is installed, launch the AMMonitor tagging app with the
# following code.
AMMonitor::launchApp()
