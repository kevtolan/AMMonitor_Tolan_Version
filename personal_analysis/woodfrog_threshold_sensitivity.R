## Wood Frog threshold sensitivity -- now just a thin wrapper around
## testThreshold(), which lives in myfunctions.R alongside the rest of the
## reusable helpers. Assumes `conx` is already an open RSQLite connection to
## VPMon_AMM.sqlite, as set up earlier in AMM_VPMon.R.

source("/Users/kevintolan/R/myfunctions.R")

result <- testThreshold(conx)

print(result$sensitivity, row.names = FALSE)
print(result$plot)
ggplot2::ggsave("woodfrog_threshold_sensitivity.png", result$plot, width = 7, height = 4.5, dpi = 150)
