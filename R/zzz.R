# @name .onAttach
# @title Launches learnr and returns startup message
# @description Startup function.
# @param ...  Default arguments of function
# @importFrom utils packageDate packageVersion installed.packages
# @importFrom learnr random_praise

.onAttach = function(...) {
  msg <-  paste0(
    'AMMonitor ',utils::packageVersion('AMMonitor'), '\n\n',
    'Just getting started? To begin the AMMonitor tutorials, enter \n',
    'learnr::run_tutorial("intro", package = "AMMonitor") in console to start,\n',
    'or click the "intro" tutorial in the tutorial pane in Rstudio.\n'
  )

 packageStartupMessage(msg)


}
