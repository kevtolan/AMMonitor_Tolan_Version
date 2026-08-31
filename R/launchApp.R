#' @name launchApp
#' @title Launch the Shiny app for AMMonitor
#' @aliases launch_app
#' @description Launches the main Shiny app for AMMonitor
#' @param amm_project_path Filepath to the outermost directory for an AMMonitor
#' project
#' @param ... Additional arguments to be passed to the app.
#' @usage launchApp(amm_project_path = getwd(), ...)
#' @importFrom shiny runApp
#' @keywords misc
#' @export
#' @examples
#' \dontrun{
#' 
#' # create a demo AMMonitor file structure in a temporary directory
#' # (to be deleted)
#' 
#' # run the function and capture the connection
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#' 
#' # look at the demo_fp
#' demo_fp
#' 
#' # launch the shiny app
#' launchApp(amm_project_path = demo_fp)
#'
#' # the database connection is automatically severed upon exit
#' 
#' 
#' }
#'
launchApp <- function(amm_project_path = getwd(), ...) {

  if (dir.exists(amm_project_path) == FALSE) stop("Could not find AMMonitor project path.")

  ammPath <<- amm_project_path

  shiny::runApp(paste(find.package('AMMonitor', lib.loc = .libPaths()), 'shiny', sep = '/'), ...)

}
