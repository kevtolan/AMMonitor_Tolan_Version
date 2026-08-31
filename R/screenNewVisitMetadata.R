#' @name screenNewVisitMetadata
#' @title Check new visits for potential errors before they are added to an
#' AMMonitor database
#' @description Runs a set of checks on new visit metadata and compares it to
#' existing database records to check for errors and offers possible solutions
#' to errors found
#' @details
#' This function runs a set of basic checks on the metadata of a new visit to
#' be added to the database to ensure there aren't any logic errors or other
#' issues. These checks include:
#'
#' \itemize{
#'  \item Are all of the required fields for a new visit (fk_locationid,
#'  fk_equipmentid, fk_personid, and visit_type) provided?
#'  \item Does the visit already exist in the database?
#'  \item If the visit type is "set," is the equipment already currently deployed?
#'  \item If the visit type is "check" or "pull," is there a matching "set" or
#'  "check" visit in the database?
#'  }
#'
#' The function will then return a list containing a status and a data frame of
#' warnings (if any). A status of 1 indicates that the visit could be added to
#' the database; a status of 0 indicates a major error that would prevent the
#' visit from being added. The warnings data frame includes the warning message,
#' a possible solution, and a severity level. It is recommended that all errors
#' are resolved before the visit is added to the database.
#'
#' @param con An open connection to an AMMonitor database
#' @param visitMetadata A data frame containing the metadata of the new visit.
#' Columns should be named to match the AMMonitor "visits" table.
#' @param disconnect TRUE or FALSE. Should the database connection be severed on
#'  exit? Default is FALSE.
#' @usage screenNewVisitMetadata(con, visitMetadata = NA, disconnect = FALSE)
#' @export
#' @return A list containing the status of the survey response (1 or 0) and a
#' data frame of any issues found with possible solutions.
#' @importFrom DBI dbSendQuery dbBind dbFetch dbClearResult dbIsValid
#' dbDisconnect
#' @examples
#'
#' \dontrun{
#'
#' # Create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#'
#' # Set a connection to the demo database
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' # Create a data frame with some example visit metadata
#' visit <- data.frame(
#'   pk_visitID = NA,
#'   fk_personid = "fbaggins",
#'   fk_locationid = "locationA",
#'   fk_equipmentid = "camera1",
#'   visit_type = "set",
#'   visit_date = "2024-01-01",
#'   visit_time = "12:00:00"
#'   )
#'
#' # Screen the new visit's metadata
#' visit_check <- screenNewVisitMetadata(
#'   con = conx,
#'   visitMetadata = visit,
#'   disconnect = FALSE)
#'
#' # Check the status of the check
#' visit_check$status
#'
#' # Check the provided warnings
#' visit_check$warnings
#'
#' # Run on a visit with missing data
#' visit <- data.frame(
#'   pk_visitID = NA,
#'   fk_personid = "fbaggins",
#'   fk_locationid = NA,
#'   fk_equipmentid = "camera1",
#'   visit_type = "set",
#'   visit_date = "2024-01-01",
#'   visit_time = "12:00:00"
#'   )
#'
#' visit_check <- screenNewVisitMetadata(
#'   con = conx,
#'   visitMetadata = visit,
#'   disconnect = FALSE)
#'
#' # Check the status of the check
#' visit_check$status
#'
#' # Check the provided warnings
#' visit_check$warnings
#'
#' }
#'

screenNewVisitMetadata <- function(con, visitMetadata = NA, disconnect = FALSE) {

  # Check Connection and Set Up Disconnect ----------------------------------

  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")

  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }

  # Set up the status and warnings for eventual outputs ------------------------

  # Set status to 1 by default (will only change to 0 with a major error)
  status <- 1

  # Create a data frame to store warnings and associated details
  warnings <- data.frame(
    warning = character(0),
    description = character(0),
    solution = character(0),
    severity = integer(0)
  )

  # Check for missing required fields ------------------------------------
  for (basicField in c('fk_locationid', 'fk_equipmentid', 'fk_personid', 'visit_type')) {
    if (visitMetadata[[basicField]] == "" | is.na(visitMetadata[[basicField]])) {
      warnings <- rbind(
        warnings,
        data.frame(
          warning = paste('missing', basicField, sep = '_'),
          description = paste('Field', basicField, 'is missing'),
          solution = paste('Go back and add', basicField, 'to visit metadata'),
          severity = 3
        )
      )
      status <- 0
    }
  }

  # Check database for a duplicate visit -------------------------------------

  # Extract key fields from visit
  params <- list(
    visitMetadata$fk_locationid,
    visitMetadata$fk_equipmentid,
    visitMetadata$fk_personid,
    format(visitMetadata$visit_date, format = "%Y-%m-%d")
  )

  # Query the database for a matching row
  rs <- DBI::dbSendQuery(
    con,
    "SELECT * FROM visits WHERE fk_locationid = $1
    AND fk_equipmentid = $2
    AND fk_personid = $3
    AND visit_date = $4;"
  )
  DBI::dbBind(rs, params)
  matchingRow <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)

  # Output warning if a matching row exists
  if (nrow(matchingRow) != 0) {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'duplicateVisit',
        description = 'A visit with matching location, equipment, person, and date already exists in the database.',
        solution = 'Check to make sure this visit was not already added to the database.',
        severity = 3
      )
    )
  }

  # Create an extra warning for a missing visit type, as it stops other checks
  # from working properly
  if (is.na(visitMetadata$visit_type)) {

    # Add a new warning to the table
    warnings <- rbind(
      warnings,
      data.frame(
        warning = "noVisitType",
        description = "No visit type was provided.",
        solution = "Add a visit type to the visit metadata, then rerun this function to complete checks.",
        severity = 3
      )
    )

  } else {

  # Setting an already-deployed piece of equipment ---------------------------

  # Get the key fields from the visit metadata to check against existing visits
  params <- list(
    visitMetadata$fk_equipmentid,
    paste0(visitMetadata$visit_date, visitMetadata$visit_time))

  # Query the database to get the most recent visit with this equipment
  rs <- DBI::dbSendQuery(
    con,
    "SELECT visit_type, fk_locationid FROM visits WHERE fk_equipmentid = $1
    AND visit_date || visit_time <= $2
    ORDER BY visit_date DESC LIMIT 1;"
  )
  DBI::dbBind(rs, params)
  lastEquipVisit <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)

  # Output a warning if the equipment was not pulled but is being set somewhere new
  if (nrow(lastEquipVisit) != 0 && lastEquipVisit$visit_type != 'pull' & visitMetadata$visit_type == 'set') {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'setDeployedEquip',
        description = paste(
          'Equipment',
          visitMetadata$fk_equipmentid,
          'is already deployed at location',
          lastEquipVisit$fk_locationid
        ),
        solution = 'Be sure a "pull" visit for the given equipment is registered before it is set in a new location.',
        severity = 2
      )
    )
  }

  # Check for a matching set or check for any check or pull -------------------

  # Get the new list of parameters
  params <- list(
    visitMetadata$fk_locationid,
    visitMetadata$fk_equipmentid,
    format(visitMetadata$visit_date, format = "%Y-%m-%d")
    )

  # Query the database for the most recent visit based on location and equipment
  rs <- DBI::dbSendQuery(
    con,
    "SELECT * FROM visits WHERE fk_locationid = $1
    AND fk_equipmentid = $2
    AND visit_date < $3
    ORDER BY visit_date DESC LIMIT 1;"
  )
  DBI::dbBind(rs, params)
  matching_visit <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)

  # If no valid visit is found and this visit is not a set, output warning
  if (nrow(matching_visit) == 0 && visitMetadata$visit_type != "set") {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'matchingVisit',
        description = paste(
          'Visit of type',
          visitMetadata$visit_type,
          'with no matching set/check visit.'
        ),
        solution = 'Make sure to enter a matching set/check for the visit.',
        severity = 2
      )
    )
  } else if (nrow(matching_visit) != 0 &&
             matching_visit$visit_type == "pull" &&
             visitMetadata$visit_type != "set") {
    warnings <- rbind(
      warnings,
      data.frame(
        warning = 'matchingVisit',
        description = paste(
          'Visit of type',
          visitMetadata$visit_type,
          'with no matching set/check visit.'
        ),
        solution = 'Make sure to enter a matching set/check for the visit.',
        severity = 2
      )
    )
  }

  }

  # Return the results of the check -------------------------------------
  return(list(
    status = status,
    warnings = warnings
  ))

}
