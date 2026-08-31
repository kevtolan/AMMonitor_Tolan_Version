#' @name taxaAdd
#' @title Add a taxon to the taxa table, based on taxonomic serial number (TSN).
#' @description Given parallel vectors containing TSN, preferred common name, 
#' and short-hand text for the taxon (to be used as the primary key), this 
#' function uses the ritis R package to get the relevant taxonomic tree for the 
#' specified taxon and add to the database taxa table.
#' @param con An existing database connection.
#' @param tsns Vector of taxonomic serial numbers (TSN's). See itis.gov
#' @param common_names Vector of preferred common names for the corresponding
#' TSN's
#' @param pk_taxonids Vector of primary key values for the corresponding taxon
#' (defaults to common name if not provided)
#' @param overwrite TRUE or FALSE, whether to overwrite existing taxon. Default
#' is FALSE
#' @param disconnect TRUE or FALSE. Should the database connection be closed 
#' on exit? Default is FALSE
#' @usage taxaAdd(con, tsns, common_names, pk_taxonids = NA, overwrite = FALSE, 
#'   disconnect = FALSE)
#' @importFrom DBI dbIsValid dbDisconnect dbGetRowsAffected dbGetQuery 
#' dbSendQuery dbBind dbFetch dbClearResult dbSendStatement dbAppendTable
#' @importFrom ritis full_record hierarchy_full 
#' @details Please see the "taxa" learnr tutorial for more information:
#' 
#' \code{learnr::run_tutorial(name = "taxa", package = "AMMonitor")}
#' @return A dataframe indicating whether each TSN was added, and if not, the
#' reason why not.
#' @references
#' For more information about Taxonomic Serial Numbers:
#' \href{https://www.itis.gov/}{https://www.itis.gov/}
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
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' # retrieve the demo taxa table
#' taxa <- DBI::dbReadTable(conx, name = "taxa")
#' 
#' # look at the taxa table; note the column "tsn" (taxonomic serial number)
#' taxa
#' 
#' # use the package ritis to find a TSN for Red-eyed Vireo
#' ritis::search_common(x = "red-eyed vireo")
#' 
#' # add the Red-eyed Vireo to the taxa table
#' taxaAdd(
#'   con = conx, 
#'   tsns = 179021,
#'   common_names = "Red-eyed Vireo",
#'   pk_taxonids = "revi",
#'   overwrite = FALSE,
#'   disconnect = FALSE
#' )
#'
#' # query the database for the new record
#' DBI::dbGetQuery(conx, statement = "SELECT * FROM taxa WHERE pk_taxonid = 'revi';")
#' 
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#' 
#' # remove the demo
#' unlink(demo_fp)
#' }

taxaAdd <- function(con, tsns, common_names, pk_taxonids = NA, overwrite = FALSE, disconnect = FALSE) {

  if (DBI::dbIsValid(con) == FALSE) stop("The database connection is not valid.")

  if (disconnect == TRUE) {
    on.exit(expr = {
      DBI::dbDisconnect(con)
    })
  }
  
  # check the length of vectors if provided
  length_tsns <- length(tsns)
  
  taxa_rank_order <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Subspecies")

  # Results indicate whether each tsn was added, and if not, the reason why not.
  results <- data.frame(
    tsn = integer(0),
    status = character(0),
    reason = character(0)
  )
  
  # Add a check for pk_taxonids to replicate NA if multiple taxa are provided
  if (all(is.na(pk_taxonids))) {
    pk_taxonids <- rep(NA, length_tsns)
  }
  
  # Get indices of any NAs so that they can be autopopulated with common names
  na_indices <- which(is.na(pk_taxonids))

  if (length(na_indices > 0)) {
    pk_taxonids[na_indices] <- common_names[na_indices]
  }

  # Initialize dataframe for new rows to taxa table
  new_dat <- DBI::dbGetQuery(
    con, "SELECT * FROM taxa WHERE pk_taxonid = ''")[0,]
  
  # Loop through each TSN, and add taxa one at a time
  for (ii in 1:length(tsns)) {

    tsn <- as.integer(tsns[ii])

    if (is.na(tsn)) {
      results <- rbind(
        results,
        data.frame(
          tsn = tsn,
          status = 'fail',
          reason = 'invalid TSN syntax'
        )
      )
      next
    }

    common_name <- common_names[ii]
    pk_taxonid <- pk_taxonids[ii]

    # Skip this taxon if it is already in the db
    param_tsn <- list(tsn)
    param_taxonID <- list(pk_taxonid)
    rs <- DBI::dbSendQuery(
      con,
      paste0("SELECT ROUND(COUNT(*)) FROM taxa WHERE tsn = $1;")
    )
    DBI::dbBind(rs, param_tsn)
    is_taxonTSN_in_db <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    
    rs <- DBI::dbSendQuery(
      con, 
      paste0("SELECT ROUND(COUNT(*)) FROM taxa WHERE pk_taxonid = $1;")
    )
    DBI::dbBind(rs, param_taxonID)
    is_taxonID_in_db <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    taxon_exists <- FALSE
    if (is_taxonTSN_in_db != 0) {
      if (overwrite == TRUE) {
        taxon_exists <- TRUE
      } else {
        results <- rbind(
          results,
          data.frame(
            tsn = tsn,
            status = 'fail',
            reason = 'tsn is already in database'
          )
        )
        next
      }
    } else if (is_taxonID_in_db != 0) {
      if (overwrite == TRUE) {
        taxon_exists <- TRUE
      } else {
        results <- rbind(
          results,
          data.frame(
            tsn = tsn,
            status = 'fail',
            reason = 'taxonID is already in database'
          )
        )
        next
      }
    }

    # Pull taxonomic info from itis, and break if tsn is invalid/unaccepted
    taxon_info <- tryCatch(
      {
        taxon_info <- ritis::full_record(tsn)
        # If taxa info is not valid, move to the next tsn
        if (length(taxon_info) == 0) {
          taxon_info <- data.frame(
            tsn = tsn,
            status = 'fail',
            reason = 'TSN not found'
          )
        } else if (!taxon_info$usage$taxonUsageRating %in% c("valid", "accepted")) {
          taxon_info <- data.frame(
            tsn = tsn,
            status = 'fail',
            reason = paste(taxon_info$usage$taxonUsageRating, 'TSN')
          )
        }
        taxon_info
      },
      error = function(cond) {
        taxon_info <- data.frame(
          tsn = tsn,
          status = 'fail',
          reason = paste('Unable to query itis.gov', cond)
        )
        taxon_info
      }
    )

    if ('status' %in% names(taxon_info)) {
      results <- rbind(results, taxon_info)
      next
    }

    # Row index for new taxon info
    new_row <- new_dat[0,]

    # Initialize a row for the taxa table, and add the TSN, common name, and pk
    new_row[1,] <- NA
    new_row$common_name[1] <- common_name
    new_row$tsn[1] <- tsn
    new_row$pk_taxonid[1] <- pk_taxonid

    # Get the rank of the taxa (e.g., subspecies, species, genus, family, order, class)
    taxa_rank <- trimws(taxon_info$taxRank$rankName)

    new_row$taxon_rank[1] <- taxa_rank

    # Get taxonomic hierarchy
    th <- ritis::hierarchy_full(tsn)

    # Loop through and add the provided taxon, as well as all higher-level taxa
    for (j in 1:min(c(which(taxa_rank_order == taxa_rank), length(taxa_rank_order)))) {
      the_taxa_rank <- paste0("rank_", tolower(taxa_rank_order[j]))
      new_row[1, the_taxa_rank] <- th[th$rankname == taxa_rank_order[j], 'taxonname']
    }

    # Add a note that this record was auto-extracted
    new_row$notes[1] <- paste(
      "Taxonomic information was harvested from a list of TSN's using the package",
      "'ritis' on",
      date()
    )
    
    if (taxon_exists == TRUE) {
      stmnt <- "UPDATE taxa SET "
      params <- list()
      param_counter <- 1
      for (c in names(new_row)) {
        if (!is.na(new_row[1, c])) {
          stmnt <- paste0(stmnt, c, " = $", param_counter, ", ")
          params <- append(params, new_row[1, c])
          param_counter <- param_counter + 1
        }
      }
      stmnt <- substr(stmnt, 1, nchar(stmnt)-2)
      params <- append(params, new_row$tsn)
      stmnt <- paste0(stmnt, " WHERE tsn = $", param_counter, ";")
      
      rs <- DBI::dbSendStatement(con, stmnt)
      dbBind(rs, params)
      rowsChanged <- DBI::dbGetRowsAffected(rs)
      dbClearResult(rs)
      
      if (rowsChanged > 0) {
        results <- rbind(
          results,
          data.frame(
            tsn = tsn,
            status = 'success',
            reason = paste0(
              'Record in database overwritten. ',
              rowsChanged,
              " rows updated across all database tables."
              )
          )
        )
      } else {
        results <- rbind(
          results,
          data.frame(
            tsn = tsn,
            status = 'fail',
            reason = paste('0 rows changed in database')
          )
        )
      }
      
    } else {
      new_dat <- rbind(new_dat, new_row)
      
      results <- rbind(
        results,
        data.frame(
          tsn = tsn,
          status = 'success',
          reason = NA
        )
      )
    }

  }

  DBI::dbAppendTable(con, 'taxa', new_dat)

  return(results)
}
