#' @name dbAddIndexes
#' @title Add indexes to the foreign-key columns of an AMMonitor database
#' @description \code{dbAddIndexes} finds every foreign-key column in a
#' database that isn't already covered by an index and creates one. SQLite
#' does not index foreign-key columns automatically, and without an index it
#' has to scan an entire child table whenever it looks up rows that reference
#' a parent row. On a large database this makes deletes (which check
#' for, and cascade to, referencing rows) and joins on those columns very
#' slow -- for example, deleting rows from a 27-million-row
#' \code{modeloutputs} table took about 2 seconds \emph{per row} until
#' \code{modeloutputs(fk_parentid)} was indexed, after which the same
#' 660,000-row delete took about a second.
#' @param con An open database connection, such as one returned by
#' \code{\link{dbSetCon}}.
#' @param dry_run Default = FALSE. If TRUE, nothing is created; the indexes
#' that \emph{would} be created are returned (and shown) so they can be
#' reviewed first.
#' @usage dbAddIndexes(con, dry_run = FALSE)
#' @return A data.frame with one row per foreign key found, giving the
#' \code{table}, the foreign-key \code{columns}, the \code{index} name,
#' the \code{status} (\code{"created"}, \code{"already indexed"}, or, for a
#' dry run, \code{"would create"}), and the \code{seconds} taken to build it.
#' @details The foreign keys are read from SQLite itself
#' (\code{PRAGMA foreign_key_list}), so nothing is hard-coded and any tables
#' or foreign keys added by a project are included. A foreign key counts as
#' already indexed if an existing index -- including a primary key or unique
#' constraint -- starts with the same columns, in which case it is left
#' alone. New indexes are named \code{idx_<table>_<columns>}. Running the
#' function again is harmless: columns that are already covered are skipped.
#'
#' Only indexes are added; no data or table structure is changed. Indexes
#' take some extra space in the database file, and make bulk inserts
#' slightly slower, so this is a function to call deliberately rather than
#' something run automatically. Building an index on a very large table can
#' take several seconds, and progress is reported with a message for each
#' one. As with any change to a database, it is sensible to back it up
#' first. An index can be removed with \code{DROP INDEX}.
#' @importFrom DBI dbGetQuery dbExecute dbListTables dbQuoteIdentifier
#' dbIsValid
#' @family database
#' @export
#' @examples
#' \dontrun{
#'
#' # create a demo AMMonitor project in a temporary directory (to be deleted)
#' demo_fp <- ammCreateMiniDemo(filepath = tempdir())
#'
#' # to work with the database, set a connection
#' conx <- dbSetCon(file.path(demo_fp, "database", "demo.sqlite"))
#'
#' # see which indexes would be created, without creating them
#' dbAddIndexes(conx, dry_run = TRUE)
#'
#' # create them
#' dbAddIndexes(conx)
#'
#' # running it again does nothing: everything is already indexed
#' dbAddIndexes(conx)
#'
#' # disconnect from the database when finished
#' DBI::dbDisconnect(conx)
#'
#' # remove the demo AMMonitor file structure
#' unlink(demo_fp, recursive = TRUE)
#'
#' }

dbAddIndexes <- function(con, dry_run = FALSE) {

  if (!DBI::dbIsValid(con)) stop("The database connection is not valid.")

  tables <- setdiff(DBI::dbListTables(con), "sqlite_sequence")
  tables <- tables[!grepl("^sqlite_", tables)]

  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))

  # Column lists of every index that already exists on a table (including
  # primary-key and unique constraints). An INTEGER PRIMARY KEY is the
  # rowid itself and doesn't appear as an index, so primary-key columns are
  # added too.
  existing_indexes <- function(tbl) {
    out <- list()

    info <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", q(tbl)))
    pk <- info[info$pk > 0, ]
    if (nrow(pk) > 0) out[[length(out) + 1]] <- pk$name[order(pk$pk)]

    idx <- DBI::dbGetQuery(con, sprintf("PRAGMA index_list(%s)", q(tbl)))
    for (nm in idx$name) {
      cols <- DBI::dbGetQuery(con, sprintf("PRAGMA index_info(%s)", q(nm)))
      cols <- cols[!is.na(cols$name), ]
      if (nrow(cols) > 0) out[[length(out) + 1]] <- cols$name[order(cols$seqno)]
    }

    out
  }

  covered <- function(fk_cols, indexes) {
    any(vapply(indexes, function(ix) {
      length(ix) >= length(fk_cols) &&
        setequal(ix[seq_along(fk_cols)], fk_cols)
    }, logical(1)))
  }

  results <- list()

  for (tbl in tables) {

    fks <- DBI::dbGetQuery(con, sprintf("PRAGMA foreign_key_list(%s)", q(tbl)))
    if (nrow(fks) == 0) next

    indexes <- existing_indexes(tbl)

    # One entry per foreign key (a composite key spans several rows that
    # share an id), columns in key order.
    for (id in unique(fks$id)) {
      one <- fks[fks$id == id, ]
      fk_cols <- one$from[order(one$seq)]
      idx_name <- paste0("idx_", tbl, "_", paste(fk_cols, collapse = "_"))

      status <- "already indexed"
      seconds <- NA_real_

      if (!covered(fk_cols, indexes)) {
        if (dry_run) {
          status <- "would create"
        } else {
          message("Creating index ", idx_name, " ...")
          seconds <- unname(system.time(
            DBI::dbExecute(con, sprintf(
              "CREATE INDEX IF NOT EXISTS %s ON %s (%s)",
              q(idx_name), q(tbl), paste(q(fk_cols), collapse = ", ")
            ))
          )[["elapsed"]])
          status <- "created"
          # Later foreign keys on this table shouldn't be judged against a
          # stale picture of what exists.
          indexes[[length(indexes) + 1]] <- fk_cols
        }
      }

      results[[length(results) + 1]] <- data.frame(
        table = tbl,
        columns = paste(fk_cols, collapse = ", "),
        index = idx_name,
        status = status,
        seconds = seconds,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      table = character(0), columns = character(0), index = character(0),
      status = character(0), seconds = numeric(0), stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, results)
}
