#' @name save_metadata_cache
#' @title Writes any unsaved cached metadata to db and updates the metadata cache.
#' @description This updates the database based on any unsaved cached metadata
#' and returns a reactiveValues object with an updated metadata cache. Not to be 
#' used outside of Shiny.
#' @param metadata_cache The metadata cache for the associated media item
#' @param annotations_cache Newly cached annotations.
#' @usage save_metadata_cache(metadata_cache, annotations_cache)
#' @return An updated copy of the metadata cache, based on any db updates.
save_metadata_cache <- function(metadata_cache, annotations_cache) {
  # Nothing to flush if the cache hasn't been populated with real data yet
  # -- its fields still hold their initial NA placeholders until
  # audio_avail() has fired at least once (e.g. before Apply Filters is
  # first pressed). One of the two call sites in audio_player.R (the
  # save_metadata_now observer) has no other guard for this, so it's
  # handled here instead of relying on every caller to check first.
  if (!is.data.frame(metadata_cache$cache$mediaMetaData)) {
    return(metadata_cache)
  }

  annos_to_add <- metadata_cache$cache$annotations[
    metadata_cache$cache$annotations$is_add == 1,
  ]
  # Add annotations
  for (i in seq_len(nrow(annos_to_add))) {
    # Insert row into annotations table
    new_anno <- annos_to_add[
      i,
      ! names(annos_to_add) %in% c('pk_annotationid', 'is_add', 'is_delete')
    ]
    rs <- DBI::dbSendQuery(
      con(), 
      paste0(
        "INSERT INTO annotations (",
        paste(names(new_anno)[!is.na(new_anno)], collapse = ', '),
        ") VALUES(",
        paste("$", 1:sum(!is.na(new_anno)), sep = "", collapse =  ", "),
        ") RETURNING pk_annotationid;"
      )
    )
    DBI::dbBind(rs, new_anno[!is.na(new_anno)])
    annotationid_new <- DBI::dbFetch(rs)[,]
    DBI::dbClearResult(rs)
    
    # Update metadata cache for newly added annotation
    metadata_cache$cache$annotations$pk_annotationid[
      which(metadata_cache$cache$annotations$pk_annotationid == annos_to_add[i, 'pk_annotationid'])
    ] <- annotationid_new
    metadata_cache$cache$annotations$is_add[
      which(metadata_cache$cache$annotations$pk_annotationid == annotationid_new)
    ] <- 0
    
    # Add any associated annotags
    annotags_to_add <- metadata_cache$cache$annotags[
      metadata_cache$cache$annotags$fk_annotationid == annos_to_add$pk_annotationid[i],
    ]
    
    # Update fk_annotationid
    if (nrow(annotags_to_add) != 0) {
      annotags_to_add$fk_annotationid <- annotationid_new
    }
    
    # Add annotags to db
    for (i in seq_len(nrow(annotags_to_add))) {
      # Insert row into annotags table
      new_annotag <- annotags_to_add[
        i,
        !names(annotags_to_add) %in% c('pk_annotagid', 'fk_librarylistid', 'item', 'is_add', 'is_delete')
      ]
      
      rs <- DBI::dbSendQuery(
        con(), 
        paste0(
          "INSERT INTO annotags (",
          paste(names(new_annotag)[!is.na(new_annotag)], collapse = ', '),
          ") VALUES(",
          paste("$", 1:sum(!is.na(new_annotag)), sep = "", collapse =  ", "),
          ") RETURNING pk_annotagid;"
        )
      )
      DBI::dbBind(rs, new_annotag[!is.na(new_annotag)])
      annotagid_new <- DBI::dbFetch(rs)[,]
      DBI::dbClearResult(rs)
      
      # Update metadata cache for newly added annotags
      metadata_cache$cache$annotags$pk_annotagid[
        which(metadata_cache$cache$annotags$pk_annotagid == annotags_to_add[i, 'pk_annotagid'])
      ] <- annotagid_new
      metadata_cache$cache$annotags$is_add[
        which(metadata_cache$cache$annotags$pk_annotagid == annotagid_new)
      ] <- 0
      metadata_cache$cache$annotags$fk_annotationid[
        which(metadata_cache$cache$annotags$pk_annotagid == annotagid_new)
      ] <- annotationid_new
    }
  }
  
  # Add annotags to existing annotations
  if (!any(is.na(annotations_cache))) {
    annotags_to_add <- annotations_cache$annotags[
      annotations_cache$annotags$pk_annotagid < 0 &
        annotations_cache$annotags$fk_annotationid > 0,
    ]
    for (i in seq_len(nrow(annotags_to_add))) {
      # Insert row into annotags table
      new_annotag <- annotags_to_add[
        i,
        ! names(annotags_to_add) %in% c('pk_annotagid', 'fk_librarylistid', 'item', 'is_add', 'is_delete')
      ]
      
      rs <- DBI::dbSendQuery(
        con(), 
        paste0(
          "INSERT INTO annotags (",
          paste(names(new_annotag)[!is.na(new_annotag)], collapse = ', '),
          ") VALUES(",
          paste("$", 1:sum(!is.na(new_annotag)), sep = "", collapse =  ", "),
          ") RETURNING pk_annotagid;"
        )
      )
      DBI::dbBind(rs, new_annotag[!is.na(new_annotag)])
      annotagid_new <- DBI::dbFetch(rs)[,]
      DBI::dbClearResult(rs)
      
      # Update metadata cache for newly added annotation
      metadata_cache$cache$annotags$pk_annotagid[
        which(metadata_cache$cache$annotags$pk_annotagid == annotags_to_add[i, 'pk_annotagid'])
      ] <- annotagid_new
      metadata_cache$cache$annotags$is_add[
        which(metadata_cache$cache$annotags$pk_annotagid == annotagid_new)
      ] <- 0
    }
  }
  
  # Add mediatags
  mediatags_to_add <- metadata_cache$cache$mediatags[
    metadata_cache$cache$mediatags$is_add == 1,
  ]
  for (i in seq_len(nrow(mediatags_to_add))) {
    # Insert row into mediatags table
    new_mediatag <- mediatags_to_add[
      i,
      ! names(mediatags_to_add) %in% c('pk_mediatagid', 'fk_medialistid', 'item', 'is_add', 'is_delete')
    ]
    
    rs <- DBI::dbSendQuery(
      con(), 
      paste0(
        "INSERT INTO mediatags (",
        paste(names(new_mediatag)[!is.na(new_mediatag)], collapse = ', '),
        ") VALUES(",
        paste("$", 1:sum(!is.na(new_mediatag)), sep = "", collapse =  ", "),
        ") RETURNING pk_mediatagid;"
      )
    )
    DBI::dbBind(rs, new_mediatag[!is.na(new_mediatag)])
    mediatagid_new <- DBI::dbFetch(rs)[,]
    DBI::dbClearResult(rs)
    
    # Update metadata cache for newly added annotation
    metadata_cache$cache$mediatags$pk_mediatagid[
      which(metadata_cache$cache$mediatags$pk_mediatagid == mediatags_to_add[i, 'pk_mediatagid'])
    ] <- mediatagid_new
    metadata_cache$cache$mediatags$is_add[
      which(metadata_cache$cache$mediatags$pk_mediatagid == mediatagid_new)
    ] <- 0
  }
  
  # Delete annotations/annotags/mediatags
  if (any(as.logical(metadata_cache$cache$annotations$is_delete))) {
    i_delete <- which(as.logical(metadata_cache$cache$annotations$is_delete))
    rs <- DBI::dbSendQuery(
      con(),
      paste0(
        'DELETE FROM annotations WHERE pk_annotationid IN (',
        paste(metadata_cache$cache$annotations$pk_annotationid[i_delete], collapse = ', '),
        ');'
      )
    )
    DBI::dbClearResult(rs)
    # Update the cache
    metadata_cache$cache$annotations <- metadata_cache$cache$annotations[
      !metadata_cache$cache$annotations$pk_annotationid %in% metadata_cache$cache$annotations$pk_annotationid[i_delete],
    ]
  }
  if (any(as.logical(metadata_cache$cache$annotags$is_delete))) {
    i_delete <- which(as.logical(metadata_cache$cache$annotags$is_delete))
    rs <- DBI::dbSendQuery(
      con(),
      paste0(
        'DELETE FROM annotags WHERE pk_annotagid IN (',
        paste(metadata_cache$cache$annotags$pk_annotagid[i_delete], collapse = ', '),
        ');'
      )
    )
    DBI::dbClearResult(rs)
    metadata_cache$cache$annotags <- metadata_cache$cache$annotags[
      !metadata_cache$cache$annotags$pk_annotagid %in% metadata_cache$cache$annotags$pk_annotagid[i_delete],
    ]
  }
  if (any(as.logical(metadata_cache$cache$mediatags$is_delete))) {
    i_delete <- which(as.logical(metadata_cache$cache$mediatags$is_delete))
    rs <- DBI::dbSendQuery(
      con(),
      paste0(
        'DELETE FROM mediatags WHERE pk_mediatagid IN (',
        paste(metadata_cache$cache$mediatags$pk_mediatagid[i_delete], collapse = ', '),
        ');'
      )
    )
    DBI::dbClearResult(rs)
    metadata_cache$cache$mediatags <- metadata_cache$cache$mediatags[
      !metadata_cache$cache$mediatags$pk_mediatagid %in% metadata_cache$cache$mediatags$pk_mediatagid[i_delete],
    ]
  }
  
  # Add any annotation verifications
  annotationverifications_to_add <- metadata_cache$cache$annotationverifications[
    metadata_cache$cache$annotationverifications$is_add == 1,
  ]
  for (i in seq_len(nrow(annotationverifications_to_add))) {
    # If an existing model verification, update it
    if (annotationverifications_to_add$pk_annoverificationid[i] > 0) {
      # Database update
      rs <- DBI::dbSendQuery(
        con(),
        paste(
          'UPDATE annotationverifications SET is_valid =',
          annotationverifications_to_add$is_valid[i],
          'WHERE pk_annoverificationid =',
          annotationverifications_to_add$pk_annoverificationid[i],
          ';'
        )
      )
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$annotationverifications$is_valid[
        which(metadata_cache$cache$annotationverifications$pk_annoverificationid == annotationverifications_to_add$pk_annoverificationid[i])
      ] <- annotationverifications_to_add$is_valid[i]
      metadata_cache$cache$annotationverifications$is_add[
        which(metadata_cache$cache$annotationverifications$pk_annoverificationid == annotationverifications_to_add$pk_annoverificationid[i])
      ] <- 0
      
    } else {
      # Insert row into modelverifications table
      new_annotationverification <- annotationverifications_to_add[
        i,
        !names(annotationverifications_to_add) %in% c('pk_annoverificationid', 'is_add', 'is_delete')
      ]
      
      rs <- DBI::dbSendQuery(
        con(), 
        paste0(
          "INSERT INTO annotationverifications (",
          paste(names(new_annotationverification)[!is.na(new_annotationverification)], collapse = ', '),
          ") VALUES(",
          paste("$", 1:sum(!is.na(new_annotationverification)), sep = "", collapse =  ", "),
          ") RETURNING pk_annoverificationid;"
        )
      )
      DBI::dbBind(rs, new_annotationverification[!is.na(new_annotationverification)])
      annotationverifid_new <- DBI::dbFetch(rs)[,]
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$annotationverifications$pk_annoverificationid[
        which(metadata_cache$cache$annotationverifications$pk_annoverificationid == annotationverifications_to_add[i, 'pk_annoverificationid'])
      ] <- annotationverifid_new
      metadata_cache$cache$annotationverifications$is_add[
        which(metadata_cache$cache$annotationverifications$pk_annoverificationid == annotationverifid_new)
      ] <- 0
    }
  }
  
  # Delete any annotation veficiations
  annotationverifications_to_delete <- metadata_cache$cache$annotationverifications[
    metadata_cache$cache$annotationverifications$is_delete == 1,
  ]
  
  for (i in seq_len(nrow(annotationverifications_to_delete))) {
    # Remove from the database
    rs <- DBI::dbSendQuery(
      con(),
      paste(
        'DELETE FROM annotationverifications WHERE pk_annoverificationid =',
        annotationverifications_to_delete$pk_annoverificationid[i],
        ';'
      )
    )
    DBI::dbClearResult(rs)
    
    # Update the cache
    metadata_cache$cache$annotationverifications <- metadata_cache$cache$annotationverifications[
      metadata_cache$cache$annotationverifications$pk_annoverificationid != annotationverifications_to_delete$pk_annoverificationid[i],
    ]
  }
  
  # Add any annotag verifications
  annotagverifications_to_add <- metadata_cache$cache$annotagverifications[
    metadata_cache$cache$annotagverifications$is_add == 1,
  ]
  for (i in seq_len(nrow(annotagverifications_to_add))) {
    # If an existing model verification, update it
    if (annotagverifications_to_add$pk_tagverificationid[i] > 0) {
      # Database update
      rs <- DBI::dbSendQuery(
        con(),
        paste(
          'UPDATE annotagverifications SET is_valid =',
          annotagverifications_to_add$is_valid[i],
          'WHERE pk_tagverificationid =',
          annotagverifications_to_add$pk_tagverificationid[i],
          ';'
        )
      )
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$annotagverifications$is_valid[
        which(metadata_cache$cache$annotagverifications$pk_tagverificationid == annotagverifications_to_add$pk_tagverificationid[i])
      ] <- annotagverifications_to_add$is_valid[i]
      metadata_cache$cache$annotagverifications$is_add[
        which(metadata_cache$cache$annotagverifications$pk_tagverificationid == annotagverifications_to_add$pk_tagverificationid[i])
      ] <- 0
      
    } else {
      # Insert row into annotagverifications table
      new_annotagverification <- annotagverifications_to_add[
        i,
        !names(annotagverifications_to_add) %in% c('pk_tagverificationid', 'is_add', 'is_delete')
      ]
      
      rs <- DBI::dbSendQuery(
        con(), 
        paste0(
          "INSERT INTO annotagverifications (",
          paste(names(new_annotagverification)[!is.na(new_annotagverification)], collapse = ', '),
          ") VALUES(",
          paste("$", 1:sum(!is.na(new_annotagverification)), sep = "", collapse =  ", "),
          ") RETURNING pk_tagverificationid;"
        )
      )
      DBI::dbBind(rs, new_annotagverification[!is.na(new_annotagverification)])
      annotagverifid_new <- DBI::dbFetch(rs)[,]
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$annotagverifications$pk_tagverificationid[
        which(metadata_cache$cache$annotagverifications$pk_tagverificationid == annotagverifications_to_add[i, 'pk_tagverificationid'])
      ] <- annotagverifid_new
      metadata_cache$cache$annotagverifications$is_add[
        which(metadata_cache$cache$annotagverifications$pk_tagverificationid == annotagverifid_new)
      ] <- 0
    }
  }
  
  # Delete any annotag veficiations
  annotagverifications_to_delete <- metadata_cache$cache$annotagverifications[
    metadata_cache$cache$annotagverifications$is_delete == 1,
  ]
  
  for (i in seq_len(nrow(annotagverifications_to_delete))) {
    # Remove from the database
    rs <- DBI::dbSendQuery(
      con(),
      paste(
        'DELETE FROM annotagverifications WHERE pk_tagverificationid =',
        annotagverifications_to_delete$pk_tagverificationid[i],
        ';'
      )
    )
    DBI::dbClearResult(rs)
    
    # Update the cache
    metadata_cache$cache$annotagverifications <- metadata_cache$cache$annotagverifications[
      metadata_cache$cache$annotagverifications$pk_tagverificationid != annotagverifications_to_delete$pk_tagverificationid[i],
    ]
  }
  
  # Add any mediatag verifications
  
  mediatagverifications_to_add <- metadata_cache$cache$mediatagverifications[
    metadata_cache$cache$mediatagverifications$is_add == 1,
  ]
  for (i in seq_len(nrow(mediatagverifications_to_add))) {
    # If an existing model verification, update it
    if (mediatagverifications_to_add$pk_mediatagverificationid[i] > 0) {
      # Database update
      rs <- DBI::dbSendQuery(
        con(),
        paste(
          'UPDATE mediatagverifications SET is_valid =',
          mediatagverifications_to_add$is_valid[i],
          'WHERE pk_mediatagverificationid =',
          mediatagverifications_to_add$pk_mediatagverificationid[i],
          ';'
        )
      )
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$mediatagverifications$is_valid[
        which(metadata_cache$cache$mediatagverifications$pk_mediatagverificationid == mediatagverifications_to_add$pk_mediatagverificationid[i])
      ] <- mediatagverifications_to_add$is_valid[i]
      metadata_cache$cache$mediatagverifications$is_add[
        which(metadata_cache$cache$mediatagverifications$pk_mediatagverificationid == mediatagverifications_to_add$pk_mediatagverificationid[i])
      ] <- 0
      
    } else {
      # Insert row into mediaverification table
      new_mediatagverification <- mediatagverifications_to_add[
        i,
        !names(mediatagverifications_to_add) %in% c('pk_mediatagverificationid', 'is_add', 'is_delete')
      ]
      
      rs <- DBI::dbSendQuery(
        con(), 
        paste0(
          "INSERT INTO mediatagverifications (",
          paste(names(new_mediatagverification)[!is.na(new_mediatagverification)], collapse = ', '),
          ") VALUES(",
          paste("$", 1:sum(!is.na(new_mediatagverification)), sep = "", collapse =  ", "),
          ") RETURNING pk_mediatagverificationid;"
        )
      )
      DBI::dbBind(rs, new_mediatagverification[!is.na(new_mediatagverification)])
      mediatagverifid_new <- DBI::dbFetch(rs)[,]
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$mediatagverifications$pk_mediatagverificationid[
        which(metadata_cache$cache$mediatagverifications$pk_mediatagverificationid == mediatagverifications_to_add[i, 'pk_mediatagverificationid'])
      ] <- mediatagverifid_new
      metadata_cache$cache$mediatagverifications$is_add[
        which(metadata_cache$cache$mediatagverifications$pk_mediatagverificationid == mediatagverifid_new)
      ] <- 0
    }
  }
  
  # Delete any mediatag veficiations
  mediatagverifications_to_delete <- metadata_cache$cache$mediatagverifications[
    metadata_cache$cache$mediatagverifications$is_delete == 1,
  ]
  
  for (i in seq_len(nrow(mediatagverifications_to_delete))) {
    # Remove from the database
    rs <- DBI::dbSendQuery(
      con(),
      paste(
        'DELETE FROM mediatagverifications WHERE pk_mediatagverificationid =',
        mediatagverifications_to_delete$pk_mediatagverificationid[i],
        ';'
      )
    )
    DBI::dbClearResult(rs)
    
    # Update the cache
    metadata_cache$cache$mediatagverifications <- metadata_cache$cache$mediatagverifications[
      metadata_cache$cache$mediatagverifications$pk_mediatagverificationid != mediatagverifications_to_delete$pk_mediatagverificationid[i],
    ]
  }
  
  # Add any model verifications
  modelverifications_to_add <- metadata_cache$cache$modelverifications[
    metadata_cache$cache$modelverifications$is_add == 1,
  ]
  for (i in seq_len(nrow(modelverifications_to_add))) {
    # If an existing model verification, update it
    if (modelverifications_to_add$pk_modelverificationid[i] > 0) {
      # Database update
      rs <- DBI::dbSendQuery(
        con(),
        paste(
          'UPDATE modelverifications SET is_valid = ',
          modelverifications_to_add$is_valid[i],
          'WHERE pk_modelverificationid = ',
          modelverifications_to_add$pk_modelverificationid[i],
          ';'
        )
      )
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$modelverifications$is_valid[
        which(metadata_cache$cache$modelverifications$pk_modelverificationid == modelverifications_to_add$pk_modelverificationid[i])
      ] <- modelverifications_to_add$is_valid[i]
      metadata_cache$cache$modelverifications$is_add[
        which(metadata_cache$cache$modelverifications$pk_modelverificationid == modelverifications_to_add$pk_modelverificationid[i])
      ] <- 0
      
    } else {
      # Insert row into modelverifications table
      new_modelverification <- modelverifications_to_add[
        i,
        !names(modelverifications_to_add) %in% c('pk_modelverificationid', 'is_add', 'is_delete')
      ]
      
      rs <- DBI::dbSendQuery(
        con(), 
        paste0(
          "INSERT INTO modelverifications (",
          paste(names(new_modelverification)[!is.na(new_modelverification)], collapse = ', '),
          ") VALUES(",
          paste("$", 1:sum(!is.na(new_modelverification)), sep = "", collapse =  ", "),
          ") RETURNING pk_modelverificationid;"
        )
      )
      DBI::dbBind(rs, new_modelverification[!is.na(new_modelverification)])
      modelverifid_new <- DBI::dbFetch(rs)[,]
      DBI::dbClearResult(rs)
      
      # Update the metadata cache
      metadata_cache$cache$modelverifications$pk_modelverificationid[
        which(metadata_cache$cache$modelverifications$pk_modelverificationid == modelverifications_to_add[i, 'pk_modelverificationid'])
      ] <- modelverifid_new
      metadata_cache$cache$modelverifications$is_add[
        which(metadata_cache$cache$modelverifications$pk_modelverificationid == modelverifid_new)
      ] <- 0
    }
  }
  # Delete any model verificiations
  modelverifications_to_delete <- metadata_cache$cache$modelverifications[
    metadata_cache$cache$modelverifications$is_delete == 1,
  ]
  
  for (i in seq_len(nrow(modelverifications_to_delete))) {
    # Remove from the database
    rs <- DBI::dbSendQuery(
      con(),
      paste(
        'DELETE FROM modelverifications WHERE pk_modelverificationid =',
        modelverifications_to_delete$pk_modelverificationid[i],
        ';'
      )
    )
    DBI::dbClearResult(rs)
    
    # Update the cache
    metadata_cache$cache$modelverifications <- metadata_cache$cache$modelverifications[
      metadata_cache$cache$modelverifications$pk_modelverificationid != modelverifications_to_delete$pk_modelverificationid[i],
    ]
  }
  
  return(metadata_cache)
}