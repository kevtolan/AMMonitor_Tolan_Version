#' @name get_new_annotations
#' @title Returns list of new annotations/annotats/mediatags to add via Shiny.
#' @description This function returns a list of any new annotations, annotags, or
#' media tags selected to add via the shiny tagger. Not to be used outside of Shiny.
#' @param metadata_cache The metadata cache for the associated media item
#' @param input Shiny's main ReactiveValues object that holds reactive inputs.
#' @param selectedUser The user performing the annotating, a pkpersonid from
#' people table
#' @param fileID The pkphotoid or pkrecordingid of the photo/recording being
#' annotated
#' @param bboxes Dataframe of bounding boxes for the given annotation
#' @usage get_new_annotations(metadata_cache, input, selectedUser, fileID, bboxes)
#' @return A list of new annotations/annotags/mediatags to be added to the db
get_new_annotations <- function(metadata_cache, input, selectedUser, fileID, bboxes) {
  # metadata_cache <- metadata_cache()
  # selectedUser <- selectedUser()
  # fileID <- photo_name()
  # bboxes <- bboxes()
  
  # Initialize results list
  annotation_cache_new <- list(
    annotations = metadata_cache$cache$annotations[0,],
    annotags = metadata_cache$cache$annotags[0,],
    mediatags = metadata_cache$cache$mediatags[0,]
  )
  
  # does the annotation have bounding boxes
  hasBB <- nrow(bboxes) != 0
  
  # annotations ----------
  if (input$newTaxon != "") {
    # Number of annotations (either 1 or the number of bounding boxes)
    n_new_annos <- ifelse(nrow(bboxes) == 0, 1, nrow(bboxes))
    
    # Get temporary annotation ID's to use for new, cached annotations
    cached_annoIDs <- metadata_cache$cache$annotations$pk_annotationid[
      metadata_cache$cache$annotations$pk_annotationid < 0
    ]
    start_annoID <- ifelse(
      length(cached_annoIDs) == 0,
      -1,
      min(cached_annoIDs) - 1
    )
    new_annotations <- data.frame(
      pk_annotationid = start_annoID:(start_annoID-n_new_annos+1),
      fk_personid = selectedUser,
      fk_mediaid = fileID,
      fk_searchlistid = NA,
      fk_taxonid = input$newTaxon,
      x_max = {if (hasBB) {bboxes$x_max} else {NA}},
      x_min = {if (hasBB) {bboxes$x_min} else {NA}},
      y_max = {if (hasBB) {bboxes$y_max} else {NA}},
      y_min = {if (hasBB) {bboxes$y_min} else {NA}},
      notes = NA,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      is_add = 1,
      is_delete = 0
    )
    
    new_annotation_exists <- sapply(
      seq_len(nrow(new_annotations)),
      function(i) {
        has_matching_anno <- any(
          metadata_cache$cache$annotations$fk_mediaid %in%
            new_annotations$fk_mediaid[i] & # mediaID match
            metadata_cache$cache$annotations$fk_personid %in%
            new_annotations$fk_personid[i] &  # personID match
            metadata_cache$cache$annotations$fk_taxonid %in%
            new_annotations$fk_taxonid[i]  # taxonID match
        )
        has_bbox <- !is.na(new_annotations$x_min[i])
        has_matching_anno && !has_bbox
      }
    )
    annotation_cache_new[['annotations']] <- rbind(
      annotation_cache_new[['annotations']],
      new_annotations[!new_annotation_exists,]
    )
  }
  
  # annotags -------
  
  # Get temporary annotation ID's to use for new, cached annotations
  cached_annotagIDs <- metadata_cache$cache$annotags$pk_annotagid[
    metadata_cache$cache$annotags$pk_annotagid < 0
  ]
  next_annotagID <- ifelse(
    length(cached_annotagIDs) == 0,
    -1,
    min(cached_annotagIDs) - 1
  )
  
  # Try each taxon label
  for (t_label in taxa_label_options$pk_librarylistid) {
    # Get matching existing annotations (cached or in db)
    annoID <- c(
      metadata_cache$cache$annotations$pk_annotationid[
        metadata_cache$cache$annotations$fk_mediaid == fileID &
          metadata_cache$cache$annotations$fk_personid == selectedUser &
          metadata_cache$cache$annotations$fk_taxonid == input$newTaxon
      ],
      annotation_cache_new$annotations$pk_annotationid
    )
    if (taxa_label_options[which(taxa_label_options$pk_librarylistid == t_label), 'list_type'] == 'numeric_list') {
      
      t_label_list <- librarylistitems[which(librarylistitems$fk_librarylistid == t_label),]
      
      if (nrow(t_label_list) != 0) {
        t_label_list$ids <- paste0(t_label_list$item, "_", t_label_list$pk_librarylistitemid)
        for (i in 1:nrow(t_label_list)) {
          the_row <- t_label_list[i,]
          if (
            !is.null(input[[the_row$ids]]) && 
            !is.na(input[[the_row$ids]]) && 
            !any(
              metadata_cache$cache$annotags$fk_librarylistitemid == the_row$fk_librarylistitemid &
              metadata_cache$cache$annotags$fk_annotationid %in% annoID
            )
          ) { #!(t_label %in% the_annoTags$fkLibraryID)) {
            
            # Add the annotag
            annotation_cache_new[['annotags']] <- rbind(
              annotation_cache_new[['annotags']],
              data.frame(
                pk_annotagid = next_annotagID,
                fk_annotationid = annoID,
                fk_librarylistitemid = the_row$pk_librarylistitemid,
                value_num = input[[the_row$ids]],
                fk_librarylistid = librarylistitems$fk_librarylistid[
                  librarylistitems$pk_librarylistitemid == the_row$pk_librarylistitemid
                ],
                item = librarylistitems$item[
                  librarylistitems$pk_librarylistitemid == the_row$pk_librarylistitemid
                ],
                is_add = 1,
                is_delete = 0
              )
            )
            next_annotagID <- next_annotagID - 1
          }
        }
      }
    } else if (taxa_label_options[which(taxa_label_options$pk_librarylistid == t_label), 'list_type'] == 'checkbox_list') {
      
      t_label_list <- librarylistitems[which(librarylistitems$fk_librarylistid == t_label),]
      
      if (nrow(t_label_list) != 0) {
        t_label_list$ids <- paste0(t_label_list$item, "_", t_label_list$pk_librarylistitemid)
        for (i in 1:nrow(t_label_list)) {
          the_row <- t_label_list[i,]
          
          if (
            !is.null(input[[the_row$ids]]) &&
            input[[the_row$ids]] &&
            !any(
              metadata_cache$cache$annotags$fk_librarylistitemid == the_row$fk_librarylistitemid &
              metadata_cache$cache$annotags$fk_annotationid %in% annoID
            )
          ) { #!(t_label %in% the_annoTags$fkLibraryID)) {
            # Add the annotag
            annotation_cache_new[['annotags']] <- rbind(
              annotation_cache_new[['annotags']],
              data.frame(
                pk_annotagid = next_annotagID,
                fk_annotationid = annoID,
                fk_librarylistitemid = the_row$pk_librarylistitemid,
                value_num = NA,
                fk_librarylistid = librarylistitems$fk_librarylistid[
                  librarylistitems$pk_librarylistitemid == the_row$pk_librarylistitemid
                ],
                item = librarylistitems$item[
                  librarylistitems$pk_librarylistitemid == the_row$pk_librarylistitemid
                ],
                is_add = 1,
                is_delete = 0
              )
            )
            next_annotagID <- next_annotagID - 1
          }
        }
      }
    } else {
      
      # Skip tags with no values entered
      if (is.null(input[[t_label]]) || input[[t_label]] == "") {next}
      
      # Skip if annotag is already in the db
      if (
        nrow(metadata_cache$cache$annotags) != 0 && # at least some annotags in the cache
        any( # Any existing tags with same annotation and annotag lirbarylistitem
          metadata_cache$cache$annotags$fk_librarylistitemid == input[[t_label]] & 
          metadata_cache$cache$annotags$fk_annotationid %in% annoID
        )
      ) { next }
      
      # Add the annotag
      annotation_cache_new[['annotags']] <- rbind(
        annotation_cache_new[['annotags']],
        data.frame(
          pk_annotagid = next_annotagID,
          fk_annotationid = annoID,
          fk_librarylistitemid = input[[t_label]],
          value_num = NA,
          fk_librarylistid = librarylistitems$fk_librarylistid[
            librarylistitems$pk_librarylistitemid == input[[t_label]]
          ],
          item = librarylistitems$item[
            librarylistitems$pk_librarylistitemid == input[[t_label]]
          ],
          is_add = 1,
          is_delete = 0
        )
      )
      next_annotagID <- next_annotagID - 1
    }
  }
  
  # mediatags ---------------------
  
  # Get temporary annotation ID's to use for new, cached annotations
  cached_mediatagIDs <- metadata_cache$cache$mediatags$pk_mediatagid[
    metadata_cache$cache$mediatags$pk_mediatagid < 0
  ]
  next_mediatagID <- ifelse(
    length(cached_mediatagIDs) == 0,
    -1,
    min(cached_mediatagIDs) - 1
  )
  
  for (nt_label in non_taxa_label_options$pk_medialistid) {
    
    existing_mediatags <- metadata_cache$cache$mediatags[
      metadata_cache$cache$mediatags$fk_mediaid == fileID &
        metadata_cache$cache$mediatags$fk_personid == selectedUser,
    ]
    
    if (non_taxa_label_options[which(non_taxa_label_options$pk_medialistid == nt_label), 'list_type'] == 'numeric_list') {
      
      
      nt_label_list <- medialistitems[which(medialistitems$fk_medialistid == nt_label),]
      
      if (nrow(nt_label_list) != 0) {
        nt_label_list$ids <- paste0(nt_label_list$item, "_", nt_label_list$pk_medialistitemid)
        for (i in 1:nrow(nt_label_list)) {
          the_row <- nt_label_list[i,]
          if (!is.null(input[[the_row$ids]]) && !is.na(input[[the_row$ids]]) && !any(existing_mediatags$fk_medialistitemid == the_row$fk_medialistitemid)) { #!(t_label %in% the_annoTags$fkLibraryID)) {
            
            annotation_cache_new[['mediatags']] <- rbind(
              annotation_cache_new[['mediatags']],
              data.frame(
                pk_mediatagid = next_mediatagID,
                fk_mediaid = fileID,
                fk_medialistitemid = the_row$pk_medialistitemid,
                fk_personid = selectedUser,
                x_max = NA,
                x_min = NA,
                y_max = NA,
                y_min = NA,
                value_num = input[[the_row$ids]],
                notes = NA,
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                fk_medialistid = the_row$fk_medialistid,
                item = the_row$item,
                is_add = 1,
                is_delete = 0
              )
            )
            next_mediatagID <- next_mediatagID - 1
          }
        }
      }
    } else if (non_taxa_label_options[which(non_taxa_label_options$pk_medialistid == nt_label), 'list_type'] == 'checkbox_list') {
      nt_label_list <- medialistitems[which(medialistitems$fk_medialistid == nt_label),]
      
      if (nrow(nt_label_list) != 0) {
        nt_label_list$ids <- paste0(nt_label_list$item, "_", nt_label_list$pk_medialistitemid)
        for (i in 1:nrow(nt_label_list)) {
          the_row <- nt_label_list[i,]
          if (!is.null(input[[the_row$ids]]) && input[[the_row$ids]] && !any(existing_mediatags$fk_medialistitemid == the_row$fk_medialistitemid)) { #!(t_label %in% the_annoTags$fkLibraryID)) {
            
            annotation_cache_new[['mediatags']] <- rbind(
              annotation_cache_new[['mediatags']],
              data.frame(
                pk_mediatagid = next_mediatagID,
                fk_mediaid = fileID,
                fk_medialistitemid = the_row$pk_medialistitemid,
                fk_personid = selectedUser,
                x_max = NA,
                x_min = NA,
                y_max = NA,
                y_min = NA,
                value_num = NA,
                notes = NA,
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                fk_medialistid = the_row$fk_medialistid,
                item = the_row$item,
                is_add = 1,
                is_delete = 0
              )
            )
            next_mediatagID <- next_mediatagID - 1
          }
        }
      }
    }
    else if (non_taxa_label_options[which(non_taxa_label_options$pk_medialistid == nt_label), 'list_type'] == 'bbox_list'){
      if (!is.null(input[[nt_label]]) && input[[nt_label]] != "" && !(input[[nt_label]] %in% existing_mediatags$fk_medialistitemid)) {
        
        # Create the new annoTag
        if (hasBB) {
          new_mediatag <- data.frame(
            pk_mediatagid = next_mediatagID,
            fk_mediaid = fileID,
            fk_medialistitemid = as.numeric(input[[nt_label]]),
            fk_personid = selectedUser,
            x_max = bboxes$x_max,
            x_min = bboxes$x_min,
            y_max = bboxes$y_max,
            y_min = bboxes$y_min,
            value_num = NA,
            notes = NA,
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            fk_medialistid = medialistitems$fk_medialistid[
              medialistitems$pk_medialistitemid == input[[nt_label]]
            ],
            item = medialistitems$item[
              medialistitems$pk_medialistitemid == input[[nt_label]]
            ],
            is_add = 1,
            is_delete = 0
          )
        } else {
          new_mediatag <- data.frame(
            pk_mediatagid = next_mediatagID,
            fk_mediaid = fileID,
            fk_medialistitemid = as.numeric(input[[nt_label]]),
            fk_personid = selectedUser,
            x_max = NA,
            x_min = NA,
            y_max = NA,
            y_min = NA,
            value_num = NA,
            notes = NA,
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            fk_medialistid = medialistitems$fk_medialistid[
              medialistitems$pk_medialistitemid == input[[nt_label]]
            ],
            item = medialistitems$item[
              medialistitems$pk_medialistitemid == input[[nt_label]]
            ],
            is_add = 1,
            is_delete = 0
          )
        }
        annotation_cache_new[['mediatags']] <- rbind(
          annotation_cache_new[['mediatags']],
          new_mediatag
        )
        next_mediatagID <- next_mediatagID - 1
      }
    } else {
      
      if (!is.null(input[[nt_label]]) && input[[nt_label]] != "" && !(input[[nt_label]] %in% existing_mediatags$fk_medialistitemid)) {
        
        annotation_cache_new[['mediatags']] <- rbind(
          annotation_cache_new[['mediatags']],
          data.frame(
            pk_mediatagid = next_mediatagID,
            fk_mediaid = fileID,
            fk_medialistitemid = as.numeric(input[[nt_label]]),
            fk_personid = selectedUser,
            x_max = NA,
            x_min = NA,
            y_max = NA,
            y_min = NA,
            value_num = NA,
            notes = NA,
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            fk_medialistid = medialistitems$fk_medialistid[
              medialistitems$pk_medialistitemid == input[[nt_label]]
            ],
            item = medialistitems$item[
              medialistitems$pk_medialistitemid == input[[nt_label]]
            ],
            is_add = 1,
            is_delete = 0
          )
        )
        next_mediatagID <- next_mediatagID - 1
      }
    }
  }
  return(annotation_cache_new)
}
