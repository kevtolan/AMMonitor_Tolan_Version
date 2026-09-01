annotation_viewer_tables_ui <- function(id, viewer_mode, mediaType = "photo") {
  ns <- NS(id)
  tagList(
    
    # Define "hot keys"
    tags$script(HTML(paste0(
      "$(function(){",
      "$(document).keyup(function(e) {",
      "if (e.which == 107 || e.which == 187) {", # + or =
      "$('#",
      ns('verify_agree'),
      "').click()",
      "}",
      "});",
      "})"
    ))),
    tags$script(HTML(paste0(
      "$(function(){",
      "$(document).keyup(function(e) {",
      "if (e.which == 109 || e.which == 189) {", # - or -
      "$('#",
      ns('verify_disagree'),
      "').click()",
      "}",
      "});",
      "})"
    ))),

    # Click-and-drag range selection across rows of the Taxon Model Outputs
    # table. A plain click still toggles a single row via reactable's own
    # onClick handler; only an actual drag (mousedown then mouseover a
    # different row before mouseup) triggers this, which computes the full
    # row range and applies it via updateReactable() server-side.
    if (viewer_mode == "modelOutputs") {
      tags$script(HTML(paste0(
        "(function() {",
        "  var tableId = '", ns('taxon_table'), "';",
        "  var rowSel = '#' + tableId + ' .rt-tbody .rt-tr';",
        "  var dragging = false;",
        "  var startIdx = null;",
        "  function rowIndex(el) {",
        "    return $(rowSel).index(el);",
        "  }",
        "  $(document).on('mousedown', rowSel, function(e) {",
        "    dragging = true;",
        "    startIdx = rowIndex(this);",
        "  });",
        "  $(document).on('mouseover', rowSel, function(e) {",
        "    if (!dragging || startIdx === null) return;",
        "    var idx = rowIndex(this);",
        "    if (idx < 0) return;",
        "    var lo = Math.min(startIdx, idx);",
        "    var hi = Math.max(startIdx, idx);",
        "    if (hi > lo) {",
        "      var state = window.Reactable ? Reactable.getState(tableId) : null;",
        "      var page = (state && state.page) || 0;",
        "      var pageSize = (state && state.pageSize) || 10;",
        "      var indices = [];",
        "      for (var i = lo; i <= hi; i++) { indices.push(page * pageSize + i + 1); }",
        "      Shiny.setInputValue('", ns('drag_selected_rows'), "', indices);",
        "    }",
        "  });",
        "  $(document).on('mouseup', function(e) {",
        "    dragging = false;",
        "    startIdx = null;",
        "  });",
        "})();"
      )))
    },

    switch(
      viewer_mode,
      modelOutputs = tagList(
        tags$h3('Taxon Model Outputs'),
        uiOutput(ns('manual_annotation_flag')),
        reactable::reactableOutput(ns('taxon_table'))
      ),
      tagList(
        tags$h3('Taxon Tags'),
        reactable::reactableOutput(ns('taxon_table')),
        tags$br(),
        tags$h3('File Tags'),
        reactable::reactableOutput(ns('file_tags_table'))
      )
    ),
    switch(
      viewer_mode,
      viewer = tags$br(),
      tagger = fluidRow(
        tags$br(),
        column(
          width = 3,
          offset = 2,
          actionButton(ns('delete_annotation'), 'Delete selected annotation(s)')
        )
      ),
      tagList(
        tags$br(),
        wellPanel(
          fluidRow(
            tags$h3('Verify Tags'),
            column(
              width = 3,
              offset = 2,
              actionButton(ns('verify_agree'), 'Agree')
            ),
            column(
              width = 3,
              actionButton(ns('verify_disagree'), 'Disagree')
            ),
            column(
              width = 3,
              actionButton(ns('reset_verify'), 'Clear')
            )
          )
        )
      )
    )
  )
}

annotation_viewer_tables_server <- function(id, selectedUser = reactive(NA), active = reactive(TRUE), file_name, updateTags = reactive(NA), viewer_mode, viewModelOutputs = reactive(FALSE), mediaType = "photo", confValue = reactive(NA), lessThan = reactive(FALSE), metadata_cache = reactive(NA)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updateTags2 <- reactiveVal(1) # To trigger an update of the annotations DT

    # Flags whether this recording has any manual (human) annotations, since
    # the Model Verifications table only ever shows modeloutputs and
    # otherwise gives no indication that tagger annotations also exist.
    output$manual_annotation_flag <- renderUI({
      req(viewer_mode == "modelOutputs")
      req(file_name())

      annotation_count <- sum(
        metadata_cache()$cache$annotations$fk_mediaid == file_name() &
          metadata_cache()$cache$annotations$is_delete == 0,
        na.rm = TRUE
      )

      if (annotation_count == 0) return(NULL)

      msg <- if (annotation_count == 1) {
        "1 manual annotation exists for this recording."
      } else {
        sprintf("%d manual annotations exist for this recording.", annotation_count)
      }

      tags$p(style = "color: #a05a00; font-weight: 600;", msg)
    })

    taxon_tags <- reactive({
      req(selectedUser())
      req(metadata_cache()$cache$annotations)
      updateTags()
      updateTags2()
      
      verifications_cache(NA)
      deleted_record(NA)
      
      # Get the annotations for the given media file
      annos <- metadata_cache()$cache$annotations[
        metadata_cache()$cache$annotations$fk_mediaid == file_name() & metadata_cache()$cache$annotations$is_delete == 0,
      ]
      
      # Filter by annotator
      if (viewer_mode == "tagger") {
        annos <- annos[annos$fk_personid == selectedUser(),]
      } else if (viewer_mode == "verifier") {
        annos <- annos[annos$fk_personid != selectedUser(),]
      }
      
      # Get annotations that match
      annoTags <- metadata_cache()$cache$annotags[
        metadata_cache()$cache$annotags$fk_annotationid %in% annos$pk_annotationid & metadata_cache()$cache$annotags$is_delete == 0,
      ]
      
      annoTable <- rbind(
        merge(
          x = annos[,c('pk_annotationid', 'fk_personid', 'fk_taxonid', 'x_min', 'x_max', 'y_min', 'y_max')], 
          y = annoTags[0,c('pk_annotagid', 'fk_annotationid', 'fk_librarylistid', 'item', 'value_num')], 
          by.x = 'pk_annotationid', 
          by.y = 'fk_annotationid', 
          all.x = TRUE
        ),
        merge(
          x = annos[,c('pk_annotationid', 'fk_personid', 'fk_taxonid', 'x_min', 'x_max', 'y_min', 'y_max')], 
          y = annoTags[,c('pk_annotagid', 'fk_annotationid', 'fk_librarylistid', 'item', 'value_num')], 
          by.x = 'pk_annotationid', 
          by.y = 'fk_annotationid', 
          all.y = TRUE
        )
      )
      
      if (nrow(annoTable) != 0 && viewer_mode == 'verifier') {
        annotationverifications <- metadata_cache()$cache$annotationverifications[
          metadata_cache()$cache$annotationverifications$fk_personid == selectedUser() & metadata_cache()$cache$annotationverifications$is_delete == 0,
          c('pk_annoverificationid', 'is_valid', 'fk_annotationid')
        ]
        annotationverifications$pk_annotagid <- rep(NA, nrow(annotationverifications))
        
        annoTable <- merge(
          annoTable,
          annotationverifications,
          by.x = c('pk_annotationid', 'pk_annotagid'),
          by.y = c('fk_annotationid', 'pk_annotagid'),
          all.x = TRUE
        )
        names(annoTable)[names(annoTable) == 'is_valid'] <- 'verified'
        
        annotagverifications <- metadata_cache()$cache$annotagverifications[
          metadata_cache()$cache$annotagverifications$is_delete == 0,
          c('pk_tagverificationid', 'is_valid', 'fk_annotagid')
        ]
        
        annoTable$verified[match(annotagverifications$fk_annotagid, annoTable$pk_annotagid)] <- annotagverifications$is_valid
        
      }
      
      # Sort the annoTable
      annoTable <- annoTable[
        order(annoTable$pk_annotationid, annoTable$pk_annotagid, na.last = FALSE),
      ]
      
      # Model outputs
      modelOutputs <- metadata_cache()$cache$modeloutputs[
        metadata_cache()$cache$modeloutputs$fk_mediaid == file_name() & metadata_cache()$cache$modeloutputs$is_delete == 0,
      ]
      
      if (viewer_mode == 'viewer' && viewModelOutputs() && nrow(modelOutputs) != 0) {
        annoTable <- rbind(
          {if (nrow(annoTable) != 0) {
            cbind(pk_modeloutputid = NA, annoTable, type = 'human')
          } else {
            data.frame()
          }},
          cbind(
            pk_modeloutputid = modelOutputs$pk_modeloutputid,
            pk_annotationid = "model",
            fk_personid = modelOutputs$model_name,
            modelOutputs[,c('fk_taxonid', 'x_min', 'x_max', 'y_min', 'y_max')],
            pk_annotagid = NA,
            fk_librarylistid = NA,
            item = modelOutputs$fk_librarylistitemid,
            value_num = modelOutputs$value_num,
            type = 'model'
          )
        )
      } else {
        annoTable <- cbind(annoTable, pk_modeloutputid = numeric(nrow(annoTable)))
      }
      
      if (viewer_mode == 'modelOutputs') {
        # Merge in model verifications
        if (nrow(modelOutputs) != 0) {
          modelOutputs <- cbind(modelOutputs, 'verified' = NA)
          
          modelverifications <- metadata_cache()$cache$modelverifications[
            metadata_cache()$cache$modelverifications$fk_modeloutputid %in% modelOutputs$pk_modeloutputid & metadata_cache()$cache$modelverifications$fk_personid == selectedUser() & metadata_cache()$cache$modelverifications$is_delete == 0,
          ]
          
          modelOutputs$verified[match(modelverifications$fk_modeloutputid, modelOutputs$pk_modeloutputid)] <- modelverifications$is_valid
        }
        
        annoTable <- modelOutputs
      }
      
      # Rename columns
      names(annoTable)[names(annoTable) == 'fk_personid'] <- 'person'
      names(annoTable)[names(annoTable) == 'fk_taxonid'] <- 'taxon'
      names(annoTable)[names(annoTable) == 'fk_librarylistid'] <- 'libraryid'
      if (mediaType == 'audio') {
        names(annoTable)[names(annoTable) == 'x_min'] <- "Start time (s)"
        names(annoTable)[names(annoTable) == 'x_max'] <- "End time (s)"
        names(annoTable)[names(annoTable) == 'y_min'] <- "Min freq (KHz)"
        names(annoTable)[names(annoTable) == 'y_max'] <- "Max freq (KHz)"
        annoTable <- annoTable[order(annoTable[["Start time (s)"]]),]
      } else {
        annoTable[,c('x_min', 'x_max', 'y_min', 'y_max')] <- NULL
      }
      rownames(annoTable) <- NULL
      annoTable
    })
    
    observe({
      updateReactable('taxon_table', selected = NA)
      if (viewer_mode != 'modelOutputs') {
        updateReactable('file_tags_table', selected = NA)
      }
    }, priority = 100) |> bindEvent(updateTags2(), file_name())
    
    taxon_table_df <- reactive({
      switch(
        paste(viewer_mode, mediaType, sep = "-"),
        "modelOutputs-photo" = taxon_tags()[,!names(taxon_tags()) %in% c('pk_annotationid', 'pk_annotagid', 'fk_mediaid', 'pk_mediatagid', 'pk_annoverificationid', 'fk_modelid', 'is_add', 'is_delete')],
        "tagger-photo" = ,
        "verifier-photo" = ,
        "viewer-photo" = taxon_tags()[,!names(taxon_tags()) %in% c('pk_annotagid', 'fk_mediaid', 'pk_mediatagid', 'pk_annoverificationid', 'fk_modelid', 'is_add', 'is_delete')],
        "modelOutputs-audio" = taxon_tags()[,!names(taxon_tags()) %in% c('pk_annotagid', 'fk_mediaid', 'pk_mediatagid', 'pk_annoverificationid', 'fk_modelid', 'is_add', 'is_delete')],
        "tagger-audio" = ,
        "verifier-audio" = ,
        "viewer-audio" = taxon_tags()[,!names(taxon_tags()) %in% c('pk_annotagid', 'fk_mediaid', 'pk_mediatagid', 'pk_modeloutputid', 'pk_annoverificationid', 'fk_modelid', 'is_add', 'is_delete')]
      )
    })
    
    # Allow annotations table to render on startup
    onStartup <- reactiveVal(TRUE)
    
    # Render table of taxon annotations
    output$taxon_table <- reactable::renderReactable({
      if (onStartup()) {
        req(taxon_table_df())
        onStartup(!onStartup())
      }
      
      switch(
        paste(viewer_mode, mediaType, sep = "-"),
        "modelOutputs-photo" = reactable(
          data = isolate(taxon_table_df()),
          columns = list(
            pk_modeloutputid = colDef(name = "ID"),
            model_name = colDef(style = list(whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis")),
            value_num = colDef(format = colFormat(digits = 2))
          ),
          sortable = FALSE,
          selection = "multiple",
          onClick = "select",
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "rgba(0, 0, 0, 0.08)"))
        ),
        "tagger-photo" = ,
        "verifier-photo" = ,
        "viewer-photo" = reactable(
          data = isolate(taxon_table_df()), 
          columns = list(
            pk_annotationid = colDef(name = "ID"), 
            pk_modeloutputid = colDef(show = FALSE),
            value_num = colDef(format = colFormat(digits = 2))
          ),
          sortable = FALSE,
          groupBy = 'pk_annotationid',
          defaultExpanded = TRUE,
          selection = "multiple", 
          onClick = "select"
        ),
        "modelOutputs-audio" = reactable(
          data = isolate(taxon_table_df()),
          columns = list(
            pk_modeloutputid = colDef(name = "ID"),
            model_name = colDef(style = list(whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis")),
            `Start time (s)` = colDef(
              filterable = TRUE,
              format = colFormat(digits = 2),
              filterMethod = JS(
                "function(rows, columnId, filterValue) {
                  return rows.filter(function(row) {
                    return row.values[columnId] >= filterValue
                  })
                }"
              ),
              filterInput = function(values, name) {
                oninput <- sprintf(
                  paste0("Reactable.setFilter('", ns("taxon_table"), "', '%s', this.value)"),
                  name
                )
                tags$input(
                  type = "range",
                  min = 0,
                  max = ifelse(length(values) > 0 && any(!is.na(values)), ceiling(max(values, na.rm = TRUE)), 0),
                  value = 0,
                  oninput = oninput,
                  onchange = oninput, # For IE11 support
                  "aria-label" = sprintf("Filter by minimum %s", name)
                )
              }
            ),
            `End time (s)` = colDef(
              filterable = TRUE,
              format = colFormat(digits = 2),
              filterMethod = JS(
                "function(rows, columnId, filterValue) {
                  return rows.filter(function(row) {
                    return row.values[columnId] <= filterValue
                  })
                }"
              ),
              filterInput = function(values, name) {
                oninput <- sprintf(
                  paste0("Reactable.setFilter('", ns("taxon_table"), "', '%s', this.value)"),
                  name
                )
                tags$input(
                  type = "range",
                  min = 0,
                  max = ifelse(length(values) > 0 && any(!is.na(values)), ceiling(max(values, na.rm = TRUE)), 0),
                  value = ifelse(length(values) > 0 && any(!is.na(values)), ceiling(max(values, na.rm = TRUE)), 0),
                  oninput = oninput,
                  onchange = oninput, # For IE11 support
                  "aria-label" = sprintf("Filter by minimum %s", name)
                )
              }
            ),
            `Min freq (KHz)` = colDef(format = colFormat(digits = 2)),
            `Max freq (KHz)` = colDef(format = colFormat(digits = 2)),
            value_num = colDef(format = colFormat(digits = 2))
          ),
          sortable = FALSE,
          defaultExpanded = TRUE,
          selection = "multiple",
          onClick = "select",
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "rgba(0, 0, 0, 0.08)"))
        ),
        "tagger-audio" = ,
        "verifier-audio" = ,
        "viewer-audio" = reactable(
          data = isolate(taxon_table_df()), 
          columns = list(
            pk_annotationid = colDef(name = "ID"),
            `Start time (s)` = colDef(
              filterable = TRUE,
              format = colFormat(digits = 2),
              filterMethod = JS(
                "function(rows, columnId, filterValue) {
                  return rows.filter(function(row) {
                    return row.values[columnId] <= filterValue
                  })
                }"
              ),
              filterInput = function(values, name) {
                oninput <- sprintf(
                  paste0("Reactable.setFilter('", ns("taxon_table"), "', '%s', this.value)"),
                  name
                )
                tags$input(
                  type = "range",
                  min = 0,
                  max = ifelse(length(values) > 0 && any(!is.na(values)), ceiling(max(values, na.rm = TRUE)), 0),
                  value = 0,
                  oninput = oninput,
                  onchange = oninput, # For IE11 support
                  "aria-label" = sprintf("Filter by minimum %s", name)
                )
              }
            ),
            `End time (s)` = colDef(
              filterable = TRUE,
              format = colFormat(digits = 2),
              filterMethod = JS(
                "function(rows, columnId, filterValue) {
                  return rows.filter(function(row) {
                    return row.values[columnId] <= filterValue
                  })
                }"
              ),
              filterInput = function(values, name) {
                oninput <- sprintf(
                  paste0("Reactable.setFilter('", ns("taxon_table"), "', '%s', this.value)"),
                  name
                )
                tags$input(
                  type = "range",
                  min = 0,
                  max = ifelse(length(values) > 0 && any(!is.na(values)), ceiling(max(values, na.rm = TRUE)), 0),
                  value = ifelse(length(values) > 0 && any(!is.na(values)), ceiling(max(values, na.rm = TRUE)), 0),
                  oninput = oninput,
                  onchange = oninput, # For IE11 support
                  "aria-label" = sprintf("Filter by minimum %s", name)
                )
              }
            ),
            `Min freq (KHz)` = colDef(format = colFormat(digits = 2)),
            `Max freq (KHz)` = colDef(format = colFormat(digits = 2)),
            value_num = colDef(format = colFormat(digits = 2))
          ),
          sortable = FALSE,
          groupBy = 'pk_annotationid',
          defaultExpanded = TRUE,
          selection = "single", 
          onClick = "select"
        )
      )
    })
    
    observeEvent(taxon_table_df(), {
      updateReactable(
        "taxon_table",
        data = taxon_table_df(),
        page = isolate(input$taxon_table__reactable__page),
        expanded = TRUE
      )
    }, ignoreInit = TRUE)
    
    # annotags table ---------------
    file_tags <- reactive({
      req(selectedUser(), metadata_cache())
      updateTags()
      updateTags2()
      
      mediatagTable <- metadata_cache()$cache$mediatags[
        metadata_cache()$cache$mediatags$fk_mediaid == file_name() & metadata_cache()$cache$mediatags$is_delete == 0,
        c('pk_mediatagid', 'fk_personid', 'fk_medialistid', 'item', 'value_num')
      ]
      
      # Filter by annotator
      if (viewer_mode == "tagger") {
        mediatagTable <- mediatagTable[mediatagTable$fk_personid == selectedUser(),]
      } else if (viewer_mode == "verifier") {
        mediatagTable <- mediatagTable[mediatagTable$fk_personid != selectedUser(),]
      }
      
      # Rename columns
      names(mediatagTable)[names(mediatagTable) == 'fk_personid'] <- 'person'
      
      if (nrow(mediatagTable) != 0 && viewer_mode == 'verifier') {
        
        mediatagverifications <- metadata_cache()$cache$mediatagverifications[
          metadata_cache()$cache$mediatagverifications$fk_personid == selectedUser() & metadata_cache()$cache$mediatagverifications$is_delete == 0,
          c('pk_mediatagverificationid', 'is_valid', 'fk_mediatagid')
        ]
        mediatagTable <- merge(
          mediatagTable,
          mediatagverifications,
          by.x = 'pk_mediatagid',
          by.y = 'fk_mediatagid',
          all.x = TRUE
        )
        names(mediatagTable)[names(mediatagTable) == 'is_valid'] <- 'verified'
      }
      
      mediatagTable
    })
    
    output$file_tags_table <- reactable::renderReactable({
      switch(
        mediaType,
        photo = reactable(
          file_tags()[,!(names(file_tags()) %in% c('pk_mediatagid', 'pk_mediatagverificationid'))],
          sortable = FALSE,
          selection = "multiple",
          onClick = "select"
        ),
        audio = reactable(
          file_tags()[,!(names(file_tags()) %in% c('pk_mediatagid'))],
          sortable = FALSE,
          selection = "single",
          onClick = "select"
        )
      )
    })
    
    observeEvent(input$delete_annotation, {
      
      showModal(modalDialog(
        shiny::HTML(paste(
          'Are you sure you want to delete the selected annotations?<br>',
          '<I>Note:</I> This action cannot be undone.'
        )),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete")
        )
      ))
    })
    
    deleted_record <- reactiveVal(NA)
    
    observeEvent(input$confirm_delete, {
      removeModal()
      deleted_record(NA)
      # TAXON TAGS
      annotations_delete <- getReactableState('taxon_table', 'selected')
      
      # ID's of tags to delete
      delete_annTagIDs <- taxon_tags()[annotations_delete, c('pk_annotationid', 'pk_annotagid')]
      
      # NOT-TAXON TAGS
      mediatags_delete <- getReactableState('file_tags_table', 'selected')
      
      # ID's of tags to delete
      delete_mediatagIDs <- file_tags()[mediatags_delete, c('pk_mediatagid')]
      
      # deleted_record(deleted_record()+1)
      deleted_record(list(annotags = delete_annTagIDs, mediatags = delete_mediatagIDs))
      updateTags2(updateTags2()+1)
    })
    
    verify_val <- reactiveVal(numeric(0))

    # Applies the row range computed by the click-and-drag JS above
    observeEvent(input$drag_selected_rows, {
      req(active())
      req(length(input$drag_selected_rows) > 0)
      updateReactable('taxon_table', selected = input$drag_selected_rows)
    })

    observeEvent(input$verify_agree, {
      if (active()) {
        verify_val(1)
      }
    })

    observeEvent(input$verify_disagree, {
      if (active()) {
        verify_val(0)
      }
    })
    
    verifications_cache <- reactiveVal(NA)
    
    observeEvent(verify_val(), {if(active()) {
      req(verify_val())
      
      verifications_cache(NA)
      
      # Get temporary annotation ID's to use for new, cached annotations
      cached_annoverificationIDs <- metadata_cache()$cache$annotationverifications$pk_annoverificationid [
        metadata_cache()$cache$annotationverifications$pk_annoverificationid < 0
      ]
      annoverifID_new <- ifelse(
        length(cached_annoverificationIDs) == 0,
        -1,
        min(cached_annoverificationIDs) - 1
      )
      
      # Get temporary annotation ID's to use for new, cached annotations
      cached_annotagverificationIDs <- metadata_cache()$cache$annotagverifications$pk_tagverificationid[
        metadata_cache()$cache$annotagverifications$pk_tagverificationid < 0
      ]
      annotagverifID_new <- ifelse(
        length(cached_annotagverificationIDs) == 0,
        -1,
        min(cached_annotagverificationIDs) - 1
      )
      
      # Get temporary mediatag ID's to use for new, cached mediatags
      cached_mediatagverificationIDs <- metadata_cache()$cache$mediatagverifications$pk_mediatagverificationid[
        metadata_cache()$cache$mediatagverifications$pk_mediatagverificationid < 0
      ]
      mediatagverifID_new <- ifelse(
        length(cached_mediatagverificationIDs) == 0,
        -1,
        min(cached_mediatagverificationIDs) - 1
      )
      
      # Get temporary modeloutputverification ID's to use for new, cached modeloutputverifications
      cached_modelverificationIDs <- metadata_cache()$cache$modelverifications$pk_modelverificationid[
        metadata_cache()$cache$modelverifications$pk_modelverificationid < 0
      ]
      modelverifID_new <- ifelse(
        length(cached_modelverificationIDs) == 0,
        -1,
        min(cached_modelverificationIDs) - 1
      )
      
      new_annotationverifications <- data.frame()
      new_annotagverifications <- data.frame()
      new_mediatagverifications <- data.frame()
      new_modelverifications <- data.frame()
      
      # TAXON TAGS
      if (length(verify_val()) == 0) {stop("No verify value found.")}
      # ID's of tags to verify
      # If no rows are selected, then verify all

      if (any(c(
        getReactableState('taxon_table', 'selected'),
        getReactableState('file_tags_table', 'selected')
      ))) {
        if (viewer_mode == "modelOutputs") {
          verify_annTagIDs <- taxon_tags()[
            getReactableState('taxon_table', 'selected'),
            'pk_modeloutputid'
          ]

          verify_mediaTagIDs <- numeric(0)
        } else {
          verify_annTagIDs <- taxon_tags()[
            getReactableState('taxon_table', 'selected'), 
            c('pk_annotationid', 'pk_annotagid')
          ]
          
          verify_mediaTagIDs <- file_tags()[
            getReactableState('file_tags_table', 'selected'),
            'pk_mediatagid'
          ]
        }
      } else if (mediaType != "audio") {
        if (viewer_mode == "modelOutputs") {
          verify_annTagIDs <- taxon_tags()[['pk_modeloutputid']]
          verify_mediaTagIDs <- numeric(0)
        } else {
          verify_annTagIDs <- taxon_tags()[c('pk_annotationid', 'pk_annotagid')]
          verify_mediaTagIDs <- file_tags()[['pk_mediatagid']]
        }
      } else {
        verify_annTagIDs <- data.frame()
        verify_mediaTagIDs <- data.frame()
      }
      
      if (viewer_mode == 'modelOutputs') {
        for (i in seq_len(length(verify_annTagIDs))) {
          # See if a valid modeloutput exists
          modeloutput_existing <- metadata_cache()$cache$modelverifications[
            metadata_cache()$cache$modelverifications$fk_modeloutputid %in% verify_annTagIDs[i] & metadata_cache()$cache$modelverifications$fk_personid == selectedUser(),
          ]
          
          if (nrow(modeloutput_existing) != 0) {
            pk_modelverificationid <- modeloutput_existing$pk_modelverificationid
          } else {
            pk_modelverificationid <- modelverifID_new
            modelverifID_new <- modelverifID_new - 1
          }
          
          new_modelverifications <- rbind(
            new_modelverifications,
            data.frame(
              pk_modelverificationid = pk_modelverificationid,
              fk_modeloutputid = verify_annTagIDs[i],
              fk_personid = selectedUser(),
              is_valid = verify_val(),
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              is_add = 1,
              is_delete = 0
            )
          )
        }
      } else {
        for (i in seq_len(nrow(verify_annTagIDs))) {
          if (is.na(verify_annTagIDs$pk_annotagid[i])) {
            # See if a valid annotation exists
            anno_existing <- metadata_cache()$cache$annotationverifications[
              metadata_cache()$cache$annotationverifications$fk_annotationid %in% verify_annTagIDs$pk_annotationid[i] & metadata_cache()$cache$annotationverifications$fk_personid == selectedUser(),
            ]
            
            if (nrow(anno_existing) != 0) {
              pk_annoverificationid <- anno_existing$pk_annoverificationid 
            } else {
              pk_annoverificationid <- annoverifID_new
              annoverifID_new <- annoverifID_new - 1
            }
            
            new_annotationverifications <- rbind(
              new_annotationverifications,
              data.frame(
                pk_annoverificationid = pk_annoverificationid,
                is_valid = verify_val(),
                fk_personid = selectedUser(),
                fk_annotationid = verify_annTagIDs$pk_annotationid[i],
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                is_add = 1,
                is_delete = 0
              )
            )
          } else {
            
            # See if a valid annotation exists
            annotag_existing <- metadata_cache()$cache$annotagverifications[
              metadata_cache()$cache$annotagverifications$fk_annotagid %in% verify_annTagIDs$pk_annotagid[i],
            ]
            
            if (nrow(annotag_existing) != 0) {
              pk_tagverificationid <- annotag_existing$pk_tagverificationid
            } else {
              pk_tagverificationid <- annotagverifID_new
              annotagverifID_new <- annotagverifID_new - 1
            }
            
            new_annotagverifications <- rbind(
              new_annotagverifications,
              data.frame(
                pk_tagverificationid = pk_tagverificationid,
                fk_annotagid = verify_annTagIDs$pk_annotagid[i],
                fk_personid = selectedUser(),
                is_valid = verify_val(),
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                is_add = 1,
                is_delete = 0
              )
            )
          }
        }
      }
      
      # Verify mediatags
      for (i in seq_len(length(verify_mediaTagIDs))) {
        # See if a valid annotation exists
        mediatag_existing <- metadata_cache()$cache$mediatagverifications[
          metadata_cache()$cache$mediatagverifications$fk_mediatagid %in% verify_mediaTagIDs[i],
        ]
        
        if (nrow(mediatag_existing) != 0) {
          pk_mediatagverificationid <- mediatag_existing$pk_mediatagverificationid
        } else {
          pk_mediatagverificationid <- mediatagverifID_new
          mediatagverifID_new <- mediatagverifID_new - 1
        }
        
        new_mediatagverifications <- rbind(
          new_mediatagverifications,
          data.frame(
            pk_mediatagverificationid = pk_mediatagverificationid,
            is_valid = verify_val(),
            fk_personid = selectedUser(),
            fk_mediatagid = verify_mediaTagIDs[i],
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            is_add = 1,
            is_delete = 0
          )
        )
      }
      
      verifications_cache(
        list(
          annotationverifications = new_annotationverifications,
          annotagverifications = new_annotagverifications,
          mediatagverifications = new_mediatagverifications,
          modelverifications = new_modelverifications
        )
      )
      
      updateTags2(updateTags2()+1)
      isolate(verify_val(numeric(0))) # Reset so it will trigger on the next click
    }})
    
    # Delete selected/all own verifications when the reset button is clicked
    observeEvent(input$reset_verify, {
      req(active())
      # ID's of tags to clear
      # If no rows are selected, then clear all
      
      if (any(c(
        getReactableState('taxon_table', 'selected'),
        getReactableState('file_tags_table', 'selected')
      ))) {
        if (viewer_mode == "modelOutputs") {
          verify_annTagIDs <- taxon_tags()[
            getReactableState('taxon_table', 'selected'),
            'pk_modeloutputid'
          ]
          
          verify_mediaTagIDs <- numeric(0)
        } else {
          verify_annTagIDs <- taxon_tags()[
            getReactableState('taxon_table', 'selected'), 
            c('pk_annotationid', 'pk_annotagid')
          ]
          
          verify_mediaTagIDs <- file_tags()[
            getReactableState('file_tags_table', 'selected'),
            'pk_mediatagid'
          ]
        }
      } else if (mediaType != "audio") {
        if (viewer_mode == "modelOutputs") {
          verify_annTagIDs <- taxon_tags()[['pk_modeloutputid']]
          verify_mediaTagIDs <- numeric(0)
        } else {
          verify_annTagIDs <- taxon_tags()[c('pk_annotationid', 'pk_annotagid')]
          verify_mediaTagIDs <- file_tags()[['pk_mediatagid']]
        }
      } else {
        verify_annTagIDs <- data.frame()
        verify_mediaTagIDs <- data.frame()
      }
      
      delete_annotationverifications <- data.frame()
      delete_annotagverifications <- data.frame()
      delete_mediatagverifications <- data.frame()
      delete_modelverifications <- data.frame()
      
      if (viewer_mode == 'modelOutputs') {
        for (i in seq_len(length(verify_annTagIDs))) {
          # Check to see if any verifications already recorded for this annotation
          verifications <- metadata_cache()$cache$modelverifications[
            metadata_cache()$cache$modelverifications$fk_modeloutputid == verify_annTagIDs[i] & metadata_cache()$cache$modelverifications$fk_personid == selectedUser(),
          ]
          
          if (nrow(verifications) != 1) {next}
          verifications$is_delete <- 1
          
          delete_modelverifications <- rbind(
            delete_modelverifications,
            verifications
          )
        }
      } else {
        for (i in seq_len(nrow(verify_annTagIDs))) {
          if (is.na(verify_annTagIDs$pk_annotagid[i])) {
            # Check to see if any verifications already recorded for this annotation
            verifications <- metadata_cache()$cache$annotationverifications[
              metadata_cache()$cache$annotationverifications$fk_annotationid == verify_annTagIDs$pk_annotationid[i] & metadata_cache()$cache$annotationverifications$fk_personid == selectedUser(),
            ]
            
            if (nrow(verifications) != 1) {next}
            verifications$is_delete <- 1
            
            delete_annotationverifications <- rbind(
              delete_annotationverifications,
              verifications
            )
          } else {
            # Take care of the taxon annotags
            # Check to see if any verifications already recorded for this annoTag
            verifications <- metadata_cache()$cache$annotagverifications[
              metadata_cache()$cache$annotagverifications$fk_annotagid == verify_annTagIDs$pk_annotagid[i] & metadata_cache()$cache$annotagverifications$fk_personid == selectedUser(),
            ]
            
            if (nrow(verifications) != 1) {next}
            verifications$is_delete <- 1
            
            delete_annotagverifications <- rbind(
              delete_annotagverifications,
              verifications
            )
          }
        }
        
        # Reset mediatag verifications
        for (i in seq_len(length(verify_mediaTagIDs))) {
          # Check to see if any verifications already recorded for this annotation
          verifications <- metadata_cache()$cache$mediatagverifications[
            metadata_cache()$cache$mediatagverifications$fk_mediatagid == verify_mediaTagIDs[i] & metadata_cache()$cache$mediatagverifications$fk_personid == selectedUser(),
          ]
          
          if (nrow(verifications) != 1) {next}
          verifications$is_delete <- 1
          
          delete_mediatagverifications <- rbind(
            delete_mediatagverifications,
            verifications
          )
        }
      }
      
      verifications_cache(
        list(
          annotationverifications = delete_annotationverifications,
          annotagverifications = delete_annotagverifications,
          mediatagverifications = delete_mediatagverifications,
          modelverifications = delete_modelverifications
        )
      )
      
      updateTags2(updateTags2()+1)
    })
    
    return(reactiveValues(
      selected_annotation_rows = reactive(
        switch(
          viewer_mode,
          modelOutputs = taxon_tags()[
            getReactableState('taxon_table', 'selected'), 
            c('pk_modeloutputid'),
            drop = FALSE
          ],
          {
            the_annotations <- taxon_tags()[
              getReactableState('taxon_table', 'selected'), 
              which(names(taxon_tags()) %in% c('pk_annotationid', 'pk_annotagid', 'pk_modeloutputid'))
            ]
            
            the_mediatags <- file_tags()[
              getReactableState('file_tags_table', 'selected'),
              'pk_mediatagid',
              drop = FALSE
            ]
            
            if (nrow(the_mediatags) != 0) {
              the_mediatags[setdiff(names(the_annotations), names(the_mediatags))] <- NA
            }
            
            if (nrow(the_annotations) != 0) {
              the_annotations[setdiff(names(the_mediatags), names(the_annotations))] <- NA
            }
            
            the_annotations <- rbind(the_annotations, the_mediatags)
            
            if (mediaType == 'audio') {
              if (nrow(the_annotations) > 1) {
                the_annotations <- the_annotations[1,]
              }
            }
            
            the_annotations
          }
        )
      ),
      deleted_annotation_rows = reactive(
        deleted_record()
      ),
      verifications_cache = reactive(verifications_cache())
    ))
  })
}
