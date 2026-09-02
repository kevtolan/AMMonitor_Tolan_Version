server <- function(input, output, session) {
  
  my_home_outputs <- my_home_server("my_home")
  
  disableMenuItems <- function( ) {
    addCssClass(selector = "a[data-value='Home']", class = "inactiveLink")
    addCssClass(selector = "a[data-value='Database']", class = "inactiveLink")
    addCssClass(selector = "a[data-value='NewAnalysis']", class = "inactiveLink")
    addCssClass(selector = "a[data-value='Photos']", class = "inactiveLink")
    addCssClass(selector = "a[data-value='Audio']", class = "inactiveLink")
  }
  
  #Disable menuitem when the app loads
  addCssClass(selector = "a[data-value='Database']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='NewAnalysis']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='Photos']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='Audio']", class = "inactiveLink")
  
  # Enable menuitems when database connection is made
  observe({
    req(con())
    
    # Enable the menu item links
    removeCssClass(selector = "a[data-value='Home']", class = "inactiveLink")
    removeCssClass(selector = "a[data-value='Database']", class = "inactiveLink")
    removeCssClass(selector = "a[data-value='NewAnalysis']", class = "inactiveLink")
    removeCssClass(selector = "a[data-value='Photos']", class = "inactiveLink")
    removeCssClass(selector = "a[data-value='Audio']", class = "inactiveLink")
  })
  
  # Database tab, database
  database_loaded <- reactiveVal(FALSE)
  observe({withProgress(message = 'Loading Database',{
    req(con())
    if (isolate(database_loaded() == FALSE)) {
      req(input$tabs == "Database" && input$database_tabs == "Database")
      database_loaded(TRUE)
      output$db_ui <- renderUI({my_db_ui("my_db")})
    }
  })})
  
  tabletabs_loaded <- reactiveVal(character(0))
  observe({
    req(con())
    my_db_output <- my_db_server("my_db")
    if (!is.null(my_db_output$db_tab_selected) && !my_db_output$db_tab_selected %in% tabletabs_loaded()) {
      tables <- shiny_table$fk_tablename[
        shiny_table$primary_tab == my_db_output$db_tab_selected
      ]
      lapply(
        X = tables,
        FUN = function(X) {
          table_server(X)
        }
      ) #end lapply
      tabletabs_loaded(c(tabletabs_loaded(), my_db_output$db_tab_selected))
    }
  })
  
  # Database tab, built-in queries
  custom_queries_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(custom_queries_loaded() == FALSE)) {
      req(input$tabs == "Database" && input$database_tabs == "Custom Queries")
      custom_queries_loaded(TRUE)
    }
    
    queries_custom_server('queries_custom')
  })
  
  # New Analysis Tab
  observe({
    req(con())
    new_analysisAMM_server(
      id = "new_analysis",
      tabSelect = reactive(input$tabs)
    )
  })
  
  # Activate "Photo Tools" Tabs
  observeEvent(input$photo_viewer, {
    updateTabsetPanel(session, 'photo_tabs', selected = 'Viewer')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$photo_tagger, {
    updateTabsetPanel(session, 'photo_tabs', selected = 'Tagger')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$photo_anno_verifier, {
    updateTabsetPanel(session, 'photo_tabs', selected = 'Annotation Verifications')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$photo_model_verifier, {
    updateTabsetPanel(session, 'photo_tabs', selected = 'Model Verifications')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  # Activate "Audio Tools" Tabs
  observeEvent(input$audio_player, {
    updateTabsetPanel(session, 'audio_tabs', selected = 'Player')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$audio_tagger, {
    updateTabsetPanel(session, 'audio_tabs', selected = 'Tagger')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$audio_anno_verifier, {
    updateTabsetPanel(session, 'audio_tabs', selected = 'Annotation Verifications')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$audio_model_verifier, {
    updateTabsetPanel(session, 'audio_tabs', selected = 'Model Verifications')
    disableMenuItems()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  # Photos tab, viewer
  photo_viewer_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(photo_viewer_loaded() == FALSE)) {
      req(input$tabs == "Photos" && input$photo_tabs == "Viewer")
      isolate(photo_viewer_loaded(TRUE))
    }
    photo_viewer_output_viewer <- image_viewer_server(
      id = "image_viewer",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Viewer"),
      selected_rows = reactive(viewer_tables_output$selected_annotation_rows())
    )
    
    viewer_tables_output <- annotation_viewer_tables_server(
      id = "annotation_viewer_tables_viewer",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Viewer"),
      file_name = reactive(photo_viewer_output_viewer$photo_name()),
      viewer_mode = 'viewer',
      viewModelOutputs = reactive(photo_viewer_output_viewer$viewModelOutputs()),
      metadata_cache = reactive(photo_viewer_output_viewer$metadata_cache())
    )
  })
  
  # Photos tab, Tagger
  photo_tagger_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(photo_tagger_loaded() == FALSE)) {
      req(input$tabs == "Photos" && input$photo_tabs == "Tagger")
      photo_tagger_loaded(TRUE)
    }
    
    photo_viewer_output_tagger <- image_viewer_server(
      id = "image_viewer_tagger",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Tagger"),
      viewer_mode = 'tagger',
      updateTags = photo_annotator_output$annoUpdate,
      annotations_cache = reactive(photo_annotator_output$annotations_cache()),
      selected_rows = reactive(tagger_tables_output$selected_annotation_rows()),
      deleted_rows = reactive(tagger_tables_output$deleted_annotation_rows())
    )
    
    photo_annotator_output <- photo_annotator_server(
      id = 'photo_annotator',
      selectedUser = reactive(my_home_outputs$selectedUser()),
      photo_name = reactive(photo_viewer_output_tagger$photo_name()),
      last_photo_name = photo_viewer_output_tagger$last_photo_name,
      the_bboxes = reactive(photo_viewer_output_tagger$bboxes()),
      autosave_rate = reactive(photo_viewer_output_tagger$autosave_rate()),
      metadata_cache = reactive(photo_viewer_output_tagger$metadata_cache()),
      deleted_rows = reactive(tagger_tables_output$deleted_annotation_rows()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Tagger")
    )
    
    tagger_tables_output <- annotation_viewer_tables_server(
      id = "annotation_viewer_tables_tagger",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Tagger"),
      file_name = reactive(photo_viewer_output_tagger$photo_name()),
      updateTags = photo_annotator_output$annoUpdate,
      viewer_mode = 'tagger',
      metadata_cache = reactive(photo_viewer_output_tagger$metadata_cache())
    )
  })
  
  # Photos tab, annotation Verifier
  photo_verifier_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(photo_verifier_loaded() == FALSE)) {
      # req(input$tabs == "Photos" && input$photo_tabs == "Verifier")
      req(input$tabs == "Photos" && input$photo_tabs == "Annotation Verifications")
      photo_verifier_loaded(TRUE)
    }
    
    image_verifier_viewer_output <- image_viewer_server(
      id = "image_verifier_viewer",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Annotation Verifications"),
      viewer_mode = "verifier",
      selected_rows = reactive(verifier_tables_output$selected_annotation_rows()),
      verifications_cache = reactive(verifier_tables_output$verifications_cache())
    )
    
    verifier_tables_output <- annotation_viewer_tables_server(
      id = "verifier_tables",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Annotation Verifications"),
      file_name = reactive(image_verifier_viewer_output$photo_name()),
      updateTags = reactive(verifier_tables_output$annoUpdate),
      viewer_mode = 'verifier',
      metadata_cache = reactive(image_verifier_viewer_output$metadata_cache())
    )
  })
  
  # Photos tab, model Verifier
  photo_model_verifier_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(photo_model_verifier_loaded() == FALSE)) {
      req(input$tabs == "Photos" && input$photo_tabs == "Model Verifications")
      photo_model_verifier_loaded(TRUE)
    }
    
    image_modelOutput_viewer_output <- image_viewer_server(
      id = "image_modelOutputs_verifier",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Model Verifications"),
      viewer_mode = 'modelOutputs',
      selected_rows = reactive(modelOutput_viewer_tables_output$selected_annotation_rows()),
      verifications_cache = reactive(modelOutput_viewer_tables_output$verifications_cache())
    )
    
    modelOutput_viewer_tables_output <- annotation_viewer_tables_server(
      id = "modelOutput_verifier_tables",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Photos" && input$photo_tabs == "Model Verifications"),
      file_name = reactive(image_modelOutput_viewer_output$photo_name()),
      # updateTags = modelOutput_viewer_tables_output$annoUpdate, # Double-check this input
      viewer_mode = 'modelOutputs',
      confValue = reactive(image_modelOutput_viewer_output$modelConf()),
      lessThan = reactive(image_modelOutput_viewer_output$modelLessThan()),
      metadata_cache = reactive(image_modelOutput_viewer_output$metadata_cache())
    )
  })
  
  # Audio tab, Player
  audio_player_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(audio_player_loaded() == FALSE)) {
      req(input$tabs == "Audio" && input$audio_tabs == "Player")
      audio_player_loaded(TRUE)
    }
    
    audio_player_output_player <- audio_player_server(
      id = "audio_player",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Player"),
      viewer_mode = "viewer",
      selected_rows = reactive(audio_player_tables_output$selected_annotation_rows())
    )
    
    audio_player_tables_output <- annotation_viewer_tables_server(
      id = "annotation_player_tables_player",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Player"),
      file_name = reactive(audio_player_output_player$audio_name()),
      viewer_mode = "viewer",
      viewModelOutputs = reactive(audio_player_output_player$viewModelOutputs()),
      metadata_cache = reactive(audio_player_output_player$metadata_cache()),
      mediaType = "audio"
    )
  })
  
  # Audio tab, Tagger
  audio_tagger_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(audio_tagger_loaded() == FALSE)) {
      req(input$tabs == "Audio" && input$audio_tabs == "Tagger")
      audio_tagger_loaded(TRUE)
    }
    
    # Server scripts for audio tagger -------------
    audio_player_output_tagger <- audio_player_server(
      id = "audio_player_tagger",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Tagger"),
      updateTags = reactive(audio_annotator_output$annoUpdate()),
      viewer_mode = "tagger",
      annotations_cache = reactive(audio_annotator_output$annotations_cache()),
      selected_rows = reactive(audio_tagger_tables_output$selected_annotation_rows()),
      deleted_rows = reactive(audio_tagger_tables_output$deleted_annotation_rows())
    )
    
    audio_annotator_output <- audio_annotator_server(
      id = 'audio_annotator',
      selectedUser = reactive(my_home_outputs$selectedUser()),
      audio_name = reactive(audio_player_output_tagger$audio_name()),
      last_audio_name = reactive(audio_player_output_tagger$last_audio_name()),
      the_bboxes = reactive(audio_player_output_tagger$bboxes()),
      metadata_cache = reactive(audio_player_output_tagger$metadata_cache()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Tagger")
    )
    
    audio_tagger_tables_output <- annotation_viewer_tables_server(
      id = "annotation_player_tables_tagger",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Tagger"),
      file_name = reactive(audio_player_output_tagger$audio_name()),
      updateTags = reactive(audio_annotator_output$annoUpdate()),
      viewer_mode = "tagger",
      metadata_cache = reactive(audio_player_output_tagger$metadata_cache()),
      mediaType = "audio"
    )
  })
  
  # Audio tab, annotation verifier
  audio_anno_verifier_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    if (isolate(audio_anno_verifier_loaded() == FALSE)) {
      req(input$tabs == "Audio" && input$audio_tabs == "Annotation Verifications")
      audio_anno_verifier_loaded(TRUE)
    }
    
    audio_player_output_verifier <- audio_player_server(
      id = "audio_player_verifier",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Annotation Verifications"),
      # updateTags = reactive(audio_verifier_tables_output$annoUpdate()),
      viewer_mode = "verifier",
      selected_rows = reactive(audio_verifier_tables_output$selected_annotation_rows()),
      verifications_cache = reactive(audio_verifier_tables_output$verifications_cache())
    )
    
    audio_verifier_tables_output <- annotation_viewer_tables_server(
      id = "annotation_player_tables_verifier",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Annotation Verifications"),
      file_name = reactive(audio_player_output_verifier$audio_name()),
      # updateTags = reactive(audio_verifier_tables_output$annoUpdate()),
      viewer_mode = "verifier",
      metadata_cache = reactive(audio_player_output_verifier$metadata_cache()),
      mediaType = "audio"
    )
  })
  
  # Audio tab, annotation verifier
  audio_model_verifier_loaded <- reactiveVal(FALSE)
  observe({
    req(con())
    # Unconditional req(), not the isolate()-guarded if() this used to be:
    # that only gated *when* the block first proceeded past this point, not
    # whether it could run again later. Once con()/input$tabs/input$audio_tabs
    # settle, Shiny can re-trigger this observe() (e.g. from reactive reads
    # inside the modules just below attaching new dependencies to it), which
    # re-called annotation_viewer_tables_server()/audio_player_server() with
    # the same ids a second time -- moduleServer() doesn't support that, and
    # it surfaced as "object 'audio_modelOutput_player_output' not found"
    # elsewhere in the app. This req() makes every run after the first a
    # true no-op.
    req(isolate(audio_model_verifier_loaded()) == FALSE)
    req(input$tabs == "Audio" && input$audio_tabs == "Model Verifications")
    audio_model_verifier_loaded(TRUE)

    audio_modelOutput_tables_output <- annotation_viewer_tables_server(
      id = "modelOutput_viewer_tables_audio",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Model Verifications"),
      file_name = reactive(audio_modelOutput_player_output$audio_name()),
      # updateTags = reactive(audio_modelOutput_tables_output$annoUpdate()), # Double-check this input
      viewer_mode = 'modelOutputs',
      metadata_cache = reactive(audio_modelOutput_player_output$metadata_cache()),
      mediaType = "audio",
      confValue = reactive(audio_modelOutput_player_output$modelConf()),
      lessThan = reactive(audio_modelOutput_player_output$modelLessThan())
    )
    
    audio_modelOutput_player_output <- audio_player_server(
      id = "audio_modelOutput_player",
      selectedUser = reactive(my_home_outputs$selectedUser()),
      active = reactive(input$tabs == "Audio" && input$audio_tabs == "Model Verifications"),
      # updateTags = reactive(audio_modelOutput_tables_output$annoUpdate()),
      viewer_mode = "modelOutputs",
      selected_rows = reactive(audio_modelOutput_tables_output$selected_annotation_rows()),
      verifications_cache = reactive(audio_modelOutput_tables_output$verifications_cache())
    )
  })
} # end of server function
