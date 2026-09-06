# load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(RSQLite)
library(DBI)
library(reactable)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "AMMonitor"),
  dashboardSidebar(
    # https://fontawesome.com/icons?d=gallery&m=free
    sidebarMenu(
      id = "tabs",
      tags$head(
        tags$style(
          ".inactiveLink {
          pointer-events: none;
          color: #333 !important;
          cursor: default;
          }"
        ),
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
      ),
      menuItem("Home", tabName = "Home", icon = icon("house")),
      menuItem("Database", tabName = "Database", icon = icon("database")),
      menuItem("Apps", tabName = "NewAnalysis", icon = icon("chart-pie")),
      menuItem("Photo Tools", tabName = "Photos", icon = icon("image")),
      menuItem("Audio Tools", tabName = "Audio", icon = icon("volume-high"))
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      # Home Page
      tabItem(
        tabName = "Home",
        my_home_ui("my_home")
      ), # end of home page
      
      
      # Database
      tabItem(
        tabName = "Database",
        tabsetPanel(
          id = "database_tabs",
          tabPanel(
            title = "Database",
            uiOutput("db_ui")
          ),
          tabPanel(
            title = "Custom Queries",
            queries_custom_ui("queries_custom")
          )
        )
      ), # end of database page
      
      # New Analysis
      tabItem(
        tabName = "NewAnalysis",
        new_analysisAMM_ui("new_analysis")
      ),
      
      #Photo facilities
      tabItem(
        tabName = "Photos",
        tabsetPanel(
          id = 'photo_tabs',
          type = 'hidden',
          tabItem(
            title = 'About',
            tabName = 'photo_tools_about',
            wellPanel(
              tags$h3('Photo Tool Options'),
              tags$p('Please select a photo tool below. Only one tool may be used per session (relaunch the app to use a new tool).'),
              fluidRow(
                column(width = 3, actionButton('photo_viewer', 'Viewer')),
                column(width = 3, actionButton('photo_tagger', 'Tagger')),
                column(width = 3, actionButton('photo_anno_verifier', 'Annotation Verifications')),
                column(width = 3, actionButton('photo_model_verifier', 'Model Verifications'))
              )
            ),
            photo_tools_about_ui("photo_tools_about")
          ),
          tabItem(
            title = 'Viewer',
            tabName = 'Viewer',
            image_viewer_ui("image_viewer", viewer_mode = "viewer"),
            annotation_viewer_tables_ui("annotation_viewer_tables_viewer", viewer_mode = "viewer")
          ),
          tabItem(
            title = 'Tagger',
            tabName = 'Tagger',
            fluidRow(
              column(
                width = 2,
                photo_annotator_ui("photo_annotator")
              ),
              column(
                width = 10,
                image_viewer_ui("image_viewer_tagger", viewer_mode = "tagger"),
                annotation_viewer_tables_ui("annotation_viewer_tables_tagger", viewer_mode = "tagger")
              )
            )
          ),
          tabItem(
            title = 'Annotation Verifications',
            tabName = 'Annotation Verifications',
            image_viewer_ui("image_verifier_viewer", viewer_mode = "verifier"),
            annotation_viewer_tables_ui('verifier_tables', viewer_mode = "verifier")
          ),
          tabItem(
            title = 'Model Verifications',
            tabName = 'Model Verifications',
            image_viewer_ui("image_modelOutputs_verifier", viewer_mode = "modelOutputs"),
            annotation_viewer_tables_ui('modelOutput_verifier_tables', viewer_mode = "modelOutputs")
          )
        )
      ),
      # Audio facilities
      tabItem(
        tabName = "Audio",
        tabsetPanel(
          id = 'audio_tabs',
          type = 'hidden',
          tabItem(
            title = 'About',
            tabName = 'audio_tools_about',
            wellPanel(
              tags$h3('Audio Tool Options'),
              tags$p('Please select an audio tool below. Only one tool may be used per session (relaunch the app to use a new tool).'),
              fluidRow(
                column(width = 3, actionButton('audio_player', 'Player')),
                column(width = 3, actionButton('audio_tagger', 'Tagger')),
                column(width = 3, actionButton('audio_anno_verifier', 'Annotation Verifications')),
                column(width = 3, actionButton('audio_model_verifier', 'Model Verifications'))
              )
            ),
            audio_tools_about_ui("audio_tools_about")
          ),
          tabItem(
            title = 'Player',
            tabName = 'Player',
            audio_player_ui("audio_player", viewer_mode = "viewer"),
            annotation_viewer_tables_ui("annotation_player_tables_player", viewer_mode = "viewer", mediaType = "audio"),
            audio_comment_box_ui("audio_player")
          ),
          tabItem(
            title = 'Tagger',
            tabName = 'Tagger',
            fluidRow(
              column(
                width = 1,
                audio_annotator_ui("audio_annotator")
              ),
              column(
                width = 11,
                audio_player_ui("audio_player_tagger", viewer_mode = "tagger")
              )
            ),
            annotation_viewer_tables_ui("annotation_player_tables_tagger", viewer_mode = "tagger", mediaType = "audio"),
            audio_comment_box_ui("audio_player_tagger")
          ),
          tabItem(
            title = 'Annotation Verifications',
            tabName = 'Annotation Verifications',
            audio_player_ui("audio_player_verifier", viewer_mode = "verifier"),
            annotation_viewer_tables_ui("annotation_player_tables_verifier", viewer_mode = "verifier", mediaType = "audio"),
            audio_comment_box_ui("audio_player_verifier")
          ),
          tabItem(
            title = 'Model Verifications',
            tabName = 'Model Verifications',
            audio_player_ui('audio_modelOutput_player', viewer_mode = "modelOutputs"),
            annotation_viewer_tables_ui('modelOutput_viewer_tables_audio', viewer_mode = "modelOutputs"),
            audio_comment_box_ui('audio_modelOutput_player')
          )
        )
      )
    ) # end of tabItems
  ), # end of dashboardBody
  tags$head(
    tags$style(
      "body {
        padding-right:0 !important;
      }"
    ),
    tags$style(
      # AdminLTE (shinydashboard's underlying theme) hardcodes its default
      # sidebar width (230px) in several places at once -- the sidebar
      # itself, the content area's left margin, the header logo, and the
      # header navbar -- all inside its own >=768px ("desktop") media
      # query. shinydashboard doesn't expose a sidebar-width option, so
      # narrowing it means overriding all of these together (only the
      # desktop breakpoint; below 768px AdminLTE turns the sidebar into an
      # overlay instead, which this leaves alone). 172.5px = 230px * 0.75,
      # 25% narrower. Narrowing the sidebar this way is also what pushes
      # the main content area (including the audio player's spectrogram,
      # which already fills the full content width) further left -- there
      # isn't a separate spectrogram-specific position to adjust.
      "@media (min-width: 768px) {
        .main-sidebar, .left-side {
          width: 172.5px;
        }
        .content-wrapper, .right-side, .main-footer {
          margin-left: 172.5px;
        }
        .main-header .logo {
          width: 172.5px;
        }
        .main-header .navbar {
          margin-left: 172.5px;
        }
      }"
    ),
    tags$style(
      # Dark theme. `body`'s own background/color covers most plain text
      # for free via normal CSS inheritance (color is an inherited
      # property, so headings/labels/paragraphs that don't set their own
      # color pick this up automatically) -- the rules below only need to
      # handle elements that set their OWN background (boxes, inputs,
      # tables, wellPanels) and so need a matching foreground color too.
      # Doesn't touch the spectrogram/waveform plots themselves (rendered
      # server-side as ggplot images) or their own color-palette dropdown
      # -- those keep whatever palette is selected there.
      "
      body, .content-wrapper, .right-side, .main-footer {
        background-color: #1a1d21 !important;
        color: #e8e8e8;
      }
      /* skin = 'black' (set on dashboardPage) only darkens the sidebar --
         that's the actual AdminLTE design, the top header/logo bar stays
         its own light color regardless of skin. Overridden here so the
         whole chrome is consistently dark. */
      .main-header .navbar, .main-header .logo, .main-header .logo:hover {
        background-color: #17191c !important;
        color: #e8e8e8 !important;
      }
      .main-header .navbar .sidebar-toggle {
        color: #e8e8e8 !important;
      }
      .box-header, .box-title {
        color: #e8e8e8 !important;
      }
      .input-group-addon {
        background-color: #2b3035 !important;
        color: #e8e8e8 !important;
        border-color: #444a50 !important;
      }
      .modal-content {
        background-color: #23272b;
        color: #e8e8e8;
      }
      .modal-header, .modal-footer {
        border-color: #3a3f44;
      }
      .box, .box-footer {
        background-color: #23272b;
        border-top-color: #3a3f44;
        color: #e8e8e8;
      }
      .box-header .btn-box-tool {
        color: #a8adb3;
      }
      .box-header .btn-box-tool:hover {
        color: #ffffff;
      }
      .well {
        background-color: #23272b !important;
        border-color: #3a3f44;
        color: #e8e8e8 !important;
      }
      .form-control,
      .selectize-input,
      select {
        background-color: #2b3035 !important;
        border-color: #444a50 !important;
        color: #e8e8e8 !important;
      }
      .form-control::placeholder {
        color: #8a8f94;
      }
      .selectize-dropdown, .selectize-dropdown-content {
        background-color: #2b3035;
        color: #e8e8e8;
        border-color: #444a50;
      }
      .selectize-dropdown .option.active,
      .selectize-dropdown-content .option.active {
        background-color: #3a4046;
      }
      .selectize-input.focus {
        border-color: #4da3ff !important;
      }
      /* !important throughout: reactable ships its own stylesheet
         (loaded as an htmlwidgets dependency after this one), which sets
         a white .rt-table/.rt-tr background at the same specificity --
         without !important these rules just lose to load order. */
      .rt-table {
        background-color: #23272b !important;
        color: #e8e8e8 !important;
      }
      .rt-thead .rt-th {
        background-color: #2b3035 !important;
        color: #e8e8e8 !important;
        border-color: #3a3f44 !important;
      }
      .rt-tr {
        border-color: #3a3f44 !important;
      }
      .rt-tr.-odd {
        background-color: #23272b !important;
      }
      .rt-tr.-even {
        background-color: #262b30 !important;
      }
      .rt-td {
        border-color: #3a3f44 !important;
      }
      .nav-tabs-custom, .nav-tabs-custom > .tab-content {
        background-color: #23272b;
      }
      .nav-tabs-custom > .nav-tabs > li.active > a {
        background-color: #23272b;
        color: #e8e8e8;
        border-color: #3a3f44;
      }
      .nav-tabs-custom > .nav-tabs > li > a {
        color: #a8adb3;
      }
      .btn-default {
        background-color: #2b3035;
        border-color: #444a50;
        color: #e8e8e8;
      }
      .btn-default:hover {
        background-color: #363c42;
        color: #ffffff;
      }
      a {
        color: #6cb6ff;
      }
      hr {
        border-top-color: #3a3f44 !important;
      }
      /* A handful of modules use a hardcoded light wellPanel background
         (skyblue/lightblue/aquamarine) as an instructional banner --
         inline styles beat external stylesheet rules, so these need
         their own !important overrides rather than relying on the
         .well rule above. */
      [style*='background: skyblue'], [style*='background:skyblue'],
      [style*='background: lightblue'], [style*='background:lightblue'],
      [style*='background: aquamarine'], [style*='background:aquamarine'] {
        background-color: #2b3035 !important;
        color: #e8e8e8 !important;
      }
      "
    ),
    tags$title("AMMonitor")
  )
) # end dashboard page
