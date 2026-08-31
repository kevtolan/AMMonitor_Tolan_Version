# load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(RSQLite)
library(DBI)
library(reactable)

ui <- dashboardPage(
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
                width = 2,
                audio_annotator_ui("audio_annotator")
              ),
              column(
                width = 10,
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
    tags$title("AMMonitor")
  )
) # end dashboard page
