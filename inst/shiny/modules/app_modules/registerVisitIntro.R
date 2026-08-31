#!! ModName = registerVisitIntro
#!! ModDisplayName = visit_registration_intro
#!! ModDescription = Intro / Instructions to the UI approach to registering a visit
#!! ModCitation = Laurence Clarfeld.  (2023). registerVisitIntro. [Source code].
#!! ModNotes = 
#!! ModActive = 1


# the ui function
registerVisitIntro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h2('Register a Visit'),
    tags$p(
      tags$b('What:'),
      'The "visit" is a core feature of the AMMonitor database that allows you to relate media files to people, equipment, and locations. Each field visit where monitoring equipment is set, checked, or pulled, is represented by a row in the database "visits" table. Visit metadata include the visit date, time, equipment, person, and location. This app is used to register visits in the databse, along with any media files that were collected during the visit.'
    ),
    tags$p(
      tags$b('Why:'),
      'First and foremost, this is how you easily register media files into your database. But tracking field visits provides many other benefits for project management, including tracking deployed equipment and measuring equipment performance.'
    ),
    tags$p(
      tags$b('How:'),
      'Before starting, make sure you have all site visit metadata available, which minimally includes the visit date, type, and location. If media files were collected during the site visit, those should be available through a local filepath (such as via an SD card reader, external hard drive, or stored on disk). Then, advance through each tab in the app, entering the required information as you go.'
    ),
    tags$p(
      tags$b('A few notes about visit "types":'),
      'There are 3 different visit types:',
      tags$ul(
        tags$li(tags$b('Set:'), 'A camera/recorder is placed.'),
        tags$li(tags$b('Check:'), 'A deployed camera/recorder is checked.'),
        tags$li(tags$b('Pull:'), 'A deployed camera/recorder is retrieved.')
      ),
      'Note that media files are never collected during a "set" visit, becuase the equipment is only just being deployed. During "check" and "pull" visits, SD cards with media files may be collected. Here are a few common scenarios that can cause confusion:',
      tags$ul(
        tags$li(tags$b('A deployed camera/recorder is moved:'), 'Let\'s say a camera/recorder is deployed at location "A" and, during a field visit, it is moved to location "B". In this scenario, a "pull" visit would be registered for location "A", and any collected media would be associated with this visit. Then, a "set" visit would be registerd for location "B".'),
        tags$li(tags$b('A deployed camera/recorder is replaced:'), 'Let\'s say a camera/recorder "A" is deployed at a location and, during a field visit, it is swapped out with camera/recorder "B". In this scenario, a "pull" visit would be registered for camera/recorder "A" at the location, and any collected media would be associated with this visit. Then, a "set" visit would be registerd for camera/recorder "B" at the same location.'),
      )
    )
  )
}


# the server function
registerVisitIntro_server <- function(id, argName1, argName2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
