library(shiny)
library(DBI)
library(RSQLite)

ui <- fluidPage(
  titlePanel("Job Seeker Portal"),
  sidebarLayout(
    sidebarPanel(
      h4("Apply for a Job"),
      textInput("app_name", "Name:", placeholder = "Enter your name"),
      textInput("app_skills", "Skills:", placeholder = "e.g., Python, R"),
      textInput("app_location", "Location:", placeholder = "Enter your location"),
      numericInput("app_experience", "Experience (Years):", 0, min = 0, max = 50),
      actionButton("submit_app", "Submit Application")
    ),
    mainPanel(
      h4("Status Message"),
      verbatimTextOutput("status_message"),
      h4("Your Submitted Applications"),
      tableOutput("submitted_apps_table")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$submit_app, {
    db <- dbConnect(SQLite(), "applications.db")
    
    existing_user <- dbGetQuery(db, "SELECT * FROM Applications WHERE Name = ?",
                                params = list(input$app_name))
    
    if (nrow(existing_user) > 0) {
      output$status_message <- renderText({
        "You have already submitted an application. Only one application per user is allowed."
      })
    } else {
      dbExecute(db, "
        INSERT INTO Applications (Name, Skills, Location, Experience, SkillCount)
        VALUES (?, ?, ?, ?, ?)",
                params = list(
                  input$app_name,
                  input$app_skills,
                  input$app_location,
                  input$app_experience,
                  length(strsplit(input$app_skills, ",")[[1]])
                )
      )
      
      output$status_message <- renderText({
        "Application successfully submitted!"
      })
    }
    
    dbDisconnect(db)
  })
  
  output$submitted_apps_table <- renderTable({
    db <- dbConnect(SQLite(), "applications.db")
    apps <- dbReadTable(db, "Applications")
    dbDisconnect(db)
    apps
  })
}

shinyApp(ui, server)