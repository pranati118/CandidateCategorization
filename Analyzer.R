library(shiny)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(cluster)

# Connect to database
db <- dbConnect(SQLite(), "applications.db")

# UI
ui <- fluidPage(
  titlePanel("Job Analyzer Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Options"),
      selectInput("skill_filter", "Filter by Skill:", 
                  choices = c("All", "Python", "R", "C", "Java", "Testing", "Cloud", "AWS", "AIML", "ML", "Photography", "Video Editing")),
      sliderInput("exp_range", "Experience Range (Years):", 
                  min = 0, max = 10, value = c(0, 10)),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Skills Distribution", plotOutput("skills_plot")),
        tabPanel("Location Distribution", plotOutput("location_plot")),
        tabPanel("Experience Analysis", plotOutput("exp_plot")),
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Fetch data from the database
  fetch_data <- reactive({
    query <- "SELECT * FROM Applications"
    data <- dbGetQuery(db, query)
    data <- data %>% filter(Experience >= input$exp_range[1],
                            Experience <= input$exp_range[2])
    if (input$skill_filter != "All") {
      data <- data %>% filter(grepl(input$skill_filter, Skills, ignore.case = TRUE))
    }
    return(data)
  })
  
  # Plot Skills Distribution
  output$skills_plot <- renderPlot({
    data <- fetch_data()
    ggplot(data, aes(x = Skills)) +
      geom_bar(fill = "skyblue") +
      labs(title = "Skills Distribution", x = "Skills", y = "Count") +
      theme_minimal()
  })
  
  # Plot Location Distribution
  output$location_plot <- renderPlot({
    data <- fetch_data()
    ggplot(data, aes(x = Location)) +
      geom_bar(fill = "lightgreen") +
      labs(title = "Location Distribution", x = "Location", y = "Count") +
      theme_minimal()
  })
  
  # Plot Experience Analysis
  output$exp_plot <- renderPlot({
    data <- fetch_data()
    ggplot(data, aes(x = Experience)) +
      geom_histogram(binwidth = 1, fill = "orange", color = "black") +
      labs(title = "Experience Analysis", x = "Years of Experience", y = "Frequency") +
      theme_minimal()
  })
  
 
}

# Run App
shinyApp(ui, server)
