if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("DBI")) install.packages("DBI")
if (!require("RSQLite")) install.packages("RSQLite")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Employer Search and Clustering Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Search Applicants"),
      textInput("skill_search", "Search by Skills:", placeholder = "e.g., Python, SQL"),
      actionButton("search_skills_btn", "Search Skills"),
      br(),
      textInput("location_search", "Search by Location:", placeholder = "e.g., New York"),
      actionButton("search_location_btn", "Search Location"),
      br(),
      h4("Clustering Options"),
      numericInput("num_clusters_skills", "Clusters for Skills (K):", value = 3, min = 2),
      actionButton("cluster_skills_btn", "Cluster Skills"),
      br(),
      numericInput("num_clusters_location", "Clusters for Location (K):", value = 3, min = 2),
      actionButton("cluster_location_btn", "Cluster Location")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("All Applications", tableOutput("all_applications")),
        tabPanel("Search by Skills", tableOutput("skill_results")),
        tabPanel("Search by Location", tableOutput("location_results")),
        tabPanel(
          "Clustering by Skills",
          plotOutput("skills_cluster_plot"),
          tableOutput("skills_cluster_table")
        ),
        tabPanel(
          "Clustering by Location",
          plotOutput("location_cluster_plot"),
          tableOutput("location_cluster_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  get_applications <- reactive({
    tryCatch({
      db <- dbConnect(SQLite(), "applications.db")
      applications <- dbReadTable(db, "Applications")
      dbDisconnect(db)
      return(applications)
    }, error = function(e) {
      print(paste("Error:", e$message))
      return(data.frame(Message = "Could not connect to the database."))
    })
  })

  output$all_applications <- renderTable({
    get_applications()
  })

  skill_search_results <- eventReactive(input$search_skills_btn, {
    applications <- get_applications()
    if ("Skills" %in% colnames(applications)) {
      filtered <- applications %>%
        filter(grepl(input$skill_search, Skills, ignore.case = TRUE))
    } else {
      filtered <- data.frame(Message = "Skills column not found in the database.")
    }
    return(filtered)
  })

  output$skill_results <- renderTable({
    results <- skill_search_results()
    if (nrow(results) == 0) {
      data.frame(Message = "No applicants found for the given skills.")
    } else {
      results
    }
  })

  location_search_results <- eventReactive(input$search_location_btn, {
    applications <- get_applications()
    if ("Location" %in% colnames(applications)) {
      filtered <- applications %>%
        filter(grepl(input$location_search, Location, ignore.case = TRUE))
    } else {
      filtered <- data.frame(Message = "Location column not found in the database.")
    }
    return(filtered)
  })

  output$location_results <- renderTable({
    results <- location_search_results()
    if (nrow(results) == 0) {
      data.frame(Message = "No applicants found for the given location.")
    } else {
      results
    }
  })

  skills_clustering <- eventReactive(input$cluster_skills_btn, {
    data_to_cluster <- skill_search_results()
    
    numeric_data <- data_to_cluster %>%
      select(where(is.numeric)) %>%
      na.omit()
    
    if (nrow(numeric_data) >= input$num_clusters_skills) {
      set.seed(123)
      kmeans_result <- kmeans(numeric_data, centers = input$num_clusters_skills)
      
      data_to_cluster$Cluster <- as.factor(kmeans_result$cluster)
    } else {
      data_to_cluster <- data.frame(
        Message = "Not enough data for clustering by skills."
      )
    }
    return(data_to_cluster)
  })

  location_clustering <- eventReactive(input$cluster_location_btn, {
    data_to_cluster <- location_search_results()
    
   
    numeric_data <- data_to_cluster %>%
      select(where(is.numeric)) %>%
      na.omit()
    
  
    if (nrow(numeric_data) >= input$num_clusters_location) {
      set.seed(123)
      kmeans_result <- kmeans(numeric_data, centers = input$num_clusters_location)
      
      
      data_to_cluster$Cluster <- as.factor(kmeans_result$cluster)
    } else {
      data_to_cluster <- data.frame(
        Message = "Not enough data for clustering by location."
      )
    }
    return(data_to_cluster)
  })
  
 
  output$skills_cluster_plot <- renderPlot({
    data <- skills_clustering()
    if (!"Message" %in% colnames(data)) {
      ggplot(data, aes(x = Experience, y = SkillCount, color = Cluster)) +
        geom_point(size = 3) +
        labs(title = "Skills Clustering", x = "Experience", y = "Skill Count") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "Not enough data for clustering.", cex = 1.5)
    }
  })
  

  output$skills_cluster_table <- renderTable({
    skills_clustering()
  })
  

  output$location_cluster_plot <- renderPlot({
    data <- location_clustering()
    if (!"Message" %in% colnames(data)) {
      ggplot(data, aes(x = Experience, y = SkillCount, color = Cluster)) +
        geom_point(size = 3) +
        labs(title = "Location Clustering", x = "Experience", y = "Skill Count") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "Not enough data for clustering.", cex = 1.5)
    }
  })
  
 
  output$location_cluster_table <- renderTable({
    location_clustering()
  })
}


shinyApp(ui = ui, server = server)
