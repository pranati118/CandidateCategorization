library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Employer Search & Clustering Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Search Applicants"),
      textInput("search_term", "Search by Name, Skill, or Location:", placeholder = "e.g., Python or New York"),
      actionButton("search_btn", "Search & Cluster")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Search Results", tableOutput("search_results")),
        tabPanel("Cluster Analysis", plotOutput("kmeans_plot"))
      )
    )
  )
)

server <- function(input, output, session) {

  search_data <- eventReactive(input$search_btn, {
    db <- dbConnect(SQLite(), "applications.db")

    search_query <- paste0("%", input$search_term, "%")
    results <- dbGetQuery(db, "
      SELECT * FROM Applications 
      WHERE Name LIKE ? OR Skills LIKE ? OR Location LIKE ?",
                          params = list(search_query, search_query, search_query)
    )
    
    dbDisconnect(db)
    return(results)
  })

  output$search_results <- renderTable({
    data <- search_data()
    if (nrow(data) == 0) {
      return(data.frame(Message = "No matching applicants found."))
    }
    data
  })

  output$kmeans_plot <- renderPlot({
    data <- search_data()
    if (nrow(data) > 2) { 
      data <- data %>%
        mutate(SkillCount = sapply(strsplit(Skills, ","), length)) %>%
        select(Experience, SkillCount) %>%
        na.omit()

      set.seed(123)
      kmeans_result <- kmeans(data, centers = 3)

      ggplot(data, aes(x = Experience, y = SkillCount, color = as.factor(kmeans_result$cluster))) +
        geom_point(size = 4) +
        labs(title = "Cluster Analysis on Search Results", x = "Experience (Years)", y = "Skill Count") +
        theme_minimal()
    } else {
      ggplot() +
        labs(title = "Not enough applicants for clustering. Try a broader search.")
    }
  })
}
shinyApp(ui, server)