library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)
library(cluster)

ui <- fluidPage(
  titlePanel("Employer Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Clustering Settings"),
      numericInput("clusters", "Number of Clusters (K):", 2, min = 1, max = 10),
      actionButton("refresh_clusters", "Refresh Clustering"),
      br(), br(),
      h4("Clustering Evaluation"),
      verbatimTextOutput("silhouette_text"),
      verbatimTextOutput("withinss_text"),
      verbatimTextOutput("suggested_k")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Applications Table", tableOutput("applications_table")),
        tabPanel("Clustering Visualization", plotOutput("kmeans_plot")),
        tabPanel("Clustered Applications", tableOutput("clustered_table")),
        tabPanel("Elbow Method", plotOutput("elbow_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Load application data from SQLite
  get_applications <- reactive({
    db <- dbConnect(SQLite(), "applications.db")
    apps <- dbReadTable(db, "Applications")
    dbDisconnect(db)
    apps
  })
  
  # Perform K-means clustering
  clustered_data <- reactive({
    data <- get_applications()
    if (nrow(data) > 0) {
      numeric_data <- data[, c("Experience", "SkillCount")]
      set.seed(123)
      km <- kmeans(numeric_data, centers = input$clusters)
      data$Cluster <- as.factor(km$cluster)
    }
    data
  })
  
  # Silhouette Score
  silhouette_score <- reactive({
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    km <- kmeans(numeric_data, centers = input$clusters)
    sil <- silhouette(km$cluster, dist(numeric_data))
    round(mean(sil[, 3]), 3)
  })
  
  # Within-Cluster Sum of Squares (WSS)
  withinss_score <- reactive({
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    km <- kmeans(numeric_data, centers = input$clusters)
    round(km$tot.withinss, 2)
  })
  
  # Suggested K using Elbow Method
  suggested_k <- reactive({
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    wss <- sapply(1:10, function(k) {
      kmeans(numeric_data, centers = k, nstart = 10)$tot.withinss
    })
    deltas <- diff(wss)
    elbow_point <- which.min(abs(deltas[-1] - deltas[-length(deltas)])) + 1
    elbow_point
  })
  
  # Output: Applications Table
  output$applications_table <- renderTable({
    get_applications()
  })
  
  # Output: K-Means Clustering Plot
  output$kmeans_plot <- renderPlot({
    data <- clustered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = Experience, y = SkillCount, color = Cluster)) +
        geom_point(size = 4) +
        theme_minimal() +
        labs(title = "K-Means Clustering of Applications",
             x = "Experience (Years)", y = "Number of Skills")
    }
  })
  
  # Output: Clustered Table
  output$clustered_table <- renderTable({
    clustered_data()
  })
  
  # Output: Silhouette Score
  output$silhouette_text <- renderPrint({
    paste("Average Silhouette Score:", silhouette_score())
  })
  
  # Output: WSS
  output$withinss_text <- renderPrint({
    paste("Total Within-Cluster Sum of Squares (WSS):", withinss_score())
  })
  
  # Output: Suggested K
  output$suggested_k <- renderPrint({
    paste("Suggested Optimal K (Elbow Heuristic):", suggested_k())
  })
  
  # Output: Elbow Plot
  output$elbow_plot <- renderPlot({
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    wss <- sapply(1:10, function(k) {
      kmeans(numeric_data, centers = k, nstart = 10)$tot.withinss
    })
    plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
         xlab = "Number of Clusters (K)",
         ylab = "Total Within-Cluster Sum of Squares (WSS)",
         main = "Elbow Method for Optimal K")
    abline(v = suggested_k(), col = "blue", lty = 2)
    abline(v = input$clusters, col = "red", lty = 3)
    legend("topright", legend = c("Suggested K", "Selected K"),
           col = c("blue", "red"), lty = c(2, 3), bty = "n")
  })
}

shinyApp(ui, server)
