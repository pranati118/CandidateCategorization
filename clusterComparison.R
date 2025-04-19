library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)
library(cluster)

ui <- fluidPage(
  titlePanel("Clustering Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("algorithm", "Clustering Algorithm:",
                  choices = c("K-Means", "Hierarchical")),
      numericInput("clusters", "Number of Clusters (K):", 3, min = 1, max = 10),
      br(),
      h4("Clustering Evaluation"),
      verbatimTextOutput("silhouette_text"),
      conditionalPanel(
        condition = "input.algorithm == 'K-Means'",
        p("Suggested K (Silhouette Method):"),
        verbatimTextOutput("suggested_k_sil"),
        p("Suggested K (Elbow Method - WSS):"),
        verbatimTextOutput("suggested_k_elbow"),
        p("WSS for selected K:"),
        verbatimTextOutput("wss_text")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Applications Table", tableOutput("applications_table")),
        tabPanel("Cluster Visualization", plotOutput("kmeans_plot")),
        tabPanel("Clustered Table", tableOutput("clustered_table")),
        tabPanel("Algorithm Comparison", tableOutput("comparison_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  get_applications <- reactive({
    db <- dbConnect(SQLite(), "applications.db")
    apps <- dbReadTable(db, "Applications")
    dbDisconnect(db)
    apps
  })
  
  clustered_data <- reactive({
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    
    if (input$algorithm == "K-Means") {
      set.seed(123)
      km <- kmeans(numeric_data, centers = input$clusters)
      data$Cluster <- as.factor(km$cluster)
    } else {
      hc <- hclust(dist(numeric_data))
      clust <- cutree(hc, k = input$clusters)
      data$Cluster <- as.factor(clust)
    }
    
    data
  })
  
  silhouette_score <- reactive({
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    
    if (input$algorithm == "K-Means") {
      km <- kmeans(numeric_data, centers = input$clusters)
      sil <- silhouette(km$cluster, dist(numeric_data))
    } else {
      hc <- hclust(dist(numeric_data))
      clust <- cutree(hc, k = input$clusters)
      sil <- silhouette(clust, dist(numeric_data))
    }
    
    round(mean(sil[, 3]), 3)
  })
  
  wss_value <- reactive({
    if (input$algorithm != "K-Means") return(NULL)
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    km <- kmeans(numeric_data, centers = input$clusters)
    round(km$tot.withinss, 2)
  })
  
  suggested_k_sil <- reactive({
    if (input$algorithm != "K-Means") return(NULL)
    
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    
    sil_scores <- sapply(2:10, function(k) {
      km <- kmeans(numeric_data, centers = k)
      mean(silhouette(km$cluster, dist(numeric_data))[, 3])
    })
    
    which.max(sil_scores) + 1
  })
  
  suggested_k_elbow <- reactive({
    if (input$algorithm != "K-Means") return(NULL)
    
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    
    wss <- sapply(1:10, function(k) {
      kmeans(numeric_data, centers = k, nstart = 10)$tot.withinss
    })
    
    # Elbow method: Look for maximum second derivative (simplified "knee" detection)
    diffs <- diff(wss)
    elbows <- diff(diffs)
    elbow_k <- which.min(elbows) + 1
    elbow_k
  })
  
  output$silhouette_text <- renderPrint({
    score <- silhouette_score()
    algo <- input$algorithm
    paste("Silhouette Score for", algo, ":", score)
  })
  
  output$suggested_k_sil <- renderPrint({
    suggested_k_sil()
  })
  
  output$suggested_k_elbow <- renderPrint({
    suggested_k_elbow()
  })
  
  output$wss_text <- renderPrint({
    wss_value()
  })
  
  output$applications_table <- renderTable({
    get_applications()
  })
  
  output$kmeans_plot <- renderPlot({
    data <- clustered_data()
    ggplot(data, aes(x = Experience, y = SkillCount, color = Cluster)) +
      geom_point(size = 4, alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("Clustering using", input$algorithm),
           x = "Experience", y = "Skill Count")
  })
  
  output$clustered_table <- renderTable({
    clustered_data()
  })
  
  output$comparison_table <- renderTable({
    data <- get_applications()
    numeric_data <- data[, c("Experience", "SkillCount")]
    
    results <- data.frame(
      Algorithm = character(),
      `Silhouette Score` = numeric(),
      `Clusters Identified` = integer(),
      stringsAsFactors = FALSE
    )
    
    # K-Means
    km <- kmeans(numeric_data, centers = input$clusters)
    sil_km <- silhouette(km$cluster, dist(numeric_data))
    results <- rbind(results, data.frame(
      Algorithm = "K-Means",
      `Silhouette Score` = round(mean(sil_km[, 3]), 3),
      `Clusters Identified` = length(unique(km$cluster))
    ))
    
    # Hierarchical
    hc <- hclust(dist(numeric_data))
    clust_h <- cutree(hc, k = input$clusters)
    sil_h <- silhouette(clust_h, dist(numeric_data))
    results <- rbind(results, data.frame(
      Algorithm = "Hierarchical",
      `Silhouette Score` = round(mean(sil_h[, 3]), 3),
      `Clusters Identified` = length(unique(clust_h))
    ))
    
    results
  })
}

shinyApp(ui, server)
