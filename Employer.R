library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)

ui <- fluidPage(
  titlePanel("Employer Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Clustering Settings"),
      numericInput("clusters", "Number of Clusters:", 2, min = 1, max = 5),
      actionButton("refresh_clusters", "Refresh Clustering")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Applications Table", tableOutput("applications_table")),
        tabPanel("Clustering Visualization", plotOutput("kmeans_plot")),
        tabPanel("Clustered Applications", tableOutput("clustered_table"))
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
    if (nrow(data) > 0) {
      numeric_data <- data[, c("Experience", "SkillCount")]
      set.seed(123)
      kmeans_result <- kmeans(numeric_data, centers = input$clusters)
      data$Cluster <- as.factor(kmeans_result$cluster)
    }
    data
  })
  
  output$applications_table <- renderTable({
    get_applications()
  })
  
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
  
  output$clustered_table <- renderTable({
    clustered_data()
  })
}

shinyApp(ui, server)