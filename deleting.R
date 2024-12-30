library(shiny)
library(RSQLite)
library(DBI)

# Connect to the database
db <- dbConnect(SQLite(), "applications.db")

# UI
ui <- fluidPage(
  titlePanel("Manage Applications - Delete Records"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("delete_id", "Enter ID to Delete:", ""),
      actionButton("delete_btn", "Delete Record"),
      hr(),
      actionButton("refresh_btn", "Refresh Data")
    ),
    
    mainPanel(
      h4("Current Records:"),
      tableOutput("records_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Function to fetch data from the database
  fetch_data <- reactive({
    dbGetQuery(db, "SELECT * FROM Applications")
  })
  
  # Display current records
  output$records_table <- renderTable({
    fetch_data()
  })
  
  # Delete record when button is clicked
  observeEvent(input$delete_btn, {
    req(input$delete_id)  # Ensure input is provided
    
    # Execute DELETE query
    query <- "DELETE FROM Applications WHERE ID = ?"
    dbExecute(db, query, params = list(input$delete_id))
    
    showNotification("Record Deleted Successfully!", type = "message")
  })
  
  # Refresh data when refresh button is clicked
  observeEvent(input$refresh_btn, {
    output$records_table <- renderTable({
      fetch_data()
    })
  })
}

# Run the app
shinyApp(ui, server)
