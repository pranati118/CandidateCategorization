# Load necessary libraries
if (!require("shiny")) install.packages("shiny")
if (!require("pdftools")) install.packages("pdftools")
if (!require("stringr")) install.packages("stringr")
if (!require("DBI")) install.packages("DBI")
if (!require("RSQLite")) install.packages("RSQLite")

library(shiny)
library(pdftools)
library(stringr)
library(DBI)
library(RSQLite)

# Simplified Function to parse resume data
parse_resume <- function(file_path) {
  # Try to read the PDF text
  text <- tryCatch({
    pdf_text(file_path)
  }, error = function(e) {
    return(NULL)  # If error, return NULL
  })
  
  if (is.null(text)) {
    return(list(Error = "Failed to read the PDF file. Please check the file format and try again."))
  }
  
  text_combined <- paste(text, collapse = "\n")
  
  # Extract Name (assumed to be all uppercase and at the beginning)
  name_pattern <- "(?m)^\\s*([A-Z\\s]+)\\s*$"
  name <- str_extract(text_combined, name_pattern)
  name <- str_trim(name)
  
  # Define common programming languages, technologies, and soft skills
  skills_list <- c(
    # Hard skills (Programming languages and technologies)
    "C Programming", "C++", "C#", "Python", "Java", "JavaScript", "Ruby", "PHP", "SQL", 
    "HTML", "CSS", "R Programming", "MATLAB", "Swift", "Go", "Kotlin", "Rust", "Ruby on Rails", 
    "Django", "React", "Node.js", "Angular", "Vue.js", "Machine Learning", "AIML", 
    "Artificial Intelligence", "Deep Learning", "Data Science", "Web Development", 
    "Cloud Computing", "AWS", "Azure", "TensorFlow", "PyTorch", "Docker", "Git", "Linux",
    
    # Soft skills (Non-technical skills)
    "Project Management", "Critical Thinking", "Teamwork", "Time Management", 
    "Communication", "Leadership", "Problem Solving", "Adaptability", "Collaboration", 
    "Creativity", "Conflict Resolution", "Decision Making", "Interpersonal Skills", 
    "Presentation", "Organizational Skills", "Multitasking", "Negotiation"
  )
  
  # Extract Skills (Match any skill from the list)
  skills <- unique(unlist(lapply(skills_list, function(skill) {
    str_extract_all(text_combined, fixed(skill))
  })))
  
  skills <- trimws(skills)  # Trim whitespace
  
  if (length(skills) == 0) {
    skills <- "Not Detected"
  } else {
    skills <- paste(skills, collapse = ", ")
  }
  
  # Extract Location (Modify based on common location names)
  location <- str_extract(text_combined, "(Bengaluru|Bangalore|[A-Za-z]+\\s-\\d{6})")
  
  # Extract Email
  email <- str_extract(text_combined, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")
  
  # Extract Phone Number
  phone <- str_extract(text_combined, "(\\+91\\s?\\d{10}|\\d{10})")
  
  # Extract Years of Experience
  experience_pattern <- "(\\d+\\s+(years?|months?))"
  experience <- str_extract(text_combined, experience_pattern)
  if (is.na(experience)) experience <- "0"
  
  # Return all extracted details as a list
  return(list(
    Name = ifelse(is.na(name), "Not Detected", name),
    Skills = ifelse(skills == "Not Detected", "Not Detected", skills),
    Location = ifelse(is.na(location), "Not Detected", location),
    Email = ifelse(is.na(email), "Not Detected", email),
    Phone = ifelse(is.na(phone), "Not Detected", phone),
    Experience = ifelse(is.na(experience), "0", experience)
  ))
}

# Define Shiny App
ui <- fluidPage(
  titlePanel("Job Seekers Portal"),
  sidebarLayout(
    sidebarPanel(
      fileInput("resume", "Upload Resume (PDF)", accept = c(".pdf")),
      actionButton("process", "Extract Information"),
      textInput("app_name", "Name"),
      textInput("app_skills", "Skills"),
      textInput("app_location", "Location"),
      textInput("app_experience", "Experience"),
      actionButton("submit_app", "Submit Application")
    ),
    mainPanel(
      h3("Extracted Information"),
      verbatimTextOutput("rawText"),  # Show raw text for debugging
      tableOutput("infoTable"),
      h4("Status Message"),
      verbatimTextOutput("status_message"),
      h4("Your Submitted Applications"),
      tableOutput("submitted_apps_table")
    )
  )
)

server <- function(input, output, session) {
  resume_data <- reactiveVal(NULL)
  
  observeEvent(input$process, {
    req(input$resume)
    parsed_data <- parse_resume(input$resume$datapath)
    resume_data(parsed_data)
    
    if (!"Error" %in% names(parsed_data)) {
      updateTextInput(session, "app_name", value = parsed_data$Name)
      updateTextInput(session, "app_skills", value = parsed_data$Skills)
      updateTextInput(session, "app_location", value = parsed_data$Location)
      updateTextInput(session, "app_experience", value = parsed_data$Experience)
    }
  })
  
  output$rawText <- renderPrint({
    req(input$resume)
    text <- pdf_text(input$resume$datapath)
    paste(text, collapse = "\n")
  })
  
  output$infoTable <- renderTable({
    req(resume_data())
    # Display an error message if failed to parse
    if ("Error" %in% names(resume_data())) {
      return(data.frame(Error = resume_data()$Error))
    }
    
    # Return extracted data in a table
    as.data.frame(t(unlist(resume_data())), stringsAsFactors = FALSE)
  })
  
  observeEvent(input$submit_app, {
    db <- dbConnect(SQLite(), "applications.db")
    
    # Ensure the Applications table exists
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS Applications (
        ID INTEGER PRIMARY KEY AUTOINCREMENT,
        Name TEXT,
        Skills TEXT,
        Location TEXT,
        Experience TEXT,
        SkillCount INTEGER
      )
    ")
    
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
