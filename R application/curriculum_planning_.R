# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Sample curriculum data
set.seed(123)
curriculum_data <- data.frame(
  Course_ID = paste0("C", 1001:1100),
  Course_Name = paste("Course", 1:100),
  Department = sample(c("Math", "Science", "Arts", "Engineering"), 100, replace = TRUE),
  Term = sample(c("Fall", "Spring", "Summer"), 100, replace = TRUE),
  Registrations = sample(50:500, 100, replace = TRUE)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Curriculum Planning Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Courses", tabName = "courses", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                column(4, valueBoxOutput("total_courses")),
                column(4, valueBoxOutput("total_registrations")),
                column(4, valueBoxOutput("popular_department"))
              ),
              fluidRow(
                box(title = "Registrations by Department", width = 6, plotOutput("registrations_by_dept")),
                box(title = "Registrations by Term", width = 6, plotOutput("registrations_by_term"))
              )
      ),
      # Courses tab
      tabItem(tabName = "courses",
              fluidRow(
                box(title = "Filters", width = 3,
                    selectInput("department_filter", "Select Department:",
                                choices = c("All", unique(curriculum_data$Department)), selected = "All"),
                    selectInput("term_filter", "Select Term:",
                                choices = c("All", unique(curriculum_data$Term)), selected = "All")
                ),
                box(title = "Course Registrations", width = 9,
                    DT::dataTableOutput("course_table"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # KPIs
  output$total_courses <- renderValueBox({
    total_courses <- nrow(curriculum_data)
    valueBox(total_courses, "Total Courses", icon = icon("book"), color = "blue")
  })
  
  output$total_registrations <- renderValueBox({
    total_registrations <- sum(curriculum_data$Registrations)
    valueBox(total_registrations, "Total Registrations", icon = icon("users"), color = "green")
  })
  
  output$popular_department <- renderValueBox({
    popular_dept <- curriculum_data %>%
      group_by(Department) %>%
      summarise(Total_Registrations = sum(Registrations)) %>%
      arrange(desc(Total_Registrations)) %>%
      slice(1) %>%
      pull(Department)
    valueBox(popular_dept, "Most Popular Department", icon = icon("university"), color = "yellow")
  })
  
  # Plots
  output$registrations_by_dept <- renderPlot({
    dept_data <- curriculum_data %>%
      group_by(Department) %>%
      summarise(Total_Registrations = sum(Registrations))
    ggplot(dept_data, aes(x = Department, y = Total_Registrations, fill = Department)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Registrations by Department", x = "Department", y = "Registrations")
  })
  
  output$registrations_by_term <- renderPlot({
    term_data <- curriculum_data %>%
      group_by(Term) %>%
      summarise(Total_Registrations = sum(Registrations))
    ggplot(term_data, aes(x = Term, y = Total_Registrations, fill = Term)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Registrations by Term", x = "Term", y = "Registrations")
  })
  
  # Course Table using DT
  output$course_table <- DT::renderDT({
    filtered_data <- curriculum_data
    if (input$department_filter != "All") {
      filtered_data <- filtered_data %>% filter(Department == input$department_filter)
    }
    if (input$term_filter != "All") {
      filtered_data <- filtered_data %>% filter(Term == input$term_filter)
    }
    DT::datatable(filtered_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the app
shinyApp(ui, server)

