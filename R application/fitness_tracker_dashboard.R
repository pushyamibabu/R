library(shiny)
library(ggplot2)
library(dplyr)

# Simulate wearable device data
simulate_fitness_data <- function() {
  users <- paste("User", 1:5)
  workout_types <- c("Running", "Cycling", "Strength", "Yoga")
  
  data.frame(
    User = sample(users, 300, replace = TRUE),
    Date = seq.Date(Sys.Date() - 299, Sys.Date(), by = "day"),
    WorkoutType = sample(workout_types, 300, replace = TRUE),
    Duration = round(runif(300, min = 20, max = 120), 1), # in minutes
    CaloriesBurned = round(runif(300, min = 150, max = 800), 1)
  )
}

fitness_data <- simulate_fitness_data()

# UI
ui <- fluidPage(
  titlePanel("Fitness Tracker Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("user_filter", "Select User:", 
                  choices = c("All", unique(fitness_data$User)), 
                  selected = "All"),
      selectInput("workout_filter", "Select Workout Type:", 
                  choices = c("All", unique(fitness_data$WorkoutType)), 
                  selected = "All"),
      dateRangeInput("date_filter", "Select Date Range:", 
                     start = min(fitness_data$Date), 
                     end = max(fitness_data$Date)),
      actionButton("update", "Update Dashboard")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Trends",
          plotOutput("calories_plot"),
          plotOutput("duration_plot")
        ),
        tabPanel(
          "Summary",
          tableOutput("summary_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive data based on filters
  filtered_data <- reactive({
    req(input$update)
    data <- fitness_data
    if (input$user_filter != "All") {
      data <- data %>% filter(User == input$user_filter)
    }
    if (input$workout_filter != "All") {
      data <- data %>% filter(WorkoutType == input$workout_filter)
    }
    data <- data %>% filter(Date >= input$date_filter[1], Date <= input$date_filter[2])
    data
  })
  
  # Calories burned trends
  output$calories_plot <- renderPlot({
    data <- filtered_data()
    data_summary <- data %>%
      group_by(Date) %>%
      summarise(TotalCalories = sum(CaloriesBurned))
    
    ggplot(data_summary, aes(x = Date, y = TotalCalories)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "darkred", size = 2) +
      theme_minimal() +
      labs(
        title = "Calories Burned Over Time",
        x = "Date",
        y = "Total Calories Burned"
      )
  })
  
  # Workout duration trends
  output$duration_plot <- renderPlot({
    data <- filtered_data()
    data_summary <- data %>%
      group_by(Date) %>%
      summarise(TotalDuration = sum(Duration))
    
    ggplot(data_summary, aes(x = Date, y = TotalDuration)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      theme_minimal() +
      labs(
        title = "Workout Duration Over Time",
        x = "Date",
        y = "Total Duration (Minutes)"
      )
  })
  
  # Summary table
  output$summary_table <- renderTable({
    data <- filtered_data()
    summary_data <- data %>%
      group_by(WorkoutType) %>%
      summarise(
        TotalWorkouts = n(),
        AvgDuration = mean(Duration),
        AvgCalories = mean(CaloriesBurned)
      )
    summary_data
  })
}

# Run the app
shinyApp(ui, server)
