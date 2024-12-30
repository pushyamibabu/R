library(shiny)
library(ggplot2)
library(dplyr)

# Simulate data for the dashboard
simulate_trial_data <- function() {
  sites <- paste("Site", 1:10)
  data.frame(
    Site = rep(sites, each = 30),
    Day = rep(1:30, times = length(sites)),
    Enrolled = sample(0:5, 300, replace = TRUE),
    AdverseEvents = sample(0:3, 300, replace = TRUE),
    Completed = sample(0:2, 300, replace = TRUE)
  )
}

trial_data <- simulate_trial_data()

# Aggregate data for key metrics
key_metrics <- trial_data %>%
  group_by(Site) %>%
  summarise(
    TotalEnrolled = sum(Enrolled),
    TotalAdverseEvents = sum(AdverseEvents),
    TotalCompleted = sum(Completed)
  )

# UI
ui <- fluidPage(
  titlePanel("Drug Trial Progress Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("site_filter", "Select Site:", 
                  choices = c("All", unique(trial_data$Site)), 
                  selected = "All"),
      sliderInput("day_filter", "Select Day Range:", 
                  min = 1, max = 30, value = c(1, 30)),
      actionButton("update", "Update Dashboard")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          plotOutput("enrollment_plot"),
          plotOutput("adverse_events_plot")
        ),
        tabPanel(
          "Key Metrics",
          tableOutput("metrics_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Filtered data reactive expression
  filtered_data <- reactive({
    req(input$update)
    data <- trial_data %>%
      filter(Day >= input$day_filter[1], Day <= input$day_filter[2])
    if (input$site_filter != "All") {
      data <- data %>% filter(Site == input$site_filter)
    }
    data
  })
  
  # Enrollment progress plot
  output$enrollment_plot <- renderPlot({
    data <- filtered_data()
    data_summary <- data %>%
      group_by(Day) %>%
      summarise(TotalEnrolled = sum(Enrolled))
    
    ggplot(data_summary, aes(x = Day, y = TotalEnrolled)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      theme_minimal() +
      labs(
        title = "Enrollment Progress",
        x = "Day",
        y = "Total Enrolled"
      )
  })
  
  # Adverse events plot
  output$adverse_events_plot <- renderPlot({
    data <- filtered_data()
    data_summary <- data %>%
      group_by(Site) %>%
      summarise(TotalAdverseEvents = sum(AdverseEvents))
    
    ggplot(data_summary, aes(x = Site, y = TotalAdverseEvents, fill = Site)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      theme_minimal() +
      labs(
        title = "Adverse Events by Site",
        x = "Site",
        y = "Total Adverse Events"
      ) +
      coord_flip()
  })
  
  # Key metrics table
  output$metrics_table <- renderTable({
    data <- filtered_data()
    key_metrics <- data %>%
      group_by(Site) %>%
      summarise(
        TotalEnrolled = sum(Enrolled),
        TotalAdverseEvents = sum(AdverseEvents),
        TotalCompleted = sum(Completed)
      )
    key_metrics
  })
}

# Run the app
shinyApp(ui, server)
