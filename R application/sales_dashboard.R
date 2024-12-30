# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Sample data
set.seed(123)
sales_data <- data.frame(
  Region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  Product = sample(c("Product A", "Product B", "Product C"), 100, replace = TRUE),
  Date = sample(seq.Date(Sys.Date() - 365, Sys.Date(), by = "day"), 100, replace = TRUE),
  Revenue = runif(100, 1000, 5000),
  Cost = runif(100, 500, 4000),
  Units_Sold = sample(1:50, 100, replace = TRUE)
)

# Calculate additional metrics
sales_data <- sales_data %>%
  mutate(
    Profit = Revenue - Cost,
    Profit_Margin = Profit / Revenue * 100,
    Conversion_Rate = Units_Sold / 50 * 100
  )

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(4, valueBoxOutput("total_revenue")),
      column(4, valueBoxOutput("avg_profit_margin")),
      column(4, valueBoxOutput("conversion_rate"))
    ),
    fluidRow(
      box(title = "Sales Overview", width = 6, plotOutput("sales_plot")),
      box(title = "Profit Margin", width = 6, plotOutput("profit_plot"))
    )
  )
)

# Server
server <- function(input, output, session) {
  # KPIs
  output$total_revenue <- renderValueBox({
    valueBox(
      paste0("$", round(sum(sales_data$Revenue), 2)),
      "Total Revenue",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$avg_profit_margin <- renderValueBox({
    valueBox(
      paste0(round(mean(sales_data$Profit_Margin, na.rm = TRUE), 2), "%"),
      "Average Profit Margin",
      icon = icon("percentage"),
      color = "blue"
    )
  })
  
  output$conversion_rate <- renderValueBox({
    valueBox(
      paste0(round(mean(sales_data$Conversion_Rate, na.rm = TRUE), 2), "%"),
      "Conversion Rate",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  # Sales Plot
  output$sales_plot <- renderPlot({
    ggplot(sales_data, aes(x = Date, y = Revenue, color = Product)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Sales Over Time", x = "Date", y = "Revenue")
  })
  
  # Profit Margin Plot
  output$profit_plot <- renderPlot({
    ggplot(sales_data, aes(x = Product, y = Profit_Margin, fill = Product)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Profit Margin by Product", x = "Product", y = "Profit Margin (%)")
  })
}

# Run the app
shinyApp(ui, server)
