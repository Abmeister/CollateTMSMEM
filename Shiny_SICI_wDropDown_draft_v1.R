# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Sample data (Replace with actual data)
df <- data.frame(
  ID = factor(rep(201:205, each = 10)),  
  L_or_R_cx = factor(rep(c("L", "R"), each = 5, times = 5)),
  Onset_Cx = rep(NA, 50),
  VisitDay = factor(rep(c("BSL D1 LCX AM", "BSL D1 LCX PM", "BSL D2 LCX AM", "BSL D2 LCX PM", "TX D1 LCX PM"), times = 10)),
  Time = rep(c("AM", "PM"), 25),
  ISI_ms = factor(rep(c(2.5, 3), 25)),  # Different test levels
  Value_MSO = rnorm(50, mean = 100, sd = 10)  # Random values
)

# Define UI
ui <- fluidPage(
  titlePanel("Value MSO Distribution by ISI_ms"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("visit_day", "Select VisitDay:", 
                  choices = c("All", levels(df$VisitDay)), 
                  selected = "All")
    ),
    
    mainPanel(
      plotOutput("histogramPlot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$visit_day == "All") {
      df
    } else {
      df %>% filter(VisitDay == input$visit_day)
    }
  })
  
  output$histogramPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Value_MSO, fill = ID)) +
      geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +  
      facet_grid(~ ISI_ms) +  # Separate plots for each test level
      scale_fill_brewer(palette = "Dark2") +  # Nice color scheme for IDs
      theme_minimal(base_size = 14) +
      labs(title = "Histogram of Value_MSO",
           x = "Value MSO",
           y = "Count",
           fill = "ID")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
