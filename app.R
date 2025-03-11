library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(RColorBrewer)

# df1 <- readRDS("df1_unfiltered.rds")
df1 <- readRDS("SNBRdf1.rds")

# Summarized data preparation function
summarized_data <- function(df1, selected_groups, selected_isi, selected_visit, selected_gender, selected_muscle) {
  df1 %>%
    filter(Group %in% selected_groups) %>%
    filter(ISI_ms %in% selected_isi) %>%
    filter(VisitType %in% selected_visit) %>%
    filter(Sex %in% selected_gender) %>%
    filter(Muscle %in% selected_muscle) %>%
    mutate(ID = as.factor(ID),
           Group = fct_relevel(Group, "Control ", "Patient ", "TOF", "GC")) %>%
    select(ID, Group, Value_MSO, ISI_ms, VisitType) %>%
    group_by(ISI_ms, ID, Group, VisitType) %>%
    summarise(Ave_RMT_Change = mean(Value_MSO, na.rm = TRUE),
              Ave_SD = sd(Value_MSO, na.rm = TRUE),
              .groups = "drop")
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "SNBR Longitudinal Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot", tabName = "plot", icon = icon("chart-line")),
      checkboxGroupInput("Group", "Select Group:", 
                         choices = c("Control ", "Patient ", "TOF", "GC"), 
                         selected = c("Control ", "Patient ", "TOF", "GC")),
      selectInput("ISI", "Select ISI:", 
                  choices = c("All", 1.5, 2, 2.5, 3), 
                  selected = "All", multiple = TRUE),
      selectInput("VisitType", "Select Visit Type:", 
                  choices = c("Both", "Baseline", "Followup"), 
                  selected = "Both"),
      selectInput("Gender", "Select Gender:", 
                  choices = c("Both", "1=Male", "2=Female"), 
                  selected = "Both"),
      selectInput("Muscle", "Select Muscle:", 
                  choices = c("FDI", "ADM"), 
                  selected = "FDI"),
      checkboxInput("showScatter", "Show Scatter Plot", value = TRUE),
      checkboxInput("showBoxplot", "Show Boxplot", value = TRUE)
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
      .content {
        background-color: #ffffff;
      }
    ")),
    fluidRow(
      # First plot (top row) takes the entire width
      box(title = "SNBR Longitudinal Dashboard", width = 12, solidHeader = TRUE, status = "primary",
          plotOutput("mainPlot", hover = "plot_hover"))
    ),
    fluidRow(
      # Bottom row: Histogram and Pie Chart
      box(title = "Age Distribution", width = 6, solidHeader = TRUE, status = "primary",
          plotOutput("ageHistogram")),
      box(title = "Recruitement Status", width = 6, solidHeader = TRUE, status = "primary",
          plotlyOutput("uniqueIdsPie"))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive plot rendering
  output$mainPlot <- renderPlot({
    # Filter data based on inputs
    groups <- input$Group
    isi <- if ("All" %in% input$ISI) c(1.5, 2, 2.5, 3) else as.numeric(input$ISI)
    visit <- if (input$VisitType == "Both") c("B", "F1") else input$VisitType
    gender <- if (input$Gender == "Both") c(1, 2) else as.numeric(strsplit(input$Gender, "=")[[1]][1])
    muscle <- input$Muscle
    
    # Summarize the data based on user selections
    plot_data <- summarized_data(df1, groups, isi, visit, gender, muscle)
    
    # Ensure that there are data points for each group selected
    if (nrow(plot_data) == 0) {
      return(NULL)  # No data to plot
    }
    
    plot <- ggplot() + 
      theme_minimal(base_size = 14) +
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray", size = 0.2),
            axis.text = element_text(color = "black"),
            axis.title = element_text(color = "black"),
            plot.title = element_text(color = "black")) +
      
      # Boxplot with raw data
      {if(input$showBoxplot) {
        geom_boxplot(data = df1 %>% 
                       filter(Group %in% groups, ISI_ms %in% isi, VisitType %in% visit, Muscle %in% muscle), 
                     aes(x = VisitType, y = Value_MSO, fill = as.factor(ISI_ms)), 
                     alpha = 0.3, position = position_dodge(width = 0.3))
      }} +
      
      # Scatter plot with error bars using summarized data
      {if(input$showScatter) {
        geom_pointrange(data = plot_data, 
                        aes(x = VisitType, y = Ave_RMT_Change, 
                            ymin = Ave_RMT_Change - Ave_SD, 
                            ymax = Ave_RMT_Change + Ave_SD, 
                            group = ISI_ms, color = as.factor(ISI_ms)),
                        position = position_dodge(width = 0.3))
      }} +
      
      # Line to connect points per ID
      geom_line(data = plot_data, 
                aes(x = VisitType, y = Ave_RMT_Change, group = ID, color = as.factor(ISI_ms)),
                position = position_dodge(width = 0.3)) +
      
      # Horizontal reference lines
      geom_hline(yintercept = 108, linetype = "dashed", color = "black") +  
      geom_hline(yintercept = 112, linetype = "dashed", color = "blue") +  
      geom_hline(yintercept = 95, linetype = "dashed", color = "red") +  
      geom_hline(yintercept = 98, linetype = "dashed", color = "red") +  
      
      ggtitle("SNBR SICI Outcomes by Group") +
      xlab("Visit Type") + ylab("SICI (%MSO)") +
      scale_y_continuous(breaks = seq(80, 140, by = 10)) +
      
      # Facet wrap for Group (1 row)
      facet_wrap(~Group, nrow = 1) 
    
    plot
  })
  
  # Age histogram plot
  output$ageHistogram <- renderPlot({
    ggplot(df1, aes(x = Age, fill = as.factor(Group))) +
      geom_histogram(bins = 15, alpha = 0.6, position = "identity") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            axis.text = element_text(color = "black"),
            axis.title = element_text(color = "black")) +
      labs(title = "Age Distribution by Group", x = "Age", y = "Frequency")
  })
  
  # Pie chart for unique IDs by group
  output$uniqueIdsPie <- renderPlotly({
    group_counts <- df1 %>%
      group_by(Group) %>%
      summarise(Unique_IDs = n_distinct(ID))
    
    plot_ly(group_counts, labels = ~Group, values = ~Unique_IDs, type = "pie", 
            textinfo = "label+percent", marker = list(colors = RColorBrewer::brewer.pal(4, "Set3"))) %>%
      layout(title = "Unique IDs by Group", showlegend = TRUE)
  })
}

# Run the app
shinyApp(ui, server)
