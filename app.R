library(shiny)
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("./processed.cleveland.data", header = FALSE, na.strings = "?",
                 col.names = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 
                               'thalach', 'exang', 'oldpeak', 'slope', 'ca', 'thal', 'target'))

# Drop rows with any missing values
data <- na.omit(data)

# Define UI
ui <- fluidPage(
  titlePanel("Heart Disease Dataset Exploration"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age", min = min(data$age, na.rm = TRUE), max = max(data$age, na.rm = TRUE), value = c(min(data$age, na.rm = TRUE), max(data$age, na.rm = TRUE))),
      selectInput("sex", "Sex", choices = c("Both" = "", "Male" = 1, "Female" = 0)),
      actionButton("toggleStats", "Show/Hide Summary Statistics"),
      actionButton("toggleLM", "Show/Hide Linear Model")
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      conditionalPanel(
        condition = "input.toggleStats % 2 == 1",
        tableOutput("summaryStats")
      ),
      p("The color of the heart disease dots represents the presence of heart disease:"),
      tags$ul(
        tags$li("Red: Heart disease present (1)"),
        tags$li("Blue: No heart disease (0)")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    data %>%
      filter((age >= input$age[1]) & (age <= input$age[2]),
             (sex == input$sex | input$sex == ""))
  })
  
  output$scatterPlot <- renderPlot({
    plot <- ggplot(filtered_data(), aes(x = age, y = chol, color = factor(target))) +
      geom_point() +
      labs(x = "Age", y = "Cholesterol", color = "Heart Disease") +
      scale_color_manual(values = c("0" = "blue", "1" = "red"))
    
    if (input$toggleLM %% 2 == 1) {
      plot <- plot + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black")
    }
    
    plot
  })
  
  output$summaryStats <- renderTable({
    summary(filtered_data())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
