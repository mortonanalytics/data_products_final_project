library(shiny)
library(caret)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons("model", "Choose your model:",
                      choices = c("Linear" = "lm",
                                  "Generalized Linear Model" = "glm",
                                  "Random Forest" = "rf",
                                  "Boosted" = "gbm",
                                  "Regression Tree" = "rpart",
                                  "Bayesian GLM" = "bayesglm"
                                  ))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h2("Training the Best Model"),
        p("This application allows the user to compare models of prediction. Simply select the model type and the application
          will fit the model to the data and report results.  The plot fits the model to a line to help the user understand
          how the model is working."),
        h3("Scatterplot with Fitted Line"),
        plotlyOutput("distPlot"),
        h3("Goodness of Fit"),
        tableOutput("stats")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  fit <- reactive({
    train(eruptions ~ waiting, data = faithful, method = input$model)
  })
  
  output$distPlot <- renderPlotly({
      plot_ly(faithful, x = ~waiting, y = ~eruptions, type = "scatter", mode = "markers") %>%
       add_lines(x = ~waiting, y = fitted(fit())) %>%
       layout(showlegend = FALSE)
   })
  
  output$stats <- renderTable({
    fit()$results
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

