library(shiny)
source("helpers.R")

ui <- fluidPage(
  
   titlePanel("Power Analysis for 2x2 Factorial Interaction"),
   
   sidebarLayout(
      sidebarPanel(
        h4("Input Standardized Means"),
        numericInput(
          "f1l1_f2l1",
          "Factor 1, Level 1 — Factor 2, Level 1",
          .2
        ),
        numericInput(
          "f1l1_f2l2",
          "Factor 1, Level 1 — Factor 2, Level 2",
          .3
        ),
        numericInput(
          "f1l2_f2l1",
          "Factor 1, Level 2 — Factor 2, Level 1",
          -.3
        ),
        numericInput(
          "f1l2_f2l2",
          "Factor 1, Level 2 — Factor 2, Level 2",
          -.5
        ),
        h4("Select Possible Sample Sizes"),
        numericInput(
          "start",
          "Minimum Sample Size",
          100
        ),
        numericInput(
          "end",
          "Maximum Sample Size",
          400
        ),
        numericInput(
          "by",
          "Step Size From Minimum to Maximum",
          50
        ),
        numericInput(
          "reps",
          "Repetitions For Each Sample Size",
          100
        ),
        h4("Run Power Analysis:"),
        actionButton("submit", "Submit"),
        br(),
        em("Depending on your inputs, please allow a few minutes to compute.")
      ),
      mainPanel(
        h4("Power Analysis Results:"),
        tableOutput("power_analysis_results"),
        h4("Mean Pattern Plot:"),
        plotOutput("mean_pattern_plot")
      )
   )
)

server <- function(input, output) {
  output$mean_pattern_plot <- renderPlot(
    plot_mean_pattern(
      input$f1l1_f2l1, input$f1l1_f2l2, input$f1l2_f2l1, input$f1l2_f2l2
    )
  )
  results <- eventReactive(input$submit, {
    power_analysis(
      f1l1_f2l1, f1l1_f2l2, f1l2_f2l1, f1l2_f2l2, reps, start, end, by
    )
  })
  output$power_analysis_results <- renderTable(results(), digits = c(0, 2))
}

# Run the application 
shinyApp(ui = ui, server = server)
