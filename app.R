library(shiny)
source("helpers.R")

ui <- fluidPage(
   titlePanel("Power Analysis for 2x2 Factorial Interaction"),
   em("See https://www.markhw.com/blog/power-twoway 
      for instructions and information."),
   br(),
   br(),
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
          "# of Simulations For Each Sample Size",
          100
        ),
        numericInput(
          "alpha",
          "Alpha Level",
          .05,
          min = .001, max = .20, step = .001
        ),
        h4("Run Power Analysis:"),
        actionButton("submit", "Submit"),
        br(),
        em("Depending on your inputs, please allow a few minutes to compute.")
      ),
      mainPanel(
        h4("Mean Pattern Plot:"),
        plotOutput("mean_pattern_plot"),
        uiOutput("power_analysis_results")
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
    m <- showNotification("Calculating Power...", duration = NULL)
    tmp <- power_analysis(
      input$f1l1_f2l1, input$f1l1_f2l2, input$f1l2_f2l1, input$f1l2_f2l2,
      input$reps, input$start, input$end, input$by, input$alpha
    )
    removeNotification(m)
    showNotification("Done!", duration = 1)
    tmp
  })
  output$power_analysis_results <- renderUI({
    if (is.data.frame(results())) {
      tagList(
        h4("Power Analysis Results:"),
        renderTable(results(), digits = c(0, 2))
      )
    }
  })
}

shinyApp(ui = ui, server = server)
