library(shiny)

# Define UI for application that draws a histogram
ui <-  shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  div(id = "myapp",
      h2("shinyjs demo"),
      checkboxInput("show", "Switch Plot", FALSE)),
  tags$head(
    tags$style(HTML("
                   #distPlot2{
                    opacity:1;
                    }
                    "))
  ),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot2")
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  observe({
    if (input$show) {
      shinyjs::hide(id = "distPlot")
      shinyjs::show(id = "distPlot2")
      
    } else {
      shinyjs::show(id = "distPlot")
      shinyjs::hide(id = "distPlot2")
    }
  })

  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot2 <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(max(x),min(x),length.out = input$bins + 100)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'red', border = 'white')
  })
})

shinyApp(ui = ui, server = server)
