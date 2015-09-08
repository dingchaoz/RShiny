library(shiny)

ui <- fluidPage(
        
        # *Input() functions,
       
        
        sliderInput(inputId = "num", 
                    label = "Choose a number", 
                    value = 25, min = 1, max = 100),
        
        # *Output() functions
        
        plotOutput("hist")
)

server <- function(input, output) {
        output$hist <- renderPlot({
                hist(rnorm(input$num))
        })
}

shinyApp(ui = ui, server = server)