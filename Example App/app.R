library(shiny)

ui <- fluidPage(
        
        # *Input() functions,
       
        
        sliderInput(inputId = "num", 
                    label = "Choose a number", 
                    value = 25, min = 1, max = 100),
        
        # *Output() functions
        
        #plotOutput("hist")
       #fluidRow(Column(1,tableOutput('table')))
       mainPanel(plotOutput("hist"),tableOutput('table'))
)

server <- function(input, output) {
        
        output$hist <- renderPlot({
                hist(rnorm(input$num))
        })
        output$table <- renderTable(data.frame(data = rnorm(input$num), ID = 1:input$num))
}

shinyApp(ui = ui, server = server)