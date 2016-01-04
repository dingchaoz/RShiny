library(shiny)

ui <- fluidPage (
  
  sliderInput(inputId = "NUM", label = "Choose a number",value = 25, min = 1, max = 100),
  textOutput("number")
  
)

server <- function(input,output){
  
  output$number <- renderText({paste("the number is ",input$NUM)})
  
}

shinyApp(server = server,ui=ui)