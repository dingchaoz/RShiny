library(shiny)


ui <- fluidPage("hello world",
         ## Example Numeric Input for the page Hello World
         numericInput(inputId = 'num',label = 'Enter Number here',value = 10, min = 0, max = 50, step = 5),
        textOutput("test")
        
       ## mainPanel(
               ## textOutput("text"))
                
)


server <- function(input,output){
       output$test <- renderText({paste("The number you entered is: ",input$num)})
       ## Expression needs to get within curly brackets and the below line wouldn't work!!
        ##output$text <- renderText({input$num)})
}

shinyApp(ui = ui,server = server)
