library(shiny)


ui <- fluidPage("hello world"
         sliderInput()
      
                
                )


server <- function(input,output){}

shinyApp(ui = ui,server = server)
