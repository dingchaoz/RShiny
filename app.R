library(shiny)
library(ggplot2)

ui <- fluidPage(
        
        # *Input() functions,
       
        
        selectInput(inputId = 'Program',label = "Program :",choices = unique(mtcars$carb),multiple = T ),
        checkboxGroupInput(inputId = "TruckGroup", "Group:",unique(mtcars$gear)),
        dateRangeInput(inputId = "DateRange",label = "DateRange",start = "2015-01-01",end = NULL,separator = "to"),
        selectInput(inputId = "Cylinder",label="CylNum",choices = unique(mtcars$cyl),multiple = T),
        # *Output() functions
        
        textOutput("ProgramSelect"),
        uiOutput("TruckGroupSelect"),
        plotOutput("scatter")
)

server <- function(input, output) {
       ## output$ProgramSelect <- renderText({
              ##  paste("The Program name is :",
       ## input$Program)})
        ##output$TruckGroupSelect <- renderUI(input$TruckGroup)
  .e <- environment()
  #dat <- reactive(input$cylinder)
  #g <- reactive(ggplot(data = mtcars,aes_string(x=input$TruckGroup,y='mpg')))
  
  output$scatter <- 
    renderPlot(qplot(x=input$Cylinder,Y=mpg,data = mtcars))
    #renderPlot({g+geom_point("alpha=1/2")})
}

shinyApp(ui = ui, server = server)