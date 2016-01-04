# A simple histogram app

library(shiny)

ui <- fluidPage(
  #Define the input of the ui, here we use sliderInput, and there are many more like checkboxButton(),actionBUtton(),etc
  sliderInput(inputId = "num", 
    label = "Choose a number", 
    value = 25, min = 1, max = 100),
  #Define the output of the ui,here we use plotOutput() which inserts a plot, and there are many more like textOutput(),tableOutput()
  #imageOutput(),etc
  #hist is the id name of the output which should be unique in most cases
  plotOutput("hist")
)

server <- function(input, output) {
  #output is just the argument of output in server function,here we SAVE the output that we want to build to output$hist
  #hist is the same as teh hist defined in the ui and is accessible after a $ sign
  #use render() function BUILD the type of output you wish to make, this function will generate html on webpage
  #here user renderPlot to reder a plot type of object
  output$hist <- renderPlot({
    # within the renderPlot (), put hte code block that actually builds the object
    # wwe can put as many as code blocks to build multiple obejcts
    # num is the id of input defined in ui, and it is accessible after $ sign and using input$num ACCESS the current value of id num
    hist(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)