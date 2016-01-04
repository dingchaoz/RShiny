# call libraries required
library(shiny)
library(RODBC)
library(RSQLServer)
library(ggplot2)

# connect to the server
conn <-odbcConnect("Capability")
PrgMap <- list(Programs = c("Pick Up", "Chasis Cab"), Database = c("Seahawk", "DragonCC"))
def_trk <- sqlQuery(conn, paste("select [TruckName] from",PrgMap$Database[[1]],".dbo. tblTrucks"))
DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[1]],".dbo. tblProcessingInfo"))

ui <- fluidPage(
        # Select Product from drop down
        
        selectInput(inputId = "Program",label = "Choose the Program", choices = PrgMap$Programs,selected = PrgMap$Programs[[1]]),
        
        # Make available choice of trucks
        selectInput(inputId = "Trucks", label = "Choose Trucks of interest here",choices = as.character(def_trk$TruckName),multiple = T,selected = as.character(def_trk$TruckName[1])),
        
        # Make available choice of Diagnostics
        
        selectInput(inputId = "Diag", label = "Choose Diagnostic of interest here",choices = as.character(DiagList$Name)),
        
      
                plotOutput("plot")
      
        
)

server <- function(input,output,session){
        # USE eventReactive() at a later stage.
         reactive({odbcCloseAll()
                conn <-odbcConnect("Capability")
                
                })
        observe({
                trk <- sqlQuery(conn, paste("select [TruckName] from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
               # browser()
                updateSelectInput(session,"Trucks",label = "Choose Trucks of interest here",choices = 
                                          as.character(trk$TruckName),selected = as.character(trk$TruckName[1]))
                DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblProcessingInfo"))
                
                updateSelectInput(session,"Diag",label = "Choose Diagnostics of interest here",choices = 
                                          as.character(DiagList$Name))
                
                
        })
       
        output$plot <- renderPlot({
                SEID <- DiagList$SEID[which(DiagList$Name==input$Diag)]
                Data <- reactive({sqlQuery(conn,paste("select * FROM ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblEventDrivenData Where ",
                                                      "TruckID in (",as.character(trucks$TruckID[which(trucks$TruckName %in% input$Trucks)]),") and SEID = ",SEID))})
                #rowser()
                if (nrow(Data()) > 0){
                        
                #browser()
                TgtDat <- Data()
                qplot(TgtDat$TruckID,TgtDat$DataValue, main = "Data Vs Trucks")
                }
                
        })
        
}

shinyApp(ui = ui, server = server)