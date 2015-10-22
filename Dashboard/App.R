# call libraries required
library(shiny)
library(RODBC)
library(RSQLServer)
library(ggplot2)

# connect to the server need to be going to global.R at a later stage.
conn <-odbcConnect("Capability")
PrgMap <- list(Programs = c("Pick Up", "Chasis Cab"), Database = c("Seahawk", "DragonCC"))
def_trk <- sqlQuery(conn, paste("select [TruckName] from",PrgMap$Database[[1]],".dbo. tblTrucks"))
DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[1]],".dbo. tblProcessingInfo"))
SoftwareBuild <- sqlQuery(conn,paste("Select distinct calibration from",PrgMap$Database[[1]], " .dbo.tblDataInBuild"))
trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[1]],".dbo. tblTrucks"))


ui <- fluidPage(
        sidebarPanel(
                # Select Product from drop down
                
                selectInput(inputId = "Program",label = "Choose the Program", choices = PrgMap$Programs,selected = PrgMap$Programs[[1]]),
                
                # Make available choice of trucks
                selectInput(inputId = "TrucksGrp", label = "Choose Trucks group",choices = as.character(trucks$Family),multiple = T,selected = as.character(trucks$Family[1])),
                
                # Make available choice of Diagnostics
                
                selectInput(inputId = "Diag", label = "Choose Diagnostic of interest here",choices = as.character(DiagList$Name)),
                
                # make available choice of trucks 
                selectInput(inputId = "Trucks",label = "Choose trucks here", choices = as.character(trucks$TruckName),multiple = T),
                
                # action Button
                actionButton(inputId = "Update", label = "Update"),
                
                
                width = 5
                
                
                
                
                
        ),
        mainPanel( plotOutput("plot"))
)

server <- function(input,output,session){
        # USE eventReactive() at a later stage.
        reactive({odbcCloseAll()
                conn <-odbcConnect("Capability")
                
        })
        observe({
                trk <- sqlQuery(conn, paste("select [TruckName] from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                 #browser()
                updateSelectInput(session,"TrucksGrp",label = "Choose Truck group of interest here",choices = 
                                          as.character(trucks$Family),selected = as.character(trucks$Family[1]))
                DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblProcessingInfo"))
                
                updateSelectInput(session,"Diag",label = "Choose Diagnostics of interest here",choices = 
                                          as.character(DiagList$Name))
                updateSelectInput(session,"Trucks", label = "choose trucks here", choices = as.character(trucks$TruckName) )
                
                
        })
        
        observeEvent(input$Update,{
                SEID <- DiagList$SEID[which(DiagList$Name==input$Diag)]
                #browser()
                trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblProcessingInfo"))
                TruckID <- trucks$TruckID[which(trucks$Family %in% input$TrucksGrp)]
                Parameter <- DiagList$CriticalParam[which(DiagList$Name==input$Diag)]
                #browser()
                if (is.null(input$Program) | is.null(input$Diag)){
                        stop("Both program and Diagnostic must be chosen")
                }
                if (!is.null(input$TrucksGrp)& !is.null(input$Trucks)){
                        TruckID <- intersect(trucks$TruckID[which(trucks$Family %in% input$TrucksGrp)],trucks$TruckID[which(trucks$TruckName %in% input$Trucks)])
                        WhereClause <- paste("where",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblEventDrivenData",".TruckID in (",paste(as.character(TruckID),collapse = ","))
                }
                else if(!is.null(input$TrucksGrp)){
                        TruckID <- trucks$TruckID[which(trucks$Family %in% input$TrucksGrp)]
                        WhereClause <- paste("where",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblEventDrivenData",".TruckID in (",paste(as.character(TruckID),collapse = ","))
                }
                else{
                        TruckID <- trucks$TruckID[which(trucks$TruckName %in% input$Trucks)]
                        WhereClause <- paste("where",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblEventDrivenData",".TruckID in (",paste(as.character(TruckID),collapse = ","))
                }
                Data <- 
                        
                        sqlQuery(conn,paste("select DataValue,TruckName FROM ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblEventDrivenData JOIN",
                                            PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks on",PrgMap$Database[[which(PrgMap$Programs==input$Program)]], 
                                            ".dbo.tblEventDrivenData.TruckID = ", PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblTrucks.TruckID",
                                            " Where ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblEventDrivenData",".TruckID in (",paste(as.character(TruckID),collapse = ","),") and SEID = ",SEID))
                
                 browser()
                 
                if (nrow(Data) > 0 ){
                        
                        output$plot <- renderPlot({
                                
                               # browser()
                                TgtDat <- Data
                                qplot(TgtDat$DataValue,TgtDat$TruckName, main = "Data Vs Trucks",position = position_jitter(0.1,0.1),color = TgtDat$TruckName,xlab = Parameter) + theme_bw()
                        }) 
                        
                }
        })
        
        
        
}

shinyApp(ui = ui, server = server)