# call libraries required
library(shiny)
library(RODBC)
library(RSQLServer)
library(ggplot2)
source('~/Documents/Coursera_R/Dashboard/POSIXt2matlabUTC.R')

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
                
                # make software choice available
                textInput(inputId = "FrmCal",label = "Software Version from"),
                textInput(inputId = "ToCal",label = "to Software Version"),
                
                # make Date Range choice available
                dateRangeInput(inputId = "DateRange",label = "Choose Date Range",start = "2012-01-01",end = "2017-01-01"),
                
                # action Button
                actionButton(inputId = "Update", label = "Update"),
                
                
                width = 5
                
                
                
                
                
        ),
        mainPanel( plotOutput("Tplot"),
                   plotOutput("hist"),
                   plotOutput("Splot"),
                   plotOutput("box")
                   
                   )
)

server <- function(input,output,session){
        # USE eventReactive() at a later stage.
        reactive({odbcCloseAll()
                conn <-odbcConnect("Capability")
                
        })
        observe({
                trk <- sqlQuery(conn, paste("select [TruckName] from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                 #
                updateSelectInput(session,"TrucksGrp",label = "Choose Truck group of interest here",choices = 
                                          as.character(trucks$Family),selected = as.character(trucks$Family[1]))
                DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblProcessingInfo"))
                
                updateSelectInput(session,"Diag",label = "Choose Diagnostics of interest here",choices = 
                                          as.character(DiagList$Name))
                updateSelectInput(session,"Trucks", label = "choose trucks here", choices = as.character(trucks$TruckName) )
                
                
        })
        
        observeEvent(input$Update,{
                #---------------------------------------------------GETTING INITAL VLAUES-----------------------------------------------------------------
                SEID <- DiagList$SEID[which(DiagList$Name==input$Diag)]
                ExtID <- DiagList$ExtID[which(DiagList$Name==input$Diag)]
                Parameter <- DiagList$CriticalParam[which(DiagList$Name==input$Diag)]
                LSL <- DiagList$LSL[which(DiagList$Name==input$Diag)]
                USL <- LSL <- DiagList$USL[which(DiagList$Name==input$Diag)]
                startDate <- POSIXt2matlabUTC(as.POSIXlt(input$DateRange[1],"UTC"))
                endDate <- POSIXt2matlabUTC(as.POSIXlt(input$DateRange[2],"UTC"))
                
                # had to re-write the below line from the observe block; uncertain about the scoping rules.
                trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblProcessingInfo"))
                TruckID <- trucks$TruckID[which(trucks$Family %in% input$TrucksGrp)]
                #Parameter <- DiagList$CriticalParam[which(DiagList$Name==input$Diag)]
                #
                #------------------------------------------------------SETTING THE WHERE CLAUSE--------------------------------------------------------------------------------
                # initializing a empty WhereClause vector
                WhereClause = as.character()
                
                # Setting where clause for Parameter and Date - this the default where clause
                if (is.na(ExtID)){
                        tbl <- ".dbo.tblMinMaxData"
                        # Calibration in below query may not be needed; thought it was needed for a very complicated reason. Thought in a nut shell - what if one PublicDataID could mean
                        # different parameters in different builds? And what if we need the different parameters as capability parameter?
                        PID <- sqlQuery(conn,paste("Select Distinct PublicDataID from",PrgMap$Database[which(PrgMap$Programs == input$Program)], ".dbo.tblDataInBuild where Data = ",paste0("'",Parameter,"'")))
                        WhereClause <- paste("Where PublicDataId in ( ",paste(PID$PublicDataID,collapse=","), ")","AND datenum between", startDate ,"AND", endDate)
                       # 
                        if (is.na(LSL)& is.na(USL)){
                                stop(paste("Ask your friends in the data analysis team update the LSL & USL Parameters for", input$Diag, "& capability parameter", Parameter))
                        }
                        else if(is.na(LSL)){
                                Value <- "DataMax"
                        }
                        else {
                                Value <- "DataMin"
                        }
                }
                else {
                        tbl <- ".dbo.tblEventDrivenData"
                        WhereClause <- paste("Where SEID = ", SEID, " AND ExtID = ", ExtID,"AND datenum between", startDate ,"AND", endDate)
                        Value <- "DataValue"
                }
                
                # Setting where clause for Truck group and Truck IDs
                if (is.null(input$Program) | is.null(input$Diag)){
                        stop("Both program and Diagnostic must be chosen")
                }
                if (!is.null(input$TrucksGrp)& !is.null(input$Trucks)){
                        TruckID <- intersect(trucks$TruckID[which(trucks$Family %in% input$TrucksGrp)],trucks$TruckID[which(trucks$TruckName %in% input$Trucks)])
                        WhereClause <- paste(WhereClause,"AND",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],tbl,".TruckID in (",paste(as.character(TruckID),collapse = ","),")")
                       
                }
                else if(!is.null(input$TrucksGrp)){
                        TruckID <- trucks$TruckID[which(trucks$Family %in% input$TrucksGrp)]
                        WhereClause <- paste(WhereClause,"AND",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],tbl,".TruckID in (",paste(as.character(TruckID),collapse = ","),")")
                }
                else if(!is.null(input$Trucks)){
                        TruckID <- trucks$TruckID[which(trucks$TruckName %in% input$Trucks)]
                        WhereClause <- paste(WhereClause,"AND",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],tbl,".TruckID in (",paste(as.character(TruckID),collapse = ","),")")
                }
                
#                 # Setting the where clause for Software
#                 if(!is.null(input$FrmCal)){
#                         WhereClause <- paste(WhereClause,"AND","CalibrationVersion > = ",input$FrmCal)
#                 }
#                 if(!is.null(input$ToCal)){
#                         WhereClause <- paste(WhereClause,"AND","CalibrationVersion < =",input$ToCal)
#                 }
                
                #Setting the where clause for Software
                if((!identical(input$FrmCal,""))){
                        WhereClause <- paste(WhereClause,"AND CalibrationVersion >=", input$FrmCal)
                }
                if((!identical(input$ToCal,""))){
                        WhereClause <- paste(WhereClause,"AND CalibrationVersion <=", input$ToCal)
                }
                #-----------------------------------------------------------------------------------------------------------------------------------------------------------------
                Data <- 
                        
                        sqlQuery(conn,paste("select",Value," as Val,TruckName, CalibrationVersion FROM ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],tbl,"JOIN",
                                            PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks on",PrgMap$Database[[which(PrgMap$Programs==input$Program)]], 
                                            tbl,".TruckID = ", PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblTrucks.TruckID",
                                            #" Where ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblEventDrivenData",".TruckID in (",paste(as.character(TruckID),collapse = ","),") and SEID = ",SEID
                                            WhereClause
                                            ))
                
                 # 
                 
                if (nrow(Data) > 0 ){
                        
                        output$Tplot <- renderPlot({
                                
                                
                                TgtDat <- Data$TruckName
                               
                                p <-ggplot(data = Data,aes(x=TruckName,y=Val,color = TruckName))+geom_boxplot(outlier.colour = "white")+  geom_jitter(position = position_jitter(0.1,0)) + coord_flip()+ theme_bw()
                                print(p)
                                
                        }) 
                        
                        output$hist <- renderPlot({
                                q <- ggplot(Data,aes(x=Val)) + geom_histogram(bandwidth = 0.5,colour="black", fill="blue")+ theme_bw()
                                print(q)
                        })
                        
                }
        })
        
        
        
}

shinyApp(ui = ui, server = server)