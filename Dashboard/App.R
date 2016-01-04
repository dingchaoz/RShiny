# call libraries required
library(shiny)
library(RODBC)
library(RSQLServer)
library(ggplot2)
library(dplyr)
library(xlsx)
source('C:/Users/ks692/Documents/Dingchao/Courses/R/RShinyDash/Dashboard/POSIXt2matlabUTC.R')
source('C:/Users/ks692/Documents/Dingchao/Courses/R/RShinyDash/Dashboard/IUPRQuery.R')
source('C:/Users/ks692/Documents/Dingchao/Courses/R/RShinyDash/Dashboard/PpK.R')

# connect to the server need to be going to global.R at a later stage.
conn <-odbcConnect("Capability")
conn2 <- odbcConnect("IUPR")
PrgMap <- list(Programs = c("Pick Up", "Chasis Cab"), Database = c("Seahawk", "DragonCC"))
def_trk <- sqlQuery(conn, paste("select [TruckName] from",PrgMap$Database[[1]],".dbo. tblTrucks"))
DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[1]],".dbo. tblProcessingInfo"))
SoftwareBuild <- sqlQuery(conn,paste("Select distinct calibration from",PrgMap$Database[[1]], " .dbo.tblDataInBuild"))
trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[1]],".dbo. tblTrucks"))
PU_Cals <- read.xlsx("DragonPU_Cals.xlsx",1)


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
                
                # make IUPR checkbox available
                checkboxInput(inputId = "IUPRInf", label = "Show IUPR information"),
                
                # action Button
                actionButton(inputId = "Update", label = "Update"),
                
                
                width = 3
                
                
                
                
                
        ),
        #         mainPanel( plotOutput("Tplot"),
        #                    plotOutput("hist"),
        #                    plotOutput("Splot"),
        #                    plotOutput("box")
        #                    
        #                    )
        mainPanel(
                fluidRow(column(12,"",
                                fluidRow(
                                        column(8,
                                               plotOutput("Tplot")),
                                        column(width = 4,
                                               plotOutput("hist")))), fluidRow(
                                                       column(6,plotOutput("Splot")),
                                                       column(width =6,plotOutput("IUPR"))
                                               ), fluidRow(
                                                       column(6,plotOutput("Dplot")),
                                                       column(width =6, plotOutput("Nplot"))
                                               )
                         
                )
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
                                          as.character(trucks$Family))
                # browser()
                #                 # Checking if there are inputs to Truck Group; if so, make the choices of trucks limited to the grouping.
                #                 if (!is.null(input$TrucksGrp)){
                #                         sizes <- tapply(input$TrucksGrp, seq(1:length(input$TrucksGrp)), nchar)
                #                 trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks where Family in (",paste0(str_pad(input$TrucksGrp,sizes+2,pad="'","both"),collapse = ","),")"))
                #                 }
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
                USL <- DiagList$USL[which(DiagList$Name==input$Diag)]
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
                
                #Setting the where clause for Software
                if((!identical(input$FrmCal,""))){
                        WhereClause <- paste(WhereClause,"AND CalibrationVersion >=", input$FrmCal)
                }
                if((!identical(input$ToCal,""))){
                        WhereClause <- paste(WhereClause,"AND CalibrationVersion <=", input$ToCal)
                }
                
                # If IUPR Data is requested
                if(input$IUPRInf == 1){
                        IUPRTrks <- sqlQuery(conn,paste("Select TruckName FROM ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblTrucks where TruckID in (",paste(as.character(TruckID),collapse = ","),")"))
                        IUPRTrks <- as.character(IUPRTrks$TruckName)
                        IUPRQry <- IUPRQuery(Program = PrgMap$Database[[which(PrgMap$Programs==input$Program)]],SEID = SEID,FrmSoftware = input$FrmCal,ToSoftware = input$ToCal,Trucks = IUPRTrks,DateRange = input$DateRange)
                        IUPRData <- sqlQuery(conn2,query = IUPRQry)
                        
                        IUPRSummary <- summarise(group_by(IUPRData, TruckName),Numerator = sum(Numerator, na.rm = T), Denominator = sum(Denominator, na.rm = T), IUPR = Numerator/Denominator)
                }
                
                #-----------------------------------------------------------------------------------------------------------------------------------------------------------------
                Data <- 
                        
                        sqlQuery(conn,paste("select",Value," as Val,TruckName, CalibrationVersion FROM ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],tbl,"JOIN",
                                            PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks on",PrgMap$Database[[which(PrgMap$Programs==input$Program)]], 
                                            tbl,".TruckID = ", PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblTrucks.TruckID",
                                            #" Where ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblEventDrivenData",".TruckID in (",paste(as.character(TruckID),collapse = ","),") and SEID = ",SEID
                                            WhereClause
                        ))
                
                if(is.na(LSL)){
                        
                        LSL_Value <- NaN
                }
                else{
                        LSL_Value <- PU_Cals$Value[which(PU_Cals$Parameter==as.character(LSL))]
                }
                if(is.na(USL)){
                        
                        USL_Value <- NaN
                }
                else{
                        USL_Value <- PU_Cals$Value[which(PU_Cals$Parameter==as.character(USL))] 
                }
                
                DescSats <- Ppk(Data$Val,LSL = LSL_Value,USL = USL_Value)
                
                
                if (nrow(Data) > 0 ){
                        
                        output$Tplot <- renderPlot({
                                
                                
                                # TgtDat <- Data$TruckName
                               
                                
                                p <-ggplot(data = Data,aes(x=TruckName,y=Val,color = TruckName))+geom_boxplot(outlier.colour = "white")+  geom_jitter(position = position_jitter(0.1,0)) +  coord_flip()+ theme_bw()+ theme(legend.position = "none") + theme(axis.title.y = element_blank())+ ylab(Parameter)
                                p <- p + geom_hline(yintercept = c(LSL_Value, USL_Value),color = "Red", linetype = "longdash" ) + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), ""))))) + expand_limits(y = LSL_Value) + scale_y_continuous(limits = c(min(c(Data$Val,LSL_Value - 0.5),na.rm = T),max(c(Data$Val,USL_Value),na.rm = T)))
                                p <- p + geom_text(data = NULL,y = LSL_Value,x = 10, label = "LSL", color = "red")
                                print(p)
                                
                        }) 
                        
                        output$Splot <- renderPlot({
                                
                                
                                # TgtDat <- Data$TruckName
                                
                                r <-ggplot(data = Data,aes(x=as.factor(CalibrationVersion),y=Val,color = as.factor(CalibrationVersion)))+geom_boxplot(outlier.colour = "white")+  geom_jitter(position = position_jitter(0.1,0)) + coord_flip()+ theme_bw()+ theme(legend.position = "none") + theme(axis.title.y = element_blank())+ ylab(Parameter)
                                r <- r + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), ""))))) 
                                print(r)
                                
                        }) 
                        
                        output$hist <- renderPlot({
                                q <- ggplot(Data,aes(x=Val)) + geom_histogram(bandwidth = 0.5,colour="black", fill="blue")+ theme_bw()+ xlab(Parameter)
                                q <- q + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), "")))))
                                print(q)
                        })
                        if(input$IUPRInf == 1){
                                output$IUPR <- renderPlot({
                                        # r <- qplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T) + geom_bar(aes(colors = IUPRSummary$TruckName))+ theme_bw()+ coord_flip()+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
                                        r <- qplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T,geom = "bar",stat = "identity")+ theme_bw()+ coord_flip()+ theme(axis.title.y = element_blank())+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
                                        print(r)
                                })
                                
                                output$Dplot <- renderPlot({
                                        # r <- qplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T) + geom_bar(aes(colors = IUPRSummary$TruckName))+ theme_bw()+ coord_flip()+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
                                        s <- qplot(data = IUPRSummary,x = TruckName, y = Denominator,na.rm = T,geom = "bar",stat = "identity")+ theme_bw()+ coord_flip()+ theme(axis.title.y = element_blank())#+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$Denominator,na.rm = T), max(IUPRSummary$Denominator,na.rm = T), by = 10),1))
                                        print(s)
                                })
                                
                                output$Nplot <- renderPlot({
                                        # r <- qplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T) + geom_bar(aes(colors = IUPRSummary$TruckName))+ theme_bw()+ coord_flip()+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
                                        t <- qplot(data = IUPRSummary,x = TruckName, y = Numerator,na.rm = T,geom = "bar",stat = "identity")+ theme_bw()+ coord_flip()+ theme(axis.title.y = element_blank())#+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$Numerator,na.rm = T), max(IUPRSummary$Numerator,na.rm = T), by = 0.1),1))
                                        print(t)
                                })
                        }
                        else {
                                output$IUPR <- renderPlot({})
                                output$Dplot <- renderPlot({})
                                output$Nplot <- renderPlot({})
                                
                        }
                        
                        
                        
                }
        })
        
        
        
}

shinyApp(ui = ui, server = server)