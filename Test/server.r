shinyServer(function(input,output,session){
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
                 
                
                DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblProcessingInfo"))
                
                updateSelectInput(session,"Diag",label = "Choose Diagnostics of interest here",choices = 
                                          as.character(DiagList$Name))
                
                
                
        })
        
        observe({
               
                trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                updateSelectInput(session,"Trucks", label = "choose trucks here", choices = as.character(trucks$TruckName[which(trucks$Family==input$TrucksGrp)]))})
        
        observeEvent(input$Update,{
                #---------------------------------------------------GETTING INITAL VLAUES-----------------------------------------------------------------
                paste("I Am here now what??")
                SEID <- DiagList$SEID[which(DiagList$Name==input$Diag)]
                ExtID <- DiagList$ExtID[which(DiagList$Name==input$Diag)]
                Parameter <- DiagList$CriticalParam[which(DiagList$Name==input$Diag)]
                LSL <- DiagList$LSL[which(DiagList$Name==input$Diag)]
                USL <- DiagList$USL[which(DiagList$Name==input$Diag)]
                startDate <- POSIXt2matlabUTC(as.POSIXlt(input$DateRange[1],"UTC"))
                endDate <- POSIXt2matlabUTC(as.POSIXlt(input$DateRange[2],"UTC"))
                # browser()
                
                # had to re-write the below line from the observe block; uncertain about the scoping rules.
                trucks <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblTrucks"))
                DiagList <- sqlQuery(conn, paste("select * from",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo. tblProcessingInfo"))
                TruckID <- trucks$TruckID[which(trucks$Family %in% input$TrucksGrp)]
                # -----------------------------------ACT DEPENDING ON THE TAB THE USER IS IN---------------------------------------
                if(input$plots == 2){
                        # browser()
                        RYG <- RYG_Grade(program = PrgMap$Database[which(PrgMap$Programs == input$Program)],FSoftware = input$FrmCal,TSoftware = input$ToCal,Trks = input$Trucks, truckGrp = input$TrucksGrp,DateRange = input$DateRange)
                        output$RYG <- renderDataTable({RYG},options = list(scrollX = TRUE))
                }
                else {
                #------------------------------------------------------SETTING THE WHERE CLAUSE--------------------------------------------------------------------------------
                # initializing a empty WhereClause vector
                WhereClause = as.character()
                
                # Setting where clause for Parameter and Date - this the default where clause
          
                if (is.na(ExtID)){
                        tbl <- ".dbo.tblMinMaxData"
                        # Calibration in below query may not be needed; thought it was needed for a very complicated reason. Thought in a nut shell - what if one PublicDataID could mean
                        # different parameters in different builds? And what if we need the different parameters as capability parameter?
                        PID <- sqlQuery(conn,paste("Select Distinct PublicDataID from",PrgMap$Database[which(PrgMap$Programs == input$Program)], ".dbo.tblDataInBuild where Data = ",paste0("'",Parameter,"'")))
                        WhereClause <- paste("Where PublicDataId in ( ",paste(PID$PublicDataID,collapse=","), ")","AND datenum between", startDate ,"AND", endDate, "AND EMBFlag = 0")
                        # 
                        if (is.na(LSL)& is.na(USL)){
                                stop(paste("Ask your friends in the data analysis team update the LSL & USL Parameters for", input$Diag, "& capability parameter", Parameter))
                        }
                        else if(!is.na(LSL)& !is.na(USL)){
                                Value <- paste("DataMin , DataMax")
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
                        WhereClause <- paste("Where SEID = ", SEID, " AND ExtID = ", ExtID,"AND datenum between", startDate ,"AND", endDate,"AND EMBFlag = 0")
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
                        # browser()
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
                 # browser()
                if(Value == "DataMin , DataMax"){
                        # browser()
                        Data <-  melt(Data,id.vars = 3:4)
                        names(Data)[names(Data)=="value"] <- "Val"
                }   
                if(is.na(LSL)){
                        
                        LSL_Value <- NaN
                }
                else{
                        # check if the LSL & USL are hard coded in the processing list
                        if (!is.na(as.numeric(as.character(LSL)))){LSL_Value = as.numeric(as.character(LSL))} else{
                                # check if the threshold is a table value
                                if(stri_sub(as.character(LSL),-3,-1,3)=='(1)'){LSL <- stri_sub(as.character(LSL),1,nchar(as.character(LSL))-3)}
                                LSL_Value <- sqlQuery(conn,paste0("select Value from tblCals1 where Family = 'Default'and CalVersion = 32170017 and Threshold LIKE '",LSL,"%'"))
                                LSL_Value <- LSL_Value$Value[1]}
                }
                if(is.na(USL)){
                        
                        USL_Value <- NaN
                }
                else{
                        if (!is.na(as.numeric(as.character(USL)))){USL_Value = as.numeric(as.character(USL))} else{
                                if(stri_sub(as.character(USL),-3,-1,3)=='(1)'){USL <- stri_sub(as.character(USL),1,nchar(as.character(USL))-3)}
                                USL_Value <- sqlQuery(conn,paste0("select Value from tblCals1 where Family = 'Default'and CalVersion = 32170017 and Threshold LIKE '",USL,"%'"))
                                USL_Value <- USL_Value$Value}
                }
                
                DescSats <- Ppk(Data$Val,LSL = LSL_Value,USL = USL_Value)
                
                
                if (nrow(Data) > 0 ){
				
						f1 <- list(
														family = "Arial, sans-serif",
														size = 12,
														color = "black"
														)
											f2 <- list(
														family = "Old Standard TT, serif",
														size = 10,
														color = "black"
														)

											a <- list(
        
													titlefont = f1,
													showticklabels = TRUE,
													tickangle = 0,
													tickfont = f2
       
       
													)
                        
                        output$Tplot <- renderPlotly({
                                
                                
                                # TgtDat <- Data$TruckName
                                
                                
                                 # p <-ggplot(data = Data,aes(x=TruckName,y=Val,fill = TruckName))+geom_boxplot(outlier.colour = "white")+  geom_jitter(position = position_jitter(0.1,0)) +  coord_flip()+ theme_bw()+ theme(legend.position = "none") + theme(axis.title.y = element_blank())+ ylab(paste(Parameter,"\n",paste("PpK =",DescSats$PpK,"Mean =", DescSats$Average, "Std.dev =",DescSats$Stdev, "Failures:",DescSats$Failures)))
                                 # p <- p + geom_hline(yintercept = c(LSL_Value,USL_Value),color = "Red", linetype = "dash" ) + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), ""))))) +  scale_y_continuous(limits = c(min(c(Data$Val,LSL_Value - 0.5,USL_Value-0.5),na.rm = T),max(c(Data$Val,USL_Value +0.5, LSL_Value + 0.5),na.rm = T)),breaks = scales::pretty_breaks(n = 10))
                                 # if(!is.na(LSL_Value)){
                                         # p <- p + geom_text(data = NULL,y = LSL_Value,x = 0.5, label = "LSL", color = "red")
                                 # }
                                 # if(!is.na(USL_Value)){
                                         # p <- p + geom_text(data = NULL,y = USL_Value,x = 0.5, label = "USL", color = "red")
                                 # }
                                 # browser()
                                 # ggplotly()
								
								 p <- plot_ly(data = Data,type = "box", boxpoints = "all", jitter = 0.1, x = Val, y = TruckName,color = TruckName,orientation ='h')
								 layout(p = p, xaxis = a, yaxis = a, showlegend = FALSE, margin= list(l=200))
										 layout(xaxis = list(title = paste(Parameter,"\n",paste("PpK =",DescSats$PpK,"Mean =", DescSats$Average, "Std.dev =",DescSats$Stdev, "Failures:",DescSats$Failures))))
                                
                        })
                        
                        output$Splot <- renderPlot({
                                
                                
                                # TgtDat <- Data$TruckName
                                
                                r <-ggplot(data = Data,aes(x=as.factor(CalibrationVersion),y=Val,color = as.factor(CalibrationVersion)))+geom_boxplot(outlier.colour = "white")+  geom_jitter(position = position_jitter(0.1,0)) + coord_flip()+ theme_bw()+ theme(legend.position = "none") + theme(axis.title.y = element_blank())+ ylab(Parameter)
                                r <- r + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), ""))))) 
                                print(r)
                                
                        }) 
                        
                        output$hist <- renderPlot({
                                q <- ggplot(Data,aes(x=Val)) + geom_histogram(colour="black", fill="blue")+ theme_bw()+ xlab(Parameter)
                                q <- q + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), "")))))
                                print(q)
                        })
                        if(input$IUPRInf == 1){
                                output$IUPR <- renderPlotly({
                                        # r <- qplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T) + geom_bar(aes(colors = IUPRSummary$TruckName))+ theme_bw()+ coord_flip()+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
                                       # r <- ggplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T,geom = "bar", fill = TruckName)+ theme_bw()+ coord_flip()+ theme(legend.position = "none")+ theme(axis.title.y = element_blank())+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
										
										r <- ggplot(data = IUPRSummary,aes(x = TruckName, y = IUPR,fill = TruckName))+ geom_bar(stat = "identity")+ theme_bw()+ coord_flip() +
										 theme(legend.position = "none")+ theme(axis.title.y = element_blank())+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
										
					# browser()					
                                         r <- r + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), "")))))
                                         #r <- ggplotly(r)
										 # grph <- gg2list(r)
 										# return(list(
 												# list(
 													 # id = "IUPR",
 													 # data = grph$data,
													 # layout = grph$layout
													 												# )
												# ))
	# # 										grph
# Just using plotly syntax
											
										 r <- plot_ly(data=IUPRSummary,x = IUPR,y = TruckName,type = "bar",color = TruckName,orientation ='h')
										layout(p = r, xaxis = a, yaxis = a, showlegend = FALSE, margin= list(l=200))
										layout(xaxis = list(title = "IUPR"))
										 
										
                                })
                                
                                output$Dplot <- renderPlot({
                                        # r <- qplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T) + geom_bar(aes(colors = IUPRSummary$TruckName))+ theme_bw()+ coord_flip()+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
                                        s <- ggplot(data = IUPRSummary,aes(x = TruckName, y = Denominator,na.rm = T,fill = TruckName))+ geom_bar(stat="identity")+ theme_bw()+ coord_flip()+ theme(legend.position = "none")+ theme(axis.title.y = element_blank())#+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$Denominator,na.rm = T), max(IUPRSummary$Denominator,na.rm = T), by = 10),1))
                                        s <- s + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), "")))))
                                         print(s)
                                })
                                
                                click_saved <- reactiveValues(doubleclick = NULL)
								click_saved2 <- reactiveValues(singleclick = NULL)
								#browser()
										observeEvent(eventExpr = input$plot_dblclick, handlerExpr ={ click_saved$doubleclick <- input$plot_dblclick})
										observeEvent(eventExpr = input$plot_click, handlerExpr ={ click_saved2$singleclick <- input$plot_click})
										
                                observe({if(is.null(click_saved$doubleclick)|!is.null(click_saved2$singleclick)){ 
										
                                        output$Nplot <-	renderPlot({
                                        # r <- qplot(data = IUPRSummary,x = TruckName, y = IUPR,na.rm = T) + geom_bar(aes(colors = IUPRSummary$TruckName))+ theme_bw()+ coord_flip()+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$IUPR,na.rm = T), max(IUPRSummary$IUPR,na.rm = T), by = 0.1),1))
                                        t <- ggplot(data = IUPRSummary,aes(x = TruckName, y = Numerator,na.rm = T,fill = TruckName))+ geom_bar(stat="identity")+ theme_bw()+ coord_flip()+ theme(legend.position = "none")+ theme(axis.title.y = element_blank())#+ scale_y_continuous(breaks = round(seq(min(IUPRSummary$Numerator,na.rm = T), max(IUPRSummary$Numerator,na.rm = T), by = 0.1),1))
                                        t <- t + ggtitle(bquote(atop(.(input$Diag), atop(italic(.(input$Program)),atop(.(input$TrucksGrp), "")))))
                                        t
                                })
								} else {
										
										#browser()
										lvls <- levels(IUPRSummary$TruckName)
										Query <- NumDenomCounts(Program = PrgMap$Database[[which(PrgMap$Programs==input$Program)]],SEID = SEID,Trucks = lvls[round(click_saved$doubleclick$y)])
										RawData <- sqlQuery(conn2, Query)
										increments <- Increments(RawData)
										increments <- increments$data
										isolate({output$Nplot <-	renderPlot({
										t <- ggplot(increments,aes(DateTime,Increments,group=1,colours('Red')))+geom_line(color='red',)+ theme_bw()+geom_point(color='red')+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
										t
												})})
										
										
								}})
                        }
                        else {
                                output$IUPR <- renderPlot({})
                                output$Dplot <- renderPlot({})
                                output$Nplot <- renderPlot({})
                                
                        }
                        
                        
                        
                }
                }
        })
        
        
        
})