# This function returns a SQL query that is intended to be usedwithin Shiny R diagnostics data analysis dashboard
# Usage : CapQuery(program,SEID, ExtID,FrmSoftware = "", ToSoftware="",Trucks="",truckGroup ="",DateRange=c("",""), CalRev="")
# Where : program is a string that denotes the database name corresponding to the program
#       : SEID & ExtID are numeric values that denote the System error ID & the Extension ID
#       : FrmSoftware & ToSoftware are numeric values that denote the software range of interest.
#       : Trucks is a character vector of trucks
#       : TruckGroup is a character vector of truck groups of interest
#       : DateRange is a numeric vector with begining of the time window and end of the time window
#       : CalRev is a numeric vector - is not used in the current version - may be later
# Example: 
# Author : Sri Seshadri; Diagnostics Data Analysis
# Date : 2015 DEC 20

CapQuery <- function(program,SEID, ExtID,Cap_Param = NULL, FrmSoftware = "", ToSoftware="",Trucks=NULL,truckGroup =NULL,DateRange=c("2012-01-01","2017-01-1"), CalRev=""){
        # -------------DATABASE CONNECTION ---------------------------------------------------------
        library(RODBC)
        library(RSQLServer)
        library(stringr)
        source('~/Documents/Coursera_R/Dashboard/POSIXt2matlabUTC.R')
        connection <-odbcConnect("Capability")
        startDate <- POSIXt2matlabUTC(as.POSIXlt(DateRange[1],"UTC"))
        endDate <- POSIXt2matlabUTC(as.POSIXlt(DateRange[2],"UTC"))
        # Error handling before begining
        if (is.null(program) | is.null(SEID)){
                stop("Both program and Diagnostic must be chosen")
        }
        truckstbl <- sqlQuery(connection,paste("Select TruckID, TruckName, Family from",program,".dbo.tblTrucks"))
        if(is.nan(ExtID)){
                if(!is.null(Cap_Param)){
                ExtID_Where <- paste0("CriticalParam = '",Cap_Param,"'")
                } else{stop("Need Critical Parameter for the argument Cap_Param for MinMax Diagnostic")}
        } else {
                ExtID_Where <- paste("ExtID =", ExtID)
        }
        # Query critical parameter
        Parameter <- sqlQuery(connection, paste("select CriticalParam, LSL, USL from",program,".dbo. tblProcessingInfo where SEID =",SEID,"and",ExtID_Where))
        #------------------------------------------------------SETTING THE WHERE CLAUSE--------------------------------------------------------------------------------
        # initializing a empty WhereClause vector
        WhereClause = as.character()
        # browser()
        # Setting where clause for Parameter and Date - this the default where clause
        if (is.na(ExtID)){
                tbl <- ".dbo.tblMinMaxData"
                # Calibration in below query may not be needed; thought it was needed for a very complicated reason. Thought in a nut shell - what if one PublicDataID could mean
                # different parameters in different builds? And what if we need the different parameters as capability parameter?
                PID <- sqlQuery(connection,paste("Select Distinct PublicDataID from", program, ".dbo.tblDataInBuild where Data = ",paste0("'",Cap_Param,"'")))
                WhereClause <- paste("Where PublicDataId in ( ",paste(PID$PublicDataID,collapse=","), ")","AND datenum between", startDate ,"AND", endDate)
                # 
                if (is.na(Parameter$LSL)& is.na(Parameter$USL)){
                        stop(paste("Ask your friends in the data analysis team update the LSL & USL Parameters for SEID =", SEID , "& capability parameter", Parameter$CriticalParam))
                }
                else if(is.na(Parameter$LSL)){
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
        
        if (!is.null(truckGroup)& !is.null(Trucks)){
                TruckID <- intersect(truckstbl$TruckID[which(truckstbl$Family %in% truckGroup)],truckstbl$TruckID[which(truckstbl$TruckName %in% Trucks)])
                WhereClause <- paste(WhereClause,"AND",program,tbl,".TruckID in (",paste(as.character(TruckID),collapse = ","),")")
                
        }
        else if(!is.null(truckGroup)){
                TruckID <- truckstbl$TruckID[which(truckstbl$Family %in% truckGroup)]
                WhereClause <- paste(WhereClause,"AND",program,tbl,".TruckID in (",paste(as.character(TruckID),collapse = ","),")")
        }
        else if(!is.null(Trucks)){
                TruckID <- truckstbl$TruckID[which(truckstbl$TruckName %in% Trucks)]
                WhereClause <- paste(WhereClause,"AND",program,tbl,".TruckID in (",paste(as.character(TruckID),collapse = ","),")")
        }
        
        #Setting the where clause for Software
        if((!identical(FrmSoftware,""))){
                WhereClause <- paste(WhereClause,"AND CalibrationVersion >=", FrmSoftware)
        }
        if((!identical(ToSoftware,""))){
                WhereClause <- paste(WhereClause,"AND CalibrationVersion <=", ToSoftware)
        }
        paste("select",Value," as Val,TruckName, CalibrationVersion FROM ",program,tbl,"JOIN",
              program,".dbo. tblTrucks on",program, 
              tbl,".TruckID = ",program,".dbo.tblTrucks.TruckID",
              #" Where ",PrgMap$Database[[which(PrgMap$Programs==input$Program)]],".dbo.tblEventDrivenData",".TruckID in (",paste(as.character(TruckID),collapse = ","),") and SEID = ",SEID
              WhereClause)
}