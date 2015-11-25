# This function returns a SQL query (string). This is intended to be used within Shiny R.
# Usage: IUPRQuery(SEID,FrmSoftware, ToSoftware,Trucks,DateRange, CalRev)
# Where : SEID is a numeric vector (of size 1)
#       : FrmSoftware is a numeric vector denoting begining software
#       : ToSoftware is a numeric vector denoting ending software
#       : Trucks is a string vector with Truck names
#       : DateRange is a numeric vector with begining of the time window and end of the time window
#       : CalRev is a numeric vector - is not used in the current version - may be later
# Example: 
# Author : Sri Seshadri ; Diagnostics Data Analysis Group Leader
# Date : 2015-11-24

IUPRQuery <- function(Program,SEID,FrmSoftware = "", ToSoftware="",Trucks="",DateRange=c("",""), CalRev=""){
        
        # ----------------------------CONNECTING TO Database---------------------------------------------------------------
        library(RODBC)
        library(RSQLServer)
        library(stringr)
        connection <- odbcConnect("IUPR")
        
        if (is.null(SEID)| nchar(Program)< 1){
                stop("Hello there!! how about letting me know the SEID & the Program?")
        }
        setIDQry <- paste("SELECT SetID FROM qryLatestSEID")
        # -------------------------WHERE CLAUSE --------------------------------------------------------------------
        if (nchar(Trucks)>0){
                TruckID <- sqlQuery(connection,paste0("Select TruckID from ",Program," .dbo.tblTrucks where TruckName in ('",paste0(str_pad(Trucks,3,pad="'","both"),collapse = ","),"')"))
                browser()
                where <- paste0("WHERE TruckID in (",TruckID,collapse = ",",")")
        }
        else {
                where <- "Where"
        }
        if (nchar(FrmSoftware) > 1 & nchar(ToSoftware) > 1) {
                where <- paste(where,"AND Calibration_version >=", FrmSoftware, "and Calibration_version <=", ToSoftware )
        }
        else if(nchar(FrmSoftware) > 1){
                where <- paste(where,"AND Calibration_version >=", FrmSoftware )
        }
        else if(nchar(ToSoftware) > 1) {
                where <- paste(where,"AND Calibration_version <=", ToSoftware )
        }
        if (nchar(DateRange[1])> 0 & nchar(DateRange[2])> 0){
                where <- paste(where,"AND DateTime BETWEEN", DateRange[1], "AND", DateRange[2])
        }
        else if(nchar(DateRange[1]) > 1){
                where <- paste(where,"AND DateTime >=", DateRange[1])
        }
        else if(nchar(DateRange[2]) > 1) {
                where <- paste(where,"AND DateTime <=", DateRange[2])
        }
        
        setIDQry <- paste(setIDQry,where)
        
        Query <- paste("SELECT SEID ,TruckName, qrySystemErrorData.Calibration_Version,qrySystemErrorData.Calibration_Revision_Number , Numerator, Denominator, IUPR from qrySystemErrorData",
                       "INNER JOIN (",setIDQry,") as temptbl on qrySystemErrorData.SetID = temptbl.SetID where SEID =", SEID,"order by TruckName,Calibration_Version, Calibration_Revision_Number")
}