# This function is intended to be used within R Shiny Dashboard
# The purpose of this function is output a query that is used to grab data from qrySystemErrorData for constraints such as
#				1. SEID as Integer
#				2. Trucks as Character vector
#  				3. Date Range is a numeric vector with begining of the time window and end of the time window
#				4. Software
#				5. Calibration Rev is a numeric vector - is not used in the current version - may be later
#				6. Program
# Author : Sri Seshadri 12 FEB 2016

NumDenomCounts <- function(Program,SEID,FrmSoftware = "", ToSoftware="",Trucks="",DateRange=c("",""), CalRev=""){
 # ----------------------------CONNECTING TO Database---------------------------------------------------------------
        library(RODBC)
        library(RSQLServer)
        library(stringr)
        connection <- odbcConnect("IUPR")
# ----------------------------------------------------------------------------------------------------------------------
		if (is.null(SEID)| nchar(Program)< 1){
                stop("Hello there!! how about letting me know the SEID & the Program?")
        } # if (is.null(SEID)| nchar(Program)< 1)
		where <- paste("Where SEID = ",SEID, "AND Numerator is NOT NULL")
		if (nchar(Trucks) > 0){
			sizes <- tapply(Trucks, seq(1:length(Trucks)), nchar)
			where <- paste(where,"AND TruckName in (",paste0(str_pad(Trucks,sizes+2,pad="'","both"),collapse = ","),")")
		} # if nchar(Trucks > 0)
		if (nchar(FrmSoftware) > 1 & nchar(ToSoftware) > 1) {
                where <- paste(where,"AND Calibration_version >=", FrmSoftware, "and Calibration_version <=", ToSoftware )
        } # if (nchar(FrmSoftware) > 1 & nchar(ToSoftware) > 1)
        else if(nchar(FrmSoftware) > 1){
                where <- paste(where,"AND Calibration_version >=", FrmSoftware )
        } # else if(nchar(FrmSoftware) > 1)
        else if(nchar(ToSoftware) > 1) {
                where <- paste(where,"AND Calibration_version <=", ToSoftware )
        } # else if(nchar(ToSoftware) > 1)
        if (nchar(DateRange[1])> 0 & nchar(DateRange[2])> 0){
                where <- paste(where,"AND DateTime BETWEEN", paste0("'",DateRange[1],"'"), "AND", paste0("'",DateRange[2],"'"))
        } # if (nchar(DateRange[1])> 0 & nchar(DateRange[2])> 0)
        else if(nchar(DateRange[1]) > 1){
                where <- paste(where,"AND DateTime >=", paste0("'",DateRange[1],"'"))
        } # else if(nchar(DateRange[1]) > 1)
        else if(nchar(DateRange[2]) > 1) {
                where <- paste(where,"AND DateTime <=", paste0("'",DateRange[2],"'"))
        } # else if(nchar(DateRange[2]) > 1)
		
		Query <- paste("Select SetID, TruckName, SEID, Numerator, Denominator,IUPR, OBDFaultStatusTable,OBDStatusFlag, OBDDiagnosticDisabled, DateTime, Calibration_version, Calibration_Revision_Number","from",paste0(Program,".dbo.qrySystemErrorData"),where, "ORDER BY TruckName,DateTime ASC")
}

