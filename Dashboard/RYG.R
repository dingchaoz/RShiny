# Grab the diagnostic to be evaluated and the Red criteria
# For each parameter; Query data & Calculate Ppk for each parameter and report the min Ppk & max number for failures
# Also Evaluate Red / Green
library(xlsx)
library(RODBC)
library(RSQLServer)
connection <-odbcConnect("Capability")
Diagnostics <- read.xlsx("FCA_RYG.xlsx",1)
# eval(parse(text = paste(length(Diagnostics$SEID),Diagnostics$Red[4])))
for(i in 1:1){
        browser()
       SQL_Query <- CapQuery(program='Seahawk',SEID=Diagnostics$SEID[i],ExtID = Diagnostics$ExtID[i],Cap_Param = Diagnostics$Parameter[i],truckGroup = 'Dragnet_PU_17')
       if(is.na(LSL)){
               
               LSL_Value <- NaN
       }
       else{
               # check if the LSL & USL are hard coded in the processing list
               if (!is.na(as.numeric(as.character(LSL)))){LSL_Value = as.numeric(as.character(LSL))} else{
                       # check if the threshold is a table value
                       if(stri_sub(as.character(LSL),-3,-1,3)=='(1)'){LSL <- stri_sub(as.character(LSL),1,nchar(as.character(LSL))-3)}
                       LSL_Value <- sqlQuery(conn,paste0("select Value from tblCals1 where Family = 'Default' and Threshold = '",LSL,"'"))
                       LSL_Value <- LSL_Value$Value}
       }
       if(is.na(USL)){
               
               USL_Value <- NaN
       }
       else{
               if (!is.na(as.numeric(as.character(USL)))){USL_Value = as.numeric(as.character(USL))} else{
                       if(stri_sub(as.character(USL),-3,-1,3)=='(1)'){USL <- stri_sub(as.character(USL),1,nchar(as.character(USL))-3)}
                       USL_Value <- sqlQuery(conn,paste0("select Value from tblCals1 where Family = 'Default' and Threshold = '",USL,"'"))
                       USL_Value <- USL_Value$Value}
       }
       
}
