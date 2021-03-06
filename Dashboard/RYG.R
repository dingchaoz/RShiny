# Grab the diagnostic to be evaluated and the Red criteria
# For each parameter; Query data & Calculate Ppk for each parameter and report the min Ppk & max number for failures
# Also Evaluate Red / Green
library(xlsx)
library(RODBC)
library(RSQLServer)
source('C:/Users/ks692/Documents/Dingchao/Courses/R/Shiny/Dashboard/CapQuery.R')
source('C:/Users/ks692/Documents/Dingchao/Courses/R/Shiny/Dashboard/PpK.R')

connection <-odbcConnect("Capability")
Diagnostics <- read.xlsx("FCA_RYG.xlsx",1)
ppks <- as.numeric()
FailedPts <- as.numeric()
RYG <- as.character()
RYG_Code <- as.numeric()

# eval(parse(text = paste(length(Diagnostics$SEID),Diagnostics$Red[4])))
for(i in 1:length(Diagnostics)){
        # browser()
       SQL_Query <- CapQuery(program='Seahawk',SEID=Diagnostics$SEID[i],ExtID = Diagnostics$ExtID[i],Cap_Param = Diagnostics$Parameter[i],truckGroup = 'Dragnet_PU_17')
       if(is.na(Diagnostics$LSL)){
               
               LSL_Value <- NaN
       }
       else{
               # check if the LSL & USL are hard coded in the processing list
               if (!is.na(as.numeric(as.character(Diagnostics$LSL)))){LSL_Value = as.numeric(as.character(Diagnostics$LSL))} else{
                       # check if the threshold is a table value
                       if(stri_sub(as.character(Diagnostics$LSL),-3,-1,3)=='(1)'){LSL <- stri_sub(as.character(Diagnostics$LSL),1,nchar(as.character(Diagnostics$LSL))-3)}
                       LSL_Value <- sqlQuery(connection,paste0("select Value from tblCals1 where Family = 'Default' and Threshold = '",Diagnostics$LSL,"'"))
                       LSL_Value <- LSL_Value$Value}
       }
       if(is.na(Diagnostics$USL)){
               
               USL_Value <- NaN
       }
       else{
               if (!is.na(as.numeric(as.character(Diagnostics$USL)))){USL_Value = as.numeric(as.character(Diagnostics$USL))} else{
                       if(stri_sub(as.character(Diagnostics$USL),-3,-1,3)=='(1)'){USL <- stri_sub(as.character(Diagnostics$USL),1,nchar(as.character(Diagnostics$USL))-3)}
                       USL_Value <- sqlQuery(connection,paste0("select Value from tblCals1 where Family = 'Default' and Threshold = '",Diagnostics$USL,"'"))
                       USL_Value <- USL_Value$Value}
       }
       
       Data <- sqlQuery(connection,SQL_Query)
       DescSats <- Ppk(Data$Val,LSL = LSL_Value,USL = USL_Value)
       ppks[i] <- DescSats$PpK
       FailedPts[i] <- DescSats$Failures
       # browser()
       if(length(grep("C_",strsplit(as.character(Diagnostics$Red[i]),split = " ")[[1]]))> 0){
               # Case in which Red criteria includes a Calibratable parameter
               x <- grep("C_",strsplit(as.character(Diagnostics$Red[i]),split = " ")[[1]])
               Threshold <- strsplit(as.character(Diagnostics$Red[i]),split = " ")[[1]][x]
               Thd_val <- sqlQuery(connection,paste0("select Value from tblCals1 where Family = 'Default' and Threshold = '",Threshold,"'"))
               eval(parse(text = paste(strsplit(as.character(Diagnostics$Red[i]),split = " ")[[1]][x],'<-',Thd_val)))
               Flags <- eval(parse(text = paste("sum(Data$Val",Diagnostics$Red[i],")")))
               if(Flags > 0){
                       RYG [i] <- "Red"
                       RYG_Code[i] <- 1
               } else {
                       RYG[i] <- "Green"
                       RYG_Code[i] <- 2
               }
       } else if(length(grep("PpK",strsplit(as.character(Diagnostics$Red[i]),split = " ")[[1]]))> 0) {
               # case where Red criteria includes a PpK < 1.5 criteria
               if(ppks[i] <= 1.5 | FailedPts[i] > 0){
                       RYG [i] <- "Red"
                       RYG_Code[i] <- 1
               } else {
                       RYG[i] <- "Green"
                       RYG_Code[i] <- 2 
               }
       } else {
               # case where the Red criteria is any data over or less than a specific value.
               
               Flags <- eval(parse(text = paste("sum(Data$Val",Diagnostics$Red[i],")")))
               if(Flags > 0){
                       RYG [i] <- "Red"
                       RYG_Code[i] <- 1
               } else {
                       RYG[i] <- "Green"
                       RYG_Code[i] <- 2
               }
       }
       
       
}
