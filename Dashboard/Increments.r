# This function is intended to be used with Shiny R
# The function  calculates the Numerator increments of any given diagnostic as queried by the "NumDenomCounts"
# The input to this function is a data frame that is an output of the function "NumDenomCounts" (NumDenomCounts.R)
library(ggplot2)
Increments <- function(Data) {
        
        size <- dim(Data)
        Diff <- c()
        for(i in 1:size[1])
        {
                if(i==1){
                        Diff[i] <- Data$Numerator[1]
                } 
                else {
                        
                        if((Data$TruckName[i]==Data$TruckName[i-1]) & (Data$Calibration_version[i]==Data$Calibration_version[i-1]))
                        {
                                if(is.na(Data$Calibration_Revision_Number[i]==Data$Calibration_Revision_Number[i-1])|(Data$Calibration_Revision_Number[i]==Data$Calibration_Revision_Number[i-1]))
                                {   
                                        if(Data$Numerator[i]>= Data$Numerator[i-1])
                                        {
                                                Diff[i]<- Data$Numerator[i]-Data$Numerator[i-1] 
                                        }
                                        else {
                                                Diff[i] <- Data$Numerator[i] 
                                        }
                                }
                        }
                        else {
                                Diff[i] <- Data$Numerator[i]
                        }
                }
        }
        Data$Increments <- Diff
        Data
}