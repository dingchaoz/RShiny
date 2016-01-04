# This function intends to calculate the PpK of a diagnostic; given the data, LSL / USL or both
# Also provides average, stdev & count of points outside the failure threshold NOTE: Points on the LSL / USL are considered failed points
# Usage : Ppk(Data, LSL = NA, USL = NA)
# Where : Data is a numeric vector
#       : LSL & USL are numeric vector
# Author : Sri Seshadri Diagnostics Data Analysis 
# Date  : 2015-Dec-13

Ppk <- function(data,LSL,USL){
        # initialize ppkmin & ppkmax as NA
        ppkmin <- NaN
        ppkmax <- NaN
        
        if(!is.na(LSL)){
                ppkmin <- round((mean(data,na.rm = T)- LSL)/(3*sd(data,na.rm = T)),4)
        }
        if(!is.na(USL)){
                ppkmax <- round((USL - mean(data,na.rm = T))/(3*sd(data,na.rm = T)),4)
        }
        
        list(PpK = min(c(ppkmin,ppkmax),na.rm = T), Average = round(mean(data,na.rm = T),4), Stdev = round(sd(data,na.rm = T),4), Failures = sum(data >= USL | data <= LSL) )
}