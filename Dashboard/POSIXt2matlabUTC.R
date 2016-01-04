#Convert POSIXct or POSIXlt objects to MATLAB datenum, in UTC time zone. 
#All time stamps with non-GMT/UTC time zones will be first converted to the 
#GMT/UTC time zone, then converted to MATLAB datenum value. 
POSIXt2matlabUTC = function(x) {
        if (class(x)[1] == "POSIXct") {
                x = as.POSIXlt(x, tz = "UTC") #convert to UTC time zone
                days = as.numeric(x) / 86400 #convert to days
                datenum = days + 719529 #convert to MATLAB datenum
        } else if (class(x)[1] == "POSIXlt") {
                x = as.POSIXlt(x, tz = "UTC") #convert to UTC time zone
                days = as.numeric(x) / 86400  #convert to days
                datenum = days + 719529 #convert to MATLAB datenum
        } else {stop("POSIXct or POSIXlt object required for input")}
        return(datenum)
}
#The output is a numeric vector of 'days since 0000-1-1 00:00', adjusted to UTC