#Convert POSIXct, POSIXlt or 'seconds since 1970-1-1' to MATLAB datenum value.
#The conversion drops any time zone associated with the POSIXt value. It is the
#user's responsibility to keep track of time zones in MATLAB datenums.
#The constant 719529 in the function is the days from 0000-1-1 to 1970-1-1.
POSIXt2matlab = function(x) {
        if (class(x)[1] == "POSIXlt"){
                days = as.numeric(as.Date(x)) #extract days since 1970-1-1
                frac.day = (((x$hour)*3600) + ((x$min)*60) + x$sec)/86400
                datenum = 719529 + days + frac.day 
                datenum = 719529 + days + frac.day		
        } else if (class(x)[1] == "POSIXct"){
                x = as.POSIXlt(x) #convert to POSIXlt class
                days = as.numeric(as.Date(x)) #extract days since 1970-1-1
                frac.day = (((x$hour)*3600) + ((x$min)*60) + x$sec)/86400
                datenum = 719529 + days + frac.day
        } else if (class(x)[1] == "numeric"){
                days = x / 86400 #convert seconds to days
                datenum = days + 719529 
        } else {
                stop("Input cannot be coerced to POSIXlt or numeric value")
        }
        return(datenum)
}
#The output is a numeric vector of 'days since 0000-1-1 00:00'. 