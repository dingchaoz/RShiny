## Assignment week 2 Part 1

## Goal : Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.
##The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
##Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument 
## and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

## Author : Srivathsan Seshadri, '2015-05-17'

pollutantmean <- function(directory,pollutant,id){
        
        ## ------------------------------------------------------------------
        ## where directory is a character vector of length 1
        ## indicating where the csv files are located
        
        ## pollutant is a character vector of length 1 indicating the
        ## name of the pollutant for which the user will calculate the 
        ## mean; either for "Sulphate" or "nitrate"
        
        ## id is an integer vector indicating the monitor ID numbers 
        ## to be used by the user.
        
        ##-------------------------------------------------------------------
        
        ## setting working directory to the specified argument directory
        setwd(directory)
        
        ## Declare an empty pollutant data vector
        
        pollutant_data <- numeric()
        
        ## Get pollutant data (Sulphate or Nitrate) for each of the monitor provided by the user from csv files in the directory
        
        
        for (k in seq_along(id)){ ## converts id to string to be used in the path
                if (id[k] < 10) {
                        fileid <- paste("00",toString(id[k]),".csv",sep="")
                }
                else if (id[k]>=10 && id[k] < 100 ){
                        fileid <- paste("0",toString(id[k]),".csv",sep="")  
                }
                else {
                        fileid <- paste(toString(id[k]),".csv",sep="")
                }
                path2file <- file.path(getwd(),fileid) ## gets the path to the file thats required to get data from
                
                header <- read.table(path2file,header=F,nrows=1,sep=",") ## Read only the header
                Requiredcol2read <- which(header==pollutant) ## Get column to read, this way we are not reading the entire file into memory, saves space
                
                ## read only the required column from file
                
                data <- read.csv(path2file,header=T)[,Requiredcol2read]
                
                ## concatenate the vector data to pollutant_data
                
                pollutant_data <- c(pollutant_data,data)
                
        }
        
        ## Calculate mean of pollutant_data without including NA's
        
        mean(pollutant_data,na.rm=T)
        
}
