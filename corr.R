## Assignment week 2 part 3

## Goal : Write a function that takes a directory of data files and a threshold for complete cases 
## and calculates the correlation between sulfate and nitrate 
## for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 
## The function should return a vector of correlations for the monitors that meet the threshold requirement. 
## If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 

## Author : Srivathsan Seshadri, '2015-05-17'

corr <- function(directory, threshold = 0){
        ##-------------------------------------------------------------------------
        
        ## where directory is a character vector of length 1 indicating 
        ## the location of the csv files
        
        ## where threshold is a numeric vector of length 1 indicating the
        ## number of completely observations
        
        ## setting working directory to the specified argument directory
        setwd("C:/Users/ku906/Documents/Coursera_R")
        
        ## sourcing complete.R that is in the working directory
        complete <- function(directory,id = 1:length(dir())){
                ##-------------------------------------------------------------------------
                
                ## where directory is a character vector of length 1 indicating 
                ## the location of the csv files
                
                ## where id is an integer vector indicating monitor ID numbers
                ## to be provided by the use
                
                ##-------------------------------------------------------------------------
                
                ## setting working directory to the specified argument directory
                setwd(directory)
                
                ## creating empty numeric vectors for id and number of complete obs (nobs)
                ids <- numeric()
                nobs <- numeric()
                
                ## Loop through files corresponding to input ids and calculate nobs for each
                ## file and append it nobs and also append id to id
                
                for (k in seq_along(id)) {## converts id to string to be used in the path
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
                        
                        ## read csv file
                        
                        x <- read.csv(path2file,header=T)
                        completeObs <- nrow(x[complete.cases(x),])
                        ids <- c(ids,id[k])
                        nobs <- c(nobs,completeObs)
                        
                }
                data.frame(ids,nobs)   ## Creating a data frame with ids and nobs
        }
        
        ##------------------------------------------------------------------------
        ## Get data frame of file ids and complete observations
        ##------------------------------------------------------------------------
        
        
        ##etwd(directory) ## Set working directory to the argument directory
        
        
        List <- complete(directory,1:length(dir())) ## Call function complete.R to get data frame of ids and complete observations
        targetList <- subset(List,nobs >= threshold) ## subset those ids that meet the threshold criteria
        
        ## create empty numeric vector for result variable corr
        corr <- numeric()
        
        ## Loop through each file corresponding to "ids" in targetList and compute correlation between sulphate and nitrate
        id = targetList$ids
        
        for (k in seq_along(id)){
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
                pollutant = c("sulfate","nitrate")
                Requiredcol2read <- c(which(header==pollutant[1]),which(header==pollutant[2])) ## Get column to read, this way we are not reading the entire file into memory, saves space
                
                ## read only the required column from file :- this way it saves memory
                
                data <- read.csv(path2file,header=T)[,Requiredcol2read]
                
                ## subset complete cases from data
                
                targetData <- data[complete.cases(data),]
                
                ## compute correlation
                correlation <- cor(targetData[,1],targetData[,2])
                corr = c(corr, correlation) ## collect correaltions in the vector corr
                
        }
        corr
}