## Assignment week 2 Part 2

##Goal : Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
## The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases

## Author : Srivathsan Seshadri, '2015-05-17'

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