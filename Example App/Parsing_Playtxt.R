# Import library to use rm_white function
library(qdapRegex)

# Read the whole data set line by line into variable data
rawdata <- readLines("play.txt")

# Remove all the blank rows in the raw data
rawdata <- rawdata[sapply(rawdata, nchar) > 0]

# A list contain the transformed data as the output
data <- list()

# A list contain the transformed data as the output
exploded_data <- list()

# An array holding the week days
days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

#  Create a data frame containig all individual tables with header and component
for(i in 1 : length(rawdata)) {
  
  
  # Replace 
  line <- strsplit(rawdata[i],split="^\\s+")
  # If there is a blank space in the starting trailing space, then it is a row of title
  if (line[[1]][1] == "") {
    
    line <- line[[1]][2] # Get the substance of the header
    line <- rm_white(line) # Remove all extra white space but only leave one space between word in the line
    header_components <- strsplit(line," ")[[1]] # Convert the string of header to a vector holding individual components
    header <- tapply(header_components,seq(1:length(header_components)),paste) # Create an array to hold individual components of header
    
  }
  else{
    
    line <- line[[1]][1] # Get the substance of the parts
    line <- rm_white(line) # Remove all extra white space but only leave one space between word in the line
    line <- gsub(","," ",line) # Replace the , with blank space
    line <- gsub("'","",line) # Remove the '
    parts_components <- strsplit(line," ")[[1]] # Convert the string of parts to a vector holding individual components
    parts <- tapply(parts_components,seq(1:length(parts_components)),paste)
    # Watch out for the memory difference between parts and parts_components
    # If the lengths of parts equal to the length of components, then it is part of the table
    if (parts[1] %in% days) {
      
      
      # Combine the 1st, 2nd, 3rd components to form one string for Date
      parts[1] <- paste(parts[1],parts[2], parts[3], parts[4],sep = " ")
      # Remove the 2nd, 3rd components which are already merged into the 1st component Date
      parts <- parts[-c(2,3,4)]
      
      # Create a data frame to hold each test record title and values
      df <- data.frame()
      # Add the values into the created data frame
      df <- rbind(df, parts)
      # Add header to the data frame
      names(df) <- header
      
      # Insert the data frame into the data array
      data[[i]] <- df
       
    }
    
    ## If not, add it as a comment
    else {
      
      df <- cbind(df, Comment = line)
      
      # Insert the data frame into the data array
      data[[i]] <- df
      
      
    }

  }  
  
}

cm <- ""
creat_Masterheader <- function(x,cm) {
  
  
  # Replace 
  line <- strsplit(x,split="^\\s+")
  # If there is a blank space in the starting trailing space, then it is a row of title
  if (line[[1]][1] == "") {
    
    line <- line[[1]][2] # Get the substance of the header
    line <- rm_white(line) # Remove all extra white space but only leave one space between word in the line
    header <- strsplit(line," ")[[1]] # Convert the string of header to a vector holding individual components
    
    
    # Append master_header with the column names in header but not in master_header yet
    cm = c(union(cm, header))
    
    
  
  }
  return (cm)
  
  
}


#  Create a data frame containig all data records into one table 
for(i in 1 : length(rawdata)) {
  
  
  # Replace 
  line <- strsplit(rawdata[i],split="^\\s+")
  # If there is a blank space in the starting trailing space, then it is a row of title
  if (line[[1]][1] == "") {
    
    line <- line[[1]][2] # Get the substance of the header
    line <- rm_white(line) # Remove all extra white space but only leave one space between word in the line
    header <- strsplit(line," ")[[1]] # Convert the string of header to a vector holding individual components
    
    if (i == 1) {  # Create a master header 
      
      master_header <- header
      
    }
    
    # Append master_header with the column names in header but not in master_header yet
    #master_header = c(master_header,header[-(header %in% master_header)])
    #master_header = paste(union(master_header, header))
    master_header = c(union(master_header, header))
    
    
  }
  else{
    
    line <- line[[1]][1] # Get the substance of the parts
    line <- rm_white(line) # Remove all extra white space but only leave one space between word in the line
    line <- gsub(","," ",line) # Replace the , with blank space
    line <- gsub("'","",line) # Remove the '
    parts <- strsplit(line," ")[[1]] # Convert the string of parts to a vector holding individual components
    #parts <- tapply(parts_components,seq(1:length(parts_components)),paste)
    # Watch out for the memory difference between parts and parts_components
    # If the lengths of parts equal to the length of components, then it is part of the table
    if (parts[1] %in% days) {
      
      
      # Combine the 1st, 2nd, 3rd components to form one string for Date
      parts[1] <- paste(parts[1],parts[2], parts[3], parts[4],sep = " ")
      # Remove the 2nd, 3rd components which are already merged into the 1st component Date
      parts <- parts[-c(2,3,4)]
      
      # Create a data frame to hold each test record title and values
      df <- data.frame()
      # Add the values into the created data frame
      df <- rbind(df, parts)
      # Add header to the data frame
      names(df) <- header
      
      # Insert the data frame into the data array
      exploded_data[[i]] <- df
      
    }
    
    ## If not, add it as a comment
    else {
      
      df <- cbind(df, Comment = line)
      
      # Insert the data frame into the data array
      exploded_data[[i]] <- df
      
      
    }
    
  }  
  
}








## Export all tables to csv
for (i in 1: length(data)) {
  
  if (!is.null(data[[i]]) ) {
    
    write.table(data[[i]],file = "exportfile_exploded.csv",append = TRUE,sep = ",",quote = F,row.name = F)
    
  }
  
}

## Export all tables to csv
for (i in 1: length(data)) {
  
  if (!is.null(data[[i]]) ) {
      
      write.table(data[[i]],file = "exportfile.csv",append = TRUE,sep = ",",quote = F,row.name = F)
  
  }

}




