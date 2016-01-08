# Read the text file as a  table
#  look for empty line as separator
#  give an ID to each block of data 
#  consolidate the data frame and transpose
library(reshape2)
raw_data <- read.table("data.txt", sep = "\t") # reads data int oa table
Cleaned_Data <- raw_data[!(raw_data$V1=="deg"|raw_data$V1=="Crank Angle"|raw_data$V1==" "),]
