#  Lines 1 & 2 did not work
# Data <- scan(file = "play.txt",what = character(), sep = " ")
# table <- read.table(file = "play.txt", sep = " ")
data <- readLines("play.txt")
table <- read.csv("play.txt",header = F, sep = " ")
# Below is a function that replaces leading and trailing white spaces with ""
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
subcomma <- function (x) gsub("^\\s+|\\s+$", ",", x)
#  The below lines delimits lines 374/5 by whitespaces
line374 <- strsplit(data[374],split="^\\s+")
line375 <- strsplit(data[375],split="^\\s+") 
#  The below line attempts to convert line375 into a vector of strings of length > 1
result <- toString(line375)
header <- line374[[1]][2]
variable<-toString(strsplit(test,split = "^\\s+"))
# variable equals "Tue Oct 13,'15 01:14   Ventura  81000772   B.Menon     67967       212     np809"
#  attempted to split string by a "," anf then reattach later

x <- strsplit(variable,split = ",")
# unlist(strsplit("a b c", split=" "))
