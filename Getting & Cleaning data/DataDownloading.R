# Week 1 of Getting & Cleaning Data
# checking if a directory named "data" exists in the working directory...if not create a new directory

if(!file.exists("data")){
        dir.create("data")
}

file.exists("data")

# downloading csv data from https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD&bom=true

fileurl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD&bom=true"
download.file(fileurl,"./data/camera.csv")
# what's the download date ? below lines set that!
dateDownloaded <- date()
Cameradata<-read.table("./data/camera.csv",sep=",",header = T)
head(Cameradata)
xlfileurl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(xlfileurl,"./data/Data.xlsx")
library(XLConnect)
wb <- loadWorkbook("./data/Data.xlsx",create = TRUE)
Cameradataxl = readWorksheet("./data/Data.xlsx",sheet=1,header = T)
library(xlsx)
Cameradata <- read.xlsx("./data/Data.xlsx",sheetIndex = 1,header = T)
