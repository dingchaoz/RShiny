Cour# Data table - good for subsetting and summarizing
library(data.table)
DF = data.frame(a=rnorm(9,0,1), y=rep(c("a","b","c"),each=3),z = rnorm(9,2,1))
DT = data.table(a=rnorm(9,0,1), y=rep(c("a","b","c"),each=3),z = rnorm(9,2,1))

library(RODBC)
#ch <- dbConnect(C:/Windows/ODBC.INI)
#dbhandle <- odbcDriverConnect('driver={SQL Server};server= W4-S129433\\CAPABILITYDB,1443;database=HDPacific;trusted_connection=true')
#res <- sqlQuery(dbhandle, 'select * from information_schema.tables')
#ibrary(RSQLServer)
#m<-dbDriver("RODBC")

library(RSQLServer)
conn <-odbcConnect("Capability") # Capability is the ODBC name.
# To remove rm(list=ls())
trucks <- sqlQuery(conn,'Select * FROM Seahawk.dbo.tblTrucks')