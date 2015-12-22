# Grab the diagnostic to be evaluated and the Red criteria
# For each parameter; Query data & Calculate Ppk for each parameter and report the min Ppk & max number for failures
# Also Evaluate Red / Green
library(xlsx)
Diagnostics <- read.xlsx("FCA_RYG.xlsx",1)
# eval(parse(text = paste(length(Diagnostics$SEID),Diagnostics$Red[4])))
for(i in 1:1){
        browser()
       SQL_Query <- CapQuery(program='Seahawk',SEID=Diagnostics$SEID[i],ExtID = Diagnostics$ExtID[i],Cap_Param = Diagnostics$Parameter[i],truckGroup = 'Dragnet_PU_17')
}
