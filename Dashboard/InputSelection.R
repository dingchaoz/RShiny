InputSelection <- function(){
        selectInput(inputId = "Program",label = "Choose the Program", choices = PrgMap$Programs,selected = PrgMap$Programs[[1]])
        
        # Make available choice of trucks
        selectInput(inputId = "Trucks", label = "Choose Trucks of interest here",choices = as.character(def_trk$TruckName),multiple = T,selected = as.character(def_trk$TruckName[1]))
        
        # Make available choice of Diagnostics
        
        selectInput(inputId = "Diag", label = "Choose Diagnostic of interest here",choices = as.character(DiagList$Name))
}