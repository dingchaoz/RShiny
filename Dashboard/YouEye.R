dashboardPage(
        dashboard(title = "Diagnostics Capability Data Analysis"),
        dashboardSidebar(
                # Select Product from drop down
                
                selectInput(inputId = "Program",label = "Choose the Program", choices = PrgMap$Programs,selected = PrgMap$Programs[[1]]),
                
                # Make available choice of trucks
                selectInput(inputId = "TrucksGrp", label = "Choose Trucks group",choices = as.character(trucks$Family),multiple = T,selected = as.character(trucks$Family[1])),
                
                # Make available choice of Diagnostics
                
                selectInput(inputId = "Diag", label = "Choose Diagnostic of interest here",choices = as.character(DiagList$Name)),
                
                # make available choice of trucks 
                selectInput(inputId = "Trucks",label = "Choose trucks here", choices = as.character(trucks$TruckName),multiple = T),
                
                # make software choice available
                textInput(inputId = "FrmCal",label = "Software Version from"),
                textInput(inputId = "ToCal",label = "to Software Version"),
                
                # make Date Range choice available
                dateRangeInput(inputId = "DateRange",label = "Choose Date Range",start = "2012-01-01",end = "2017-01-01"),
                
                # make IUPR checkbox available
                checkboxInput(inputId = "IUPRInf", label = "Show IUPR information"),
                
                # action Button
                actionButton(inputId = "Update", label = "Update"),
                
                
                width = 3,
                
                
                sidebarMenu(
                        menuItem("Dashboard", tabname = "dashboard"),
                        menuItem("RYG", tabname = "RYG")
                )    
                
                
        
        ),
        dashboardBody(
                tabItems(
                        tabItem("dashboard",
                                fluidRow(column(12,"",
                                                fluidRow(
                                                        column(8,
                                                               plotOutput("Tplot")),
                                                        column(width = 4,
                                                               plotOutput("hist")))), fluidRow(
                                                                       column(6,plotOutput("Splot")),
                                                                       column(width =6,plotOutput("IUPR"))
                                                               ), fluidRow(
                                                                       column(6,plotOutput("Dplot")),
                                                                       column(width =6, plotOutput("Nplot"))
                                                               )
                                         
                                ))
                )
        )
        
)