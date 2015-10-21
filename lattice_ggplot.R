# Lattice plotting
library(lattice)
library(ggplot2)
library(datasets)
airquality<-transform(airquality,Month=factor(Month))
xyplot(Ozone~Wind,airquality,main="Ozone Vs Wind")
# Auto labeling which is not done in base plotting

xyplot(Ozone~Wind|Month,layout=c(3,2),airquality,main="Ozone Vs Wind") # Auto labeling which is not done in base plotting
stripplot(Ozone~Month,jitter.data=T,factor=0.5,data = airquality)
# Coloring by groups is a pain in the neck http://stackoverflow.com/questions/22920381/different-coloring-of-groups-in-r-plot

# try to solve the above pain using ggplot2
qplot(Month,Ozone,data=airquality,color = Month,geom="jitter")
# The jitter seems awful

qplot(Month,Ozone,data=airquality,color=Month,position = position_jitter(0.1,0.1), main = "Ozonw Vs Month") + theme_bw()
# theme_bw() was to set the background to white

qplot(Ozone,data=airquality,fill=Month) + theme_bw()
airquality <- na.omit(airquality)
means <- aggregate(airquality$Ozone,list(airquality$Month),mean)
qplot(Solar.R,Ozone,data = airquality,facets = .~Month,color = Month,geom_hline(yintercept = means$x)) + theme_bw()
# Look the above line of code doesn't give you the line intended within each facet
qplot(Solar.R,Ozone,data = airquality,facets = .~Month,color = Month,geom_abline(a=c(50,60,70,80,90))) + theme_bw()
## No help in the above line of code

g <- ggplot(aes(x=Solar.R,y=Ozone), data = airquality) + geom_point(aes(color = Month)) + facet_grid(.~Month)
hline.data <- data.frame(z=c(50,75,100,125,150),month=unique(airquality$Month))
g + geom_hline(aes(z))
