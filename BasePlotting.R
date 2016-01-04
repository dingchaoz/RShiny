# Plot practise
## Base plotting
library(datasets)
par(mfrow=c(1,3),mar=c(4,4,2,1))# par function sets graphical parameters ; mar sets the margins starting from the

boxplot(subset(mtcars$mpg,mtcars$gear==3),col = "blue")
boxplot(subset(mtcars$mpg,mtcars$gear==4),col="red")
boxplot(subset(mtcars$mpg,mtcars$gear==5),col="yellow")
abline(h=15) # drawing a horizontal like */
title(main = "Bof plot of mpg",sub = "test",ylab = "miles per gallon", outer = T)
mtext("overall tiltle here",outer = TRUE)
#dev.copy2pdf(device=dev.cur(),file="firstplot.pdf")
#dev.off() # closes the previous plot device


#frame()
with(airquality,{
  #plot.new() 
  par(mfrow=c(1,1))
  plot(Wind,Ozone,main="Wind vs Ozone")
  with(subset(airquality,Month==5),points(Wind,Ozone,col="red"))
  with(subset(airquality,Month!=5),points(Wind,Ozone,col="blue"))
  legend("topright",pch=1,col=c("red","blue"),legend=c("May","Rest of the year"))
  #dev.copy2pdf(pdf,"firstplot.pdf")
  #dev.off()
})
