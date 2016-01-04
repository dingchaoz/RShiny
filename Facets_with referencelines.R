library(datasets)
library(ggplot2)

str(mtcars)
p <- qplot(mpg,wt,data=mtcars,facets = ~vs )
hline.data <- data.frame(z=c(2,4), vs = unique(mtcars$vs))

p + geom_hline(aes(yintercept = z),hline.data)