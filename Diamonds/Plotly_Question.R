library(ggplot2)
library(plotly)

# plot a box plot using ggplot2

h <- ggplot(data = diamonds,aes(x = cut, y= price, color = cut)) + geom_boxplot(outlier.colour = "white")+  geom_jitter(position = position_jitter(0.1,0))+  coord_flip()+ theme_bw()+ theme(legend.position = "none") + theme(axis.title.y = element_blank())
# add a line to the plot
h1 <- h + geom_hline(aes(yintercept=20000), color="red", linetype="dashed")

# print h

print(h1)

# convert the "h1" into plotly

ggplotly(p = h1)

# unsuccessful in converting the plot to plotly .... 
# > ggplotly(p = h1)
#   Error in col2rgb(x) : invalid color name 'Fair'
# Why does the category "Fair" comes up as color?

I <- gg2list(p = h1)
#Error in col2rgb(x) : invalid color name 'Fair'


