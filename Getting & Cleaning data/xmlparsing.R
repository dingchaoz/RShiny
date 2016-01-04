# This is week #1 of getting and cleaning data for reading xml file
library(XML) # requires XML package
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternalNodes = TRUE) # Loads the xml file
RootNode = xmlRoot(doc) # compare doc and RootNode; doc appears to have <?xml version="1.0" encoding="UTF-8"?> and RootNode seems to drop <?xml version="1.0" encoding="UTF-8"?>
xmlName(RootNode) # this gives the tag names that exist under the root node of an xml file
RootNode[[1]][[1]] # access every element in a tag

xmlSApply(RootNode,xmlName) # applies the function xmlName to every tag within the RootNode; this gives a string of elements within every "food" tag
xmlSApply(RootNode,xmlValue) # applies the function xmlValue to every tag within the RootNode
Items <- xpathSApply(RootNode,'//name',xmlValue) # unlike the above commands; this allows to get the value of the tag "name" within the Root NOde
price <- xpathSApply(RootNode,'//price',xmlValue)
Menu<-list(itms = Items,prz = price) # trying to save the items and price in a list for the heck of it

fileurl2 <- 'http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens'
doc2 <- htmlTreeParse(fileurl2,useInternalNodes = TRUE)
RootNode2 <- xmlRoot(doc2)
xmlName(RootNode2)
names(RootNode2)
Team <- xpathSApply(doc2,"//td[@class='right']",xmlValue)
Team2 <- xpathSApply(doc2,"//a[@href]",xmlValue,"http://insider.espn.go.com/nfl/draft/player/_/id/46858")

# Example from http://www.stat.berkeley.edu/~statcur/Workshop2/Presentations/XML.pdf
# http://www.ncbi.nlm.nih.gov/books/NBK3828/#publisherhelp.Example_of_a_Standard_XML

url <- "http://www.zillow.com/homes/Greenwood-IN-46143_rb/"
document <- htmlTreeParse(url,useInternalNodes = TRUE)
Root <- xmlRoot(document)
xmlName(Root)
names(Root)
StAddress <- xpathSApply(document,"//span[@itemprop='streetAddress']",xmlValue)
ListPrice <- xpathSApply(document,"//dt[@class='price-large zsg-h2 zsg-content_collapsed']",xmlValue)
BedBathSqFt <- xpathSApply(document,"//span[@class='beds-baths-sqft']",xmlValue)