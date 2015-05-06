# reading xml into R 

# applying "XML" package
library(XML)

fileUrl<-"http://www.w3schools.com/xml/cd_catalog.xml"
(doc<-xmlTreeParse(fileUrl,useInternal=T))

# check the xml file
rootNode<-xmlRoot(doc)
xmlName(rootNode) # root element name
length(names(rootNode))

rootNode[[1]]
rootNode[[1]][[1]]

# using XPath to extract values
xmlSApply(rootNode,xmlValue)

TITLE<-xpathSApply(rootNode,"//TITLE",xmlValue)
ARTIST<-xpathSApply(rootNode,"//ARTIST",xmlValue)
COUNTRY<-xpathSApply(rootNode,"//COUNTRY",xmlValue)
COMPANY<-xpathSApply(rootNode,"//COMPANY",xmlValue)
PRICE<-xpathSApply(rootNode,"//PRICE",xmlValue)
YEAR<-xpathSApply(rootNode,"//YEAR",xmlValue)

# merge into a new data frame
CD_catalog<-data.frame(TITLE, ARTIST, COUNTRY, COMPANY, PRICE, YEAR)
dim(CD_catalog)
CD_catalog

#export the tidy data into a csv file
write(CD_catalog,"CD_catalog.csv")