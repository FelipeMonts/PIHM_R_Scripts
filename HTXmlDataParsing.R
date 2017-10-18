# Felipe Montes 20115 02 09
# XML Parsing 
# Taken from Nolan, Deborah, and Duncan Temple Lang. 2013. XML and Web Technologies for Data Sciences with R. 2014 edition. New York: Springer

setwd("C:/Felipe/XML/XML and Web Technologies for Data Sciences with R")

# Install necesary packages

# install.packages("XML")


# Call to use the installed packages

library("XML")

#Trial with 1 year of Hydroterre Data reduced to a bear minimum close to 10 data points
#Parsing the hydroterre XML document 
Forcing1<-xmlParse("C:/Felipe/XML/XML and Web Technologies for Data Sciences with R/HydroTerre_ETV_Data_month/HT_Forcing.xml")



#Trial, Example 1-2 Converting XML to and R dataframe
#First we need to bypass the nodes that do not have data and get to the node where the data starts
#xmlRoot() will get the toplevel node and treating the child of the root element as objects of an R list we can get to the "Forcing_Outputs" node and the Forcing_List node inside the "Forcing_Outputs" node were the data is located


#First, we get the set of nodes that have all the Hydroterre forcing records. This is done by using the Parsed Forcing1 xml in R, getting to the rot node and extracting all the childs of the node that have the information

ForcingNodes<-getNodeSet(Forcing1,"//Forcing_Record")

#Then we get a list of the HUC12 ID included in the Hydroterre file

HUC12.IDs<-xpathApply(Forcing1,"/PIHM_Forcing/Forcing_Inputs/HUC_ID_List",xmlValue)




#Using getNodeSet() and PAX a subset of nodes can be Selected. To do that we use the PAX sinthax within R Using the XML package

#First Try to gell all the Index-Start node Sets

Index.Start<-getNodeSet(Forcing1,"//Index_Start")
Index.Start.Value<-xmlToDataFrame(Index.Start)

#Same With Index_End nodes
Index.End<-getNodeSet(Forcing1,"//Index_End")
Index.End.Value<-xmlToDataFrame(Index.End)


Data.nodes<-paste0("//",HUC12.IDs[[1]])

# With the rest of the infromation in the Forcing Record  List
Forcing.Data.Nodes<-getNodeSet(Forcing1,Data.nodes)

Forcing.Data.Value<-xmlToDataFrame(Forcing.Data.Nodes)




