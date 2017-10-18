#Felipe Montes 20115 02 09
#XML Parsing 
#Taken from Nolan, Deborah, and Duncan Temple Lang. 2013. XML and Web Technologies for Data Sciences with R. 2014 edition. New York: Springer
#This version works on the LionX linus cluster and is capable of handling large ammount of data

setwd("/gpfs/home/frm10/scratch")


#Install necesary packages

#install.packages("XML")


#Call to use the installed packages

library("XML")

#First, we get the set of nodes that have all the Hydroterre forcing records. This is done by using the Parsed Forcing1 xml in R, getting to the rot node and extracting all the childs of the node that have the information

#Parsing the hydroterre XML document 
Forcing1<-xmlParse("C:/Felipe/XML/XML and Web Technologies for Data Sciences with R/HydroTerre_ETV_Data_month/HT_Forcing.xml")


#ForcingNodes<-getNodeSet(Forcing1,"//Forcing_Record")

#Then we get a list of the HUC12 ID included in the Hydroterre file, and we get it by getting the values from the HUC_ID_List nodeand convertting them into a R List object  

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

write.csv(Forcing.Data.Value,file="ForcingData.csv")





