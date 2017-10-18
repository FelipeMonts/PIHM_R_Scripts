##############################Program to plot the messh files in PIHM
##############################Based on the Book SpatialData analyisis in R
####    Preeeliminaries



########### Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/R_Library/library")


########### Call the library packages needed for the program to work #############



library(rgdal);

library(sp);
   
library(RColorBrewer) ;
   
library(lattice) ;
   
library(ggplot2)  ;
   
library(rgeos)   ;
   
library(tmap) ;
  
library(dplyr)  ;

library(tidyr)  ;
   

########### Read infromation about the shape files ###########

WE38.mesh.info<-ogrInfo("./input/ShapeFiles/MergeVectorLayer000_q25_a50000.shp","MergeVectorLayer000_q25_a50000");

### The number of polygons in the shape file is WE38.mesh.info$nrows

WE38.mesh.info$nrows


##### Read the shape files form the shape files used in preparation of the mesh file #######

WE38.mesh<-readOGR("./input/ShapeFiles/MergeVectorLayer000_q25_a50000.shp","MergeVectorLayer000_q25_a50000" ); 

### The Shape file polygons ID are from 0 to n, therefore would not match the elements ID in PIHM. 
### To change that the polygon Ids can be changed to make them match

for ( i  in 1:WE38.mesh.info$nrows) {
     
     WE38.mesh@polygons[[i]]@ID<-as.character(i)
}
###### Similarly for the river shape File

WE38.river.info<-ogrInfo("./input/ShapeFiles/Stream12000_sln30_xln_Decomp_Decomp_Decomp.shp","Stream12000_sln30_xln_Decomp_Decomp_Decomp" )    ;


WE38.river.info$nrows

WE38.river<-readOGR("./input/ShapeFiles/Stream12000_sln30_xln_Decomp_Decomp_Decomp.shp","Stream12000_sln30_xln_Decomp_Decomp_Decomp" );


for ( i  in 1:WE38.river.info$nrows) {
     
     WE38.river@lines[[i]]@ID<-as.character(i)
}



##### plot the shape File

plot(WE38.mesh, col="light green",lwd=2);
lines(WE38.river,col="BLUE",lwd=3) ;
title("WE38 PIHM MESH") ;


####### read the dataframe with the attributes collected from the ground water output obtaqined from pihm

########## Identify the output runs that are in the output file

PIHM.runs<-list.files(path=".\\output")   ;

length(PIHM.runs) ;

print(PIHM.runs)  ;


##########    Read the ground water table data ######### 

WE38.GW<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.GW.txt"),header=F,as.is=T) ;


# convert the time stamp to posixct 

DateTime<-as.POSIXct(WE38.GW[,1],format="%Y-%m-%d %H:%M", tz="GMT") ;

#### Select a time Range that it is interesting to explore

DateTime.range<-c(as.POSIXct("2008-03-01"),as.POSIXct("2008-03-15",tz="GMT")); #Based on a specific range of data that one might want to explore


Rows.Time.range<-which(DateTime >= DateTime.range[1] & DateTime <= DateTime.range[2] ); 
Columns.Time.range<-which(DateTime >= DateTime.range[1] & DateTime <= DateTime.range[2] ); 

# Only select columns 2 (the first is the TS) to 242+1 (the number of mesh elemnts), the rest is the channel ground water storage (32 elements)
  
WE38.GW.Land<-WE38.GW[Rows.Time.range,2:243] ;
   
WE38.GW.River<-WE38.GW[Rows.Time.range,seq(244, dim(WE38.GW)[2])] ;
   
   


# Find the largest and smaller values in the data set for the land segments
   
GW.Land.range<-range(WE38.GW.Land) ;

   
# plot and explore the ground water time series   
plot(DateTime[Rows.Time.range],WE38.GW.Land[,1],type="l",ylim=GW.Land.range);

for (i in seq(2,dim(WE38.GW.Land)[2])) {
  points(DateTime[Rows.Time.range],WE38.GW.Land[,i],type='l',col=i ) ;
} ;
   

# Find the largest and smaller values in the data set for the land segments
   
GW.River.range<-range(WE38.GW.River) ;
   
   
# plot ot explore the ground water time series   
   
plot(DateTime,WE38.GW.River[,1],type="l",ylim=GW.River.range);
   
   for (i in seq(2,dim(WE38.GW.River)[2])) {
     points(DateTime,WE38.GW.River[,i],type='l',col=i ) ;
   }
   

####### attach the GW data to the shape file as attibutes
   

GW.TimeSeries<-as.data.frame(t(WE38.GW.Land))

row.names(GW.TimeSeries)<-seq(1,dim(GW.TimeSeries)[1]) ;


WE38.GW.spdf<-SpatialPolygonsDataFrame(WE38.mesh,GW.TimeSeries, match.ID = T);

###### Name the columns of the attribute data frame according to the date range

names(WE38.GW.spdf@data)<-c(as.character(DateTime[Rows.Time.range]),"polygonID")

DateTime[Rows.Time.range]


###### Get the Polygons IDs and labeling coordinates

WE38.ID<-sapply(slot(WE38.GW.spdf,"polygons"),function(x) slot(x,"ID")) ;
WE38.labpt<-sapply(slot(WE38.GW.spdf,"polygons"),function(x) slot(x,"labpt")) ;



# #  Plot the Spatial polygonDataFrame with the Lattice package
# 
# plot(WE38.GW.spdf);
# text(coordinates(t(WE38.labpt)),WE38.ID);
# 
# Colors.in.map<-brewer.pal(12,"Set3")
# spplot(WE38.GW.spdf,Columns,main="WE38 Soil Water");




#################### using the Package tmap to plot the spatial data

##################### creating a color pallete that shows more than 12 colors

color.palette<- colorRampPalette(c("yellow","green", "blue"), space="rgb");
pie(rep(1,40),col=color.palette (40) )  ;


#####   Plotting the map

####GEt the Polygon ID 
WE38.GW.spdf$polygonID<-sapply(slot(WE38.GW.spdf,"polygons"),function(x) slot(x,"ID")) ;

#### Plotting the polygon shape file using the tmap

qtm(shp=WE38.GW.spdf, fill="2008-03-04", fill.palette=colorRampPalette(c("yellow","green", "blue"), space="rgb")(5),text="polygonID"); 

WE38.river$LineID<-sapply(slot(WE38.river,"lines"),function(x) slot(x,"ID"))  ;

tm_shape(WE38.GW.spdf) +
     tm_fill("2008-03-04", palette=colorRampPalette(c("yellow","green", "blue"), space="rgb")(11), n=10 )+ 
#     tm_text("polygonID")
tm_shape(WE38.river) + ##########Plot the river and other lines
     tm_lines(col="blue",lwd=3)
#     tm_text("LineID")
     

############## Plotting a series of maps for a rain storm simulation  ###########

i<-as.character(DateTime[Rows.Time.range]) [14]
     
      tm_shape(WE38.GW.spdf)+
      tm_borders(col = "Black", lwd = 1, lty = "solid", alpha = NA) +   
      tm_fill(i, palette=colorRampPalette(c("lightblue", "blue","darkblue"), space="rgb")(9))+ 
#     tm_text("polygonID")
     tm_shape(WE38.river) + ##########Plot the river and other lines
     tm_lines(col="blue",lwd=3)
#     tm_text("LineID")






#################  Plotting  surface flow to stream from left terrain .rivFlx2   #################################

##########    Read the .rivFlx2  data ######### 

WE38.rivFlx2<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx2.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx2)

# Find the largest and smaller values in the data set 
   
WE38.rivFlx2.range<-range(WE38.rivFlx2[,seq(2,dim(WE38.rivFlx2)[2])]) ;


# plot and explore .rivFlx2  time series   
plot(DateTime,WE38.rivFlx2[,2],type="l",ylim=WE38.rivFlx2.range);

for (i in seq(3,dim(WE38.rivFlx2)[2])) {
  points(DateTime,WE38.rivFlx2[,i],type='l',col=i ) ;
} ;
   

##########    Read the .rivFlx4  data ######### 

WE38.rivFlx4<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx4.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx4)

# Find the largest and smaller values in the data set 
   
WE38.rivFlx4.range<-range(WE38.rivFlx4[,seq(2,dim(WE38.rivFlx4)[2])]) ;


# plot and explore .rivFlx4  time series   
plot(DateTime,WE38.rivFlx4[,2],type="l",ylim=WE38.rivFlx4.range);

for (i in seq(3,dim(WE38.rivFlx4)[2])) {
  points(DateTime,WE38.rivFlx4[,i],type='l',col=i ) ;
} ;
   


##########    Read the .rivFlx6  data ######### 

WE38.rivFlx6<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx6.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx6)

# Find the largest and smaller values in the data set 
   
WE38.rivFlx6.range<-range(WE38.rivFlx6[,seq(2,dim(WE38.rivFlx6)[2])]) ;


# plot and explore .rivFlx6  time series   
plot(DateTime,WE38.rivFlx6[,2],type="l",ylim=WE38.rivFlx6.range);

for (i in seq(3,dim(WE38.rivFlx6)[2])) {
  points(DateTime,WE38.rivFlx6[,i],type='l',col=i ) ;
} ;
   


##########    Read the .rivFlx7  data ######### 

WE38.rivFlx7<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx7.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx7)

# Find the largest and smaller values in the data set 
   
WE38.rivFlx7.range<-range(WE38.rivFlx7[,seq(2,dim(WE38.rivFlx7)[2])]) ;


# plot and explore .rivFlx7  time series   
plot(DateTime,WE38.rivFlx7[,2],type="l",ylim=WE38.rivFlx7.range);

for (i in seq(3,dim(WE38.rivFlx7)[2])) {
  points(DateTime,WE38.rivFlx7[,i],type='l',col=i ) ;
} ;
   

