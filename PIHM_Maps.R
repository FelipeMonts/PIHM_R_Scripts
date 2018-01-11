##############################Program to plot the messh files in PIHM
##############################Based on the Book SpatialData analyisis in R
####    Preeeliminaries



########### Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/Sotware&Coding/R_Library/library")  ;


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

# HYDC.info<-ogrInfo("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\HydroTerreFullManhantango\\HansYostDeepCreek\\Aug2920171550\\3DomainDecomposition\\MergeVectorLayer000.shp");
### The number of polygons in the shape file is WE38.mesh.info$nrows

##### Read the shape files form the shape files used in preparation of the mesh file #######

# HYDC<-readOGR("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\HydroTerreFullManhantango\\HansYostDeepCreek\\Aug2920171550\\3DomainDecomposition\\MergeVectorLayer000.shp" ); 







# Read information about the shape file

HYDC.info<-ogrInfo("C:/Users/frm10/Downloads/HansYoust/HYDC.shp") ;



HYDC.info$nrows



# Read  the shape file

HYDC<-readOGR(dsn="C:/Users/frm10/Downloads/HansYoust",layer="HYDC") ;


str(HYDC, max.level = 2)  ;


# get the ID's of the line segments in the shape file read

sapply(slot(HYDC,"lines"), function(x) slot(x,"ID"))  


#get the coordinates of the first line segment of the shape file read

coordinates(HYDC)[[1]]

# get the coordinates of all the line segments of the shape file read in a list form


lapply(coordinates(HYDC),function(x) x[[1]])

# get the coordinates of all the line segments of the shape file read in a matrix form

sapply(coordinates(HYDC),function(x) x[[1]])


sapply(slot(HYDC,"lines"),function(x) slot(x,"ID"))







Point.coords.matrix<-matrix(data=sapply(coordinates(HYDC),function(x) x[[1]]), nrow = HYDC.info$nrows, ncol = 4, byrow=T) ;

Point.coords.df<-data.frame(Point.coords.matrix[,c(1,3)],Point.coords.matrix[,c(2,4)]);
names(Point.coords.df)<-c('P1.X', 'P1.Y', 'P2.X', 'P2.Y')

Point.coords.df$Line<-sapply(slot(HYDC,"lines"), function(x) slot(x,"ID")) ;





Stacked.Point.coords<-rbind(Point.coords.matrix[,c(1,3)],Point.coords.matrix[,c(2,4)])   ;


str(Stacked.Point.coords) ;
  
  
Unique.Point.coords<-data.frame(unique(Stacked.Point.coords))  ;

names(Unique.Point.coords)<-c("X" , "Y");

Unique.Point.coords$Point.ID<-seq(1:dim(Unique.Point.coords)[1]) ;


str(Unique.Point.coords)  ;



A<-merge(Point.coords.df[,c("P1.X", "P1.Y" ,  "Line")], Unique.Point.coords, by.x=c('P1.X', 'P1.Y'), by.y=c("X" , "Y"), all.x=T)

B<-merge(Point.coords.df[,c("P2.X", "P2.Y" ,  "Line")], Unique.Point.coords, by.x=c('P2.X', 'P2.Y'), by.y=c("X" , "Y"), all.x=T)



merge(A,B,by="Line")




#### read the shape files with all the nodes and coordinates 

HYDC.nodes<-readOGR("C:/Users/frm10/Downloads/HansYoust/HYDC_nodes_Geometry.shp")  ;

HYDC.nodes@data

From.points<-HYDCLines[,c(1,3)]   ;

To.points<-HYDCLines[,c(2,4)]  ;

From<-merge(HYDC.nodes@data,From.points, by.x=c("xcoord","ycoord"), by.y=c(1,2), all.x=T, sort=F) ;

head(From) ;

To<-merge(HYDC.nodes@data,To.points, by.x=c("xcoord","ycoord"), by.y=c(1,2), all.x=T, sort=F) ;

head(To) ;


# .node files
# 
# First line: <# of vertices> <dimension (must be 2)> <# of attributes> <# of boundary markers (0 or 1)>
#   Remaining lines: <vertex #> <x> <y> [attributes] [boundary marker] 
# 
# Blank lines and comments prefixed by `#' may be placed anywhere. Vertices must be numbered consecutively, starting from one or zero.
# 
# The attributes, which are typically floating-point values of physical quantities (such as mass or conductivity) associated with the nodes of a finite element mesh, are copied unchanged to the output mesh. If -q, -a, -u, or -s is selected, each new Steiner point added to the mesh will have quantities assigned to it by linear interpolation.
# 
# If the fourth entry of the first line is `1', the last column of the remainder of the file is assumed to contain boundary markers. Boundary markers are used to identify boundary vertices and vertices resting on PSLG segments. The .node files produced by Triangle contain boundary markers in the last column unless they are suppressed by the -B switch. 
# 

# .poly files
# 
# First line: <# of vertices> <dimension (must be 2)> <# of attributes> <# of boundary markers (0 or 1)>
#   Following lines: <vertex #> <x> <y> [attributes] [boundary marker]
# One line: <# of segments> <# of boundary markers (0 or 1)>
#   Following lines: <segment #> <endpoint> <endpoint> [boundary marker]
# One line: <# of holes>
#   Following lines: <hole #> <x> <y>
# Optional line: <# of regional attributes and/or area constraints>
#   Optional following lines: <region #> <x> <y> <attribute> <maximum area> 
# 
# A .poly file represents a PSLG, as well as some additional information. PSLG stands for Planar Straight Line Graph, a term familiar to computational geometers. By definition, a PSLG is just a list of vertices and segments. A .poly file can also contain information about holes and concavities, as well as regional attributes and constraints on the areas of triangles.
# 
# The first section lists all the vertices, and is identical to the format of .node files. <# of vertices> may be set to zero to indicate that the vertices are listed in a separate .node file; .poly files produced by Triangle always have this format. A vertex set represented this way has the advantage that it may easily be triangulated with or without segments (depending on whether the .poly or .node file is read).
#   
#   The second section lists the segments. Segments are edges whose presence in the triangulation is enforced (although each segment may be subdivided into smaller edges). Each segment is specified by listing the indices of its two endpoints. This means that you must include its endpoints in the vertex list. Each segment, like each vertex, may have a boundary marker.
# 
# If -q, -a, -u, and -s are not selected, Triangle will produce a constrained Delaunay triangulation (CDT), in which each segment appears as a single edge in the triangulation. If -q, -a, -u, or -s is selected, Triangle will produce a conforming constrained Delaunay triangulation, in which segments may be subdivided into smaller edges. If -D is selected as well, Triangle will produce a conforming Delaunay triangulation, so every triangle is Delaunay, and not just constrained Delaunay.
# 
# The third section lists holes (and concavities, if -c is selected) in the triangulation. Holes are specified by identifying a point inside each hole. After the triangulation is formed, Triangle creates holes by eating triangles, spreading out from each hole point until its progress is blocked by PSLG segments; you must be careful to enclose each hole in segments, or your whole triangulation might be eaten away. If the two triangles abutting a segment are eaten, the segment itself is also eaten. Do not place a hole directly on a segment; if you do, Triangle will choose one side of the segment arbitrarily.
# 
# The optional fourth section lists regional attributes (to be assigned to all triangles in a region) and regional constraints on the maximum triangle area. Triangle will read this section only if the -A switch is used or the -a switch is used without a number following it, and the -r switch is not used. Regional attributes and area constraints are propagated in the same manner as holes; you specify a point for each attribute and/or constraint, and the attribute and/or constraint will affect the whole region (bounded by segments) containing the point. If two values are written on a line after the x and y coordinates, the first such value is assumed to be a regional attribute (but will only be applied if the -A switch is selected), and the second value is assumed to be a regional area constraint (but will only be applied if the -a switch is selected). You may specify just one value after the coordinates, which can serve as both an attribute and an area constraint, depending on the choice of switches. If you are using the -A and -a switches simultaneously and wish to assign an attribute to some region without imposing an area constraint, use a negative maximum area.
# 
# Blank lines and comments prefixed by `#' may be placed anywhere. Vertices, segments, holes, and regions must be numbered consecutively, starting from one or zero. (The choice to begin the numbering from one or zero must be consistent across all objects.)
# 
# When a triangulation is created from a .poly file, you must either enclose the entire region to be triangulated in PSLG segments, or use the -c switch, which encloses the convex hull of the input vertex set. If you do not use the -c switch, Triangle will eat all triangles that are not enclosed by segments; if you are not careful, your whole triangulation may be eaten away. If you do use the -c switch, you can still produce concavities by the appropriate placement of holes just within the convex hull.
# 
# An ideal PSLG has no intersecting segments, nor any vertices that lie upon segments (except, of course, the endpoints of each segment.) You aren't required to make your .poly files ideal, but you should be aware of what can go wrong. Segment intersections are relatively safe - Triangle will calculate the intersection points for you and add them to the triangulation - as long as your machine's floating-point precision doesn't become a problem. You are tempting the fates if you have three segments that cross at the same location, and expect Triangle to figure out where the intersection point is. Thanks to floating-point roundoff error, Triangle will probably decide that the three segments intersect at three different points, and you will find a minuscule triangle in your output - unless Triangle tries to refine the tiny triangle, uses up the last bit of machine precision, and fails to terminate at all. You're better off putting the intersection point in the input files, and manually breaking up each segment into two. Similarly, if you place a vertex at the middle of a segment, and hope that Triangle will break up the segment at that vertex, you might get lucky. On the other hand, Triangle might decide that the vertex doesn't lie precisely on the line, and you'll have a needle-sharp triangle in your output - or a lot of tiny triangles if you're generating a quality mesh.
# 
# When Triangle reads a .poly file, it also writes a .poly file, which includes all edges that are subsegments of input segments. If the -c switch is used, the output .poly file will also include all of the edges on the convex hull. Hence, the output .poly file is useful for finding edges associated with input segments and setting boundary conditions in finite element simulations. More importantly, you will need it if you plan to refine the output mesh, and don't want segments to be missing in later triangulations. 

# .area files
# 
# First line: <# of triangles>
#   Following lines: <triangle #> <maximum area> 
# 
# An .area file associates with each triangle a maximum area that is used for mesh refinement. As with other file formats, every triangle must be represented, and they must be numbered consecutively (from one or zero). Blank lines and comments prefixed by `#' may be placed anywhere. A triangle may be left unconstrained by assigning it a negative maximum area. 
# 




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
   

