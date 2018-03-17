############################################################################################################################


# Luke, Douglas. 2015. A User's Guide to Network Analysis in R. Use R! Cham: Springer International Publishing. https://doi.org/10.1007/978-3-319-23883-8.
# 
# Using R network packages to manage the river network in PIHM and PIHM GIS
# 
# Felipe Montes 01/23/2018
# 
############################################################################################################################



############################################################################################################################


# Kolaczyk, Eric D., and Gábor Csárdi. 2014. Statistical Analysis of Network Data with R. Vol. 65. Use R! New York, NY: Springer New York. https://doi.org/10.1007/978-1-4939-0983-4.

# https://github.com/kolaczyk/sand/blob/master/sand/inst/code/chapter2.R

# Using R network packages to manage the river network in PIHM and PIHM GIS
# 
# Felipe Montes 01/24/2018


############################################################################################################################

#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

#  Set Working directory


library(devtools)  ;


install.packages("sand") ;
library(sand)
#install_sand_packages() ;

load(paste0('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs\\',Project,'\\MM_PHIMInputsR_V2.RData'));


# From MM_PIHMInputsR.V2.R

g.River.edges<-FROM.TO.River.Nodes[,c('FROM.x', 'TO.x', 'INDEX')]  ;

g.River.vertices<-River.Nodes.Elevation ;

g.River<-graph.data.frame(g.River.edges, vertices=g.River.vertices, directed = T) ;

tkplot(g.River,layout=as.matrix.data.frame(River.Nodes.Elevation[,c('X', 'Y')]), vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

plot(g.River,layout=as.matrix.data.frame(River.Nodes.Elevation[,c('X', 'Y')]), vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

plot(g.River, layout=layout_nicely,vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

tkplot(g.River, canvas.width=1800, canvas.height=900, layout=layout_nicely,vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

vcount(g.River) 

ecount(g.River)

str(g.River)
V(g.River)
E(g.River)


##### Add the river nodes from the unrefined river mesh#########################
library(sp) ;

library(rgdal) ;


# Read information about the shape file

MergedRiver.info<-ogrInfo('C:/Aun Trabajo en Proceso/HansYostDeepCreek/MergedRiver.shp');

MergedRiver.info$nrows 


# Read  the shape file

MergedRiver<-readOGR('C:/Aun Trabajo en Proceso/HansYostDeepCreek/MergedRiver.shp') ;


str(MergedRiver, max.level = 2)  ;


# get the ID's of the line segments in the shape file read

sapply(slot(MergedRiver,"lines"), function(x) slot(x,"ID"))  


#get the coordinates of the first line segment of the shape file read

coordinates(MergedRiver)[[1]]

# get the coordinates of all the line segments of the shape file read in a list form


lapply(coordinates(MergedRiver),function(x) x[[1]])

# get the coordinates of all the line segments of the shape file read as an  array 

sapply(coordinates(MergedRiver),function(x) x[[1]])


# transform the array of line segments coordinates into a matrix format 




MergedRiver.coords.matrix<-matrix(data=sapply(coordinates(MergedRiver),function(x) x[[1]]), nrow = MergedRiver.info$nrows, ncol = 4, byrow=T) ;


# arrange the matrix of line segments coordinates into a coherent data rame with lines representd by two points consiting of a pari of x,y coordinates

MergedRiver.coords.df<-data.frame(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)]);

names(MergedRiver.coords.df)<-c('P1.X', 'P1.Y', 'P2.X', 'P2.Y') ;

#addd a line ID to the data frame to be able to differentiate line components 

#Point.coords.df$Line<-sapply(slot(HYDC,"lines"), function(x) slot(x,"ID")) ;

MergedRiver.coords.df$Line.ID<-as.character(seq(1:MergedRiver.info$nrows)) ;

# stack the points of all the lines obtained in matrix from to remove repeated points ( there are many, all the lines that are contiguous share common points) and assign unique points a unique point ID

MergedRiver.Stacked.Point.coords<-rbind(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)])   ;


str(MergedRiver.Stacked.Point.coords) ;



MergedRiver.Unique.Point.coords<-data.frame(unique(MergedRiver.Stacked.Point.coords))  ;

names(MergedRiver.Unique.Point.coords)<-c("X" , "Y");

MergedRiver.Unique.Point.coords$Point.ID<-seq(1:dim(MergedRiver.Unique.Point.coords)[1]) ;

head(MergedRiver.Unique.Point.coords) ;
str(MergedRiver.Unique.Point.coords)  ;

#Use the new point and lines unique identity to create a line point data frame that has, each point unique id forming the lines. This is acomplished by merging the unique points data frame with each set of from and to points in the lines data frame



MergedRiver.From.points<-merge(MergedRiver.coords.df[,c("P1.X", "P1.Y" ,  "Line.ID")], MergedRiver.Unique.Point.coords, by.x=c('P1.X', 'P1.Y'), by.y=c("X" , "Y"), all.x=T);

head(MergedRiver.From.points) ;

str(MergedRiver.From.points) ;


MergedRiver.To.points<-merge(MergedRiver.coords.df[,c("P2.X", "P2.Y" ,  "Line.ID")], MergedRiver.Unique.Point.coords, by.x=c('P2.X', 'P2.Y'), by.y=c("X" , "Y"), all.x=T);



MergedRiver.Line.Point<-merge(MergedRiver.From.points,MergedRiver.To.points,by="Line.ID");

str(MergedRiver.Line.Point)
head(MergedRiver.Line.Point)

###### Put together the  Merged river and the refined river nodes ######

head(MergedRiver.Unique.Point.coords) ;
str(MergedRiver.Unique.Point.coords);
head(River.Nodes.Elevation) ;
str(River.Nodes.Elevation) ;


Refined.Merged.Nodes<-merge(River.Nodes.Elevation, MergedRiver.Unique.Point.coords, by=c('X', 'Y'),all.x=T) ;

head(Refined.Merged.Nodes)
str(Refined.Merged.Nodes)
summary(Refined.Merged.Nodes)

g.River.2.edges<-FROM.TO.River.Nodes[,c('FROM.x', 'TO.x', 'INDEX')]  ;

g.River.2.vertices<-Refined.Merged.Nodes[, c('Index', 'X', 'Y','Zmax', 'Point.ID') ] ;


g.River.2<-graph.data.frame(g.River.2.edges, vertices=g.River.2.vertices, directed = T) ;


plot(g.River.2, layout=layout_nicely,vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

tkplot(g.River.2, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=NA)

is.simple(g.River.2)

E(g.River.2)
degree(g.River.2, mode="in")
which(degree(g.River.2) == 0)
degree(g.River.2, mode="out")
which(degree(g.River.2, mode="out") == 2)

V(g.River.2)[is.na(Point.ID)]$color='Blue'
V(g.River.2)[!is.na(Point.ID)]$color='Red'


tkplot(g.River.2, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=g.River.2.vertices$Index, vertex.label.cex=1, vertex.label.dist=1)   ;

tkplot(g.River.2, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=paste(g.River.2.vertices$Index,round(g.River.2.vertices$Zmax,2),sep="-"), vertex.label.cex=1, vertex.label.dist=1) ;


tkplot(g.River.2[1:100], canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=paste(g.River.2.vertices$Index,round(g.River.2.vertices$Zmax,2),sep="-"), vertex.label.cex=1, vertex.label.dist=1);

g.River.sub<-induced.subgraph(g.River.2,seq(1,30)) ;

V(g.River.sub)[is.na(Point.ID)]$color='Blue'
V(g.River.sub)[!is.na(Point.ID)]$color='Red'


tkplot(g.River.sub, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1,vertex.label=paste(V(g.River.sub)$name,round(V(g.River.sub)$Zmax,2),sep="-"),vertex.label.cex=1, vertex.label.dist=1,margin=0.2) ;


str(g.River.sub)


V(g.River.sub)$name

list.vertex.attributes(g.River.sub)

list.edge.attributes(g.River.sub)


# Correct  vertex Zmax based on the grap and visial inspection

Ncorrec<-Refined.Merged.Nodes ;

#The correction is easy to do in Excel


library(XLConnect);

writeWorksheetToFile('C:/Users/frm10/Downloads/Ncorrec.xlsx', Ncorrec, sheet="Ncorrec", startRow=2);

Correct.Nodes<-readWorksheetFromFile('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Ncorrec.xlsx', sheet="Ncorrec", startRow = 1, endRow = 305, startCol= 1, endCol=8 );

head(Correct.Nodes)

plot(Correct.Nodes$Index,Correct.Nodes$Zmax.Correct)


River.Nodes.Elevation.Corrected<-RNEC<-Correct.Nodes[,c('Index' , 'X' , 'Y' , 'Zmin' , 'Zmax' , 'Zmax.Correct')] ;



