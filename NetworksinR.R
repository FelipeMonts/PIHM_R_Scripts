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


Project<-"MergeVectorLayer000_q25_a100000" ;



library(devtools)  ;


#install.packages("sand") ;
library(sand)
#install_sand_packages() ;

load(paste0('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs\\',Project,'\\MM_PHIMInputsR_V2.RData'));




##### Add the river nodes from the  river mesh#########################
library(sp) ;

library(rgdal) ;


# Read information about the shape file

MergedRiver.info<-ogrInfo('C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/Stream8000_sln32_dens150m_xln_Decomp.shp');



MergedRiver.info$nrows 


# Read  the shape file

MergedRiver<-readOGR('C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/Stream8000_sln32_dens150m_xln_Decomp.shp');


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


# arrange the matrix of line segments coordinates into a coherent data frame with lines representd by two points consiting of a pari of x,y coordinates

MergedRiver.coords.df<-data.frame(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)]);

names(MergedRiver.coords.df)<-c('P1.X', 'P1.Y', 'P2.X', 'P2.Y') ;

#addd a line ID to the data frame to be able to differentiate line components 

#Point.coords.df$Line<-sapply(slot(HYDC,"lines"), function(x) slot(x,"ID")) ;

MergedRiver.coords.df$Line.ID<-as.character(seq(1:MergedRiver.info$nrows)) ;

# stack the points of all the lines obtained in matrix from to remove repeated points ( there are many, all the lines that are contiguous share common points) and assign unique points a unique point ID

MergedRiver.Stacked.Point.coords<-rbind(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)])   ;

head(MergedRiver.Stacked.Point.coords) ;

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

head(MergedRiver.To.points)

str(MergedRiver.To.points)


MergedRiver.Line.Point<-merge(MergedRiver.From.points,MergedRiver.To.points,by="Line.ID");


head(MergedRiver.Line.Point)

str(MergedRiver.Line.Point)


###### Put together the  Merged river and the refined river nodes ######

head(MergedRiver.Unique.Point.coords) ;
str(MergedRiver.Unique.Point.coords);
head(River.Nodes.Elevation) ;
str(River.Nodes.Elevation) ;


Refined.Merged.Nodes<-merge(River.Nodes.Elevation, MergedRiver.Unique.Point.coords, by=c('X', 'Y'),all.x=T) ;

head(Refined.Merged.Nodes)
str(Refined.Merged.Nodes)
summary(Refined.Merged.Nodes)


g.River.2.edges<-MergedRiver.Line.Point[,c("Point.ID.x", "Point.ID.y" , "Line.ID") ] ;

g.River.2.edges<-g.River.2.edges[order(g.River.2.edges$Line.ID),] ;

head(g.River.2.edges,20)

# g.River.2.edges<-FROM.TO.River.Nodes[,c('FROM.x', 'TO.x', 'INDEX')]  ;

g.River.2.vertices<-Refined.Merged.Nodes[, c('Index', 'X', 'Y','Zmax', 'Point.ID') ] ;


head(g.River.2.vertices)

#g.River.2.vertices<-Refined.Merged.Nodes[, c('Index', 'X', 'Y','Zmax', 'Point.ID') ] ;


g.River.2<-graph.data.frame(g.River.2.edges, vertices=g.River.2.vertices, directed = T) ;

str(g.River.2);



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

tkplot(g.River.2, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.5, vertex.label=paste(g.River.2.vertices$Index,round(g.River.2.vertices$Zmax,2),sep="-"), vertex.label.cex=1, vertex.label.dist=1) ;


# g.River.sub<-induced.subgraph(g.River.2,seq(1,30)) ;
# 
# V(g.River.sub)[is.na(Point.ID)]$color='Blue'
# V(g.River.sub)[!is.na(Point.ID)]$color='Red'
# 
# 
# tkplot(g.River.sub, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=1.0,vertex.label=paste(V(g.River.sub)$name,round(V(g.River.sub)$Zmax,2),sep="-"),vertex.label.cex=1, vertex.label.dist=1,margin=0.2) ;
# 
# 
# str(g.River.sub)
# 
# 
# V(g.River.sub)$name
# 
# list.vertex.attributes(g.River.sub)
# 
# list.edge.attributes(g.River.sub)



Edgs<-igraph::as_data_frame(g.River.2, what=c("edges"));

Vertcs<-igraph::as_data_frame(g.River.2, what=c("vertices")) ;

from.Vertcs<-merge(Edgs, Vertcs, by.x="from", by.y="name", all.x=T ) ;

to.Vertcs<-merge(Edgs, Vertcs, by.x="to", by.y="name", all.x=T ) ;

Edgs.Zmax<-merge(from.Vertcs, to.Vertcs, by="Line.ID") ;

Edgs.Zmax$Elev.Dif<-Edgs.Zmax$Zmax.x-Edgs.Zmax$Zmax.y ;

head(Edgs.Zmax)

g.River.3.edges<-merge(Edgs,Edgs.Zmax, by="Line.ID")[,c("from" , "to" , "Line.ID" , "from.x" , "to.x" , "X.x" , "Y.x" , "Zmax.x" , "Point.ID.x" , "to.y" , "from.y" , "X.y" , "Y.y" , "Zmax.y" , "Point.ID.y" ,"Elev.Dif"  )] ;

names(g.River.3.edges)


g.River.3<-graph.data.frame(g.River.3.edges, vertices=g.River.2.vertices, directed = T) ;


V(g.River.3)[is.na(Point.ID)]$color='Blue'
V(g.River.3)[!is.na(Point.ID)]$color='Red'


edge_attr(g.River.3, 'Elev.Dif')





tkplot(g.River.3, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.5, vertex.label=g.River.2.vertices$Index, vertex.label.cex=1, vertex.label.dist=1, edge.label=round(g.River.3.edges$Elev.Dif,2), edge.label.cex=1)  ;




tkplot(graph.data.frame(g.River.3.edges[g.River.3.edges$Elev.Dif<0,]),canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.5,vertex.label=g.River.2.vertices$Index, vertex.label.cex=1, vertex.label.dist=1, edge.label=round(g.River.3.edges$Elev.Dif,2), edge.label.cex=1)





g.River.3.sub<-subgraph.edges(g.River.3,eids=which(E(g.River.3)$Elev.Dif < 0 ));

g.River.3.sub.data<-as_long_data_frame(g.River.3.sub) ;


vertex.attributes(g.River.3.sub)

V(g.River.3.sub)[g.River.3.sub.data$from]

tkplot(g.River.3.sub,canvas.width=1800, canvas.height=900, layout=layout_nicely ,V(g.River.3.sub)$name, vertex.size= 2, edge.arrow.size=0.5, vertex.label.cex=1, vertex.label.dist=1, edge.label=round(g.River.3.sub.data$Elev.Dif,3),edge.label.cex=1) ;


g.River.4.sub<-make_ego_graph(g.River.3,3,as_ids(V(g.River.3.sub)[g.River.3.sub.data$from]), mode=c("all"))  ;

length(g.River.4.sub)

V(g.River.4.sub[[1]])

as_long_data_frame(g.River.4.sub[[1]])


for (i in seq(1:length(g.River.4.sub))) {
 
  tkplot(g.River.4.sub[[i]],canvas.width=1800, canvas.height=900, layout=layout_nicely ,V(g.River.4.sub[[i]])$name, vertex.size= 2, edge.arrow.size=0.5, vertex.label.cex=1, edge.label=round(E(g.River.4.sub[[i]])$Elev.Dif,8),vertex.label.dist=1,edge.label.cex=1) ; 
  
}


str(g.River.3.sub.data$Point.ID.x)

mesh.Elements[which(mesh.Elements$Index %in% g.River.3.sub.data$Point.ID.y),]
mesh.Nodes[which(mesh.Nodes$Index %in% g.River.3.sub.data$Point.ID.y),]

str(mesh.Elements$Index)
str(mesh.Nodes)


mesh.Nodes.corrected<-mesh.Nodes   ;


###################### Correct the River nodes to the right elevation ################################################

mesh.Nodes.corrected[which(mesh.Nodes$Index == 13 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 13 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 13),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 18 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 18 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 18),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 34 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 34 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 34),c("Elev.Dif")]

 mesh.Nodes.corrected[which(mesh.Nodes$Index == 46 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 46 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 46),c("Elev.Dif")]

 mesh.Nodes.corrected[which(mesh.Nodes$Index == 47 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 47 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 46),c("Elev.Dif")]
 

mesh.Nodes.corrected[which(mesh.Nodes$Index == 56 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 56 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 56),c("Elev.Dif")]


mesh.Nodes.corrected[which(mesh.Nodes$Index == 83 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 83 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 83),c("Elev.Dif")]



mesh.Nodes.corrected[which(mesh.Nodes$Index == 91 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 91 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 91),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 98 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 98 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 98),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 130 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 130 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 130),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 199 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 199),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 198),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 216 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 216 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 215),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 228 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 228 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 227),c("Elev.Dif")]

mesh.Nodes.corrected[which(mesh.Nodes$Index == 234 ),c( 'Zmax')]<-mesh.Nodes[which(mesh.Nodes$Index == 234 ),c( 'Zmax')] + g.River.3.sub.data[which(g.River.3.sub.data$Point.ID.y== 233),c("Elev.Dif")]





#The correction is easy to do in Excel


# library(XLConnect);
# 
# writeWorksheetToFile('C:/Users/frm10/Downloads/Ncorrec.xlsx', Ncorrec, sheet="Ncorrec", startRow=2);
# 
# Correct.Nodes<-readWorksheetFromFile('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Ncorrec.xlsx', sheet="Ncorrec", startRow = 1, endRow = 305, startCol= 1, endCol=8 );
# 
# head(Correct.Nodes)
# 
# plot(Correct.Nodes$Index,Correct.Nodes$Zmax.Correct)
# 
# 
# River.Nodes.Elevation.Corrected<-RNEC<-Correct.Nodes[,c('Index' , 'X' , 'Y' , 'Zmin' , 'Zmax' , 'Zmax.Correct')] ;
# 


