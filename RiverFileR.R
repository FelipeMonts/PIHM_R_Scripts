##############################################################################################################
# 
# 
# Program to exctract the river data from TauDEM and PHIM-GIS  and triangle outputs and creat the river file for MM-PIHM Simulations 
# 
# 
#  Felipe Montes 2019 /10  /10 
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################


#  Tell the program where the package libraries are  #####################

.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory


setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\Yahara20200110") ;  


###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


# Install the packages that are needed #






###############################################################################################################
#                           load the libraries that are neded   
###############################################################################################################


library(sp) ;
library(rgdal) ;
library(sets) ;
library(utils);


########### Load the results of the runing Triangle in PIHM GIS from the R script PIHM_Triangle_MeshFile  ###########


load('PIHM_Triangle_MeshFile.RData')  ;

#### Check the TIN file ####

plot(Watershed.TIN)


########### Load the river and merge  files rom PIHM_GIS ###################


Merge<-readOGR(".\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.shp") ;

plot(Merge, col="RED", add=T)


########### Read River shape file (simplified and splited) information  ###########

River.shp<-readOGR(".\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110River.shp") ;

River.shp@data
plot(River.shp, col="BLUE",add=T)



###########  get the coordinates of the river vertex  ###########



######## get the coordinates from the river shape file
RivCords<-coordinates(River.shp)

str(RivCords)

River.shp@data$from.X<-sapply(sapply(RivCords,'[',1),'[',1,1) ;

River.shp@data$from.Y<-sapply(sapply(RivCords,'[',1),'[',1,2) ;



River.shp@data$to.X<-sapply(sapply(RivCords,'[',1),'[',2,1) ;

River.shp@data$to.Y<-sapply(sapply(RivCords,'[',1),'[',2,2) ;

####### match the river shape file vertex coordinates with the .node file from trinagle 


River.data.1<-merge(River.shp@data, Watershed.1.node, by.x=c("from.X","from.Y"), by.y=c("X","Y"),all.x=T);

River.data.2<-merge(River.shp@data, Watershed.1.node, by.x=c("to.X","to.Y"), by.y=c("X","Y"),all.x=T);

River.data.3<-merge(River.data.1, River.data.2, by="ID") ;


####### match the river edges with the edges from the trianglulation and tin mesh

head(River.data.3)

#River.data.3[,c("ID","from.X.x","from.Y.x", "to.X.x", "to.Y.x", "FID.x", "INDEX.x","S_Order.x", "BoundaryMarker.x" , "to.X.y",  "to.Y.y" ,"from.X.y" ,  "from.Y.y" , "FID.y", "INDEX.y" ,"S_Order.y", "BoundaryMarker.y")]

River.data.3[,c("INDEX.x", "S_Order.x" , "BoundaryMarker.x", "INDEX.y" ,"S_Order.y", "BoundaryMarker.y")]


###### creating a 'sets' object to be used with the "sets" package and perform set operations on the river segments and nodes and traingle elements and nodes
### https://cran.r-project.org/web/packages/sets/index.html



#### River shape file segments node set

River.data.4<-as.list(as.data.frame(t(River.data.3[,c("INDEX.x","INDEX.y")]))) #,River.data.3[,c("INDEX.y")]);

names(River.data.4)<-paste0("R_",River.data.3$FID.x)  ;

str(River.data.4)



##### Trinagle elements node data set


ele.data<-as.list(as.data.frame(t(Watershed.1.ele[,c("node1" , "node2" , "node3")])))

names(ele.data)<-paste0("T_" ,Watershed.1.ele$triangle);

str(ele.data)

RivSeg.Triangle<-data.frame(character(), character()) ;
names(RivSeg.Triangle)<-c('Triangle','RiverSegment') ;

for (i in seq(1,length(ele.data))) {
  for(j in seq(1,length(River.data.4))) {
    if (as.set(River.data.4[[j]]) <= as.set(ele.data[[i]])) {
      RivSeg.Triangle<-rbind(RivSeg.Triangle,data.frame(as.character(names(ele.data[i])),as.character(names(River.data.4[j]))))
    }
  }
}


names(RivSeg.Triangle)<-c('Triangle','RiverSegment') ;
print(RivSeg.Triangle)

####### check the boudaries of the .poly and .node files to find the river segments ########



#  https://www.cs.cmu.edu/~quake/triangle.help.html
# Boundary Markers from :
#   
#   Boundary markers are tags used mainly to identify which output vertices
# and edges are associated with which PSLG segment, and to identify which
# vertices and edges occur on a boundary of the triangulation.  A common
# use is to determine where boundary conditions should be applied to a
# finite element mesh.  You can prevent boundary markers from being written
# into files produced by Triangle by using the -B switch.
# 
# The boundary marker associated with each segment in an output .poly file
# and each edge in an output .edge file is chosen as follows:
#   - If an output edge is part or all of a PSLG segment with a nonzero
# boundary marker, then the edge is assigned the same marker.
# - Otherwise, if the edge lies on a boundary of the triangulation
# (even the boundary of a hole), then the edge is assigned the marker
# one (1).
# - Otherwise, the edge is assigned the marker zero (0).
# The boundary marker associated with each vertex in an output .node file
# is chosen as follows:
#   - If a vertex is assigned a nonzero boundary marker in the input file,
# then it is assigned the same marker in the output .node file.
# - Otherwise, if the vertex lies on a PSLG segment (even if it is an
#                                                    endpoint of the segment) with a nonzero boundary marker, then the
# vertex is assigned the same marker.  If the vertex lies on several
# such segments, one of the markers is chosen arbitrarily.
# - Otherwise, if the vertex occurs on a boundary of the triangulation,
# then the vertex is assigned the marker one (1).
# - Otherwise, the vertex is assigned the marker zero (0).
# 
# If you want Triangle to determine for you which vertices and edges are on
# the boundary, assign them the boundary marker zero (or use no markers at
#                                                     all) in your input files.  In the output files, all boundary vertices,
# edges, and segments will be assigned the value one.
# 
# 
# 








# # get the ID's of the line segments in the shape file read
# 
# River.SegID<-as.numeric(sapply(slot(River.shp,"lines"), function(x) slot(x,"ID")));
# 
# 
# #get the coordinates of the first line segment of the shape file read
# 
# coordinates(River.shp)[[1]]
# 
# 
# # get the coordinates of all the line segments of the shape file read as an  array
# 
# 
# River.FromPoint<-data.frame(t(sapply(coordinates(River.shp),function(x) x[[1]][1,]))) ;
# 
# River.FromPoint$ID<-River.SegID ;
# 
# River.ToPoint<-data.frame(t(sapply(coordinates(River.shp),function(x) x[[1]][2,]))) ;
# 
# 
# River.ToPoint$ID<-River.SegID ;
# 
# 
# 
# 
# ###### match the coordinates of the river segments with the coordinates of the nodes of the triangles
# 
# River.FromNode<-merge(River.FromPoint, Watershed.1.node, by.x=c("X1", "X2"), by.y=c('X' , 'Y')) ;
# 
# River.ToNode<-merge(River.ToPoint, Watershed.1.node, by.x=c("X1", "X2"), by.y=c('X' , 'Y')) ;
# 
# 
# River.FromTo<-merge(River.FromNode[,c('ID' , 'INDEX')] , River.ToNode[,c('ID' , 'INDEX')], by=c('ID')) ;
# 
# str(River.FromTo)
# tail(River.FromTo)
# 
# 
# 
# ##### create a list of two nodes combination corresponding to each river element
# 
# 
# 
# River.SegList<-as.list(as.data.frame(t(River.FromTo)[c(2,3),]))  ;
# 
# 
# names(River.SegList)<-River.FromTo$ID  ;
# 
# 
# head(River.SegList)
# 
# 
# ##### create a list of two nodes combinations correponding to each trinagle side element
# 
# 
# 
# 
# Triangle.NodeList<-as.list(as.data.frame(t(Watershed.1.ele)[2:4,])) ;
# 
# names(Triangle.NodeList)<-Watershed.1.ele[,c('triangle')] ;
# 
# 
# ##### take a permutation (combn2) of two at time on each triangle node set to represent all the edges of the triangle
# 
# 
# Triangle.EdgeList<-lapply(Triangle.NodeList, function(x) combn2(x)) ;
# 
# 
# head(River.SegList)
# 
# 
# ##### convert the trinagle edges nodes and the river segments into point sets and compare them to determine whic trinagles have edges on the river
# 
# 
# str(River.SegList[[1]])   # Are different classes int and num. River.SegList needs tobe converted to integer before set comparison
# 
# str(Triangle.EdgeList[[1]][1,])
# 
# str(as.integer(River.SegList[[1]]))
# 
# ####  compare the river segmenst witth each traingles edge set using set comparison
# 
# 
# 
# set(as.integer(River.SegList[[1]]))
# 
# set(Triangle.EdgeList[[1]][1,], Triangle.EdgeList[[1]][2,] ,Triangle.EdgeList[[1]][3,])
# 
# 
# #### initialize a dataframe for the trinagle elements number
# 
# 
# 
# 
# 
# RiverTriangles<-data.frame(triangle = integer(), node1 = integer(), node2 = integer(), node3 = integer(), River.Seg = integer()  ) ;
# 
# Row.counter = 0 ;
# 
# for (j in seq(1, length(River.SegList))) {
#   for (i in seq(1,length(Triangle.EdgeList))) {
#     if (set(as.integer(River.SegList[[j]][1]),as.integer(River.SegList[[j]][2])) %e% set(set(Triangle.EdgeList[[i]][1,1],Triangle.EdgeList[[i]][1,2]),set(Triangle.EdgeList[[i]][2,1],Triangle.EdgeList[[i]][2,2]),set(Triangle.EdgeList[[i]][3,1],Triangle.EdgeList[[i]][3,2]))) {
# 
#       Row.counter = Row.counter +1
# 
#       RiverTriangles[Row.counter, c("triangle", "node1" , "node2" , "node3")]<-Watershed.1.ele[i,]
#       RiverTriangles[Row.counter, c("River.Seg")]<-as.integer(names(River.SegList)[j])
# 
# 
#     }
# 
#   }
# }
# 
# RiverTriangles[duplicated(RiverTriangles$triangle),]
# 
# 
# RiverTriangles[RiverTriangles$triangle == 220,]
# 
# 
# River.neigh<-merge(RiverTriangles,Watershed.1.neigh, by='triangle',all.x=T)[order(River.neigh$River.Seg),]


head(River.FromTo)





