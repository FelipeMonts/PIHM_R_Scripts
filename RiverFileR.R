##############################################################################################################
# 
# 
# Program to exctract the river data from the TauDEM and PHIM-GIS outputs and creat the river file for MM-PIHM Simulations 
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


setwd('C:/Felipe/Students Projects/Stephanie/HalfmoonWatershed/MM_PHIM_inputs') ;   #  setwd(RevisedOutputs.dir)   ;



###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


# Install the packages that are needed #






###############################################################################################################
#                           load the libraries that are neded   
###############################################################################################################




########### Read River shape file (simplified and splited) information  ###########


River.info<-ogrInfo("C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/2VectorProcessing/StreamPolyline200m_xln.shp" );

River.shp<-readOGR("C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/2VectorProcessing/StreamPolyline200m_xln.shp", p4s='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' );

River.shp@lines

coordinates(River.shp)


str(River.shp)

plot(River.shp)


# get the ID's of the line segments in the shape file read

River.SegID<-as.numeric(sapply(slot(River.shp,"lines"), function(x) slot(x,"ID")));


#get the coordinates of the first line segment of the shape file read

coordinates(River.shp)[[1]]


# get the coordinates of all the line segments of the shape file read as an  array


River.FromPoint<-data.frame(t(sapply(coordinates(River.shp),function(x) x[[1]][1,]))) ;

River.FromPoint$ID<-River.SegID ;

River.ToPoint<-data.frame(t(sapply(coordinates(River.shp),function(x) x[[1]][2,]))) ;


River.ToPoint$ID<-River.SegID ;




###### match the coordinates of the river segments with the coordinates of the nodes of the triangles

River.FromNode<-merge(River.FromPoint, Watershed.1.node, by.x=c("X1", "X2"), by.y=c('X' , 'Y')) ;

River.ToNode<-merge(River.ToPoint, Watershed.1.node, by.x=c("X1", "X2"), by.y=c('X' , 'Y')) ;


River.FromTo<-merge(River.FromNode[,c('ID' , 'INDEX')] , River.ToNode[,c('ID' , 'INDEX')], by=c('ID')) ;

str(River.FromTo)
tail(River.FromTo)



##### create a list of two nodes combination corresponding to each river element



River.SegList<-as.list(as.data.frame(t(River.FromTo)[c(2,3),]))  ;


names(River.SegList)<-River.FromTo$ID  ;


head(River.SegList)


##### create a list of two nodes combinations correponding to each trinagle side element




Triangle.NodeList<-as.list(as.data.frame(t(Watershed.1.ele)[2:4,])) ;

names(Triangle.NodeList)<-Watershed.1.ele[,c('triangle')] ;


##### take a permutation (combn2) of two at time on each triangle node set to represent all the edges of the triangle


Triangle.EdgeList<-lapply(Triangle.NodeList, function(x) combn2(x)) ;


head(River.SegList)


##### convert the trinagle edges nodes and the river segments into point sets and compare them to determine whic trinagles have edges on the river


str(River.SegList[[1]])   # Are different classes int and num. River.SegList needs tobe converted to integer before set comparison

str(Triangle.EdgeList[[1]][1,])

str(as.integer(River.SegList[[1]]))

####  compare the river segmenst witth each traingles edge set using set comparison



set(as.integer(River.SegList[[1]]))

set(Triangle.EdgeList[[1]][1,], Triangle.EdgeList[[1]][2,] ,Triangle.EdgeList[[1]][3,])


#### initialize a dataframe for the trinagle elements number





RiverTriangles<-data.frame(triangle = integer(), node1 = integer(), node2 = integer(), node3 = integer(), River.Seg = integer()  ) ;

Row.counter = 0 ;

for (j in seq(1, length(River.SegList))) {
  for (i in seq(1,length(Triangle.EdgeList))) {
    if (set(as.integer(River.SegList[[j]][1]),as.integer(River.SegList[[j]][2])) %e% set(set(Triangle.EdgeList[[i]][1,1],Triangle.EdgeList[[i]][1,2]),set(Triangle.EdgeList[[i]][2,1],Triangle.EdgeList[[i]][2,2]),set(Triangle.EdgeList[[i]][3,1],Triangle.EdgeList[[i]][3,2]))) {

      Row.counter = Row.counter +1

      RiverTriangles[Row.counter, c("triangle", "node1" , "node2" , "node3")]<-Watershed.1.ele[i,]
      RiverTriangles[Row.counter, c("River.Seg")]<-as.integer(names(River.SegList)[j])


    }

  }
}

RiverTriangles[duplicated(RiverTriangles$triangle),]


RiverTriangles[RiverTriangles$triangle == 220,]


River.neigh<-merge(RiverTriangles,Watershed.1.neigh, by='triangle',all.x=T)[order(River.neigh$River.Seg),]


head(River.FromTo)





