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

View(River.data.3)
####### match the river edges with the edges from the trianglulation and tin mesh

head(River.data.3)

#River.data.3[,c("ID","from.X.x","from.Y.x", "to.X.x", "to.Y.x", "FID.x", "INDEX.x","S_Order.x", "BoundaryMarker.x" , "to.X.y",  "to.Y.y" ,"from.X.y" ,  "from.Y.y" , "FID.y", "INDEX.y" ,"S_Order.y", "BoundaryMarker.y")]

River.data.3[,c("INDEX.x", "S_Order.x" , "BoundaryMarker.x", "INDEX.y" ,"S_Order.y", "BoundaryMarker.y")]


###### creating a 'sets' object to be used with the "sets" package and perform set operations on the river segments and nodes and traingle elements and nodes
### https://cran.r-project.org/web/packages/sets/index.html



#### River shape file segments node set

River.data.4<-as.list(as.data.frame(t(River.data.3[,c("INDEX.x","INDEX.y")]))) #,River.data.3[,c("INDEX.y")]);

names(River.data.4)<-paste0("R_",River.data.3$ID)  ;

str(River.data.4)



##### Trinagle elements node data set preocessing

### convert the tringel nodes data set into a list with each element composed of the nodes that form each trinagle.


ele.data<-as.list(as.data.frame(t(Watershed.1.ele[,c("node1" , "node2" , "node3")])))

names(ele.data)<-paste0("T_" ,Watershed.1.ele$triangle);

str(ele.data)



#####  use set operations using the sets package https://cran.r-project.org/web/packages/sets/index.html to compare each river segment node pairs with all the triangles node triplets and find the ones that are propper subsets; that is the trinagles that contain the two nodes of the river.

#### also calculate the symetric difference between the river nodes and the Trinagle nodes to extract the node that belongs to each trinagle neighboring a river segment 



RivSeg.Triangle<-data.frame(character(), character(),integer(), integer(),integer(), integer(),integer(), integer()) ;
names(RivSeg.Triangle)<-c('Triangle','Tnode_1', 'Tnode_2' , 'Tnode_3', 'RiverSegment', 'Rnode_1', 'Rnode_2', 'SymDiff') ;

for (i in seq(1,length(ele.data))) {
  for(j in seq(1,length(River.data.4))) {
    if (as.set(River.data.4[[j]]) <= as.set(ele.data[[i]])) {
      
      #as.integer(as.set(ele.data[[i]]) %D% as.set(River.data.4[[j]]))

      RivSeg.Triangle<-rbind(RivSeg.Triangle,data.frame(as.character(names(ele.data[i])), as.integer(ele.data[[i]][1]),as.integer(ele.data[[i]][2]), as.integer(ele.data[[i]][3]) , as.character(names(River.data.4[j])), as.integer(River.data.4[[j]][1]),as.integer(River.data.4[[j]][2]),as.integer(as.set(ele.data[[i]]) %D% as.set(River.data.4[[j]])) ))
      
      
    }
  }
}


names(RivSeg.Triangle)<-c('Triangle','Tnode_1', 'Tnode_2' , 'Tnode_3', 'RiverSegment', 'Rnode_1', 'Rnode_2', 'SymDiff') ;
print(RivSeg.Triangle)



####### Organize the river segments and the triangle neighbors ########


RivSeg.Triangle.1<-merge(RivSeg.Triangle,River.data.3[,c("RiverSegment" ,"INDEX.x", "from.X.x" , "from.Y.x" , "INDEX.y" ,  "to.X.x" , "to.Y.x" )], by="RiverSegment") ;

##### The river segment orinetation is opposite to the direction of the river segment; that is the right triangle is the trinagle to the right of the river segment from the perspective of the "to" to "from" nodes coordinates. This is opposite the water flow in the river.



head(RivSeg.Triangle.1)

head(Watershed.1.node)


RivSeg.Triangle.2<-merge(RivSeg.Triangle.1,Watershed.1.node, by.x="SymDiff", by.y="INDEX", all.x=T) ;

#### from the symetric difference between the river nodes and the Trinagle nodes  and the  extracted the node that belongs to each trinagle neighboring a river segment find the angle between the river segment and the triangle segment, formed by the comon origin node of the river and the trinagle ( the "to" node coordinates) and the simetrical difference node only belonging to the triangle. 

### to find the angle between two vectors use the dot product (scalar product) of the two vectors


### Calculate the scalar product (dot product) of the river and the triangle segments using matrix multiplication  - %*%  - and extracting the diago0nal of the resulting matrix

RivSeg.Triangle.2$RivS_vect_X<-RivSeg.Triangle.2$from.X.x - RivSeg.Triangle.2$to.X.x  ;

RivSeg.Triangle.2$RivS_vect_Y<-RivSeg.Triangle.2$from.Y.x - RivSeg.Triangle.2$to.Y.x  ;

RivSeg.Triangle.2$TriS_vect_X<-RivSeg.Triangle.2$X - RivSeg.Triangle.2$to.X.x  ;

RivSeg.Triangle.2$TriS_vect_Y<-RivSeg.Triangle.2$Y -  RivSeg.Triangle.2$to.Y.x  ;




Riv_dot_TriangleEdge<-diag(as.matrix(RivSeg.Triangle.2[,c("RivS_vect_X","RivS_vect_Y")]) %*%  t(as.matrix(RivSeg.Triangle.2[,c("TriS_vect_X" , "TriS_vect_Y")] ))) ;


### to calculate the norm of each vector use the scalar product (dot product) of the segment to itself- %*%  - extracting the diago0nal of the resulting matrix and finding the square root of it

Riv_norm<-sqrt(diag(as.matrix(RivSeg.Triangle.2[,c("RivS_vect_X", "RivS_vect_Y")]) %*% t(as.matrix(RivSeg.Triangle.2[,c("RivS_vect_X", "RivS_vect_Y")])))) ;

TriangleEdge_norm<-sqrt(diag(as.matrix(RivSeg.Triangle.2[,c("TriS_vect_X" , "TriS_vect_Y" )]) %*% t(as.matrix(RivSeg.Triangle.2[,c("TriS_vect_X" , "TriS_vect_Y" )])))) ;


##### Get vthe unit vector coordinates for each  river and triangle vector


RivSeg.Triangle.2$RivS_Uvect_X<-RivSeg.Triangle.2$RivS_vect_X/Riv_norm ;

RivSeg.Triangle.2$RivS_Uvect_Y<-RivSeg.Triangle.2$RivS_vect_Y/Riv_norm ;

RivSeg.Triangle.2$RivS_vect_AngleFromX<-acos(RivSeg.Triangle.2$RivS_Uvect_X) ;



##### Using Matrix rotation to allign both vectors to the x axis 

RivSeg.Triangle.2$RivS_Uvect_X_axs<-(RivSeg.Triangle.2$RivS_Uvect_X * cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX)) - (RivSeg.Triangle.2$RivS_Uvect_Y*sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX))  ;

RivSeg.Triangle.2$RivS_Uvect_Y_axs<-(RivSeg.Triangle.2$RivS_Uvect_X * sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX)) + (RivSeg.Triangle.2$RivS_Uvect_Y*cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX))  ;






RivSeg.Triangle.2$TriS_Uvect_X<-RivSeg.Triangle.2$TriS_vect_X/TriangleEdge_norm

RivSeg.Triangle.2$TriS_Uvect_Y<-RivSeg.Triangle.2$TriS_vect_Y/TriangleEdge_norm


RivSeg.Triangle.2$TriS_Uvect_X_axs<-(RivSeg.Triangle.2$TriS_Uvect_X * cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX )) - (RivSeg.Triangle.2$TriS_Uvect_Y *sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX) ) ;

RivSeg.Triangle.2$TriS_Uvect_Y_axs<-(RivSeg.Triangle.2$TriS_Uvect_X * sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX) ) + (RivSeg.Triangle.2$TriS_Uvect_Y * cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX) ) ;

CosineRiv_TriangleEdge<-Riv_dot_TriangleEdge/(Riv_norm*TriangleEdge_norm) ;

RivSeg.Triangle.2$Angle<-(acos(CosineRiv_TriangleEdge)*180)/pi


head(RivSeg.Triangle.2)


RivSeg.Triangle.2$Side<-"A" 

RivSeg.Triangle.2[which(RivSeg.Triangle.2$TriS_Uvect_Y_axs > 0) , "Side" ]<-c("LEFT") ;
RivSeg.Triangle.2[which(RivSeg.Triangle.2$TriS_Uvect_Y_axs < 0), "Side" ]<-c("RIGHT") ;



RivSeg.Triangle.2[which(RivSeg.Triangle.2$Side == "RIGHT") , ] 


RivSeg.Triangle.2[which(RivSeg.Triangle.2$Side == "LEFT") , ] 


diag(as.matrix(RivSeg.Triangle.2[,c('to.X.x' , 'to.Y.x', 'from.X.x', 'from.Y.x')]) %*%  t(as.matrix(RivSeg.Triangle.2[,c('to.X.x' , 'to.Y.x', 'X', 'Y')])))

diag()


t(as.matrix(RivSeg.Triangle.2[,c('to.X.x' , 'to.Y.x', 'X', 'Y')]))









                            