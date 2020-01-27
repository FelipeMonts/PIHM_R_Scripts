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

River.data.3$RiverSegment<-paste0("R_",River.data.3$ID)  ;

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



RivSeg.Triangle<-data.frame(character(), integer(),integer(), integer(),character(), integer(),integer(), integer() ) ;
names(RivSeg.Triangle)<-c('Triangle','Tnode_1', 'Tnode_2' , 'Tnode_3', 'RiverSegment', 'Rnode_1', 'Rnode_2', 'SymDiff') ;

for (i in seq(1,length(ele.data))) {
  for(j in seq(1,length(River.data.4))) {
    if (as.set(River.data.4[[j]]) <= as.set(ele.data[[i]])) {
      
      #as.integer(as.set(ele.data[[i]]) %D% as.set(River.data.4[[j]]))

      RivSeg.Triangle<-rbind(RivSeg.Triangle,data.frame(as.character(names(ele.data[i])), as.integer(ele.data[[i]][1]),as.integer(ele.data[[i]][2]), as.integer(ele.data[[i]][3]) , as.character(names(River.data.4[j])), as.integer(River.data.4[[j]][1]),as.integer(River.data.4[[j]][2]),as.integer(as.set(ele.data[[i]]) %D% as.set(River.data.4[[j]]))))
      
      
    }
  }
}


names(RivSeg.Triangle)<-c('Triangle','Tnode_1', 'Tnode_2' , 'Tnode_3', 'RiverSegment', 'Rnode_1', 'Rnode_2', 'SymDiff') ;
print(RivSeg.Triangle)



####### Organize the river segments and the triangle neighbors ########


RivSeg.Triangle.1<-merge(RivSeg.Triangle,River.data.3[,c("RiverSegment" ,"INDEX.x", "from.X.x" , "from.Y.x" , "INDEX.y" ,  "to.X.x" , "to.Y.x", "S_Order.x" )], by="RiverSegment") ;

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


##### Get the unit vector coordinates for each  river and triangle vector by scaling them by their norm


RivSeg.Triangle.2$RivS_Uvect_X<-RivSeg.Triangle.2$RivS_vect_X/Riv_norm ;

RivSeg.Triangle.2$RivS_Uvect_Y<-RivSeg.Triangle.2$RivS_vect_Y/Riv_norm ;

RivSeg.Triangle.2$RivS_vect_AngleFromX<-acos(RivSeg.Triangle.2$RivS_Uvect_X) ;



##### Using Matrix rotation to allign both unitary river vectors to the x axis 

RivSeg.Triangle.2$RivS_Uvect_X_axs<-(RivSeg.Triangle.2$RivS_Uvect_X * cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX)) - (RivSeg.Triangle.2$RivS_Uvect_Y*sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX))  ;

RivSeg.Triangle.2$RivS_Uvect_Y_axs<-(RivSeg.Triangle.2$RivS_Uvect_X * sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX)) + (RivSeg.Triangle.2$RivS_Uvect_Y*cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX))  ;


##### Make the triangle and the river vestors unit vectors by scalling them to their norm


RivSeg.Triangle.2$TriS_Uvect_X<-RivSeg.Triangle.2$TriS_vect_X/TriangleEdge_norm

RivSeg.Triangle.2$TriS_Uvect_Y<-RivSeg.Triangle.2$TriS_vect_Y/TriangleEdge_norm

##### Using Matrix rotation to allign both unitary trinagle vectors to the x axis 


RivSeg.Triangle.2$TriS_Uvect_X_axs<-(RivSeg.Triangle.2$TriS_Uvect_X * cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX )) - (RivSeg.Triangle.2$TriS_Uvect_Y *sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX) ) ;

RivSeg.Triangle.2$TriS_Uvect_Y_axs<-(RivSeg.Triangle.2$TriS_Uvect_X * sin(-RivSeg.Triangle.2$RivS_vect_AngleFromX) ) + (RivSeg.Triangle.2$TriS_Uvect_Y * cos(-RivSeg.Triangle.2$RivS_vect_AngleFromX) ) ;

##### Caluclating the cosine of the angle between the river vector and the triangle vector using the dot product 

CosineRiv_TriangleEdge<-Riv_dot_TriangleEdge/(Riv_norm*TriangleEdge_norm) ;

RivSeg.Triangle.2$Angle<-(acos(CosineRiv_TriangleEdge)*180)/pi


head(RivSeg.Triangle.2)



###########################################################################################################################################################
# ##### Determining at which side is each trinagle by the sign of the sine of the angle (y Coordinate) between of the unit river vector alligned with the x axis and the unit trinagle vector. The direction of the triangles is based on the to-from direction of the eiver segments as follows:
# 
#                             River from node \/                            \/
#                                             |                             |
#                                             |        River flow direction |
#                                             |                             |
#                         Left Triangle       |       Right Triangle        \/
#                                             | 
#                                             | 
#                                             |
#                                             |
#                              River to node  \/
#
##########################################################################################################################################################


####### Collecting the trinagel siedsa on the "Side" column of the data frame.

RivSeg.Triangle.2$Side<-"A" 

RivSeg.Triangle.2[which(RivSeg.Triangle.2$TriS_Uvect_Y_axs > 0) , "Side" ]<-c("LEFT") ;
RivSeg.Triangle.2[which(RivSeg.Triangle.2$TriS_Uvect_Y_axs < 0), "Side" ]<-c("RIGHT") ;


###### Checking that there is no parallel or 0 angles on the river and trinagel vectors

RivSeg.Triangle.2[which(RivSeg.Triangle.2$TriS_Uvect_Y_axs == 0),] ;

RivSeg.Triangle.2[which(RivSeg.Triangle.2$TriS_Uvect_Y_axs == 0), "Side" ]<-c("UP") ;


###### Check ona of the river segments to test if the results agree with the shape files

RivSeg.Triangle.2[which(RivSeg.Triangle.2$Triangle == 'T_94'),]
                        



###### ordering the data frame according to the river nodes, starting from the river node "from" 


RivSeg.Triangle.3<-RivSeg.Triangle.2[order(RivSeg.Triangle.2$Rnode_1),] ;

#RivSeg.Triangle.3$RivXTriangle<-paste0(RivSeg.Triangle.3$RiverSegment,'_' , RivSeg.Triangle.3$Triangle) ;


names(RivSeg.Triangle.3)
head(RivSeg.Triangle.3)

###### aggregating results according to the river format required fro PIHM

str(RivSeg.Triangle.3[, c("RiverSegment", "Rnode_1" , "Rnode_2"  )])


##### collect all the left side triangles

Riv.1<-RivSeg.Triangle.3[which(RivSeg.Triangle.3$Side == 'LEFT'), c("RiverSegment", "Rnode_1" , "Rnode_2",'Side' , 'Triangle', 'BoundaryMarker', "S_Order.x")] ;


##### collect all the right side triangles



Riv.2<-RivSeg.Triangle.3[which(RivSeg.Triangle.3$Side == 'RIGHT'), c("RiverSegment", "Rnode_1" , "Rnode_2",'Side' , 'Triangle' ,'BoundaryMarker' ,"S_Order.x")] ;


##### Merge left and side trinagles in the fromat reqired by PIHM

Riv.3<-merge(Riv.1,Riv.2, by=c("RiverSegment", "Rnode_1" , "Rnode_2", "S_Order.x" ), all=T);

Riv.4<-Riv.3[order(Riv.3$Rnode_1),] ;

head(Riv.4)

str(Riv.4)

View(Riv.4)


#### indexing the river segments from 1 to N 

map_Riv_Seg<-data.frame(Riv.Indx_GIS=unique(as.character(Riv.4$RiverSegment)), Riv.Indx_PIHM=seq(1,length(unique(as.character(Riv.4$RiverSegment))))) ; 

Riv.5<-merge(Riv.4,map_Riv_Seg, by.x='RiverSegment', by.y='Riv.Indx_GIS' ) ;
head(Riv.5)



#### work on getting the down river stream segment to match PIHM format



##### get the down river segment
Riv.6<-merge(Riv.5, Riv.5[,c('RiverSegment' , 'Rnode_1', 'Rnode_2')], by.x='Rnode_2', by.y='Rnode_1') ;
head(Riv.6)

Riv.7<-merge(Riv.6,map_Riv_Seg, by.x='RiverSegment.y', by.y='Riv.Indx_GIS' ) ;

print(names(Riv.7))
head(Riv.7)

Riv.7.Nam<-c("DOWN_GIS", "TO" , "INDEX_GIS" ,   "FROM" , "SHAPE" , "Side.x" ,  "LEFT_TRIANGLE" , "BoundaryMarker.x", "Side.y" , "RIGHT_TRIANGLE", "BoundaryMarker.y" ,"INDEX_PIHM" ,   "Rnode_2.y", "DOWN_PIHM"  )

data.frame(names(Riv.7),Riv.7.Nam)

names(Riv.7)<-Riv.7.Nam ;

head(Riv.7)


##### Final format of the file  to conform to the .riv file in PIHM

Riv.7[,c("INDEX_PIHM","FROM", "TO" , "DOWN_PIHM" , "LEFT_TRIANGLE" ,"RIGHT_TRIANGLE", "SHAPE" )] ;

Riv.7$LEFT<-as.integer(substr(Riv.7$LEFT_TRIANGLE,start=3,stop=10));

Riv.7$RIGHT<-as.integer(substr(Riv.7$RIGHT_TRIANGLE,start=3,stop=10));

head(Riv.7)

Riv.8<-Riv.7[,c('INDEX_PIHM' , 'FROM' , 'TO' , 'DOWN_PIHM' , 'LEFT' , 'RIGHT', 'SHAPE' , 'SHAPE')];

Riv.8$BC<-0 ;

Riv.8$RES<-0 ;


head(Riv.8) 

names(Riv.8)<-c( 'INDEX', 'FROM' , 'TO' , 'DOWN' , 'LEFT' , 'RIGHT', 'SHAPE' , 'MATL' , 'BC ' , 'RES') ;

head(Riv.8) 


Riv.NUMRIV<-data.frame(c('NUMRIV'), NUMRIV=dim(Riv.8)[1] , stringsAsFactors = F) ;

####### Write the river file into the text file. riv.



######## Print the NUMRIV line

Riv.NUMRIV

write.table(Riv.NUMRIV, file=paste0(Watershed.name, ".riv"), row.names=F ,col.names=F, quote=F, sep ="\t") ;



####   write the river  section of the river file 

write.table(Riv.8, file=paste0(Watershed.name, ".riv"), row.names=F ,col.names=T, quote=F, sep ="\t", append = T) ;






