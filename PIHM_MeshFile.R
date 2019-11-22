##############################################################################################################
# 
# 
#       Program  to generate suitable input format for MM-PIHM  .mesh file without any input from PIHMGIS, only based on R and QGIS
#       An update from MM_PHIMInputsR_V2.R
#    
# 
#       For MM_PHIM input format corresponding to  (Prerelease 0.6.0 Alpha git@github.com:PSUmodeling/MM-PIHM.git)
# 
# 
#  Felipe Montes 2019 /11 /22
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


setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs') ;    ;   #  setwd(RevisedOutputs.dir)   ;



###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


# Install the packages that are needed #






###############################################################################################################
#                           load the libraries that are neded   
###############################################################################################################



###  load the libraries that are neded -- need to filter some of these  ---  

# load libraries
# library(Hmisc) ;
# library(plyr) ;
# library(dplyr)  ;
# library(soilDB) ;
# library(raster) ;
# library(aqp) ;
library(sp) ;
library(rgdal) ;
library(raster) ;
library(combinat) ;
library(sets) ;
# library(rgeos) ; 
# library(lattice) ;
# library(MASS) ;
# library(RColorBrewer) ;
# library(ggplot2)  ;
# #library(tmap) ;
# library(tidyr)  ;
# library(devtools) ;
# library(stats)
# 




######################################################################################################################################
# 
# 
#  Read the files resulting from the mesh generation using the Triangle software and create the Mesh File
# 
# 
# 
######################################################################################################################################

########### Read TIN shape file infromation  ###########

Watershed.TIN.info<-ogrInfo("../Oct0920191330/DomainDecomposition/MergeFeatures_q30_a1000000_o.shp" );



Watershed.TIN<-readOGR("../Oct0920191330/DomainDecomposition/MergeFeatures_q30_a1000000_o.shp",  p4s='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' );

print(Watershed.TIN)

plot(Watershed.TIN)


########### Read the .1.ele file  ###########

### Description from https://www.cs.cmu.edu/~quake/triangle.ele.html

# .ele files
# 
# First line: <# of triangles> <nodes per triangle> <# of attributes>
#   Remaining lines: <triangle #> <node> <node> <node> ... [attributes] 
# 
# Blank lines and comments prefixed by `#' may be placed anywhere. Triangles must be numbered consecutively, starting from one or zero. Nodes are indices into the corresponding .node file. The first three nodes are the corner vertices, and are listed in counterclockwise order around each triangle. (The remaining nodes, if any, depend on the type of finite element used.)
# 
# As in .node files, the attributes are typically floating-point values of physical quantities (such as mass or conductivity) associated with the elements (triangles) of a finite element mesh. Because there is no simple mapping from input to output triangles, an attempt is made to interpolate attributes, which may result in a good deal of diffusion of attributes among nearby triangles as the triangulation is refined. Attributes do not diffuse across segments, so attributes used to identify segment-bounded regions remain intact.
# 
# In output .ele files, all triangles have three nodes each unless the -o2 switch is used, in which case subparametric quadratic elements with six nodes are generated. The fourth, fifth, and sixth nodes lie on the midpoints of the edges opposite the first, second, and third vertices. 


Watershed.1.ele<-read.table("../Oct0920191330/DomainDecomposition/MergeFeatures.1.ele", header=FALSE, sep= "", skip=1) ;

names(Watershed.1.ele)<-c("triangle", "node1","node2","node3") ;

Watershed.NUMELE<-read.table("../Oct0920191330/DomainDecomposition/MergeFeatures.1.ele", header=FALSE, sep= "", nrows= 1)[1,1] ;

str(Watershed.1.ele)
tail(Watershed.1.ele)


########### Read the .1.node file  ###########


### Description from https://www.cs.cmu.edu/~quake/triangle.node.html

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

Watershed.1.node<-read.table("../Oct0920191330/DomainDecomposition/MergeFeatures.1.node", header=FALSE, sep= "", skip=1) ;

names(Watershed.1.node)<-c("INDEX" , "X" , "Y" , "BoundaryMarker") ;

Watershed.NUMNODE<-read.table("../Oct0920191330/DomainDecomposition/MergeFeatures.1.node", header=FALSE, sep= "", nrows=1)[1,1] ;

str(Watershed.1.node)
tail(Watershed.1.node)


####### Create a point shape file from the nodes coordinates to extract the ZMAX Values from the DEM


Node.Points<-SpatialPointsDataFrame(coords=Watershed.1.node[,c("X", "Y")],  proj4string=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'), data=Watershed.1.node, coords.nrs = c(2,3), match.ID = T);


####### Load the DEM from the fill pits file of the Reater processing directory


FillPits<-raster("../DEM_10m_NED/YaharaClipped2Conus.asc", crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') 

# image(FillPits)
# 
# plot(FillPits)

####### Extract the elevation data for each node


Node.Points.ZMAX<-data.frame(coordinates(Node.Points),data=Node.Points, raster::extract(FillPits,Node.Points));

names(Node.Points.ZMAX)

str(Node.Points.ZMAX)

head(Node.Points.ZMAX)


Node.Points.ZMAX[is.na(Node.Points.ZMAX$raster..extract.FillPits..Node.Points.), ] 


####### Read the 1.1 neigh file from triangle


### description from https://www.cs.cmu.edu/~quake/triangle.neigh.html

# .neigh files
# 
# First line: <# of triangles> <# of neighbors per triangle (always 3)>
#   Following lines: <triangle #> <neighbor> <neighbor> <neighbor> 
# 
# Blank lines and comments prefixed by `#' may be placed anywhere. Triangles are numbered consecutively, starting from one or zero. Neighbors are indices into the corresponding .ele file. An index of -1 indicates no neighbor (because the triangle is on an exterior boundary). The first neighbor of triangle i is opposite the first corner of triangle i, and so on.
# 
# Triangle can produce .neigh files (use the -n switch), but cannot read them. 


Watershed.1.neigh<-read.table("../Oct0920191330/DomainDecomposition/MergeFeatures.1.neigh", header=FALSE, sep= "", skip=1) ;

names(Watershed.1.neigh)<-c("triangle", "neighbor1","neighbor2","neighbor3") ;

str(Watershed.1.neigh)
tail(Watershed.1.neigh)


save.image(file='PIHMMeshFile.RData');




###### Add the depth of the soil from the Soils file extracted from the GSURGO database and calculated using the SoilDepthSSurgo_V2.R. 

load('SoilDepthSSurgo_V2.RData') ;

head(Nodes.soil.depth.avg)

str(Nodes.soil.depth.avg)


#### change Nodes.soil.depth.avg$node.Factor from factor class  to integer


Nodes.soil.depth.avg$node<-as.integer(as.character(Nodes.soil.depth.avg$node.Factor)) ;

head(Nodes.soil.depth.avg)

str(Nodes.soil.depth.avg)


##### calculate Zmin from Zmax and soil depth and check if there is any negative difference between Zmax-Zmin.SSURGO 



Node.Points.ZMAX_ZMIN<-merge(Node.Points.ZMAX,Nodes.soil.depth.avg, by.x='data.INDEX', by.y='node') ;

####  Soil depth is in cm and ZMAX is in meters above sea level    

Node.Points.ZMAX_ZMIN$Diff.Z<-Node.Points.ZMAX_ZMIN$raster..extract.FillPits..Node.Points. - (Node.Points.ZMAX_ZMIN$soil.depth / 100 ); # meters - cm/100 

Node.Points.ZMAX_ZMIN[which(Node.Points.ZMAX_ZMIN$Diff.Z <= 0),] ;

plot(Node.Points.ZMAX_ZMIN$data.INDEX, Node.Points.ZMAX_ZMIN$Diff.Z )  ;

Node.Points.ZMAX_ZMIN[which(Node.Points.ZMAX_ZMIN$Diff.Z <= 0.20),]  ;






################## Write out the appropiate formated "Mesh" File for the MM-PIHM input format ##################################


Watershed.name<-"Yahara_"  ;


####  write the first lines of the new MM-PIHM mesh file
str(data.frame('NUMELE', Watershed.NUMELE,stringsAsFactors=F))


write.table(data.frame('NUMELE', Watershed.NUMELE,stringsAsFactors=F), file=paste0(Watershed.name, ".mesh"), row.names=F ,col.names=F, quote=F, sep ="\t") ;


#### create the triangle mesh elements section

head(Watershed.1.ele)
str(Watershed.1.ele)

head(Watershed.1.neigh)
str(Watershed.1.neigh)



#### Merge the .ele and .neigh information

ele_and_neigh<-merge(Watershed.1.ele, Watershed.1.neigh, by='triangle')

names(ele_and_neigh)<-c('INDEX','NODE1', 'NODE2' , 'NODE3', 'NABR1' , 'NABR2' ,'NABR3') ;

head(ele_and_neigh)
str(ele_and_neigh)


#### Change the boundary elements neighbors from -1 to 0

ele_and_neigh$NABR1[which(ele_and_neigh$NABR1 == -1)]<-0

ele_and_neigh$NABR2[which(ele_and_neigh$NABR2 == -1)]<-0

ele_and_neigh$NABR3[which(ele_and_neigh$NABR3 == -1)]<-0

which(ele_and_neigh[,] == -1)



#write.table(ele_and_neigh, file=paste0(Watershed.name, ".mesh"), row.names=F , col.names=T, quote=F, sep ="\t", append = T) ;




########## Create The Node section of the mesh file



head(Watershed.1.node)
str(Watershed.1.node)


head(Node.Points.ZMAX_ZMIN)
str(Node.Points.ZMAX_ZMIN)


Node_and_ZMAX_ZMIN <-merge(Watershed.1.node,Node.Points.ZMAX_ZMIN, by.x='INDEX'  , by.y='data.INDEX' ) ;

head(Node_and_ZMAX_ZMIN)
str(Node_and_ZMAX_ZMIN)



mesh.Part2<-Node_and_ZMAX_ZMIN[,c('INDEX', 'X.x', 'Y.x', 'Diff.Z', 'raster..extract.FillPits..Node.Points.')] ;

names(mesh.Part2)<-c('INDEX' , 'X' , 'Y' , 'ZMIN',  'ZMAX') ;

head(mesh.Part2)
str(mesh.Part2)


######## Print the NUMMODE line

Watershed.NUMNODE

#write.table(data.frame('NUMNODE', Watershed.NUMNODE , stringsAsFactors=F), file=paste0(Watershed.name, ".mesh"), row.names=F ,col.names=F, quote=F, sep ="\t", append = T) ;



####   write the The Node section of the mesh file

#write.table(mesh.Part2, file=paste0(Watershed.name, ".mesh"), row.names=F ,col.names=T, quote=F, sep ="\t", append = T) ;

#### Store the results to be available to other codes

save.image(file='PIHMMeshFile.RData');

