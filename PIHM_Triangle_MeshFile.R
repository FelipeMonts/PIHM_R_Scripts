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


setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\Yahara20200110") ;      #  setwd(RevisedOutputs.dir)   ;



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



Watershed.TIN<-readOGR('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110_q30_a1000000.shp',  p4s='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' );

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


Watershed.1.ele<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.1.ele', header=FALSE, sep= "", skip=1) ;

names(Watershed.1.ele)<-c("triangle", "node1","node2","node3") ;

Watershed.NUMELE<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.1.ele', header=FALSE, sep= "", nrows= 1)[1,1] ;

str(Watershed.1.ele)
tail(Watershed.1.ele)
View(Watershed.1.ele)


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

Watershed.1.node<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.1.node', header=FALSE, sep= "", skip=1) ;

names(Watershed.1.node)<-c("INDEX" , "X" , "Y" , "BoundaryMarker") ;

Watershed.NUMNODE<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.1.node', header=FALSE, sep= "", nrows=1)[1,1] ;

str(Watershed.1.node)
tail(Watershed.1.node)
View(Watershed.1.node)

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


####### Read the 1.neigh file from triangle


### description from https://www.cs.cmu.edu/~quake/triangle.neigh.html

# .neigh files
# 
# First line: <# of triangles> <# of neighbors per triangle (always 3)>
#   Following lines: <triangle #> <neighbor> <neighbor> <neighbor> 
# 
# Blank lines and comments prefixed by `#' may be placed anywhere. Triangles are numbered consecutively, starting from one or zero. Neighbors are indices into the corresponding .ele file. An index of -1 indicates no neighbor (because the triangle is on an exterior boundary). The first neighbor of triangle i is opposite the first corner of triangle i, and so on.
# 
# Triangle can produce .neigh files (use the -n switch), but cannot read them. 


Watershed.1.neigh<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.1.neigh', header=FALSE, sep= "", skip=1) ;

names(Watershed.1.neigh)<-c("triangle", "neighbor1","neighbor2","neighbor3") ;

str(Watershed.1.neigh)
tail(Watershed.1.neigh)


####### Read the 1.poly file from triangle

# poly files
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


Watershed.1.poly.info<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.poly', header=FALSE, sep= "", nrows=1) ;

Watershed.1.poly.info[2,1:2]<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.poly', header=FALSE, sep= "", skip=Watershed.1.poly.info$V1+2, nrows=1) ;


Watershed.1.poly.Nodes<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.poly', header=FALSE, sep= "", skip=1, nrows=Watershed.1.poly.info$V1) ;

Watershed.1.poly.edges<-read.table('.\\Jan1020201420\\3DomainDecomposition\\MergefeaturesModExploded20200110.poly', header=FALSE, sep= "", skip=Watershed.1.poly.info$V1+3, nrows=Watershed.1.poly.info$V1) ;


View(Watershed.1.poly)



save.image(file='PIHM_Triangle_MeshFile.RData');


