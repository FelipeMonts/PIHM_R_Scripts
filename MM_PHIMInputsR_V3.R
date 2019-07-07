################### Program  to generate suitable input format for MM-PIHM without any input from PIHMGIS, only based on R and QGIS
################### An update from MM_PHIMInputsR_V2.R
################### Felipe Montes
################### 2019 07 03

##################  For MM_PHIM input format corresponding to  (Prerelease 0.6.0 Alpha git@github.com:PSUmodeling/MM-PIHM.git) 





#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;



#      set the working directory



setwd('C:/Felipe/Students Projects/Stephanie/HalfmoonWatershed/MM_PHIM_inputs') ;


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
#  Read the files resulting from the mesh generation using the Trianlge software and create the Mesh File
# 
# 
# 
######################################################################################################################################

########### Read TIN shape file infromation  ###########

Watershed.TIN.info<-ogrInfo("C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/Jun2420191944/3DomainDecomposition/MergeVectorLayer200_q25_250_3.shp", layer='MergeVectorLayer200_q25_250_3' );

Watershed.TIN<-readOGR("C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/Jun2420191944/3DomainDecomposition/MergeVectorLayer200_q25_250_3.shp", layer='MergeVectorLayer200_q25_250_3', p4s='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' );

print(Watershed.TIN)


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


Watershed.1.ele<-read.table('C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/Jun2420191944/3DomainDecomposition/MergeVectorLayer200.ele', header=FALSE, sep= "", skip=1) ;

names(Watershed.1.ele)<-c("triangle", "node1","node2","node3") ;

Watershed.NUMELE<-read.table('C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/Jun2420191944/3DomainDecomposition/MergeVectorLayer200.ele', header=FALSE, sep= "", nrows= 1)[1,1] ;

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

Watershed.1.node<-read.table('C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/Jun2420191944/3DomainDecomposition/MergeVectorLayer200.node', header=FALSE, sep= "", skip=1) ;

names(Watershed.1.node)<-c("INDEX" , "X" , "Y" , "BoundaryMarker") ;

Watershed.NUMNODE<-read.table('C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/Jun2420191944/3DomainDecomposition/MergeVectorLayer200.node', header=FALSE, sep= "", nrows=1)[1,1] ;

str(Watershed.1.node)
tail(Watershed.1.node)


####### Create a point shape file from the nodes coordinates to extract the ZMAX Values from the DEM


Node.Points<-SpatialPointsDataFrame(coords=Watershed.1.node[,c("X", "Y")],  proj4string=CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'), data=Watershed.1.node, coords.nrs = c(2,3), match.ID = T);


####### Load the DEM from the fill pits file of the Reater processing directory


FillPits<-raster("C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/1RasterProcessing/FillPitGrid.asc", crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') 




####### Extract the elevation data for each node


Node.Points.ZMAX<-data.frame(coordinates(Node.Points),data=Node.Points, raster::extract(FillPits,Node.Points));

names(Node.Points.ZMAX)

str(Node.Points.ZMAX)




###### Add the depth of the soil from the Soils file extracted from the GSURGO database.









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


Watershed.1.neigh<-read.table('C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/Jun2420191944/3DomainDecomposition/MergeVectorLayer200.neigh', header=FALSE, sep= "", skip=1) ;

names(Watershed.1.neigh)<-c("triangle", "neighbor1","neighbor2","neighbor3") ;

str(Watershed.1.neigh)
tail(Watershed.1.neigh)


########## Create The Triangle section of the mesh file

mesh.Part1<-merge(Watershed.1.ele,Watershed.1.neigh, by=c('triangle')) ;

names(mesh.Part1)<-c('INDEX' , 'NODE1' , 'NODE2' , 'NODE3' , 'NABR1' , 'NABR2' , 'NABR3') ;



########## Create The Node section of the mesh file

mesh.Part2<-Node.Points.ZMAX[,c('data.INDEX', 'X', 'Y', 'raster..extract.FillPits..Node.Points.')] ;

names(mesh.Part2)<-c('INDEX' , 'X' , 'Y' ,  'ZMAX') ; ### Add the ZMIN name when the information comes from Soils

head(mesh.Part2)





######################################################################################################################################
# 
# 
#                                  Create the river File from the TauDEM aoutput and the Mesh files
# 
# 
# 
######################################################################################################################################


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
















MergedRiver.coords.df<-data.frame(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)]);

names(MergedRiver.coords.df)<-c('P1.X', 'P1.Y', 'P2.X', 'P2.Y') ;

#addd a line ID to the data frame to be able to differentiate line components 

#Point.coords.df$Line<-sapply(slot(HYDC,"lines"), function(x) slot(x,"ID")) ;

MergedRiver.coords.df$Line.ID<-as.character(seq(1:MergedRiver.info$nrows)) ;

# stack the points of all the lines obtained in matrix from to remove repeated points ( there are many, all the lines that are contiguous share common points) and assign unique points a unique point ID

MergedRiver.Stacked.Point.coords<-rbind(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)])   ;

head(MergedRiver.Stacked.Point.coords) ;

str(MergedRiver.Stacked.Point.coords) ;

















Project<-"MergeVectorLayer000_q25_a100000" ;

load(paste0('./',Project,'/PIHMInputsR.RData'));

load(paste0('./',Project,'/SoilsSurgoPIHM.RData'));


load(paste0('./',Project,'/FillNoDataSoils.RData'));

#load(paste0('./',Project,'/NetworksinR.RData'));

load(paste0('./',Project,'/SoilDepthSSurgo.RData'));

## attach('./PIHMInputsR.RData', ); Adds the database with the objects created to the path R searches for objects. It is safer than load, but one needs to remember the name of the variables when programming. 


####### Store the name of the project to read and write files more easily #############

# Project<-"MergeVectorLayer000_q25_a100000"   ;

#Project<-"DataModel" ;

###




######## Store the name of the directory whre the modified MM-PIHM inputs are to be stored


#dir.create(Project);


RevisedOutputs.dir<-paste0('./',Project,'/') ;




# Create the path to read the input files by pasting RevisedOutputs.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-paste0(RevisedOutputs.dir,Project) ;

################## Write out the appropiate formated "Mesh" File for the MM-PIHM input format ##################################
##       First, create the mesh element part

head(mesh.Elements)

MESH.1<-data.frame(c('NUMELE'),NumEle);

## write the first lines of the new MM-PIHM mesh file

write.table(MESH.1, file=paste0(inputfile.name, ".MESH"), row.names=F ,col.names=F, quote=F, sep ="\t") ;


MESH.2<-data.frame(c('INDEX') ,c('NODE1') , c('NODE2') , c('NODE3'), c('NABR1') , c('NABR2') ,c('NABR3')) ;

write.table(MESH.2[1,], file=paste0(inputfile.name, ".MESH"), row.names=F , col.names=F, quote=F, sep ="\t", append = T) ;

## write the mesh data into the new MM-PIHM mesh file

write.table(mesh.Elements, file=paste0(inputfile.name, ".MESH"), row.names=F , col.names=F, quote=F, sep ="\t", append = T) ;


head(mesh.Elements)


##       Second Create the node elements part 
### write the first lines of the node elements

head(mesh.Nodes)
str(mesh.Nodes)

NumEle
NumNode

head(mesh.Nodes)
str(mesh.Nodes)

head(Rev.mesh.Nodes.SSURGO)
str(Rev.mesh.Nodes.SSURGO)


Rev.mesh.Nodes<-merge(x=mesh.Nodes,y=Rev.mesh.Nodes.SSURGO, by=c( 'X', 'Y' , 'Index' , 'Zmin'), all.x=T, all.y=F, sort=F ) ;

head(Rev.mesh.Nodes)
str(Rev.mesh.Nodes)

Rev.mesh.Nodes<-Rev.mesh.Nodes[order(Rev.mesh.Nodes$Index),c('Index' , 'X' , 'Y', 'Zmin.SSURGO' , 'Zmax.Riv.Corr.x')] ;


#New.mesh.Nodes[401:600,]
head(Rev.mesh.Nodes)

######## Write the mesh file ###########################################################################################

NODES.1<-data.frame(c('NUMNODE'),NumNode )   ;
write.table(NODES.1 , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=F, quote=F, sep ="\t") ;


header.mesh.Nodes<-c('INDEX' , 'X' , 'Y' , 'ZMIN' , 'ZMAX');

#write.table(New.mesh.Nodes , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


write.table(Rev.mesh.Nodes[,c('Index' , 'X' , 'Y', 'Zmin.SSURGO' , 'Zmax.Riv.Corr.x')] , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


###################   Write the appropiate formated "Attributes" File for the MM-PIHM input format  #################################

#### import the shape files from QGIS with the LC mode from each triangle ####

########### Read infromation about the shape files ###########

# HansYoust.LC.info<-ogrInfo("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Landcover/LC_Stat.shp");

Project.LC.info<-ogrInfo("C:/Aun Trabajo en Proceso/HansYostDeepCreek/DomainDecomposition2.shp");


#### read the shape file that has been created in QGIS using the zonal statistics

# HansYoust.LC<-readOGR("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Landcover/LC_Stat.shp")  ;

Project.LC<-readOGR("C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/3DomainDecomposition/MergeVectorLayer000_q25_a100000.shp");

str(Project.LC, max.level = 2) ;

plot(Project.LC) ;
####  plot(HansYoust.LC);


str(Project.LC@data) ;


#### Extract the LC index corresponding to the mode in each mesh triangle


# HansYoust.LC@data$Lc_mode<-as.factor(HansYoust.LC@data$Lc_mode) ;

Project.LC@data$LC_majorit<-as.factor(Project.LC@data$HY_LC_majo) ;

# LC.indexs<-as.integer(levels(HansYoust.LC@data$Lc_mode))  ;

LC.indexs<-as.integer(levels(Project.LC@data$LC_majorit))  ;

str(LC.indexs)

# HansYoust.LC@data$LC.index<-HansYoust.LC@data$Lc_mode ;

Project.LC@data$LC.index<-Project.LC@data$LC_majorit


head(Project.LC@data$LC.index)


########################################################################################################################## 


# Because land cover index 11 in NLCD is open water, and index 12 is permanent snow, they should be incorporated as water/ snow bodies. To work around this limitation for small triangles or small ponds that does not contribute to the simulation result, they can be represented as emergent herbaceous wetlands, NLCD index 95 .


##########################################################################################################################



#Change Project.LC@data$LC.index that have index 11 to index 95

#inspect the levels of Project.LC@data$LC.index 

levels(Project.LC@data$LC.index)<-c(levels(Project.LC@data$LC_majorit),"95")
head(Project.LC@data)

Project.LC@data$LC.index[which(Project.LC@data$LC.index == "11")]<-c("95")   ;

Project.LC@data$LC.index[which(Project.LC@data$LC.index == "95")] ;

######  Merge the LC index with the rest of the attribute table

######### Load the attribute file to change the LC codes from PIHM GIS to the mode from spatial statistics in QGIS

att<-read.table(paste0(Project.Directory,"\\",DataModel.dir,"\\",Project,".att"),as.is=T,col.names=c('Index', 'Soil', 'Geol','LC','IS_IC', 'Snw_IC', 'Srf_IC', 'Ust_IC', 'St_IC', 'Ppt', 'Tmp', 'RH', 'Wnd', 'Rn', 'G', 'VP', 'S', 'mF', 'BC.0', 'BC.1', 'BC.2', 'mP'));

names(att) ;


###### and Alternative to bypass the creation of the att table is to create it here by loading the soils and geology indicess and start the att table from there

att<-Project.GSSURGO@data[,c("Ele_ID" ,"MUKEYS.index","MUKEYS.index")]
names(att)<-c('Index', 'Soil', 'Geol')
str(att)


att.expanded.1<-merge(att,Project.LC@data, by.x='Index' , by.y='Ele_ID') ;

head(att.expanded.1)
str(att.expanded.1)

############# Load the vegetation parameter table and the convertion parameters for PIHM - MM ################

NUMLC<-read.table("./vegprmt.tbl", skip=0, as.is=T, nrows=1) ;


vegprmt.tbl<-read.table("./vegprmt.tbl", skip=1, sep="", as.is=T, header=T, nrows=NUMLC[1,2]) ;

Description<-read.table("./vegprmt.tbl", skip=1, sep="\t", as.is=T, header=T, nrows=NUMLC[1,2], comment.char="") ;

vegprmt.tbl$Description<-sapply(strsplit(Description[,1], "#"), "[" , 2) ;

Otherprmt.tbl<-read.table("./vegprmt.tbl", skip=NUMLC[1,2]+2, sep="", as.is=T, header=F, nrows=5) ;



############# Load the vegetation parameter map from the NLCD to the MM-PIHM Land Cover type 
############# "NLCD land cover class mapping to PIHM land cover type ############################          


NLCD_PIHM.lc<-read.table("./vegprmt.tbl", skip=NUMLC[1,2]+10, sep= ">" , as.is=T, header=F,comment.char="") ;

NLCD.lc<-NLCD_PIHM.lc[,2];

PIHM.lc<-as.integer(sapply(strsplit(NLCD_PIHM.lc[,1], split = " "), "[" , 2)) ;

PIHM_to_NLCD<-merge(data.frame(NLCD.lc, PIHM.lc), vegprmt.tbl, by.x= "NLCD.lc", by.y= "INDEX", all=T) ;

head(PIHM_to_NLCD)

PIHM_to_NLCD[!is.na(PIHM_to_NLCD$PIHM.lc), ]



######### MErge with the NLCD_to_PIHM data to change the NLCD LC data to the MM-PIHM Land Cover


att.expanded.2<-merge(att.expanded.1,PIHM_to_NLCD, by.x="LC.index", by.y="PIHM.lc", all.x=T ) ;
head(att.expanded.2)

###### change the name of the LC column that will be used in the revised attributes

revised.names<-names(att)   ;

revised.names[4]<- "LC" ;


######## Merge the att data frame  with the Mukey.map data frame to replace the PIHM Soil index by the index in the GSSURGO extracted data

att.expanded.3<-merge(att.expanded.2,MUKEYS.map.1, by.x='Index', by.y='Ele_ID') ;


Revised.att<-att.expanded.3[order(att.expanded.3$Index),] ;

head(Revised.att)



Revised.att$METEO<-1 ;

Revised.att$LAI<-Revised.att$SS<-Revised.att$BC0<-Revised.att$BC1<-Revised.att$BC2<-0 ;

head(Revised.att)


# #############################################################################################################################
# 
# #   Still need to correct the attribute tables with the correct soil index in the triangles that do have MUKEYs Gaps
# 
# ###########################################################################################################################

Revised.att[, 'MUKEYS.index']<-as.numeric(Revised.att[, 'MUKEYS.index']) ;

# Revised.att[Revised.att$Index %in% Mukey_Gaps_indx[,'Ele_ID'],] [2,'MUKEYS.index']<-dim(HansYoust_Soil)[1]  ;

Revised.att[Revised.att$Index %in% Mukey_Gaps_indx[,'Ele_ID'],] [, 'MUKEYS.index']<-seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Mukey_Gaps_All.Nabr)[1]) ;



write.table(Revised.att[,c('Index', 'MUKEYS.index', 'MUKEYS.index', 'NLCD.lc','METEO', 'LAI','SS', 'BC0', 'BC1', 'BC2')], file=paste0(inputfile.name, '.ATT') , row.names=F, col.names=c('INDEX' , 'SOIL' , 'GEOL' ,	'LC' ,	'METEO' ,	'LAI',	'SS' ,	'BC0' ,	'BC1' ,	'BC2'), quote=F , sep = "\t" ) ;

#write.table(Revised.att[,c('Index' , 'Soil' , 'Geol', 'NLCD.lc', 'METEO',	'LAI',	'SS' ,	'BC0' ,	'BC1' ,	'BC2')], file=paste0(inputfile.name, '.ATT') , row.names=F, col.names=c('INDEX' , 'SOIL' , 'GEOL' ,	'LC' ,	'METEO' ,	'LAI',	'SS' ,	'BC0' ,	'BC1' ,	'BC2'), quote=F , sep = "\t" ) ;


###################   Write the appropiate formated "River" File for the MM-PIHM input format  #################################

### Write the First line of the .Riv File

write.table(data.frame(c('NUMRIV'),NumRiv ),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F, quote=F, sep= "\t" ) ;


##   Add river elements
names(riv.elements)<-c( 'INDEX', 'FROM' , 'TO' ,  'DOWN' , 	'LEFT' , 	'RIGHT' , 	'SHAPE' ,	'MATL' ,	'IC' ,	'BC' ,	'RES' )  ;

###########################################################################################################################

# Writing code to make sure all the river segments have positive slope


###########################################################################################################################




############  Check river elements for differences in height and flow patterns #####################

#select the nodes that are both in the mesh and in the river segments
#select first the unique nodes in the river

River.Nodes<-unique(c(riv.elements$FromNode, riv.elements$ToNode))  ;

head(River.Nodes)
str(River.Nodes)

# from the mesh file select the nodes that belong to the river

# New.River.Nodes.Elevation<-River.Nodes.Elevation.Corrected[River.Nodes.Elevation.Corrected$Index %in% River.Nodes, ]
River.Nodes.Elevation<-mesh.Nodes[mesh.Nodes$Index %in% River.Nodes, ] ;
#River.Nodes.Elevation<-mesh.Nodes.corrected[mesh.Nodes.corrected$Index %in% River.Nodes, ] ;



# head(New.River.Nodes.Elevation)
# str(New.River.Nodes.Elevation)

head(River.Nodes.Elevation)
str(River.Nodes.Elevation)

# connect the River nodes with the corresponding information in the mesh file

# New.River.Nodes.Elevation.FROM<-merge(riv.elements,New.River.Nodes.Elevation, by.x='FromNode' , by.y='Index', all.x=T, sort=F) ;

head(riv.elements)
str(riv.elements) 

River.Nodes.Elevation.FROM<-merge(riv.elements,Rev.mesh.Nodes.SSURGO, by.x='FROM' , by.y='Index', all.x=T, sort=F) ;


# head(New.River.Nodes.Elevation.FROM,50)
# str(New.River.Nodes.Elevation.FROM)


head(River.Nodes.Elevation.FROM)
str(River.Nodes.Elevation.FROM)


# New.River.Nodes.Elevation.TO<-merge(riv.elements,New.River.Nodes.Elevation, by.x='ToNode' , by.y='Index', all.x=T,sort=F) ;

River.Nodes.Elevation.TO<-merge(riv.elements,Rev.mesh.Nodes.SSURGO, by.x='FROM' , by.y='Index', all.x=T,sort=F) ;


# head(New.River.Nodes.Elevation.TO,50)
# str(New.River.Nodes.Elevation.TO)




head(River.Nodes.Elevation.TO)
str(River.Nodes.Elevation.TO)

#calculate the difference in elevation between the FROM and TO river nodes. Zmax is the surface elevation, Zmin is the bed rock elevation

# New.River.Nodes.Elevation.FROM_TO<-merge(New.River.Nodes.Elevation.FROM,New.River.Nodes.Elevation.TO,by='Index', sort=T) ;

#head(New.River.Nodes.Elevation.FROM_TO)


#New.River.Nodes.Elevation.FROM_TO$Max_Elev_Dif<-New.River.Nodes.Elevation.FROM_TO$Zmax.Correct.x - New.River.Nodes.Elevation.FROM_TO$Zmax.Correct.y

#head(New.River.Nodes.Elevation.FROM_TO,50)

River.Nodes.Max_Elev_Dif<-River.Nodes.Elevation.FROM$Zmax - River.Nodes.Elevation.TO$Zmax ;

# head(New.River.Nodes.Max_Elev_Dif,100)
# str(New.River.Nodes.Max_Elev_Dif)


head(River.Nodes.Max_Elev_Dif)
str(River.Nodes.Max_Elev_Dif)



# Explore the River nodes and segments

# plot(New.River.Nodes.Elevation.FROM[,c("INDEX")],New.River.Nodes.Max_Elev_Dif, col="BLUE", ylim=c(-10,10)) ;
# points(New.River.Nodes.Elevation.TO[,c("INDEX")],New.River.Nodes.Max_Elev_Dif, col="RED" ) ;


plot(River.Nodes.Elevation.FROM[,c('INDEX')],River.Nodes.Max_Elev_Dif, col="BLUE") ;
points(River.Nodes.Elevation.TO[,c('INDEX')],River.Nodes.Max_Elev_Dif, col="RED" ) ;


# New.River.Nodes.Elevation.FROM[which(New.River.Nodes.Max_Elev_Dif < 0), c("INDEX")] ;
# 
# New.River.Nodes.Elevation.TO[which(New.River.Nodes.Max_Elev_Dif < 0), c("INDEX")]  ;
# 


River.Nodes.Elevation.FROM[which(River.Nodes.Max_Elev_Dif < 0), c('INDEX')] ;

#River.Nodes.Elevation.FROM[which(River.Nodes.Max_Elev_Dif < 0), ] ;

River.Nodes.Elevation.TO[which(River.Nodes.Max_Elev_Dif < 0), c('INDEX')]  ;



# New.River.Nodes.Elevation.FROM$Max_Elev_Dif<-New.River.Nodes.Max_Elev_Dif ;
# 
# 
# plot(New.River.Nodes.Elevation.FROM$INDEX,New.River.Nodes.Elevation.FROM$Max_Elev_Dif)
# 



River.Nodes.Elevation.FROM$Max_Elev_Dif<-River.Nodes.Max_Elev_Dif ;


plot(River.Nodes.Elevation.FROM$INDEX,River.Nodes.Elevation.FROM$Max_Elev_Dif)


head(River.Nodes.Elevation.FROM)
str(River.Nodes.Elevation.FROM)

head(riv.elements)



############### Write the river elements file .riv in the right PIHM format #######################



write.table(riv.elements[,c( 'INDEX', 'FROM' , 'TO' ,  'DOWN' , 	'LEFT' , 	'RIGHT' , 	'SHAPE' ,	'MATL' ,	'BC' ,	'RES' )],file=paste0(inputfile.name, ".RIV"), append=T, row.names=F , quote=F, sep= "\t" ) ;


##    Add river Shape


## write the word Shape as title before writting the table with the data


write.table(data.frame(c('SHAPE'),NumShape),file=paste0(inputfile.name, ".RIV") , row.names=F , col.names=F, quote=F, append=T , sep= "\t") ;


header.riv.Shape<-c('INDEX', 'DPTH' ,  'OINT' ,	'CWID' );

write.table(riv.shape,file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=header.riv.Shape, quote=F, append=T, sep = "\t") ;


##   Add river Material



write.table(data.frame(c('MATERIAL'),NumMat ),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;




###############################################################################################################
#                         Print the corrected  .riv file in th right format
#                         Taken and adapted from the R code MM_PHIMinputs on the PIHM_R_Scripts directory
#                         2017 10 25 By Felipe Montes
###############################################################################################################



##  Convert units of Manning's roughness coefficient [day m-1/3] , River bank hydraulic conductivity (horizontal KH) and
##   River bed hydraulic conductivity (vertical KV) [m/day] into  [s m-1/3] and [m/s] 

riv.material$ROUGH<-signif(riv.material$n * 86400, 2) ;

riv.material$KH<-signif(riv.material$KsatH / 86400, 2)  ;

riv.material$KV<-signif(riv.material$KsatV / 86400, 2)  ;


header.riv.Material<-c( 'INDEX' , 'ROUGH' ,  'CWR' ,	'KH' ,	'KV' ,	'BEDTHCK');


write.table(riv.material[,c('Index','ROUGH' ,  'Cwr' ,	'KH' ,	'KV' ,	'Bed')],file=paste0(inputfile.name, ".RIV") , row.names=F , col.names=header.riv.Material, quote=F, append=T , sep = "\t") ;

##   Add boundary condition


write.table(data.frame(c('BC'),BC[2]),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F ,quote=F, append=T, sep = "\t") ;


##   Add Reservoirs

write.table(data.frame(c('RES'),Res[2]),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;



save.image(file=paste0('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs\\',Project,'\\MM_PHIMInputsR_V2.RData'));







