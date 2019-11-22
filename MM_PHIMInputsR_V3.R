##############################################################################################################
#
#
#       Program  to generate suitable input format for MM-PIHM without any input from PIHMGIS, only based on R and QGIS
#       An update from MM_PHIMInputsR_V2.R
#
#
#       For MM_PHIM input format corresponding to  (Prerelease 0.6.0 Alpha git@github.com:PSUmodeling/MM-PIHM.git)
#
#
#  Felipe Montes 2019 /07 /03
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
#  Read the files resulting from the mesh generation using the Triangle software and create the Mesh File from the PIHM_MeshFile.R
#
#
#
######################################################################################################################################


load('PIHMMeshFile.RData')

##### Add the depth of the soil from the Soils file extracted from the GSURGO database adn calculated using the SoilDepthSSurgo_V2.R.

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



write.table(ele_and_neigh, file=paste0(Watershed.name, ".mesh"), row.names=F , col.names=T, quote=F, sep ="\t", append = T) ;




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

write.table(data.frame('NUMNODE', Watershed.NUMNODE , stringsAsFactors=F), file=paste0(Watershed.name, ".mesh"), row.names=F ,col.names=F, quote=F, sep ="\t", append = T) ;



####   write the The Node section of the mesh file

write.table(mesh.Part2, file=paste0(Watershed.name, ".mesh"), row.names=F ,col.names=T, quote=F, sep ="\t", append = T) ;





load('PIHMMeshFile.RData');

 


######################################################################################################################################
#
#
#                                  Create the river File from the TauDEM output and the Mesh files
#
#
#
######################################################################################################################################


# ########### Read River shape file (simplified and splited) information  ###########
#
#
# River.info<-ogrInfo("C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/2VectorProcessing/StreamPolyline200m_xln.shp" );
#
# River.shp<-readOGR("C:/Felipe/Students Projects/Stephanie/HalfmoonPIHM/2VectorProcessing/StreamPolyline200m_xln.shp", p4s='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' );
#
# River.shp@lines
#
# coordinates(River.shp)
#
#
# str(River.shp)
#
# plot(River.shp)
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
#
#
# head(River.FromTo)
#
#
#
#




######################################################################################################################################
# 
# 
#                                  Create the attribute File 
# 
# 
# 
######################################################################################################################################





#### import the shape files from QGIS with the LC mode from each triangle ####



########### Read infromation about the shape files ###########


Project.LC.info<-ogrInfo("../Oct0920191330/DomainDecomposition/MergeFeatures_q30_a1000000_o.shp" );


#### read the shape file that has been created in QGIS using the zonal statistics



Project.LC<-readOGR("../Oct0920191330/DomainDecomposition/MergeFeatures_q30_a1000000_o.shp",  p4s='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' );

str(Project.LC, max.level = 2) ;

plot(Project.LC) ;
####  plot(HansYoust.LC);


str(Project.LC@data) ;


#### Extract the LC index corresponding to the mode in each mesh triangle


# HansYoust.LC@data$Lc_mode<-as.factor(HansYoust.LC@data$Lc_mode) ;

Project.LC@data$LandCover.factor<-as.factor(Project.LC@data$LC_Mode_ma) ;

head(Project.LC@data)

str(Project.LC@data)


########################################################################################################################## 


# Because land cover index 11 in NLCD is open water, and index 12 is permanent snow, they should be incorporated as water/ snow bodies. To work around this limitation for small triangles or small ponds that does not contribute to the simulation result, they can be represented as emergent herbaceous wetlands, NLCD index 95 .


##########################################################################################################################

#inspect the levels of Project.LC@data$LC.index 

levels(Project.LC@data$LandCover.factor) 



# add level "95" to the list of levels for the factor LandCover.factor

levels(Project.LC@data$LandCover.factor) <-c(levels(Project.LC@data$LandCover.factor) ,"95") ;
head(Project.LC@data)


#Change Project.LC@data$LC.index that have index 11 to index 95



Project.LC@data$LandCover.factor[which(Project.LC@data$LandCover.factor == "11")]<-c("95")   ;

Project.LC@data$LandCover.factor[which(Project.LC@data$LandCover.factor == "95")] ;



### Change Project.LC@data$LandCover.factor from factor class to integer class

LC.index<-as.integer(as.character(levels(Project.LC@data$LandCover.factor)))  ;

str(LC.index)



############# Load the vegetation parameter table and the convertion parameters for PIHM - MM ################

NUMLC<-read.table("vegprmt.tbl", skip=0, as.is=T, nrows=1) ;


vegprmt.tbl<-read.table("vegprmt.tbl", skip=1, sep="", as.is=T, header=T, nrows=NUMLC[1,2]) ;

Description<-read.table("vegprmt.tbl", skip=1, sep="\t", as.is=T, header=T, nrows=NUMLC[1,2], comment.char="") ;

vegprmt.tbl$Description<-sapply(strsplit(Description[,1], "#"), "[" , 2) ;

Otherprmt.tbl<-read.table("vegprmt.tbl", skip=NUMLC[1,2]+2, sep="", as.is=T, header=F, nrows=5) ;


############# convert NLCD land cover class mapping to PIHM land cover type ############################          


NLCD_PIHM.lc<-read.table("vegprmt.tbl", skip=NUMLC[1,2]+2+6, sep= ">" , as.is=T, header=F,comment.char="") ;

NLCD.lc<-NLCD_PIHM.lc[,2];
str(NLCD_PIHM.lc[,2])

PIHM.lc<-as.integer(sapply(strsplit(NLCD_PIHM.lc[,1], split = " "), "[" , 2)) ;

str(PIHM.lc)

str(vegprmt.tbl)


PIHM_to_NLCD<-merge(data.frame(NLCD.lc, PIHM.lc), vegprmt.tbl, by.x= "NLCD.lc", by.y= "INDEX", all=T) ;

head(PIHM_to_NLCD)
str(PIHM_to_NLCD)

PIHM_to_NLCD[!is.na(PIHM_to_NLCD$PIHM.lc), ]


###### Get the soi and geology data #######################################


load('SoilsSurgoPIHM.RData');

load('FillNoDataSoils.RData') ;

###### Get the soils index that had no data and whose data was obtained from averaging the neighbor triangles (from FillNoDataSoils.RData)

Mukey_Gaps_indx_Soil

Mukey_Gaps_indx_neighbors_Soil

Index_Neighbor_Mukeys_para.Soil

Project_Soil.Rev


Mukeys_Gaps_Geology_indx

Mukey_Gaps_indx_neighbors_Geology

Index_Neighbor_Mukeys_para.Geology


Project_Geology.Rev


##### Revise the soils map according to the soils that had no data and were filled with the average of the neighbor triangles


head(MUKEYS.map.1)  #from the SoilSurgoPIHM.R file 

str(MUKEYS.map.1)

MUKEYS.map.1$INDEX<-as.integer(as.character(MUKEYS.map.1$MUKEYS.index)) ;

MUKEYS.map.1_Soil<-MUKEYS.map.1   ;


MUKEYS.map.1_Soil[MUKEYS.map.1_Soil$MUKEYS.index %in% Mukey_Gaps_indx_Soil$MUKEYS.index, 'INDEX'] <- Project_Soil.Rev[Project_Soil.Rev$MUKEY==-999,'INDEX']



##### Revise the Geology map according to the geology that had no data and were filled with the average of the neighbor triangles

MUKEYS.map.1_Geology<-MUKEYS.map.1   ;

MUKEYS.map.1_Geology[MUKEYS.map.1_Geology$MUKEYS.index %in% Mukeys_Gaps_Geology_indx$MUKEYS.index, 'INDEX'] <- Project_Geology.Rev[Project_Geology.Rev$MUKEY==-999,'INDEX']




#####   Get the land cover data   ##################################

head(Project.LC@data)

str(Project.LC@data)


Project.LC@data$LandCover<-as.integer(as.character(Project.LC@data$LandCover.factor))


head(PIHM_to_NLCD)

str(PIHM_to_NLCD)

######### Merge with the NLCD_to_PIHM data to change the NLCD LC data to the MM-PIHM Land Cover

LandCover.att<-merge(Project.LC@data,PIHM_to_NLCD, by.x= 'LandCover' ,by.y='PIHM.lc' ) ;


######## Merge the att data frame  with the Mukey.map data frame to replace the PIHM Soil index by the index in the GSSURGO extracted data

Revised.att<-merge(MUKEYS.map.1,LandCover.att, by.x='Ele_ID' , by.y= 'Ele_ID' )[] ;



head(Revised.att)
str(Revised.att)

Watershed.att<-Revised.att[,c('Ele_ID', 'INDEX','INDEX', 'NLCD.lc')];

names(Watershed.att)<-c('INDEX',  'SOIL' ,  'GEOL' , 'LC' )  ;

head(Watershed.att)
str(Watershed.att)

Watershed.att$METEO<-1 ;

Watershed.att$LAI<-Watershed.att$SS<-Watershed.att$BC1<-Watershed.att$BC2<-Watershed.att$BC3<-0 ;

head(Watershed.att)
str(Watershed.att)


Watershed.att[,c('INDEX',  'SOIL' ,  'GEOL' , 'LC', 'METEO' , 'LAI' , 'SS' , 'BC1' , 'BC2' , 'BC3' )]



# #############################################################################################################################
# 
# #   Write the .att attribute file
# 
# ###########################################################################################################################



write.table(Watershed.att[,c('INDEX',  'SOIL' ,  'GEOL' , 'LC', 'METEO' , 'LAI' , 'SS' , 'BC1' , 'BC2' , 'BC3' )], file=paste0(Watershed.name, ".att") , row.names=F, quote=F , sep = "\t" ) ;






# ################################################################################################################################
# 
# 
#           The creating the river file  from the output of the trinagulation is not jet complete  therefore in the mean time
#           the river file created from PIHM GIS is going to be used to get create the riv file
#           
#  
# 
#           
# ################################################################################################################################

PHIMGIS.RIV.NUMRIV<-read.table(file='Yahara.riv', header=F , sep="", as.is=T, nrows=1 ) ;

# PHIMGIS.RIV.names<-read.table(file='Yahara.riv',skip=1, header=F , sep="", as.is=T, nrows=1 ) ;

PHIMGIS.RIV.names<-c( "INDEX",	"FROM" ,	"TO" ,	"DOWN" ,	"LEFT" ,	"RIGHT" ,	"SHAPE" ,	 "MATL" ,	"BC" ,	"RES" );


PHIMGIS.RIV.data<-read.table(file='Yahara.riv',skip=1, header=F , sep="", as.is=T, nrows=PHIMGIS.RIV.NUMRIV[1,1] ) ;


PHIMGIS.RIV<-PHIMGIS.RIV.data[,-9] ;

names(PHIMGIS.RIV)<-PHIMGIS.RIV.names;


head(PHIMGIS.RIV)


PHIMGIS.RIV[duplicated(PHIMGIS.RIV$DOWN),]

PHIMGIS.RIV[PHIMGIS.RIV$DOWN==1,]

PHIMGIS.RIV[PHIMGIS.RIV$DOWN==59,]




PHIMGIS.RIV.Shape<-read.table(file='Yahara.riv',skip=1+PHIMGIS.RIV.NUMRIV[1,1], header=F , sep="", as.is=T, nrows=1 ) ;

PHIMGIS.RIV.Shape.data<-read.table(file='Yahara.riv',skip=1+PHIMGIS.RIV.NUMRIV[1,1]+1, header=F , sep="", as.is=T, nrows=PHIMGIS.RIV.Shape[1,2] ) ;

names(PHIMGIS.RIV.Shape.data)<-c('INDEX', 'DPTH' ,  'OINT' ,	'CWID' );


PHIMGIS.RIV.Material<-read.table(file='Yahara.riv',skip=1+PHIMGIS.RIV.NUMRIV[1,1]+1 +PHIMGIS.RIV.Shape[1,2], header=F , sep="", as.is=T, nrows=1 ) ;

PHIMGIS.RIV.Material.data<-read.table(file='Yahara.riv',skip=1+PHIMGIS.RIV.NUMRIV[1,1]+1 +PHIMGIS.RIV.Shape[1,2]+1, header=F , sep="", as.is=T, nrows=PHIMGIS.RIV.Material[1,2] ) ;

names(PHIMGIS.RIV.Material.data)<-c( 'INDEX' , 'rough' ,  'CWR' ,	'kh' ,	'kv' ,	'BEDTHCK');


PHIMGIS.RIV.IC<-read.table(file='Yahara.riv', skip=1+PHIMGIS.RIV.NUMRIV[1,1]+1 +PHIMGIS.RIV.Shape[1,2]+1+PHIMGIS.RIV.Material[1,2], header=F , sep="", as.is=T, nrows=1 ) ;


PHIMGIS.RIV.IC.data<-read.table(file='Yahara.riv', skip=1+PHIMGIS.RIV.NUMRIV[1,1]+1 +PHIMGIS.RIV.Shape[1,2]+1+PHIMGIS.RIV.Material[1,2]+1, header=F , sep="", as.is=T, nrows=PHIMGIS.RIV.IC[1,2] ) ;




# Need to add:
#    BC	0
#    RES	0
# 


# NUMRIV  101
# INDEX   FROM    TO      DOWN    LEFT    RIGHT   SHAPE   MATL    BC      RES



# 
# 
# 
# 


############### Write the river elements file .riv in the right PIHM format #######################
PHIMGIS.RIV.NUMRIV[1,2]<-c("NUMRIV") ;

write.table(PHIMGIS.RIV.NUMRIV[1,c(2,1)],file=paste0(Watershed.name, ".riv"), append=T, row.names=F , col.names=F, quote=F, sep= "\t" ) ;

write.table(PHIMGIS.RIV,file=paste0(Watershed.name, ".riv"), append=T, row.names=F ,col.names=T,  quote=F, sep= "\t" ) ;


##    Add river Shape


## write the word Shape as title before writting the table with the data


write.table(data.frame(c('SHAPE'),PHIMGIS.RIV.Shape[1,2]), file=paste0(Watershed.name, ".riv") , row.names=F , col.names=F, quote=F, append=T , sep= "\t") ;



write.table(PHIMGIS.RIV.Shape.data, file=paste0(Watershed.name, ".riv") , row.names=F , col.names=T, quote=F, append=T , sep= "\t") ;



##   Add river Material


###############################################################################################################
#                         Print the corrected  .riv file in th right format
#                         Taken and adapted from the R code MM_PHIMinputs on the PIHM_R_Scripts directory
#                         2017 10 25 By Felipe Montes
###############################################################################################################



##  Convert units of Manning's roughness coefficient [day m-1/3] , River bank hydraulic conductivity (horizontal KH) and
##   River bed hydraulic conductivity (vertical KV) [m/day] into  [s m-1/3] and [m/s] 

PHIMGIS.RIV.Material.data$ROUGH<-signif(PHIMGIS.RIV.Material.data$rough * 86400, 2) ;

PHIMGIS.RIV.Material.data$KH<-signif(PHIMGIS.RIV.Material.data$kh / 86400, 2)  ;

PHIMGIS.RIV.Material.data$KV<-signif(PHIMGIS.RIV.Material.data$kv  / 86400, 2)  ;






write.table(data.frame(c('MATERIAL'),PHIMGIS.RIV.Material[1,2]),file=paste0(Watershed.name, ".riv"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;



write.table(PHIMGIS.RIV.Material.data[,c('INDEX' , 'ROUGH' , 'CWR' , 'KH', 'KV', 'BEDTHCK')],file=paste0(Watershed.name, ".riv"), row.names=F , col.names=T, quote=F, append=T , sep = "\t") ;


##   Add boundary condition and Reservoirs


write.table(data.frame(c('BC', 'RES'),c(0,0)),file=paste0(Watershed.name, ".riv"), row.names=F , col.names=F ,quote=F, append=T, sep = "\t") ;



###############################################################################################################
#
#                         check the oulet segment "Down" column to be -3
#                         
###############################################################################################################

head(PHIMGIS.RIV)

head(Watershed.1.node)

Boundary.nodes<-Watershed.1.node[Watershed.1.node$BoundaryMarker==1,c("INDEX")] ;

PHIMGIS.RIV[PHIMGIS.RIV$TO %in% Boundary.nodes,c('DOWN')] == -3



save.image(file='MM_PHIMInputsR_V3.RData');







