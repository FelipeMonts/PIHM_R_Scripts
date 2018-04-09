##############################################################################################################
# 
# 
# Program to detect Trianlges with ceontroid height  that are below their neighbors triangles centroid heights and 
# correct the heights
# 
# Felipe Montes 2018  04  08    
#
#  The program is based on the source code for PIHM on the files:
#
#  src/include pihm_const.h , line 91 #define NUM_EDGE 3
#
#  src/initialize.c,  line 274 /* Calculate Centroid 
#  elem[i].topo.zmax = (zmax[0] + zmax[1] + zmax[2]) / 3.0
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

#  Set Working directory

setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs')   

#  Windows.Directory<-gsub("\\\\", "/", readClipboard())

####### Store the name of the project to read and write files more easily #############

Project<-"MergeVectorLayer000_q25_a100000" ;


#Project<-"DataModel" ;



######## Store the name of the directory wehre the modified MM-PIHM inputs are to be stored

RevisedOutputs.dir<-paste0('./',Project,'/') ;




# Create the path to read the input files by pasting RevisedOutputs.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-paste0(RevisedOutputs.dir,Project) ;


# ****************************************READ THE MESH FILE .mesh****************************************************************


#   Because the Messh file has in the first line the number of elements (NumEle) and the number of nodes (NumNode), we can use that information to read the table more efficiently



# The mesh file has the following structure: first it lists the Elements and then it lists the nodes.
# The elements part have the following structure: INDEX   NODE1   NODE2   NODE3   NABR1   NABR2   NABR3
# The nodes part has the folowing structure: INDEX   X               Y               ZMIN            ZMAX
# Zmax is the surface elevation of the node and Zmin is the bed elevation of the node

############### Read the the trinagles nodes and neighbors  ##############

mesh.NUMELE<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,nrows=1,skip=0, colClasses =c('character', 'numeric') )[1,2]

mesh.elem.head<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,nrows=1,skip=1);


mesh.Triangles<-read.table(paste0(inputfile.name, ".MESH"), as.is=T, nrows= mesh.NUMELE, skip=2, col.names = mesh.elem.head[1,]);

head(mesh.Triangles)

############### Read the Nodes coordinates and altitude information


mesh.NUMNODE<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,skip = mesh.NUMELE+2, nrows=1)[1,2];

mesh.Nodes.head<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,skip = mesh.NUMELE+3, nrows=1);

mesh.Nodes<-read.table(paste0(inputfile.name, ".MESH"), as.is=T, skip = mesh.NUMELE+4, nrows=mesh.NUMNODE, col.names =mesh.Nodes.head[1,] );


head(mesh.Nodes)

mesh.Elements<-read.table(paste0(inputfile.name, ".MESH0"),as.is=T,skip=1, nrows=NumEle[2],col.names=c('Index', 'Node.0', 'Node.1', 'Node.2', 'Nabr.0', 'Nabr.1', 'Nabr.2'));


###############  Match the triangle nodes and neighbors to the coordinates and altitude #################

Triangles.N1<-merge(x=mesh.Triangles,y=mesh.Nodes, by.x = 'NODE1' , by.y= 'INDEX', all.x=T) ;

head(Triangles.N1)

names(Triangles.N1)[8:11]<-c( 'NODE1_X' , 'NODE1_Y', 'NODE1_ZMIN' , 'NODE1_ZMAX') ;



Triangles.N2<-merge(x=Triangles.N1,y=mesh.Nodes, by.x = 'NODE2' , by.y= 'INDEX', all.x=T) ;

head(Triangles.N2)

names(Triangles.N2)[12:15]<-c( 'NODE2_X' , 'NODE2_Y', 'NODE2_ZMIN' , 'NODE2_ZMAX') ;


Triangles.N3<-merge(x=Triangles.N2,y=mesh.Nodes, by.x = 'NODE3' , by.y= 'INDEX', all.x=T) ;

head(Triangles.N3)

names(Triangles.N3)[16:19]<-c( 'NODE3_X' , 'NODE3_Y', 'NODE3_ZMIN' , 'NODE3_ZMAX') ;

str(Triangles.N3)


################## Calculate the centroid and the average altitude of the triangle ####################

Triangles.N3$Centroid_X<-(Triangles.N3$NODE1_X + Triangles.N2$NODE2_X + Triangles.N3$NODE3_X)/3 ;


Triangles.N3$Centroid_Y<-(Triangles.N3$NODE1_Y + Triangles.N2$NODE2_Y + Triangles.N3$NODE3_Y)/3 ;


Triangles.N3$Average_ZMIN<-(Triangles.N3$NODE1_ZMIN + Triangles.N2$NODE2_ZMIN + Triangles.N3$NODE3_ZMIN)/3 ;


Triangles.N3$Average_ZMAX<-(Triangles.N3$NODE1_ZMAX + Triangles.N2$NODE2_ZMAX + Triangles.N3$NODE3_ZMAX)/3 ;

head(Triangles.N3)


################# Check if the triangles are sinks based on the altitude of their Neighboors  ############


Triangles.B0<-Triangles.N3[,c( 'INDEX' , 'NABR1' , 'NABR2' , 'NABR3' , 'Centroid_X' , 'Centroid_Y' , 'Average_ZMIN' , 'Average_ZMAX')] ;

head(Triangles.B0)

names(Triangles.B0)[5:8]<-c( "Cent_X" ,  "Cent_Y"  , "Cent_ZMIN" , "Cent_ZMAX") ;

str(Triangles.B0)

Triangles.B1<-merge(x=Triangles.B0, y=Triangles.N3[,c( 'INDEX' , 'Centroid_X' , 'Centroid_Y' , 'Average_ZMIN' , 'Average_ZMAX')], by.x= 'NABR1', by.y= 'INDEX', all.x =T ) ;
  
head(Triangles.B1) 

names(Triangles.B1)[9:12]<-c( "NABR1_X" ,  "NABR1_Y"  , "NABR1_ZMIN" , "NABR1_ZMAX") ;

str(Triangles.B1)


Triangles.B2<-merge(x=Triangles.B1, y=Triangles.N3[,c( 'INDEX' , 'Centroid_X' , 'Centroid_Y' , 'Average_ZMIN' , 'Average_ZMAX')], by.x= 'NABR2', by.y= 'INDEX' ) ;

head(Triangles.B2) 

names(Triangles.B2)[13:16]<-c( "NABR2_X" ,  "NABR2_Y"  , "NABR2_ZMIN" , "NABR2_ZMAX") ;

str(Triangles.B2)


Triangles.B3<-merge(x=Triangles.B2, y=Triangles.N3[,c( 'INDEX' , 'Centroid_X' , 'Centroid_Y' , 'Average_ZMIN' , 'Average_ZMAX')], by.x= 'NABR3', by.y= 'INDEX' ) ;

head(Triangles.B3) 

names(Triangles.B3)[17:20]<-c( "NABR3_X" ,  "NABR3_Y"  , "NABR3_ZMIN" , "NABR3_ZMAX") ;

str(Triangles.B3)


################## Find wich trinagles have a centroid that is below its neighbors  ####################


Triangles.Island<-Triangles.B3[which((Triangles.B3$Cent_ZMAX <= Triangles.B3$NABR1_ZMAX) & (Triangles.B3$Cent_ZMAX <= Triangles.B3$NABR2_ZMAX) & (Triangles.B3$Cent_ZMAX <= Triangles.B3$NABR3_ZMAX)), c( 'INDEX' , 'Cent_ZMAX' , 'NABR1_ZMAX' , 'NABR2_ZMAX' , 'NABR3_ZMAX')];



################# Remove tirangles that have an edge next to the river ###################################

##### Read the river table to get the trinagles in the river   ######


riv.NUMRIV<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,nrows=1, skip=0, colClasses =c('character', 'numeric') )[1,2] ;

riv.head<-read.table(paste0(inputfile.name, ".RIV"), as.is=T, nrows=1, skip=1);


riv<-read.table(paste0(inputfile.name, ".riv"), as.is=T, nrows= riv.NUMRIV, skip=2, col.names = riv.head[1,]);

head(riv)

###### Trinagles bordering the river ######

Triangle.Border_River<-unique(c(riv$LEFT,riv$RIGHT)) ;


###### Filter triancles that are not river borders ######

Triangles.Island[!Triangles.Island$INDEX %in%Triangle.Border_River, ]

str(Triangles.Island[!Triangles.Island$INDEX %in%Triangle.Border_River, ])









  
