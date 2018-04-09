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

mesh.NUMELE<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,nrows=1,skip=0, colClasses =c('character', 'numeric') )[1,2]

mesh.elem.head<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,nrows=1,skip=1);

# The mesh file has the following structure: first it lists the Elements and then it lists the nodes.
# The elements part have the following structure: INDEX   NODE1   NODE2   NODE3   NABR1   NABR2   NABR3
# The nodes part has the folowing structure: INDEX   X               Y               ZMIN            ZMAX
# Zmax is the surface elevation of the node and Zmin is the bed elevation of the node

mesh.Triangles<-read.table(paste0(inputfile.name, ".MESH"), as.is=T, nrows= mesh.NUMELE, skip=2, col.names = mesh.elem.head[1,]);

head(mesh.Triangles)


mesh.NUMNODE<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,skip = mesh.NUMELE+2, nrows=1)[1,2];

mesh.Nodes.head<-read.table(paste0(inputfile.name, ".MESH"),as.is=T,skip = mesh.NUMELE+3, nrows=1);

mesh.Nodes<-read.table(paste0(inputfile.name, ".MESH"), as.is=T, skip = mesh.NUMELE+4, nrows=mesh.NUMNODE, col.names =mesh.Nodes.head[1,] );


head(mesh.Nodes)

mesh.Elements<-read.table(paste0(inputfile.name, ".MESH0"),as.is=T,skip=1, nrows=NumEle[2],col.names=c('Index', 'Node.0', 'Node.1', 'Node.2', 'Nabr.0', 'Nabr.1', 'Nabr.2'));







