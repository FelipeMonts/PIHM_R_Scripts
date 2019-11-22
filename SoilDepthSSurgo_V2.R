##############################################################################################################
# 
# 
# Program to use the information from GSSURGO to creat a soil depth layer for PIHM 
# 
# Felipe Montes 2018  02 11
# 
# Uses the following codes ; SoilsSurgoPIHM
# 
# 
############################################################################################################### 



###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

#  Set Working directory

#setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs') 

# RevisedOutputs.dir<-paste0(Project.Directory,'\\MM_PHIM_INPUTS') ;


#setwd('C:/Felipe/Students Projects/Stephanie/HalfmoonWatershed/MM_PHIM_inputs') ;   #  setwd(RevisedOutputs.dir)   ;


setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs') 

#  Windows.Directory<-gsub("\\\\", "/", readClipboard())
#  C:\Felipe\PIHM-CYCLES\PIHM\PIHM_Felipe\CNS\Manhantango\HydroTerreFullManhantango\HansYostDeepCreek\Aug2920171550

#  C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/SWATPIHMRcode 



library(Hmisc) ;
library(plyr) ;
library(dplyr)  ;
library(soilDB) ;
library(raster) ;
library(aqp) ;
library(sp) ;
library(rgdal) ;

####### Store the name of the project to read and write files more easily #############

#Project<-"MergeVectorLayer000_q25_a100000" ;


#Project<-"DataModel" ;



load('SoilsSurgoPIHM.RData');

load('FillNoDataSoils.RData');

load('PIHMMeshFile.RData') ;

#load('NetworksinR.RData') ;



# ######## Store the name of the directory whre the modified MM-PIHM inputs are to be stored
# 
# RevisedOutputs.dir<-paste0('./',Project,'/') ;
# 
# 
# # Create the path to read the input files by pasting RevisedOutputs.dir and the Project name together with the file ".name" ie ".mesh"
# 
# inputfile.name<-paste0(RevisedOutputs.dir,Project) ;




# ###########################################################################################################################
#
# ###     Prepare depth to bed rock data to be incorporated into the mesh file when a uniform soil profile depth
# ###     is not what is desired
#
# ###########################################################################################################################



###### Get the soil depth from the SoilSurgoPIHM.R file  for each triangle  #####

head(MUKEYS.INDX) ;  #from the SoilSurgoPIHM.R file 
str(MUKEYS.INDX) ;

head(MUKEYS.MAP) ;  #from the SoilSurgoPIHM.R file 
str(MUKEYS.MAP) ;

head(Mukey.Pedon)   #from the SoilSurgoPIHM.R file 

str(Mukey.Pedon)

str(Mukey.Pedon@horizons)

str(Mukey.Pedon@site)

##### convert MUKEYS.map.1$MUKEYS.mode from factor to integer ###


Element.soil.depth<-merge(MUKEYS.MAP, Mukey.Pedon@site[,c('mukey_ID', 'soil.depth')], by='mukey_ID') ;

head(Element.soil.depth)

str(Element.soil.depth)


####  Get the information related to the nodes for each triangle 


head(Watershed.1.ele)  #MM_PHIMInputsR 

str(Watershed.1.ele)

Nodes.soil.depth<-merge(Watershed.1.ele, Element.soil.depth, by.x='triangle', by.y='Ele_ID' ) ;


head(Nodes.soil.depth)

str(Nodes.soil.depth)

#### get all the nodes and it soil depth together 

Node1.soil.depth<-Nodes.soil.depth[,c('node1','soil.depth')] ;
names(Node1.soil.depth)[1]<-"node"

Node2.soil.depth<-Nodes.soil.depth[,c('node2','soil.depth')] ;
names(Node2.soil.depth)[1]<-"node"

Node3.soil.depth<-Nodes.soil.depth[,c('node3','soil.depth')] ;
names(Node3.soil.depth)[1]<-"node"



Nodes.all.soil.depth<-rbind(Node1.soil.depth, Node2.soil.depth, Node3.soil.depth) ;

Nodes.all.soil.depth$node.Factor<-as.factor(Nodes.all.soil.depth$node) ;

head(Nodes.all.soil.depth) 

str(Nodes.all.soil.depth) 

Nodes.soil.depth.avg<-aggregate(soil.depth ~ node.Factor, data=Nodes.all.soil.depth, FUN='mean') ;

str(Nodes.soil.depth.avg) 

### convert Nodes.soil.depth.avg$node.Factor from a factor to an integer to merge with the Watershed.1.node



Nodes.soil.depth.avg$node<-as.integer(Nodes.soil.depth.avg$node.Factor);

head(Nodes.soil.depth.avg)

str(Nodes.soil.depth.avg)


#### Check if there are any rows with NA in the soil depth column

anyNA(Nodes.soil.depth.avg)

###  The missing points are water bodies with mukey 539762  and would need to be corrected

### Points with Mukey 539759 are urban land Urban land-Udults complex

### Points with Mukey 539758 are Weikert and Klinesville shaly silt loams, steep


### Fill the rows with NA in the soil depth column with soil depth equal to 0.555

Nodes.soil.depth.avg[which(is.na(Nodes.soil.depth.avg$soil.depth)),c('soil.depth')]<-0.555 ;

head(Nodes.soil.depth.avg)



############    Merge with the Node.Points  file from the MM_PIHMInputsR_V3.R ##########################################




save.image(file='SoilDepthSSurgo_V2.RData');


