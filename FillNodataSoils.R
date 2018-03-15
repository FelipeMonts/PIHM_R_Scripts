##############################################################################################################
# 
# 
# Program to fill gaps in the soils data prepared for PHIM
# 
# Felipe Montes 2018 / 02 07
# 
# Uses the following codes ; SoilsSurgoPIHM, PHIMInputs, MM_PHIMInputs, PIHM_Maps
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
#  C:\Felipe\PIHM-CYCLES\PIHM\PIHM_Felipe\CNS\Manhantango\HydroTerreFullManhantango\HansYostDeepCreek\Aug2920171550

#  C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/SWATPIHMRcode 


####### Store the name of the project to read and write files more easily #############

Project<-"MergeVectorLayer000_q25_a100000"

#Project<-"DataModel" ;



load(paste0('./',Project,'/SoilsSurgoPIHM.RData'));



######## Store the name of the directory whre the modified MM-PIHM inputs are to be stored


#dir.create(Project);


RevisedOutputs.dir<-paste0('./',Project,'/') ;




# Create the path to read the input files by pasting RevisedOutputs.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-paste0(RevisedOutputs.dir,Project) ;



##########################################################################################################################
##
## some of the dominant components Mukeys do no have data available. The strategy to fill these gaps is to get the average of the Mukeys parameters of the neighboring Triangles. The same happens with the Geology data and the same strategy can be used to complete the Geology data as well.
##
##########################################################################################################################

# Which soils have NA data

Project_Soil_NA<-data.frame(which(is.na(Project_Soil), arr.ind=T)) ;


# Which mukeys do not have data


Mukey_Gaps<-Project_Soil[unique(Project_Soil_NA$row),'MUKEY'] ;

##### Find the soil index  and the triangles that have Mukeys corresponding to the Mukey_Gaps

Mukey_Gaps_indx<-MUKEYS.map.1[MUKEYS.map.1$MUKEYS.mode %in% Mukey_Gaps, ]    ;


###### Find the neighboring triangles of the Triangles with the Mukey_Gaps 


Mukey_Gaps_indx_neighbors<-mesh.Elements[mesh.Elements$Index %in% Mukey_Gaps_indx$Ele_ID, c('Index' , 'Nabr.0' , 'Nabr.1' , 'Nabr.2')]   ;

###### retrieve the representative mukeys of the neighboring triangles for the Soil and Geology parameters

Neighbor_Mukeys<-MUKEYS.map.1[MUKEYS.map.1$Ele_ID %in% unique(unlist(Mukey_Gaps_indx_neighbors, use.names = F)), ]  ;


Neighbor_Mukeys_para.Soil<-Project_Soil[Project_Soil$MUKEY %in% Neighbor_Mukeys$MUKEYS.mode, c('MUKEY', 'SILT' , 'CLAY' , 'OM' ,  'BD') ]  ;

names(Neighbor_Mukeys_para.Soil)<-c('MUKEY', paste0('SOIL.', c('SILT' , 'CLAY' , 'OM' ,  'BD'))) ;



Neighbor_Mukeys_para.Geology<-Project_Geology[Project_Geology$MUKEY %in% Neighbor_Mukeys$MUKEYS.mode, c('MUKEY', 'SILT' , 'CLAY' , 'OM' ,  'BD') ]  ;

names(Neighbor_Mukeys_para.Geology)<-c('MUKEY', paste0('GEOL.', c('SILT' , 'CLAY' , 'OM' ,  'BD'))) ;


Mukey_Gaps_indx_neighbors.para<-merge(merge(Neighbor_Mukeys,Neighbor_Mukeys_para.Soil, by.x='MUKEYS.mode', by.y='MUKEY', all.x=T), Neighbor_Mukeys_para.Geology, by.x='MUKEYS.mode', by.y='MUKEY', all.x=T) ;


#######  Put together the representative MUKEYS parameters for Neighbouring triangles

Mukey_Gaps_Nabr.0<-merge(Mukey_Gaps_indx_neighbors[,c('Index', 'Nabr.0')],Mukey_Gaps_indx_neighbors.para, by.x='Nabr.0', by.y='Ele_ID', all.x=T, sort=F ) ;

names(Mukey_Gaps_Nabr.0)<-c(names(Mukey_Gaps_Nabr.0)[1:4], paste0('Nabr.0.',names(Mukey_Gaps_indx_neighbors.para)[4:11]))


Mukey_Gaps_Nabr.1<-merge(Mukey_Gaps_indx_neighbors[,c('Index', 'Nabr.1')],Mukey_Gaps_indx_neighbors.para, by.x='Nabr.1', by.y='Ele_ID', all.x=T , sort=F )  ;

names(Mukey_Gaps_Nabr.1)<-c(names(Mukey_Gaps_Nabr.1)[1:4], paste0('Nabr.1.',names(Mukey_Gaps_indx_neighbors.para)[4:11]))


Mukey_Gaps_Nabr.2<-merge(Mukey_Gaps_indx_neighbors[,c('Index', 'Nabr.2')],Mukey_Gaps_indx_neighbors.para, by.x='Nabr.2', by.y='Ele_ID' , all.x=T , sort=F )  ;

names(Mukey_Gaps_Nabr.2)<-c(names(Mukey_Gaps_Nabr.2)[1:4], paste0('Nabr.2.',names(Mukey_Gaps_indx_neighbors.para)[4:11]))

####  Gathering all the data of neighbors together to be ready to take the mean of the neighbor triangles 

Mukey_Gaps_All.Nabr<-merge(merge(Mukey_Gaps_Nabr.0,Mukey_Gaps_Nabr.1,by='Index'),Mukey_Gaps_Nabr.2,by='Index')  ;



######## Average the representative Mukeys properties  of the neighboring triangles for Soils

Mukey_Gaps_All.Nabr$Avg.SOIL.SILT<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.SILT', 'Nabr.1.SOIL.SILT' , 'Nabr.2.SOIL.SILT')], MARGIN=1,FUN='mean', na.rm=T) ;

Mukey_Gaps_All.Nabr$Avg.SOIL.CLAY<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.CLAY', 'Nabr.1.SOIL.CLAY' , 'Nabr.2.SOIL.CLAY')], MARGIN=1,FUN='mean', na.rm=T) ;

Mukey_Gaps_All.Nabr$Avg.SOIL.OM<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.OM', 'Nabr.1.SOIL.OM' , 'Nabr.2.SOIL.OM')], MARGIN=1,FUN='mean', na.rm=T) ;

Mukey_Gaps_All.Nabr$Avg.SOIL.BD<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.BD', 'Nabr.1.SOIL.BD' , 'Nabr.2.SOIL.BD')], MARGIN=1,FUN='mean', na.rm=T) ;


######## Average the representative Mukeys properties  of the neighboring triangles for Geology ##############

Mukey_Gaps_All.Nabr$Avg.GEOL.SILT<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.SILT', 'Nabr.1.GEOL.SILT' , 'Nabr.2.GEOL.SILT')], MARGIN=1,FUN='mean', na.rm=T) ;

Mukey_Gaps_All.Nabr$Avg.GEOL.CLAY<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.CLAY', 'Nabr.1.GEOL.CLAY' , 'Nabr.2.GEOL.CLAY')], MARGIN=1,FUN='mean', na.rm=T) ;

Mukey_Gaps_All.Nabr$Avg.GEOL.OM<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.OM', 'Nabr.1.GEOL.OM' , 'Nabr.2.GEOL.OM')], MARGIN=1,FUN='mean', na.rm=T) ;

Mukey_Gaps_All.Nabr$Avg.GEOL.BD<-apply(Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.BD', 'Nabr.1.GEOL.BD' , 'Nabr.2.GEOL.BD')], MARGIN=1,FUN='mean', na.rm=T) ;


head(Mukey_Gaps_All.Nabr) 

str(Mukey_Gaps_All.Nabr)



Mukey_Gaps_All.Nabr$New.MUKEY<--999  ;

######### in the Project_Soil data frame, find all the NA and NAN and replace them with silt=0.33, clay=0.33. OM=0.5, BD= 1.0

#Find all the NAN and NA in the Project_Soil data frame

Project_Soil.NA<-which(is.na.data.frame(Project_Soil)==T,arr.ind=T) ;

#Select wich rows have NAN and NA

Project_Soil.NA.Rows<-unique(Project_Soil.NA[,1])  ;

# replace NA and NAN in silt, clay, OM, BD with the standarized arbitrary values  with silt=0.33, clay=0.33. OM=0.5, BD= 1.0

StandardSoilValues<-data.frame(Silt=33.3, Clay=33.3, OM=0.5, BD=1.0)

Project_Soil[Project_Soil.NA.Rows, c(2,3,4,5)]<-StandardSoilValues




######## Add new rows to the Soil. file to include the new created soil parameters from neighbors and create a revised soil parameter file



# Create a new revised soil data file

Project_Soil.Rev<-Project_Soil ;

# Add the newly created data from neighbors to the soil data files 


Project_Soil.Rev[seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Mukey_Gaps_All.Nabr)[1]), c('INDEX')]<-seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Mukey_Gaps_All.Nabr)[1]) ;



Project_Soil.Rev[seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Mukey_Gaps_All.Nabr)[1]), c('MUKEY' ,'SILT' , 'CLAY' , 'OM' ,  'BD' )]<-Mukey_Gaps_All.Nabr[,c('New.MUKEY', 'Avg.SOIL.SILT', 'Avg.SOIL.CLAY' , 'Avg.SOIL.OM' , 'Avg.SOIL.BD')] ;



Project_Soil.Rev[seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Mukey_Gaps_All.Nabr)[1]),c('QTZ', 'DMAC', 'MACVF', 'MACHF', 'BETA', 'ALPHA', 'MINSMC', 'MAXSMC', 'KSATH', 'KSATV' ,'KINF')]<--999



######## Add new rows to the Geology. file to include the new created Geology parameters from neighbors and create a revised Geology parameter file

# Create a new revised Geology  data file

Project_Geology.Rev<-Project_Geology ;


# Add the newly created data from neighbors to the soil data files 


Project_Geology.Rev[seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Mukey_Gaps_All.Nabr)[1]), c('INDEX')]<-seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Mukey_Gaps_All.Nabr)[1]) ;


Project_Geology.Rev[seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Mukey_Gaps_All.Nabr)[1]), c('MUKEY' ,'SILT' , 'CLAY' , 'OM' ,  'BD' )]<-Mukey_Gaps_All.Nabr[,c('New.MUKEY', 'Avg.GEOL.SILT', 'Avg.GEOL.CLAY' , 'Avg.GEOL.OM' , 'Avg.GEOL.BD')] ;


Project_Geology.Rev[seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Mukey_Gaps_All.Nabr)[1]),c('QTZ', 'DMAC', 'MACVF', 'MACHF', 'BETA', 'ALPHA', 'MINSMC', 'MAXSMC', 'KSATH', 'KSATV' ,'KINF')]<--999



######### in the Project_Geology.Rev data frame, Fill all the NAN and NA with the no-data key -999

######### in the Project_Geology.Rev data frame, find all the NA and NAN and replace them with silt=0.33, clay=0.33. OM=0.5, BD= 1.0


#Find all the NAN and NA in the Project_Geology.Rev data frame

Project_Geology.Rev.NA<-which(is.na.data.frame(Project_Geology.Rev)==T,arr.ind=T) ;

#Select wich rows have NAN and NA

Project_Geology.Rev.NA.Rows<-unique(Project_Geology.Rev.NA[,1])  ;

# replace NA and NAN in silt, clay, OM, BD with the standarized arbitrary values  with silt=0.33, clay=0.33. OM=0.5, BD= 1.0


Project_Geology.Rev[Project_Geology.Rev.NA.Rows, c(2,3,4,5)]<-StandardSoilValues







#############################################################################################################################
#
#
####################### Write the soil and geology data in the format approptiate for PIHM to take #############################

# NUMSOIL<-data.frame(c('NUMSOIL'), dim(HansYoust_Soil)[1]) ;

NUMSOIL<-data.frame(c('NUMSOIL'), dim(Project_Soil)[1]) ;


# NUMGEOL<-data.frame(c('NUMGEOL'), dim(HansYoust_Geology)[1]) ;

NUMGEOL<-data.frame(c('NUMGEOL'), dim(Project_Geology)[1]) ;


write.table(NUMSOIL,file=paste0(inputfile.name, '_Soil.txt'), row.names=F , quote=F, sep = "\t", col.names=F) ;

# write.table(HansYoust_Soil[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Soil.txt'), row.names=F , quote=F, sep = "\t", append= T) ;

write.table(Project_Soil[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Soil.txt'), row.names=F , quote=F, sep = "\t", append= T) ;

####################  Add DINF , KMACV_RO  and KMACH_RO  at the end of the soil file ################
# DINF (type: double, unit: m) A virtual top soil layer thickness across which infiltration is calculated.
# KMACV RO (type: double, unit: dimensionless) Ratio between vertical macropore hydraulic conduc-
#   tivity and vertical saturated infiltration hydraulic conductivity.
# KMACH RO (type: double, unit: dimensionless) Ratio between horizontal macropore hydraulic con-
#   ductivity and horizontal saturated hydraulic conductivity.

DINF_etc<-data.frame(c('DINF' , 'KMACV_RO', 'KMACH_RO'), c( 0.10, 100.0 , 1000.0 )) ;

write.table(DINF_etc,file=paste0(inputfile.name, '_Soil.txt'), row.names=F , col.names=F ,quote=F, sep = "\t", append= T) ;


# NUMGEOL<-data.frame(c('NUMGEOL'), dim(HansYoust_Geology)[1]) ;

NUMGEOL<-data.frame(c('NUMGEOL'), dim(Project_Geology)[1]) ;


# write.table(HansYoust_Geology[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Geology.txt'), row.names=F , quote=F, sep = "\t", append= T) ;

write.table(Project_Geology[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Geology.txt'), row.names=F , quote=F, sep = "\t", append= T) ;


write.table(DINF_etc, file=paste0(inputfile.name, '_Geology.txt'), row.names=F , quote=F, sep = "\t", col.names=F, append= T ) ;





save.image(file=paste0('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs\\',Project,'\\FillNoDataSoils.RData'));




