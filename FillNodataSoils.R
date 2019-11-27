##############################################################################################################
# 
# 
#       Program to fill gaps in the soils data prepared for PHIM
#       
#        Uses the following codes ; SoilsSurgoPIHM, PHIMInputs, MM_PHIMInputs, PIHM_Maps
# 
#       
# 
# 
#  Felipe Montes 2018 / 02 07
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
#                             Setting up working directory                        
###############################################################################################################


#      set the working directory


setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs') ;       #  setwd(RevisedOutputs.dir)   ;



###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


# Install the packages that are needed #

# install.packages('ggplot2', dep=TRUE)
# install.packages('base64enc' , dep=TRUE)
# install.packages("raster", dep = TRUE)
# install.packages('plyr', dep=TRUE)
# install.packages('Hmisc', dep=TRUE)
# install.packages('soilDB', dep=TRUE) # stable version from CRAN + dependencies
# install.packages("soilDB", repos="http://R-Forge.R-project.org") # most recent copy from r-forge
# install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source") # SSOAP and XMLSchema




# install.packages("foreign")
# install.packages("httr", dep=TRUE)
# install.packages("rgdal", dep = TRUE)

# install.packages("rgeos", dep = TRUE)
# install.packages("RColorBrewer")
# install.packages("latticeExtra")
# install.packages("reshape")
# install.packages("dplyr", dep=TRUE)
# install.packages("aqp", dep=TRUE)





###############################################################################################################
#                           load the libraries that are neded   
###############################################################################################################


### Load the data obtained with the SoilsSurgoPIHM 


load('SoilsSurgoPIHM.RData');

### ### Load the data obtained with the MM_PIHMInputsR_V3.R

#load('MM_PHIMInputsR_V3.RData');

load('PIHMMeshFile.RData');

##########################################################################################################################
##
## some of the dominant components Mukeys do no have data available. The strategy to fill these gaps is to get the average of the Mukeys parameters of the neighboring Triangles. The same happens with the Geology data and the same strategy can be used to complete the Geology data as well.
##
##########################################################################################################################

# Which soils have NA data

Project_Soil_NA<-data.frame(which(is.na(Project_Soil), arr.ind=T)) ;


# Which mukeys do have NA data


Mukey_Gaps_Soil.1<-Project_Soil[unique(Project_Soil_NA$row),'MUKEY'] ;


# Which soils have NAN data

Project_Soil_NAN<-data.frame(which(is.nan(as.matrix(Project_Soil[,-c(1, 6)])),arr.ind=T)) ;


# Which mukeys do have NAN data

Mukey_Gaps_Soil.2<-Project_Soil[unique(Project_Soil_NAN$row),'MUKEY'] ;


# Which soils have zero "0" data

Project_Soil_Zero<-data.frame(which(as.matrix(Project_Soil[,-c(1, 6)])==0, arr.ind=T)) ;

# Which mukeys do zero "0" data

Mukey_Gaps_Soil.3<-Project_Soil[unique(Project_Soil_Zero$row),'MUKEY'] ;


# All the mukeys that have defective data (Na, NAN , 0.000)

Mukey_Gaps_Soil<-unique(c(Mukey_Gaps_Soil.1, Mukey_Gaps_Soil.2, Mukey_Gaps_Soil.3 )) ;



# Complementary information about the soils that have defective data


Mukey.Pedon@horizons[Mukey.Pedon@horizons$mukey %in% Mukey_Gaps_Soil,]


##### Find the soil index  and the triangles that have Mukeys corresponding to the Mukey_Gaps

Mukey_Gaps_indx_Soil<-MUKEYS.MAP[MUKEYS.MAP$MUKEYS.mode %in% Mukey_Gaps_Soil, ]    ;



#####  Only triangles with the Pits mukey  1588007 will be filled with data from adjacent triangles. Data for Traingles with mukeys 423299 Marsh and 753456 Alluvial land, wet will be filled with literature values.
str(Mukey_Gaps_Soil)



Mukey_Gaps_Pits<-1588007

Mukey_Gaps_indx_Soil<-MUKEYS.MAP[MUKEYS.MAP$MUKEYS.mode %in% Mukey_Gaps_Pits, ] 


###### Find the neighboring triangles of the Triangles with the Mukey_Gaps 


Mukey_Gaps_indx_neighbors_Soil<-Watershed.1.neigh[Watershed.1.neigh$triangle %in% Mukey_Gaps_indx_Soil$Ele_ID, ] ; #c( "triangle" , "neighbor1" , "neighbor2" , "neighbor3")]   ;

###### retrieve the representative mukeys of the neighboring triangles for the Soil  parameters


Neighbor_Mukeys_Soil<-MUKEYS.MAP[MUKEYS.MAP$Ele_ID %in% unique(unlist(Mukey_Gaps_indx_neighbors_Soil, use.names = F)), ]  ;

###### The nieghbors trinagles of the trinagles with the pits mukey have mukeys corresponding to Map Unit No.753522 Houghton Muck Peatlands with no reliable data. Therefore there is no need to continue the soils fill procedure



Neighbor_Mukeys_para.Soil<-Project_Soil[Project_Soil$MUKEY %in% Neighbor_Mukeys_Soil$MUKEYS.mode, c('MUKEY', 'SILT' , 'CLAY' , 'OM' ,  'BD') ]  ;

names(Neighbor_Mukeys_para.Soil)<-c('MUKEY', paste0('SOIL.', c('SILT' , 'CLAY' , 'OM' ,  'BD'))) ;



Index_Neighbor_Mukeys_para.Soil<-merge(Neighbor_Mukeys_Soil,Neighbor_Mukeys_para.Soil, by.x='MUKEYS.mode', by.y='MUKEY', all.x=T) ;



#######  Put together the representative MUKEYS parameters for Neighbouring triangles

Soil_Mukey_Gaps_Nabr.0<-merge(Mukey_Gaps_indx_neighbors_Soil[,c("triangle" , "neighbor1")],Index_Neighbor_Mukeys_para.Soil, by.x="neighbor1", by.y='Ele_ID', all.x=T, sort=F ) ;

names(Soil_Mukey_Gaps_Nabr.0)<-c(names(Soil_Mukey_Gaps_Nabr.0)[1:4], paste0('Nabr.0.',names(Index_Neighbor_Mukeys_para.Soil)[4:7])) ;


Soil_Mukey_Gaps_Nabr.1<-merge(Mukey_Gaps_indx_neighbors_Soil[,c("triangle" , "neighbor2")], Index_Neighbor_Mukeys_para.Soil, by.x="neighbor2", by.y='Ele_ID', all.x=T , sort=F )  ;

names(Soil_Mukey_Gaps_Nabr.1)<-c(names(Soil_Mukey_Gaps_Nabr.1)[1:4], paste0("Nabr.1.",names(Index_Neighbor_Mukeys_para.Soil)[4:7])) ;


Soil_Mukey_Gaps_Nabr.2<-merge(Mukey_Gaps_indx_neighbors_Soil[,c("triangle", "neighbor3")],Index_Neighbor_Mukeys_para.Soil, by.x="neighbor3", by.y='Ele_ID' , all.x=T , sort=F )  ;

names(Soil_Mukey_Gaps_Nabr.2)<-c(names(Soil_Mukey_Gaps_Nabr.2)[1:4], paste0("Nabr.2.",names(Index_Neighbor_Mukeys_para.Soil)[4:7])) ;

####  Gathering all the data of neighbors together to be ready to take the mean of the neighbor triangles 

Soil_Mukey_Gaps_All.Nabr<-merge(merge(Soil_Mukey_Gaps_Nabr.0,Soil_Mukey_Gaps_Nabr.1,by="triangle"),Soil_Mukey_Gaps_Nabr.2,by="triangle")  ;
names(Soil_Mukey_Gaps_All.Nabr)




######## Average the representative Mukeys properties  of the neighboring triangles for Soils, removing NA, NAN and 0 values 

#change all zero values to NA

Soil_Mukey_Gaps_All.Nabr[Soil_Mukey_Gaps_All.Nabr==0]<-NA ;

#Calculate the means without accounting for NA values

Soil_Mukey_Gaps_All.Nabr$Avg.SOIL.SILT<-apply(Soil_Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.SILT', 'Nabr.1.SOIL.SILT' , 'Nabr.2.SOIL.SILT')], MARGIN=1,FUN='mean', na.rm=T) ;

Soil_Mukey_Gaps_All.Nabr$Avg.SOIL.CLAY<-apply(Soil_Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.CLAY', 'Nabr.1.SOIL.CLAY' , 'Nabr.2.SOIL.CLAY')], MARGIN=1,FUN='mean', na.rm=T) ;

Soil_Mukey_Gaps_All.Nabr$Avg.SOIL.OM<-apply(Soil_Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.OM', 'Nabr.1.SOIL.OM' , 'Nabr.2.SOIL.OM')], MARGIN=1,FUN='mean', na.rm=T) ;

Soil_Mukey_Gaps_All.Nabr$Avg.SOIL.BD<-apply(Soil_Mukey_Gaps_All.Nabr[, c('Nabr.0.SOIL.BD', 'Nabr.1.SOIL.BD' , 'Nabr.2.SOIL.BD')], MARGIN=1,FUN='mean', na.rm=T) ;



Soil_Mukey_Gaps_All.Nabr$New.MUKEY<--999  ;


######## Add new rows to the Soil. file to include the new created soil parameters from neighbors and create a revised soil parameter file



# Create a new revised soil data file

Project_Soil.Rev<-Project_Soil ;

View(Project_Soil.Rev)

# # Add the newly created data from neighbors to the soil data files 
# 
# 
# Project_Soil.Rev[seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Soil_Mukey_Gaps_All.Nabr)[1]), c('INDEX')]<-seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Soil_Mukey_Gaps_All.Nabr)[1]) ;
# 
# 
# 
# Project_Soil.Rev[seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Soil_Mukey_Gaps_All.Nabr)[1]), c('MUKEY' ,'SILT' , 'CLAY' , 'OM' ,  'BD' )]<-Soil_Mukey_Gaps_All.Nabr[,c('New.MUKEY', 'Avg.SOIL.SILT', 'Avg.SOIL.CLAY' , 'Avg.SOIL.OM' , 'Avg.SOIL.BD')] ;
# 
# 
# 
# Project_Soil.Rev[seq(dim(Project_Soil)[1]+1,dim(Project_Soil)[1]+dim(Soil_Mukey_Gaps_All.Nabr)[1]),c('QTZ', 'DMAC', 'MACVF', 'MACHF', 'BETA', 'ALPHA', 'MINSMC', 'MAXSMC', 'KSATH', 'KSATV' ,'KINF')]<--999
# 
# 


################################################################################################################################
#
#
#                         Write notes on the corrections made for the soils that have deffective data or no data in the 
#                         Soil Ssurgo database
# 
# 
#####################################################################################################################################

Project_Soil.Rev$comments<-c('#') ;

Project_Soil.Rev[Project_Soil.Rev$MUKEY== 542030, c('comments')]<-c('# mukey 542030, is Opequon  https://casoilresource.lawr.ucdavis.edu/sde/?series=opequon') ;

Project_Soil.Rev[Project_Soil.Rev$MUKEY== 542033, c('comments')]<-c('# mukey 542033, is also a Opequon  https://casoilresource.lawr.ucdavis.edu/sde/?series=opequon') ;


Project_Soil.Rev[Project_Soil.Rev$MUKEY== 542034, c('comments')]<-c('mukey 542034 is abandoned mine pitts, this have no data at all') ;


Project_Soil.Rev[Project_Soil.Rev$MUKEY== 539762, c('comments')]<-c('# Map Unit No. 539762 is a water body and is not available to query from the SDA_query function');


Project_Soil.Rev[Project_Soil.Rev$MUKEY== 539759, c('comments')]<-c('# Map Unit No. 539759 are urban land Urban land-Udults complex') ;

Project_Soil.Rev[Project_Soil.Rev$MUKEY== 542043, c('comments')]<-c('# Map Unit No. 542034 Pits  https://casoilresource.lawr.ucdavis.edu/soil_web/ssurgo.php?action=explain_mapunit&mukey=542034') ;

Project_Soil.Rev[Project_Soil.Rev$MUKEY== 753522, c('comments')]<-c('# Map Unit No.753522 Houghton Muck Peatlands with no reliable data') ;


Project_Soil.Rev[Project_Soil.Rev$MUKEY== 423299, c('comments')]<-c('# Map Unit No. 423299 Marsh Peatlands with no reliable data') ;


Project_Soil.Rev[Project_Soil.Rev$MUKEY== 753456, c('comments')]<-c('# Map Unit No.753456 Alluvial Wet Peatlands with no reliable data') ;


Project_Soil.Rev[Project_Soil.Rev$MUKEY== 753597, c('comments')]<-c('# Map Unit No. 753597 is  Water');

Project_Soil.Rev[Project_Soil.Rev$MUKEY== 700421, c('comments')]<-c('# Map Unit No. 700421is Water');




################################################################################################################################
#
#
#                         Write the soil data in the format approptiate for PIHM to take  
# 
# 
################################################################################################################################







##### When no changes are necesary to the data just equate the Project_Soil.Rev to the original fiel and that is it

#Project_Soil.Rev<-Project_Soil[order(Project_Soil$INDEX ),] ;

# NUMSOIL<-data.frame(c('NUMSOIL'), dim(HansYoust_Soil)[1]) ;

NUMSOIL<-data.frame(c('NUMSOIL'), dim(Project_Soil.Rev)[1]) ;



write.table(NUMSOIL,file='Soil.txt', row.names=F , quote=F, sep = "\t", col.names=F) ;

# write.table(HansYoust_Soil[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Soil.txt'), row.names=F , quote=F, sep = "\t", append= T) ;






###### Get 4 signifficant digits on the caluclated values for soils
str(Project_Soil.Rev)

Project_Soil.Final<-data.frame(Project_Soil.Rev[,'INDEX'],signif(Project_Soil.Rev[,c('SILT',  'CLAY',	'OM','BD') ], 4), Project_Soil.Rev[, c('KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')], Project_Soil.Rev[,c('compname' , 'taxorder', 'taxsuborder' , 'taxgrtgroup', 'taxsubgrp')] , as.integer(Project_Soil.Rev[,'MUKEY']), Project_Soil.Rev['comments'])  ;

names(Project_Soil.Final)[1]<-c('INDEX') ;
names(Project_Soil.Final)[dim(Project_Soil.Final)[2]-1]<-c('MUKEY') ;


View(Project_Soil.Final)

write.table(Project_Soil.Final,file='Soil.txt', row.names=F , quote=F, sep = "\t", append= T) ;



####################  Add DINF , KMACV_RO  and KMACH_RO  at the end of the soil file ###########################################


# DINF (type: double, unit: m) A virtual top soil layer thickness across which infiltration is calculated.
# KMACV RO (type: double, unit: dimensionless) Ratio between vertical macropore hydraulic conduc-
#   tivity and vertical saturated infiltration hydraulic conductivity.
# KMACH RO (type: double, unit: dimensionless) Ratio between horizontal macropore hydraulic conductivity and 
# horizontal saturated hydraulicconductivity.


################################################################################################################################



DINF_etc<-data.frame(c('DINF' , 'KMACV_RO', 'KMACH_RO'), c( 0.10, 100.0 , 1000.0 )) ;

write.table(DINF_etc,file='Soil.txt', row.names=F , col.names=F ,quote=F, sep = "\t", append= T) ;




  
  

#################################################################################################################################
#
#                   Some of the dominant components Geology Mukeys do no have data available. 
#                   The strategy to fill these gaps is to get the average of the Mukeys parameters of the neighboring Triangles. 
#                   The same strategy can be used to complete the Soils data.
#
#################################################################################################################################



# Create a new revised Geology  data file

Project_Geology.Rev<-Project_Geology ;


# Which geology data have NA data

Project_Geology.NA<-data.frame(which(is.na(Project_Geology), arr.ind=T)) ;

# Which mukeys do not have data


Mukeys_Gaps_Geology<-Project_Geology[unique(Project_Geology.NA$row),'MUKEY'] ;

str(Mukeys_Gaps_Geology)

##### Find the soil index  and the triangles that have Mukeys corresponding to the Mukeys_Geology_Gaps

str(MUKEYS.MAP)


Mukeys_Gaps_Geology_indx<-MUKEYS.MAP[MUKEYS.MAP$MUKEYS.mode %in% as.numeric(Mukeys_Gaps_Geology), ]    ;


###### Find the neighboring triangles of the Triangles with the Mukeys_Geology_Gaps


Mukey_Gaps_indx_neighbors_Geology<-Watershed.1.neigh[Watershed.1.neigh$triangle %in% Mukeys_Gaps_Geology_indx$Ele_ID, ] ; #c( "triangle" , "neighbor1" , "neighbor2" , "neighbor3")]   ;


###### retrieve the representative mukeys of the neighboring triangles for the Geology parameters

Neighbor_Mukeys_Geology<-MUKEYS.MAP[MUKEYS.MAP$Ele_ID %in% unique(unlist(Mukey_Gaps_indx_neighbors_Geology, use.names = F)), c('GSURGO_Mod','Ele_ID') ]  ;


str(Project_Geology)

Neighbor_Mukeys_para.Geology<-Project_Geology[Project_Geology$MUKEY %in% Neighbor_Mukeys_Geology, c('MUKEY', 'SILT' , 'CLAY' , 'OM' ,  'BD') ]  ;

names(Neighbor_Mukeys_para.Geology)<-c('MUKEY', paste0('GEOL.', c('SILT' , 'CLAY' , 'OM' ,  'BD'))) ;



Index_Neighbor_Mukeys_para.Geology<-merge(Mukeys_Gaps_Geology_indx,Neighbor_Mukeys_para.Geology, by.x='MUKEYS.mode', by.y='MUKEY', all.x=T)  ;






# ######## Add new rows to the Geology. file to include the new created Geology parameters from neighbors and create a revised Geology parameter file
# 
# # Create a new revised Geology  data file
# 
# Project_Geology.Rev<-Project_Geology ;


#######  Put together the representative MUKEYS parameters for Neighbouring triangles

Geology_Mukey_Gaps_Nabr.0<-merge(Mukey_Gaps_indx_neighbors_Geology[,c("triangle" , "neighbor1")],Index_Neighbor_Mukeys_para.Geology, by.x="neighbor1", by.y='Ele_ID', all.x=T, sort=F ) ;

names(Geology_Mukey_Gaps_Nabr.0)<-c(names(Geology_Mukey_Gaps_Nabr.0)[1:4], paste0('Nabr.0.',names(Index_Neighbor_Mukeys_para.Geology)[4:7])) ;

Geology_Mukey_Gaps_Nabr.1<-merge(Mukey_Gaps_indx_neighbors_Geology[,c("triangle" , "neighbor2")], Index_Neighbor_Mukeys_para.Geology, by.x="neighbor2", by.y='Ele_ID', all.x=T , sort=F )  ;

names(Geology_Mukey_Gaps_Nabr.1)<-c(names(Geology_Mukey_Gaps_Nabr.1)[1:4], paste0("Nabr.1.",names(Index_Neighbor_Mukeys_para.Geology)[4:7])) ;

Geology_Mukey_Gaps_Nabr.2<-merge(Mukey_Gaps_indx_neighbors_Geology[,c("triangle", "neighbor3")],Index_Neighbor_Mukeys_para.Geology, by.x="neighbor3", by.y='Ele_ID' , all.x=T , sort=F )  ;

names(Geology_Mukey_Gaps_Nabr.2)<-c(names(Geology_Mukey_Gaps_Nabr.2)[1:4], paste0("Nabr.2.",names(Index_Neighbor_Mukeys_para.Geology)[4:7])) ;



####  Gathering all the data of neighbors together to be ready to take the mean of the neighbor triangles 

Geology_Mukey_Gaps_All.Nabr<-merge(merge(Geology_Mukey_Gaps_Nabr.0,Geology_Mukey_Gaps_Nabr.1,by="triangle"),Geology_Mukey_Gaps_Nabr.2,by="triangle")  ;
names(Geology_Mukey_Gaps_All.Nabr)



######## Average the representative Mukeys properties  of the neighboring triangles for Geology, removing NA, NAN and 0 values  ##############

#change all zero values to NA

Geology_Mukey_Gaps_All.Nabr[Geology_Mukey_Gaps_All.Nabr==0]<-NA ;


#Calculate the means without accounting for NA values

Geology_Mukey_Gaps_All.Nabr$Avg.GEOL.SILT<-apply(Geology_Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.SILT', 'Nabr.1.GEOL.SILT' , 'Nabr.2.GEOL.SILT')], MARGIN=1,FUN='mean', na.rm=T) ;


Geology_Mukey_Gaps_All.Nabr$Avg.GEOL.CLAY<-apply(Geology_Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.CLAY', 'Nabr.1.GEOL.CLAY' , 'Nabr.2.GEOL.CLAY')], MARGIN=1,FUN='mean', na.rm=T) ;


Geology_Mukey_Gaps_All.Nabr$Avg.GEOL.OM<-apply(Geology_Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.OM', 'Nabr.1.GEOL.OM' , 'Nabr.2.GEOL.OM')], MARGIN=1,FUN='mean', na.rm=T) ;


Geology_Mukey_Gaps_All.Nabr$Avg.GEOL.BD<-apply(Geology_Mukey_Gaps_All.Nabr[, c('Nabr.0.GEOL.BD', 'Nabr.1.GEOL.BD' , 'Nabr.2.GEOL.BD')], MARGIN=1,FUN='mean', na.rm=T) ;


Geology_Mukey_Gaps_All.Nabr$New.MUKEY<--999  ;

# Add the newly created data from neighbors to the soil data files 


Project_Geology.Rev[seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Geology_Mukey_Gaps_All.Nabr)[1]), c('INDEX')]<-seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Geology_Mukey_Gaps_All.Nabr)[1]) ;


Project_Geology.Rev[seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Geology_Mukey_Gaps_All.Nabr)[1]), c('MUKEY' ,'SILT' , 'CLAY' , 'OM' ,  'BD' )]<-Geology_Mukey_Gaps_All.Nabr[,c('New.MUKEY', 'Avg.GEOL.SILT', 'Avg.GEOL.CLAY' , 'Avg.GEOL.OM' , 'Avg.GEOL.BD')] ;


Project_Geology.Rev[seq(dim(Project_Geology)[1]+1,dim(Project_Geology)[1]+dim(Geology_Mukey_Gaps_All.Nabr)[1]),c('QTZ', 'DMAC', 'MACVF', 'MACHF', 'BETA', 'ALPHA', 'MINSMC', 'MAXSMC', 'KSATH', 'KSATV' ,'KINF')]<--999



################################################################################################################################
#
#
#                         make corrections for the geology that have deffective data or no data in the 
#                         Soil Ssurgo database
# 
# 
##############################################################################################################################################

Project_Geology.Rev$comments<-c('#') ;

Project_Geology.Rev[Project_Geology.Rev$MUKEY== 542030, c('comments')]<-c('# mukey 542030, is Opequon  https://casoilresource.lawr.ucdavis.edu/sde/?series=opequon') ;

Project_Geology.Rev[Project_Geology.Rev$MUKEY== 542033, c('comments')]<-c('# mukey 542033, is also a Opequon  https://casoilresource.lawr.ucdavis.edu/sde/?series=opequon') ;


Project_Geology.Rev[Project_Geology.Rev$MUKEY== 542034, c('comments')]<-c('mukey 542034 is abandoned mine pitts, this have no data at all') ;

Project_Geology.Rev[Project_Geology.Rev$MUKEY== 539762, c('comments')]<-c('# Map Unit No. 539762 is a water body and is not available to query from the SDA_query function');


Project_Geology.Rev[Project_Geology.Rev$MUKEY== 539759, c('comments')]<-c('# Map Unit No. 539759 are urban land Urban land-Udults complex') ;

Project_Geology.Rev[Project_Geology.Rev$MUKEY== 542043, c('comments')]<-c('# Map Unit No. 542034 Pits  https://casoilresource.lawr.ucdavis.edu/soil_web/ssurgo.php?action=explain_mapunit&mukey=542034') ;

Project_Geology.Rev[Project_Geology.Rev$MUKEY== 753522, c('comments')]<-c('# Map Unit No.753522 Houghton Muck Peatlands with no reliable data') ;


Project_Geology.Rev[Project_Geology.Rev$MUKEY== 423299, c('comments')]<-c('# Map Unit No. 423299 Marsh Peatlands with no reliable data') ;


Project_Geology.Rev[Project_Geology.Rev$MUKEY== 753456, c('comments')]<-c('# Map Unit No.753456 Alluvial Wet Peatlands with no reliable data') ;


Project_Geology.Rev[Project_Geology.Rev$MUKEY== 753597, c('comments')]<-c('# Map Unit No. 753597 is  Water');

Project_Geology.Rev[Project_Geology.Rev$MUKEY== 700421, c('comments')]<-c('# Map Unit No. 700421is Water');

  
  


################################################################################################################################
#
#
#                         Write the geology data in the format approptiate for PIHM to take  
# 
# 
################################################################################################################################


##### When no changes are necesary to the data just equate the Project_Soil.Rev to the original fiel and that is it
##  Project_Geology.Rev<-Project_Geology[order(Project_Geology$INDEX ),] ;



# NUMGEOL<-data.frame(c('NUMGEOL'), dim(HansYoust_Geology)[1]) ;

NUMGEOL<-data.frame(c('NUMGEOL'), dim(Project_Geology.Rev)[1]) ;

write.table(NUMGEOL,file='Geology.txt', row.names=F , quote=F, sep = "\t", col.names=F) ;


# write.table(HansYoust_Geology[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Geology.txt'), row.names=F , quote=F, sep = "\t", append= T) ;

Project_Geology.Final<-data.frame(Project_Geology.Rev[,'INDEX'],signif(Project_Geology.Rev[,c('SILT',  'CLAY',	'OM','BD') ], 4), Project_Geology.Rev[, c('KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')], Project_Geology.Rev[,c('compname' , 'taxorder', 'taxsuborder' , 'taxgrtgroup', 'taxsubgrp')] , as.integer(Project_Geology.Rev[,'MUKEY']), Project_Geology.Rev['comments'])   ;



names(Project_Geology.Final)[1]<-c('INDEX') ;

names(Project_Geology.Final)[dim(Project_Geology.Final)[2]-1]<-c('MUKEY');

View(Project_Geology.Final)

write.table(Project_Geology.Final,file='Geology.txt', row.names=F , quote=F, sep = "\t", append= T) ;


write.table(DINF_etc, file='Geology.txt', row.names=F , quote=F, sep = "\t", col.names=F, append= T ) ;






save.image(file='FillNoDataSoils.RData') ;




