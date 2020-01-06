##############################################################################################################
# 
# 
#     Program to fill i gap values in the .soil file in PIHM with values obtained from the literature
#     Developed to complement the information in SSURGO wiht infomration obtaned thorugh a literature review
#     of peatlands, mucks and bogland soils for the Yahara watershed in Wisconsin
# 
# 
#  Felipe Montes 2019 11/22
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


setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs') ;      #  setwd(RevisedOutputs.dir)   ;



###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


# Install the packages that are needed #

#install.packages(c("readxl", "openxlsx"))install.packages(c("readxl", "openxlsx"))




###############################################################################################################
#                           load the libraries that are neded   
###############################################################################################################


library(openxlsx)


###############################################################################################################
#                            Read .soil text file from the PIHM inputs                     
###############################################################################################################

NUM_Soil<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Soil.txt", header=F, sep="", nrows=1 , as.is=T) ; 

str(NUM_Soil)

Soil<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Soil.txt", header=T, sep = '\t', skip=1, nrows=NUM_Soil[1,2], as.is=T ) ;# NUM[1,2]

View(Soil)
str(Soil)


DINF_etc_Soil<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Soil.txt", header=F, sep = '\t', skip=NUM_Soil[1,2]+2,as.is=T) ;


View(DINF_etc_Soil)
str(DINF_etc_Soil)





###############################################################################################################
#                            Read values obtained from th eliterature and aggregated in an excell spreadsheet      
###############################################################################################################

Lit.data<-read.xlsx("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\soils_GSSURGO_wi_3752292_01\\PeatHydraulicParameters.xlsx", sheet='PIHM_Parameters')

str(Lit.data)

View(Lit.data)

names(Lit.data)

###############################################################################################################
#                           Match the literature data with the colums of the .soil data
###############################################################################################################

Soil_NA<-which(is.na(Soil[,seq(1,dim(Soil)[2]-1)]), arr.ind=T)[,1] ;

Soil_0<-which(Soil==0, arr.ind=T)[,1] ;

Soil_missing<-unique(c(Soil_NA,Soil_0)) ;

Soil[Soil_missing,names(Lit.data)]<-Lit.data   ;

View(Soil)

################################################################################################################################
#
#
#                         Write the soil data in the format approptiate for PIHM to take  
# 
# 
################################################################################################################################


write.table(NUM_Soil,file='Soil_Rev.txt', row.names=F , quote=F, sep = "\t", col.names=F) ;

# write.table(HansYoust_Soil[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Soil.txt'), row.names=F , quote=F, sep = "\t", append= T) ;


write.table(Soil,file='Soil_Rev.txt', row.names=F , quote=F, sep = "\t", append= T) ;



write.table(DINF_etc_Soil,file='Soil_Rev.txt', row.names=F , col.names=F ,quote=F, sep = "\t", append= T) ;





###############################################################################################################
#                            Read .Geology text file from the PIHM inputs                     
###############################################################################################################

NUM_Geology<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Geology.txt", header=F, sep="", nrows=1 , as.is=T) ; 

str(NUM_Soil)

Geology<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Geology.txt", header=T, sep = '\t', skip=1, nrows=NUM_Soil[1,2], as.is=T ) ;# NUM[1,2]

View(Geology)
str(Geology)


DINF_etc_Geology<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Soil.txt", header=F, sep = '\t', skip=NUM_Soil[1,2]+2,as.is=T) ;


View(DINF_etc_Geology)
str(DINF_etc_Geology)



###############################################################################################################
#                           Match the literature data with the colums of the .GEology data
###############################################################################################################

Geology_NA<-which(is.na(Geology[,seq(1,dim(Geology)[2]-1)]), arr.ind=T)[,1] ;

Geology_0<-which(Geology==0, arr.ind=T)[,1] ;

Geology_missing<-unique(c(Geology_NA,Geology_0)) ;

Geology[Geology_missing,names(Lit.data)]<-Lit.data   ;

View(Geology)



################################################################################################################################
#
#
#                         Write the Geology data in the format approptiate for PIHM to take  
# 
# 
################################################################################################################################


write.table(NUM_Geology,file='Geology_Rev.txt', row.names=F , quote=F, sep = "\t", col.names=F) ;

# write.table(HansYoust_Soil[, c('INDEX','SILT',  'CLAY',	'OM','BD', 'KINF', 'KSATV' , 'KSATH' , 'MAXSMC' , 'MINSMC' , 'ALPHA' , 'BETA' , 'MACHF' , 'MACVF' , 'DMAC', 'QTZ')],file=paste0(inputfile.name, '_Soil.txt'), row.names=F , quote=F, sep = "\t", append= T) ;


write.table(Geology,file='Geology_Rev.txt', row.names=F , quote=F, sep = "\t", append= T) ;



write.table(DINF_etc_Geology,file='Geology_Rev.txt', row.names=F , col.names=F ,quote=F, sep = "\t", append= T) ;

save.image(file='SoilLiteratureValuesPIHM.RData') ;

