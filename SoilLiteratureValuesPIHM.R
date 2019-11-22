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

library(readxl)

library(openxlsx)


###############################################################################################################
#                            Read .soil text file from the PIHM inputs                     
###############################################################################################################

NUM<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Geology.txt", header=F, sep="", nrows=1 ) ; 

str(NUM)

Geology<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\MM_PHIM_inputs\\Geology.txt", header=T, sep="", skip=1,nrows=NUM[1,2] ) ;

str(Geology)


###############################################################################################################
#                            Read values obtained from th eliterature and aggregated in an excell spreadsheet      
###############################################################################################################

Lit.data<-read.xlsx("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM SIMULATIONS\\YAHARA\\soils_GSSURGO_wi_3752292_01\\PeatHydraulicParameters.xlsx", sheet='PIHM_Parameters')

str(Lit.data)

View(Lit.data)

View(t(Lit.data))
t(Lit.data[,1])

###############################################################################################################
#                           Match the literature data with the colums of the .soil data
###############################################################################################################

Geology[1,]
t(Lit.data[,1])


