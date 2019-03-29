##############################################################################################################
# 
# 
# Program to plot soil data from cycles output
# 
# Felipe Montes 2019  03 29
# 
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

setwd('C:\\Felipe\\Willow_Project\\Willow_Experiments\\Willow_Rockview\\WillowRockViewData\\Wilow Harvest 2019') 


#Loand And install packages 
#install.packages('readxl', 'fansi', dependencies = T)

library(readxl,fansi) ;

#Read the data from the excel spreadsheet
