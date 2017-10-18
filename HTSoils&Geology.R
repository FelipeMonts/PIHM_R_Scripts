#   Felipe Montes
#   2015 05 18
#   Program to process HT Soils Data 


#   Setting the working directory

setwd('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/PIHMInputsR');


#   Reading the Soil information from Hydroterre HT_Soil_Surgo.txt text files


HT_Soil_Surgo<-read.table('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/hydroterre_MANHANTANGO/HT_Soil_Surgo.txt',header=F,skip=2);

names(HT_Soil_Surgo)<-c('MUKEY','SILT',	'CLAY',	'OM',	'BD');

#   Writting the corrected soil information into a text file

write.table(HT_Soil_Surgo , file='Soil_Surgo.txt');


#   Reading the Geololgy information from Hydroterre HT_Geology.txt text files


HT_Geology<-read.table('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/hydroterre_MANHANTANGO/HT_Geology.txt',header=F,skip=2);

names(HT_Geology)<-c('MUKEY','SILT',  'CLAY',	'OM',	'BD');


#  Correcting the indexz number in the MUKEY column of the Geology file. It needs to start at 1

HT_Geology[,'MUKEY']<-HT_Geology[,'MUKEY']-1 ;


#   Writting the corrected geology information into a text file


write.table(HT_Geology , file='Geology.txt');




