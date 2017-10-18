#   Felipe Montes
#   2015 05 18
#   Program to merge HT forcing data 


#   Setting the working directory

setwd('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/PIHMInputsR');


#   Reading the Forcing Tables

ForcData79_89<-read.csv('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerre_Manhantango_ETV_Data_1979_1989/ForcingData79_89.csv',header=T);


ForcData89_99<-read.csv('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreManhantango_ETV_Data_1989_1999/ForcingData89_99.csv',header=T);


ForcData99_2009<-read.csv('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreManhantango_ETV_Data_1999_2010/ForcingData99_2009.csv', header=T);


#   Selecting the variables that correspond to WE38 and aggregating the forcing data into one file


WE38ColNames<-c('DateTime','Precip_045394','Temp_045394','RH_045394','Wind_045394','RN_045394','VP_045394','LW_045394');


WE38_ForcData79_2009<-rbind(ForcData79_89[,WE38ColNames], ForcData89_99[,WE38ColNames],ForcData99_2009[,WE38ColNames]);




#   writting the complete forcing table to a txt file;

write.table(WE38_ForcData79_2009, file='Forcing.txt');


#    Reading the Lookup tables for the Land cover paramaters


WE38_ForcingLookupData79_89<-read.csv('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerre_Manhantango_ETV_Data_1979_1989/ForcingLookupData79_89.csv');

WE38_ForcingLookupData89_99<-read.csv('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreManhantango_ETV_Data_1989_1999/ForcinglookupData89_99.csv');


WE38_ForcingLookupData99_2009<-read.csv('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreManhantango_ETV_Data_1999_2010/ForcingLookupData99_2009.csv');


names(WE38_ForcingLookupData79_89)

#   The Land Cover data File has a very cumbersome structure.... Needs more work on it



