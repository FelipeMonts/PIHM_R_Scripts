###########    Program to import and inspect Forcing Data from Hydroterre and transform it into the apropriate format and units to be used
###########    in PIHM
###########    Felipe Montes 2015 09 17



#      set the working directory

setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\RScripts");


#      Import data from the parsed XML downloaded from Hydroterre

#      The data is segmented into decades and the decades have overlapping years therefore need to be corrected

#       First Dedade
HT.Data_79_89<-read.csv('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\HydroTerre_Manhantango_ETV_Data_1979_1989\\ForcingData79_89.csv',header=T, as.is= T) ;


# the time stamp is in the format 1979-01-01T00:00:00-05:00 wich is UTC - 05:00 (UTC- 5 hours is standard easten time EST)
# Split the DateTime string in hydroterre using the T to get the Date and time separated

HT.Date_79_89<-sapply(strsplit(HT.Data_79_89$DateTime,"T"),'[',1);

# Take the second part to the Split Hydroterre DateTime and get the time component and discard the "-05:00"

HT.Time_79_89<-sapply(strsplit(HT.Data_79_89$DateTime,"T"),'[',2);

HT.Time.UTC_79_89<-sapply(strsplit(HT.Time_79_89,"-"),"[",1);

# Put together the time and date from Hydroterre in the format of MM-PHIM: 2008-01-01 00:00 and substact 5 hours (60*60*5 sec) to get EST

HT.Data_79_89$UTC<-as.POSIXct(paste(HT.Date_79_89,HT.Time.UTC_79_89), format= "%Y-%m-%d %H:%M:%S")  ;


#       Second decade


HT.Data_89_99<-read.csv('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\HydroTerreManhantango_ETV_Data_1989_1999\\ForcingData89_99.csv',header=T, as.is= T) ;


HT.Date_89_99<-sapply(strsplit(HT.Data_89_99$DateTime,"T"),'[',1);

# Take the second part to the Split Hydroterre DateTime and get the time component and discard the "-05:00"

HT.Time_89_99<-sapply(strsplit(HT.Data_89_99$DateTime,"T"),'[',2);

HT.Time.UTC_89_99<-sapply(strsplit(HT.Time_89_99,"-"),"[",1);

# Put together the time and date from Hydroterre in the format of MM-PHIM: 2008-01-01 00:00 and substact 5 hours (60*60*5 sec) to get EST

HT.Data_89_99$UTC<-as.POSIXct(paste(HT.Date_89_99,HT.Time.UTC_89_99), format= "%Y-%m-%d %H:%M:%S")  ;



#      Third decade

  
HT.Data_99_09<-read.csv('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\Manhantango\\HydroTerreManhantango_ETV_Data_1999_2010\\ForcingData99_2009.csv', header=T, as.is=T) ; 

HT.Date_99_09<-sapply(strsplit(HT.Data_99_09$DateTime,"T"),'[',1);

# Take the second part to the Split Hydroterre DateTime and get the time component and discard the "-05:00"

HT.Time_99_09<-sapply(strsplit(HT.Data_99_09$DateTime,"T"),'[',2);

HT.Time.UTC_99_09<-sapply(strsplit(HT.Time_99_09,"-"),"[",1);

# Put together the time and date from Hydroterre in the format of MM-PHIM: 2008-01-01 00:00 and substact 5 hours (60*60*5 sec) to get EST

HT.Data_99_09$UTC<-as.POSIXct(paste(HT.Date_99_09,HT.Time.UTC_99_09), format= "%Y-%m-%d %H:%M:%S")  ;


#   Finally 2010


# HT.Data_2010<-read.csv('',header=T, as.is=T) ;


#   Put the complete data series together for processing


HT.Data<-rbind(HT.Data_79_89[HT.Data_79_89$UTC>=as.POSIXct("1979-01-01 00:00:00") & HT.Data_79_89$UTC<=as.POSIXct("1989-12-31 23:00:00"),],HT.Data_89_99[HT.Data_89_99$UTC>=as.POSIXct("1990-01-01 00:00:00") & HT.Data_89_99$UTC<=as.POSIXct("1999-12-31 23:00:00"), ],HT.Data_99_09[HT.Data_99_09$UTC>=as.POSIXct("2001-01-01 00:00:00") & HT.Data_99_09$UTC<=as.POSIXct("2009-12-31 23:00:00"), ])



#  Calculate the time stamp in Eastern standard time from the UTC time stamp

PIHM.TS<-HT.Data$Time.EST<-HT.Data$UTC-(60*60*5) ;



#      Select the columns that have the right Forcing index For WE 38 it is 045394



#     names of the columns to be selected: based on the quandrant index according to location.


#    MM-PIHM will need the following data series and corresponding header columns 

###   NUM_METEO_TS    1
###   METEO_TS        1       WIND_LVL    10.0
###   TIME                    PRCP		SFCTMP  RH      SFCSPD  SOLAR   LONGWV  PRES
###   TS                      kg/m2/s		K       %       m/s     W/m2    W/m2    Pa

#   The Matching of variables between Hydroterre and MM-PIHM is  as follows
# 
#    HYDROTERRE                                                          PIHM    
#    DateTime  (UTC)                                                          TS
#    Precip_#Index# (m/d)                                                PRCP (kg/m2/s)
#    Temp_#Index#  (C)                                                   SFCTMP (K)
#    RH_#Index#     (1/100)                                                RH (%)
#    Wind_#Index#  (m/day)                                               SFCSPD (m/s)
#    RN__#Index#   (J /m2 day * 86400 seconds/day == (W/m2))             SOLAR(W/m2)
#    LW_#Index#    (J /m2 day * 86400 seconds/day == (W/m2))             LONGWv (W/m2)
#    ?                                                                   PRES(pa)
#    VP_#Index#    (Pa)                                                  Vapor  Pressure (Pa)
#    








  
########### Select the index or indexes which will be used to extract data   #################

HT.index<-c("045394");



 
########### Extract the index precipitation and convert it HT units (m/d) to PIHM units kg/m2/s #########

Precip.index<-which(names(HT.Data) %in% paste("Precip", HT.index, sep="_"));

#   HT [m/d] x [m2/m2] x [1000kg H2O/1 m3H2O] x [1d/86400 s] = HT [m/d] x [1000/86400]= HT [kg /m2 s]

PIHM.PRCP<-round(signif(HT.Data[,Precip.index]*(1000/86400),digits=3),8) ;


##########  Extract HT index Temperature  and convert to PIHM units
#   HT [c] + 273.15 K = HT K

Temp.index<-which(names(HT.Data) %in% paste("Temp", HT.index, sep="_")) ;

PIHM.SFCTMP<-round(HT.Data[,Temp.index]+273.15,digits=2) ;



##########  Extract HT index Relative humidity  and convert to PIHM units
#   HT [1/100] x 100 = HT %

RH.index<-which(names(HT.Data) %in% paste("RH", HT.index, sep="_")) ;

PIHM.RH<-round(HT.Data[,RH.index]*100, digits=2)  ;


##########  Extract HT index Wind Speed and convert to PIHM units
#   HT [m/d] x [d / 86400 s] = HT [m/s]


Wind.index<-which(names(HT.Data) %in% paste("Wind", HT.index, sep="_")) ;

PIHM.SFCSPD<-round(HT.Data[,Wind.index]/86400, digits=2)  ;




##########  Extract HT index Solar Radiation and convert to PIHM units

# HT [Joule/m2 s] x [Watt x s / Joule ] x [86400 s/d] = HT [w /m2 d]


RN.index<-which(names(HT.Data) %in% paste("RN", HT.index, sep="_")) ;

PIHM.SOLAR<-round(HT.Data[,RN.index]/86400, digits=2)  ;




##########  Extract HT index long wave radiation and convert to PIHM units

# HT [Joule/m2 s] x [Watt x s / Joule ] x [86400 s/d] = HT [w /m2 d]


LW.index<-which(names(HT.Data) %in% paste("LW", HT.index, sep="_")) ;

PIHM.LONGWV<-round(HT.Data[,LW.index]/86400, digits=2)  ;




#########  Print the table in the correct format with the corresponding column  headers

###   NUM_METEO_TS    1
###   METEO_TS        1       WIND_LVL    10.0
###   TIME                    PRCP		SFCTMP  RH      SFCSPD  SOLAR   LONGWV  PRES
###   TS                      kg/m2/s		K       %       m/s     W/m2    W/m2    Pa

#    Add the heading column headings with the correct format

cat(c("NUM_METEO_TS", "1" ), file="WE38.FORC", sep= "\t", fill=T) ;
cat(c("METEO_TS" , "1" ,"WIND_LVL", "10.0" ), file="WE38.FORC", append=T, sep= "\t", fill=T) ;
cat(c("TIME" , "PRCP" , "SFCTMP" , "RH" , "SFCSPD" , "SOLAR", "LONGWV" , "PRES"), file="WE38.FORC", append=T, sep= "\t", fill=T) ;
cat(c("TS" , "kg/m2/s" , "K" , "%" , "m/s" , "W/m2" , "W/m2" , "Pa"),file="WE38.FORC", append=T, sep= "\t", fill=T) ;

#  Add the data to the table

PIHM.data<-data.frame( PIHM.TS, PIHM.PRCP, PIHM.SFCTMP , PIHM.RH , PIHM.SFCSPD, PIHM.SOLAR , PIHM.LONGWV);

#########  Needs to get pressure in PA

PIHM.data$PRES<-97000.00;


######### Writing all the data into a table with the correct format


##  write.table(format(PIHM.data , scientific=6), file="WE38.FORC", append=T, sep= "\t", quote=F , col.names = F, row.names = F) ;


######### writting a subset of the table with the correct format

data.subset<-which(PIHM.data$PIHM.TS >= as.POSIXct("2005-01-01 00:00:00") & PIHM.data$PIHM.TS<=as.POSIXct("2010-12-31 23:59"));


PIHM.data.sub<-PIHM.data[data.subset,] ;

# Printed the time stamp in the correct format for PIHM inputs MM-PIHM

PIHM.data.sub$PIHM.TS<-format(PIHM.data.sub$PIHM.TS,format="%Y-%m-%d %H:%M") ;


write.table(format(PIHM.data.sub, scientific=6), file="WE38.FORC", append=T, sep= "\t", quote=F , col.names = F, row.names = F) ;



















