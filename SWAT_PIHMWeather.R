###########    Program to import and compare weather data from Hydroterre, PIHM and SWAT 
###########    
###########    Felipe Montes 2015 09 22 

#      set the working directory

setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\RScripts");

#      set the path to the library where packages are installed


.libPaths("C:/Felipe/R_Library/library")

##########   Read Weather files from SWAT output     #####################

#   save the path for the SWAT data directory

SWAT.data.dir<-c("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE38_forPIHM_11jun2015\\Weather\\Weather_from1981\\")


#  lkup_we38hmd.txt  to obtain Latitude, Longitude and elevation


SWAT.header<-read.csv(paste0(SWAT.data.dir,"lkup_we38hmd.txt")) [,c("LAT", "LONG" ,  "ELEVATION")]  ;


#   read the relative humidity  "hmd" time series starting date, the data series, create the time stamp for the series and add it to the time series


# get the dimensions of the hmd data

hmd.dim<-dim(read.table(paste0(SWAT.data.dir,"we38hmd.txt"),header=T)) ;


SWAT.Date.start<-read.table (paste0(SWAT.data.dir,"we38hmd.txt"),header = F, nrows=1) ;

SWAT.Date.start.Posix<-as.Date(as.character(SWAT.Date.start),format="%Y%m%d") ;


SWAT.Data<-read.table(paste0(SWAT.data.dir,"we38hmd.txt"),header=T,col.names = c("hmd"))*100 ;

SWAT.Data$DateTime<-seq(SWAT.Date.start.Posix, by = "day", length.out = hmd.dim [1]) ;




#   read the Precipitation "pcp"

SWAT.Data$pcp<-read.table(paste0(SWAT.data.dir, "we38pcp.txt") ,header=T,col.names = c("pcp"))[,c("pcp")];



#   read solar radiation "slr"

slr.dim<-dim(read.table(paste0(SWAT.data.dir, "we38slr.txt"),header=T));

# The table for radiation has more data than the others. Assuming the data series ends in the same year, the data will be taken from the last record to the record that matches the length of the other data




SWAT.Data$slr<-read.table(paste0(SWAT.data.dir, "we38slr.txt") ,header=T,col.names = c("slr")) [seq(slr.dim[1]-((hmd.dim[1])-1),slr.dim[1]),c("slr")] ;



#  read temperature "tmp", which is comma separated and has two columns, Max tmp and min tmp

tmp.dim<-dim(read.csv(paste0(SWAT.data.dir, "we38tmp.txt"), header=F, skip=1 ));


SWAT.Data$tmp<-read.csv(paste0(SWAT.data.dir, "we38tmp.txt"), header=F,skip=1, col.names = c("tmp_max","tmp_min"))[,c("tmp_max","tmp_min")];

# read wind "wnd'


wnd.dim<-dim(read.csv(paste0(SWAT.data.dir, "we38wnd.txt"), header=T));


SWAT.Data$wnd<-read.table(paste0(SWAT.data.dir,"we38wnd.txt"),header=T,col.names= c("wnd"))[,c("wnd")];


# Read the flow output at the watershed gage in cubic meters per second cms. This is the format that was chosen for the STEWARDS watershed database



Flow<-read.csv('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\WE38_Monitored\\WE38DailyFlow.csv', header=F, as.is=T, col.names=c("PAMCWE38", "Date","Flow_cms","","Comment") ) ; 

Flow$Flow_cms<-as.numeric(Flow$Flow_cms) ;


Flow$DatePosix<-as.POSIXct(Flow$Date, format="%m/%e/%Y") ;

Flow$DatePosix<-format(Flow$DatePosix,format="%Y-%m-%d %H:%M:%S" ) ;

SWAT.Data$DateTime<-format(SWAT.Data$DateTime,format="%Y-%m-%d %H:%M:%S")  ;


Flow.Data<-merge(SWAT.Data[,c("DateTime","pcp")],Flow[Flow$DatePosix >= SWAT.Date.start.Posix ,c("DatePosix","Flow_cms")], by.x="DateTime", by.y="DatePosix");

Flow.Data$DateTime<-as.POSIXct(Flow.Data$DateTime) ;



###### PLot a hydrograph together with a hyetograph using the package EcoHydrology and the function hydrograph

library(EcoHydRology) ;

hydrograph(Flow.Data,P.units="mm", S.units="cms",S1.col="blue") ;
title(main="WE38")


###### Plot the Hyetograph together with a hyetograph by hand and more flexible, based on the R Water Module 3
#  https://web.ics.purdue.edu/~vmerwade/rwater.html

###### set the graphical parameters right for creating the hyetograph

#There are a lof of NA values in the data set

Flow.Data.NA<-Flow.Data[is.na(Flow.Data$pcp)|is.na(Flow.Data$Flow_cms),]

# mar :A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.

par(mar=c(3,5,1,4)+0.1);


# find the range of values to be plotted

pcp.range<-range(Flow.Data$pcp) ; 

flow.range<-range(Flow.Data$Flow_cms[!is.na(Flow.Data$Flow_cms)]); 

DateTime.range<-range(Flow.Data$DateTime);


#  plot the pcp
plot(Flow.Data$DateTime, Flow.Data$pcp, type="h",yaxt="n",xaxt="n", ylim=rev(c(0,4*pcp.range[2])), bty="n", main="WE38",col="light blue",ylab=NA);

# add axis with proper labeling

axis(side = 3, pos = 0, tck = 0,xaxt = "n") ;
axis(side = 4, at = pretty(seq(0, floor(pcp.range[2] + 1),length=c(5))),labels=pretty(seq(0, floor(pcp.range[2] + 1),length=c(5)))) ;
mtext(side=4,paste(names(Flow.Data)[2],"mm"),line = 2, cex = 0.9, adj = 1) ;

#add flow  plot
par(new=T);

plot(Flow.Data$DateTime,Flow.Data$Flow_cms, type='l',col="blue",axes=FALSE,yaxt="n", ylab="flow cms",xlab="date", ylim =c(0, 1.2*flow.range[2]));

#add axis with appropriate labels
axis(side = 1, at=seq(DateTime.range[1],DateTime.range[2],length.out=10),labels=format(seq(DateTime.range[1],DateTime.range[2],length.out=10),"%d-%b-%y")) ;
axis(side=2,at=pretty(seq(0,floor(flow.range[2]+1),length=10))) ;


#add aditional flow data 
lines(River.DateTime,River[,3],col="red")






###########   Import the data from PIHM outputs    ##################

#  C:\Felipe\PIHM-CYCLES\PIHM\PIHM_Felipe\CNS\WE-38\PIHMOutputs\WE38.1509261543\WE38.rivFlx1.txt

River<-read.table("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\PIHMOutputs\\WE38.1509261543\\WE38.rivFlx1.txt", header=F, as.is=T, nrows=500);

River.DateTime<-as.POSIXct(River[,1],format="%Y-%m-%d %H:%M") ;

plot(River.DateTime,River[,3],type='l',ylim=c(-1,10));  
for (i in seq(2,dim(River)[2])) {
  points(River.DateTime,River[,i],type='l',col=i ) ;
}


############## Create the flow duration curve for calibration   ##############

####### Sorting the Flow_cms data from the Flow.Data File

ordered.Flow.Data<-Flow.Data[with(Flow.Data,order(Flow_cms,na.last=T,decreasing = T)),] ;

###### Normalization factor =  total days with flow values different than na

Normalization.time<-dim(ordered.Flow.Data)[1]- sum(is.na(ordered.Flow.Data$Flow_cms)) ;


ordered.Flow.Data$Rank<-seq(1:dim(ordered.Flow.Data)[1]) ;

ordered.Flow.Data$Prob.exced<-ordered.Flow.Data$Rank/Normalization.time ;

### Plot the flow Duration Curve for the whole period


plot(ordered.Flow.Data$Prob.exced,ordered.Flow.Data$Flow_cms,log="y",col="red");

head(ordered.Flow_cms)





