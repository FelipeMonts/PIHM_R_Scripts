#############################################################################################################################
#
#  Program read binary files from the phim output.
#  Based on the Matlab program that Yuning developed for the same process
#   
#   function [time, var] = pihm_read_output_func (filename, nvar, elem)
# 
#   % filename: Name of the output file
#   % nvar: Number of elements contained. It should be either the number of
#   % triangular element or the number of river segments, depending on the file
#   % you are reading.   Triangles = 883 , River Seg =114
#   % elem: Index of elements that you want to read. If you want to read all
#   % elements, use 1:nvar.
# 
#   fid = fopen (filename);
# 
#   fseek(fid, 0, 'eof');
#   dim = ftell (fid) / 8 / (nvar + 1);
# 
#   frewind(fid);
#   rawtime = fread (fid, double (dim), 'double', nvar * 8);
# 
#   frewind (fid);
#   var = zeros (dim, length(elem));
#   for i = 1 : dim
#   fseek (fid, 8, 0);
#   data = fread (fid, double (nvar), 'double');
#   var(i, :) = data(elem);
#   end
# 
#   time = rawtime / 60.0 / 60.0 / 24.0 + datenum (1970, 1, 1);
#   fclose (fid);
# 
# #
#
#  Felipe Montes,  2017/03/29

# 
#     cd('D:/')
#     ls
#     nvar = 883
#     elem = 1:nvar
#     filename = 'WE38.recharge.dat'
# 
# 
# 
#     %   function [time, var] = pihm_read_output_func (filename, nvar, elem);
#     % 
#     %   % filename: Name of the output file
#     %   % nvar: Number of elements contained. It should be either the number of
#     %   % triangular element or the number of river segments, depending on the file
#     %   % you are reading.   Triangles = 883 , River Seg =114
#     %   % elem: Index of elements that you want to read. If you want to read all
#     %   % elements, use 1:nvar.
#     % 
#     fid = fopen('WE38.recharge.dat');
# 
#     fseek(fid, 0, 'eof'); % fseek / Move to specified position in file / eof = end of file 
#     dim = ftell (fid) / 8 / (nvar + 1); % ftell  /Position in open file / divided by 8 bytes per entry/
#     % divided by the number triangle elements (nvar = 883)
#     % results in the number of time steps the ouput was recorded (if in days for one year is 366 lines) %
#   
#   
#     frewind(fid);  %  frewind / Move file position indicator to beginning of open file
#     rawtime  = fread (fid, double (dim), 'double', nvar * 8) % fread / Read data from binary file / fread(fileID,sizeA,precision,skip)
# 
#     frewind (fid); %  frewind / Move file position indicator to beginning of open file
#     var = zeros (dim,length(elem)); %  zeros / Create array of all zeros / 
#     %zeros(sz1,...,szN) returns an sz1-by-...-by-szN array of zeros where sz1,...,szN indicate the size of each dimension. For example, zeros(2,3) returns a 2-by-3 matrix
#     for i = 1 : dim
#     fseek (fid, 8, 0);
#     data = fread (fid, double (nvar), 'double');
#     var(i, :) = data(elem);
#     end
#     % 
#     time = rawtime / 60.0 / 60.0 / 24.0 + datenum (1970, 1, 1);
#     %   fclose (fid);

#
##############################################################################################################################


############################### Record Time To start##########################################################



TimeStart<-Sys.time()  ;



###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

#  Set Working directory


setwd("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_R_Scripts/MM_PIHM_outputs") ; 

############### Create a project directory where all processed results will be stored#############

# Output.Project<-c("HansYoust.1711211109")  ;

Output.Project<-c("Project.1804102016")  ;

dir.create(paste0("./",Output.Project)) ;
####


###############################################################################################################
#                         Call packages needed to process the data 
#                             
###############################################################################################################

library(foreign) ;
library(dplyr) ;
library(hexView) ;
library(lattice) ;
library(rgdal);

library(sp);

library(RColorBrewer) ;

library(lattice) ;

library(ggplot2)  ;

library(rgeos)   ;

library(tmap) ;

library(dplyr)  ;

library(tidyr)  ;


###############################################################################################################
#                      Read binary files from PIHM Output
###############################################################################################################

############  Table 2: Description of PIHM output variables.  ########


# Variable Extension Description Unit
# SURF surf Surface water level m
# UNSAT unsat Unsaturated water storage m
# GW gw Element groundwater level m
# RIVSTG stage River stage m
# RIVGW rivgw River groundwater level m
# SNOW snow Water-equivalent snow depth m
# CMC is Canopy interception m
# INFIL infil Infiltration rate m s???1
# RECHARGE recharge Recharge rate m s???1
# EC ec Canopy evaporation m s???1
# ETT ett Total transpiration m s???1
# EDIR edir Soil evaporation m s???1
# RIVFLX0 rivflx0 Longitudinal flow to river m3 s???1
# RIVFLX1 rivflx1 Longitudinal flow from river m3 s???1
# RIVFLX2 rivflx2 Lateral overland flow to river from left m3 s???1
# RIVFLX3 rivflx3 Lateral overland flow to river from right m3 s???1
# RIVFLX4 rivflx4 Lateral groundwater flow to river from left m3 s???1
# RIVFLX5 rivflx5 Lateral groundwater flow to river from right m3 s???1
# RIVFLX6 rivflx6 Leakage flow from river to aquifer m3 s???1
# RIVFLX7 rivflx7 Longitudinal flow to river aquifer m3 s???1
# RIVFLX8 rivflx8 Longitudinal flow from river aquifer m3 s???1
# RIVFLX9 rivflx9 Lateral groundwater flow to aquifer from left m3 s???1
# RIVFLX10 rivflx10 Lateral groundwater flow to aquifer from right m3 s???1
# SUBFLX subflx[0-2] Subsurface water flux m3 s???1
# SURFFLX surfflx[0-2] Surface water flux m3 s???1


###############################################################################################################
#                      Read binary files from River Segments
###############################################################################################################


list.files(paste0("./",Output.Project))   ;

file_info<-file.info(paste0('./',Output.Project,'/', 'Project.rivflx0.dat'))   ; # information about the binary file

word.size=8 ;  # eight bytes per word

NUMRIV=241 ; # number of river elements in the output simulation

No.Rows<-file_info$size/word.size/(NUMRIV+1) ; # number of rows obtained by dividing the file size in bytes
# by wordsize*(Tiangle.no +1 (time stamp))  

Read_data<-readBin(paste0('./',Output.Project,'/', 'Project.rivflx0.dat') , "numeric" , size=word.size, n=file_info$size/word.size) ; # read the binary file

rivflx0<-as.data.frame(matrix(data=Read_data, ncol=NUMRIV+1, byrow=T)); # create a matrix with the binary file
# and then transforme it into a dataframe.


rivflx0$Time<-as.POSIXct(rivflx0[,1], origin="1970-01-01", tz="UTC") ;
str(rivflx0)

plot(rivflx0$Time, rivflx0[,3] , type= "l" , col="RED") ;

xyplot()




###############################################################################################################
#                      Read binary files from triangles
###############################################################################################################

list.files(paste0("./",Output.Project))   ;


file_info<-file.info(paste0('./',Output.Project,'/', 'Project.gw.dat'))   ; # information about the binary file

word.size=8 ;  # eight bytes per word

Triangle.no=NumEle=1958 ; # number of triangles in the output simulation

No.Rows<-file_info$size/word.size/(Triangle.no+1) ; # number of rows obtained by dividing the file size in bytes
# by wordsize*(Tiangle.no +1 (time stamp))  

Read_data<-readBin(paste0('./',Output.Project,'/', 'Project.gw.dat') , "numeric" , size=word.size, n=file_info$size/word.size) ; # read the binary file

gw<-as.data.frame(matrix(data=Read_data, ncol=Triangle.no+1, byrow=T)); # create a matrix with the binary file
# and then transforme it into a dataframe.


gw$Time<-as.POSIXct(Recharge[,1], origin="1970-01-01", tz="UTC") ;
str(gw)

plot(gw$Time,gw[,3])



###############################################################################################################
#                              Read Mesh Triangle Shape files 
###############################################################################################################



########### Read infromation about the shape files ###########

Project.mesh.info<-ogrInfo("C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/3DomainDecomposition/MergeVectorLayer000_q25_a100000.shp")  ; 



### The number of polygons in the shape file is  Project.mesh.info$nrows

Project.mesh.info$nrows

#### read the shape file that has been created in QGIS using the zonal statistics

# HansYoust.GSSURGO<-readOGR("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/GSSURGO/HY_GSURGO.shp")  ;

Project.mesh<-readOGR("C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/3DomainDecomposition/MergeVectorLayer000_q25_a100000.shp")  ;  




###############################################################################################################
#                              Read River Shape files 
###############################################################################################################


Project.river.info<-ogrInfo("C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/Stream8000_sln32_dens150m_xln_Decomp.shp")   ;


### The number of river segments in the shape file is  Project.river.info$nrows

Project.river.info$nrows


Project.river<-readOGR('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Aug2920171550/2VectorProcessing/Stream10000_sln30_xln_Decomp.shp' )


###############################################################################################################
#                             Plot the triangle mesh and the river
###############################################################################################################

plot(Project.mesh, col="light green",lwd=1);
lines(Project.river,col="BLUE",lwd=2) ;
title("Project MESH & River") ;



###############################################################################################################
#                             Other Exploratory plots
###############################################################################################################



#### Select a time Range that it is interesting to explore

DateTime.range<-c(GW$Time[1],GW$Time[length(GW$Time)]); #Based on a specific range of data that one might want to explore


Rows.Time.range<-which(GW$Time >= DateTime.range[1] & GW$Time <= DateTime.range[2] ); 
Columns.Time.range<-which(GW$Time >= DateTime.range[1] & GW$Time <= DateTime.range[2] ); 

# Only select columns 2 (the first is the TS) to 242+1 (the number of mesh elemnts), the rest is the channel ground water storage (32 elements)

GW.Land<-GW[Rows.Time.range,2:(Output.Project.mesh.info$nrows+1)] ;




# Find the largest and smaller values in the data set for the land segments

GW.Land.range<-range(GW.Land) ;


# plot and explore the ground water time series   
plot(GW$Time[Rows.Time.range],GW.Land[,1],type="l", ylim=GW.Land.range);

for (i in seq(2,dim(GW.Land)[2])) {
  points(GW$Time[Rows.Time.range],GW.Land[,i],type='l',col=i ) ;
} ;


## find the riangle with the greTEST gw LEVEL

GW.Max.Triangle<-which.max(unlist(t(GW.Land))) - (floor(which.max(unlist(t(GW.Land)))/dim(GW.Land)[2])*dim(GW.Land)[2])




GW.Land[,GW.Max.Triangle]


####### attach the GW data to the shape file as attibutes


GW.TimeSeries<-as.data.frame(t(GW.Land))

row.names(GW.TimeSeries)<-seq(1,dim(GW.TimeSeries)[1]) ;


GW.spdf<-SpatialPolygonsDataFrame(Output.Project.mesh,GW.TimeSeries, match.ID = T);

###### Name the columns of the attribute data frame according to the date range

names(GW.spdf@data)<-c(as.character(GW$Time[Rows.Time.range]))

GW$Time[Rows.Time.range]

dim(GW.Land)


Here I Stopped on 2017 11 21







###### Get the Polygons IDs and labeling coordinates

WE38.ID<-sapply(slot(WE38.GW.spdf,"polygons"),function(x) slot(x,"ID")) ;
WE38.labpt<-sapply(slot(WE38.GW.spdf,"polygons"),function(x) slot(x,"labpt")) ;



# #  Plot the Spatial polygonDataFrame with the Lattice package
# 
# plot(WE38.GW.spdf);
# text(coordinates(t(WE38.labpt)),WE38.ID);
# 
# Colors.in.map<-brewer.pal(12,"Set3")
# spplot(WE38.GW.spdf,Columns,main="WE38 Soil Water");




#################### using the Package tmap to plot the spatial data

##################### creating a color pallete that shows more than 12 colors

color.palette<- colorRampPalette(c("yellow","green", "blue"), space="rgb");
pie(rep(1,40),col=color.palette (40) )  ;


#####   Plotting the map

####GEt the Polygon ID 
WE38.GW.spdf$polygonID<-sapply(slot(WE38.GW.spdf,"polygons"),function(x) slot(x,"ID")) ;

#### Plotting the polygon shape file using the tmap

qtm(shp=WE38.GW.spdf, fill="2008-03-04", fill.palette=colorRampPalette(c("yellow","green", "blue"), space="rgb")(5),text="polygonID"); 

WE38.river$LineID<-sapply(slot(WE38.river,"lines"),function(x) slot(x,"ID"))  ;

tm_shape(WE38.GW.spdf) +
  tm_fill("2008-03-04", palette=colorRampPalette(c("yellow","green", "blue"), space="rgb")(11), n=10 )+ 
  #     tm_text("polygonID")
  tm_shape(WE38.river) + ##########Plot the river and other lines
  tm_lines(col="blue",lwd=3)
#     tm_text("LineID")


############## Plotting a series of maps for a rain storm simulation  ###########

i<-as.character(DateTime[Rows.Time.range]) [14]

tm_shape(WE38.GW.spdf)+
  tm_borders(col = "Black", lwd = 1, lty = "solid", alpha = NA) +   
  tm_fill(i, palette=colorRampPalette(c("lightblue", "blue","darkblue"), space="rgb")(9))+ 
  #     tm_text("polygonID")
  tm_shape(WE38.river) + ##########Plot the river and other lines
  tm_lines(col="blue",lwd=3)
#     tm_text("LineID")






#################  Plotting  surface flow to stream from left terrain .rivFlx2   #################################

##########    Read the .rivFlx2  data ######### 

WE38.rivFlx2<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx2.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx2)

# Find the largest and smaller values in the data set 

WE38.rivFlx2.range<-range(WE38.rivFlx2[,seq(2,dim(WE38.rivFlx2)[2])]) ;


# plot and explore .rivFlx2  time series   
plot(DateTime,WE38.rivFlx2[,2],type="l",ylim=WE38.rivFlx2.range);

for (i in seq(3,dim(WE38.rivFlx2)[2])) {
  points(DateTime,WE38.rivFlx2[,i],type='l',col=i ) ;
} ;


##########    Read the .rivFlx4  data ######### 

WE38.rivFlx4<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx4.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx4)

# Find the largest and smaller values in the data set 

WE38.rivFlx4.range<-range(WE38.rivFlx4[,seq(2,dim(WE38.rivFlx4)[2])]) ;


# plot and explore .rivFlx4  time series   
plot(DateTime,WE38.rivFlx4[,2],type="l",ylim=WE38.rivFlx4.range);

for (i in seq(3,dim(WE38.rivFlx4)[2])) {
  points(DateTime,WE38.rivFlx4[,i],type='l',col=i ) ;
} ;



##########    Read the .rivFlx6  data ######### 

WE38.rivFlx6<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx6.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx6)

# Find the largest and smaller values in the data set 

WE38.rivFlx6.range<-range(WE38.rivFlx6[,seq(2,dim(WE38.rivFlx6)[2])]) ;


# plot and explore .rivFlx6  time series   
plot(DateTime,WE38.rivFlx6[,2],type="l",ylim=WE38.rivFlx6.range);

for (i in seq(3,dim(WE38.rivFlx6)[2])) {
  points(DateTime,WE38.rivFlx6[,i],type='l',col=i ) ;
} ;



##########    Read the .rivFlx7  data ######### 

WE38.rivFlx7<-read.table(paste0(".\\output","\\",PIHM.runs[1],"\\","WE38.rivFlx7.txt"),header=F,as.is=T) ;

dim(WE38.rivFlx7)

# Find the largest and smaller values in the data set 

WE38.rivFlx7.range<-range(WE38.rivFlx7[,seq(2,dim(WE38.rivFlx7)[2])]) ;


# plot and explore .rivFlx7  time series   
plot(DateTime,WE38.rivFlx7[,2],type="l",ylim=WE38.rivFlx7.range);

for (i in seq(3,dim(WE38.rivFlx7)[2])) {
  points(DateTime,WE38.rivFlx7[,i],type='l',col=i ) ;
} ;


