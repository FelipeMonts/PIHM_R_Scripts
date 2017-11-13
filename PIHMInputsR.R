###########    Program to import and create PIHM input files from Hydroterre services
###########    Felipe Montes 2015 03 03

###########   Updated 2017 10 18 ######################
#Added the code to the PIHM_R_Scripts repository in GitHub and updated the directories

###########    Felipe Montes 2015 06 27 Update: Added a function to save the objects produced in the file PIHMInputsR.RData on the working directory. In this way these objects can be read by another program and be modified easily to produce the desired output. For example if different output format is needed for PIHMV2.2 and MM-PIHM. Also, modified the file paths to be relative file paths as suggested in Gandrud, Christopher. 2013. Reproducible Research with R and R Studio. Boca Raton: Chapman and Hall/CRC. 


##########    Felipe Montes 2017 03 01 Update: Added function to be able to use the directory in which PIHM outputs are saved and read all of the input files into PIHM

#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/R_Library/library")





#      set the working directory
#     setwd("./PIHMInputsR");

#Project.Directory<-"C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\WE38_Files_PIHM_Cycles20170208\\Feb2720171451" ;

Windows.Directory<-gsub("\\\\", "/", readClipboard())
#  C:\Felipe\PIHM-CYCLES\PIHM\PIHM_Felipe\CNS\Manhantango\HydroTerreFullManhantango\HansYostDeepCreek\Aug2920171550



Project.Directory<-Windows.Directory

setwd(Project.Directory)    ;

#Store the name of the directory where the inputs are:

DataModel.dir<-"4DataModelLoader" ;


#Store  the name of the project :


Project<-"MergeVectorLayer000_q30_a200000"


#  read already created files from PIHM examples to study their structure

# Create the path to read the input files by pasting DataModel.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-(paste(".",DataModel.dir,Project, sep="/")) ;

# ****************************************List all the files in the input directory****************************************************************

list.files(paste(".",DataModel.dir, sep="/"))    ;


############## The list of files that PIHM Need - Master list  ###################

PIHMMasterFiles<-c(".ATT" , ".CALIB" , ".FORC" , ".GEOL" , ".IBC" , ".INIT" , ".LC" , ".LSM", ".MESH", ".PARA" , ".RIV" , ".SOIL")   ;


# ****************************************READ THE MESH FILE .mesh****************************************************************


#   Because the Messh file has in the first line the number of elements (NumEle) and the number of nodes (NumNode), we can use that information to read the table more efficiently

mesh.NumEle.NumNode<-read.table(paste0(inputfile.name, ".MESH0"),as.is=T,nrows=1,skip=0, col.names=c("NumEle","NumNode"));

NumEle<-mesh.NumEle.NumNode$NumEle;
NumNode<-mesh.NumEle.NumNode$NumNode;

# The mesh file has the following structure: first it lists the Elements and then it lists the nodes.
# The elements part have the following structure: Index Node[0] Node[1] Node[2] Nabr[0] Nabr[1] Nabr[2]
# The nodes part has the folowing structure: Index X Y Zmin Zmax
# Zmax is the surface elevation of the node and Zmin is the bed elevation of the node
# see the PIHM2x_input_file_format.pdf file

mesh.Elements<-read.table(paste0(inputfile.name, ".MESH0"),as.is=T,skip=1, nrows=NumEle,col.names=c('Index', 'Node.0', 'Node.1', 'Node.2', 'Nabr.0', 'Nabr.1', 'Nabr.2'));


mesh.Nodes<-read.table(paste0(inputfile.name, ".MESH0"),as.is=T,skip=NumEle+1, nrows=NumNode, col.names=c('Index','X','Y','Zmin','Zmax'));





# *************************************READ THE ATTRIBUTES FILE .att  *************************************************************




# The attribute file has 22 varibles with attributes values: Index Soil Geol LC IS_IC Snw_IC Srf_IC Ust_IC St_IC Ppt Tmp RH Wnd Rn G VP S mF BC[0] BC[1] BC[2] mP

# Variable Name: Variable Type: Variable Description: Remarks:
# Index Integer Element Index
# Soil Integer Soil Class
# Geol Integer Geology Class
# LC Integer Land Cover Class
# IS_IC Integer Interception Storage Initial Condition
# Snw_IC double Snow Accumulation Initial Condition
# Srf_IC double Surfaceflow State Initial Condition
# Ust_IC double Usaturated State Initial Condition
# St_IC double Saturated State Initial Condition
# Ppt Integer Precipitation Series
# Temp Integer Temperature Series
# RH Integer Rel. Humidity Series
# Wnd Integer Wind Velocity Series
# Rn Integer Solar Radiation Series
# G Integer Dummy
# VP Integer Vapor Pressure
# S Integer Source/Sink
# mF Integer Melt Factor Series
# BC[0] Integer
# Boundary condition Type on
# edge
# BC[1] Integer .........
# BC[2] Integer ........
# mP Integer Macropore present or not 1:Yes/ 0: No


att<-read.table(paste0(inputfile.name, ".ATT"),as.is=T,col.names=c('Index', 'Soil', 'Geol','LC','IS_IC', 'Snw_IC', 'Srf_IC', 'Ust_IC', 'St_IC', 'Ppt', 'Tmp', 'RH', 'Wnd', 'Rn', 'G', 'VP', 'S', 'mF', 'BC.0', 'BC.1', 'BC.2', 'mP'));


head(att)

# ******************************************* READ HYDROTERRE SOIL FILE  .soil***********************************************************




####  The Hydroterre Soil.txt file has the information extracted from Hydroterre, that passes to PIHM-GIS to calculate soil properties and form the PIHM input .soil file

HT_soil<-read.table(file = "../GSSURGO/HansYoust_Soil.txt", as.is=T, header=T, skip=1) ;


# ******************************************READ SOIL FILE  .soil***********************************************************


# The soil file has 9 columns with attributes as follows: Index KsatV ThetaS ThetaR infD Alpha Beta hAreaF macKsatV,
# It starts with single column in the first row, NumSoil, which indicates the Number of soil classes

# Variable Name: Variable Type: Variable Description: Remarks
# NumSoil Integer Number of Soil Classes
# Index Integer Soil Class Number Beginning with 1
# KsatV* Double
# Vertical Saturated Hydraulic
# Conductivity
# ThetaS Double Porosity
# ThetaR Double Residual Porosity
# infD Double
# Top soil layer across which infiltration is
# calculated, Generally set to 0.1 m
# Alpha* Double Van Genuchten Soil Parameter
# Beta* Double Van Genuchten Soil Parameter
# hAreaF Double Horizontal Area Fraction of Macropore
# macKsatV Double
# Vertical macropore Hydraulic
# conductivity
# 

## NumSoil<-read.table(paste0(inputfile.name, ".soil"),as.is=T,nrows=1)[1,1];  ####  comented just for the PIHM WE38 files, not comment to read the regular files

#soil.header<-read.table(paste0(inputfile.name," (2)", ".soil"),as.is=T,nrows=1) ;

#NumSoil<-soil.header[1,1]; 

NumSoil<-dim(HT_soil)[1] ;

#soil.col_names<-soil.header[1,-1]  ;


## soil<-read.table(paste0(inputfile.name, ".soil"),as.is=T,skip=1,col.names=c('Index', 'KsatV', 'ThetaS', 'ThetaR', 'infD', 'Alpha', 'Beta', 'hAreaF', 'macKsatV'));

                   
soil<-HT_soil ;




# ********************************************READ THE GEOLOGY FILE  .geol*****************************************************************
HT_Geology<-read.table(file = "../GSSURGO/HansYoust_Geology.txt", as.is=T, header=T, skip=1) ;


# The geology file is very similar to the soil file. The geology file has 11 columns with attributes as follows:
# Index KsatH KsatV ThetaS ThetaR infD Alpha Beta vAreaF macKsatH macD
# It starts with single column in the first row, NumGeol, which indicates the Number of geology classes

# Variable Name: Variable Type: Variable Description: Remarks
# NumGeol Integer Number of Geology Classes
# Index Integer Geology Class Number Beginning with 1
# KsatV Double
# Vertical Saturated Hydraulic
# Conductivity
# ThetaS Double Porosity
# ThetaR Double Residual Porosity
# infD Double
# Top soil layer across which infiltration is
# calculcated
# Generally set to 0.1
# m
# Alpha Double Van Genuchten Soil Parameter
# Beta Double Van Genuchten Soil Parameter
# vAreaF Double Vertical Area Fraction of Macropore
# macKsatH Double
# Horizontal macropore Hydraulic
# conductivity
# macD Double Macropore Depth


#geol<-read.table(paste0(inputfile.name, ".geol"),as.is=T,skip=1,fill=T,col.names=c('Index', 'KsatH','KsatV', 'ThetaS', 'ThetaR', 'infD', 'Alpha', 'Beta', 'vAreaF', 'macKsatH','macD'));


 geol<-HT_Geology ;
# *****************************************READ THE LAND COVER FILE .lc **********************************************************


# the land cover file starts with single column in the first row, NumLC, which indicates the Number of land cover classes
# It has 8 columns with attribute values: Index LAImax Rmin Rs_ref Albedo VegFrac n RzD

# Variable Name: Variable Type: Variable Description: Remarks
# NumLC Integer Number of Lanc Cover Classes
# Index Integer Land Cover Class Number
# LAImax Double Maximum LAI
# Rmin Double Minimum Stomatal Resistance
# Rs_ref Double Reference Stomatal Resistance
# Albedo Double Albedo
# VegFrac Double Vegetation Fraction
# n Double Manning's Roughness Coefficient day/m1/3
# RzD Double Root Zone Depth


# NumLC<-read.table(paste0(inputfile.name, ".lc"),as.is=T,nrows=1)[1,1];
# 
# lc<-read.table(paste0(inputfile.name, ".lc"),as.is=T,skip=1,col.names=c('Index', 'LAImax', 'Rmin', 'Rs_ref', 'Albedo', 'VegFrac', 'n', 'RzD'));


# *************************************************READ THE RIVER FILE . riv  ****************************************************



# The river file has several blocks of data in the same file
# It starts with a single column in the first row names NumRiv, which indicates the number of river segments
# Then is has a block of data with the river elements and nodes. This block has the following variables:
# Index FromNode ToNode Down LeftEle RightEle Shape Material IC BC Res

# Variable Name: Variable Type: Variable Description: Remarks
# NumRiv Integer Number of River Segments
# Index Integer River Segment ID Beginning with 1
# FromNode Integer From Node ID
# ToNode Integer To Node ID
# Down Integer Downstream Segment ID
# LeftEle Integer Left Element ID
# RightEle Integer Right Element ID
# Shape Integer Shape ID
# Material Integer Material ID
# IC Integer Initial Condition ID
# BC Integer Boundary Condition ID
# Res Integer Reservoir ID


NumRiv<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,nrows=1)[1,1];


riv.elements<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1,nrows=NumRiv,col.names=c('Index', 'FromNode', 'ToNode', 'Down', 'LeftEle', 'RightEle', 'Shape', 'Material', 'IC', 'BC', 'Res'));

# the the files continues with a block containing shape attributes. The block starts with a row with two coulms "Shape" and "NumShape"
# Then it is followed by a list of attributes regarding the shape: Index Depth InterpOrd WidCoeff

# Variable Name: Variable Type: Variable Description: Remarks
# Index Integer Shape ID Beginning with 1
# Depth Double Depth of the River Segment
# InterpOrder Integer Interpolation Order * 1 if a rectangular
# WidCoeff Double Width Coefficient * width if a rectangular
# * Interpolation Order (b) and Widht Coefficient (a) are parameters defining relation between Width and Depth of a river segment as: [D = a x (W/2)b].

shape<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1+NumRiv,nrows=1);

NumShape<-shape[1,2];

riv.shape<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1+NumRiv+1,nrows=NumShape,col.names=c('Index', 'Depth', 'InterpOrd', 'WidCoeff'));


# Afterwards the file continues with the Material information for the river shapes. The block starts with a row with two coulms "Material" and NumMat
# The file continues with 6 columns with the following attribute values: Index n Cwr KsatH KsatV Bed

# Variable Name: Variable Type: Variable Description: Remarks
# NumMat Integer Number of Material Types
# Index Integer Material ID Beginning with 1
# n Double Manning's Roughness Coefficient
# Cwr Double Discharge Coefficient
# KsatH Double Side (Horizontal) Hydraulic Conductivity in m day-1
# KsatV Double Bed Hydraulic Conductivity m day-1
# Bed Double Bed Depth



Material<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1+NumRiv+1+NumShape,nrows=1);

NumMat<-Material[1,2];

riv.material<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1+NumRiv+1+NumShape+1,nrows=NumMat,col.names=c('Index', 'n', 'Cwr', 'KsatH','KsatV','Bed'));


# The river file then continues with a block describing the initial conditions for the river elements.
# The block starts with a row with two coulms "IC" and NumIC
# Then it has a block of with two coulms: Index Value


# Variable Name: Variable Type: Variable Description: Remarks
# NumIC Integer Number of Initial Condition Types
# Index Integer Initial Condition ID Beginning with 1
# Value Double Intial Condition Water Table

IC<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1+NumRiv+1+NumShape+1+NumMat,nrows=1);

NumIC<-IC[1,2]; 

riv.IC<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=NumRiv+1+NumShape+1+1+NumMat+1,nrows=NumIC,col.names=c('Index', 'Value'));


# Finaly the river file contains the boundary condition information
# It starts with a row with two coulms "BC" and NumBC. NumBC indicates the number of boundary conditions time series. In this example the NumBC=0, therefore there is no information about the boundary conditions. 
# see the PIHM2x_input_file_format.pdf file

BC<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1+NumRiv+1+NumShape+1+NumMat+1+NumIC,nrows=1);

NumBC<-BC[1,2];


# Finally the river file has a last line with a double coumn and following variables "Res" NumRes
# this information indicates the number of reservoirs



Res<-read.table(paste0(inputfile.name, ".RIV"),as.is=T,skip=1+NumRiv+1+NumShape+1+NumMat+1+NumIC+max(c(NumBC,1)),nrows=1);


# *****************************************************READ THE FORCING FILE .forc ********************************************


# The forcing file starts with a single line and 10 columns as follows:NumPrep NumTemp NumRH NumWind NumRn NumG NumVP NumLC NumMF NumSS

# Variable Name: Variable Type: Variable Description: Remarks
# NumPrep Integer Number of precipitation time-series
# NumTemp Integer Number of temperature time-series
# NumRH Integer Number of relative humidity time-series
# NumWind Integer Number of wind velocity time-series
# NumRn Integer Number of solar radiation time-series
# NumG - Dummy
# NumVP Integer Number of vapor pressure time-series
# NumLAI Integer Number of LAI time-series
# NumMF Integer Number of melt factor time-series
# NumSS Integer Number of source/sink
# Index Integer Time-series ID
# Length Integer Number of time steps
# Time Double Time
# Value Double Data value
# Height Double Height of wind velocity observation
# IsFactor Double Interception Storage Factor


#Read the first few lines of the forcing file

METEO.header.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T,nrows=1)   ;

METEO.header.2<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, nrows=1, skip=1)   ;

METEO.header.3<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, nrows=2, skip=2)   ;


METEO.col.names<-data.frame(METEO.header.3[1],METEO.header.3[1,])[1,]   ;
     
     
forc<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=4, header=F, col.names=METEO.col.names) ;

head(forc)

str(forc)


# #forc<-read.table(paste0(inputfile.name, ".FORC"),as.is=T,nrows=1, col.names=c('NumPrep', 'NumTemp', 'NumRH', 'NumWind', 'NumRn', 'NumG', 'NumVP', 'NumLC', 'NumMF', 'NumSS'));
# 
# NumPrep<-forc[1,1];
# 
# # Then The file continues with a line with the following parameters; "Prep" Index Length. These mark the start of the precipitation time series, indicathe the Index (spatial index of the particular forcing time series)  and the length of it (number of records). 
# 
# 
# Prep.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=1,nrows=1,col.names=c('Prep','Index','Length'));
# 
# # Then the file continues with the records of the first precipitation time series, in the form: Time  Value"
# # Afterwards continues with the records of the second precipitation time series (if present according to NumPrep)
# 
# Prep.1.length<-Prep.1[1,3]
# 
# forc.Prep.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2,nrows=Prep.1.length,col.names=c('Time','Value'));


# After the last precipitation time series the file contains similar format for the Temperature, Relative humidity, Wind, Net radiation, Vapor Pressure, Leaf area index, Melt factor time series, Source sink time series

# Variable Name: Variable Type: Variable Description: Remarks
# NumPrep Integer Number of precipitation time-series
# NumTemp Integer Number of temperature time-series
# NumRH Integer Number of relative humidity time-series
# NumWind Integer Number of wind velocity time-series
# NumRn Integer Number of solar radiation time-series
# NumG - Dummy
# NumVP Integer Number of vapor pressure time-series
# NumLAI Integer Number of LAI time-series
# NumMF Integer Number of melt factor time-series
# NumSS Integer Number of source/sink
# Index Integer Time-series ID
# Length Integer Number of time steps
# Time Double Time
# Value Double Data value
# Height Double Height of wind velocity observation
# IsFactor Double Interception Storage Factor

# Temp.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length,nrows=1,col.names=c('Temp','Index','Length'));
# 
# Temp.1.Length<-Temp.1[1,3];
# 
# forc.Temp.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2,nrows=Temp.1.Length,col.names=c('Time','Value'));
# 
# 
# # Read relative Humidity
# 
# 
# RH.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length,nrows=1,col.names=c('RH','Index','Length'));
# 
# RH.1.Length<-RH.1[1,3];
# 
# forc.RH.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2,nrows=RH.1.Length,col.names=c('Time','Value'));
# 
# 
# # Read Wind
# 
# 
# Wind.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length,nrows=1,col.names=c('Wind','Index','Length','Height'));
# 
# Wind.1.Length<-Wind.1[1,3];
# 
# forc.Wind.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2,nrows=Wind.1.Length,col.names=c('Time','Value'));
# 
# # Read Net Radiation
# 
# Rn.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length,nrows=1,col.names=c('Rn','Index','Length'));
# 
# Rn.1.Length<-Rn.1[1,3];
# 
# forc.Rn.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2,nrows=Rn.1.Length,col.names=c('Time','Value'));
# 
# 
# # Read Vapor pressure
# 
# Vp.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2+Rn.1.Length,nrows=1,col.names=c('VP','Index','Length'));
# 
# Vp.1.Length<-Vp.1[1,3];
# 
# forc.Vp.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2+Rn.1.Length+2,nrows=Vp.1.Length,col.names=c('Time','Value'));
# 
# 
# # Read Leaf Area index for LC 1
# 
# LAI.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2+Rn.1.Length+2+Vp.1.Length,nrows=1,col.names=c('LAI','Index','Length', 'IsFactor'));
# 
# # IsFactor is the Interception storage factor
# 
# LAI.1.Length<-LAI.1[1,3];
# 
# forc.LAI.1<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2+Rn.1.Length+2+Vp.1.Length+2,nrows=LAI.1.Length,col.names=c('Time','Value'));
# 
# # Read Leaf Area index for LC 2
# 
# LAI.2<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2+Rn.1.Length+2+Vp.1.Length+2+LAI.1.Length,nrows=1,col.names=c('LAI','Index','Length', 'IsFactor'));
# 
# 
# LAI.2.Length<-LAI.2[1,3];
# 
# forc.LAI.2<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2+Rn.1.Length+2+Vp.1.Length+2+LAI.1.Length+2,nrows=LAI.2.Length,col.names=c('Time','Value'));
# 
# # Read Leaf Area index for LC 3
# # Read Leaf Area index for LC 4 (NumLC=4)
# 
# 
# 
# # Then it needs to read Roughness length (RL), also called displacement height DH in some PIHM versions
# 
# 
# # Locate the start of the RL time series after all the blocks of LC time series are acounted for.
# 
# Leftover.table<-read.table(paste0(inputfile.name, ".FORC"),as.is=T, fill=T, skip=2+Prep.1.length+2+Temp.1.Length+2+RH.1.Length+2+Wind.1.Length+2+Rn.1.Length+2+Vp.1.Length+2+LAI.1.Length+2+LAI.2.Length);
# 
# # Locate the start of the RL 1 time series
# RL.1.Start<-Leftover.table[which(Leftover.table=="RL"),];
# 
# # The RL factor data series Starts with a three column line with the following values: "RL" Index Length.
# 
# RL.1.length<-RL.1.Start[1,3];
# 
# # Locate the row where MF data series start
# 
# RL.1.Start.row<-as.numeric(row.names(RL.1.Start[1,]));
# 
# # store the melting factor time series from it's start to the length of the time series
# 
# forc.RL.1<-Leftover.table[seq(RL.1.Start.row+1,RL.1.Start.row+RL.1.length),c(1,2)]
# 
# 
# 
# # Finaly it needs to read the Melting Factor time Series (MF)  
# 
# 
# 
# # Here we first read the rest of the records we have not yet read and select only the ones corresponding to the MF
# 
# # Locate the start of the MF time series
# MF.Start<-Leftover.table[which(Leftover.table=="MF"),];
# 
# # The melting factor data series Starts with a three column line with the following values: "MF" Index Length.
# 
# MF.length<-MF.Start[1,3];
# 
# # Locate the row where MF data series start
# 
# MF.Start.row<-as.numeric(row.names(MF.Start));
# 
# # store the melting factor time series from it's start to the length of the time series
# 
# forc.MF<-Leftover.table[seq(MF.Start.row+1,MF.Start.row+MF.length),c(1,2)];
# 

# ********************************************************READ THE BOUNDARY CONDITIONS FILE .ibc ****************************************




# IBC file contains all the information related to boundary conditions corresponding to elements

# The file structure is as follows

# NumBC1 NumBC2
# "BC1" Index Length
# Time Value
# Repeat Length times.

# Variable Name Variable Type Variable Description Remarks
# NumBC1 Integer Number of Dirichlet BC
# NumBC2 Integer
# Number of Neumann
# BC
# Index Integer Boundary Condition ID
# Length Integer Number of time steps
# Time Double Time
# Value Double Value (m or m/day)

ibc<-read.table(paste0(inputfile.name,".IBC"),as.is=T, fill=T);

ibc.NumBC1<-ibc[1,1];
ibc.NumBC2<-ibc[1,2];

# Because in the example NumBC1 and NumBC2 are 0 then no other information is read


# ****************************************************READ THE SUNDIALS PARAMETER FILE .para********************************************



# Para file provides all the control data to the model. It contains solver options; model modes; also parameters that govern model error.


# File Structure:
# Verbose Debug Init_type
# PgwD PsurfD PsnowD PrivStg
# PRech PIsD PusD
# Pet0 Pet1 Pet2
# Priv0 Priv1 Priv2 Priv3 Priv4 Priv5 Priv6 Priv7 Priv8 Priv9 Priv10
# gwDInt surfDInt snowDint rivStgInt
# RechInt IsDInt usDInt etInt rivFlxInt
# UsatMode SatMode RivMode
# Solver GSType MaxK Delta
# AbsTol RelTol InitStep MaxStep ETstep
# StartTime EndTime Output
# a b

# Variable Name: Variable Type: Variable Description: Remarks
# Verbose: Integer: Verbose mode?: Yes/No :: 1/0
# Debug: Integer: Debug mode?: Yes/No :: 1/0
# Init_type: Integer: State initialization type: Relax(0); AttFile(1); InitFile(3)

# PgwD, PsurfD, PsnowD, PrivStg, Prech, PisD, PusD, Pet0, Pet1,Pet2 : Integer : Print: Groundwater, Surface, Water, Snow, River Stage, Rechage to Ground Water, Interception Storage, Unsaturated Storage, Interception Loss, Transpiration, Evaporation from, Ground: Yes/No :: 1/0
# Priv0, Priv1: Integer: Print: Longitudonal {Flow To,Flow from} a river element: Yes/No :: 1/0
# Priv2, Priv3: Integer Print: Lateral Overland Flow To a river element from {Left, Right}: Yes/No :: 1/0
# Priv4, Priv5: Integer Print: Lateral Groundwater Flow To a river element from {Left, Right} :Yes/No :: 1/0
# Priv6: Integer Print: Leakage/Base Flow To/From aquifer: Yes/No :: 1/0
# Priv7, Priv8: Integer Print: Longitudonal {Flow To, Flow from} a aquifer element beneath river: Yes/No :: 1/0
# Priv9, Priv10 Integer Print: Lateral Groundwater Flow To a aquifer element from {Left,Right} beneath river Yes/No :: 1/0
# gwDInt : integer :Print Interval: Groundwater:  Unit is in minutes
# surfDInt : integer :Print Interval: Surface Water : Unit is in minutes
# snowDint : integer :Print Interval: Snow : Unit is in minutes
# rivStgInt : integer :Print Interval: River Stage : Unit is in minutes 
# RechInt : integer :Print Interval: Rechage to Ground Water : Unit is in minutes  
# IsDInt : integer :Print Interval: Interception Storage : Unit is in minutes 
# usDInt : integer :Print Interval: Unsaturated Storage : Unit is in minutes 
# etInt : integer :Print Interval: Evapotranspiration: Unit is in minutes 
# rivFlxInt : integer :Print Interval: River Flow: Unit is in minutes 
# UsatMode: Integer : Unsaturation formulation : 2
# SatMode: Integer : Saturation formulation : Kinematic(1); Diffusion(2)
# RivMode : Integer : River formulation : Kinematic(1); Diffusion(2)
# Solver : Integer : Cvode Solver Type: Iterative(2)
# GSType : Integer : GS Solver Type : Modified(1); Classical(2)
# MaxK : Integer : Max Krylov dimension
# Delta : Double : GMRES convergence criterion
# AbsTol:  Double : Absolute Tolerance
# RelTol : Double : Relative Tolerance
# InitStep : Double : Initial time-step [see SUNDIALS manual]
# MaxStep : Double : Maximum time-step [see SUNDIALS manual]
# Etstep : Double : ET time-step
# StartTime : Double : Simulation start time
# EndTime : Double : Simulation end time
# Output : Double : Output step-size
# a * : Double : Step-size factor
# b * : Double : Base step-size
# * stepsize = b x a^i


para<-read.table(paste0(inputfile.name, ".PARA"),as.is=F, fill=T);

# This parameter file structure is very cumbersome. Need to find a way to make it reasonable
# One option is to create a list and name the components 

para.list<-list(as.vector(para[1, 1:3]),as.vector(para[2, 1:4]),as.vector(para[3, 1:3]),as.vector(para[4, 1:3]),as.vector(para[5, 1:10]),as.vector(para[6, 1:4]),as.vector(para[7, 1:5]),as.vector(para[8, 1:3]),as.vector(para[9, 1:4]),as.vector(para[10, 1:5]),as.vector(para[11, 1:3]),as.vector(para[12, 1:2]));

# Name the components of the list

names(para.list[[1]])<-c("Verbose","Debug","Init_type");
names(para.list[[2]])<-c("PgwD","PsurfD", "PsnowD", "PrivStg");
names(para.list[[3]])<-c("PRech", "PIsD"," PusD");
names(para.list[[4]])<-c("Pet0","Pet1","Pet2");
names(para.list[[5]])<-c("Priv0", "Priv1"," Priv2", "Priv3", "Priv4", "Priv5", "Priv6", "Priv7", "Priv8", "Priv9") ; #, "Priv10");
names(para.list[[6]])<-c("gwDInt", "surfDInt", "snowDint", "rivStgInt");
names(para.list[[7]])<-c("RechInt", "IsDInt", "usDInt", "etInt", "rivFlxInt");
names(para.list[[8]])<-c("UsatMode", "SatMode", "RivMode");
names(para.list[[9]])<-c("Solver", "GSType", "MaxK Delta");
names(para.list[[10]])<-c("AbsTol", "RelTol", "InitStep", "MaxStep", "ETstep");
names(para.list[[11]])<-c("StartTime", "EndTime", "Output");
names(para.list[[12]])<-c("a","b");


# *****************************************************READ THE INITIAL CONDITIONS FILE .init **********************************************

init<-read.table(paste0(inputfile.name, ".INIT") ,as.is=F, fill=T);

# The init file contains the initial condition state variables IS , Snow , Overland , UnSat, Sat and
# the river initial state condition variables RiverState SatBeneath River
init.element<-init[seq(1:NumEle),];
names(init.element)<-c("IS" , "Snow" , "Overland" , "UnSat", "Sat");

init.riverSegment<-init[seq(NumEle+1,NumEle+NumRiv),];
names(init.riverSegment)<-c("RiverState", "SatBeneath River");


# *****************************************************READ THE CALIBRATION FILE .calib**********************************************


#  Read the Calibration File which is similar to the solver parameter file and has a cumbersome file structure


# geolKsatH geolKsatV soilKsatV macKsatH macKsatV
# infD RzD macD
# Porosity Alpha Beta
# vAreaF hAreaf
# VegFrac Albedo Rough
# Precep Temp
# Et0 Et1 Et2
# rivRough rivKsatH rivKsatV rivBedThickness
# rivDepth rivWidCoeff

# Variable Name: Variable Type: Variable Description (Multiplicative Coefficients for): Remarks
# infD : Double : Infiltration Depth
# RzD : Double : Root Zone Depth
# macD : Double : Macropore Depth
# Alpha,  Beta: Double : Van genuchten parameters
# vAreaF, hAreaF : Double :xVertical, horizontal macropore area fraction
# VegFrac: Double : Vegetation Fraction
# Albedo : Double:  Albedo
# Rough : Double : Manning’s n
# Precep : Double : Precipitation
# Temp : Double : Temperature
# Et0 : Double : Interception Loss
# Et1 : Double : Transpiration
# Et2 : Double : Evaporation from Ground
# rivRough : Double : River Manning’s n
# rivKsatH, rivKsatV : Double : Conductivity of river walls and bed
# rivBedThickness : Double : River bed thickness
# rivDepth : Double : River depth
# rivWidCoeff : Double : River width coefficient


# Read the file
calib<-read.table(paste0(inputfile.name, ".CALIB") ,as.is=F, fill=T);

# Transform the cumbersome format into a list
calib.list<-list(calib[1,],calib[2,1:3],calib[3,1:3],calib[4,1:2],calib[5,1:3],calib[6,1:2],calib[7,1:3],calib[8,1:4],calib[9,1:2]);

# Name the variables in the list
names(calib.list[[1]])<-c("geolKsatH", "geolKsatV", "soilKsatV", "macKsatH", "macKsatV");
names(calib.list[[2]])<-c("infD", "RzD", "macD");
names(calib.list[[3]])<-c("Porosity", "Alpha", "Beta");
names(calib.list[[4]])<-c("vAreaF", "hAreaf");
names(calib.list[[5]])<-c("VegFrac", "Albedo", "Rough");
names(calib.list[[6]])<-c("Precep", "Temp");
names(calib.list[[7]])<-c("Et0", "Et1", "Et2");
names(calib.list[[8]])<-c("rivRough", "rivKsatH", "rivKsatV", "rivBedThickness");
names(calib.list[[9]])<-c("rivDepth", "rivWidCoeff");



 #######################  Save the objects created in the file PIHMInputsR.RData using the function save image    ###################

######## Create the directory where the objects created in the file PIHMInputsR.RData are to be saved

dir.create(paste0('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs\\',Project));

save.image(file=paste0('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs\\',Project,'\\PIHMInputsR.RData'));

