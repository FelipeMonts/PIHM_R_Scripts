################### Program to use the objects created by the PIHMInputsR program, to generate suitable input format for MM-PIHM
################### Felipe Montes
################### 2015 06 27

#################    Felipe Montes 2017 03 01 Update: Added function to be able to use the directory in which PIHM outputs are saved and read all of the input files into PIHM
################   Added explicit working directory path
################   Add tab separation to the output tables to match the PIHM format

##################  The PIHM inputs in the latest verion of the MM-PIHM model (Prerelease 0.6.0 Alpha git@github.com:PSUmodeling/MM-PIHM.git)  have changed again. The Revised version of MM_PHIMInputsR, MM_PHIMInputsR_V2, will incorporate these changes and the changes that have been programed in the land cover and river file as well
#####################    Felipe Montes 
####################     2017 10 30





#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/R_Library/library")



#      set the working directory
#     setwd("./PIHMInputsR");



setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\WE38_Files_PIHM_Cycles20170208\\Feb2720171451")    ;


##     Read the objects from the 'PIHMInputsR.RData' file in the current working space and put them in the global environment

## Load the objects into the global environment

## attach('./PIHMInputsR.RData', ); Adds the database with the objects created to the path R searches for objects. It is safer than load, but one needs to remember the name of the variables when programming. 

load('./PIHMInputsR.RData');



########Create the directory to store the modified MM-PIHM inputs

dir.create('./MM_Inputs');

#Store the name of the directory where the inputs are:

DataModel.dir<-"MM_Inputs" ;


#Store  the name of the project :


Project<-"WE38"



# Create the path to read the input files by pasting DataModel.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-(paste(".",DataModel.dir,Project, sep="/")) ;

################## Write out the appropiate formated "Mesh" File for the MM-PIHM input format ##################################

##       First, create the mesh element part

header.mesh.Elements<-c(NumEle , 'NODE1' , 'NODE2' , 'NODE3' , 'NABR1' , 'NABR2' , 'NABR3') ;



write.table(mesh.Elements, file=paste0(inputfile.name, ".mesh"), row.names=F ,col.names=header.mesh.Elements, quote=F, sep ="\t") ;


##       Second Creat the node elements part 

header.mesh.Nodes<-c(NumNode , 'X' , 'Y' , 'ZMIN' , 'ZMAX');

write.table(mesh.Nodes , file=paste0(inputfile.name, ".mesh") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


###################   Write the appropiate formated "Attributes" File for the MM-PIHM input format  #################################



header.att<-c( 'IND' ,  'SOIL' ,	'GEOL' ,	'LC' ,	'CMC' ,	'SNOWH' ,	'HSFC' ,	'UNSAT' ,	'GW' ,	'METEO' ,	 'LAI' , 	'SS' , 	'BC0' ,	'BC1' ,	'BC2' , 	'MACP' );



write.table(att[,c('Index', 'Soil', 'Geol','LC','IS_IC', 'Snw_IC', 'Srf_IC', 'Ust_IC', 'St_IC', 'Ppt', 'Tmp', 'RH', 'Wnd', 'Rn', 'G', 'VP', 'S', 'mF', 'BC.0', 'BC.1', 'BC.2', 'mP')], file=paste0(inputfile.name, ".att") , row.names=F, quote=F , sep = "\t" ) ; # ,col.names=header.att, quote=F



##     Need to merge the attributes of PIHM V2.2 with the MM-PIHM before continuing with the attribute file



###################   Write the appropiate formated Soil" File for the MM-PIHM input format  #################################



header.soil<-c( NumSoil , 'INFK' ,	'MAXSMC' ,	'MINSMC' ,	'DINF' ,	'ALPHA' ,	'BETA' ,	'MACHF' ,	'SATMACHK' ,	'QTZ');

write.table(soil,file=paste0(inputfile.name, ".soil") , append=T , row.names=F , quote=F , sep= "\t") ;


##     Need to merge the soil of PIHM V2.2 with the MM-PIHM before continuing with the attribute file




###################   Write the appropiate formated "Geology" File for the MM-PIHM input format  #################################


header.geology<-c( NumSoil , 'SATHK' ,	'SATDK' , 	'MAXSMC' , 	'MINSMC' ,	'ALPHA' ,	'BETA' , 	'MACVF' ,	'SATMACKH' ,	'DMAC' );


write.table(geol,file=paste0(inputfile.name, ".geol") , row.names=F , quote=F , sep= "\t") ;



###################   Write the appropiate formated "Land cover" File for the MM-PIHM input format  #################################


write.table(lc,file=paste0(inputfile.name, ".lc") , row.names=F , col.names=c(NumLC, " ", " " ," ", " " , " " , " " , " "), quote=F , sep= "\t") ;




###################   Write the appropiate formated "River" File for the MM-PIHM input format  #################################


##   Add river elements
header.riv<-c( NumRiv, 'FROM' , 'TO' ,  'DOWN' , 	'LEFT' , 	'RIGHT' , 	'SHAPE' ,	'MATERIAL' ,	'IC' ,	'BC' ,	'RES' )  ;

write.table(riv.elements,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=header.riv, quote=F, sep= "\t" ) ;


##    Add river Shape


## write the word Shape as title before writting the tabel with the data

Shape.title<-c('Shape');

write.table(Shape.title,file=paste0(inputfile.name, ".riv") , row.names=F , col.names=F, quote=F, append=T , sep= "\t") ;


header.riv.Shape<-c(NumShape, 'RIVDPTH' ,  'O_INT' ,	'C_WID' );

write.table(riv.shape,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=header.riv.Shape, quote=F, append=T, sep = "\t") ;


##   Add river Material

Material.title<-('Material');

write.table(Material.title,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;


header.riv.Material<-c( NumMat , 'RIV_ROUGH' ,  'CWR' ,	'RIVHK' ,	'RIVVK' ,	'BEDTHICK_CAL');

write.table(riv.material,file=paste0(inputfile.name, ".riv") , row.names=F , col.names=header.riv.Material, quote=F, append=T , sep = "\t") ;

##   Add initial condition

IC.title<-c('IC');

write.table(IC.title,file=paste0(inputfile.name, ".riv") , row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;


write.table(riv.IC, file=paste0(inputfile.name, ".riv") , row.names=F , col.names= c(NumIC, 'HRIV'), quote=F, append=T , sep = "\t") ;

##   Add boundary condition


write.table(BC[2],file=paste0(inputfile.name, ".riv"), row.names=F , col.names= c('BC'), quote=F, append=T, sep = "\t") ;


##   Add Reservoirs

write.table(Res[2],file=paste0(inputfile.name, ".riv"), row.names=F , col.names=c('RES'), quote=F, append=T , sep = "\t") ;












