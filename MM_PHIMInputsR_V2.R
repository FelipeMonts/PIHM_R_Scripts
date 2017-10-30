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


setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs')    ;

#setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\WE38_Files_PIHM_Cycles20170208\\Feb2720171451")    ;


##     Read the objects from the 'PIHMInputsR.RData' file in the current working space and put them in the global environment

## Load the objects into the global environment

## attach('./PIHMInputsR.RData', ); Adds the database with the objects created to the path R searches for objects. It is safer than load, but one needs to remember the name of the variables when programming. 

Project<-'WE38'   ;


load(paste0('./',Project,'/PIHMInputsR.RData'));



######## Store the name of the directory whre the modified MM-PIHM inputs are to be stored


dir.create(Project);

#Store the name of the directory where the inputs are:

DataModel.dir<-paste0('./',Project,'/') ;




# Create the path to read the input files by pasting DataModel.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-paste0(DataModel.dir,Project) ;

################## Write out the appropiate formated "Mesh" File for the MM-PIHM input format ##################################

##       First, create the mesh element part

header.mesh.Elements<-c(NumEle , 'NODE1' , 'NODE2' , 'NODE3' , 'NABR1' , 'NABR2' , 'NABR3') ;



write.table(mesh.Elements, file=paste0(inputfile.name, ".mesh"), row.names=F ,col.names=header.mesh.Elements, quote=F, sep ="\t") ;


##       Second Creat the node elements part 

header.mesh.Nodes<-c(NumNode , 'X' , 'Y' , 'ZMIN' , 'ZMAX');

write.table(mesh.Nodes , file=paste0(inputfile.name, ".mesh") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


###################   Write the appropiate formated "Attributes" File for the MM-PIHM input format  #################################


############# Load the vegetation parameter table and the convertion parameters for PIHM - MM ################

NUMLC<-read.table("./vegprmt.tbl", skip=0, as.is=T, nrows=1) ;


vegprmt.tbl<-read.table("./vegprmt.tbl", skip=1, sep="", as.is=T, header=T, nrows=NUMLC[1,2]) ;

Description<-read.table("./vegprmt.tbl", skip=1, sep="\t", as.is=T, header=T, nrows=NUMLC[1,2], comment.char="") ;

vegprmt.tbl$Description<-sapply(strsplit(Description[,1], "#"), "[" , 2) ;

Otherprmt.tbl<-read.table("./vegprmt.tbl", skip=NUMLC[1,2]+2, sep="", as.is=T, header=F, nrows=5) ;



############# Load the vegetation parameter map from the NLCD to the MM-PIHM Land Cover type 
############# "NLCD land cover class mapping to PIHM land cover type ############################          


NLCD_PIHM.lc<-read.table("./vegprmt.tbl", skip=NUMLC[1,2]+10, sep= ">" , as.is=T, header=F,comment.char="") ;

PIHM.lc<-NLCD_PIHM.lc[,2];

NLCD.lc<-as.integer(sapply(strsplit(NLCD_PIHM.lc[,1], split = " "), "[" , 2)) ;

NLCD_to_PIHM<-merge(data.frame(NLCD.lc, PIHM.lc), vegprmt.tbl, by.x= "PIHM.lc", by.y= "INDEX", all=T) ;


NLCD_to_PIHM[!is.na(NLCD_to_PIHM$NLCD.lc),]


######### Load the attribute file to change the LC codes from NLCD to the NEw PIHM

att<-read.table("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Aug2920171550/4DataModelLoader/MergeVectorLayer000_q30_a200000.att",as.is=T,col.names=c('Index', 'Soil', 'Geol','LC','IS_IC', 'Snw_IC', 'Srf_IC', 'Ust_IC', 'St_IC', 'Ppt', 'Tmp', 'RH', 'Wnd', 'Rn', 'G', 'VP', 'S', 'mF', 'BC.0', 'BC.1', 'BC.2', 'mP'));

names(att)

######### MErge with the NLCD_to_PIHM data to change the NLCD LC data to the MM-PIHM Land Cover


att.expanded<-merge(att,NLCD_to_PIHM, by.x="LC", by.y="NLCD.lc", all.x=T ) ;


###### change the name of the LC column that will be used in the revised attributes

revised.names<-names(att)   ;

revised.names[4]<- "PIHM.lc" ;

Revised.att<-att.expanded[order(att.expanded$Index),revised.names] ;

names(Revised.att)[4]<-'LC'  ;


write.table(Revised.att[,c('Index', 'Soil', 'Geol','LC','IS_IC', 'Snw_IC', 'Srf_IC', 'Ust_IC', 'St_IC', 'Ppt', 'Tmp', 'RH', 'Wnd', 'Rn', 'G', 'VP', 'S', 'mF', 'BC.0', 'BC.1', 'BC.2', 'mP')], file="C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Aug2920171550/4DataModelLoader/Revised.att" , row.names=F, quote=F , sep = "\t" ) ; # ,col.names=header.att, quote=F




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












