################### Program to use the objects created by the PIHMInputsR program, to generate suitable input format for MM-PIHM
################### Felipe Montes
################### 2015 06 27

#################    Felipe Montes 2017 03 01 Update: Added function to be able to use the directory in which PIHM outputs are saved and read all of the input files into PIHM
################   Added explicit working directory path
################   Add tab separation to the output tables to match the PIHM format

#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/R_Library/library")



#      set the working directory

setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs');

#  setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\WE38_Files_PIHM_Cycles20170208\\Feb2720171451")    ;


##     Read the objects from the 'PIHMInputsR.RData' file in the current working space and put them in the global environment

## Load the objects into the global environment

## attach('./PIHMInputsR.RData', ); Adds the database with the objects created to the path R searches for objects. It is safer than load, but one needs to remember the name of the variables when programming. 

Project<-"WE38"


load(paste0('./', Project, '/PIHMInputsR.RData'))  ;



########Create the directory to store the modified MM-PIHM inputs

dir.create(paste0('./', Project));

#Store the name of the directory where the inputs are:

DataModel.dir<-Project<-"WE38" ;


#Store  the name of the project :


Project<-"WE38"



# Create the path to write  the input files by pasting DataModel.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-(paste(".",DataModel.dir,Project, sep="/")) ;

################## Write out the appropiate formated "Mesh" File for the MM-PIHM input format ##################################

##       First, create the mesh element part

head(mesh.Elements)

MESH.1<-data.frame(c('NUMELE'),NumEle);

## write the first lines of the new MM-PIHM mesh file

write.table(MESH.1, file=paste0(inputfile.name, ".MESH"), row.names=F ,col.names=F, quote=F, sep ="\t") ;


MESH.2<-data.frame(c('INDEX') ,c('NODE1') , c('NODE2') , c('NODE3'), c('NABR1') , c('NABR2') ,c('NABR3')) ;

write.table(MESH.2[1,], file=paste0(inputfile.name, ".MESH"), row.names=F , col.names=F, quote=F, sep ="\t", append = T) ;

## write the mesh data into the new MM-PIHM mesh file

write.table(mesh.Elements, file=paste0(inputfile.name, ".MESH"), row.names=F , col.names=F, quote=F, sep ="\t", append = T) ;

### write the first lines of the node elements

head(mesh.Nodes)

NumEle
NumNode

 
NODES.1<-data.frame(c('NUMNODE'),NumNode )   ;
write.table(NODES.1 , file=paste0(inputfile.name, ".mesh") , append=T , row.names=F ,col.names=F, quote=F, sep ="\t") ;


##       Second Create the node elements part

header.mesh.Nodes<-c('INDEX' , 'X' , 'Y' , 'ZMIN' , 'ZMAX');

write.table(mesh.Nodes , file=paste0(inputfile.name, ".mesh") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


##################   Write the appropiate formated "Attributes" File for the MM-PIHM input format  #################################



#header.att<-c( 'IND' ,  'SOIL' ,	'GEOL' ,	'LC' ,	'CMC' ,	'SNOWH' ,	'HSFC' ,	'UNSAT' ,	'GW' ,	'METEO' ,	 'LAI' , 	'SS' , 	'BC0' ,	'BC1' ,	'BC2' , 	'MACP' );    commented to adapt the new inputs of the WE38 to the MM-PIHM

head(att)

header.att<-c( 'INDEX' ,  'SOIL' ,	'GEOL' ,	'LC' ,	'METEO' , 'LAI' , 	'SS' , 	'BC0' ,	'BC1' ,	'BC2' );

att.1<-att

att.1[,c("Soil" , "Geol", "LC")]<-data.frame(c(1),c(1),c(12)) ;

att.1$LAI<-0  ;

head(att.1)



#write.table(att[,c('Index', 'Soil', 'Geol','LC','Ppt', 'Ppt', 'S', 'BC.0', 'BC.1', 'BC.2')], file=paste0(inputfile.name, ".att") , row.names=F, quote=F , sep = "\t" , col.names=header.att ) ; # ,col.names=header.att, quote=F


write.table(att.1[,c('Index', 'Soil', 'Geol','LC','Ppt', 'LAI', 'S', 'BC.0', 'BC.1', 'BC.2')], file=paste0(inputfile.name, ".att") , row.names=F, quote=F , sep = "\t" , col.names=header.att ) ;



#### Table Modified to accomodate the PIHM SWAT imputs ###






##     Need to merge the attributes of PIHM V2.2 with the MM-PIHM before continuing with the attribute file



# ###################   Write the appropiate formated Soil" File for the MM-PIHM input format  #################################
# 
# header.soil.1<- data.frame(c("NUMSOIL") , NumSoil) ;
# 
# 
# 
# #header.soil.2<-c( NumSoil , 'INFK' ,	'MAXSMC' ,	'MINSMC' ,	'DINF' ,	'ALPHA' ,	'BETA' ,	'MACHF' ,	'SATMACHK' ,	'QTZ');
# 
# names(HT_soil) ;
# 
# names(soil) ;
# 
# 
# 
# DINF=0.10
# KMACV_RO=100.0
# KMACH_RO=1000.0
# 
# soil.new<-data.frame( soil[1:114,c(1)] , HT_soil[,2:5] , soil[1:114,c(2 , 2 , 2 , 3 , 4 , 6 , 7 , 8 , 8 , 8  , 10 )]) ;
# 
# names(soil.new)<-c( toupper(names(soil)[1]) , names(HT_soil)[2:5] , names(soil)[c(2 , 2 , 2 , 3 , 4 , 6 , 7 , 8 , 8 , 8  , 10 )] )   ;
# 
# head(soil.new)
#      
# 
# ##### Write the first line of the table with the NUMSOIL number
# 
# write.table(header.soil.1[1,],file=paste0(inputfile.name, ".soil") , col.names=F ,row.names=F , quote=F , sep= "\t") ;
# 
# 
# ##### Write the table with the data
# 
# write.table(soil.new,file=paste0(inputfile.name, ".soil") , append=T , row.names=F , quote=F , sep= "\t") ;
# 
# 
# ##### Write the factors DINF=0.10 , KMACV_RO=100.0 , KMACH_RO=1000.0 at the end of the table
# 
# soil.factor<-c(DINF=0.10 , KMACV_RO=100.0 , KMACH_RO=1000.0 ) ;
# 
# 
# 
# write.table(soil.factor,file=paste0(inputfile.name, ".soil") , append=T , col.names=F , row.names=T , quote=F , sep= "\t") ;
# 
# 
# ##     Need to merge the soil of PIHM V2.2 with the MM-PIHM before continuing with the attribute file
# 
# 
# 
# 
# ###################   Write the appropiate formated "Geology" File for the MM-PIHM input format  #################################
# 
# 
# header.geology<-c( NumSoil , 'SATHK' ,	'SATDK' , 	'MAXSMC' , 	'MINSMC' ,	'ALPHA' ,	'BETA' , 	'MACVF' ,	'SATMACKH' ,	'DMAC' );
# 
# 
# 
# write.table(geol,file=paste0(inputfile.name, ".geol") , row.names=F , quote=F , sep= "\t") ;
# 
# 

# ###################   Write the appropiate formated "Land cover" File for the MM-PIHM input format  #################################
# 
# 
# write.table(lc,file=paste0(inputfile.name, ".lc") , row.names=F , col.names=c(NumLC, " ", " " ," ", " " , " " , " " , " "), quote=F , sep= "\t") ;
# 
# 
# 

# ###################   Write the appropiate formated "River" File for the MM-PIHM input format  #################################
# 
# ### Add NUMRIV and the number of river segments  NumRiv to the begining of the .RIV file
# 
# header.riv.0<-data.frame(c("NUMRIV") , NumRiv) ;
# 
# head(riv.elements)
# 
# riv.elements.1<-riv.elements[,-9] ;
# 
# head(riv.elements.1) 
# 
# ##   Add river elements
# header.riv.1<-c( 'INDEX', 'FROM' , 'TO' ,  'DOWN' , 	'LEFT' , 	'RIGHT' , 'SHAPE' ,	'MATL' ,'BC' ,	'RES' )  ;
# 
# write.table(header.riv.0,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=F, quote=F, sep= "\t" ) ;
# 
# write.table(riv.elements.1,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=header.riv.1, quote=F, sep= "\t", append = T ) ;
# 
# 
# ##    Add river Shape
# 
# 
# ## write the word Shape as title before writting the tabel with the data
# 
# Shape.title<-data.frame(c('SHAPE'),NumShape) ;
# 
# write.table(Shape.title,file=paste0(inputfile.name, ".riv") , row.names=F , col.names=F, quote=F, append=T , sep= "\t") ;
# 
# 
# header.riv.Shape<-c('INDEX', 'DPTH' ,  'OINT' ,	'CWID' );
# 
# write.table(riv.shape,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=header.riv.Shape, quote=F, append=T, sep = "\t") ;
# 
# 
# ##   Add river Material
# 
# Material.title<-data.frame(c('MATERIAL'),NumMat) ;
# 
# write.table(Material.title,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;
# 
# 
# header.riv.Material<-c( 'INDEX' , 'ROUGH' ,  'CWR' ,	'KH' ,	'KV' ,	'BEDTHCK');
# 
# write.table(riv.material,file=paste0(inputfile.name, ".riv") , row.names=F , col.names=header.riv.Material, quote=F, append=T , sep = "\t") ;
# 
# # ##   Add initial condition
# # 
# # IC.title<-c('IC');
# # 
# # write.table(IC.title,file=paste0(inputfile.name, ".riv") , row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;
# # 
# # 
# # write.table(riv.IC, file=paste0(inputfile.name, ".riv") , row.names=F , col.names= c(NumIC, 'HRIV'), quote=F, append=T , sep = "\t") ;
# 
# ##   Add boundary condition
# 
# 
# write.table(BC,file=paste0(inputfile.name, ".riv"), row.names=F , col.names= F, quote=F, append=T, sep = "\t") ;
# 
# 
# ##   Add Reservoirs
# 
# write.table(Res,file=paste0(inputfile.name, ".riv"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;
# 
# 

# ###################   Write the appropiate formated Forcing File  METEO for the MM-PIHM input format  #################################

METEO.header.1  

METEO.header.2

METEO.header.3


METEO.col.names
     

head(forc)

str(forc)

forc.1<-data.frame(paste(forc[,1],forc[,2], sep=" "),forc[,-c(1,2)])  ;

METEO.Line1<-data.frame(c("METEO_TS"), METEO.header.1[2] , METEO.header.2[1,c(3,4)]  )    ;

write.table(METEO.Line1,file=paste0(inputfile.name, ".METEO"), row.names=F , col.names=F, quote=F, sep= "\t" ) ;

write.table(METEO.header.3,file=paste0(inputfile.name, ".METEO"), row.names=F , col.names=F, quote=F, sep= "\t", append =T ) ;

write.table(forc.1,file=paste0(inputfile.name, ".METEO"), row.names=F , col.names=F, quote=F, sep= "\t", append =T ) ;


# ###################   Write the appropiate formated Forcing File  METEO for the MM-PIHM input format  #################################


#    Need to add aproptiate Calib , IBC, and PARA files

