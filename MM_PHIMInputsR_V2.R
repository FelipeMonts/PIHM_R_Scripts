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


####### Store the name of the project to read and write files more easily #############

Project<-"MergeVectorLayer000_q30_a200000"   ;



load(paste0('./',Project,'/PIHMInputsR.RData'));



######## Store the name of the directory whre the modified MM-PIHM inputs are to be stored


#dir.create(Project);


RevisedOutputs.dir<-paste0('./',Project,'/') ;




# Create the path to read the input files by pasting RevisedOutputs.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-paste0(RevisedOutputs.dir,Project) ;

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



##       Second Create the node elements part 
### write the first lines of the node elements

head(mesh.Nodes)

NumEle
NumNode


NODES.1<-data.frame(c('NUMNODE'),NumNode )   ;
write.table(NODES.1 , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=F, quote=F, sep ="\t") ;


header.mesh.Nodes<-c('INDEX' , 'X' , 'Y' , 'ZMIN' , 'ZMAX');

write.table(mesh.Nodes , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


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

att<-read.table(paste0(Project.Directory,"\\",DataModel.dir,"\\",Project,".att"),as.is=T,col.names=c('Index', 'Soil', 'Geol','LC','IS_IC', 'Snw_IC', 'Srf_IC', 'Ust_IC', 'St_IC', 'Ppt', 'Tmp', 'RH', 'Wnd', 'Rn', 'G', 'VP', 'S', 'mF', 'BC.0', 'BC.1', 'BC.2', 'mP'));

names(att)

######### MErge with the NLCD_to_PIHM data to change the NLCD LC data to the MM-PIHM Land Cover


att.expanded<-merge(att,NLCD_to_PIHM, by.x="LC", by.y="NLCD.lc", all.x=T ) ;

###### change the name of the LC column that will be used in the revised attributes

revised.names<-names(att)   ;

revised.names[4]<- "PIHM.lc" ;


######## Merge the att data frame  with the Mukey.map data frame to replace the PIHM Soil index by the index in the GSSURGO extracted data

att.expanded.2<-merge(att.expanded,MUKEYS.map.1, by.x='Index', by.y='Ele_ID') ;

###### change the name of the LC column that will be used in the revised attributes


Revised.att<-att.expanded.2[order(att.expanded.2$Index),] ;





Revised.att$LAI<-0  ;

Revised.att$METEO<-1  ;


# #############################################################################################################################
# 
# #   Still need to corret the attribute tables with the correct soil index in the triangles that do have MUKEYs Gaps
# 
# ###########################################################################################################################

Revised.att[, 'MUKEYS.index']<-as.numeric(Revised.att[, 'MUKEYS.index']) ;

Revised.att[Revised.att$Index %in% Mukey_Gaps_indx[,'Ele_ID'],] [2,'MUKEYS.index']<-dim(HansYoust_Soil)[1]  ;

 write.table(Revised.att[,c('Index', 'MUKEYS.index', 'MUKEYS.index', 'LC','METEO', 'LAI','S', 'BC.0', 'BC.1', 'BC.2')], file=paste0(inputfile.name, '.ATT') , row.names=F, col.names=c('INDEX' , 'SOIL' , 'GEOL' ,	'LC' ,	'METEO' ,	'LAI',	'SS' ,	'BC0' ,	'BC1' ,	'BC2'), quote=F , sep = "\t" ) ;





# ###################   Write the appropiate formated Soil" File for the MM-PIHM input format  #################################
# 
# 
# 
# header.soil<-c( NumSoil , 'INFK' ,	'MAXSMC' ,	'MINSMC' ,	'DINF' ,	'ALPHA' ,	'BETA' ,	'MACHF' ,	'SATMACHK' ,	'QTZ');
# 
# write.table(soil,file=paste0(inputfile.name, ".soil") , append=T , row.names=F , quote=F , sep= "\t") ;
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
# write.table(geol,file=paste0(inputfile.name, ".geol") , row.names=F , quote=F , sep= "\t") ;
# 
# 

###################   Write the appropiate formated "River" File for the MM-PIHM input format  #################################

### Write the First line of the .Riv File

write.table(data.frame(c('NUMRIV'),NumRiv ),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F, quote=F, sep= "\t" ) ;


##   Add river elements
names(riv.elements)<-c( 'INDEX', 'FROM' , 'TO' ,  'DOWN' , 	'LEFT' , 	'RIGHT' , 	'SHAPE' ,	'MATL' ,	'IC' ,	'BC' ,	'RES' )  ;

write.table(riv.elements[,c( 'INDEX', 'FROM' , 'TO' ,  'DOWN' , 	'LEFT' , 	'RIGHT' , 	'SHAPE' ,	'MATL' ,	'BC' ,	'RES' )],file=paste0(inputfile.name, ".RIV"), append=T, row.names=F , quote=F, sep= "\t" ) ;


##    Add river Shape


## write the word Shape as title before writting the tabel with the data


write.table(data.frame(c('SHAPE'),NumShape),file=paste0(inputfile.name, ".RIV") , row.names=F , col.names=F, quote=F, append=T , sep= "\t") ;


header.riv.Shape<-c('INDEX', 'DPTH' ,  'OINT' ,	'CWID' );

write.table(riv.shape,file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=header.riv.Shape, quote=F, append=T, sep = "\t") ;


##   Add river Material



write.table(data.frame(c('MATERIAL'),NumMat ),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;




###############################################################################################################
#                         Print the corrected  .riv file in th right format
#                         Taken and adapted from the R code MM_PHIMinputs on the PIHM_R_Scripts directory
#                         2017 10 25 By Felipe Montes
###############################################################################################################



##  Convert units of Manning's roughness coefficient [day m-1/3] , River bank hydraulic conductivity (horizontal KH) and
##   River bed hydraulic conductivity (vertical KV) [m/day] into  [s m-1/3] and [m/s] 

riv.material$ROUGH<-signif(riv.material$n * 86400, 2) ;

riv.material$KH<-signif(riv.material$KsatH / 86400, 2)  ;

riv.material$KV<-signif(riv.material$KsatV / 86400, 2)  ;


header.riv.Material<-c( 'INDEX' , 'ROUGH' ,  'CWR' ,	'KH' ,	'KV' ,	'BEDTHCK');


write.table(riv.material[,c('Index','ROUGH' ,  'Cwr' ,	'KH' ,	'KV' ,	'Bed')],file=paste0(inputfile.name, ".RIV") , row.names=F , col.names=header.riv.Material, quote=F, append=T , sep = "\t") ;

##   Add boundary condition


write.table(data.frame(c('BC'),BC[2]),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F ,quote=F, append=T, sep = "\t") ;


##   Add Reservoirs

write.table(data.frame(c('RES'),Res[2]),file=paste0(inputfile.name, ".RIV"), row.names=F , col.names=F, quote=F, append=T , sep = "\t") ;












