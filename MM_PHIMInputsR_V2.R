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


.libPaths("C:/Felipe/Sotware&Coding/R_Library/library")  ;



#      set the working directory
#     setwd("./PIHMInputsR");


setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs')    ;

#setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\WE38_Files_PIHM_Cycles20170208\\Feb2720171451")    ;


##     Read the objects from the 'PIHMInputsR.RData' file in the current working space and put them in the global environment

## Load the objects into the global environment

## attach('./PIHMInputsR.RData', ); Adds the database with the objects created to the path R searches for objects. It is safer than load, but one needs to remember the name of the variables when programming. 


####### Store the name of the project to read and write files more easily #############

#Project<-"MergeVectorLayer000_q30_a200000"   ;

Project<-"DataModel" ;



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


######### Correct the Nodes that have incorrect height in the river segment before writing the file ################### 

# mesh.Nodes[mesh.Nodes$Index == 52,'Zmax']<-mesh.Nodes[mesh.Nodes$Index == 51,'Zmax']




######## Write the mesh file ###########################################################################################

NODES.1<-data.frame(c('NUMNODE'),NumNode )   ;
write.table(NODES.1 , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=F, quote=F, sep ="\t") ;


header.mesh.Nodes<-c('INDEX' , 'X' , 'Y' , 'ZMIN' , 'ZMAX');

write.table(mesh.Nodes , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


###################   Write the appropiate formated "Attributes" File for the MM-PIHM input format  #################################

#### import the shape files from QGIS with the LC mode from each triangle ####

########### Read infromation about the shape files ###########

# HansYoust.LC.info<-ogrInfo("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Landcover/LC_Stat.shp");

Project.LC.info<-ogrInfo("C:/Aun Trabajo en Proceso/HansYostDeepCreek/DomainDecomposition2.shp");


#### read the shape file that has been created in QGIS using the zonal statistics

# HansYoust.LC<-readOGR("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Landcover/LC_Stat.shp")  ;

Project.LC<-readOGR("C:/Aun Trabajo en Proceso/HansYostDeepCreek/DomainDecomposition2.shp");

str(Project.LC, max.level = 2) ;

plot(Project.LC) ;
####  plot(HansYoust.LC);


str(Project.LC@data) ;


#### Extract the LC index corresponding to the mode in each mesh triangle


# HansYoust.LC@data$Lc_mode<-as.factor(HansYoust.LC@data$Lc_mode) ;

Project.LC@data$LC_majorit<-as.factor(Project.LC@data$LC_majorit) ;

# LC.indexs<-as.integer(levels(HansYoust.LC@data$Lc_mode))  ;

LC.indexs<-as.integer(levels(Project.LC@data$LC_majorit))  ;

str(LC.indexs)

# HansYoust.LC@data$LC.index<-HansYoust.LC@data$Lc_mode ;

Project.LC@data$LC.index<-Project.LC@data$LC_majorit



######  Merge the LC index with the rest of the attribute table

######### Load the attribute file to change the LC codes from PIHM GIS to the mode from spatial statistics in QGIS

att<-read.table(paste0(Project.Directory,"\\",DataModel.dir,"\\",Project,".att"),as.is=T,col.names=c('Index', 'Soil', 'Geol','LC','IS_IC', 'Snw_IC', 'Srf_IC', 'Ust_IC', 'St_IC', 'Ppt', 'Tmp', 'RH', 'Wnd', 'Rn', 'G', 'VP', 'S', 'mF', 'BC.0', 'BC.1', 'BC.2', 'mP'));

names(att) ;

# att.expanded.1<-merge(att,HansYoust.LC@data, by.x='Index' , by.y='Ele_ID') ;

att.expanded.1<-merge(att,Project.LC@data, by.x='Index' , by.y='Ele_ID') ;


############# Load the vegetation parameter table and the convertion parameters for PIHM - MM ################

NUMLC<-read.table("./vegprmt.tbl", skip=0, as.is=T, nrows=1) ;


vegprmt.tbl<-read.table("./vegprmt.tbl", skip=1, sep="", as.is=T, header=T, nrows=NUMLC[1,2]) ;

Description<-read.table("./vegprmt.tbl", skip=1, sep="\t", as.is=T, header=T, nrows=NUMLC[1,2], comment.char="") ;

vegprmt.tbl$Description<-sapply(strsplit(Description[,1], "#"), "[" , 2) ;

Otherprmt.tbl<-read.table("./vegprmt.tbl", skip=NUMLC[1,2]+2, sep="", as.is=T, header=F, nrows=5) ;



############# Load the vegetation parameter map from the NLCD to the MM-PIHM Land Cover type 
############# "NLCD land cover class mapping to PIHM land cover type ############################          


NLCD_PIHM.lc<-read.table("./vegprmt.tbl", skip=NUMLC[1,2]+10, sep= ">" , as.is=T, header=F,comment.char="") ;

NLCD.lc<-NLCD_PIHM.lc[,2];

PIHM.lc<-as.integer(sapply(strsplit(NLCD_PIHM.lc[,1], split = " "), "[" , 2)) ;

PIHM_to_NLCD<-merge(data.frame(NLCD.lc, PIHM.lc), vegprmt.tbl, by.x= "NLCD.lc", by.y= "INDEX", all=T) ;


PIHM_to_NLCD[!is.na(PIHM_to_NLCD$PIHM.lc), ]



######### MErge with the NLCD_to_PIHM data to change the NLCD LC data to the MM-PIHM Land Cover


att.expanded.2<-merge(att.expanded.1,PIHM_to_NLCD, by.x="LC_majorit", by.y="PIHM.lc", all.x=T ) ;

###### change the name of the LC column that will be used in the revised attributes

revised.names<-names(att)   ;

revised.names[4]<- "LC" ;


######## Merge the att data frame  with the Mukey.map data frame to replace the PIHM Soil index by the index in the GSSURGO extracted data

att.expanded.3<-merge(att.expanded.2,MUKEYS.map.1, by.x='Index', by.y='Ele_ID') ;

###### change the name of the LC column that will be used in the revised attributes


Revised.att<-att.expanded.3[order(att.expanded.3$Index),] ;





Revised.att$LAI<-0  ;

Revised.att$METEO<-1  ;


# #############################################################################################################################
# 
# #   Still need to corret the attribute tables with the correct soil index in the triangles that do have MUKEYs Gaps
# 
# ###########################################################################################################################

Revised.att[, 'MUKEYS.index']<-as.numeric(Revised.att[, 'MUKEYS.index']) ;

# Revised.att[Revised.att$Index %in% Mukey_Gaps_indx[,'Ele_ID'],] [2,'MUKEYS.index']<-dim(HansYoust_Soil)[1]  ;

Revised.att[Revised.att$Index %in% Mukey_Gaps_indx[,'Ele_ID'],] [2,'MUKEYS.index']<-dim(Project_Soil)[1]  ;

 write.table(Revised.att[,c('Index', 'MUKEYS.index', 'MUKEYS.index', 'NLCD.lc','METEO', 'LAI','S', 'BC.0', 'BC.1', 'BC.2')], file=paste0(inputfile.name, '.ATT') , row.names=F, col.names=c('INDEX' , 'SOIL' , 'GEOL' ,	'LC' ,	'METEO' ,	'LAI',	'SS' ,	'BC0' ,	'BC1' ,	'BC2'), quote=F , sep = "\t" ) ;





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


############  Check river elementsfor differences in height and flow patterns #####################
River.Nodes<-unique(c(riv.elements$FROM, riv.elements$TO))  ;

River.Nodes.Elevation<-mesh.Nodes[mesh.Nodes$Index %in% River.Nodes, ] ;

River.Nodes.Elevation.FROM<-merge(riv.elements,River.Nodes.Elevation, by.x='FROM' , by.y='Index', all.x=T) ;

River.Nodes.Elevation.TO<-merge(riv.elements,River.Nodes.Elevation, by.x='TO' , by.y='Index', all.x=T) ;

River.Nodes.Max_Elev_Dif<-River.Nodes.Elevation.FROM$Zmax - River.Nodes.Elevation.TO$Zmax ;
           
River.Nodes.Min_Elev_Dif<-River.Nodes.Elevation.FROM$Zmin - River.Nodes.Elevation.TO$Zmin ;

River.Nodes.Elevation.FROM[which(River.Nodes.Max_Elev_Dif < 0),]

River.Nodes.Elevation.TO[which(River.Nodes.Max_Elev_Dif < 0),]




River.Nodes.Elevation.FROM[River.Nodes.Elevation.FROM$FROM %in% c(50:55),]

River.Nodes.Elevation.TO[River.Nodes.Elevation.TO$TO %in% c(50:55),]

############### Write the riverl elemnts file .riv in the right PIHM format #######################



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












