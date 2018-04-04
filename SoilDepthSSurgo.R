##############################################################################################################
# 
# 
# Program to use the information from GSSURGO to creat a soil depth layer for PIHM 
# 
# Felipe Montes 2018  02 11
# 
# Uses the following codes ; SoilsSurgoPIHM
# 
# 
############################################################################################################### 



###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

#  Set Working directory

setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs')   

#  Windows.Directory<-gsub("\\\\", "/", readClipboard())
#  C:\Felipe\PIHM-CYCLES\PIHM\PIHM_Felipe\CNS\Manhantango\HydroTerreFullManhantango\HansYostDeepCreek\Aug2920171550

#  C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/SWATPIHMRcode 



library(Hmisc) ;
library(plyr) ;
library(dplyr)  ;
library(soilDB) ;
library(raster) ;
library(aqp) ;
library(sp) ;
library(rgdal) ;

####### Store the name of the project to read and write files more easily #############

Project<-"MergeVectorLayer000_q25_a100000" ;


#Project<-"DataModel" ;



load(paste0('./',Project,'/SoilsSurgoPIHM.RData'));

load(paste0('./',Project,'/FillNoDataSoils.RData'));

load(paste0('./',Project,'/MM_PHIMInputsR_V2.RData')) ;

######## Store the name of the directory whre the modified MM-PIHM inputs are to be stored

RevisedOutputs.dir<-paste0('./',Project,'/') ;




# Create the path to read the input files by pasting RevisedOutputs.dir and the Project name together with the file ".name" ie ".mesh"

inputfile.name<-paste0(RevisedOutputs.dir,Project) ;

# ###########################################################################################################################
# 
# ###     Prepare depth to bed rock data to be incorporated into the mesh file when a uniform soil profile depth
# ###     is not what is desired 
# 
# ###########################################################################################################################

##### Read the nodes and the corresponding Mukey from the TX file formed from the shape file that extracted the nodes of the mesh shape file in Qgis


Nodes.Mukeys.info<-ogrInfo("C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/3DomainDecomposition/MergeVectorLayer000_q25_a100000_Nodes.shp");


Nodes.Mukeys<-readOGR("C:/Aun Trabajo en Proceso/HansYostDeepCreek/Mar0820181045/3DomainDecomposition/MergeVectorLayer000_q25_a100000_Nodes.shp")  ;

str(Nodes.Mukeys, max.level = 3)  ;

str(Nodes.Mukeys@data)

#### Extract the Mukeys corresponding to each Node
####  Convert the Mukeys into a factor and extract the levels of the factor to get the Mukeys from which we need soil 
####  bedrock information


Nodes.Mukeys@data$Mukey.factor<-as.factor(Nodes.Mukeys@data$HY_SOIL_ma) ;

head(Nodes.Mukeys@data) 

levels(Nodes.Mukeys@data$Mukey.factor)

####  Convert the Mukeys into a factor and extract the levels of the factor to get the Mukeys from which we need soil 
####  bedrock information

Mukeys_Nodes<-levels(Nodes.Mukeys@data$Mukey.factor)  ;





################################ Query the Soil Data access database with SQL through R #################


# from https://sdmdataaccess.sc.egov.usda.gov/queryhelp.aspx
# and https://sdmdataaccess.sc.egov.usda.gov/documents/ReturningSoilTextureRelatedAttributes.pdf


# --Sample query begins.
# --Note that a pair of dashes denotes the beginning of a comment. 
# SELECT
# saversion, saverest, -- attributes from table "sacatalog"
# l.areasymbol, l.areaname, l.lkey, -- attributes from table "legend"
# musym, muname, museq, mu.mukey, -- attributes from table "mapunit"
# comppct_r, compname, localphase, slope_r, c.cokey, -- attributes from table "component"
# hzdept_r, hzdepb_r, ch.chkey, -- attributes from table "chorizon"
# sandtotal_r, silttotal_r, claytotal_r, --total sand, silt and clay fractions from table "chorizon"
# sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r,--sand sub-fractions from table "chorizon"
# texdesc, texture, stratextsflag, chtgrp.rvindicator, -- attributes from table "chtexturegrp"
# texcl, lieutex, -- attributes from table "chtexture"
# texmod -- attributes from table "chtexturemod"
# FROM sacatalog sac
# INNER JOIN legend l ON l.areasymbol = sac.areasymbol AND l.areatypename = 'Non-MLRA Soil Survey Area'
# INNER JOIN mapunit mu ON mu.lkey = l.lkey
# AND mu.mukey IN
# ('107559','107646','107674','107682','107707','107794','107853','107854','107865','107867','107869','107870','107871')
# LEFT OUTER JOIN component c ON c.mukey = mu.mukey
# LEFT OUTER JOIN chorizon ch ON ch.cokey = c.cokey
# LEFT OUTER JOIN chtexturegrp chtgrp ON chtgrp.chkey = ch.chkey
# LEFT OUTER JOIN chtexture cht ON cht.chtgkey = chtgrp.chtgkey
# LEFT OUTER JOIN chtexturemod chtmod ON chtmod.chtkey = cht.chtkey
# --WHERE.
# --ORDER BY l.areaname, museq, comppct_r DESC, compname, hzdept_r -- standard soil report ordering
# --Sample query ends. 

# extract the map unit keys from the RAT, and format for use in an SQL IN-statement
#in.statement2 <- format_SQL_in_statement(MUKEYS$ID); 

in.statement2 <- format_SQL_in_statement(Mukeys_Nodes); 


# format query in SQL- raw data are returned

Pedon.query.Nodes<- paste0("SELECT component.mukey, component.cokey, compname, comppct_r, majcompflag, slope_r, hzdept_r, hzdepb_r,hzthk_r, hzname, awc_r, sandtotal_r, silttotal_r, claytotal_r, om_r,dbtenthbar_r, dbthirdbar_r, dbfifteenbar_r, fraggt10_r, frag3to10_r, sieveno10_r, sieveno40_r, sieveno200_r, ksat_r  FROM component JOIN chorizon ON component.cokey = chorizon.cokey AND mukey IN ", in.statement2," ORDER BY mukey, comppct_r DESC, hzdept_r ASC") ;

# now get component and horizon-level data for these map unit keys
Pedon.info.Nodes<- SDA_query(Pedon.query.Nodes);
head(Pedon.info.Nodes) ;
str(Pedon.info.Nodes)  ;

########################################## IMPORTANT ###########################################################

### Map Unit No. 539762 is a water body and is not available to query from the SDA_query function

### Map Unit No. 539759 are urban land Urban land-Udults complex

###############################################################################################################

# filter components that are the major components of each unit map with the Flag majcompflag=='Yes'

Pedon.info.Nodes.MajorC<-Pedon.info.Nodes[which(Pedon.info.Nodes$majcompflag == 'Yes'),]  ;
head(Pedon.info.Nodes.MajorC) ; 
str(Pedon.info.Nodes.MajorC)  ;


# check if there are mukeys with more than one dominant component

Pedon.info.Nodes.MajorC$mukey.factor<-as.factor(Pedon.info.Nodes.MajorC$mukey) ;

str(Pedon.info.Nodes.MajorC$mukey.factor)


Pedon.info.Nodes.MajorC$mukey_comppct_r<-paste(Pedon.info.Nodes.MajorC$mukey.factor,Pedon.info.Nodes.MajorC$comppct_r, sep = "_") ;

# Select major component mukeys that have also the highest component percent comppct_r

head(Pedon.info.Nodes.MajorC)  ;

Dominant.Nodes<- aggregate(comppct_r ~ mukey.factor, data=Pedon.info.Nodes.MajorC, FUN="max" , drop=T, simplify=T) ;

head(Dominant.Nodes)  ;

str(Dominant.Nodes) ;

Dominant.Nodes$mukey_comppct_r<-paste(Dominant.Nodes$mukey.factor,Dominant.Nodes$comppct_r, sep ="_");


Mukey.Pedon.Nodes<-Pedon.info.Nodes.MajorC[Pedon.info.Nodes.MajorC$mukey_comppct_r %in% Dominant.Nodes$mukey_comppct_r,]  ;

str(Mukey.Pedon.Nodes) ;


# Creating Mukey ID for each dominant component


Mukey.Pedon.Nodes$mukey_ID<-as.character(Mukey.Pedon.Nodes$mukey) ;


#  str(Mukey.Pedon);


#  Transform the Pedon.info query in to the right format to be converted into a SoilProfileCollection object
#   https://ncss-tech.github.io/AQP/aqp/aqp-intro.html


#Pedon.info$id<-Pedon.info$mukey ;
# Pedon.info$top<-Pedon.info$hzdept_r ;
# Pedon.info$bottom<-Pedon.info$hzdept_r ;
#Pedon.info$name<-Pedon.info$hzname ;

depths(Mukey.Pedon.Nodes)<-mukey_ID ~ hzdept_r + hzdepb_r  ;
str(Mukey.Pedon.Nodes) ;


plot(Mukey.Pedon.Nodes, name='hzname',color='claytotal_r')  ;


# Add soil profile depth  soil.depth in meters (cm/100) .



Mukey.Pedon.Nodes$soil.depth<-profileApply(Mukey.Pedon.Nodes, FUN=max)/100 ; 

plot(Mukey.Pedon.Nodes@site$mukey_ID,Mukey.Pedon.Nodes@site$soil.depth)
with(Mukey.Pedon.Nodes@site, text(Mukey.Pedon.Nodes@site$mukey_ID,Mukey.Pedon.Nodes@site$soil.depth, labels=Mukey.Pedon.Nodes@site$mukey_ID, cex=1, srt=90, pos=4) )

str(Mukey.Pedon.Nodes) ;

Mukey.Pedon.Nodes@site

#### Match the soil depth with the nodes in the mesh file

##get the coordinates of the nodes from the shape file

head(Nodes.Mukeys@data)
str(Nodes.Mukeys@data)

head(Nodes.Mukeys@coords)
str(Nodes.Mukeys@coords)
rownames(Nodes.Mukeys@data)

Nodes.Mukeys.Coords<-data.frame(Nodes.Mukeys@coords,Nodes.Mukeys@data)  ;


#### Each point in node in the triangle is repeated a number of times equal to the number of trianlges that share that node

duplicated(Nodes.Mukeys@coords) ;

head(Nodes.Mukeys.Coords[order(Nodes.Mukeys.Coords$coords.x1),])

Unique.Nodes.Mukeys.Coords<-unique(Nodes.Mukeys.Coords)
head(Unique.Nodes.Mukeys.Coords)
str(Unique.Nodes.Mukeys.Coords)



## Merge the nodes data with the soil depth of the GSSURGO extracted data

head(Mukey.Pedon.Nodes@site)
str(Mukey.Pedon.Nodes@site)

Mukey.Pedon.Nodes@site$Mukey.factor<-as.factor(Mukey.Pedon.Nodes@site$mukey_ID);

levels(Mukey.Pedon.Nodes@site$Mukey.factor);
levels(Unique.Nodes.Mukeys.Coords$Mukey.factor);


Nodes.Mukeys.Soil.Depth<-merge(Unique.Nodes.Mukeys.Coords, Mukey.Pedon.Nodes@site, by='Mukey.factor') ;
 
head(Nodes.Mukeys.Soil.Depth)
str(Nodes.Mukeys.Soil.Depth)

#### Check if there are nodes wihout soil depth ###


anyNA(Nodes.Mukeys.Soil.Depth$soil.depth)

range(Nodes.Mukeys.Soil.Depth$soil.depth)

plot(Nodes.Mukeys.Soil.Depth$mukey_ID,Nodes.Mukeys.Soil.Depth$soil.depth)
with(Nodes.Mukeys.Soil.Depth, text(Nodes.Mukeys.Soil.Depth$mukey_ID,Nodes.Mukeys.Soil.Depth$soil.depth, labels=Nodes.Mukeys.Soil.Depth$mukey_ID, cex=1, srt=90, pos=4) )

############    Merge with the mesh file from the PIHMInputsR.R ##########################################



head(mesh.Nodes)
str(mesh.Nodes)

Rev.mesh.Soil.Depth<-merge(mesh.Nodes,Nodes.Mukeys.Soil.Depth, by.x=c('X','Y'), by.y=c('coords.x1' , 'coords.x2'), all.x=T, sort=F) ;

#### Check if there are any rows with NA in the soil depth column

anyNA(Rev.mesh.Soil.Depth$soil.depth)

Rev.mesh.Soil.Depth[which(is.na(Rev.mesh.Soil.Depth$soil.depth)),] ;

###  The missing points are water bodies with mukey 539762  and would need to be corrected

### Points with Mukey 539759 are urban land Urban land-Udults complex

### Points with Mukey 539758 are Weikert and Klinesville shaly silt loams, steep


### Fill the rows with NA in the soil depth column with soil depth equal to 0.555

Rev.mesh.Soil.Depth[which(is.na(Rev.mesh.Soil.Depth$soil.depth)),c('soil.depth')]<-0.555 ;

head(Rev.mesh.Soil.Depth)
#### calculate Zmin from Zmax and soil depth

Rev.mesh.Soil.Depth$Zmin.SSURGO<-Rev.mesh.Soil.Depth$Zmax.Riv.Corr-Rev.mesh.Soil.Depth$soil.depth ;

## check if there is any negative difference between Zmax-Zmin.SSURGO


Rev.mesh.Soil.Depth$Diff.Z<-Rev.mesh.Soil.Depth$Zmax.Riv.Corr-Rev.mesh.Soil.Depth$Zmin.SSURGO ;

Rev.mesh.Soil.Depth[which(Rev.mesh.Soil.Depth$Diff.Z <= 0),] ;

plot(Rev.mesh.Soil.Depth$Index, Rev.mesh.Soil.Depth$Diff.Z)  ;

Rev.mesh.Soil.Depth[which(Rev.mesh.Soil.Depth$Diff.Z <= 0.20),]  ;

#### put together the columns needed for the mesh file

Rev.mesh.Nodes.SSURGO<-Rev.mesh.Soil.Depth[order(Rev.mesh.Soil.Depth$Index),c('Index', 'X' , 'Y' , 'Zmin' ,'Zmin.SSURGO','Zmax' , 'Zmax.Riv.Corr' )] ;

head(Rev.mesh.Nodes.SSURGO)
str(Rev.mesh.Nodes.SSURGO)

plot(Rev.mesh.Nodes.SSURGO$Index, (Rev.mesh.Nodes.SSURGO$Zmax-Rev.mesh.Nodes.SSURGO$Zmin.SSURGO))   ;










#### write the revised mesh file########################################################################################

################## Write out the appropiate formated "Mesh" File for the MM-PIHM input format ##################################
##       First, create the mesh element part

head(mesh.Elements)

MESH.1<-data.frame(c('NUMELE'),NumEle);

## write the first lines of the new MM-PIHM mesh file

write.table(MESH.1, file=paste0(inputfile.name, "_REV_SSURGO.MESH"), row.names=F ,col.names=F, quote=F, sep ="\t") ;


MESH.2<-data.frame(c('INDEX') ,c('NODE1') , c('NODE2') , c('NODE3'), c('NABR1') , c('NABR2') ,c('NABR3')) ;

write.table(MESH.2[1,], file=paste0(inputfile.name, "_REV_SSURGO.MESH"), row.names=F , col.names=F, quote=F, sep ="\t", append = T) ;

## write the mesh data into the new MM-PIHM mesh file

write.table(mesh.Elements, file=paste0(inputfile.name, "_REV_SSURGO.MESH"), row.names=F , col.names=F, quote=F, sep ="\t", append = T) ;


head(mesh.Elements)


##       Second Create the node elements part 
### write the first lines of the node elements

head(mesh.Nodes)
str(mesh.Nodes)

NumEle
NumNode





######## Write the mesh file ###########################################################################################

NODES.1<-data.frame(c('NUMNODE'),NumNode )   ;
write.table(NODES.1 , file=paste0(inputfile.name, "_REV_SSURGO.MESH") , append=T , row.names=F ,col.names=F, quote=F, sep ="\t") ;


header.mesh.Nodes<-c('INDEX' , 'X' , 'Y' , 'ZMIN' , 'ZMAX');

#write.table(New.mesh.Nodes , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


#write.table(Rev.mesh.Nodes[,c('Index.x' , 'X' , 'Y', 'Zmin.x' , 'Zmax.x')] , file=paste0(inputfile.name, ".MESH") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;

write.table(Rev.mesh.Nodes.SSURGO , file=paste0(inputfile.name, "_REV_SSURGO.MESH") , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep ="\t") ;


save.image(file=paste0('C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_R_Scripts\\MM_PIHM_inputs\\',Project,'\\SoilDepthSSurgo.RData'));

