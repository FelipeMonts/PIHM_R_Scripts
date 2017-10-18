# Program to import and visualize PIHM output files 
# Felipe Montes 2015 05 21


# set the working directory
setwd('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/PIHMOutputsR'); 

#***********************************Read output files from PIHM********************************************************

General.Output<-read.table('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/PIHMOutputsR/WE38.1505191051/WE38.GW.txt',as.is=T);


#*************************************Read Mesh File  and plot it*******************************************************

#      Install the misc3d package

library(misc3d);

#        Read the Mesh File


Mesh.ElementsFile<-read.table('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/PIHMOutputsR/May1920150749/4DataModelLoader/WE38.mesh', skip=1);






# ****************************************READ THE MESH FILE .mesh****************************************************************


# because the Messh file has in the first line the number of elements (NumEle) and the number of nodes (NumNode), we can use that information to read the table more efficiently

mesh.NumEle.NumNode<-read.table("C:/Felipe/GitHub/PIHMInputsR/SSHCZO_20141026_3.0/shalehills/Oct1820141240/4DataModelLoader/sshczo.mesh.0",as.is=T,nrows=1,skip=0, col.names=c("NumEle","NumNode"));

NumEle<-mesh.NumEle.NumNode$NumEle;
NumNode<-mesh.NumEle.NumNode$NumNode;

# The mesh file has the following structure: first it lists the Elements and then it lists the nodes.
# The elements part have the following structure: Index Node[0] Node[1] Node[2] Nabr[0] Nabr[1] Nabr[2]
# The nodes part has the folowing structure: Index X Y Zmin Zmax
# Zmax is the surface elevation of the node and Zmin is the bed elevation of the node
# see the PIHM2x_input_file_format.pdf file