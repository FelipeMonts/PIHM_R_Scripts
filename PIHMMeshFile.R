########################## Program To extract Mesh Values and change the Bed Depth in the PIHM inputs
####################  Felipe Montes 2015 11 04 

#      set the working directory

setwd("C:\\Felipe\\PIHM-CYCLES\\PIHM\\PIHM_Felipe\\CNS\\WE-38\\RScripts");

#      set the path to the library where packages are installed


.libPaths("C:/Felipe/R_Library/library") ;

## C:\Felipe\PIHM-CYCLES\PIHM\PIHM_Felipe\CNS\WE-38\PIHMOutputs\Spin2_2005_2009\input\WE38\WE38.mesh


###### Read the head of the sections of the file that have the element numbers and the cloumn names

mesh.Elements.head<-read.table("..\\PIHMOutputs\\Spin2_2005_2009\\input\\WE38\\WE38.mesh",header=F, nrows=1 ,as.is=T); 

NumEle<-as.numeric(mesh.Elements.head[1,1]) ;

mesh.Nodes.head<-read.table("..\\PIHMOutputs\\Spin2_2005_2009\\input\\WE38\\WE38.mesh",header=F, skip=NumEle+1, nrows=1,as.is=T); 

NumNode<-as.numeric(mesh.Nodes.head[1,1]) ;


##################### Read the nodes and elements information ##################################


mesh.Elements<-read.table("..\\PIHMOutputs\\Spin2_2005_2009\\input\\WE38\\WE38.mesh",header=T, nrows=NumEle, as.is=T ); 


mesh.Nodes<-read.table("..\\PIHMOutputs\\Spin2_2005_2009\\input\\WE38\\WE38.mesh",header=T, skip=NumEle+1, as.is=T  );


####################### Change the values of the Parameters as needed

# Change Zmin to 2 m deep

mesh.Nodes$ZMIN<-mesh.Nodes$ZMAX - 2 ;



################## Write out the appropiate formated "Mesh" File for the MM-PIHM input format ##################################


##       First, create the mesh element part

header.mesh.Elements<-c(NumEle , 'NODE1' , 'NODE2' , 'NODE3' , 'NABR1' , 'NABR2' , 'NABR3') ;


write.table(mesh.Elements, file="..\\PIHMOutputs\\Spin2_2005_2009\\input\\WE38\\WE38.mesh.New", row.names=F ,col.names=header.mesh.Elements, quote=F,sep= "\t") ;


##       Second Create the node elements part 



header.mesh.Nodes<-c(NumNode , 'X' , 'Y' , 'ZMIN' , 'ZMAX');


write.table(mesh.Nodes , file="..\\PIHMOutputs\\Spin2_2005_2009\\input\\WE38\\WE38.mesh.New" , append=T , row.names=F ,col.names=header.mesh.Nodes, quote=F, sep= "\t") ;


