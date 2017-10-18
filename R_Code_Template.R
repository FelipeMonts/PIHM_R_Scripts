#
#############################################################################################################################
#
#  Program to extract and summarize outputs from N2O Rose Simulations made in C-Cycles by Dr. Debasish Saha.
#
#  Felipe Montes,  2017/02/15
#
##############################################################################################################################






###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/R_Library/library")

#  Set Working directory


setwd("C:/Felipe/OrganicTransitions_N2OROSE/CyclesSimulation/RoseRCodeScripts/SumarizingC_CyclesSimulations/ROSE Simulation Work/High C no cover crop") ;

# Create a directory for sumarizing outputs

# dir.create()

Scenario.name<-"High_C_no_cover"


#    Inlcude the necesary packages

#    library(lattice); 

#    options(java.parameters = "-Xmx4g" ); 

#    options(java.parameters = "-Xmx1024m" );

    library(XLConnect);


#  initialize the dataframe where data will be collected

Summary.High_C_no_cover<-data.frame()  ;

#  i=list.files()[2]

 for (i in list.files()) {
     

     ###############################################################################################################
     #                         Read data sets into R            
     ###############################################################################################################

     #paste0("./",list.files()[1],"/N.dat")

     #  For High C no  cover crop

     #   c:\Felipe\OrganicTransitions_N2OROSE\CyclesSimulation\RoseRCodeScripts\SumarizingC_CyclesSimulations\ROSE Simulation Work\High C no cover      crop


     ######  Read only the columns that are wanted  


     Nitrogen.dat.cols<-c("character", "NULL", "NULL","NULL","NULL","NULL","NULL","NULL", "NULL","NULL","NULL", "NULL" , NA , "NULL") ;

     Nitrogen.dat<-read.table(paste0("./",i,"/N.dat"), header=F , skip=2 , sep="\t" , as.is= T ,colClasses =Nitrogen.dat.cols )  ;

     ####



     Nitrogen.dat.names<-read.table(paste0("./",i,"/N.dat"), header=T, nrows=1, sep="\t", colClasses =Nitrogen.dat.cols )   ;

     names(Nitrogen.dat)<-names(Nitrogen.dat.names) ;

     head(Nitrogen.dat)  ;   str(Nitrogen.dat)   ;


     Nitrogen.dat$DATE<-as.Date(Nitrogen.dat$DATE,format="%F" )  ;



     #   Read The Maize.dat file
     
     
     ######  Read only the columns that are wanted  
     
     #read.table("./ROSE Simulation Work/High C no cover crop/High C no cover crop 84-15/Maize.dat" , header=F, nrows=1, sep='\t') ;
     
     
     
     Maize.dat.cols<-c("character", "character" , "character", "NULL","NULL","NULL","NULL","NULL","NULL", "NULL","NULL","NULL", "NULL" , "NULL" , "NULL" , "NULL", "NULL") ;
     
     Maize.dat<-read.table(paste0("./",i,"/Maize.dat"), header=F, skip=2,sep='\t',colClasses =Maize.dat.cols) ;
     
     Maize.dat.names<-read.table(paste0("./",i,"/Maize.dat"), header=T, nrows=1, sep='\t',colClasses =Maize.dat.cols) ;
     
     names(Maize.dat)<-names(Maize.dat.names)   ;
     
     
     Maize.dat$DATE<-as.Date(Maize.dat$DATE,format="%F" )  ;
     
     head(Maize.dat)  ;   str(Maize.dat)  ;
     
     
     
     #   Read The Soybean.dat file
     
     
     ######  Read only the columns that are wanted  
     
     #read.table("./ROSE Simulation Work/High C no cover crop/High C no cover crop 84-15/Soybean.dat" , header=F, nrows=1, sep='\t') ;
     
     Soy.dat.cols<-c("character", "character" , "character", "NULL","NULL","NULL","NULL","NULL","NULL", "NULL","NULL","NULL", "NULL" , "NULL" , "NULL" , "NULL", "NULL") ;
     
     Soy.dat<-read.table(paste0("./",i,"/Soybean.dat"), header=F, skip=2,sep='\t',colClasses =Soy.dat.cols) ;
     
     Soy.dat.names<-read.table(paste0("./",i,"/Soybean.dat"), header=T, nrows=1, sep='\t',colClasses =Maize.dat.cols) ;
     
     
     names(Soy.dat)<-names(Soy.dat.names)   ;
     
     Soy.dat$DATE<-as.Date(Soy.dat$DATE,format="%F" )  ;
     
     head(Soy.dat)  ;   str(Soy.dat)  ;
     
     
     ###############################################################################################################
     #                        Merge the datasets into a dataframe     
     ###############################################################################################################
     
     Crop.dat<-merge(Maize.dat,Soy.dat,by="DATE");
     
     head(Crop.dat)  ;   str(Crop.dat)  ;
     
     
     
     Nleach.dat<-merge(Crop.dat,Nitrogen.dat, by ="DATE")  ;
     
     names(Nleach.dat)[c(2:5)]<-c("Maize", "Maize_Stage" ,"Soy", "Soy_Stage") ;
     
     
     head(Nleach.dat)  ;   str(Nleach.dat)  ;
     
     ############## Converting the charracter data for crop and crop stage into factors 
     
     Nleach.dat$Maize<-as.factor(Nleach.dat$Maize);
     
     levels(Nleach.dat$Maize)<-c("Fallow", "Maize" , "No_Crop") ;
     
     Nleach.dat$Maize_Stage<-as.factor(Nleach.dat$Maize_Stage);
     
     levels(Nleach.dat$Maize_Stage)<-c("Maturity" , "n/a" , "Planting" , "Pre_emergence" , "Reproductive_Growth" , "Vegetative_growth")   ;
     
     
     Nleach.dat$Soy<-as.factor(Nleach.dat$Soy)  ;
     
     levels(Nleach.dat$Soy)<-c("Fallow" , "No_Crop" , "Soybean") ;
     
     Nleach.dat$Soy_Stage<-as.factor(Nleach.dat$Soy_Stage);
     
     levels(Nleach.dat$Soy_Stage)<-c("Maturity" , "n/a" , "Planting" , "Pre_emergence" , "Reproductive_Growth" , "Vegetative_growth")   ;
     
     
     plot(Nleach.dat$DATE,Nleach.dat$NO3.LEACHING,type="l");
     
   
     ###############################################################################################################
     #                       Select periods of Corn to Soy bean to calculate cummulative leaching
     ###############################################################################################################
     
     Nleach.Corn.Soy.dat<-Nleach.dat[Nleach.dat$Maize_Stage == "Planting" |  Nleach.dat$Soy_Stage == "Maturity"  ,]  ;
     
     
     #  Get rid of the dates with multiple maturities
     
     Select.Periods<-(Nleach.Corn.Soy.dat$DATE[c(seq(2,length(Nleach.Corn.Soy.dat$DATE)),length(Nleach.Corn.Soy.dat$DATE))] - Nleach.Corn.Soy.dat$DATE)==0 | (Nleach.Corn.Soy.dat$DATE[c(seq(2,length(Nleach.Corn.Soy.dat$DATE)),length(Nleach.Corn.Soy.dat$DATE))] - Nleach.Corn.Soy.dat$DATE) >1 
     
     
     
     # extract correct periods 
     
     Nleach.Periods<-Nleach.Corn.Soy.dat[Select.Periods,]
     
     
     # Initialize the data frame to store the cumulative leaching
     
     Nleach.cum<-data.frame()   ;
     
     
     for (j in seq (1, dim(Nleach.Periods)[1]-2,2)) {
          
          print(j)
          
          Cum.leaching<-Nleach.Periods[c(j,j+1), c("DATE", "NO3.LEACHING")] 
          
          Cum.leaching
          
          days=seq.Date(Nleach.Periods$DATE[j],Nleach.Periods$DATE[j+1],"day")   ;
          
          head(days) ; tail(days)  ;
          
          Cum.leaching[,2]=sum(Nleach.dat[Nleach.dat$DATE %in% days,c("NO3.LEACHING")])   ;
          
          Cum.leaching
          
          
          Nleach.cum<-rbind(Nleach.cum,Cum.leaching)     ;
          
          Nleach.cum
          
          
          
          rm(Cum.leaching,days) ;
          
          
     }
     
     
     
     
     Nleach.cum$File_Name<-i  ;
     
     
     Summary.High_C_no_cover<-rbind(Summary.High_C_no_cover,Nleach.cum)    ;
     
     Data_to_Remove<-ls()[!ls() %in% c("Summary.High_C_no_cover","i")]
     
     rm(Data_to_Remove)     ;
     
     
}

###############################################################################################################
#                          Sumarixing the results and writing them into excell               
###############################################################################################################


Summary.High_C_no_cover

Summary.High_C_no_cover$Start.year<-as.numeric(format(as.Date(substr(Summary.High_C_no_cover$File_Name,start=21, stop=23),format="%y"), "%Y")  ) ;

Summary.High_C_no_cover<-Summary.High_C_no_cover[order(Summary.High_C_no_cover$Start.year,Summary.High_C_no_cover$DATE),]   ;

Summary.High_C_no_cover$DATE<-format(Summary.High_C_no_cover$DATE,"%F")    ;

Summary.High_C_no_cover<-Summary.High_C_no_cover[,c("Start.year", "DATE", "NO3.LEACHING", "File_Name")]      ;

writeWorksheetToFile("../OutputSummary/Summary.xlsx",Summary.High_C_no_cover, sheet="High_C_no_cover" )      ; 





