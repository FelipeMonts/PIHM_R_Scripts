#############################################################################################################################
#
#  Program read binary files from the phim output.
#  Based on the Matlab program that Yubibg developed for the same process
#   
#   function [time, var] = pihm_read_output_func (filename, nvar, elem)
# 
#   % filename: Name of the output file
#   % nvar: Number of elements contained. It should be either the number of
#   % triangular element or the number of river segments, depending on the file
#   % you are reading.   Triangles = 883 , River Seg =114
#   % elem: Index of elements that you want to read. If you want to read all
#   % elements, use 1:nvar.
# 
#   fid = fopen (filename);
# 
#   fseek(fid, 0, 'eof');
#   dim = ftell (fid) / 8 / (nvar + 1);
# 
#   frewind(fid);
#   rawtime = fread (fid, double (dim), 'double', nvar * 8);
# 
#   frewind (fid);
#   var = zeros (dim, length(elem));
#   for i = 1 : dim
#   fseek (fid, 8, 0);
#   data = fread (fid, double (nvar), 'double');
#   var(i, :) = data(elem);
#   end
# 
#   time = rawtime / 60.0 / 60.0 / 24.0 + datenum (1970, 1, 1);
#   fclose (fid);
# 
# #
#
#  Felipe Montes,  2017/03/29

# 
#     cd('D:/')
#     ls
#     nvar = 883
#     elem = 1:nvar
#     filename = 'WE38.recharge.dat'
# 
# 
# 
#     %   function [time, var] = pihm_read_output_func (filename, nvar, elem);
#     % 
#     %   % filename: Name of the output file
#     %   % nvar: Number of elements contained. It should be either the number of
#     %   % triangular element or the number of river segments, depending on the file
#     %   % you are reading.   Triangles = 883 , River Seg =114
#     %   % elem: Index of elements that you want to read. If you want to read all
#     %   % elements, use 1:nvar.
#     % 
#     fid = fopen('WE38.recharge.dat');
# 
#     fseek(fid, 0, 'eof'); % fseek / Move to specified position in file / eof = end of file 
#     dim = ftell (fid) / 8 / (nvar + 1); % ftell  /Position in open file / divided by 8 bytes per entry/
#     % divided by the number triangle elements (nvar = 883)
#     % results in the number of time steps the ouput was recorded (if in days for one year is 366 lines) %
#   
#   
#     frewind(fid);  %  frewind / Move file position indicator to beginning of open file
#     rawtime  = fread (fid, double (dim), 'double', nvar * 8) % fread / Read data from binary file / fread(fileID,sizeA,precision,skip)
# 
#     frewind (fid); %  frewind / Move file position indicator to beginning of open file
#     var = zeros (dim,length(elem)); %  zeros / Create array of all zeros / 
#     %zeros(sz1,...,szN) returns an sz1-by-...-by-szN array of zeros where sz1,...,szN indicate the size of each dimension. For example, zeros(2,3) returns a 2-by-3 matrix
#     for i = 1 : dim
#     fseek (fid, 8, 0);
#     data = fread (fid, double (nvar), 'double');
#     var(i, :) = data(elem);
#     end
#     % 
#     time = rawtime / 60.0 / 60.0 / 24.0 + datenum (1970, 1, 1);
#     %   fclose (fid);

#
##############################################################################################################################


############################### Record Time To start##########################################################



TimeStart<-Sys.time()  ;



###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/Sotware&Coding/R_Library/library")  ;

#  Set Working directory


setwd("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/SWATPIHMRcode") ; 


###############################################################################################################
#                         Call packages needed to process the data 
#                             
###############################################################################################################

library("foreign") ;
library("dplyr") ;
library("hexView") ;



###############################################################################################################
#                      Read binary files 
###############################################################################################################
list.files("../")   ;

file_info<-file.info("../WE38.recharge.dat")   ; # information about the binary file

word.size=8 ;  # eight bytes per word

Triangle.no=883 ; # number of triangles in the output simulation

No.Rows<-file_info$size/word.size/(Triangle.no+1) ; # number of rows obtained by dividing the file size in bytes
# by wordsize*(Tiangle.no +1 (time stamp))  

Read_data<-readBin("../WE38.recharge.dat" , "numeric" , size=word.size, n=file_info$size/word.size) ; # read the binary filw

Recharge<-as.data.frame(matrix(data=Read_data, ncol=Triangle.no+1, byrow=T)); # create a matrix with the binary file
# and then transforme it into a dataframe.


Recharge$Time<-as.POSIXct(Recharge[,1], origin="1970-01-01", tz="UTC") ;



