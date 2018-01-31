############################################################################################################################


# Luke, Douglas. 2015. A User's Guide to Network Analysis in R. Use R! Cham: Springer International Publishing. https://doi.org/10.1007/978-3-319-23883-8.
# 
# Using R network packages to manage the river network in PIHM and PIHM GIS
# 
# Felipe Montes 01/23/2018
# 
############################################################################################################################


#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

#  Set Working directory


library(devtools)  ;
install_github("DougLuke/UserNetR")  ;

install.packages("statnet")  ;


library(statnet)
library(UserNetR)
data(Moreno)

gender <- Moreno %v% "gender"

plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)

network.size(Moreno)

summary(Moreno,print.adj=FALSE)


netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1,matrix.type="adjacency")
class(net1)

summary(net1)

gplot(net1, vertex.col =2, displaylabels = TRUE)

netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
net2 <- network(netmat2,matrix.type="edgelist")
network.vertex.names(net2) <- c("A","B","C","D","E")

summary(net2)


set.vertex.attribute(net1, "gender", c("F", "F", "M",
                                       "F", "M"))
net1 %v% "alldeg" 
  
detach(package:statnet) ;

library(igraph)  ;




############################################################################################################################


# Kolaczyk, Eric D., and Gábor Csárdi. 2014. Statistical Analysis of Network Data with R. Vol. 65. Use R! New York, NY: Springer New York. https://doi.org/10.1007/978-1-4939-0983-4.

# https://github.com/kolaczyk/sand/blob/master/sand/inst/code/chapter2.R

# Using R network packages to manage the river network in PIHM and PIHM GIS
# 
# Felipe Montes 01/24/2018


############################################################################################################################



install.packages("sand") ;
library(sand)
install_sand_packages() ;

g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7) ;

V(g)

E(g)

str(g)


dg <- graph.formula(1-+2, 1-+3, 2++3)
plot(dg)


dg <- graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)
str(dg)


V(dg)$name <- c("Sam", "Mary", "Tom")

V(dg)$gender <- c("M","F","M")

V(dg)$color <- "red"

g$name <- "Toy Graph"



g.lazega <- graph.data.frame(elist.lazega, 
                             directed="FALSE", 
                             vertices=v.attr.lazega);


g.lazega$name <- "Lazega Lawyers"  ;


plot(g.lazega)

is.simple(g.lazega) 

mg <- g + edge(2,3) ;

str(mg)

is.simple(mg)

g <- make_ring(10)
g$layout <- layout_in_circle
plot(g)
tkplot(g)
rglplot(g)


g <- barabasi.game(100)
plot(g, layout=layout_with_fr, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

# CHUNK 8
library(igraphdata)
data(karate)
# Reproducible layout
set.seed(42)
l <- layout.kamada.kawai(karate)
# Plot undecorated first.
par(mfrow=c(1,1))
plot(karate, layout=l, vertex.label=NA)
# Now decorate, starting with labels.
V(karate)$label <- sub("Actor ", "", V(karate)$name)
# Two leaders get shapes different from club members.
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"
# Differentiate two factions by color.
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"
# Vertex area proportional to vertex strength
# (i.e., total weight of incident edges).
V(karate)$size <- 4*sqrt(graph.strength(karate))
V(karate)$size2 <- V(karate)$size * .5
# Weight edges by number of common activities
E(karate)$width <- E(karate)$weight
# Color edges by within/between faction.
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[ F1 %--% F1 ]$color <- "pink"
E(karate)[ F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"
# Offset vertex labels for smaller points (default=0).
V(karate)$label.dist <- 
  ifelse(V(karate)$size >= 10, 0, 0.75)
# Plot decorated graph, using same layout.
plot(karate, layout=l)


# CHUNK 9

data(lazega)
# Office location indicated by color.
colbar <- c("red", "dodgerblue", "goldenrod")
v.colors <- colbar[V(lazega)$Office]
# Type of practice indicated by vertex shape.
v.shapes <- c("circle", "square")[V(lazega)$Practice]
# Vertex size proportional to years with firm.
v.size <- 3.5*sqrt(V(lazega)$Years)
set.seed(42)
l <- layout.fruchterman.reingold(lazega)
plot(lazega, layout=l, vertex.color=v.colors,
     vertex.shape=v.shapes, vertex.size=v.size)



#####   trying with the river network  ####################

# From MM_PIHMInputsR.V2.R

g.River.edges<-FROM.TO.River.Nodes[,c('FROM.x', 'TO.x', 'INDEX')]  ;

g.River.vertices<-River.Nodes.Elevation ;

g.River<-graph.data.frame(g.River.edges, vertices=g.River.vertices, directed = T) ;

tkplot(g.River,layout=as.matrix.data.frame(River.Nodes.Elevation[,c('X', 'Y')]), vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

plot(g.River,layout=as.matrix.data.frame(River.Nodes.Elevation[,c('X', 'Y')]), vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

plot(g.River, layout=layout_nicely,vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

tkplot(g.River, canvas.width=1800, canvas.height=900, layout=layout_nicely,vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

plot(g.River,layout=as.matrix.data.frame(River.Nodes.Elevation[,c('Zmax','Zmax')]), vertex.size= 2, edge.arrow.size=0.1, vertex.label=NA)

tkplot(g.River,canvas.width=1800, canvas.height=900,layout=as.matrix.data.frame(River.Nodes.Elevation[,c('Zmax','Zmax')]), vertex.size= 2, edge.arrow.size=0.1, vertex.label=NA)





vcount(g.River) 

ecount(g.River)

str(g.River)
V(g.River)
E(g.River)

tkplot(g.River)




##### Add the river nodes from the unrefined river mesh#########################
library(sp) ;

library(rgdal) ;


# Read information about the shape file

MergedRiver.info<-ogrInfo('C:/Aun Trabajo en Proceso/HansYostDeepCreek/MergedRiver.shp');

MergedRiver.info$nrows 


# Read  the shape file

MergedRiver<-readOGR('C:/Aun Trabajo en Proceso/HansYostDeepCreek/MergedRiver.shp') ;


str(MergedRiver, max.level = 2)  ;


# get the ID's of the line segments in the shape file read

sapply(slot(MergedRiver,"lines"), function(x) slot(x,"ID"))  


#get the coordinates of the first line segment of the shape file read

coordinates(MergedRiver)[[1]]

# get the coordinates of all the line segments of the shape file read in a list form


lapply(coordinates(MergedRiver),function(x) x[[1]])

# get the coordinates of all the line segments of the shape file read as an  array 

sapply(coordinates(MergedRiver),function(x) x[[1]])


# transform the array of line segments coordinates into a matrix format 




MergedRiver.coords.matrix<-matrix(data=sapply(coordinates(MergedRiver),function(x) x[[1]]), nrow = MergedRiver.info$nrows, ncol = 4, byrow=T) ;


# arrange the matrix of line segments coordinates into a coherent data rame with lines representd by two points consiting of a pari of x,y coordinates

MergedRiver.coords.df<-data.frame(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)]);

names(MergedRiver.coords.df)<-c('P1.X', 'P1.Y', 'P2.X', 'P2.Y') ;

#addd a line ID to the data frame to be able to differentiate line components 

#Point.coords.df$Line<-sapply(slot(HYDC,"lines"), function(x) slot(x,"ID")) ;

MergedRiver.coords.df$Line.ID<-as.character(seq(1:MergedRiver.info$nrows)) ;

# stack the points of all the lines obtained in matrix from to remove repeated points ( there are many, all the lines that are contiguous share common points) and assign unique points a unique point ID

MergedRiver.Stacked.Point.coords<-rbind(MergedRiver.coords.matrix[,c(1,3)],MergedRiver.coords.matrix[,c(2,4)])   ;


str(MergedRiver.Stacked.Point.coords) ;



MergedRiver.Unique.Point.coords<-data.frame(unique(MergedRiver.Stacked.Point.coords))  ;

names(MergedRiver.Unique.Point.coords)<-c("X" , "Y");

MergedRiver.Unique.Point.coords$Point.ID<-seq(1:dim(MergedRiver.Unique.Point.coords)[1]) ;

head(MergedRiver.Unique.Point.coords) ;
str(MergedRiver.Unique.Point.coords)  ;

#Use the new point and lines unique identity to create a line point data frame that has, each point unique id forming the lines. This is acomplished by merging the unique points data frame with each set of from and to points in the lines data frame



MergedRiver.From.points<-merge(MergedRiver.coords.df[,c("P1.X", "P1.Y" ,  "Line.ID")], MergedRiver.Unique.Point.coords, by.x=c('P1.X', 'P1.Y'), by.y=c("X" , "Y"), all.x=T);

head(MergedRiver.From.points) ;

str(MergedRiver.From.points) ;


MergedRiver.To.points<-merge(MergedRiver.coords.df[,c("P2.X", "P2.Y" ,  "Line.ID")], MergedRiver.Unique.Point.coords, by.x=c('P2.X', 'P2.Y'), by.y=c("X" , "Y"), all.x=T);



MergedRiver.Line.Point<-merge(MergedRiver.From.points,MergedRiver.To.points,by="Line.ID");

str(MergedRiver.Line.Point)
head(MergedRiver.Line.Point)

###### Put together the  Merged river and the refined river nodes ######

head(MergedRiver.Unique.Point.coords) ;
str(MergedRiver.Unique.Point.coords);
head(River.Nodes.Elevation) ;
str(River.Nodes.Elevation) ;


Refined.Merged.Nodes<-merge(River.Nodes.Elevation, MergedRiver.Unique.Point.coords, by=c('X', 'Y'),all.x=T) ;

head(Refined.Merged.Nodes)
str(Refined.Merged.Nodes)
summary(Refined.Merged.Nodes)

g.River.2.edges<-FROM.TO.River.Nodes[,c('FROM.x', 'TO.x', 'INDEX')]  ;

g.River.2.vertices<-Refined.Merged.Nodes[, c('Index', 'X', 'Y','Zmax', 'Point.ID') ] ;


g.River.2<-graph.data.frame(g.River.2.edges, vertices=g.River.2.vertices, directed = T) ;


plot(g.River.2, layout=layout_nicely,vertex.size= 1, edge.arrow.size=0.1, vertex.label=NA)

tkplot(g.River.2, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=NA)

is.simple(g.River.2)

E(g.River.2)
degree(g.River.2, mode="in")
which(degree(g.River.2) == 0)
degree(g.River.2, mode="out")
which(degree(g.River.2, mode="out") == 2)

V(g.River.2)[is.na(Point.ID)]$color='Blue'
V(g.River.2)[!is.na(Point.ID)]$color='Red'


tkplot(g.River.2, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=g.River.2.vertices$Index, vertex.label.cex=1, vertex.label.dist=1)   ;

tkplot(g.River.2, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=paste(g.River.2.vertices$Index,round(g.River.2.vertices$Zmax,2),sep="-"), vertex.label.cex=1, vertex.label.dist=1) ;


tkplot(g.River.2[1:100], canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1, vertex.label=paste(g.River.2.vertices$Index,round(g.River.2.vertices$Zmax,2),sep="-"), vertex.label.cex=1, vertex.label.dist=1);

g.River.sub<-induced.subgraph(g.River.2,seq(1,30)) ;

V(g.River.sub)[is.na(Point.ID)]$color='Blue'
V(g.River.sub)[!is.na(Point.ID)]$color='Red'


tkplot(g.River.sub, canvas.width=1800, canvas.height=900, layout=layout_nicely ,vertex.size= 2, edge.arrow.size=0.1,vertex.label=paste(V(g.River.sub)$name,round(V(g.River.sub)$Zmax,2),sep="-"),vertex.label.cex=1, vertex.label.dist=1,margin=0.2) ;


str(g.River.sub)


V(g.River.sub)$name

list.vertex.attributes(g.River.sub)

list.edge.attributes(g.River.sub)


# Correct  vertex Zmax based on the grap and visial inspection

Ncorrec<-Refined.Merged.Nodes ;

#The correction is easy to do in Excel


library(XLConnect);

writeWorksheetToFile('C:/Users/frm10/Downloads/Ncorrec.xlsx', Ncorrec, sheet="Ncorrec", startRow=2);

Correct.Nodes<-readWorksheetFromFile('C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/Manhantango/HydroTerreFullManhantango/HansYostDeepCreek/Ncorrec.xlsx', sheet="Ncorrec", startRow = 1, endRow = 305, startCol= 1, endCol=8 );

head(Correct.Nodes)

plot(Correct.Nodes$Index,Correct.Nodes$Zmax.Correct)


River.Nodes.Elevation.Corrected<-RNEC<-Correct.Nodes[,c('Index' , 'X' , 'Y' , 'Zmin' , 'Zmax' , 'Zmax.Correct')] ;



