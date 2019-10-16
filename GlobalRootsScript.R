##Global_Roots_Mapping##
# In this project, we compile raster data representing biome distributions under scenarios of potential land cover (Ramankutty)in the absense of human influence, contemporary land cover (GLC2000) and projected ICPP scenarios and combine them with empirically derived rooting depth distributions to calculate changes to rooting depths resulting from anthropogenic activity. 

#Needed packages:
install.packages("sp")
install.packages("rgdal")
install.packages("mefa")
install.packages("rworldmap")
install.packages("SDMTools")
install.packages("plotfunctions")
install.packages("ggmap")
install.packages("rgeos")
install.packages("raster")
install.packages("ncdf4")
install.packages("reshape2")
install.packages("RNetCDF")
install.packages("chron")

library(rgeos)
library(ggmap)
library(ggplot2)
library(maps)
library(sp)
library(rgdal)
library(dplyr)
library(mefa)
library(colorspace)
library(RColorBrewer)
library(raster)
library(maptools)
library(SDMTools)
library(plotfunctions)
library(rworldmap)
library(ncdf4)
library(reshape2)
library(RNetCDF)
library(chron)
library(lattice)


###Datasets needed:
 #Potential veg, modified to divide northern and arid deserts and to #account for known rooting limitations in northern biomes above 60 #degrees N, were frozen soils restrict root growth.

 potentVeg=raster("PVeg_Adj2.tif")


 #Contemporary veg, modified to divide northern and arid deserts and to #account for known rooting limitations in northern biomes above 60 #degrees N, were frozen soils restrict root growth. Because #contemporary map categories were reassigned to align with potential #veg categories, there are 2 distinct maps for root equation #assignemnts and aboveground vegetation class assignments. 

GLC2000Veg <- raster("Contemporary_Vegetation.tif")
NewGLC <- raster("Contemporary_roots.tif")



###Plot Potential Veg distribution (Fig 1A)
##This command plots the 2 maps together:
par(mfrow = c(1,2),mar = c(1.2, 1, 1.5,0.25))

#Make color designations for the map
colPoV <- c("forestgreen", "green4", "darkgreen", "darkseagreen","darkblue", "darkorchid", "red2", "red4", "firebrick3", "firebrick4", "lightblue", "grey", "lightblue")

plot(potentVeg, col = colPoV,legend = F, main = "Potential Vegetation",axes = F, box = F)

#Plot legend separately for paper figs. 

VegNames1 <- c("TropEver", "TropDecid","TempEver", "TempDecid", "Boreal", "MixedVeg", "Savanna", "GrassSteppe", "DenseShrub", "OpenShrub", "Tundra", "Arid/SemiArid", "Agriculture", "Urban", "Burned")
#Make a blank plot and put the legend in it. 
plot(5,-5,col = "white")
LedgendVegCol1 <- c("forestgreen", "green4", "darkgreen", "darkseagreen","darkblue", "darkorchid", "red2", "red4", "firebrick3", "firebrick4", "lightblue", "grey", "orange2", "black", "bisque4")
legend("bottomleft", legend = VegNames1, fill = LedgendVegCol1, cex = 1.25, bty = "n", x.intersp = 0.3, y.intersp = 0.4)


###Plot Contemporary Veg distribution (Fig 1B)

#Veg csv to denote colors to use for contemp veg map, so that they match potential veg map.

VegDat <- read.csv("ContVegR.csv")
#Vector of biome numbers (1-24)
VegNum <- VegDat$VALUE

#R-relevant Color numbers to go with biomes
VegCol <- VegDat$Color

#Make into a matrix and reassign color values to the veg raster cells.
MatValsVeg <- c(VegNum,VegCol)
NewVeg <- matrix(MatValsVeg, ncol = 2, byrow = F)
ContVeg2 <- reclassify(GLC2000Veg, NewVeg)


#Now make the map
colBio16 <- c("black","bisque4", "darkgreen", "forestgreen","darkblue", "darkorchid",  "orange2","orange2", "orange2", "darkseagreen", "grey", "red2", "red4", "red2", "lightblue","white")
VegNames <- c("Urban", "Burned", "TempEver", "TropEver","Boreal", "MixedVeg", "Ag Lands", "TempDecid", "Arid/SemiArid", "Savanna", "GrassSteppe","Tundra")
plot(ContVeg2, col = colBio16, legend = F, main = "Contemporary Vegetation")

##make a separate legend as in potential veg map above. 
LedgendVegCol <- c("black","bisque4", "darkgreen", "forestgreen","darkblue", "darkorchid",  "orange2", "darkseagreen", "grey", "red2", "red4", "lightblue","white")
legend("bottomleft", legend = VegNames, fill = LedgendVegCol,  cex = 0.6, bty = "n", x.intersp = 0.25, y.intersp = 0.5)


###Apply rooting depth equations to vegetation rasters and map them D99, D95, D50 for potential/contemporary.
##For potential veg
#These csv files contain the rooting depth, equation based datasets for D99, D95 and D50
#The first includes deserts but sets their root depth to 0
BiomeMeas <- read.csv("/Users/lindaparsons/Desktop/Biomes_Roots_Data/BiomeRootDepth_Datasets_Desert0.csv")
#The second sets desert roots to a 4m depth 
BiomeMeas2 <- read.csv("/Users/kbs/Desktop/Global Roots/BiomeRootDepth_Datasets.csv")
#The last sets desert cells as "NA" so they are not part of the rooting depth average. This is because a lot of desert regions don't register any meaningful GPP values in satelite-derived datasets. 
BiomeMeas3 <- read.csv("BiomeRootDepth_Datasets_DesNA.csv")

#Apply these .csv values to the raster


##For Deserts with 0m roots

#D99
bioValsP <- c(1:13) 
D99P <- BiomeMeas$D99 
matValsP <- c(bioValsP, D99P)
reclass_matP <- matrix(matValsP, ncol=2, byrow = F)
PVegD99 <- reclassify(PVeg_adj, reclass_matP)

$D95
D95P <- BiomeMeas$D95 
matValsP2 <- c(bioValsP, D95P)
reclass_matP2 <- matrix(matValsP2, ncol=2, byrow = F)
PVegD95 <- reclassify(PVeg_adj, reclass_matP2)

#D50
D50P <- BiomeMeas$D50 
matValsP3 <- c(bioValsP, D50P)
reclass_matP3 <- matrix(matValsP3, ncol=2, byrow = F)
PVegD50 <- reclassify(PVeg_adj, reclass_matP3)
cellStats(PVegD50, 'mean', na.rm = T) # If desert is 0, 0.1439884


#For deserts with 4m Deep roots

#D99
D99P2 <- BiomeMeas2$D99 
matValsP2 <- c(bioValsP, D99P2)
reclass_matP2 <- matrix(matValsP2, ncol=2, byrow = F)
PVegD992 <- reclassify(Pveg_adj, reclass_matP2)

#D95
D95P2 <- BiomeMeas2$D95 
matValsP2 <- c(bioValsP, D95P2)
reclass_matP2 <- matrix(matValsP2, ncol=2, byrow = F)
PVegD952 <- reclassify(Pveg_adj, reclass_matP2)


#For deserts with roots = NA

#D99
D99P3 <- BiomeMeas3$D99 
matValsP3 <- c(bioValsP, D99P3)
reclass_matP3 <- matrix(matValsP3, ncol=2, byrow = F)
PVegD993 <- reclassify(PVeg_adj, reclass_matP3)
#To calculate the mean, use the following code:
#cellStats(PVegD993, 'mean', na.rm = T ) # 1.838896 m 

#D95
D95P3 <- BiomeMeas3$D95 
matValsP3 <- c(bioValsP, D95P3)
reclass_matP3 <- matrix(matValsP3, ncol=2, byrow = F)
PVegD953 <- reclassify(PVeg_adj, reclass_matP3)
#To calculate the mean, use the following code:
#cellStats(PVegD953, 'mean', na.rm = T ) # 1.087

#D50
D50P3 <- BiomeMeas3$D50 
matValsP4 <- c(bioValsP, D50P3)
reclass_matP4 <- matrix(matValsP4, ncol=2, byrow = F)
PVegD503 <- reclassify(PVeg_adj, reclass_matP4)
#To calculate the mean, use the following code:
#cellStats(PVegD503, 'mean', na.rm = T)



##For Contemporary Veg

##Read in same 3 .csv files as for potential but with root values for contemporary biomes, D99, D95, D50

VegDat <- read.csv("ContRootsR.csv")	#This one includes deep, 4m desert roots
VegDat2_Des0 <- read.csv("ContRootsR2_Desert0.csv") #This one sets desert roots to 0,
VegDat3_DesNA <- read.csv("ContRootsR_DesertNA.csv") #This one makes desert roots "NA"

#Obtain the biome class number values
VegNum <- VegDat$VALUE
VegNum2 <- VegDat2_Des0$VALUE
VegNum3 <- VegDat3_DesNA$VALUE

#Make vectors of the rooting depth values
#For 4m Deep roots
D99 <- VegDat$D99
D95 <- VegDat$D95
D50 <- VegDat$D50

#For 0m deep roots
D99_D0 <- VegDat2_Des0$D99
D95_D0 <- VegDat2_Des0$D95
D50_D0 <- VegDat2_Des0$D50

#For NA desert roots
D99_DNA <- VegDat3_DesNA$D99
D95_DNA <- VegDat3_DesNA$D95
D50_DNA <- VegDat3_DesNA$D50


#make matrices with biome number in one column and D99/D95/D50 values in another

MatValsVeg <- c(VegNum,D99)
MatValsVeg2 <- c(VegNum,D95)

MatValsVeg_D0 <- c(VegNum2,D99_D0)
MatValsVeg2_D0 <- c(VegNum2,D95_D0)
MatValsVeg3_D0 <- c(VegNum2,D50_D0)

MatValsVeg_DNA <- c(VegNum3,D99_DNA)
MatValsVeg2_DNA <- c(VegNum3,D95_DNA)
MatValsVeg3_DNA <- c(VegNum3,D50_DNA)

NewVeg <- matrix(MatValsVeg, ncol = 2, byrow = F)
NewVeg2 <- matrix(MatValsVeg2, ncol = 2, byrow = F)

NewVeg_D0 <- matrix(MatValsVeg_D0, ncol = 2, byrow = F)
NewVeg2_D0 <- matrix(MatValsVeg2_D0, ncol = 2, byrow = F)
NewVeg3_D0 <- matrix(MatValsVeg3_D0, ncol = 2, byrow = F)

NewVeg_DNA <- matrix(MatValsVeg_DNA, ncol = 2, byrow = F)
NewVeg2_DNA <- matrix(MatValsVeg2_DNA, ncol = 2, byrow = F)
NewVeg3_DNA <- matrix(MatValsVeg3_DNA, ncol = 2, byrow = F)

#Use the matrices to reclassify biome class designations according to their rooting depth metrics

ContVeg2 <- reclassify(NewGLC, NewVeg)# new mean D99:0.5522924
ContVeg3 <- reclassify(NewGLC, NewVeg2)

#For desert roots = 0m
ContVeg4 <- reclassify(NewGLC, NewVeg_D0)
ContVeg5 <- reclassify(NewGLC, NewVeg2_D0)
ContVeg6_D50 <- reclassify(NewGLC, NewVeg3_D0)

#Check the means for each metric, D99, D95, D50
cellStats(ContVeg4, 'mean', na.rm = T)
cellStats(ContVeg5, 'mean', na.rm = T)
cellStats(ContVeg6_D50, 'mean', na.rm = T)

#For Desert roots = NA
ContVeg7 <- reclassify(NewGLC, NewVeg_DNA) #D99
ContVeg8 <- reclassify(NewGLC, NewVeg2_DNA) #D95
ContVeg9 <- reclassify(NewGLC, NewVeg3_DNA) #D50

#Check the means for each metric, D99, D95, D50
cellStats(ContVeg7, 'mean', na.rm = T )#0.4285623, with desert = 0, 1.4668m
cellStats(ContVeg8, 'mean', na.rm = T )
cellStats(ContVeg9, 'mean', na.rm = T )



##Use these new root depth rasters to make maps, histograms, and 95% CIs/Stats (Fig 1C-F, Histograms, and Stats)

#RootDepths Maps
par(mfrow=c(2,2), mar=c(0.5,.5,.5,.5))
 
#Potential Veg D95 (Fig 1C)
plot(PVegD953, zlim = c(0,3.2), col = rev(sequential_hcl(100)), box=F, axes =F)
#legend("bottomleft", legend = "Potential D95, mean = 1.08m", cex = 0.5, bty = "n")

#Contemporary Veg D95 (Fig 1D)
plot(ContVeg8, zlim = c(0,3.2), col = rev(sequential_hcl(100)), legend = F, box=F, axes =F)
#legend("bottomleft", legend = "Contemporary D95, mean = 1m", cex = 0.5, bty = "n")

#Potential Veg D95 (Fig 1E)
plot(PVegD993, zlim = c(0,3.2), col = rev(sequential_hcl(100)), legend = F, box=F, axes =F)
#legend("bottomleft", legend = "Potential D99, mean = 1.84m", cex = 0.5, bty = "n")

#Contemporary Veg D99 (Fig 1F)
plot(ContVeg7, zlim = c(0,3.2), col = rev(sequential_hcl(100)), legend = F, box=F, axes =F)
#legend("bottomleft", legend = "Contemporary D99, mean = 1.67m", cex = 0.5, bty = "n")

###SUPPLEMENT_CompanionFigD50
par(mfrow=c(1,2), mar = c(0,0,0,0))

#Potential Veg D50 (Fig S1A)
plot(PVegD503, zlim = c(0,0.3), col = rev(sequential_hcl(100)), box=F, axes =F)
#Contemporary Veg D50 (Fig S1B)
plot(ContVeg9, zlim = c(0,0.3), col = rev(sequential_hcl(100)), legend = F, box=F, axes =F)


###Making Histograms
##Histogram for D99 (Desert roots set to NA)
par(mfrow = c(2,1))
hist(PVegD993, col=rgb(0,0,1,0.5), freq = F, breaks = c( 0, 0.25,0.5,0.75,1,1.25, 1.5,1.75,2,2.25,2.5,2.75,3,3.25), xlab = "Rooting Depth (D99)")
hist(ContVeg7, col= rgb(1,0,0,0.5), add = F, freq = F,breaks = c(0, 0.25,0.5,0.75,1,1.25, 1.5,1.75,2,2.25,2.5,2.75,3,3.25))
#legend("topright", legend = c("Potential", "Contemporary"), fill = c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)), bty = "n", cex = 1)

##Histogram for D95
par(mfrow = c(2,1))
hist(PVegD953, col=rgb(0,0,1,0.5), freq = F, breaks = c( 0, 0.25,0.5,0.75,1,1.25, 1.5,1.75,2), xlab = "Rooting Depth (D95)")
hist(ContVeg8, col= rgb(1,0,0,0.5), add = F, freq = F,breaks = c(0, 0.25,0.5,0.75,1,1.25, 1.5,1.75,2))
#legend("topright", legend = c("Potential", "Contemporary"), fill = c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)), bty = "n", cex = 1)

##Histogram for D50
par(mfrow = c(2,1), mar = c(4,4,2,1))
hist(PVegD503, col=rgb(0,0,1,0.5), freq = F, breaks = c( 0,0.1,0.2,0.3), xlab = "Rooting Depth (D50)", ylim = c(0,8))
hist(ContVeg9, col= rgb(1,0,0,0.5), add = F, freq = F,breaks = c(0,0.1,0.2,0.3))


####Calculating a Statistic
##First attempt will be 95% CI around the mean of:

  #Cont D99, potential D99
cellStats(ContVeg7, stat = 'sd', na.rm = T) #Find the sd because there's no se equation
NAs <- freq(ContVeg7,digits=0, value= NA)	#find the number of NA cells that were not included in the calculation
se7 <- 0.9257605/(sqrt(659352960-NAs)) # Divide the sd by the square root of n to get the se. For N, we know the number of raster cells and must subtract the number of NAs to get the number of cells used in calculation
##95%CI, this gives you the +/- value for the mean
se7*1.96


sdPveg99 <- cellStats(PVegD993, stat = 'sd', na.rm = T)
NAPveg <- freq(PVegD993,digits=0, value= NA)
sePveg <- sdPveg99/(sqrt(9331200-NAPveg))
#95%CI Pveg
(1.96*sePveg)


  #Cont D95, potential D95

sd8 <- cellStats(ContVeg8, stat = 'sd', na.rm = T)
NAs8 <- freq(ContVeg8,digits=0, value= NA)
se8 <- sd8/(sqrt(659352960-NAs8))
##95%CI
se8*1.96


sdPveg95 <- cellStats(PVegD953, stat = 'sd', na.rm = T)
NAPveg95 <- freq(PVegD953,digits=0, value= NA)
sePveg95 <- sdPveg95/(sqrt(9331200-NAPveg95))
#95%CI Pveg
(1.96*sePveg95)


  #Cont D50, potential D50

sd9 <- cellStats(ContVeg9, stat = 'sd', na.rm = T)
NAs9 <- freq(ContVeg9,digits=0, value= NA)
se9 <- sd9/(sqrt(659352960-NAs9))
##95%CI
se9*1.96

sdPveg50 <- cellStats(PVegD503, stat = 'sd', na.rm = T)
NAPveg50 <- freq(PVegD503,digits=0, value= NA)
sePveg50 <- sdPveg50/(sqrt(9331200-NAPveg50))
#95%CI Pveg
(1.96*sePveg50)

###########Same as above, but with desert = 0 instead of NA

#Cont D99, potential D99
sd4 <- cellStats(ContVeg4, stat = 'sd', na.rm = T)
NAs4 <- freq(ContVeg4,digits=0, value= NA)
se4 <- sd4 /(sqrt(659352960-NAs4))
##95%CI
se4*1.96

sdPveg990 <- cellStats(PVegD99, stat = 'sd', na.rm = T)
NAPveg0 <- freq(PVegD99,digits=0, value= NA)
sePveg0 <- sdPveg990/(sqrt(9331200-NAPveg0))
#95%CI Pveg
(1.96*sePveg0)


#Cont D95, potential D95

sd5 <- cellStats(ContVeg5, stat = 'sd', na.rm = T)
NAs5 <- freq(ContVeg5,digits=0, value= NA)
se5 <- sd5/(sqrt(659352960-NAs5))
##95%CI
se5*1.96

sdPveg950 <- cellStats(PVegD95, stat = 'sd', na.rm = T)
NAPveg950 <- freq(PVegD95,digits=0, value= NA)
sePveg950 <- sdPveg950/(sqrt(9331200-NAPveg950))
#95%CI Pveg
(1.96*sePveg950)


#Cont D50, potential D50

sd6 <- cellStats(ContVeg6_D50, stat = 'sd', na.rm = T)
NAs6 <- freq(ContVeg6_D50,digits=0, value= NA)
se6 <- sd6/(sqrt(659352960-NAs6))
##95%CI
se6*1.96

sdPveg500 <- cellStats(PVegD50, stat = 'sd', na.rm = T)
NAPveg500 <- freq(PVegD50,digits=0, value= NA)
sePveg500 <- sdPveg500/(sqrt(9331200-NAPveg500))
#95%CI Pveg
(1.96*sePveg500)


###Okay. Now that we have maps of each rooting depth metric individually, time to make maps of difference between contemporary and potential vegetation.

###Difference Plotted with Desert = NA

Resample contemp veg maps so that they are the same resolution as p. veg maps

D99Cont <- resample(ContVeg7, PVegD993)
D95Cont <- resample(ContVeg8, PVegD953)
D50Cont <- resample(ContVeg9, PVegD503)

#Plot Difference in D99 from potential to contemp veg transition
##(Fig2 for MS)
#Set plot parameters
par(mfrow= c(1,1), mar = c (0.5,0.5, 0.5, 0.5))
plot((D99Cont- PVegD993), col = rev(diverge_hcl(100, power = 1)), zlim = c(-3.10,3.10), box = F, axes = F, legend =T)

###SUPPLEMENTAL Figs S2 and S3 for MS
par(mfrow= c(1,1), mar = c (0.5,0.5, 0.5, 0.5))
#D95 difference
plot((D95Cont- PVegD953), col = rev(diverge_hcl(100, power = 1)), zlim = c(-3.1,3.1), box = F, axes = F, legend =T)

#D50 Difference 
par(mfrow= c(1,1), mar = c (0.5,0.5, 0.5, 0.5))
plot((D50Cont- PVegD503), col = rev(diverge_hcl(100, power = 1)), zlim = c(-0.3,0.3), box = F, axes = F, legend =T)


###From the difference data find mean differences and determine the extent of woody encroachment vs ag expansion and rooting implications

Diff99 <- (D99Cont*100)-(PVegD993*100) #Make a raster of differences between contemp and p veg cell values
#Determine the mean difference 
cellStats(Diff99,'mean',na.rm=T) # - 6.58 

#Same for D95
Diff95 <- (D95Cont*100)-(PVegD953*100)
cellStats(Diff95,'mean',na.rm=T) # -2.63

#Same for D50
Diff50 <- (D50Cont*100)-(PVegD503*100)
cellStats(Diff50,'mean',na.rm=T) # -2.63


#subset the values that are above 0. This is approximate woody encroachment. 

#find the mean of root deepening cells in potential veg roots raster
mean(PVegD993[Diff99$layer > 0], na.rm = T)#1.251601

#find the mean of root deepening regions in contemporary map:
mean(D99Cont[Diff99 > 0], na.rm =T) #1.904363

1.251601-1.904363 # 65cm
(1.251601-1.904363)/1.251601 # 52%

##Estimating effect of Ag and encroachment on root depth

#First, reclass all contemp Ag to a single class
VegClas <- c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
NewClas <- c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,16,16,19,20,21,22,23,24)
AgComb <- cbind(VegClas, NewClas)
AgReclass <- matrix(AgComb, ncol=2, byrow = F)
ContVeg_AgCombo <- reclassify(NewGLC, AgReclass)

#make sure new ag class map is same resolution as potential veg raster
ContVeg_Adj <- resample(ContVeg_AgCombo, PVegD993)

#Find the mean root depth of cells in the potential veg map that are now agricultural lands
PotentialAgRoot <- mean(PVegD993[ContVeg_Adj == 16], na.rm =T)
PotentialAgRoot #2.317712

#We know the average in ag regions now is 1.5m for D99 so the trunction is found by this equation:
(2.317712 - 1.5)/2.317712






###IPCC PROJECTIONS###

#These are based on Landuse harmonization datasets from UMD. They are a different type of file (.nc) and #have to be read in differently. The following codes are not very elegant. They could be tighter and #could have a lot more loops and functions. But it helped me a lot to actually see each piece inside the #black box at work, and to be able to manipulate that myself. So here it is! I think copied this chunk #of code for each RCP scenario analyzed and just swapped the scenario numbers.

###SCENARIO RCP 8.5####
ncpath <- "/Users/lindaparsons/Desktop/Biomes_Roots_Data/" #THIS IS YOUR WORKING DIRECTORY
ncname <- "RCP8.5"  
ncfname <- paste(ncpath, ncname, sep="")
RCP8.5 <- nc_open(ncfname)

#Extract Longitude
lon <- ncvar_get(RCP8.5,"lon")
nlon <- dim(lon)
head(lon)

#Extract Latitude
lat <- ncvar_get(RCP8.5,"lat")
nlat <- dim(lat)

#Extract Time Data
time <- ncvar_get(RCP8.5,"time")
tunits <- ncatt_get(RCP8.5,"time","units")
nt <- dim(time)

# Select variables of interest for maps. Since I want altered land uses, I'll do urban/AG covers mostly
dname <- "secdf"
SecFor <- ncvar_get(RCP8.5,dname)
dlname <- ncatt_get(RCP8.5,dname,"long_name")
dunits <- ncatt_get(RCP8.5,dname,"units")
fillvalue <- ncatt_get(RCP8.5,dname,"_FillValue")

dname2 <- "secdn"
SecNonF <- ncvar_get(RCP8.5,dname2)
dlname <- ncatt_get(RCP8.5,dname2,"long_name")
dunits <- ncatt_get(RCP8.5,dname2,"units")
fillvalue <- ncatt_get(RCP8.5,dname2,"_FillValue")

dname3 <- "urban"
urban <- ncvar_get(RCP8.5,dname3)
dlname <- ncatt_get(RCP8.5,dname3,"long_name")
dunits <- ncatt_get(RCP8.5,dname3,"units")
fillvalue <- ncatt_get(RCP8.5,dname3,"_FillValue")

dname4 <- "c4ann"
C4Crop <- ncvar_get(RCP8.5,dname4)
dlname <- ncatt_get(RCP8.5,dname4,"long_name")
dunits <- ncatt_get(RCP8.5,dname4,"units")
fillvalue <- ncatt_get(RCP8.5,dname4,"_FillValue")

dname5 <- "c3ann"
C3Crop <- ncvar_get(RCP8.5,dname5)
dlname <- ncatt_get(RCP8.5,dname5,"long_name")
dunits <- ncatt_get(RCP8.5,dname5,"units")
fillvalue <- ncatt_get(RCP8.5,dname5,"_FillValue")

dname6 <- "c3per"
C3CropPer <- ncvar_get(RCP8.5,dname6)
dlname <- ncatt_get(RCP8.5,dname6,"long_name")
dunits <- ncatt_get(RCP8.5,dname6,"units")
fillvalue <- ncatt_get(RCP8.5,dname6,"_FillValue")

dname7 <- "c4ann"
C4CropPer <- ncvar_get(RCP8.5,dname7)
dlname <- ncatt_get(RCP8.5,dname7,"long_name")
dunits <- ncatt_get(RCP8.5,dname7, "units")
fillvalue <- ncatt_get(RCP8.5,dname7,"_FillValue")

dname8 <- "c3nfx"
C3Nfix <- ncvar_get(RCP8.5,dname8)
dlname <- ncatt_get(RCP8.5,dname8,"long_name")
dunits <- ncatt_get(RCP8.5,dname8,"units")
fillvalue <- ncatt_get(RCP8.5,dname8,"_FillValue")

dname9 <- "pastr"
Pasture <- ncvar_get(RCP8.5,dname9)
dlname <- ncatt_get(RCP8.5,dname9,"long_name")
dunits <- ncatt_get(RCP8.5,dname9,"units")
fillvalue <- ncatt_get(RCP8.5,dname9,"_FillValue")

dname10 <- "range"
Range <- ncvar_get(RCP8.5,dname10)
dlname <- ncatt_get(RCP8.5,dname10,"long_name")
dunits <- ncatt_get(RCP8.5,dname10,"units")
fillvalue <- ncatt_get(RCP8.5,dname10,"_FillValue")


# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))


SecFor[SecFor==fillvalue$value] <- NA
length(na.omit(as.vector(SecFor[,,1])))

#Edit the data down toe the time slice of interest, in this case the year 2100. Do this for each cover classification in the dataset...

m <- 85
SecF_slice <- SecFor[,,m]

SecNonF[SecNonF==fillvalue$value] <- NA
length(na.omit(as.vector(SecNonF[,,1])))
m <- 85
SecN_slice <- SecNonF[,,m]

C4Crop_slice
C4Crop[C4Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C4Crop[,,1])))
C4Crop_slice <- C4Crop[,,m]

C3Crop[C3Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C3Crop[,,1])))
C3Crop_slice <- C3Crop[,,m]

urban[urban==fillvalue$value] <- NA
length(na.omit(as.vector(urban[,,1])))
urban_slice <- urban[,,m]

C3CropPer[C3CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C3CropPer[,,1])))
C3CropPer_slice <- C3CropPer[,,m]

C4CropPer[C4CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C4CropPer[,,1])))
C4CropPer_slice <- C4CropPer[,,m]

C3Nfix[C3Nfix==fillvalue$value] <- NA
length(na.omit(as.vector(C3Nfix[,,1])))
C3Nfix_slice <- C3Nfix[,,m]

Pasture[Pasture==fillvalue$value] <- NA
length(na.omit(as.vector(Pasture[,,1])))
Pasture_slice <- Pasture[,,m]

Range[Range==fillvalue$value] <- NA
length(na.omit(as.vector(Range[,,1])))
Range_slice <- Range[,,m]

#Now put all those into vectors of land classes by lat/lon
lonlat <- as.matrix(expand.grid(lon,lat))
#pfor <- as.vector(primF_slice)
#pNon <- as.vector(primN_slice)
c4Crop <- as.vector(C4Crop_slice)
secFor <- as.vector(SecF_slice)
secNF <- as.vector(SecN_slice)
c3Crop <- as.vector(C3Crop_slice)
c4Perr <- as.vector(C4CropPer_slice)
c3Perr <- as.vector(C3CropPer_slice)
Urban <- as.vector(urban_slice)
range2 <- as.vector(Range_slice)
pasture2 <- as.vector(Pasture_slice)
c3Nfx <- as.vector(C3Nfix_slice)


#And then make them a dataframe
RCP8.5_dat <- data.frame(cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx))

#names(RCP8.5_dat) <- c("lon","lat",paste(dname, as.character(m), sep="_"),paste(dname2, as.character(m), sep="_"),paste(dname3, as.character(m), sep="_"), paste(dname4, as.character(m), sep="_"), paste(dname5, as.character(m), sep="_"),paste(dname6, as.character(m), sep="_"),paste(dname7, as.character(m), sep="_"),paste(dname8, as.character(m), sep="_"),paste(dname9, as.character(m), sep="_"),paste(dname10, as.character(m), sep="_"))
names(RCP8.5_dat) <- c("lon","lat","c4Crop", "secFor", "secNF", "c3Crop", "c4Perr","c3Perr","Urban", "range", "pasture","c3Nfx")

#Now designate rooting depths for crop areas (all 1.5m for D99)
RCP8.5_dat$CropArea <- rowSums(RCP8.5_dat[,c("c3Crop", "c4Crop", "c4Perr", "c3Perr", "c3Nfx")], na.rm=TRUE)
RCP8.5_dat$CropRoots <- RCP8.5_dat$CropArea * 1.5

#Designate rooting depth for pastures and range lands, based on Zheng 2001 there is a separate class for this
RCP8.5_dat$PastureArea <- rowSums(RCP8.5_dat[,c("pasture", "range")], na.rm=TRUE)
RCP8.5_dat$PastureRoots <- RCP8.5_dat$PastureArea * 2.1

#Designate rooting depths for all remaining classes
#RCP8.5_dat$Secondary <- rowSums(RCP8.5_dat[,c("secFor", "secNF")], na.rm = T)
#RCP8.5_dat$SecRoots <- RCP8.5_dat$Secondary * 2
RCP8.5_dat$SecForRoot <-  RCP8.5_dat$secFor * 2
RCP8.5_dat$SecNFRoot <- RCP8.5_dat$secNF *1.5
RCP8.5_dat$UrbanRoots <-RCP8.5_dat$Urban * 0
RCP8.5_dat$RootsTot <- rowSums(RCP8.5_dat[,c("CropRoots", "PastureRoots", "UrbanRoots", "SecForRoot", "SecNFRoot")], na.rm=TRUE)
 
#Make into spatial data

grid <- expand.grid(x=lon, y=lat)
#Find the mean of rooting depth in new map
mean(RCP8.5_dat$RootsTot, na.rm = T) 
RCP8.5_Roots <- select(RCP8.5_dat, lat,lon,RootsTot)

#Then convert the data to a raster for subsequent calculations

coordinates(RCP8.5_Roots) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(RCP8.5_Roots) <- TRUE
# coerce to raster
RCP8.5RootsRaster <- raster(RCP8.5_Roots)

RCP8.5_RootsRe <- resample(ContVeg7, RCP8.5RootsRaster) # make the new raster and the corresponding contemporary veg raster the same resolution.
Make a plot (Fig S4A)
par(mfrow = c(2,2), mar = c(1,1,1,1))
plot(((RCP8.5_RootsRe*100)-(RCP8.5RootsRaster*100)), col = diverge_hcl(20), zlim = c(-310, 310))

#Find the mean of the difference between contemporary and projected roots
RCP8.5Sub<- (RCP8.5_RootsRe*100)-(RCP8.5RootsRaster*100)
cellStats(RCP8.5Sub, 'mean', na.rm = T)



#############RCP2.6################
ncpath2 <- "/Users/lindaparsons/Desktop/Biomes_Roots_Data/"
ncname2 <- "RCP2.6"  
ncfname2 <- paste(ncpath2, ncname2, sep="")
RCP2.6 <- nc_open(ncfname2)


#Extract Longitude
lon <- ncvar_get(RCP2.6,"lon")
nlon <- dim(lon)

#Extract Latitude
lat <- ncvar_get(RCP2.6,"lat")
nlat <- dim(lat)


#Extract Time Data
time <- ncvar_get(RCP2.6,"time")
tunits <- ncatt_get(RCP2.6,"time","units")
nt <- dim(time)

# Select variables of interest for maps. Since I want altered land uses, I'll do urban/AG covers mostly
dname <- "secdf"
SecFor <- ncvar_get(RCP2.6,dname)
dlname <- ncatt_get(RCP2.6,dname,"long_name")
dunits <- ncatt_get(RCP2.6,dname,"units")
fillvalue <- ncatt_get(RCP2.6,dname,"_FillValue")

dname2 <- "secdn"
SecNonF <- ncvar_get(RCP2.6,dname2)
dlname <- ncatt_get(RCP2.6,dname2,"long_name")
dunits <- ncatt_get(RCP2.6,dname2,"units")
fillvalue <- ncatt_get(RCP2.6,dname2,"_FillValue")

dname3 <- "urban"
urban <- ncvar_get(RCP2.6,dname3)
dlname <- ncatt_get(RCP2.6,dname3,"long_name")
dunits <- ncatt_get(RCP2.6,dname3,"units")
fillvalue <- ncatt_get(RCP2.6,dname3,"_FillValue")

dname4 <- "c4ann"
C4Crop <- ncvar_get(RCP2.6,dname4)
dlname <- ncatt_get(RCP2.6,dname4,"long_name")
dunits <- ncatt_get(RCP2.6,dname4,"units")
fillvalue <- ncatt_get(RCP2.6,dname4,"_FillValue")

dname5 <- "c3ann"
C3Crop <- ncvar_get(RCP2.6,dname5)
dlname <- ncatt_get(RCP2.6,dname5,"long_name")
dunits <- ncatt_get(RCP2.6,dname5,"units")
fillvalue <- ncatt_get(RCP2.6,dname5,"_FillValue")

dname6 <- "c3per"
C3CropPer <- ncvar_get(RCP2.6,dname6)
dlname <- ncatt_get(RCP2.6,dname6,"long_name")
dunits <- ncatt_get(RCP2.6,dname6,"units")
fillvalue <- ncatt_get(RCP2.6,dname6,"_FillValue")

dname7 <- "c4ann"
C4CropPer <- ncvar_get(RCP2.6,dname7)
dlname <- ncatt_get(RCP2.6,dname7,"long_name")
dunits <- ncatt_get(RCP2.6,dname7, "units")
fillvalue <- ncatt_get(RCP2.6,dname7,"_FillValue")

dname8 <- "c3nfx"
C3Nfix <- ncvar_get(RCP2.6,dname8)
dlname <- ncatt_get(RCP2.6,dname8,"long_name")
dunits <- ncatt_get(RCP2.6,dname8,"units")
fillvalue <- ncatt_get(RCP2.6,dname8,"_FillValue")

dname9 <- "pastr"
Pasture <- ncvar_get(RCP2.6,dname9)
dlname <- ncatt_get(RCP2.6,dname9,"long_name")
dunits <- ncatt_get(RCP2.6,dname9,"units")
fillvalue <- ncatt_get(RCP2.6,dname9,"_FillValue")

dname10 <- "range"
Range <- ncvar_get(RCP2.6,dname10)
dlname <- ncatt_get(RCP2.6,dname10,"long_name")
dunits <- ncatt_get(RCP2.6,dname10,"units")
fillvalue <- ncatt_get(RCP2.6,dname10,"_FillValue")


# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))


SecFor[SecFor==fillvalue$value] <- NA
length(na.omit(as.vector(SecFor[,,1])))

m <- 85
SecF_slice <- SecFor[,,m]
#image(lon,lat,SecF_slice, col=rev(brewer.pal(10,"RdBu")))

SecNonF[SecNonF==fillvalue$value] <- NA
length(na.omit(as.vector(SecNonF[,,1])))
m <- 85
SecN_slice <- SecNonF[,,m]

C4Crop[C4Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C4Crop[,,1])))
C4Crop_slice <- C4Crop[,,m]

C3Crop[C3Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C3Crop[,,1])))
C3Crop_slice <- C3Crop[,,m]

urban[urban==fillvalue$value] <- NA
length(na.omit(as.vector(urban[,,1])))
urban_slice <- urban[,,m]

C3CropPer[C3CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C3CropPer[,,1])))
C3CropPer_slice <- C3CropPer[,,m]

C4CropPer[C4CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C4CropPer[,,1])))
C4CropPer_slice <- C4CropPer[,,m]

C3Nfix[C3Nfix==fillvalue$value] <- NA
length(na.omit(as.vector(C3Nfix[,,1])))
C3Nfix_slice <- C3Nfix[,,m]

Pasture[Pasture==fillvalue$value] <- NA
length(na.omit(as.vector(Pasture[,,1])))
Pasture_slice <- Pasture[,,m]

Range[Range==fillvalue$value] <- NA
length(na.omit(as.vector(Range[,,1])))
Range_slice <- Range[,,m]

lonlat <- as.matrix(expand.grid(lon,lat))
#pfor <- as.vector(primF_slice)
#pNon <- as.vector(primN_slice)
c4Crop <- as.vector(C4Crop_slice)
secFor <- as.vector(SecF_slice)
secNF <- as.vector(SecN_slice)
c3Crop <- as.vector(C3Crop_slice)
c4Perr <- as.vector(C4CropPer_slice)
c3Perr <- as.vector(C3CropPer_slice)
Urban <- as.vector(urban_slice)
range2 <- as.vector(Range_slice)
pasture2 <- as.vector(Pasture_slice)
c3Nfx <- as.vector(C3Nfix_slice)

#propSum <-cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx)

RCP2.6_dat <- data.frame(cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx))

#names(RCP8.5_dat) <- c("lon","lat",paste(dname, as.character(m), sep="_"),paste(dname2, as.character(m), sep="_"),paste(dname3, as.character(m), sep="_"), paste(dname4, as.character(m), sep="_"), paste(dname5, as.character(m), sep="_"),paste(dname6, as.character(m), sep="_"),paste(dname7, as.character(m), sep="_"),paste(dname8, as.character(m), sep="_"),paste(dname9, as.character(m), sep="_"),paste(dname10, as.character(m), sep="_"))
names(RCP2.6_dat) <- c("lon","lat","c4Crop", "secFor", "secNF", "c3Crop", "c4Perr","c3Perr","Urban", "range", "pasture","c3Nfx")

RCP2.6_dat$CropArea <- rowSums(RCP2.6_dat[,c("c3Crop", "c4Crop", "c4Perr", "c3Perr", "c3Nfx")], na.rm=TRUE)
RCP2.6_dat$CropRoots <- RCP2.6_dat$CropArea * 1.5

RCP2.6_dat$PastureArea <- rowSums(RCP2.6_dat[,c("pasture", "range")], na.rm=TRUE)
RCP2.6_dat$PastureRoots <- RCP2.6_dat$PastureArea * 2.1

#RCP8.5_dat$Secondary <- rowSums(RCP8.5_dat[,c("secFor", "secNF")], na.rm = T)
#RCP8.5_dat$SecRoots <- RCP8.5_dat$Secondary * 2
RCP2.6_dat$SecForRoot <-  RCP2.6_dat$secFor * 2
RCP2.6_dat$SecNFRoot <- RCP2.6_dat$secNF *1.5
RCP2.6_dat$UrbanRoots <-RCP2.6_dat$Urban * 0
RCP2.6_dat$RootsTot <- rowSums(RCP2.6_dat[,c("CropRoots", "PastureRoots", "UrbanRoots", "SecForRoot", "SecNFRoot")], na.rm=TRUE)

grid <- expand.grid(x=lon, y=lat)
cutpts <- c(0,0.01,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2)
colourCount = length(unique(RCP2.6_dat$RootsTot))
getPalette = colorRampPalette(brewer.pal(9, "Blues"))
levelplot(RCP2.6_dat$RootsTot ~ x+y, data = grid, at=cutpts, cuts=24, pretty=T, 
          col.regions=(getPalette(colourCount)))
mean(RCP2.6_dat$RootsTot, na.rm = T) 

RCP2.6_Roots <- select(RCP2.6_dat, lat,lon,RootsTot)

coordinates(RCP2.6_Roots) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(RCP2.6_Roots) <- TRUE
# coerce to raster
RCP2.6RootsRaster <- raster(RCP2.6_Roots)

##Fig 3A 
RCP2.6_RootsRe <- resample(ContVeg7, RCP2.6RootsRaster)
plot(((RCP2.6_RootsRe*100)-(RCP2.6RootsRaster*100)), col = rev(diverge_hcl(20)), legend = F, zlim = c (-310, 310))

plot(((RCP2.6_RootsRe)-(RCP2.6RootsRaster)), col = (diverge_hcl(50)), legend , zlim = c (-3.10, 3.10))
#RootsRaster = RCP, Future; Roots_Re=Contemporary > 0 : Future < now, Shallowing;  <0: Future > now deepening

plot(RCP2.6RootsRaster)
RCP2.6Sub<- (RCP2.6_RootsRe*100)-(RCP2.6RootsRaster*100)
cellStats(RCP2.6Sub, 'mean', na.rm = T) #48.14615



#################RCP6.0
ncpath3 <- "/Users/lindaparsons/Desktop/Biomes_Roots_Data/"
ncname3 <- "RCP6.0"  
ncfname3 <- paste(ncpath3, ncname3, sep="")
RCP6.0 <- nc_open(ncfname3)


#Extract Longitude
lon <- ncvar_get(RCP6.0,"lon")
nlon <- dim(lon)

#Extract Latitude
lat <- ncvar_get(RCP6.0,"lat")
nlat <- dim(lat)


#Extract Time Data
time <- ncvar_get(RCP6.0,"time")
tunits <- ncatt_get(RCP6.0,"time","units")
nt <- dim(time) 

# Select variables of interest for maps. Since I want altered land uses, I'll do urban/AG covers mostly
dname <- "secdf"
SecFor <- ncvar_get(RCP6.0,dname)
dlname <- ncatt_get(RCP6.0,dname,"long_name")
dunits <- ncatt_get(RCP6.0,dname,"units")
fillvalue <- ncatt_get(RCP6.0,dname,"_FillValue")

dname2 <- "secdn"
SecNonF <- ncvar_get(RCP6.0,dname2)
dlname <- ncatt_get(RCP6.0,dname2,"long_name")
dunits <- ncatt_get(RCP6.0,dname2,"units")
fillvalue <- ncatt_get(RCP6.0,dname2,"_FillValue")

dname3 <- "urban"
urban <- ncvar_get(RCP6.0,dname3)
dlname <- ncatt_get(RCP6.0,dname3,"long_name")
dunits <- ncatt_get(RCP6.0,dname3,"units")
fillvalue <- ncatt_get(RCP6.0,dname3,"_FillValue")

dname4 <- "c4ann"
C4Crop <- ncvar_get(RCP6.0,dname4)
dlname <- ncatt_get(RCP6.0,dname4,"long_name")
dunits <- ncatt_get(RCP6.0,dname4,"units")
fillvalue <- ncatt_get(RCP6.0,dname4,"_FillValue")

dname5 <- "c3ann"
C3Crop <- ncvar_get(RCP6.0,dname5)
dlname <- ncatt_get(RCP6.0,dname5,"long_name")
dunits <- ncatt_get(RCP6.0,dname5,"units")
fillvalue <- ncatt_get(RCP6.0,dname5,"_FillValue")

dname6 <- "c3per"
C3CropPer <- ncvar_get(RCP6.0,dname6)
dlname <- ncatt_get(RCP6.0,dname6,"long_name")
dunits <- ncatt_get(RCP6.0,dname6,"units")
fillvalue <- ncatt_get(RCP6.0,dname6,"_FillValue")

dname7 <- "c4ann"
C4CropPer <- ncvar_get(RCP6.0,dname7)
dlname <- ncatt_get(RCP6.0,dname7,"long_name")
dunits <- ncatt_get(RCP6.0,dname7, "units")
fillvalue <- ncatt_get(RCP6.0,dname7,"_FillValue")

dname8 <- "c3nfx"
C3Nfix <- ncvar_get(RCP6.0,dname8)
dlname <- ncatt_get(RCP6.0,dname8,"long_name")
dunits <- ncatt_get(RCP6.0,dname8,"units")
fillvalue <- ncatt_get(RCP6.0,dname8,"_FillValue")

dname9 <- "pastr"
Pasture <- ncvar_get(RCP6.0,dname9)
dlname <- ncatt_get(RCP6.0,dname9,"long_name")
dunits <- ncatt_get(RCP6.0,dname9,"units")
fillvalue <- ncatt_get(RCP6.0,dname9,"_FillValue")

dname10 <- "range"
Range <- ncvar_get(RCP6.0,dname10)
dlname <- ncatt_get(RCP6.0,dname10,"long_name")
dunits <- ncatt_get(RCP6.0,dname10,"units")
fillvalue <- ncatt_get(RCP6.0,dname10,"_FillValue")


# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))


SecFor[SecFor==fillvalue$value] <- NA
length(na.omit(as.vector(SecFor[,,1])))

m <- 85
SecF_slice <- SecFor[,,m]
#image(lon,lat,SecF_slice, col=rev(brewer.pal(10,"RdBu")))

SecNonF[SecNonF==fillvalue$value] <- NA
length(na.omit(as.vector(SecNonF[,,1])))
m <- 85
SecN_slice <- SecNonF[,,m]

C4Crop[C4Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C4Crop[,,1])))
C4Crop_slice <- C4Crop[,,m]

C3Crop[C3Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C3Crop[,,1])))
C3Crop_slice <- C3Crop[,,m]

urban[urban==fillvalue$value] <- NA
length(na.omit(as.vector(urban[,,1])))
urban_slice <- urban[,,m]

C3CropPer[C3CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C3CropPer[,,1])))
C3CropPer_slice <- C3CropPer[,,m]

C4CropPer[C4CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C4CropPer[,,1])))
C4CropPer_slice <- C4CropPer[,,m]

C3Nfix[C3Nfix==fillvalue$value] <- NA
length(na.omit(as.vector(C3Nfix[,,1])))
C3Nfix_slice <- C3Nfix[,,m]

Pasture[Pasture==fillvalue$value] <- NA
length(na.omit(as.vector(Pasture[,,1])))
Pasture_slice <- Pasture[,,m]

Range[Range==fillvalue$value] <- NA
length(na.omit(as.vector(Range[,,1])))
Range_slice <- Range[,,m]

lonlat <- as.matrix(expand.grid(lon,lat))
#pfor <- as.vector(primF_slice)
#pNon <- as.vector(primN_slice)
c4Crop <- as.vector(C4Crop_slice)
secFor <- as.vector(SecF_slice)
secNF <- as.vector(SecN_slice)
c3Crop <- as.vector(C3Crop_slice)
c4Perr <- as.vector(C4CropPer_slice)
c3Perr <- as.vector(C3CropPer_slice)
Urban <- as.vector(urban_slice)
range2 <- as.vector(Range_slice)
pasture2 <- as.vector(Pasture_slice)
c3Nfx <- as.vector(C3Nfix_slice)

#propSum <-cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx)

RCP6.0_dat <- data.frame(cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx))

#names(RCP8.5_dat) <- c("lon","lat",paste(dname, as.character(m), sep="_"),paste(dname2, as.character(m), sep="_"),paste(dname3, as.character(m), sep="_"), paste(dname4, as.character(m), sep="_"), paste(dname5, as.character(m), sep="_"),paste(dname6, as.character(m), sep="_"),paste(dname7, as.character(m), sep="_"),paste(dname8, as.character(m), sep="_"),paste(dname9, as.character(m), sep="_"),paste(dname10, as.character(m), sep="_"))
names(RCP6.0_dat) <- c("lon","lat","c4Crop", "secFor", "secNF", "c3Crop", "c4Perr","c3Perr","Urban", "range", "pasture","c3Nfx")

RCP6.0_dat$CropArea <- rowSums(RCP6.0_dat[,c("c3Crop", "c4Crop", "c4Perr", "c3Perr", "c3Nfx")], na.rm=TRUE)
RCP6.0_dat$CropRoots <- RCP6.0_dat$CropArea * 1.5

RCP6.0_dat$PastureArea <- rowSums(RCP6.0_dat[,c("pasture", "range")], na.rm=TRUE)
RCP6.0_dat$PastureRoots <- RCP6.0_dat$PastureArea * 2.1

#RCP8.5_dat$Secondary <- rowSums(RCP8.5_dat[,c("secFor", "secNF")], na.rm = T)
#RCP8.5_dat$SecRoots <- RCP8.5_dat$Secondary * 2
RCP6.0_dat$SecForRoot <-  RCP6.0_dat$secFor * 2
RCP6.0_dat$SecNFRoot <- RCP6.0_dat$secNF *1.5
RCP6.0_dat$UrbanRoots <-RCP6.0_dat$Urban * 0
RCP6.0_dat$RootsTot <- rowSums(RCP6.0_dat[,c("CropRoots", "PastureRoots", "UrbanRoots", "SecForRoot", "SecNFRoot")], na.rm=TRUE)

grid <- expand.grid(x=lon, y=lat)
cutpts <- c(0,0.01,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2)
colourCount = length(unique(RCP6.0_dat$RootsTot))
getPalette = colorRampPalette(brewer.pal(9, "Blues"))
levelplot(RCP6.0_dat$RootsTot ~ x+y, data = grid, at=cutpts, cuts=24, pretty=T, 
          col.regions=(getPalette(colourCount)))

RCP6.0_Roots <- select(RCP6.0_dat, lat,lon,RootsTot)

coordinates(RCP6.0_Roots) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(RCP6.0_Roots) <- TRUE
# coerce to raster
RCP6.0RootsRaster <- raster(RCP6.0_Roots)

RCP6.0_RootsRe <- resample(ContVeg7, RCP6.0RootsRaster)
plot(((RCP6.0_RootsRe*100)-(RCP6.0RootsRaster*100)), col = diverge_hcl(20), legend =F, zlim = c(-310,310))

plot(RCP6.0RootsRaster)
RCP6.0Sub<- (RCP6.0_RootsRe*100)-(RCP6.0RootsRaster*100)
cellStats(RCP6.0Sub, 'mean', na.rm = T) 



#################RCP4.5
ncpath4 <- "/Users/lindaparsons/Desktop/Biomes_Roots_Data/"
ncname4 <- "RCP4.5"  
ncfname4 <- paste(ncpath4, ncname4, sep="")
RCP4.5 <- nc_open(ncfname4)


lon <- ncvar_get(RCP4.5,"lon")
nlon <- dim(lon)

#Extract Latitude
lat <- ncvar_get(RCP4.5,"lat")
nlat <- dim(lat)


#Extract Time Data
time <- ncvar_get(RCP4.5,"time")
tunits <- ncatt_get(RCP4.5,"time","units")
nt <- dim(time) 

# Select variables of interest for maps. Since I want altered land uses, I'll do urban/AG covers mostly
dname <- "secdf"
SecFor <- ncvar_get(RCP4.5,dname)
dlname <- ncatt_get(RCP4.5,dname,"long_name")
dunits <- ncatt_get(RCP4.5,dname,"units")
fillvalue <- ncatt_get(RCP4.5,dname,"_FillValue")

dname2 <- "secdn"
SecNonF <- ncvar_get(RCP4.5,dname2)
dlname <- ncatt_get(RCP4.5,dname2,"long_name")
dunits <- ncatt_get(RCP4.5,dname2,"units")
fillvalue <- ncatt_get(RCP4.5,dname2,"_FillValue")

dname3 <- "urban"
urban <- ncvar_get(RCP4.5,dname3)
dlname <- ncatt_get(RCP4.5,dname3,"long_name")
dunits <- ncatt_get(RCP4.5,dname3,"units")
fillvalue <- ncatt_get(RCP4.5,dname3,"_FillValue")

dname4 <- "c4ann"
C4Crop <- ncvar_get(RCP4.5,dname4)
dlname <- ncatt_get(RCP4.5,dname4,"long_name")
dunits <- ncatt_get(RCP4.5,dname4,"units")
fillvalue <- ncatt_get(RCP4.5,dname4,"_FillValue")

dname5 <- "c3ann"
C3Crop <- ncvar_get(RCP4.5, dname5)
dlname <- ncatt_get(RCP4.5,dname5,"long_name")
dunits <- ncatt_get(RCP4.5,dname5,"units")
fillvalue <- ncatt_get(RCP4.5,dname5,"_FillValue")

dname6 <- "c3per"
C3CropPer <- ncvar_get(RCP4.5,dname6)
dlname <- ncatt_get(RCP4.5,dname6,"long_name")
dunits <- ncatt_get(RCP4.5,dname6,"units")
fillvalue <- ncatt_get(RCP4.5,dname6,"_FillValue")

dname7 <- "c4ann"
C4CropPer <- ncvar_get(RCP4.5,dname7)
dlname <- ncatt_get(RCP4.5,dname7,"long_name")
dunits <- ncatt_get(RCP4.5,dname7, "units")
fillvalue <- ncatt_get(RCP4.5,dname7,"_FillValue")

dname8 <- "c3nfx"
C3Nfix <- ncvar_get(RCP4.5,dname8)
dlname <- ncatt_get(RCP4.5,dname8,"long_name")
dunits <- ncatt_get(RCP4.5,dname8,"units")
fillvalue <- ncatt_get(RCP4.5,dname8,"_FillValue")

dname9 <- "pastr"
Pasture <- ncvar_get(RCP4.5,dname9)
dlname <- ncatt_get(RCP4.5,dname9,"long_name")
dunits <- ncatt_get(RCP4.5,dname9,"units")
fillvalue <- ncatt_get(RCP4.5,dname9,"_FillValue")

dname10 <- "range"
Range <- ncvar_get(RCP4.5,dname10)
dlname <- ncatt_get(RCP4.5,dname10,"long_name")
dunits <- ncatt_get(RCP4.5,dname10,"units")
fillvalue <- ncatt_get(RCP4.5,dname10,"_FillValue")


# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))


SecFor[SecFor==fillvalue$value] <- NA
length(na.omit(as.vector(SecFor[,,1])))

m <- 85
SecF_slice <- SecFor[,,m]
#image(lon,lat,SecF_slice, col=rev(brewer.pal(10,"RdBu")))

SecNonF[SecNonF==fillvalue$value] <- NA
length(na.omit(as.vector(SecNonF[,,1])))
m <- 85
SecN_slice <- SecNonF[,,m]

C4Crop[C4Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C4Crop[,,1])))
C4Crop_slice <- C4Crop[,,m]

C3Crop[C3Crop==fillvalue$value] <- NA
length(na.omit(as.vector(C3Crop[,,1])))
C3Crop_slice <- C3Crop[,,m]

urban[urban==fillvalue$value] <- NA
length(na.omit(as.vector(urban[,,1])))
urban_slice <- urban[,,m]

C3CropPer[C3CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C3CropPer[,,1])))
C3CropPer_slice <- C3CropPer[,,m]

C4CropPer[C4CropPer==fillvalue$value] <- NA
length(na.omit(as.vector(C4CropPer[,,1])))
C4CropPer_slice <- C4CropPer[,,m]

C3Nfix[C3Nfix==fillvalue$value] <- NA
length(na.omit(as.vector(C3Nfix[,,1])))
C3Nfix_slice <- C3Nfix[,,m]

Pasture[Pasture==fillvalue$value] <- NA
length(na.omit(as.vector(Pasture[,,1])))
Pasture_slice <- Pasture[,,m]

Range[Range==fillvalue$value] <- NA
length(na.omit(as.vector(Range[,,1])))
Range_slice <- Range[,,m]

lonlat <- as.matrix(expand.grid(lon,lat))
#pfor <- as.vector(primF_slice)
#pNon <- as.vector(primN_slice)
c4Crop <- as.vector(C4Crop_slice)
secFor <- as.vector(SecF_slice)
secNF <- as.vector(SecN_slice)
c3Crop <- as.vector(C3Crop_slice)
c4Perr <- as.vector(C4CropPer_slice)
c3Perr <- as.vector(C3CropPer_slice)
Urban <- as.vector(urban_slice)
range2 <- as.vector(Range_slice)
pasture2 <- as.vector(Pasture_slice)
c3Nfx <- as.vector(C3Nfix_slice)

#propSum <-cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx)

RCP4.5_dat <- data.frame(cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx))

#names(RCP8.5_dat) <- c("lon","lat",paste(dname, as.character(m), sep="_"),paste(dname2, as.character(m), sep="_"),paste(dname3, as.character(m), sep="_"), paste(dname4, as.character(m), sep="_"), paste(dname5, as.character(m), sep="_"),paste(dname6, as.character(m), sep="_"),paste(dname7, as.character(m), sep="_"),paste(dname8, as.character(m), sep="_"),paste(dname9, as.character(m), sep="_"),paste(dname10, as.character(m), sep="_"))
names(RCP4.5_dat) <- c("lon","lat","c4Crop", "secFor", "secNF", "c3Crop", "c4Perr","c3Perr","Urban", "range", "pasture","c3Nfx")

RCP4.5_dat$CropArea <- rowSums(RCP4.5_dat[,c("c3Crop", "c4Crop", "c4Perr", "c3Perr", "c3Nfx")], na.rm=TRUE)
RCP4.5_dat$CropRoots <- RCP4.5_dat$CropArea * 1.5

RCP4.5_dat$PastureArea <- rowSums(RCP4.5_dat[,c("pasture", "range")], na.rm=TRUE)
RCP4.5_dat$PastureRoots <- RCP4.5_dat$PastureArea * 2.1

#RCP8.5_dat$Secondary <- rowSums(RCP8.5_dat[,c("secFor", "secNF")], na.rm = T)
#RCP8.5_dat$SecRoots <- RCP8.5_dat$Secondary * 2
RCP4.5_dat$SecForRoot <-  RCP4.5_dat$secFor * 2
RCP4.5_dat$SecNFRoot <- RCP4.5_dat$secNF *1.5
RCP4.5_dat$UrbanRoots <-RCP4.5_dat$Urban * 0
RCP4.5_dat$RootsTot <- rowSums(RCP4.5_dat[,c("CropRoots", "PastureRoots", "UrbanRoots", "SecForRoot", "SecNFRoot")], na.rm=TRUE)

grid <- expand.grid(x=lon, y=lat)
cutpts <- c(0,0.01,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2)
colourCount = length(unique(RCP4.5_dat$RootsTot))
getPalette = colorRampPalette(brewer.pal(9, "Blues"))
levelplot(RCP4.5_dat$RootsTot ~ x+y, data = grid, at=cutpts, cuts=24, pretty=T, 
          col.regions=(getPalette(colourCount)))

RCP4.5_Roots <- select(RCP4.5_dat, lat,lon,RootsTot)

coordinates(RCP4.5_Roots) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(RCP4.5_Roots) <- TRUE
# coerce to raster
RCP4.5RootsRaster <- raster(RCP4.5_Roots)

RCP4.5_RootsRe <- resample(ContVeg7, RCP4.5RootsRaster)
plot(((RCP4.5_RootsRe*100)-(RCP4.5RootsRaster*100)), col = diverge_hcl(20), legend =F, zlim = c(-310, 310))

plot(RCP4.5RootsRaster)
RCP4.5Sub<- (RCP4.5_RootsRe*100)-(RCP4.5RootsRaster*100)
cellStats(RCP4.5Sub, 'mean', na.rm = T) 




#Now change prescribed rooting depths so they measure D50 for scenarios

###RCP2.6_change in D50####

#Re-run the original dataframe with no root depths assigned
RCP2.6_dat <- data.frame(cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx))

#names(RCP8.5_dat) <- c("lon","lat",paste(dname, as.character(m), sep="_"),paste(dname2, as.character(m), sep="_"),paste(dname3, as.character(m), sep="_"), paste(dname4, as.character(m), sep="_"), paste(dname5, as.character(m), sep="_"),paste(dname6, as.character(m), sep="_"),paste(dname7, as.character(m), sep="_"),paste(dname8, as.character(m), sep="_"),paste(dname9, as.character(m), sep="_"),paste(dname10, as.character(m), sep="_"))
names(RCP2.6_dat) <- c("lon","lat","c4Crop", "secFor", "secNF", "c3Crop", "c4Perr","c3Perr","Urban", "range", "pasture","c3Nfx")

#Now add new rooting depths for D50
RCP2.6_dat$CropArea <- rowSums(RCP2.6_dat[,c("c3Crop", "c4Crop", "c4Perr", "c3Perr", "c3Nfx")], na.rm=TRUE)
RCP2.6_dat$CropRoots <- RCP2.6_dat$CropArea * 0.178

RCP2.6_dat$PastureArea <- rowSums(RCP2.6_dat[,c("pasture", "range")], na.rm=TRUE)
RCP2.6_dat$PastureRoots <- RCP2.6_dat$PastureArea * 0.1688

#RCP8.5_dat$Secondary <- rowSums(RCP8.5_dat[,c("secFor", "secNF")], na.rm = T)
#RCP8.5_dat$SecRoots <- RCP8.5_dat$Secondary * 2
RCP2.6_dat$SecForRoot <-  RCP2.6_dat$secFor * 0.176
RCP2.6_dat$SecNFRoot <- RCP2.6_dat$secNF * 0.172
RCP2.6_dat$UrbanRoots <-RCP2.6_dat$Urban * 0
RCP2.6_dat$RootsTot <- rowSums(RCP2.6_dat[,c("CropRoots", "PastureRoots", "UrbanRoots", "SecForRoot", "SecNFRoot")], na.rm=TRUE)

RCP2.6_RootsD50 <- select(RCP2.6_dat, lat,lon,RootsTot)

coordinates(RCP2.6_RootsD50) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(RCP2.6_RootsD50) <- TRUE
# coerce to raster
RCP2.6RootsRasterD50 <- raster(RCP2.6_RootsD50)

RCP2.6_RootsRe50 <- resample(ContVeg9, RCP2.6RootsRasterD50)
plot(((RCP2.6_RootsRe50*100)-(RCP2.6RootsRasterD50*100)), col = diverge_hcl(20), legend =F, zlim = c(-310,310))

RCP2.6SubD50<- (RCP2.6_RootsRe50*100)-(RCP2.6RootsRasterD50*100)
cellStats(RCP2.6SubD50, 'mean', na.rm = T) 


###RCP6.0_change in D50####

RCP6.0_dat <- data.frame(cbind(lonlat,c4Crop, secFor, secNF, c3Crop, c4Perr,c3Perr,Urban, range2, pasture2,c3Nfx))

#names(RCP8.5_dat) <- c("lon","lat",paste(dname, as.character(m), sep="_"),paste(dname2, as.character(m), sep="_"),paste(dname3, as.character(m), sep="_"), paste(dname4, as.character(m), sep="_"), paste(dname5, as.character(m), sep="_"),paste(dname6, as.character(m), sep="_"),paste(dname7, as.character(m), sep="_"),paste(dname8, as.character(m), sep="_"),paste(dname9, as.character(m), sep="_"),paste(dname10, as.character(m), sep="_"))
names(RCP6.0_dat) <- c("lon","lat","c4Crop", "secFor", "secNF", "c3Crop", "c4Perr","c3Perr","Urban", "range", "pasture","c3Nfx")

RCP6.0_dat$CropArea <- rowSums(RCP6.0_dat[,c("c3Crop", "c4Crop", "c4Perr", "c3Perr", "c3Nfx")], na.rm=TRUE)
RCP6.0_dat$CropRoots <- RCP6.0_dat$CropArea * 0.178

RCP6.0_dat$PastureArea <- rowSums(RCP6.0_dat[,c("pasture", "range")], na.rm=TRUE)
RCP6.0_dat$PastureRoots <- RCP6.0_dat$PastureArea * 0.1688

#RCP8.5_dat$Secondary <- rowSums(RCP8.5_dat[,c("secFor", "secNF")], na.rm = T)
#RCP8.5_dat$SecRoots <- RCP8.5_dat$Secondary * 2
RCP6.0_dat$SecForRoot <-  RCP6.0_dat$secFor * 0.176
RCP6.0_dat$SecNFRoot <- RCP6.0_dat$secNF * 0.172
RCP6.0_dat$UrbanRoots <-RCP6.0_dat$Urban * 0
RCP6.0_dat$RootsTot <- rowSums(RCP6.0_dat[,c("CropRoots", "PastureRoots", "UrbanRoots", "SecForRoot", "SecNFRoot")], na.rm=TRUE)

RCP6.0_RootsD50 <- select(RCP6.0_dat, lat,lon,RootsTot)

coordinates(RCP6.0_RootsD50) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(RCP6.0_RootsD50) <- TRUE
# coerce to raster
RCP6.0RootsRasterD50 <- raster(RCP6.0_RootsD50)

RCP6.0_RootsRe50 <- resample(ContVeg9, RCP6.0RootsRasterD50)

par(mfrow=c(1,2))
plot(((RCP2.6_RootsRe50*100)-(RCP2.6RootsRasterD50*100)), col = diverge_hcl(20), legend =T, zlim = c(-20,25))
plot(((RCP6.0_RootsRe50*100)-(RCP6.0RootsRasterD50*100)), col = diverge_hcl(20), legend =F, zlim = c(-20,25))

RCP6.0SubD50<- (RCP6.0_RootsRe50*100)-(RCP6.0RootsRasterD50*100)
min(RCP6.0SubD50)
RCP2.6SubD50
cellStats(RCP6.0SubD50, 'mean', na.rm = T) 



##Calculate means of different scenarios used in the MS

cellStats(RCP6.0SubD50, 'mean', na.rm = T) 
cellStats(RCP6.0Sub, 'mean', na.rm = T) ###Smallest Change
cellStats(RCP2.6Sub, 'mean', na.rm = T) ###Largest Change
cellStats(RCP2.6SubD50, 'mean', na.rm = T)
cellStats(RCP8.5Sub, 'mean',na.rm=T)



#####FIGURE 3 for MS
par(mfrow=c(1,2), mar = c(.1,.1,.1,.1))
plot(((RCP2.6_RootsRe)-(RCP2.6RootsRaster)), col = (diverge_hcl(50)), legend , zlim = c (-3.10, 3.10), box = F, axes = F, legend = T)
plot(((RCP6.0_RootsRe)-(RCP6.0RootsRaster)), col = diverge_hcl(50), legend =F, zlim = c(-3.10,3.10), box = F, axes = F)

  #### FIG3 Supplemental ###
par(mfrow=c(1,2), mar = c(.1,.1,.1,.1))
par(mfrow=c(1,2), mar = c(2,2,2,2))
plot(((RCP4.5_RootsRe)-(RCP4.5RootsRaster)), col = (diverge_hcl(50)), legend =T , zlim = c (-3.10, 3.10), box = F, axes = F)
plot(((RCP8.5_RootsRe)-(RCP8.5RootsRaster)), col = diverge_hcl(50), legend =F, zlim = c(-3.10,3.10), box = F, axes =F)

par(mfrow=c(1,2), mar = c(.1,.1,.1,.1))
par(mfrow=c(1,2), mar = c(2,2,2,2))
plot(((RCP2.6_RootsRe50)-(RCP2.6RootsRasterD50)), col = diverge_hcl(20), legend = T, zlim = c(-.20,.25), box = F, axes = F)
plot(((RCP6.0_RootsRe50)-(RCP6.0RootsRasterD50)), col = diverge_hcl(20), legend =F, zlim = c(-.20,.25), box = F, axes = F)



#RootsRaster = RCP, Future; Roots_Re=Contemporary > 0 : Future < now, Shallowing;  <0: Future > now deepening


#T-Tests comparing differences in contemporary and potential veg roots
#D99
D99Diff <- D99Cont-PVegD993
t.test(D99Diff$layer)
#Check to make sure differences are distirbuted normally.
D99Diff <- na.omit(D99Diff$layer)
D99Diff <- as.vector(D99Diff$layer)
qqnorm(D99Diff) #looks good.

#D95
D95Diff <- D95Cont-PVegD953 #t = -85.342, df = 1907100, p-value < 2.2e-16
t.test(D95Diff$layer)
#Check to make sure differences are distirbuted normally.
D95Diff <- na.omit(D95Diff$layer)
D95Diff <- as.vector(D95Diff$layer)
qqnorm(D95Diff) #Also looks good.

#D50
D50Diff <- D50Cont-PVegD503
t.test(D50Diff$layer)
#Check to make sure differences are distirbuted normally.
D50Diff <- na.omit(D50Diff$layer)
D50Diff <- as.vector(D50Diff$layer)
qqnorm(D50Diff) #Not as good a the others but still okay


## T-Tests for future scenarios vs. contemporary roots

#RCP2.6
par(mfrow=c(1,1))
t.test(RCP2.6Sub$layer)
#Check whether differences are distributed normally
Diff2.6 <- na.omit(RCP2.6Sub$layer)
Diff2.6 <- as.vector(Diff2.6$layer)
qqnorm(Diff2.6) #Looks good!

#RCP2.6_D50
t.test(RCP2.6SubD50$layer)
#Check whether differences are distributed normally
Diff2.6_d5 <- na.omit(RCP2.6SubD50$layer)
Diff2.6_d5 <- as.vector(Diff2.6_d5$layer)
qqnorm(Diff2.6_d5)##This one looks ok, no wilcoxon


#RCP6.0
t.test(RCP6.0Sub$layer)
#Check whether differences are distributed normally
Diff6.0 <- na.omit(RCP6.0Sub$layer)
Diff6.0 <- as.vector(Diff6.0$layer)
qqnorm(Diff6.0) #Looks okay but not as good as others. Try Wilcoxon to check significance.
hist(Diff6.0)
Roots6.0 <- as.vector(RCP6.0_RootsRe$layer)
Roots6.0A <- as.vector(RCP6.0RootsRaster$RootsTot) 
wilcox.test(Roots6.0,Roots6.0A, paired = T)

#RCP6.0_D50
t.test(RCP6.0SubD50$layer)
Diff6.05 <- na.omit(RCP6.0SubD50$layer)
Diff6.05 <- as.vector(Diff6.05$layer)
qqnorm(Diff6.05)
hist(Diff6.05)##This looks ok to me. No wilxocon.

#RCP8.5
t.test(RCP8.5Sub$layer)
#Check whether differences are distributed normally
Diff8.5 <- na.omit(RCP8.5Sub$layer)
Diff8.5 <- as.vector(Diff8.5$layer)
qqnorm(Diff8.5)#Okay, but not as good as the others
#Try Wilcoxon to check significance.
Roots8.5 <- as.vector(RCP8.5_RootsRe$layer)
Roots8.5A <- as.vector(RCP8.5RootsRaster$RootsTot) 
wilcox.test(Roots8.5,Roots8.5A, paired = T)
#Wilcoxon signed rank test with continuity correction

#RCP4.5
t.test(RCP4.5Sub$layer)
#Check whether differences are distributed normally
Diff4.5 <- na.omit(RCP4.5Sub$layer)
Diff4.5 <- as.vector(Diff4.5$layer)
qqnorm(Diff4.5)
hist(Diff4.5)
#Hist looks ok to me. QQ a little bit of tail. Check with Wilcoxon.
Roots4.5 <- as.vector(RCP4.5_RootsRe$layer)
Roots4.5A <- as.vector(RCP4.5RootsRaster$RootsTot) 
wilcox.test(Roots4.5,Roots4.5A, paired = T)

