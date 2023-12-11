
rm(list=ls())
setwd("C:/Users/rwojciki/Documents/Wenije Shrub shps")
path="C:/Users/rwojciki/Documents/Wenije Shrub shps/csvs for model_3" #28 blanks deleted
file.names <- dir(path, pattern =".csv")
RootCompL <- list()#creating empty list where overlap values will go


library(doParallel)
library(foreach)

myCluster <- makeCluster(11, # number of cores to use
                         type = "PSOCK") # type of cluster

registerDoParallel(myCluster)

res<-foreach(i=1:(length(file.names)), .combine=rbind) %dopar% {
  b<-paste("C:/Users/rwojciki/Documents/Wenije Shrub shps/csvs for model_3/",i, sep='')
  samp<-read.csv(b)
  CanopyR<-samp[,11] #column with shrub radius
  Shrublox<-samp[,8:9] #column with shrub centroid location
  Gsize <- 100              # Define the plot size in meters
  Ginc <-                  # Define the pixel size in m (e.g. 0.01 = 1 cm)
  # Main value of small increments will be to have smooth circles (not blocky)
  NShrub <- length(CanopyR)                # Number of shrubs within domain
  RootM <- ##rootradius multiplier value basedon canopy size
  Xsize <- trunc(Gsize/Ginc)  # Grid size (number of pixels)
  Ysize <- trunc(Gsize/Ginc)  # Grid size (number of pixels)
  CanopyC <- (CanopyR/Ginc)
  GlobalMatrix <- matrix(0,nrow=Xsize, ncol=Ysize)      # Create global matrix for summed results
  Overlap <- GlobalMatrix								  # To hold shrub overlap locations
  ShrubMatrix <- array(0, dim = c(Xsize,Ysize, NShrub)) # Create 3 dimensional array for all the shrubs
  ShrubPixels <- ShrubMatrix							  # To hold shrub presence true/false
  #ShrubLocs <- array(0, dim = c(Nshrub,2))              # An array to hold shrub locations (in meters) 
  ShrubCell <- Shrublox   
  ShrubCell <- trunc(Shrublox/Ginc)
  for(xloc in 1:Xsize) {
    for(yloc in 1:Ysize) {
      for (i in 1:NShrub) {
        RootC<-(RootM*CanopyC[i])  #defining root radius specific to shrub size. obv will need to fix
        dist <- sqrt((xloc-ShrubCell[i,1])^2 + (yloc-ShrubCell[i,2])^2)
        Rweight <- min(1, (RootC-dist)/(RootC))   # this is a linear decline from 1 at center to zero at dist 
        ifelse(dist >= RootC,
               ShrubMatrix[xloc,yloc,i] <- 0,          # If not in range of shrub this cell gets a zero
               ShrubMatrix[xloc,yloc,i] <- Rweight)    # If within range of shrub it gets Rweight  and Y are switched depending on Northing, Easting format
      }
    }
  }
  for (i in 1:NShrub) {
    GlobalMatrix <- GlobalMatrix + ShrubMatrix[, ,i]
  } 
  ShrubPixels <- ShrubMatrix > 0
  for (i in 1:NShrub) {
    locs <-  which(ShrubPixels[,,i] == "TRUE", arr.ind=TRUE)
    Overlap[locs] <- Overlap[locs] + 1
  }
  locs <- which(Overlap[,] > 1, arr.ind=TRUE)
  RootComp <- sum(GlobalMatrix[locs])
  RootCompInd <- 100 * RootComp/length(GlobalMatrix)
  RootCompL<-append(RootCompL, RootCompInd)
}



RootC<-function(i){
  b<-paste("C:/Users/rwojciki/Documents/Wenije Shrub shps/csvs for model_3_nondel/",file.names[i], sep='')
  samp<-read.csv(b)
  CanopyR<-samp[,11]
  Shrublox<-samp[,8:9]
  Gsize <- 100              # Define the plot size in meters, 80 for NPP, 100 for satellite
  Ginc <- .5                 # Define the pixel size in m (e.g. 0.01 = 1 cm)
  # Main value of small increments will be to have smooth circles (not blocky)
  NShrub <- length(CanopyR)                # Number of shrubs within domain
  RootM <- 4
  Xsize <- trunc(Gsize/Ginc)  # Grid size (number of pixels)
  Ysize <- trunc(Gsize/Ginc)  # Grid size (number of pixels)
  CanopyC <- (CanopyR/Ginc)
  GlobalMatrix <- matrix(0,nrow=Xsize, ncol=Ysize)      # Create global matrix for summed results
  Overlap <- GlobalMatrix								  # To hold shrub overlap locations
  ShrubMatrix <- array(0, dim = c(Xsize,Ysize, NShrub)) # Create 3 dimensional array for all the shrubs
  ShrubPixels <- ShrubMatrix							  # To hold shrub presence true/false
  #ShrubLocs <- array(0, dim = c(Nshrub,2))              # An array to hold shrub locations (in meters) 
  ShrubCell <- Shrublox   
  ShrubCell <- trunc(Shrublox/Ginc)
  for(xloc in 1:Xsize) {
    for(yloc in 1:Ysize) {
      for (i in 1:NShrub) {
        RootC<-(RootM*CanopyC[i])  #defining root radius specific to shrub size. obv will need to fix
        dist <- sqrt((xloc-ShrubCell[i,1])^2 + (yloc-ShrubCell[i,2])^2)
        Rweight <- min(1, (RootC-dist)/(RootC))   # this is a linear decline from 1 at center to zero at dist 
        ifelse(dist >= RootC,
               ShrubMatrix[xloc,yloc,i] <- 0,          # If not in range of shrub this cell gets a zero
               ShrubMatrix[xloc,yloc,i] <- Rweight)    # If within range of shrub it gets Rweight  and Y are switched depending on Northing, Easting format
      }
    }
  }
  for (i in 1:NShrub) {
    GlobalMatrix <- GlobalMatrix + ShrubMatrix[, ,i]
  } 
  ShrubPixels <- ShrubMatrix > 0
  for (i in 1:NShrub) {
    locs <-  which(ShrubPixels[,,i] == "TRUE", arr.ind=TRUE)
    Overlap[locs] <- Overlap[locs] + 1
  }
  locs <- which(Overlap[,] > 1, arr.ind=TRUE)
  RootComp <- sum(GlobalMatrix[locs])
  RootCompInd <- 100 * RootComp/length(GlobalMatrix)
  #RootCompL<-append(RootCompL, RootCompInd)
  list(RootCompInd)
}

res<-foreach(i=1:(length(file.names)), .combine=rbind) %dopar% {
  RootC(i)
}

stopCluster(myCluster)
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

capture.output((res), file = "")

write.csv(res,"")
setwd("")

#works perfectly!
lapply(1:length(res), function(x) write.table(t(as.data.frame(RootCompL[x])), 
                                              'RootCompRM4.csv', append= T, sep=',', 
                                              quote = F, col.names = F))

#calculating mean area , total area and density from classified imagery csv. 
AreaL <- list()
MareaL<- list()
DensityL<-list()
for (i in file.names){
  b<-paste("",i, sep='')
  samp<-read.csv(b)
  Asum<-sum(samp$RArea)
  MA<- mean(samp$RArea)
  Density<-length(samp$RArea)
  AreaL <-append (AreaL, Asum)
  MareaL<-append(MareaL, MA)
  DensityL<-append(DensityL, Density)
}
df.RCL<-as.data.frame(unlist(RootCompL))
df.AL<-as.data.frame(unlist(AreaL))
df.MAL<-as.data.frame(unlist(MareaL))
df.DL<-as.data.frame(unlist(DensityL))

full.DF<-cbind(df.RCL, df.AL, df.MAL, df.DL)

write.csv(full.DF, "")




