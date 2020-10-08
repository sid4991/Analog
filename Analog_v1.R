library(raster)
library(FNN)
library(RColorBrewer)
library(colorRamps)
library(adehabitatLT)

setwd("~/sid/WSU/climate change anolouge/Climate Bash/FruitandVeg_32Location_Climate/Gridmet")

#### DATA PREPARATION ##
## ANALOG CODE STARTS FROM LINE 96 ##


## importing gridmet data ##
filenames <- list.files(full.names=TRUE)
read_csv_filename <- function(filename){
  ret <- read.csv(filename,skip = 16)
  ret$Source <- filename #EDIT
  ret
}

GR_import.list <- ldply(filenames, read_csv_filename)


GR_import.list1 <- separate(data = GR_import.list, col = Source , into = c("Loc", "2", "3", "4", "5","6"), sep = "_")
GR_import.list1 <- separate(data = GR_import.list1, col = Loc , into = c("1","Loc"), sep = "./")
GR_import.list1 <- separate(data = GR_import.list1, col = yyyy.mm.dd, into = c("yyyy", "mm", "dd"), sep = "\\-")
GridmetYYMMDD <- GR_import.list1[,-c(4,6,9,11:15)]
GridmetYYMMDD <- GridmetYYMMDD[!(GridmetYYMMDD$yyyy =="2020"),]
GridmetYY <- GridmetYYMMDD[,-c(2,3)]

GridmetYY$tasmin.K. <- (GridmetYY$tmmn.K. - 273.15) * 9/5 + 32
GridmetYY$tasmax.K. <- (GridmetYY$tmmx.K. - 273.15) * 9/5 + 32
GridmetYY$GDD <- (GridmetYY$tmmx.K. + GridmetYY$tmmn.K.) / 2 - BaseTemp



GridmetYY$Season= as.factor(GridmetYY$Season)
Listseason= unique(GridmetYY$Season)
for(i in 1:length(Listseason)){
  GridSeasonWiseSample=GridmetYY[GridmetYY$Season%in%Listseason[i],]
  name= Listseason[i]
  write.csv(GridSeasonWiseSample, file = paste0(name,"_sample_GM.csv"))
}

group <- Fall_sample_GM %>% group_by(Loc)
Fall_sample_GM <- group %>% summarise_all(list(mean))

group <- GridmetYY %>% group_by(Loc)
Amatrix <- group %>% summarise_all(list(mean))
Amatrix <- Amatrix[,-c(2)]
## End

## Import RCP4.5 and create Matrix B
setwd("~/sid/WSU/climate change anolouge/Climate Bash/FruitandVeg_32Location_Climate/RCP4.5")
filenames1 <- list.files(full.names=TRUE)
read_csv_filename <- function(filename){
  ret <- read.csv(filename,skip = 16)
  ret$Source <- filename #EDIT
  ret
}

RCP45_import.list <- ldply(filenames1, read_csv_filename)

RCP45_import.list1 <- separate(data = RCP45_import.list, col = Source , into = c("a", "b", "Model", "c", "Scenario","d","e","f","g","Location"), sep = "\\_")
RCP45_import.list1 <- separate(data = RCP45_import.list1, col = Location , into = c("Location","type"), sep = "\\.")
RCP45_YYMMDD <- separate(data = RCP45_import.list1, col = yyyy.mm.dd, into = c("yyyy", "mm", "dd"), sep = "\\-")
Raw_RCP45 <- RCP45_YYMMDD[,-c(2,3,4,6,9,10,12,14:17,19:21)] #includes all data ##
RCP4.5 <- Raw_RCP45[,-c(8)]

#Select the Model
RCP4.5_model <- subset(RCP4.5, RCP4.5$Model == "bcc-csm1-1-m",select=c(2:4,7))

groupB <- RCP4.5_model %>% group_by(Location)
Bmatrix <- groupB %>% summarise_all(list(mean))
Bmatrix <- rename(Bmatrix, c("Location" = "Loc", "tasmin.K." = "tmmn.K.", "tasmax.K." = "tmmx.K."))
Bmatrix <- Bmatrix[,c(1, 2, 4,3)]


## End of matrix B

## Creating C matrix

groupC <- GridmetYY %>% group_by(yyyy,Loc)
Cmatrix <- groupC %>% summarise_all(list(mean))
Cmatrix <- Cmatrix[,-c(1)]


### UPDATED DATA IMPORTED  FROM FILE 140920 ###
A <- Amatrix 
B <- Bmatrix  
C <- Cmatrix

A <- A[,-c(1)]



########################
### Calculation of sigma dissimilarity
########################

# Principal component truncation rule
trunc.SDs <- 0.1 #truncation 

#initiate the data frame to store the projected sigma dissimilarity of best analogs for each grid cell. 
NN.sigma <- rep(NA,nrow(Bmatrix))
loc <- rep(NA,nrow(Bmatrix))



for(j in 1: nrow(Bmatrix)){      
  
  loc[j] = (B[j,]$Location)

  Bj <- B[j,]  
  Cj <- C[which(C$Loc==loc),]
 
   # Step 1: express climate data as standardized anomalies of reference period (1951-1990) ICV at ICV proxy j. 
  Cj.sd <- apply(Cj[ ,-c(1)],2,sd, na.rm=T)  #standard deviation of 1951-1990 interannual variability in each climate variable, ignoring missing years
  A.prime <- sweep(A,MARGIN=2,Cj.sd,`/`) #standardize the reference ICV    
  Bj.prime <- sweep(Bj[ ,-c(1)],MARGIN=2,Cj.sd,`/`) #standardize the analog pool    
  Cj.prime <- sweep(Cj[ ,-c(1)],MARGIN=2,Cj.sd,`/`) #standardize the projected future conditions of grid cells represented by ICV proxy j

  ## Step 2: Extract the principal components (PCs) of the reference period ICV and project all data onto these PCs
  PCA <- prcomp(Cj.prime[!is.na(apply(Cj.prime,1,mean)),])   #Principal components analysis. The !is.na(apply(...)) term is there simply to select all years with complete observations in all variables. 
  PCs <- max(which(unlist(summary(PCA)[1])>trunc.SDs))    # find the number of PCs to retain using the PC truncation rule of eigenvector stdev > the truncation threshold
  X<- as.data.frame(predict(PCA,A.prime))   #project the analog pool onto the PCs
  Yj<- as.data.frame(predict(PCA,Bj.prime)) #project the projected future conditions onto the PCs
  Zj<- as.data.frame(predict(PCA,Cj.prime)) #project the reference ICV onto the PCs
  
  ## Step 3a: express PC scores as standardized anomalies of reference interannual variability 
  Zj.sd <- apply(Zj,2,sd, na.rm=T)     #standard deviation of 1951-1990 interannual variability in each principal component, ignoring missing years
  X.prime <- sweep(X,MARGIN=2,Zj.sd,`/`) #standardize the analog pool    
  Yj.prime <- sweep(Yj,MARGIN=2,Zj.sd,`/`) #standardize the projected conditions   
  
  ## Step 3b: find the sigma dissimilarity of each projected condition with its best analog (Euclidean nearest neighbour) in the observed analog pool. 
  NN.dist <- as.vector(get.knnx(data=X.prime[,1:PCs],query=Yj.prime[,1:PCs],k=1,algorithm="brute")[[2]]) # Euclidean nearest neighbour distance in the z-standardized PCs of interannual climatic variability, i.e. the Mahalanobian nearest neighbour. 
  NN.chi <- pchi(NN.dist,PCs) # percentile of the nearest neighbour distance on the chi distribution with degrees of freedom equaling the dimensionality of the distance measurement (PCs)
  NN.sigma<- qchi(NN.chi,1) # values of the chi percentiles on a standard half-normal distribution (chi distribution with one degree of freedom)
  
  print(j) 
}


write.csv(NN.sigma,"NN.sigma.Sid1.csv", row.names=FALSE)
write.csv(NN_dist_tb,"NN_dist_tb.csv", row.names=FALSE)
write.csv(NN_sigma_tb,"NN_sigma_tb.csv", row.names=FALSE)
write.csv(NNs_loc_year_tb,"NNs_loc_year_tb.csv", row.names=FALSE)

write.csv(Amatrix,"Amatrix.csv", row.names=FALSE)
write.csv(Bmatrix,"Bmatrix.csv", row.names=FALSE)
write.csv(Cmatrix,"Cmatrix.csv", row.names=FALSE)
