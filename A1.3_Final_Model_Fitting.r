#######################################################################################
##############################   FINAL MODEL FITTING ##################################
#######################################################################################
### This is the main script used to fit a Maxent distribution model and transfer it to 
### paleoclimate scenarios. AUC test is used to evaluate the models. For this
### the species data are partitioned into training and test data
#######################################################################################
###################################  Index  ###########################################
### 1 - Load packages
### 2 - Load data (predictors layers, presences & background)
### 3 - Model fitting & tranfers
### 4 - Combining models predictions
### 5 - Export Final Maps
#######################################################################################

#######################################################################################
################################## 1 - Load packages ##################################
#######################################################################################

rm(list = ls(all = TRUE))
# Set a directory
setwd ("C:/Herpetofauna/Frogs")###CHANGE###
getwd()

#Load Packages
library(raster) # stack(), scale(), crop(), writeRaster() & raster() functions
library(sp) # required for raster packages, coordinates() function
library(dismo) # for evaluate() & maxent() functions
library(rJava) # required for dismo
library(maptools) # readShapeSpatial() function
library(rgeos) # required for maptools
data(wrld_simpl) # countries boundaries

#######################################################################################
############## 2 - Load data(predictors layers, presences & background) ###############
#######################################################################################
##### Data that was prepared in "A1_Data_Preparation" must be contained in their ###### 
##################### directories, so first define the directories ####################

# Define the extent of a rectangular study area: lon 45W 34W; lat 2S 17S
ext = extent(-42, -34, -10, -2)

############################## Predictors Layers ######################################

### load predictors layers used in model trainning

# finds all the files with extension "asc" in the directory 
files <- list.files(path=paste('C:/Herpetofauna/Frogs/Layers/Set1', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
predictors <- stack(files) # create a raster stack
projection(predictors) <- CRS('+proj=longlat +datum=WGS84') # Project stack
names(predictors)
plot(predictors)

### load predictors layers used in model tranfers 

### Last Glacial Maximum Scenario
# finds all the files with extension "asc" in the directory 
files1 <- list.files(path=paste('C:/Herpetofauna/Frogs/T_layers/LGM', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
transfer1 <- stack(files1) # create a raster stack
projection(transfer1) <- CRS('+proj=longlat +datum=WGS84') # Project stack

### Last Interglacial Scenario
# finds all the files with extension "asc" in the directory
files2 <- list.files(path=paste('C:/Herpetofauna/Frogs/T_layers/LIG', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
transfer2 <- stack(files2) # create a raster stack
projection(transfer2) <- CRS('+proj=longlat +datum=WGS84') # Project stack

### Mid-Holocene Scenario
# finds all the files with extension "asc" in the directory
files3 <- list.files(path=paste('C:/Herpetofauna/Frogs/T_layers/MidH', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
transfer3 <- stack(files3) # create a raster stack
projection(transfer3) <- CRS('+proj=longlat +datum=WGS84') # Project stack

### SSP 245 for 2050 scenario
# finds all the files with extension "asc" in the directory 
files4 <- list.files(path=paste('C:/Herpetofauna/Frogs/T_layers/2050_SSP245', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
transfer4 <- stack(files4) # create a raster stack
projection(transfer4) <- CRS('+proj=longlat +datum=WGS84') # Project stack

### SSP 585 for 2050 scenario
# finds all the files with extension "asc" in the directory
files5 <- list.files(path=paste('C:/Herpetofauna/Frogs/T_layers/2050_SSP585', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
transfer5 <- stack(files5) # create a raster stack
projection(transfer5) <- CRS('+proj=longlat +datum=WGS84') # Project stack

### SSP 245 for 2070 scenario
# finds all the files with extension "asc" in the directory
files6 <- list.files(path=paste('C:/Herpetofauna/Frogs/T_layers/2070_SSP245', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
transfer6 <- stack(files6) # create a raster stack
projection(transfer6) <- CRS('+proj=longlat +datum=WGS84') # Project stack

### SSP 585 for 2070 scenario
# finds all the files with extension "asc" in the directory
files7 <- list.files(path=paste('C:/Herpetofauna/Frogs/T_layers/2070_SSP585', sep=''), ###CHANGE###
pattern='asc', full.names=TRUE)
transfer7 <- stack(files7) # create a raster stack
projection(transfer7) <- CRS('+proj=longlat +datum=WGS84') # Project stack

#################################### Presence Data ####################################

# this is the file wich presence records  we will use:
###CHANGE according to the species###
file <- paste("C:/Herpetofauna/Frogs/frogs_joint.csv", sep="")#####CHANGE######
pres <- read.table(file, header=TRUE, sep=',')
# we do not need the first column
pres <- pres[,-1]
# extract values of the predictors at the presence points
presValues <- extract(predictors, pres)
# plot first layer of the RasterStack
plot(predictors, 1)
points(pres, col='red', pch='+')

################################# Background Data #####################################

set.seed(0) # setting random seed to always create the same random set
backgr <- randomPoints(predictors, 10000) # select 10,000 random points
# extract values from predictors to background points
backValues <- extract(predictors, backgr)

#######################################################################################
############################ 3 - Model fitting & tranfers #############################
#######################################################################################

repetition <- c(1:10) # the number of repetitions is defined
# now we can fitt and test our model twenty times

# create an empty vector to store everything

e <- list() # models evaluate are stored in a list called 'e'

maxResults <- list() # Maxent results are stored in a list called 'maxResults'

# models tranfers are stored in a list called 'pmx'
pmx <- list() # current scenario
pmx1 <- list() # Last Glacial Maximum Scenario
pmx2 <- list() # Last Interglacial Scenario
pmx3 <- list() # Mid-Holocene Scenario
pmx4 <- list() # SSP 245 for 2050 scenario
pmx5 <- list() # SSP 585 for 2050 scenario
pmx6 <- list() # SSP 245 for 2070 scenario
pmx7 <- list() # SSP 585 for 2070 scenario


for(w in repetition) {
# a set of 75% of randon select presence records are used to fit the model
samp <- sample(nrow(presValues), round(0.75 * nrow(presValues)))
trainPres <- presValues[samp,]
# the others 25% is only used to evaluate the model
testPres <- presValues[-samp,]

trainData <- data.frame(rbind(trainPres, backValues)) # create a dataframe for train
# differentiate presence and background values
pb <- c(rep(1, nrow(trainPres)), rep(0, nrow(backValues)))

# differentiate presence and abscense (background) values
pa <- c(rep(1, nrow(testPres)), rep(0, nrow(backValues)))
# create a dataframe for test
testData <- data.frame(cbind(pa, rbind(testPres, backValues)))

### Fitting a model
### function maxent() is used(Hijmans et al., 2023)
# In args:
# *feature class: keep only features indicated in selected model name 
# (Calibration_results folder)
#  feature class: "h" "hinge", "l" "linear", "q" "quadratic", "p" "product", "t" 
#  "threshold"  /  keep "noautofeature"
# *betamultiplier: value that multiply all automatic regularization parameters
#  this value must be the one indicated by the selected model
# (Calibration_results folder)
# in this example we just use "quadratic" and "threshold" features and betamultiplier = 2
mx <- maxent(trainData, 
             pb,
			 path = "C:/Herpetofauna/Frogs/Final_Models/output",###########CHANGE############
			 args = c("redoifexists", "notooltips", "noautofeature", "linear", "quadratic",
			 "hinge", "product", "nothreshold", "betamultiplier=3", "outputformat=cloglog"))###CHANGE###
# models evaluate
e[[w]] <- evaluate(testData[testData==1,], testData[testData==0,], mx)
# Maxent results are exported as .asc file
###Change according to the specie###
maxResults[[w]] <- read.csv("C:/Herpetofauna/Frogs/Final_Models/output/maxentResults.csv")####CHANGE####
# models tranfers
pmx[[w]] <- predict(predictors, mx, ext=ext, progress='') # current scenario
pmx1[[w]] <- predict(transfer1, mx, ext=ext, progress='') # Last Glacial Maximum Scenario
pmx2[[w]] <- predict(transfer2, mx, ext=ext, progress='') # Last Interglacial Scenario
pmx3[[w]] <- predict(transfer3, mx, ext=ext, progress='') # Mid-Holocene Scenario
pmx4[[w]] <- predict(transfer4, mx, ext=ext, progress='') # SSP 245 for 2050 scenario
pmx5[[w]] <- predict(transfer5, mx, ext=ext, progress='') # SSP 585 for 2050 scenario
pmx6[[w]] <- predict(transfer6, mx, ext=ext, progress='') # SSP 245 for 2070 scenario 
pmx7[[w]] <- predict(transfer7, mx, ext=ext, progress='') # SSP 585 for 2070 scenario
 }

# extract AUC values
auc <- sapply( e, function(x){slot(x, 'auc')} )

# extract "Maximum of the sum of the sensitivity and specificity" threshold
mst <- sapply( e, function(x){ x@t[which.max(x@TPR + x@TNR)] } )
threshold <- mean(mst)

# create a dataframe with Maxent results
maxentResults <- data.frame(rbind(maxResults[[1]], maxResults[[2]], maxResults[[3]], 
maxResults[[4]], maxResults[[5]], maxResults[[6]], maxResults[[7]], maxResults[[8]], 
maxResults[[9]], maxResults[[10]]))

# AUC values are exported as .csv file
write.csv(x = auc, 
           file = "C:/Herpetofauna/Frogs/Final_Models/trainAucValues.csv") ######CHANGE######

# Maxent results are exported as .csv file
write.csv(x = maxentResults, 
           file = "C:/Herpetofauna/Frogs/Final_Models/maxentResults.csv") ######CHANGE######
		   
# MST threshold are exported as .csv file
write.csv(x = threshold, 
           file = "C:/Herpetofauna/Frogs/Final_Models/mstthreshold.csv") ######CHANGE######

#######################################################################################
########################## 4 - Combining models predictions ###########################
#######################################################################################

# create a raster stack (predictions for current scenario)
models <- stack(pmx[[1]], pmx[[2]],pmx[[3]], pmx[[4]], pmx[[5]], pmx[[6]], 
pmx[[7]],pmx[[8]], pmx[[9]], pmx[[10]])

# create a raster stack (predictions for Last Glacial Maximum Scenario)
models1 <- stack(pmx1[[1]], pmx1[[2]],pmx1[[3]], pmx1[[4]], pmx1[[5]], pmx1[[6]], 
pmx1[[7]],pmx1[[8]], pmx1[[9]], pmx1[[10]])

# create a raster stack (predictions for Last Interglacial Scenario) 
models2 <- stack(pmx2[[1]], pmx2[[2]],pmx2[[3]], pmx2[[4]], pmx2[[5]], pmx2[[6]], 
pmx2[[7]],pmx2[[8]], pmx2[[9]], pmx2[[10]])

# create a raster stack (predictions for Mid-Holocene Scenario)
models3 <- stack(pmx3[[1]], pmx3[[2]],pmx3[[3]], pmx3[[4]], pmx3[[5]], pmx3[[6]], 
pmx3[[7]],pmx3[[8]], pmx3[[9]], pmx3[[10]])

# create a raster stack (predictions for SSP 245 for 2050 scenario)
models4 <- stack(pmx4[[1]], pmx4[[2]],pmx4[[3]], pmx4[[4]], pmx4[[5]], pmx4[[6]], 
pmx4[[7]],pmx4[[8]], pmx4[[9]], pmx4[[10]])

# create a raster stack (predictions for SSP 585 for 2050 scenario) 
models5 <- stack(pmx5[[1]], pmx5[[2]],pmx5[[3]], pmx5[[4]], pmx5[[5]], pmx5[[6]], 
pmx5[[7]],pmx5[[8]], pmx5[[9]], pmx5[[10]])

# create a raster stack (predictions for SSP 245 for 2070 scenario)
models6 <- stack(pmx6[[1]], pmx6[[2]],pmx6[[3]], pmx6[[4]], pmx6[[5]], pmx6[[6]], 
pmx6[[7]],pmx6[[8]], pmx6[[9]], pmx6[[10]])

# create a raster stack (predictions for SSP 585 for 2070 scenario)
models7 <- stack(pmx7[[1]], pmx7[[2]],pmx7[[3]], pmx7[[4]], pmx7[[5]], pmx7[[6]], 
pmx7[[7]],pmx7[[8]], pmx7[[9]], pmx7[[10]])


# compute the simple average for predictions:
m <- mean(models) # current scenario
m1 <- mean(models1) # Last Glacial Maximum Scenario
m2 <- mean(models2) # Last Interglacial Scenario
m3 <- mean(models3) # Mid-Holocene Scenario
m4 <- mean(models4) # SSP 245 for 2050 scenario
m5 <- mean(models5) # SSP 585 for 2050 scenario
m6 <- mean(models6) # SSP 245 for 2070 scenario
m7 <- mean(models7) # SSP 585 for 2070 scenario

# plot models results 
par(mfrow=c(2,4))
plot(m, main='Current')
plot(m1, main='LGM')
plot(m2, main='LIG')
plot(m3, main='Mid-Holocene')
plot(m4, main='2050 SSP245')
plot(m5, main='2050 SSP585')
plot(m6, main='2070 SSP245')
plot(m7, main='2070 SSP585')

#######################################################################################
############################### 5 - Export Final Maps #################################
#######################################################################################

# #### CHANGE THE OUTPUT "Frogs" DIRECTORY ACCORDING TO THE SPECIES ####

# continuous model (current scenario)
writeRaster(m,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/current_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)			

# continuous model(Last Glacial Maximum Scenario)
writeRaster(m1,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/LGM_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)	

# continuous model (Last Interglacial Scenario)
writeRaster(m2,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/LIG_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)
			
# continuous model (Mid-Holocene Scenario) 	
writeRaster(m3,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/MidH_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)			

# continuous model(SSP 245 for 2050 scenario)
writeRaster(m4,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/2050_ssp245_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)	

# continuous model (SSP 585 for 2050 scenario)
writeRaster(m5,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/2050_ssp585_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)
			
# continuous model (SSP 245 for 2070 scenario)	
writeRaster(m6,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/2070_ssp245_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)	

# continuous model (SSP 585 for 2070 scenario)	
writeRaster(m7,
            filename  = "C:/Herpetofauna/Frogs/Final_Models/2070_ssp585_cont.tif", #######CHANGE######
            format    = 'GTiff',
            NAflag    = -9999,
            overwrite = TRUE)					

#######################################################################################
################################  END OF CODE  ########################################
#######################################################################################
#References

#Hijmans, R. J.; Phillips, S.; Leathwick, J. & Elith, J. (2023). dismo: species 
#distribution modeling. – R package ver. 1.3-5 Available at: 
#<https://CRAN.R-project.org/package=dismo>. Access in: jul 2023.

#Hijmans, R. J. and Elith, J. Species distribution modeling with R. (2017). Avaliable 
#at: <https://cran.r-hub.io/web/packages/dismo/vignettes/sdm.pdf> Access in: jul 2023