#---------------------------------------------------------------------------------
# 01_UAV_Modelling
# Select variables and fit Maxent models for the UAV data.
# Output predictive maps and stats from the bootstrapping procedure.
# Now updated for validation using different data
# April 2020
#---------------------------------------------------------------------------------

# Set libraries ------------------------------------------------------------------

library(here)
library(dismo)
require(maptools)
require(raster)
require(rJava)
require(sp)
require(dismo)
require(rgdal)
require(rgeos)
require(doParallel)
library(sf)
library(tidyverse)

rasterOptions(tmpdir='E:/tmp_raster')


# Global options -----------------------------------------------------------------

prefix<-'north'
data.version<-'050420'
seed<-12345
target<-'kawekas'


# Functions ----------------------------------------------------------------------



# Function for NDVI according to the Senterra defintion
calcNDVI<-function(NIR, RED){
  
  
  (2.700 * NIR - RED) / (2.700 * NIR + RED)
  
  
}


# Function for NDRE according to the Senterra defintion
calcNDRE<-function(NIR, RE){
  
  
  (NIR - RE) / (NIR + RE)
  
}





runMaxent <- function(raster, pred_raster, outname){ 
  # Run maxent classifier with and without resuction of variance in the presence data
  # Evaluations are carried using all, only sunny and only shadowed presence data
  # Model results are saved as RData in a "Results" folder, while raster predictions are
  # returned as function outputs
  
  names(pred_raster) = names(raster)
  # unique loop name
  
  ### fit overall model
  fit <- maxent(x=raster, p = train_pos, a = train_back, removeDuplicates=F)
  # eveluate results
  eval <- evaluate(p=test_pos, a=test_back, model = fit, x=spec)
  #eval_sunny <- evaluate(p=test_pos[-which(over(test_pos, shadows) == 0)], a=test_back, model = fit, x=raster)
  #eval_shadow <- evaluate(p=test_pos[which(over(test_pos, shadows) == 0)], a=test_back, model = fit, x=raster)
  save(fit, file = here('out', paste0(outname, "fit_", i, ".RData"))) 
  save(eval, file = here('out', paste0(outname, "eval_", i, ".RData"))) 
  #save(eval_sunny, file = paste0("results/eval/all/", outname, "_sunny_", i, ".RData"))
  #save(eval_shadow, file = paste0("results/eval/all/", outname, "_shadows_", i, ".RData"))
  #beginCluster(6)
  #pred <- clusterR(pred_raster, raster::predict, args = list(model = fit))
  #endCluster()
  
  
}

# Function to obtain model accuracies
getACC <- function(eval){
  # accuracies
  auc <- eval@auc
  kappa <- max(eval@kappa)
  TPR <- eval@TPR[which.max( eval@kappa )]
  TNR <- eval@TNR[which.max( eval@kappa )]
  FPR <- eval@FPR[which.max( eval@kappa )]
  FNR <- eval@FNR[which.max( eval@kappa )]
  mat <- eval@confusion[which.max( eval@kappa ),]
  num <- (mat[1]*mat[4])-(mat[3]*mat[2])
  den <- (mat[1]+mat[2])*(mat[3]+mat[4])
  tss <- num/den
  
  
  
  # prepare output
  out <- list(auc, kappa, TPR, TNR, FPR, FNR, tss)
  names(out) <- c("AUC", "Kappa", "TPR", "TNR", "FPR", "FNR", "TSS")
  out
  
}

#getACC(eval = eval)

# Load data -----------------------------------------------------------------------



## load require shapefiles and rasters
aoi = readOGR(here('data', prefix, 'shape',  paste0(prefix, "_aoi.shp")))
aoi_target = readOGR(here('data', target, 'shape',  paste0(target, "_aoi.shp")))

#studyarea = gBuffer(studyarea, byid=TRUE, width=0)
#treeallpoly = st_read(here('data', prefix, 'shape',  paste0(prefix, "_shapes_modified_", data.version, ".shp")))
treeallpoly = readOGR(here('data', prefix, 'shape',  paste0(prefix, "_shapes_modified_", data.version, ".shp")))
treeallpoly = gBuffer(treeallpoly, byid=TRUE, width=0)

treeallpolytarget = readOGR(here('data', target, 'shape',  paste0(target, "_shapes_", data.version, ".shp")))
treeallpolytarget = gBuffer(treeallpolytarget, byid=TRUE, width=0)

nontreepoly = gDifference(aoi, treeallpoly)
nontreepolytarget = gDifference(aoi_target, treeallpolytarget)
# load training areas (polygons) 
#aoi = st_read(here('data', prefix, 'shape',  paste0(prefix, "_aoi.shp")))



# load raster data 
spec = stack(here('data', prefix, 'raster',  paste0(prefix, "_merged_mosaic_01.tif")))
als_og  = stack(here('data', prefix, 'raster',  "wsw_als_metrics_hanmer.tif"))

alstarget  = stack(here('data', target, 'raster',  "wsw_als_metrics_hanmer.tif"))


spectarget = stack(here('data', target, 'raster',  paste0(target, "_merged_mosaic_01.tif")))


names(spec)<-c("RED", "GREEN", "BLUE", "ALPHA1", "NIR", "JUNK", "RE", "ALPHA2")
names(spectarget)<-c("RED", "GREEN", "BLUE", "ALPHA1", "NIR", "JUNK", "RE", "ALPHA2")

#Define Band number for senterra mosaic
BLUE<-3
GREEN<-2
RED<-1
RED.EDGE<-7
NIR<-5


# Calculate NDVI and NDRE
spec$ndvi<-calcNDVI(NIR = spec[[NIR]], RED = spec[[RED]])
spec$ndvi[spec$ndvi < 0] <- 0

spectarget$ndvi<-calcNDVI(NIR = spectarget[[NIR]], RED = spectarget[[RED]])
spectarget$ndvi[spectarget$ndvi < 0] <- 0

spec$ndre<-calcNDRE(NIR = spec[[NIR]], RE = spec[[RED.EDGE]])
spectarget$ndre<-calcNDRE(NIR = spectarget[[NIR]], RE = spectarget[[RED.EDGE]])


spec<-stack(spec$RED, spec$GREEN, spec$BLUE, spec$NIR, spec$RE, spec$ndre, spec$ndvi)
spectarget<-stack(spectarget$RED, spectarget$GREEN, spectarget$BLUE, spectarget$NIR, spectarget$RE, spectarget$ndre, spectarget$ndvi)


#plot(als_og$zmax)
#plot(aoi, add=TRUE)
#plot(treeallpoly, col = 'red', add=TRUE)
#plot(nontreepoly, col = 'blue', add=TRUE)

# Prepare the als raster
names(als_og)<-c('zmax', 'zmean',  'zsd',  'zskew',  'zkurt', 'zentropy', 'pzabovezmean', 'pzabove2', 'zq5',  'zq10',        
              'zq15', 'zq20', 'zq25', 'zq30', 'zq35', 'zq40', 'zq45', 'zq50', 'zq55', 'zq60', 
              'zq65', 'zq70', 'zq75', 'zq80', 'zq85', 'zq90', 'zq95', 'zpcum1', 'zpcum2', 'zpcum3', 
              'zpcum4', 'zpcum5', 'zpcum6', 'zpcum7', 'zpcum8', 'zpcum9', 'itot', 'imax', 'imean',
              'isd',  'iskew',  'ikurt',  'ipground', 'ipcumzq10', 'ipcumzq30',  'ipcumzq50',  'ipcumzq70',
              'ipcumzq90',  'p1th', 'p2th', 'p3th', 'p4th', 'p5th', 'pground',  'n', 'area') 

names(alstarget)<-c('zmax', 'zmean',  'zsd',  'zskew',  'zkurt', 'zentropy', 'pzabovezmean', 'pzabove2', 'zq5',  'zq10',        
                 'zq15', 'zq20', 'zq25', 'zq30', 'zq35', 'zq40', 'zq45', 'zq50', 'zq55', 'zq60', 
                 'zq65', 'zq70', 'zq75', 'zq80', 'zq85', 'zq90', 'zq95', 'zpcum1', 'zpcum2', 'zpcum3', 
                 'zpcum4', 'zpcum5', 'zpcum6', 'zpcum7', 'zpcum8', 'zpcum9', 'itot', 'imax', 'imean',
                 'isd',  'iskew',  'ikurt',  'ipground', 'ipcumzq10', 'ipcumzq30',  'ipcumzq50',  'ipcumzq70',
                 'ipcumzq90',  'p1th', 'p2th', 'p3th', 'p4th', 'p5th', 'pground',  'n', 'area')

# Crop the als data to the aoi
als<-crop(als_og, aoi, snap='near')

alstarget<-crop(alstarget, aoi_target, snap='near')

# Only choose the variables of interest for pre-selection - REVIEW
als_pred<-stack(als$zmax, als$zmean, als$zsd, als$pzabovezmean,
                als$zpcum1, als$zpcum2, als$zpcum3, 
                als$zpcum4, als$zpcum5, als$zpcum6,
                als$zq5, als$zq10, als$zq20, als$zq30, als$zq50, als$zq75, als$zq95,
                als$isd, als$imean,
                als$ikurt, als$imax, als$pground,
                als$p1th, als$p2th, als$p3th, als$p4th, als$p5th)

als_pred_target<-stack(alstarget$zmax, alstarget$zmean, alstarget$zsd, alstarget$pzabovezmean,
                alstarget$zpcum1, alstarget$zpcum2, alstarget$zpcum3,
                alstarget$zpcum4, alstarget$zpcum5, alstarget$zpcum6,
                alstarget$zq5, alstarget$zq10, alstarget$zq20, alstarget$zq30, alstarget$zq50, alstarget$zq75, alstarget$zq95,
                alstarget$isd, alstarget$imean,
                alstarget$ikurt, alstarget$imax, alstarget$pground,
               alstarget$p1th, alstarget$p2th, alstarget$p3th, alstarget$p4th, alstarget$p5th)

# resample the als raster to match the UAV
als_pred<-resample(als_pred, spec, "bilinear")

als_pred_target<-resample(als_pred_target, spectarget, "bilinear")




# Make the combined spectral and ALS raster dataset
spec_als<-stack(spec, als_pred)

spec_als_target<-stack(spectarget, als_pred_target)

# predict raster
#texture_pred = stack(paste0("raw/", prefix, "/texture_clip.tif"))
#hyper_pred = stack(paste0("raw/", prefix, "/hyperspectral_clip.tif"))
#rgb_pred = stack(paste0("raw/", prefix, "/rgb_clip.tif"))[[1:3]]
#structure_pred = stack(paste0("raw/", prefix, "/structure_clip.tif"))
#texture_pred <- resample(texture_pred, rgb_pred)








# Fit initial models and review variable importance ----------------------------------------------------------


# Create positive and background random points
# set.seed(seed)
# background <-st_sample(aoi, size = 2000, type = 'random')
# st_crs(background)<-2193
# background<-st_sf(background)
# background<-as(background, 'Spatial')

background = spsample(aoi, 2000, type="random")
positive = spsample(treeallpoly, 2000, type="random")
negative = spsample(nontreepoly, 2000, type = "random")

background_target = spsample(aoi_target, 2000, type="random")
positive_target = spsample(treeallpolytarget, 2000, type="random")
negative_target = spsample(nontreepolytarget, 2000, type = "random")

#plot(spectarget$ndvi)
#plot(aoi_target, add=TRUE)
#plot(negative_target, add=TRUE)

# set.seed(seed)
# positive<-st_sample(treeallpoly, size = 2000, type = 'random')
# st_crs(positive)<-2193
# positive<-st_sf(positive)
# positive<-as(positive, 'Spatial')


# First spectral model
# rgbn_model <- maxent(x=spec, p = positive, a = background, removeDuplicates=F)
# rgbn_model@results[ grep("permutation", row.names(as.data.frame(rgbn_model@results))), ]
# eval <- evaluate(p=positive, a=negative, model = rgbn_model, x=spec)


# First als model
#als_model <- maxent(x=als_pred, p = positive, a = background, removeDuplicates=F)
#als_model@results[ grep("permutation", row.names(as.data.frame(als_model@results))), ]


# First combined model
# als_spec_model <- maxent(x=spec_als, p = positive, a = background, removeDuplicates=F)
#als_spec_model@results[ grep("permutation", row.names(as.data.frame(als_spec_model@results))), ]









# Cross Validation -----------------------------------------------------------


# run all combinations
raster_list <- list(spec, als_pred, spec_als)
names(raster_list) <- c("spec", "als_pred", "spec_als")

# Predict over the same rasters as used to fit the model for now.
pred_list <- list(spectarget, als_pred_target, spec_als_target)
names(pred_list) <- c("spectarget", "als_pred_target", "spec_als_target")

#registerDoParallel(6)

# prepare storing lists for predicted rasters
#pred <- list()
#pred2 <- list()


# Convert sp to sf for ease of folding. May not need to do this eventually
pos.sf<-st_as_sf(positive)
back.sf<-st_as_sf(background)
neg.sf<-st_as_sf(negative)
pos.targ.sf<-st_as_sf(positive_target)
neg.targ.sf<-st_as_sf(negative_target)

#plot(spectarget$ndvi)
#plot(neg.targ.sf, add=TRUE)


out<-data.frame()

# k-fold cross validation
for (i in 1:10){
  #i=1
  print(i)
  
  #Randomly shuffle the positive data 
  yourData<-pos.sf[sample(nrow(pos.sf)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_pos <- yourData[testIndexes, ]
    train_pos <- yourData[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  }
  
  #Randomly shuffle the background data 
  yourData<-back.sf[sample(nrow(back.sf)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_back <- yourData[testIndexes, ]
    train_back <- yourData[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  }
  
  #Randomly shuffle the background data 
  yourData<-neg.sf[sample(nrow(neg.sf)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_neg <- yourData[testIndexes, ]
    train_neg <- yourData[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  }
  
  
  #Randomly shuffle the background data 
  yourData<-neg.targ.sf[sample(nrow(neg.targ.sf)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_targ_neg <- yourData[testIndexes, ]
    train_targ_neg <- yourData[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  }
  
  #Randomly shuffle the background data 
  yourData<-pos.targ.sf[sample(nrow(pos.targ.sf)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_targ_pos <- yourData[testIndexes, ]
    train_targ_pos <- yourData[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  }
  
  
  
  
  
  
  test_back<-as(test_back, 'Spatial')
  train_back<-as(train_back, 'Spatial')
  test_pos<-as(test_pos, 'Spatial')
  train_pos<-as(train_pos, 'Spatial')
  test_neg<-as(test_neg, 'Spatial')
  train_neg<-as(train_neg, 'Spatial')
  test_targ_neg<-as(test_targ_neg, 'Spatial')
  train_targ_neg<-as(train_targ_neg, 'Spatial')
  test_targ_pos<-as(test_targ_pos, 'Spatial')
  train_targ_pos<-as(train_targ_pos, 'Spatial')
  
  
  # loop throught datasets sharing the same training/validation samples
  for (j in 1:length(raster_list)){ 
    #j=1
    # unique loop name
    outname = paste0(names(raster_list[j]), "_", prefix)
    fit <- maxent(x=raster_list[[j]], p = train_pos, a = train_back, removeDuplicates=F)
    #fit <- maxent(x=spec, p = train_pos, a = train_back, removeDuplicates=F)
    eval <- evaluate(p=test_targ_pos, a=test_targ_neg, model = fit, x=pred_list[[j]])
    
    #plot(pred_list[[j]]$ndvi)
    #plot(test_targ_neg, add = TRUE)
    
    ac<-as.data.frame(getACC(eval))
    ac$source<-outname
    ac$target<-target
    
    out<-rbind(out, ac)
    
    #save(pred_out, file = here('results', paste0(outname, ".RData")))
    print(paste0("Done ", outname, "!!!"))
  }
}


write_csv(out, here('results', paste0(prefix, data.version, 'targ_', target, '.csv')))
