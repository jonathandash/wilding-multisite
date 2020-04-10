# Tree finding and delineation


library(lidR)
library(tidyverse)
library(raster)
library(rgdal)
library(tictoc)
library(here)
library(rlas)

#path<-'E:/hanmer-wildings/LAZ_subset/BT250105.laz'

path<-'E:/hanmer-wildings/LAZ_subset/BT250306.laz'
#las<-readLAS(path)

#lascheck(las)
#summary(las)


#plot(las, color = "Classification")

#las_class <- lasfilter(las, Classification == 1)
#plot(las_class)

las <- readLAS(path, filter="-keep_class 1 2 5") # Keep high vegetation and ground point classes`

#dtm <- grid_terrain(las, algorithm = knnidw(k = 8, p = 2))
dtm <- grid_terrain(las, algorithm = tin())
las_normalized <- lasnormalize(las, dtm)

# Create a filter to remove points above 95th percentile of height
lasfilternoise = function(las, sensitivity)
{
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- lasmergespatial(las, p95, "p95")
  las <- lasfilter(las, Z < p95*sensitivity)
  las$p95 <- NULL
  return(las)
}

las_denoised <- lasfilternoise(las_normalized, sensitivity = 1.2)

#plot(las_denoised)

chm <- grid_canopy(las_denoised, 0.3, pitfree(c(0,0.5,1,2,5,10,15), c(3,1.5), subcircle = 0.2))

#plot_dtm3d(chm)

#Smooth the CHM
ker <- matrix(1,5,5)
chm_s <- focal(chm, w = ker, fun = median)

#Tree detection
algo <- watershed(chm_s, th = 1)
algo2<- li2012(dt1 = 1.4, R =0, hmin = 1)
las_watershed  <- lastrees(las_denoised, algo)

# remove points that are not assigned to a tree
trees <- lasfilter(las_watershed, !is.na(treeID))

# View the results
#plot(trees, color = "treeID", colorPalette = pastel.colors(100))


hulls  <- tree_hulls(trees, type = "concave", concavity = 2, func = .stdmetrics)
#plot(hulls)


writeOGR(obj=hulls, dsn=here('out'), layer="BT250306_hulls", driver="ESRI Shapefile") # this is in geographical projection


