#Wall to Wall ALS metrics
# Provide ALS metrics across the carea covered by the UAV imagery at Hanmer first.


# set libraries ----------------------------

library(lidR)
library(sf)
library(raster)
library(tidyverse)
library(here)
library(raster)


# Read data
ctg<-readLAS('C:/paper2-Tekapo/code/data/plane-als-full-study-site/BALMORAL-merged.laz')



#print(ctg)
#plot(ctg)


# Ignore points with elevations less than 0
#opt_filter(ctg) <- "-drop_z_below 0"
las<-lasfilter(ctg, Z>0)

#opt_output_files(ctg) <- ""

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



metrics_w2w <- grid_metrics(las_denoised, .stdmetrics, res = 1)
names(metrics_w2w)

#crs(metrics_w2w)

writeRaster(metrics_w2w, here('out', 'wsw_als_metrics_hanmer.tif'), overwrite = TRUE)
