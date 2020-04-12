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
ctg<-readLAS('C:/paper2-Tekapo/code/data/plane-als-study-site/balmoral_merged.laz')



#print(ctg)
#plot(ctg)


# Ignore points with elevations less than 0
#opt_filter(ctg) <- "-drop_z_below 0"
las<-lasfilter(ctg, Z>0)

#opt_output_files(ctg) <- ""

metrics_w2w <- grid_metrics(ctg, .stdmetrics, res = 1)
names(metrics_w2w)

writeRaster(metrics_w2w, here('out', 'wsw_als_metrics_hanmer.tif'), overwrite = TRUE)
