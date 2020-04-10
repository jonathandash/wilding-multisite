# Map locations of the UAV data collection.


# Set libraries-------------------------------------------
library(sf)
library(raster)
library(here)
library(tidyverse)

rasterOptions(tmpdir='E:\\raster_tmp') #store the raster temp files on the 1TB F drive on desktop 
rasterOptions(maxmemory = 1e+09) # Increases raster processing time



# Read data ----------------------------------------------



image.path.north<-"E:/hanmer-wildings/UAV/senterra/north/north_merged_mosaic_01.tif"
my.ms.north<-stack(image.path.north)

names(my.ms.north)<-c("RED", "GREEN", "BLUE", "ALPHA1", "NIR", "JUNK", "RE", "ALPHA2")

image.path.south<-"E:/hanmer-wildings/UAV/senterra/south/south_merged_mosaic_01.tif"
my.ms.south<-stack(image.path.south)

names(my.ms.south)<-c("RED", "GREEN", "BLUE", "ALPHA1", "NIR", "JUNK", "RE", "ALPHA2")

image.path.west<-"E:/hanmer-wildings/UAV/senterra/west/west_merged_mosaic_01.tif"
my.ms.west<-stack(image.path.west)

names(my.ms.west)<-c("RED", "GREEN", "BLUE", "ALPHA1", "NIR", "JUNK", "RE", "ALPHA2")

image.path.kawekas<-'E:/hanmer-wildings/UAV/kawekas/kawekas_merged_mosaic_01.tif'
my.ms.kawekas<-stack(image.path.kawekas)

names(my.ms.kawekas)<-c("RED", "GREEN", "BLUE", "ALPHA1", "NIR", "JUNK", "RE", "ALPHA2")


dtm.path<-"E:/hanmer-wildings/merged_dtm_clipped.tif"
dtm<-raster(dtm.path)

dtm.kawekas.path<-'E:/hanmer-wildings/kawekas_dtm/dtm_south.tif'
dtm.kawekas<-raster(dtm.kawekas.path)
#plot(dtm.kawekas)
# Get Bounding boxes of UAV rasters

p.north<-st_read(here('data', 'north', 'shape', 'north_aoi.shp'))
p.south<-st_read(here('data', 'south', 'shape', 'south_aoi.shp'))
p.west<-st_read(here('data', 'west', 'shape', 'west_aoi.shp'))

# get bounding box of raster


# e.north <- extent(my.ms.north)
# e.south<-extent(my.ms.south)
# e.west<-extent(my.ms.west)
e.kawekas<-extent(my.ms.kawekas)

# coerce to a SpatialPolygons object
# p.north <- as(e.north, 'SpatialPolygons')  
# plot(p.north)
# 
# p.south <- as(e.south, 'SpatialPolygons')  
# plot(p.south)
# 
# p.west <- as(e.west, 'SpatialPolygons')
# 
 p.kawekas<-as(e.kawekas, 'SpatialPolygons')

plot(dtm.kawekas)
plot(p.kawekas, add=TRUE)

plot(dtm)
plot(p.south, add=TRUE)
plot(p.north, add=TRUE)

#p.south<-st_as_sf(p.south)
#p.north<-st_as_sf(p.north)
#p.west<-st_as_sf(p.west)
#p.kawekas<-st_as_sf(p.kawekas)

#st_write(p.north, here('out', 'north_bounding_box.shp'))
#st_write(p.south, here('out', 'south_bounding_box.shp'))

dtm_spdf<-as(dtm, "SpatialPixelsDataFrame")
dtm_df<-as.data.frame(dtm_spdf)
colnames(dtm_df)<-c('elevation', 'x', 'y')

m1<-ggplot() +
  geom_tile(data = dtm_df, aes(x=x, y=y, fill=elevation))+
  scale_fill_viridis_c() +
  geom_sf(data = p.south, fill = 'red', alpha = 0.5) +
  geom_sf(data = p.north, fill = 'grey', alpha =0.5) +
  geom_sf(data = p.west, fill = 'cyan', alpha = 0.5)+
  theme_bw() #+
  #theme(axis.title.x=element_blank(),
  #      axis.text.x=element_blank(),
  #      axis.ticks.x=element_blank()) +
  #scale_x_continuous(limits = c(172.88, 172.93), expand = c( 0 , 0 )) +
  #scale_y_continuous(limits = c(42.48, 42.42), expand = c( 0 , 0 ))

m1

png(here('out', 'hanmer_uav_map.png'), w=17, h=16, units = 'cm', res=500)
m1
dev.off()

dtm_kaw_spdf<-as(dtm.kawekas, "SpatialPixelsDataFrame")
dtm_kaw_df<-as.data.frame(dtm_kaw_spdf)
colnames(dtm_kaw_df)<-c('elevation', 'x', 'y')

m2<-ggplot() +
  geom_tile(data = dtm_kaw_df, aes(x=x, y=y, fill=elevation))+
  scale_fill_viridis_c() +
  geom_sf(data = p.kawekas, fill = 'red', alpha = 0.5) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
