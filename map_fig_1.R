# Map Figure 1
# Hanmer paper
# Will need NZ outline and inlays showing UAV imagery from both sites
#
# Will probably need to do some of this in a GIS


# Set libraries

library(raster)
library(sf)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(ggspatial)
library(ggrepel)
theme_set(theme_bw())
library(spdplyr)
library(cowplot)


nz<-ne_countries(country = 'new zealand', scale = "large", returnclass = "sf")


kawekas<-st_read(here('data', 'Kawekas.kml'))
kawekas$Name <- 'KFP'
kawekas_cent<-st_centroid(kawekas)
kaw.xy<-as.data.frame(st_coordinates(kawekas_cent))
kawekas_cent <- kawekas_cent %>% bind_cols(kaw.xy)

hanmer<-st_read(here('data', 'Hanmer.kml'))
hanmer$Name <- 'HFP'
hanmer_cent<-st_centroid(hanmer)
han.xy<-as.data.frame(st_coordinates(hanmer_cent))
hanmer_cent <- hanmer_cent %>% bind_cols(han.xy)

tekapo<-st_read(here('data', 'LTekapo.kml'))
tekapo$Name <- 'LP'
tekapo_cent<-st_centroid(tekapo)
tek.xy<-as.data.frame(st_coordinates(tekapo_cent))
tekapo_cent <- tekapo_cent %>% bind_cols(tek.xy)

class(nz)

mp<-ggplot()+
  geom_sf(data = nz) +
  #coord_sf(crs = st_crs(2193)) +
  geom_sf(data=kawekas, fill = '#e8b715', colour='#e8b715')+
  geom_sf(data=hanmer, fill = '#0a2494', colour = '#0a2494')+
  geom_sf(data=tekapo, fill = '#7cab3f', colour = '#7cab3f')+
  geom_text_repel(data = kawekas_cent, aes(x = X, y = Y, label = Name),  colour = 'black', 
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 0)+
  geom_text_repel(data = hanmer_cent, aes(x = X, y = Y, label = Name), colour='black',
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 0)+
  geom_text_repel(data = tekapo_cent, aes(x = X, y = Y, label = Name), colour='black',
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 0)+
  coord_sf(xlim = c(165, 180), ylim = c(-48, -34), expand = FALSE) +
  labs(x = 'Longitude', y='Latitude') +
  #theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
mp


png(here('out', 'NZ_map.png'), h=20, w=17, units='cm', res=500)
mp
dev.off()


# Make the inset map of HFP for the second panel

p.north<-st_read(here('data', 'north', 'shape', 'north_aoi.shp'))
p.south<-st_read(here('data', 'south', 'shape', 'south_aoi.shp'))
p.west<-st_read(here('data', 'west', 'shape', 'west_aoi.shp'))

p.north$Name <- 'HFP-1'
north_cent<-st_centroid(p.north)
north.xy<-as.data.frame(st_coordinates(north_cent))
north_cent <- north_cent %>% bind_cols(north.xy)


p.south$Name <- 'HFP-2'
south_cent<-st_centroid(p.south)
south.xy<-as.data.frame(st_coordinates(south_cent))
south_cent <- south_cent %>% bind_cols(south.xy)

p.west$Name <- 'HFP-3'
west_cent<-st_centroid(p.west)
west.xy<-as.data.frame(st_coordinates(west_cent))
west_cent <- west_cent %>% bind_cols(west.xy)

dtm.path<-"E:/hanmer-wildings/merged_dtm_clipped.tif"
dtm<-raster(dtm.path)


dtm_spdf<-as(dtm, "SpatialPixelsDataFrame")
dtm_df<-as.data.frame(dtm_spdf)
colnames(dtm_df)<-c('elevation', 'x', 'y')
dtm_df<-as.data.frame(dtm_df)

m1<- dtm_df %>% filter(y < 5302000,
                       x>1591000,
                       y>5298000,
                       x<1594000)%>%
  ggplot() +
  geom_tile(aes(x=x, y=y, fill=elevation)) +
  scale_fill_gradientn(colours = terrain.colors(100))+
  geom_sf(data = p.south, fill = '#e8b715', alpha = 1) +
  geom_sf(data = p.north, fill = '#0a2494', alpha =1) +
  geom_sf(data = p.west, fill = '#7cab3f', alpha = 1)+
  theme_bw() +
  geom_text_repel(data = north_cent, aes(x = X, y = Y, label = Name),  colour = 'white', 
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 0)+
  geom_text_repel(data = south_cent, aes(x = X, y = Y, label = Name),  colour = 'black', 
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 0)+
  geom_text_repel(data = west_cent, aes(x = X, y = Y, label = Name),  colour = 'black', 
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 0) +
  labs(x = 'Longitude', y='Latitude') +
  #theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'bottom')
  #coord_sf(xlim = c(172.88, 172.93), ylim = c(-42.48, -42.42), expand = FALSE) 
#+
#theme(axis.title.x=element_blank(),
#      axis.text.x=element_blank(),
#      axis.ticks.x=element_blank()) +
#scale_x_continuous(limits = c(172.88, 172.93), expand = c( 0 , 0 )) +
#scale_y_continuous(limits = c(42.48, 42.42), expand = c( 0 , 0 ))

m1
plot_grid(mp, m1, ncol = 1)

png(here('out', 'hfp.png'), h=20, w=17, units='cm', res=500)
m1
dev.off()


