# Map Figure 1
# Hanmer paper
# Will need NZ outline and inlays showing UAV imagery from both sites
#
# Will probably need to do some of this in Arc


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


nz<-ne_countries(country = 'new zealand', scale = "large", returnclass = "sf")


kawekas<-st_read(here('data', 'Kawekas.kml'))
kawekas$Name <- 'Kawekas Forest Park'
kawekas_cent<-st_centroid(kawekas)
kaw.xy<-as.data.frame(st_coordinates(kawekas_cent))
kawekas_cent <- kawekas_cent %>% bind_cols(kaw.xy)

hanmer<-st_read(here('data', 'Hanmer.kml'))
hanmer$Name <- 'Hanmer Forest Park'
hanmer_cent<-st_centroid(hanmer)
han.xy<-as.data.frame(st_coordinates(hanmer_cent))
hanmer_cent <- hanmer_cent %>% bind_cols(han.xy)

class(nz)

mp<-ggplot()+
  geom_sf(data = nz) +
  #coord_sf(crs = st_crs(2193)) +
  geom_sf(data=kawekas, fill = '#e8b715', colour='#e8b715')+
  geom_sf(data=hanmer, fill = '#0a2494', colour = '#0a2494')+
  geom_text_repel(data = kawekas_cent, aes(x = X, y = Y, label = Name),  colour = 'black', 
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 10)+
  geom_text_repel(data = hanmer_cent, aes(x = X, y = Y, label = Name), colour='black',
                  arrow = arrow(length = unit(0.01, "npc")),
                  box.padding = 15)+
  coord_sf(xlim = c(165, 180), ylim = c(-48, -34), expand = FALSE) +
  labs(x = 'Longitude', y='Latitude') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



png(here('out', 'NZ_map.png'), h=23, w=15, units='cm', res=500)
mp
dev.off()
