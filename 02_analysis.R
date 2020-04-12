#---------------------------------------------------------------------------------------------
# 02_analysis
# Read the outputs of the uav 01_modelling analyse and produce some graphs for the manuscript.
# Jonathan Dash
#--------------------------------------------------------------------------------------------


library(here)
library(tidyverse)


# Read data

north.base<-read_csv(here('results', 'north050420.csv'))
north.base$site<-'HFP-1'
south.base<-read_csv(here('results', 'south050420.csv'))
south.base$site<-'HFP-2'
west.base<-read_csv(here('results', 'west050420.csv'))
west.base$site<-'HFP-3'

# Model accuracy comparison

base.all<-bind_rows(north.base, south.base, west.base) %>%
  mutate(source = str_replace(source, 'spec_north', 'spectral')) %>%
  mutate(source = str_replace(source, 'als_pred_north', 'als')) %>%
  mutate(source = str_replace(source, 'als_pred_south', 'als')) %>%
  mutate(source = str_replace(source, 'als_pred_west', 'als')) %>%
  mutate(source = str_replace(source, 'spec_south', 'spectral')) %>%
  mutate(source = str_replace(source, 'spec_west', 'spectral')) %>%
  mutate(source = str_replace(source, 'spec_als_north', 'spectral + als')) %>%
  mutate(source = str_replace(source, 'spec_als_south', 'spectral + als')) %>%
  mutate(source = str_replace(source, 'spec_als_west', 'spectral + als')) %>% 
  pivot_longer(cols = AUC:TSS, names_to = 'stat')
          
str(base.all)
         
base.sum<- base.all %>% group_by(site, source, stat) %>%
  summarise(mean.value = mean(value, na.rm=TRUE))


write_csv(base.sum, here('results', 'base_summary.csv'))

base.sum %>% filter(stat %in% c('AUC', 'Kappa', 'TSS')) %>%
  ggplot(aes(y= mean.value, x=source, group=stat, colour = stat, shape = stat)) +
  geom_point() +
  geom_line()+
  facet_wrap(.~site, ncol = 1)



# Within site model accuracy

# Between site model accuracy