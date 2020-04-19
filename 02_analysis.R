#---------------------------------------------------------------------------------------------
# 02_analysis
# Read the outputs of the uav 01_modelling analyse and produce some graphs for the manuscript.
# Jonathan Dash
#--------------------------------------------------------------------------------------------

# Set libraries ----------------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)
library(patchwork)


# Read data --------------------------------------------------------------------

north.base<-read_csv(here('results', 'north050420.csv'))
north.base$site<-'HFP-1'
south.base<-read_csv(here('results', 'south050420.csv'))
south.base$site<-'HFP-2'
west.base<-read_csv(here('results', 'west050420.csv'))
west.base$site<-'HFP-3'
tekapo.base<-read_csv(here('results', 'tekapo050420targ_tekapo.csv'))
tekapo.base$site<-'LP-1'
kawekas.base<-read_csv(here('results', 'kawekas050420targ_kawekas.csv'))
kawekas.base$site<-'KFP-1'

#intra site

north.south<-read_csv(here('results', 'north050420targ_south.csv'))
north.south$site<-'HFP-1'
north.south$targ<-'HFP-2'

north.west<-read_csv(here('results', 'north050420targwest.csv'))
north.west$site<-'HFP-1'
north.west$targ<-'HFP-3'

south.north<-read_csv(here('results', 'south050420targ_north.csv'))
south.north$site<-'HFP-2'
south.north$targ<-'HFP-1'

south.west<-read_csv(here('results', 'south050420targ_west.csv'))
south.west$site<-'HFP-2'
south.west$targ<-'HFP-3'

west.north<-read_csv(here('results', 'west050420targ_north.csv'))
west.north$site<-'HFP-3'
west.north$targ<-'HFP-1'

west.south<-read_csv(here('results', 'west050420targ_south.csv'))
west.south$site<-'HFP-3'
west.south$targ<-'HFP-2'


# inter site

# Read through all the csv files in a location and store as a single file
# All data wrangling for comparison done here on import

inter.site <-
  list.files(here('results',  'intersite'), 
             pattern = "*.csv",
             full.names = TRUE) %>% 
  map_df(~read_csv(.)) %>%
  separate(source, c('A', 'B', 'C'), remove = TRUE) %>%
  mutate(C = 
           C %>% 
           is.na %>%
           ifelse(B, C) ) %>%
  mutate(A = ifelse(B == 'als', 'spectral + als', A)) %>%
  select(-B) %>%
  rename(model = A,
         source = C) %>%
  mutate(source_complexity = ifelse(source == 'tekapo', 'Low',
                                    ifelse(source == 'kawekas', 'High', 'Moderate')),
         receiver_complexity= ifelse(target == 'tekapo', 'Low',
                                     ifelse(target == 'kawekas', 'High', 'Moderate'))) %>%
  pivot_longer(cols = AUC:TSS, names_to = 'stat') %>%
  mutate(model = str_replace(model, 'spec$', 'spectral')) %>%
  unite(col = 'col_id', source_complexity:receiver_complexity, remove = FALSE)

inter.site$source_complexity<- factor(inter.site$source_complexity, levels = c('Low', 'Moderate', 'High'))
inter.site$receiver_complexity<- factor(inter.site$receiver_complexity, levels = c('Low', 'Moderate', 'High'))



# Base model accuracy comparison ----------------------------------------------------

base.all<-bind_rows(north.base, south.base, west.base, tekapo.base, kawekas.base) %>%
  mutate(source = str_replace(source, 'spec_north', 'spectral')) %>%
  mutate(source = str_replace(source, 'als_pred_north', 'als')) %>%
  mutate(source = str_replace(source, 'als_pred_south', 'als')) %>%
  mutate(source = str_replace(source, 'als_pred_west', 'als')) %>%
  mutate(source = str_replace(source, 'spec_south', 'spectral')) %>%
  mutate(source = str_replace(source, 'spec_west', 'spectral')) %>%
  mutate(source = str_replace(source, 'spec_als_north', 'spectral + als')) %>%
  mutate(source = str_replace(source, 'spec_als_south', 'spectral + als')) %>%
  mutate(source = str_replace(source, 'spec_als_west', 'spectral + als')) %>% 
  mutate(source = str_replace(source, 'spec_tekapo', 'spectral')) %>%
  mutate(source = str_replace(source, 'als_pred_tekapo', 'als')) %>%
  mutate(source = str_replace(source, 'spec_als_tekapo', 'spectral + als')) %>%
  mutate(source = str_replace(source, 'spec_kawekas', 'spectral')) %>%
  mutate(source = str_replace(source, 'als_pred_kawekas', 'als')) %>%
  mutate(source = str_replace(source, 'spec_als_kawekas', 'spectral + als')) %>%
  pivot_longer(cols = AUC:TSS, names_to = 'stat')
          
str(base.all)
         
base.sum<- base.all %>% group_by(site, source, stat) %>%
  summarise(mean.value = mean(value, na.rm=TRUE))

base.all %>% group_by(stat) %>%
  summarise(mean.value = mean(value, na.rm = TRUE))



write_csv(base.sum, here('results', 'base_summary.csv'))

base.sum %>% filter(stat %in% c('AUC', 'Kappa', 'TSS')) %>%
  ggplot(aes(y= mean.value, x=source, group=stat, colour = stat, shape = stat, fill = stat)) +
  geom_point() +
  scale_fill_brewer(palette = 'Set2')+
  scale_colour_brewer(palette = 'Set2')+
  geom_line()+
  facet_wrap(.~site, ncol = 3) +
  theme_bw()


bar.within<-base.sum %>% filter(stat %in% c('TSS')) %>%
  ggplot(aes(y= mean.value, x=source, group=stat, colour = source, fill = source)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5) +
  scale_fill_brewer(palette = 'Set2')+
  scale_colour_brewer(palette = 'Set2')+
  #geom_line()+
  facet_wrap(.~site, ncol = 3) +
  theme_bw() +
  labs(y = 'Total skill statistic (TSS)', x = 'Model') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #coord_flip()


png(here('out', 'within_bar.png'))
bar.within
dev.off()


# Within site model accuracy --------------------------------------------------

intra.site<-bind_rows(north.south, 
                      south.north, 
                      north.west, 
                      south.west, 
                      west.north, 
                      west.south) %>%
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


# calculate means
intra.sum<- intra.site %>% group_by(site, targ, source, stat) %>%
  summarise(mean.intra.value = mean(value, na.rm=TRUE))


# Overall mean of the intrasite data
intra.site %>% group_by(stat) %>%
  summarise(mean.value = mean(value, na.rm = TRUE))


# bring in the base and display as a change from the base
#base.sum$targ <- base.sum$site 
base.sum$scenario<-'base'

intra.sum$scenario<-'intra'

base.sum <- base.sum %>% select(site,  source, stat, mean.value, scenario)

intra.base<-inner_join(base.sum, intra.sum, by = c('site', 'source', 'stat')) %>%
  mutate(valu.change = mean.intra.value - mean.value) 


tt<-intra.base %>% filter(stat %in% c('AUC', 'Kappa', 'TSS')) %>% 
  group_by(site, source, stat) %>%
  summarise(base = mean(mean.value),
            intra = mean(mean.intra.value)) %>%
  mutate(change = base - intra) 

labs<- tt %>% filter(site == 'HFP-3',
                     stat == 'AUC')

intrasite.results<-tt %>%  ggplot(aes(x = site , y = change, group = source, colour = source, shape = source)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_label_repel( data = labs,
    aes(x = site, y= change, group = source, label=source, colour = source)) +
  labs(y = "Accuracy base - Accuracy transfer", x = 'Source AOI - fitting dataset', colour = 'Model' ) +
  scale_fill_brewer(palette = 'Set2')+
  scale_colour_brewer(palette = 'Set2')+
  facet_wrap(.~stat, ncol=3)+
  theme_bw() +
  #ylim(c(-0.1, 0.1))+
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png(here('out', 'intrasite_results.png'), h=8, w = 20, units = 'cm', res=500)
intrasite.results
dev.off()

# Make stats on intra site comparison for inclusion in manuscript
intra.base %>% filter(stat == 'AUC') %>% 
  group_by(source) %>%
  summarise(base = mean(mean.value),
            intra = mean(mean.intra.value)) %>%
  mutate(change = base - intra) 


intra.base %>% filter(stat == 'Kappa') %>% 
  group_by(source) %>%
  summarise(base = mean(mean.value),
            intra = mean(mean.intra.value)) %>%
  mutate(change = base - intra) 


intra.base %>% filter(stat == 'TSS') %>% 
  group_by(source) %>%
  summarise(base = mean(mean.value),
            intra = mean(mean.intra.value)) %>%
  mutate(change = base - intra) 


# Between site model accuracy --------------------------------------------------


# Intersite compasison

# overall average of inter sites
inter.site %>% group_by(stat) %>%
  summarise(mean.value = mean(value, na.rm = TRUE)) 



# base by model type
base.s<-base.all %>% group_by(source, stat) %>%
  summarise(mean.value.base = mean(value, na.rm = TRUE)) %>%
  filter(stat %in% c('AUC', 'Kappa', 'TSS')) %>%
  rename(model = source)

# intra all by model
intra.s<-intra.site %>% group_by(source, stat) %>%
  summarise(mean.value.intra = mean(value, na.rm = TRUE)) %>%
  filter(stat %in% c('AUC', 'Kappa', 'TSS')) %>%
  rename(model = source)


# intersite by model type
inter.site %>% group_by(model, stat) %>%
  summarise(mean.value.inter = mean(value, na.rm = TRUE)) %>%
  filter(stat %in% c('AUC', 'Kappa', 'TSS')) %>%
  left_join(base.s) %>%
  mutate(difference = mean.value.base - mean.value.inter)


  
# intersite by source complexity
inter.site %>% group_by(source_complexity, stat) %>%
  summarise(mean.value = mean(value))


# inter.site by receiver complexity
inter.site %>% group_by(receiver_complexity, stat) %>%
  summarise(mean.value = mean(value))


# inter.site by receiver and site complexity
inter.site %>% group_by(source_complexity, receiver_complexity,  stat, col_id) %>%
  summarise(mean.value = mean(value)) %>% 
  filter(stat %in% c('AUC', 'Kappa', 'TSS')) %>% 
  ggplot(aes(x = source_complexity, y = receiver_complexity, size = mean.value, colour = mean.value)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(10,50)) +
  theme_bw() +
  scale_colour_viridis_c()+
  facet_wrap(.~stat) 
  #theme(legend.position = 'none')
  



# inter.site by receiver and site complexity
p1<-inter.site %>% 
  group_by(source_complexity, receiver_complexity,  model, stat, col_id) %>%
  summarise(mean.value = mean(value)) %>% 
  filter(stat %in% c('AUC')) %>% 
  ggplot(aes(x = source_complexity, y = receiver_complexity, colour = mean.value, fill = mean.value)) +
  geom_tile(alpha = 1) +
  geom_text(aes(label = round(mean.value, 2)), colour = '#a84551') +
  #scale_size(range = c(10,50)) +
  theme_bw() +
  scale_colour_viridis_c(option = 'E')+
  scale_fill_viridis_c(option = 'E')+
  facet_wrap(.~model) +
  labs(x = '', y = '', fill = 'Mean value', colour = 'Mean value', title = 'AUC')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #theme(legend.position = 'none')
p1

# inter.site by receiver and site complexity
p2<-inter.site %>% 
  group_by(source_complexity, receiver_complexity, model,  stat, col_id) %>%
  summarise(mean.value = mean(value)) %>% 
  filter(stat %in% c('Kappa')) %>% 
  ggplot(aes(x = source_complexity, y = receiver_complexity, colour = mean.value, fill = mean.value)) +
  geom_tile(alpha = 0.9) +
  geom_text(aes(label = round(mean.value, 2)), colour = '#a84551') +
  #scale_size(range = c(10,50)) +
  theme_bw() +
  scale_colour_viridis_c(option = 'E')+
  scale_fill_viridis_c(option = 'E')+
  facet_wrap(.~model) +
  labs(x = '', y = 'Receiver AOI complexity', fill = 'Mean value', colour = 'Mean value', title = 'Kappa')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# inter.site by receiver and site complexity
p3<-inter.site %>% 
  group_by(source_complexity, receiver_complexity,  model, stat, col_id) %>%
  summarise(mean.value = mean(value)) %>% 
  filter(stat %in% c('TSS')) %>% 
  ggplot(aes(x = source_complexity, y = receiver_complexity, colour = mean.value, fill = mean.value)) +
  geom_tile(alpha = 0.9) +
  geom_text(aes(label = round(mean.value, 2)), colour = '#a84551') +
  #scale_size(range = c(10,50)) +
  theme_bw() +
  scale_colour_viridis_c(option = 'E')+
  scale_fill_viridis_c(option = 'E')+
  facet_wrap(.~model) +
  labs(x = 'Donor AOI complexity', y = '', fill = 'Mean value', colour = 'Mean value', title = 'TSS')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




p.out<- p1 / p2 /p3 #+ plot_annotation(tag_levels = 'a', tag_suffix = ')')
p.out

png(here('out', 'inter_site_tile.png'), h = 17, w = 19, units = 'cm', res= 500)
p.out
dev.off()



