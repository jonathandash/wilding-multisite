#---------------------------------------------------------------------------------------------
# 02_analysis
# Read the outputs of the uav 01_modelling analyse and produce some graphs for the manuscript.
# Jonathan Dash
#--------------------------------------------------------------------------------------------

# Set libraries ----------------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)


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



# Model accuracy comparison ----------------------------------------------------

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


# Between site model accuracy