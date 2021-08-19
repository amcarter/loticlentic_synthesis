library(tidyverse)
library(data.table)
library(xts)
library(dygraphs)

setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')
# read in summarized powell center data for residence time and k values
nres <- read_csv('data/normalized_residence_times_and_kO2.csv', 
                 guess_max = 1000000)

sts <- nres %>%
  group_by(sitecode) %>%
  summarize(across(c('kO2', 'iT_norm'), 
                   .fns = list(mean = ~mean(., na.rm = T), 
                               median = ~median(., na.rm = T), 
                               cv = ~sd(., na.rm = T)))) %>%
  ungroup() %>%
  mutate(type = case_when(iT_norm_median > 0.75 ~ 'lentic',
                          iT_norm_median < 0.25 ~ 'lotic', 
                          TRUE ~ 'intermediate'))

res <- left_join(nres, sts, by = 'sitecode')
  
ggplot(res, aes(iT_norm, col = log(kO2_median), group = sitecode)) +
  geom_density() + 
  ylim(0, 11) +
  # theme(legend.position = 'none') +
  xlab('normalized residence time') + 
  scale_color_gradientn(colors = rainbow(5))# geom_vline(xintercept = c(0, 1))


ggplot(sts, aes(kO2_median, iT_norm_median, col = type)) +
  geom_point()

res %>%
  filter(sitecode %in% c('nwis_02264000', 'nwis_06893890', 'nwis_11044000')) %>%
  ggplot(aes(iT_norm, kO2, col = sitecode)) +
  geom_point()

d <- res %>%
  filter(sitecode %in% c('nwis_08052700','nwis_02264000', 'nwis_06893890', 'nwis_11044000'))

ggplot(d, aes(iT_norm, log(kO2), col = DO.obs/DO.sat)) +
  geom_point() +
  scale_color_gradientn(colors = plasma(4), na.value = 'white') +
  xlab('turnover time') +
  ylab('turbulence') +
  facet_wrap(~sitecode)
  ylim(0, 10)
d %>%
  mutate(DO = DO.obs/DO.sat) %>%
  select(kO2, iT_norm, DO, GPP, ER) %>%
  xts(order.by = d$date) %>%
  dygraph() %>%
  dyRangeSelector()  

d <- res %>%
  filter(sitecode %in% c('nwis_02264000', 'nwis_06893890', 'nwis_11044000')) %>%

sts %>% 
  filter(iT_norm_cv > 0.33) 
