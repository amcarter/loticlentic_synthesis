# compare metabolism to residence times across sites. 
# This is inspired by Hotchkiss 2018 - do we see systematic patterns across
#   gradients of residence time in GPP and ER? Would it be worth thinking
#   about this at an individual site level or extending it to include lakes?

library(tidyverse)
setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')
# read in summarized powell center data for residence time and k values
nres <- read_csv('data/normalized_residence_times_and_kO2.csv', 
                 guess_max = 1000000)

sts <- nres %>%
  select(-ends_with('sd'), -class, -iT_norm) %>%
  group_by(sitecode) %>%
  summarize(across(everything(), 
                   .fns = list(mean = ~mean(., na.rm = T), 
                               median = ~median(., na.rm = T), 
                               cv = ~sd(., na.rm = T)))) %>%
  ungroup() 
