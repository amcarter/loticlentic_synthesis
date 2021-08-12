# Data exploration powell data

library(tidyverse)
library(lubridate)
library(StreamPULSE)
setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')

dat <- read_csv('data/powell_data_import/compiled_daily_model_results.csv') %>%
  as_tibble() %>%
  rename(sitecode = site_name)
# plot(dat$GPP, dat$ER, pch = 20, col = alpha(1, 0.1))
# abline(0, -1)

site_dat = readRDS('data/phil_powell_data/lotic_site_info_filtered.rds') %>%
  as_tibble() %>%
  select(sitecode = Site_ID,
         Stream_PAR_sum, Disch_ar1, Disch_skew, MOD_ann_NPP, Width)
# load and prepare streampulse data ####

sp_list = readRDS('data/phil_powell_data/lotic_gap_filled.rds')

#summarize by site
sp_names = names(sp_list)

for(i in 1:length(sp_list)){
  sp_list[[i]]$sitecode = sp_names[i]
}

sp_full = Reduce(bind_rows, sp_list) %>%
  as_tibble() %>%
  select(sitecode, Date, Year, DOY, GPP_C, ER_C, GPP_C_filled, ER_C_filled) %>%
  arrange(sitecode, Date)

# plot(sp_full$GPP_C_filled, sp_full$ER_C_filled, pch = 20, col = alpha(1, 0.1))
# abline(0,-1)

# Get model results from the SP portal for sites that aren't powell center:
sites <- unique(sp_full$sitecode)
sites <- sites[!grepl('nwis', sites)]
for(s in sites){
  dates <- StreamPULSE::query_available_data(substr(s, 1, 2), 
                                    substr(s, 4, nchar(s)))$datebounds %>%
    mutate(across(everything(), 
                  year))
  years <- seq(dates[1,1], dates[1,2])
  for(y in years){
    error_encountered <- FALSE
    m <- tryCatch(StreamPULSE::request_results(s, y),
                  error = function(e) error_encountered <<- TRUE)
    if(error_encountered) next
                    
    mm <- m$model_results$fit$daily %>%
      select(date, GPP = GPP_mean, GPP.lower = GPP_2.5pct, 
             GPP.upper = GPP_97.5pct, ER = ER_mean, ER.lower = ER_2.5pct,
             ER.upper = ER_97.5pct, K600 = K600_daily_mean, 
             K600.upper = K600_daily_2.5pct, K600.lower = K600_daily_97.5pct) %>%
      mutate(sitecode = s)
    mm <- m$model_results$data %>%
      select(date, DO.obs, DO.sat, depth, temp.water, discharge) %>%
      mutate(DO.psat = DO.obs/DO.sat) %>%
      full_join(mm, by = 'date') 
    dat <- bind_rows(dat, mm)
  }
}

sp_all <- sp_full %>% 
  rename(date = Date) %>%
  left_join(dat, by = c("sitecode", "date")) %>%
  arrange(sitecode, date) %>%
  select(-Year, -ends_with(c( 'n_eff')), -resolution) %>%
  group_by(sitecode) %>%
  mutate_at(vars(-date, -sitecode), zoo::na.approx, na.rm = F, maxgap = 3)%>%
  ungroup()



write_csv(sp_all, 'data/phil_powell_data/lotic_gap_filled_dataframe_with_metadata.csv')
# compare filtered and unfiltered datasets ####

plot(dat$GPP, dat$ER, col = 'red')
points(sp_all$GPP, sp_all$ER)
abline(0,-1, col = 3)

par(mfrow = c(1,3))
plot(density(dat$GPP.Rhat), xlim = c(1, 1.1))
lines(density(sp_all$GPP.Rhat, na.rm = T), col = 2)
plot(density(dat$ER.Rhat), xlim = c(1, 1.1))
lines(density(sp_all$ER.Rhat, na.rm = T), col = 2)
plot(density(dat$K600.Rhat), xlim = c(1, 1.7))
lines(density(sp_all$K600.Rhat, na.rm = T), col = 2)

# explore data
plot(log(sp_all$discharge), sp_all$DO.psat, pch = 20, col = alpha(1, .2))
pal = colorRampPalette(c('blue', 'red'))
cols <- pal(10)
for(i in unique(sp_all$sitecode)){
  d <- sp_all[sp_all$sitecode == i,] %>%
    arrange(date) 
  n <- nrow(d)
  d$col = sort(rep(1:10, length.out = n))
  if(sum(!is.na(d$discharge)) == 0) next
  plot(d$discharge, d$DO.psat, log = 'x', pch = 20, col = cols[d$col], main = i)
}
sp_all %>%
  filter(!is.na(discharge)) %>%
  ggplot(aes(log(discharge), DO.psat, col = date)) +
    geom_point()+
    facet_wrap('sitecode')
  all$discharge[sp_all$sitecode == i],
       sp_all$DO.psat[sp_all$sitecode == i], main = i, log = x)
}