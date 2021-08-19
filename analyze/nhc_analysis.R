# compile input and model output data for powell center sites into daily values

library(tidyverse)
library(streamMetabolizer)
library(lubridate)
library(data.table)

setwd("C:/Users/Alice Carter/git/loticlentic_synthesis/")
source('src/analyze/calculate_integrated_residence_time.R')

# site metadata:
site_dat <- read_csv('data/streampulse/all_basic_site_data.csv') %>%
  select(regionID, siteID, lon = longitude)
mod_dat <- read_csv('data/phil_powell_data/lotic_gap_filled_dataframe_with_metadata.csv')  %>%
  select(sitecode, date, K600)
meta_dat <- readRDS('data/phil_powell_data/lotic_site_info_full.rds')

# compile input data: ####
filelist <- list.files('data/streampulse/model_inputs/')
d <- read_csv('data/streampulse/model_inputs/all_sp_data01.csv') %>%
  slice(-1) %>%
  left_join(site_dat, by = c('regionID', 'siteID'))
cols <- colnames(d)
d <- d %>%
  mutate(sitecode = paste(regionID, siteID, sep = '_'), 
         dateTimeUTC = ymd_hms(dateTimeUTC, tz = 'UTC'),
         value = as.numeric(value)) %>%
  select(-siteID, -starts_with('flag')) %>%
  mutate(solar_time = 
           streamMetabolizer::convert_UTC_to_solartime(dateTimeUTC, lon, 
                                                       'apparent solar'), 
         date = as.Date(solar_time))
  
daily <- d %>%
  select(-dateTimeUTC, -solar_time,  -lon) %>%
  mutate(value = ifelse(value <= -1000, NA, value))%>%
  group_by(regionID, sitecode, date, variable) %>%
  summarize(mean = mean(value, na.rm = T),
            sdev = sd(value, na.rm = T), 
            amp = max(value, na.rm = T) - 
              min(value, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c('regionID', 'sitecode', 'date'), 
              names_from = 'variable', values_from = c('mean', 'sdev', 'amp')) %>%
  select(regionID, sitecode, date, depth_m = mean_Depth_m, discharge_m3s = 
           mean_Discharge_m3s, DO.obs = mean_DO_mgL, DO.sat = mean_satDO_mgL, 
         light_PAR = mean_Light_PAR, temp.water = mean_WaterTemp_C, depth_sd = 
           sdev_Depth_m, discharge_sd = sdev_Discharge_m3s, DO.obs_sd = sdev_DO_mgL, 
           DO.sat_sd = sdev_satDO_mgL, light_PAR_sd = sdev_Light_PAR,
           temp.water_sd = sdev_WaterTemp_C, DO.amp = amp_DO_mgL)

widths <- select(meta_dat, sitecode = Site_ID, width_m = Width)   
dd <- data.table(d)
rt_dat <- dd[, .(value = mean(value, na.rm = T)), 
             by = c('regionID', 'sitecode', 'date', 
                    'solar_time', 'variable')] %>%
  as_tibble() %>%
  pivot_wider(id_cols = c('regionID', 'sitecode', 'date', 'solar_time'), 
              names_from = 'variable', values_from = 'value') %>% 
  left_join(mod_dat, by = c('sitecode', 'date')) %>%
  select(regionID, sitecode, datetime = solar_time, depth_m = Depth_m, 
         discharge_m3s = Discharge_m3s, temp.water = WaterTemp_C, K600) %>%
  left_join(widths, by = 'sitecode') %>%
  filter(sitecode == 'NC_NHC')
  
sites <- unique(d$sitecode)
res_times <- data.frame()
for(s in sites){
  dd <- rt_dat #%>%
    # filter(sitecode == s)
  if(sum(!is.na(dd$K600))/nrow(dd) < 0.1) next
  rt <- calculate_iTR(dd)
  rt$sitecode = s
  res_times <- bind_rows(rt, res_times)
}

daily_dat <- left_join(daily, res_times, by = c('sitecode', 'date'))


for(f in filelist[2:length(filelist)]){
  d <- read_csv(paste0('data/powell_data_import/model_inputs/', f),
                col_names = F)
  colnames(d) <- cols
  d <- d %>%
    mutate(sitecode = str_replace(siteID, '-', '_')) %>%
    select(-siteID) %>%
    left_join(latlon, by = 'sitecode') %>%
    mutate(solar_time = 
             streamMetabolizer::convert_UTC_to_solartime(dateTimeUTC, lon, 
                                                         'apparent solar'), 
           date = as.Date(solar_time))
  daily <- d %>%
    select(-dateTimeUTC, -solar_time, -lat, -lon) %>%
    group_by(regionID, sitecode, date, variable) %>%
    summarize(mean = mean(value, na.rm = T),
              sdev = sd(value, na.rm = T), 
              amp = max(value, na.rm = T) - 
                min(value, na.rm = T)) %>%
    ungroup() %>%
    pivot_wider(id_cols = c('regionID', 'sitecode', 'date'), 
                names_from = 'variable', values_from = c('mean', 'sdev', 'amp')) %>%
    select(regionID, sitecode, date, depth_m = mean_Depth_m, discharge_m3s = 
             mean_Discharge_m3s, DO.obs = mean_DO_mgL, DO.sat = mean_satDO_mgL, 
           light_PAR = mean_Light_PAR, temp.water = mean_WaterTemp_C, depth_sd = 
             sdev_Depth_m, discharge_sd = sdev_Discharge_m3s, DO.obs_sd = sdev_DO_mgL, 
           DO.sat_sd = sdev_satDO_mgL, light_PAR_sd = sdev_Light_PAR,
           temp.water_sd = sdev_WaterTemp_C, DO.amp = amp_DO_mgL)
  
  widths <- select(meta_dat, sitecode = Site_ID, width_m = Width)   
  rt_dat <- d %>% pivot_wider(id_cols = c('regionID', 'sitecode', 
                                          'date', 'solar_time'), 
                              names_from = 'variable', values_from = 'value') %>% 
    left_join(mod_dat, by = c('sitecode', 'date')) %>%
    select(regionID, sitecode, datetime = solar_time, depth_m = Depth_m, 
           discharge_m3s = Discharge_m3s, temp.water = WaterTemp_C, K600) %>%
    left_join(widths, by = 'sitecode') 
  
  sites <- unique(d$sitecode)
  res_times <- data.frame()
  for(s in sites){
    dd <- rt_dat %>%
      filter(sitecode == s)
    rt <- calculate_iTR(dd)
    rt$sitecode = s
    res_times <- bind_rows(rt, res_times)
  }
  
  daily_dat1 <- left_join(daily, res_times, by = c('sitecode', 'date'))
  daily_dat <- bind_rows(daily_dat, daily_dat1)

}

write_csv(daily_dat, 'data/powell_data_import/compiled_daily_model_inputs.csv')

# compile model outputs ####

filelist <- list.files('data/powell_data_import/model_outputs/model_objects/')
predictions <- data.frame()
for(f in filelist){
  m <- read_rds(paste0('data/powell_data_import/model_outputs/model_objects/', f))
  if(nrow(m$predictions)<1) next
  predictions <- bind_rows(predictions, m$predictions)
}
write_csv(predictions, 'data/powell_data_import/compiled_daily_model_results.csv')
