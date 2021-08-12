# Calculate lotic and lentic behavior of stream reaches based on discharge
# According to the integrated residence time calculations in Jones et al 2016

library(tidyverse)
setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')

# load data: ####
# dat <- read_csv('data/phil_powell_data/lotic_gap_filled_dataframe_with_metadata.csv')
# site_dat = readRDS('data/phil_powell_data/lotic_site_info_filtered.rds') %>%
#   as_tibble() %>%
#   select(sitecode = Site_ID, Lat, Lon,
#          Stream_PAR_sum, Disch_ar1, Disch_skew, MOD_ann_NPP, Width)
# # make example dataset
# d <- read_csv('data/powell_data_import/model_inputs/all_powell_data01.csv') %>%
#   mutate(sitecode = str_replace(siteID, '-', '_')) %>%
#   select(-siteID)
# w <- which(d$sitecode %in% site_dat$sitecode)[1]
# ss <- d$sitecode[w]
# s <- d %>%
#   filter(sitecode == ss) %>%
#   pivot_wider(id_cols = c('regionID', 'sitecode', 'dateTimeUTC'),
#               names_from = 'variable', values_from = 'value') %>%
#   mutate(width_m = site_dat$Width[site_dat$sitecode==ss],
#          lon = site_dat$Lon[site_dat$sitecode==ss],
#          solar_time =
#            streamMetabolizer::convert_UTC_to_solartime(dateTimeUTC, lon,
#                                                        'apparent solar')) %>%
#   select(-dateTimeUTC) %>%
#   arrange(solar_time)
# mod <- read_csv('data/powell_data_import/compiled_daily_model_results.csv') %>%
#   filter(site_name == ss) %>%
#   select(date, K600)
# 
# s <- s %>%
#   mutate(date = as.Date(solar_time)) %>%
#   left_join(mod, by = 'date') %>%
#   select(datetime = solar_time, depth_m = Depth_m,
#          discharge_m3s = Discharge_m3s, width_m, K600, temp.water = WaterTemp_C)
# write_csv(s, 'data/example_iTR_calculation_site.csv')
# dat <- read_csv('data/example_iTR_calculation_site.csv', guess_max = 100000)
# dat <- dat[14961:nrow(dat),]


# Function to calculate iTR
# dataframe must contain columns: width_m, depth_m, discharge_m3s, K600
# and datetime in either local or solar time.

# calculate reach length as 3v/KO2, because we are interested in metabolism and 
#   oxygen as response variables and this captures the length of 90% turnover?
#   diChappro 1992 I believe
## This should be Ko2 in the calculation (d-1)

calculate_iTR <- function(dat, lotic_th = 2.894e-5, 
                          lentic_th = 1.157e-3 ){
  if(!('velocity_ms' %in% colnames(dat))) {
    dat$velocity_ms <- NA_real_
  }
  

  
  d <- dat %>%
    mutate(velocity_ms = zoo::na.approx(velocity_ms, maxgap = 96, na.rm = F),
           velocity_ms = ifelse(!is.na(velocity_ms), velocity_ms, 
                             discharge_m3s/width_m/depth_m),
           K600 = zoo::na.approx(K600, maxgap = 96, na.rm = F),
           depth_m = zoo::na.approx(depth_m, maxgap = 96, na.rm = F),
           width_m = zoo::na.approx(width_m, maxgap = 96, na.rm = F),
           discharge_m3s = zoo::na.approx(discharge_m3s, maxgap = 96, na.rm = F),
           k600 = K600 * depth_m,
           kO2 = streamMetabolizer::convert_k600_to_kGAS(k600, temp.water),
           KO2 = kO2/depth_m,
           reach_length_m = 3 * velocity_ms/KO2 * 60 * 60 * 24, # s/d
           volume_m3 = width_m * reach_length_m * depth_m, 
           discharge_vol = discharge_m3s * 60 * 15,
           date = as.Date(datetime))
  res_times <- tibble::tibble(
    date = seq(from = d$date[1], to = d$date[nrow(d)], by = 'day')) %>%
    mutate(iTR = NA_real_)
  for(i in 1:nrow(res_times)){
    if(i%%100 == 0) print (i/nrow(res_times))
    dd <- d %>%
      filter(date >= res_times$date[i]) %>%
      arrange(datetime) %>%
      mutate(vol_cum_m3 = cumsum(discharge_vol),# calculate cumulative discharge
             vol_diff = volume_m3 - vol_cum_m3) # find when it exceeds system volume
    ddd <- dd %>% 
      filter(vol_diff <= 0)
    p <- as.numeric(ddd$datetime[1] - dd$datetime[1], units = 'days')
    res_times$iTR[i] <- p
    # if(is.na(dd$discharge_vol[1])|is.na(dd$volume_m3[1])) next
    # p <- tryCatch(Position(function(x) x >= dd$volume_m3, dd$vol_cum_m3),
    #               error = function(e) p <- NA_real_)
    # if(is.na(p)){
    #   res_times$iTR[i] <- NA
    #   next
    # }
    # res_times$iTR[i] <- as.numeric(dd$datetime[p+1] - dd$datetime[1], 
    #                                units = 'days')
  }
  
  vol = median(d$volume_m3, na.rm = T)
  lotic = lotic_th * vol^0.6699
  lentic = lentic_th * vol^0.6699
  res_times <- d %>% group_by(date) %>%
    summarize(volume_m3 = mean(volume_m3, na.rm = T)) %>%
    right_join(res_times, by = 'date') %>%
    mutate(class = case_when(iTR <= lotic ~ 'lotic',
                           iTR > lotic & iTR < lentic ~ 'inter', 
                           iTR >= lentic ~ 'lentic', 
                           TRUE ~ NA_character_))
  return(res_times)
}

# rtimes <- calculate_iTR(dat)
# write_csv(rtimes, 'data/example_iTR_calculation_output.csv')

calc_LL_thresholds <- function(vol, lotic_th = 2.894e-5, lentic_th = 1.157e-3){
  lotic = lotic_th * vol^0.6699
  lentic = lentic_th * vol^0.6699
  ll_rt_thresh <- data.frame(lotic = lotic, lentic = lentic)
  
  return(ll_rt_thresh)
}
