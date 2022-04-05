# Compare different continuous metrics of lotic and lentic

setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')
source('src/analyze/calculate_integrated_residence_time.R')
# 1. calculate integrated residence time based on Jones et al.
#     normalize calculations based on volume so that lotic and lentic are a 
#     range from 0 to 1 for all systems

# read in data with calculated times
# iTR is in days
dat <- read_csv('data/powell_data_import/compiled_daily_model_inputs.csv')
ll <- dat %>%
  group_by(sitecode) %>%
  summarize(vol = median(volume_m3, na.rm = T))

ll <- bind_cols(ll, log(calc_LL_thresholds(ll$vol))) 

# transform so that lotic = 0 and lentic = 1
ll <- ll %>%
  mutate(slope = 1/(lentic - lotic), 
         intercept = -lotic * slope) %>%
  select(sitecode, slope, intercept)

d <- left_join(dat, ll, by = 'sitecode') %>%
  # filter(!is.na(iTR), class == 'lentic') %>%
  filter(iTR!=0) %>%
  mutate(iT_norm = (intercept + slope * log(iTR)))

# plot(density(d$iT_norm, na.rm = T))
# summary(d$iT_norm)
# abline(v = c(0,1))

# 2. Calculate turbulence based on k (gas exchange velocity in m/d)
#     I am going to use k600 for now
filled <- read_csv('data/phil_powell_data/lotic_gap_filled_dataframe_with_metadata.csv') %>%
  select(-ends_with('Rhat'))
m <- filled %>%
  mutate(k600 = depth * K600, 
         kO2 = streamMetabolizer::convert_k600_to_kGAS(k600, temp.water)) %>%
  select(sitecode, date, GPP, ER, K600, kO2)
# mod <- read_csv('data/powell_data_import/compiled_daily_model_results.csv')
d <- left_join(m, d, by = c('sitecode', 'date')) %>%
  rename(iTR_days = iTR) %>%
  select(-slope, -intercept)

write_csv(d, 'data/normalized_residence_times_and_kO2.csv')

dd = d[seq(1, nrow(d), 5), ]

par(mfrow = c(2,2))
plot(dd$iT_norm, dd$DO.obs)#, pch = 20)#, col = alpha('black', 0.3))
plot(dd$kO2, dd$DO.obs, log = 'x')#at, pch = 20, col = alpha('black', 0.3))
plot(dd$iT_norm, dd$DO.obs/dd$DO.sat)#, pch = 20, col = alpha('black', 0.3))
plot(dd$kO2, dd$DO.obs/dd$DO.sat, log = 'x')#, pch = 20, col = alpha('black', 0.3))

ggplot(dd, aes(iT_norm, DO.obs/DO.sat, col = log(kO2))) +
  geom_point()
ggplot(dd, aes(DO.obs, DO.sat, col = temp.water)) +
  geom_point()

ggplot(dd, aes(iT_norm, log(kO2), col = DO.obs/DO.sat)) +
  geom_point(size = 0.2)

