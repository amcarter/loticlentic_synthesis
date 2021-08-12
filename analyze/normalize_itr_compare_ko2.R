# Compare different continuous metrics of lotic and lentic

setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')
source('src/analyze/calculate_integrated_residence_time.R')
# 1. calculate integrated residence time based on Jones et al.
#     normalize calculations based on volume so that lotic and lentic are a 
#     range from 0 to 1 for all systems

# read in data with calculated times
 
dat <- read_csv('data/powell_data_import/compiled_daily_model_inputs.csv')
ll <- dat %>%
  group_by(sitecode) %>%
  summarize(vol = median(volume_m3, na.rm = T))

ll <- bind_cols(ll, log(calc_LL_thresholds(ll$vol))) 

# transform so that lotic = 1 and lentic = 10
ll <- ll %>%
  mutate(slope = 1/(lentic - lotic), 
         intercept = -lotic * slope) %>%
  select(sitecode, slope, intercept)

d <- left_join(dat, ll, by = 'sitecode') %>%
  # filter(!is.na(iTR), class == 'lentic') %>%
  mutate(iT_norm = (intercept + slope * log(iTR)))

# plot(density(d$iT_norm, na.rm = T))
# summary(d$iT_norm)
# abline(v = c(0,1))

# 2. Calculate turbulence based on k (gas exchange velocity in m/d)
#     I am going to use k600 for now

mod <- read_csv('data/powell_data_import/compiled_daily_model_results.csv')
m <- mod %>%
  mutate(k600 = depth * K600, 
         kO2 = streamMetabolizer::convert_k600_to_kGAS(k600, temp.water)) %>%
  select(sitecode = site_name, date, GPP, ER, K600, kO2)
d <- left_join(d, m, by = c('sitecode', 'date')) %>%
  select(-iTR, -slope, -intercept)



par(mfrow = c(2,2))
plot(d$iT_norm, d$DO.sat, pch = 20, col = alpha('black', 0.3))
plot(d$kO2, d$DO.sat, pch = 20, col = alpha('black', 0.3))
plot(d$iT_norm, d$DO.obs, pch = 20, col = alpha('black', 0.3))
plot(d$kO2, d$DO.obs, pch = 20, col = alpha('black', 0.3))

ggplot(d, aes(iT_norm, DO.sat, col = kO2)) +
  geom_point()
ggplot(d, aes(DO.obs, DO.sat, col = temp.water)) +
  geom_point()
