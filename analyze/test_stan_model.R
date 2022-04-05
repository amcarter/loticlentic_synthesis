# Write linear stan model of DO based on lenticity metrics
# A Carter

# setup ####
library(tidyverse)
library(lme4)
library(rstan)
library(gdata)
library(bayesplot)

setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')
# read in summarized powell center data for residence time and k values
nres <- read_csv('data/normalized_residence_times_and_kO2.csv', 
                 guess_max = 1000000)


# Examine data ####
ggplot(nres, aes(iT_norm, DO.obs, col = sitecode)) +
  geom_point() +
  theme(legend.position = 'none')
# try a mixed effects linear model:
mm <- lm(DO.obs ~ iT_norm + log(kO2), data = nres)
summary(mm)


mr <- lmer(DO.obs ~ iT_norm + log(kO2) + (1|sitecode), data = nres)
summary(mr)

# Stan model ####
# prepare data for a stan model
sites <- unique(nres$sitecode)
res_time <- zoo::na.approx(nres$iT_norm[nres$sitecode == sites[13]], na.rm = F)
kO2 <- zoo::na.approx(nres$kO2[nres$sitecode == sites[13]], na.rm = F)
DO.obs <- zoo::na.approx(nres$DO.obs[nres$sitecode == sites[13]], na.rm = F)
w <- unique(c(which(is.na(res_time)), which(is.na(kO2)), which(is.na(DO.obs))))
res_time <- res_time[-w]
kO2 <- log(kO2[-w])
DO <- DO.obs[-w]
N <- length(DO)
DOpre <- c(DO[1], DO[1:N-1])
stan_dat <- list(N = N, res_time = res_time, kO2 = kO2, DOpre = DOpre, DO = DO)

write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] res_time; // Predictor1
 vector[N] kO2; // Predictor2
 vector[N] DOpre;// intercept
 vector[N] DO; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta1; // Slope1 (regression coefficients)
 real beta2; // Slope2 (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 DO ~ normal(alpha + DOpre + res_time * beta1 + kO2 * beta2, sigma);
}

generated quantities {
} // The posterior predictive distribution",

"src/stan/stan_model1.stan")

stanc('src/stan/stan_model1.stan')
stan_mod1 <- 'src/stan/stan_model1.stan'

fit <- stan(file = stan_mod1, data = stan_dat, warmup = 500, iter = 1000, 
            chains = 4, cores = 2, thin = 1)
fit

posterior <- extract(fit)
str(posterior)


traceplot(fit)
stan_dens(fit)
stan_hist(fit)
plot(fit, show_density = FALSE, ci_level = 0.5, outer_level = 0.95, fill_color = "salmon")
