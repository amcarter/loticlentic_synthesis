library(streamMetabolizer)
library(tidyverse)
N = 10000
# model discharge as a random walk. This could be improved.
q = rep(10, N)
for(i in 2:N){
  q[i] <- q[i-1] + rnorm(1)
  if(q[i]<0) q[i] = 0
}


L = 1000
W = 20
rt <- tibble(d = streamMetabolizer::calc_depth(Q = q),
             q = q,
             iT = rep(NA_real_, N)) %>%
  mutate(vol = (d + 0.5) * L * W )
for(i in 1:N){
dd <- rt[i:N,] %>% 
         mutate(vcum = cumsum(q*60*15),
         vdiff = vcum - vol)
rt$iT[i] <- min(which(dd$vdiff>=0))
}
 
par(mfrow = c(2,1),
    mar = c(2,4,1,1))
plot(rt$q, type = 'l', ylab = 'discharge')
plot(rt$iT, type = 'l', ylab = 'int res time')

rt %>%
  mutate(index = seq(1:N),
    type = case_when(iT >quantile(iT, 0.75) ~ 'lentic',
                     iT < quantile(iT, 0.25) ~'lotic',
                          TRUE ~ 'int')) %>%
  ggplot(aes(index, q, col = type)) +
  geom_point() +
  ylab('discharge')
