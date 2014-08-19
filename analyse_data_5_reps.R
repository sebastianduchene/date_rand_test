dat <- read.table('compiled_data.txt', as.is = T)

colnames(dat) <- c('run_name', 'cal_time', 'sim_rate', 'sd_rate', 'slope', 'r', 'rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high', paste0('r1_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r2_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r3_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r4_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r5_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')) )


pass_cr1 <- vector()
pass_cr2 <- vector()

for(i in 1:nrow(dat)){
      rates_high <-dat[i, grep('r.+rate_high', colnames(dat))] 
      rates_low <- dat[i, grep('r.+rate_low', colnames(dat))] 
      pass_cr1[i] <- dat$rate_mean[i] >= max(rates_high) | dat$rate_mean[i] <= min(rates_low)
      pass_cr2[i] <- dat$rate_low[i] >= max(rates_high) | dat$rate_high[i] <= min(rates_low)
}



plot(dat$cal_time[dat$sd_rate == 0.01], dat$rate_mean[dat$sd_rate == 0.01], col = c('red', 'black')[as.numeric(pass_cr1) + 1], ylim = c(0, 0.015))
lines(x = c(0, 30), y = c(0.0001, 0.0001))

plot(dat$cal_time[dat$sd_rate == 0.3], dat$rate_mean[dat$sd_rate == 0.3], col = c('red', 'black')[as.numeric(pass_cr1) + 1], ylim = c(0, 0.015))
lines(x = c(0, 30), y = c(0.0001, 0.0001))

