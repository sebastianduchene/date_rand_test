library(ggplot2)
dat <- read.table('compiled_data.txt', as.is = T)

colnames(dat) <- c('run_name', 'cal_time', 'sim_rate', 'sd_rate', 'slope', 'r', 'rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high', paste0('r1_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r2_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r3_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r4_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r5_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')) )



pass_cr1 <- vector()
pass_cr2 <- vector()
pass_cr3 <- vector()
pass_true <- dat$sim_rate < dat$rate_high & dat$sim_rate > dat$rate_low
cv_rates <- (dat$rate_high - dat$rate_low) / dat$rate_mean <= 0.2


rate_median <- dat$rate_low + ((dat$rate_high - dat$rate_low) / 2)

for(i in 1:nrow(dat)){
      rates_high <-dat[i, grep('r.+rate_high', colnames(dat))] 
      rates_low <- dat[i, grep('r.+rate_low', colnames(dat))]
      rates_med <- dat[i, grep('r.+rate_mean', colnames(dat))] 
      #pass_cr1[i] <- dat$rate_mean[i] >= max(rates_high) | dat$rate_mean[i] <= min(rates_low)
      pass_cr1[i] <- rate_median[i] >= max(rates_high) | rate_median[i] <= min(rates_low)
      pass_cr2[i] <- dat$rate_low[i] >= max(rates_high) | dat$rate_high[i] <= min(rates_low)
      pass_cr3[i] <- sum( ((rates_high - rates_low) / rates_med) > ((dat$rate_high[i] - dat$rate_low[i]) / dat$rate_mean[i] )) >= 4
}




dat <- cbind(dat, rate_median, pass_cr1, pass_cr2, pass_cr3, pass_true, cv_rates)

dat <- dat[-which(dat$rate_median > 0.1), ]
dat <- dat[-which((dat$rate_high - dat$rate_low) > 0.002), ]


#plot(dat$cal_time[dat$sd_rate == 0.01], rate_median[dat$sd_rate == 0.01], col = c('red', 'black')[as.numeric(pass_cr1) + 1], ylim = c(0, 0.003))
#points(dat$cal_time[dat$sd_rate == 0.01], dat$rate_high[dat$sd_rate == 0.01], col = c('red', 'black')[as.numeric(pass_cr1) + 1], pch = 2)
#points(dat$cal_time[dat$sd_rate == 0.01], dat$rate_low[dat$sd_rate == 0.01], col = c('red', 'black')[as.numeric(pass_cr1) + 1], pch = 2)

#lines(x = c(0, 30), y = c(0.0001, 0.0001))

d1 <- dat[dat$sd_rate == 0.01, ]

plot_1 <- ggplot(d1, aes(x = cal_time, y = rate_median, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))

d2 <- dat[dat$sd_rate == 0.01, ]

plot_2 <- ggplot(d2, aes(x = cal_time, y = rate_median, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))

plot_3 <- ggplot(d1, aes(x = cal_time, y = rate_median, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))


plot_4 <- ggplot(d1, aes(cal_time, y = rate_median, colour = cv_rates)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))


plot_5 <- ggplot(d2, aes(x = cal_time, y = rate_median, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))


#coord_cartesian(ylim = c(0, 0.003)) + 




