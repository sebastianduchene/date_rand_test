library(ggplot2)
library(gridExtra)

dat <- read.table('compiled_dat_3.txt', as.is = T)
dat <- dat[, 1:42]

colnames(dat)  <- c('run_name', 'cal_time', 'sim_rate', 'sd_rate', 'slope', 'r', 'rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high', paste0('r1_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r2_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r3_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r4_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r5_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')) )

pass_cr1 <- vector()
pass_cr2 <- vector()
pass_cr3 <- vector()
pass_true <- dat$sim_rate < dat$rate_high & dat$sim_rate > dat$rate_low
cv_rates <- (dat$rate_high - dat$rate_low) / dat$rate_mean <= 0.2

rate_median <- dat$rate_low + ((dat$rate_high - dat$rate_low) / 2)

for(i in 1:nrow(dat)){
      rates_high <-dat[i, grep('r.+rate_high', colnames(dat))]
      rates_high <- rates_high[!is.na(rates_high)]
      rates_low <- dat[i, grep('r.+rate_low', colnames(dat))]
      rates_low <- rates_low[!is.na(rates_low)]
      rates_med <- dat[i, grep('r.+rate_mean', colnames(dat))] 
      rates_med <- rates_med[!is.na(rates_med)]

      #pass_cr1[i] <- dat$rate_mean[i] >= max(rates_high) | dat$rate_mean[i] <= min(rates_low)
      pass_cr1[i] <- rate_median[i] >= max(rates_high) | rate_median[i] <= min(rates_low)
      pass_cr2[i] <- dat$rate_low[i] >= max(rates_high) | dat$rate_high[i] <= min(rates_low)
      pass_cr3[i] <- sum( ((rates_high - rates_low) / rates_med) > ((dat$rate_high[i] - dat$rate_low[i]) / dat$rate_mean[i] )) >= 4
}

dat <- cbind(dat, rate_median, pass_cr1, pass_cr2, pass_cr3, pass_true, cv_rates)

#lines(x = c(0, 30), y = c(0.0001, 0.0001))

#dat <- dat[-which(dat$rate_high > 0.1), ]

#d1 <- dat[dat$sd_rate == 0.01 & dat$sim_rate == 0.001, ]
#d1 <- d1[-which(d1$rate_median > 0.01), ]
#d1 <- d1[-which((d1$rate_high - d1$rate_low) > 0.01), ]

#d1$cal_time <- log10(d1$cal_time)
stop('preparing data')



plot_true <- ggplot(d1, aes(x = cal_time, y = rate_median, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001))

plot_cr1 <- ggplot(d1, aes(x = cal_time, y = rate_median, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.001))

plot_cr2 <- ggplot(d1, aes(x = cal_time, y = rate_median, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.001))

plot_cr3 <- ggplot(d1, aes(x = cal_time, y = rate_median, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.001))

pdf('plots_1.pdf', width = 14, height = 7)
grid.arrange(plot_true, plot_cr1, plot_cr2, plot_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.001 and sd=0.01')
dev.off()

d2 <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0.01, ]
d2$cal_time <- log10(d2$cal_time)

plot_true_d2 <- ggplot(d2, aes(x = cal_time, y = rate_median, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01))

plot_cr1_d2 <- ggplot(d2, aes(x = cal_time, y = rate_median, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.01))

plot_cr2_d2 <- ggplot(d2, aes(x = cal_time, y = rate_median, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.01))

plot_cr3_d2 <- ggplot(d2, aes(x = cal_time, y = rate_median, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.01))

pdf('plots_2.pdf', width = 14, height = 7)
grid.arrange(plot_true_d2, plot_cr1_d2, plot_cr2_d2, plot_cr3_d2, ncol = 2, nrow = 2, main = 'Simulations with rate=0.01 and sd=0.01')
dev.off()


d3 <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0.01, ]
d3$cal_time <- log10(d3$cal_time)
d3 <- d3[-which(d3$rate_high > 0.02), ]


plot_true_d3 <- ggplot(d3, aes(x = cal_time, y = rate_median, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001))

plot_cr1_d3 <- ggplot(d3, aes(x = cal_time, y = rate_median, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.0001))

plot_cr2_d3 <- ggplot(d3, aes(x = cal_time, y = rate_median, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.0001))

plot_cr3_d3 <- ggplot(d3, aes(x = cal_time, y = rate_median, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high))+ geom_hline(aes(yintercept = 0.0001))

pdf('plots_3.pdf', width = 14, height = 7)
grid.arrange(plot_true_d3, plot_cr1_d3, plot_cr2_d3, plot_cr3_d3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.0001 and sd=0.01')
dev.off()

