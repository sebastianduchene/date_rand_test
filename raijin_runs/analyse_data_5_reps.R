library(ggplot2)
library(gridExtra)

dat <- read.table('compiled_4.txt', as.is = T)
dat <- dat[, 1:42]

colnames(dat)  <- c('run_name', 'cal_time', 'sim_rate', 'sd_rate', 'slope', 'r', 'rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high', paste0('r1_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r2_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r3_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r4_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r5_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')) )


#dat$rate_high  <- dat$rate_high / 10

#dat$rate_low <- dat$rate_low / 10

pass_cr1 <- vector()
pass_cr2 <- vector()
pass_cr3 <- vector()
pass_true <- dat$sim_rate < dat$rate_high & dat$sim_rate > dat$rate_low
cv_rates <- (dat$rate_high - dat$rate_low) / dat$rate_mean <= 0.2

rate_median <- dat$rate_low + ((dat$rate_high - dat$rate_low) / 2)

for(i in 1:nrow(dat)){
      rates_high <-dat[i, grep('r.+rate_high', colnames(dat))]
      rates_high <- rates_high[!is.na(rates_high)][1:3]
      rates_low <- dat[i, grep('r.+rate_low', colnames(dat))]
      rates_low <- rates_low[!is.na(rates_low)][1:3]
      rates_med <- dat[i, grep('r.+rate_mean', colnames(dat))] 
      rates_med <- rates_med[!is.na(rates_med)]

      pass_cr1[i] <- dat$rate_mean[i] >= max(rates_high) | dat$rate_mean[i] <= min(rates_low)
      #pass_cr1[i] <- rate_median[i] >= max(rates_high) | rate_median[i] <= min(rates_low)
      pass_cr2[i] <- dat$rate_low[i] >= max(rates_high) | dat$rate_high[i] <= min(rates_low)
      pass_cr3[i] <- sum( ((rates_high - rates_low) / rates_med) > ((dat$rate_high[i] - dat$rate_low[i]) / dat$rate_mean[i] )) > 4
}

dat <- cbind(dat, rate_median, pass_cr1, pass_cr2, pass_cr3, pass_true, cv_rates)


#dat <- dat[-which(dat$rate_high > 0.1), ]

#d1 <- dat[dat$sd_rate == 0.01 & dat$sim_rate == 0.001, ]
#d1 <- d1[-which(d1$rate_median > 0.01), ]
#d1 <- d1[-which((d1$rate_high - d1$rate_low) > 0.01), ]

#d1$cal_time <- log10(d1$cal_time)

#dat <- dat[-which(dat$rate_high > 0.4), ]

##plot clocklike data for high rate

#pdf('clock.pdf')

d_clock_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0, ]

plot_clock_high_true <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_clock_high_cr1 <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_clock_high_cr2 <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_clock_high_cr3 <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

grid.arrange(plot_clock_high_true, plot_clock_high_cr1, plot_clock_high_cr2, plot_clock_high_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.01 and 0 rate variation')

stop('clock high rate')


## plot clocklike data for medium rate

d_clock_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0, ]

plot_clock_med_true <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_clock_med_cr1 <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_clock_med_cr2 <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_clock_med_cr3 <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

grid.arrange(plot_clock_med_true, plot_clock_med_cr1, plot_clock_med_cr2, plot_clock_med_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.001 and 0 rate variation')


## plot clocklike data for low rate

d_clock_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0, ]

plot_clock_low_true <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_clock_low_cr1 <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_clock_low_cr2 <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_clock_low_cr3 <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

grid.arrange(plot_clock_low_true, plot_clock_low_cr1, plot_clock_low_cr2, plot_clock_low_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.0001 and 0 rate variation')
#dev.off()

######################################
######################################

##plot low variation data for high rate
#pdf('low_var.pdf')


d_1_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0.01, ]

plot_1_high_true <- ggplot(d_1_high, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_1_high_cr1 <- ggplot(d_1_high, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_1_high_cr2 <- ggplot(d_1_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_1_high_cr3 <- ggplot(d_1_high, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

grid.arrange(plot_1_high_true, plot_1_high_cr1, plot_1_high_cr2, plot_1_high_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.01 and low rate variation')

## plot 1like data for medium rate

d_1_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0.01, ]

plot_1_med_true <- ggplot(d_1_med, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_1_med_cr1 <- ggplot(d_1_med, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_1_med_cr2 <- ggplot(d_1_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_1_med_cr3 <- ggplot(d_1_med, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

grid.arrange(plot_1_med_true, plot_1_med_cr1, plot_1_med_cr2, plot_1_med_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.001 and low rate variation')


## plot 1like data for low rate

d_1_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0.01, ]

plot_1_low_true <- ggplot(d_1_low, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_1_low_cr1 <- ggplot(d_1_low, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_1_low_cr2 <- ggplot(d_1_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_1_low_cr3 <- ggplot(d_1_low, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))


grid.arrange(plot_1_low_true, plot_1_low_cr1, plot_1_low_cr2, plot_1_low_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.0001 and low rate variation')



######################################
######################################

##plot high variation data for high rate


d_hvar_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0.1, ]

plot_hvar_high_true <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_hvar_high_cr1 <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_hvar_high_cr2 <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

plot_hvar_high_cr3 <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.01)) #+ coord_cartesian(ylim = c(0, 0.5))

grid.arrange(plot_hvar_high_true, plot_hvar_high_cr1, plot_hvar_high_cr2, plot_hvar_high_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.01 and high rate variation')


## plot 1like data for medium rate

d_hvar_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0.1, ]

plot_hvar_med_true <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_hvar_med_cr1 <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_hvar_med_cr2 <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

plot_hvar_med_cr3 <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.001)) #+ coord_cartesian(ylim = c(0, 0.05))

grid.arrange(plot_hvar_med_true, plot_hvar_med_cr1, plot_hvar_med_cr2, plot_hvar_med_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.001 and high rate variation')


## plot 1like data for low rate

d_hvar_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0.1, ]

plot_hvar_low_true <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_hvar_low_cr1 <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_hvar_low_cr2 <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

plot_hvar_low_cr3 <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr3)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = 0.0001)) #+ coord_cartesian(ylim = c(0, 0.025))

grid.arrange(plot_hvar_low_true, plot_hvar_low_cr1, plot_hvar_low_cr2, plot_hvar_low_cr3, ncol = 2, nrow = 2, main = 'Simulations with rate=0.0001 and high rate variation')


stop('high var low rate')



#plot(d_clock_low$cal_time, d_clock_low$rate_mean, ylim = c(0, 0.05))
#points(d_clock_low$cal_time, d_clock_low$rate_high, col = 'red')
#points(d_clock_low$cal_time, d_clock_low$rate_low, col = 'red')




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

