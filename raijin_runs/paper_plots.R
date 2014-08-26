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
      rates_high <- rates_high[!is.na(rates_high)] * 4
      rates_low <- dat[i, grep('r.+rate_low', colnames(dat))] #/ 2
      rates_low <- rates_low[!is.na(rates_low)]
      rates_med <- dat[i, grep('r.+rate_mean', colnames(dat))] 
      rates_med <- rates_med[!is.na(rates_med)]

      pass_cr1[i] <- dat$rate_mean[i] >= max(rates_high) | dat$rate_mean[i] <= min(rates_low)
      #pass_cr1[i] <- rate_median[i] >= max(rates_high) | rate_median[i] <= min(rates_low)
      pass_cr2[i] <- dat$rate_low[i] >= max(rates_high) | dat$rate_high[i] <= min(rates_low)
      pass_cr3[i] <- sum( ((rates_high - rates_low) / rates_med) > ((dat$rate_high[i] - dat$rate_low[i]) / dat$rate_mean[i] )) > 4
}

dat <- cbind(dat, rate_median, pass_cr1, pass_cr2, pass_cr3, pass_true, cv_rates)

dat$rate_mean <- log10(dat$rate_mean)
dat$rate_low <- log10(dat$rate_low)
dat$rate_high <- log10(dat$rate_high)

get_pvs <- function(clock_dat){
	fp_cr1 <- sum((clock_dat$pass_cr1 != clock_dat$pass_true)[clock_dat$pass_cr1])
	fn_cr1 <- sum((clock_dat$pass_cr1 != clock_dat$pass_true)[!clock_dat$pass_cr1])
	tp_cr1 <- sum((clock_dat$pass_cr1 == clock_dat$pass_true)[clock_dat$pass_cr1])
	tn_cr1 <- sum((clock_dat$pass_cr1 == clock_dat$pass_true)[!clock_dat$pass_cr1])
	ppv_cr1 <- tp_cr1 /  (tp_cr1 + fp_cr1)
	npv_cr1 <- tn_cr1 / (tn_cr1 + fn_cr1)

	fp_cr2 <- sum((clock_dat$pass_cr2 != clock_dat$pass_true)[clock_dat$pass_cr2])
	fn_cr2 <- sum((clock_dat$pass_cr2 != clock_dat$pass_true)[!clock_dat$pass_cr2])
	tp_cr2 <- sum((clock_dat$pass_cr2 == clock_dat$pass_true)[clock_dat$pass_cr2])
	tn_cr2 <- sum((clock_dat$pass_cr2 == clock_dat$pass_true)[!clock_dat$pass_cr2])
	ppv_cr2 <- tp_cr2 /  (tp_cr2 + fp_cr2)
	npv_cr2 <- tn_cr2 / (tn_cr2 + fn_cr2)

	return(c(fp_cr1, fn_cr1, fp_cr2, fn_cr2))

#	return(c(ppv_cr1, npv_cr1, ppv_cr2, npv_cr2))
}



d_clock_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0 & dat$pass_true == T, ]
d_clock_high$cal_time <- log10(d_clock_high$cal_time)

#pdf('plots_test_1.pdf', width = 10, height = 7)

plot_clock_high_true <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, fill = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + ylab('') + guides(fill = FALSE) + xlab('') + theme_bw()

plot_clock_high_cr1 <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + xlab('') + ylab('') + annotate('text', x = c(1.25, 1.25), y = c(-2.2, -2.3), label = c(paste('FP = ', get_pvs(d_clock_high)[1]), paste('FN = ', get_pvs(d_clock_high)[2]))  , size = 2) 



plot_clock_high_cr2 <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1.25, 1.25), y = c(-2.2, -2.3), label = c(paste('FP = ', get_pvs(d_clock_high)[3]), paste('FN = ', get_pvs(d_clock_high)[4]))  , size = 2) 



################################
################################
################################

d_clock_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0 , ]
d_clock_med$cal_time <- log10(d_clock_med$cal_time)


plot_clock_med_true <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + ylab(expression(paste(Log[10], ' Rate', ' (subst/site/year)'  ))) + guides(fill = FALSE) + xlab('') + theme_bw() + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) 

plot_clock_med_cr1 <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw()+ylab('') + xlab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_clock_med)[1]), paste('FN = ', get_pvs(d_clock_med)[2]))  , size = 2) 

plot_clock_med_cr2 <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_clock_med)[3]), paste('FN = ', get_pvs(d_clock_med)[4])) , size = 2) 



################################
################################
################################

d_clock_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0 , ]
d_clock_low$cal_time <- log10(d_clock_low$cal_time)


plot_clock_low_true <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + ylab('') + guides(fill = FALSE) + xlab('') + theme_bw() + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) 

plot_clock_low_cr1 <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + xlab(expression(paste(Log[10], ' Calibration', ' (year)' ))) + ylab('')  + annotate('text', x = c(1.2, 1.2), y = c(-3, -3.2), label = c(paste('FP = ', get_pvs(d_clock_low)[1]), paste('FN = ', get_pvs(d_clock_low)[2])) , size = 2) 

plot_clock_low_cr2 <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1.2, 1.2), y = c(-3, -3.2), label = c(paste('FP = ', get_pvs(d_clock_low)[3]), paste('FN = ', get_pvs(d_clock_low)[4])) , size = 2) 

grid.arrange(plot_clock_high_true, plot_clock_high_cr1, plot_clock_high_cr2, plot_clock_med_true, plot_clock_med_cr1, plot_clock_med_cr2, ncol = 3, nrow = 2, main = 'Simulations with strict clock')

#grid.arrange(plot_clock_high_true, plot_clock_high_cr1, plot_clock_high_cr2, plot_clock_med_true, plot_clock_med_cr1, plot_clock_med_cr2,plot_clock_low_true, plot_clock_low_cr1, plot_clock_low_cr2,  ncol = 3, nrow = 3, main = 'Simulations with strict clock')


################################
################################
################################
# LOW RATE VARIATION
################################
################################
################################

d_lvar_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0.01 & dat$pass_true == T, ]
d_lvar_high$cal_time <- log10(d_lvar_high$cal_time)

plot_lvar_high_true <- ggplot(d_lvar_high, aes(x = cal_time, y = rate_mean, fill = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + ylab('') + guides(fill = FALSE) + xlab('') + theme_bw()

plot_lvar_high_cr1 <- ggplot(d_lvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + xlab('') + ylab('')  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_lvar_high)[1]), paste('FN = ', get_pvs(d_lvar_high)[2]))  , size = 2) 

plot_lvar_high_cr2 <- ggplot(d_lvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('')  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_lvar_high)[3]), paste('FN = ', get_pvs(d_lvar_high)[4]))  , size = 2) 


################################
################################
################################

d_lvar_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0.01 , ]
d_lvar_med$cal_time <- log10(d_lvar_med$cal_time)


plot_lvar_med_true <- ggplot(d_lvar_med, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + ylab(expression(paste(Log[10], ' Rate', ' (subst/site/year)'  ))) + guides(fill = FALSE) + xlab('') + theme_bw() + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) 

plot_lvar_med_cr1 <- ggplot(d_lvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw()+ylab('') + xlab('')  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_lvar_med)[1]), paste('FN = ', get_pvs(d_lvar_med)[2]))  , size = 2) 

plot_lvar_med_cr2 <- ggplot(d_lvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('')  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_lvar_med)[3]), paste('FN = ', get_pvs(d_lvar_med)[4]))  , size = 2) 

################################
################################
################################

d_lvar_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0.01 , ]
d_lvar_low$cal_time <- log10(d_lvar_low$cal_time)


plot_lvar_low_true <- ggplot(d_lvar_low, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + ylab('') + guides(fill = FALSE) + xlab('') + theme_bw() + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) 

plot_lvar_low_cr1 <- ggplot(d_lvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + xlab(expression(paste(Log[10], ' Calibration', ' (year)' ))) + ylab('')  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_lvar_low)[1]), paste('FN = ', get_pvs(d_lvar_low)[2]))  , size = 2) 

plot_lvar_low_cr2 <- ggplot(d_lvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('')  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_lvar_low)[3]), paste('FN = ', get_pvs(d_lvar_low)[4]))  , size = 2) 


#grid.arrange(plot_lvar_high_true, plot_lvar_high_cr1, plot_lvar_high_cr2, plot_lvar_med_true, plot_lvar_med_cr1, plot_lvar_med_cr2,plot_lvar_low_true, plot_lvar_low_cr1, plot_lvar_low_cr2,  ncol = 3, nrow = 3, main = 'Simulations with low rate variation')



################################
################################
################################
# HIGH RATE VARIATION
################################
################################
################################

d_hvar_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0.1 & dat$pass_true == T, ]
d_hvar_high$cal_time <- log10(d_hvar_high$cal_time)

plot_hvar_high_true <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, fill = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + ylab('') + guides(fill = FALSE) + xlab('') + theme_bw()

plot_hvar_high_cr1 <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + xlab('') + ylab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_hvar_high)[1]), paste('FN = ', get_pvs(d_hvar_high)[2]))  , size = 2) 

plot_hvar_high_cr2 <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_hvar_high)[3]), paste('FN = ', get_pvs(d_hvar_high)[4]))  , size = 2) 


################################
################################
################################

d_hvar_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0.1 , ]
d_hvar_med$cal_time <- log10(d_hvar_med$cal_time)


plot_hvar_med_true <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + ylab(expression(paste(Log[10], ' Rate', ' (subst/site/year)'  ))) + guides(fill = FALSE) + xlab('') + theme_bw() + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) 

plot_hvar_med_cr1 <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw()+ylab('') + xlab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_hvar_med)[1]), paste('FN = ', get_pvs(d_hvar_med)[2]))  , size = 2) 

plot_hvar_med_cr2 <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_hvar_med)[3]), paste('FN = ', get_pvs(d_hvar_med)[4]))  , size = 2) 

################################
################################
################################

d_hvar_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0.1 , ]
d_hvar_low$cal_time <- log10(d_hvar_low$cal_time)


plot_hvar_low_true <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_true)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + ylab('') + guides(fill = FALSE) + xlab('') + theme_bw() + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) 

plot_hvar_low_cr1 <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr1)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + xlab(expression(paste(Log[10], ' Rate', ' (subst/site/year)' ))) + ylab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_hvar_low)[1]), paste('FN = ', get_pvs(d_hvar_low)[2]))  , size = 2) 

plot_hvar_low_cr2 <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point() + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE) + scale_colour_manual(values = c('red', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('FP = ', get_pvs(d_hvar_low)[3]), paste('FN = ', get_pvs(d_hvar_low)[4]))  , size = 2) 


grid.arrange(plot_hvar_high_true, plot_hvar_high_cr1, plot_hvar_high_cr2, plot_hvar_med_true, plot_hvar_med_cr1, plot_hvar_med_cr2, ncol = 3, nrow = 2, main = 'Simulations with high rate variation')

#grid.arrange(plot_hvar_high_true, plot_hvar_high_cr1, plot_hvar_high_cr2, plot_hvar_med_true, plot_hvar_med_cr1, plot_hvar_med_cr2,plot_hvar_low_true, plot_hvar_low_cr1, plot_hvar_low_cr2,  ncol = 3, nrow = 3, main = 'Simulations with high rate variation')

#dev.off()
