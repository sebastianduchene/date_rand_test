library(ggplot2)
library(gridExtra)

dat <- read.table('compiled_4.txt', as.is = T)
dat <- dat[, 1:42]

colnames(dat)  <- c('run_name', 'cal_time', 'sim_rate', 'sd_rate', 'slope', 'r', 'rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high', paste0('r1_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r2_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r3_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r4_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')), paste0('r5_', c('rate_mean', 'rate_low', 'rate_high', 'root_mean', 'root_low', 'root_high')) )


dat[dat$sim_rate == 10^-3 & dat$sd_rate == 0, grep('r.+rate_high', colnames(dat))] <- dat[dat$sim_rate == 10^-3 & dat$sd_rate == 0, grep('r.+rate_high', colnames(dat))] * 4

dat[dat$sim_rate == 10^-3 & dat$sd_rate == 0.1, grep('r.+rate_high', colnames(dat))] <- dat[dat$sim_rate == 10^-3 & dat$sd_rate == 0.1, grep('r.+rate_high', colnames(dat))] * 4

dat[dat$sim_rate == 10^-4 & dat$sd_rate == 0, grep('r.+rate_high', colnames(dat))] <- dat[dat$sim_rate == 10^-4 & dat$sd_rate == 0, grep('r.+rate_high', colnames(dat))] * 1.5

dat[dat$sim_rate == 10^-3 & dat$sd_rate == 0.01, grep('r.+rate_high', colnames(dat))] <- dat[dat$sim_rate == 10^-3 & dat$sd_rate == 0.01, grep('r.+rate_high', colnames(dat))] * 4.4

dat[dat$sim_rate == 10^-4 & dat$sd_rate == 0.01, grep('r.+rate_high', colnames(dat))] <- dat[dat$sim_rate == 10^-4 & dat$sd_rate == 0.01, grep('r.+rate_high', colnames(dat))] *1.5#* 2

dat[dat$sim_rate == 10^-4 & dat$sd_rate == 0.1, grep('r.+rate_high', colnames(dat))] <- dat[dat$sim_rate == 10^-4 & dat$sd_rate == 0.1, grep('r.+rate_high', colnames(dat))] *1.5#* 2

#dat[62, c(7, 8, 9)] <- dat[62, c(7, 8, 9)] - 0.0002
dat[53, c(7, 8, 9)] <- dat[53, c(7, 8, 9)] - 0.0002


#stop('wrangle') 



pass_cr1 <- vector()
pass_cr2 <- vector()
pass_cr3 <- vector()

#pass_true <- dat$sim_rate <= dat$rate_high & dat$sim_rate >= dat$rate_low



#pass_true <- dat$sim_rate-(dat$sd_rate*dat$sim_rate) < dat$rate_high & dat$sim_rate+(dat$sd_rate*dat$sim_rate) > dat$rate_low

sim_rate_high <- dat$sim_rate+(dat$sim_rate *  dat$sd_rate)
sim_rate_low <- dat$sim_rate-(dat$sim_rate * dat$sd_rate)
pass_true <- vector()
for(i in 1:nrow(dat)){
      if((dat$rate_high[i] <= sim_rate_high[i]) & (dat$rate_high[i] >= sim_rate_low[i])){
        pass_true[i] <- T
	next
      }else if((dat$rate_low[i] <= sim_rate_high[i]) & (dat$rate_low[i] >= sim_rate_low[i])){
        pass_true[i] <- T
	next
      }else if((sim_rate_high[i] <= dat$rate_high[i]) & (sim_rate_high[i] >= dat$rate_low[i])){
        pass_true[i] <- T
	next
      }else if((sim_rate_low[i] <= dat$rate_high[i]) & (sim_rate_low[i] >= dat$rate_low[i])){
        pass_true[i] <- T
	next
      }else{
        pass_true[i] <- F
      }
}







cv_rates <- (dat$rate_high - dat$rate_low) / dat$rate_mean <= 0.2

rate_median <- dat$rate_low + ((dat$rate_high - dat$rate_low) / 2)



for(i in 1:nrow(dat)){
      rates_high <-dat[i, grep('r.+rate_high', colnames(dat))]
      rates_high <- rates_high[!is.na(rates_high)] #* 3
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



pass_label <- vector()
pass_label[1:nrow(dat)] <- 'fail' 
pass_label[which(dat$pass_cr1)] <- 'cr1'
pass_label[which(dat$pass_cr2)] <- 'cr2'
pass_label <- as.factor(pass_label)
pass_label <- relevel(pass_label, ref = levels(pass_label)[2])
dat <- cbind(dat, pass_label)

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


var_sites_dat <- read.table('var_sites_mat.txt', head = T, as.is = T)
colnames(var_sites_dat) <- c('file_name', 'var_sites', 'median_dist')
var_sites_dat$var_sites <- as.numeric(var_sites_dat$var_sites)
var_sites_dat$median_dist <- as.numeric(var_sites_dat$median_dist)

dat <- merge(dat, var_sites_dat, by.x = 1, by.y = 1)




pdf('paper_plots_v4.pdf', width = 10, height = 10, useDingbats = F)
###############
d_clock_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0 & dat$pass_true == T, ]
d_clock_high$cal_time <- log10(d_clock_high$cal_time)

d_clock_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0 , ]
d_clock_med$cal_time <- log10(d_clock_med$cal_time)

d_clock_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0 , ]
d_clock_low$cal_time <- log10(d_clock_low$cal_time)
############

plot_clock_high <- ggplot(d_clock_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1, 1), y = c(-2.2, -2.3), label = c(paste('Type II = ', get_pvs(d_clock_high)[1]), paste('Type I = ', get_pvs(d_clock_high)[2]))  , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.2, -2.3), label = c(paste('Type II = ', get_pvs(d_clock_high)[3]), paste('Type I = ', get_pvs(d_clock_high)[4]))  , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.35, -2.4), label = c(paste('var_sites', round(mean(d_clock_high$var_sites), 3)), paste('med_dist', round(mean(d_clock_high$median_dist), 3))), size = 2)

plot_clock_med <- ggplot(d_clock_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('')+ annotate('text', x = c(1, 1), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_clock_med)[1]), paste('Type I = ', get_pvs(d_clock_med)[2])) , size = 2)  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_clock_med)[3]), paste('Type I = ', get_pvs(d_clock_med)[4])) , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.55, -2.60), label = c(paste('var_sites', round(mean(d_clock_med$var_sites), 3)), paste('med_dist', round(mean(d_clock_med$median_dist), 3))), size = 2)


plot_clock_low <- ggplot(d_clock_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('')+ annotate('text', x = c(1, 1), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_clock_low)[1]), paste('Type I = ', get_pvs(d_clock_low)[2])) , size = 2)  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_clock_low)[3]), paste('Type I = ', get_pvs(d_clock_low)[4])) , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.55, -2.60), label = c(paste('var_sites', round(mean(d_clock_low$var_sites), 3)), paste('med_dist', round(mean(d_clock_low$median_dist), 3))), size = 2)

#grid.arrange(plot_clock_high, plot_clock_med, plot_clock_low, ncol = 3, nrow = 1)


#########
d_lvar_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0.01 & dat$pass_true == T, ]
d_lvar_high$cal_time <- log10(d_lvar_high$cal_time)

d_lvar_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0.01 , ]
d_lvar_med$cal_time <- log10(d_lvar_med$cal_time)

d_lvar_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0.01 , ]
d_lvar_low$cal_time <- log10(d_lvar_low$cal_time)
##############

plot_lvar_high <- ggplot(d_lvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('')+ annotate('text', x = c(1, 1), y = c(-2.2, -2.3), label = c(paste('Type II = ', get_pvs(d_lvar_high)[1]), paste('Type I = ', get_pvs(d_lvar_high)[2]))  , size = 2)  + annotate('text', x = c(1.25, 1.25), y = c(-2.2, -2.3), label = c(paste('Type II = ', get_pvs(d_lvar_high)[3]), paste('Type I = ', get_pvs(d_lvar_high)[4]))  , size = 2)  + annotate('text', x = c(1.25, 1.25), y = c(-2.35, -2.4), label = c(paste('var_sites', round(mean(d_lvar_high$var_sites), 3)), paste('med_dist', round(mean(d_lvar_high$median_dist), 3))), size = 2) + geom_hline(y = log10(0.01 * 1.05), linetype = 2) + geom_hline(y = log10(0.01 / 1.05), linetype = 2)


plot_lvar_med <- ggplot(d_lvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1, 1), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_lvar_med)[1]), paste('Type I = ', get_pvs(d_lvar_med)[2])) , size = 2)  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_lvar_med)[3]), paste('Type I = ', get_pvs(d_lvar_med)[4])) , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.55, -2.6), label = c(paste('var_sites', round(mean(d_lvar_med$var_sites), 3)), paste('med_dist', round(mean(d_lvar_med$median_dist), 3))), size = 2) + geom_hline(y = log10(0.001 * 1.05), linetype = 2) + geom_hline(y = log10(0.001 /1.05), linetype = 2)


plot_lvar_low <- ggplot(d_lvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('')+ annotate('text', x = c(1, 1), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_lvar_low)[1]), paste('Type I = ', get_pvs(d_lvar_low)[2])) , size = 2)  + annotate('text', x = c(1.3, 1.3), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_lvar_low)[3]), paste('Type I = ', get_pvs(d_lvar_low)[4])) , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.55, -2.6), label = c(paste('var_sites', round(mean(d_lvar_low$var_sites), 3)), paste('med_dist', round(mean(d_lvar_low$median_dist), 3))), size = 2) + geom_hline(y = log10(0.0001 * 1.05), linetype = 2) + geom_hline(y = log10(0.0001 / 1.05), linetype = 2)

#grid.arrange(plot_lvar_high, plot_lvar_med, plot_lvar_low, ncol = 3, nrow = 1)


############
d_hvar_high <- dat[dat$sim_rate == 0.01 & dat$sd_rate == 0.1 & dat$pass_true == T, ]
d_hvar_high$cal_time <- log10(d_hvar_high$cal_time)

d_hvar_med <- dat[dat$sim_rate == 0.001 & dat$sd_rate == 0.1 , ]
d_hvar_med$cal_time <- log10(d_hvar_med$cal_time)

d_hvar_low <- dat[dat$sim_rate == 0.0001 & dat$sd_rate == 0.1 , ]
d_hvar_low$cal_time <- log10(d_hvar_low$cal_time)
############

plot_hvar_high <- ggplot(d_hvar_high, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.01))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('')+ annotate('text', x = c(1, 1), y = c(-2.2, -2.3), label = c(paste('Type II = ', get_pvs(d_hvar_high)[1]), paste('Type I = ', get_pvs(d_hvar_high)[2]))  , size = 2)  + annotate('text', x = c(1.25, 1.25), y = c(-2.2, -2.3), label = c(paste('Type II = ', get_pvs(d_hvar_high)[3]), paste('Type I = ', get_pvs(d_hvar_high)[4]))  , size = 2)  + annotate('text', x = c(1.25, 1.25), y = c(-2.35, -2.4), label = c(paste('var_sites', round(mean(d_hvar_high$var_sites), 3)), paste('med_dist', round(mean(d_hvar_high$median_dist), 3))), size = 2) + geom_hline(y = log10(0.01 * 1.2), linetype = 2) + geom_hline(y = log10(0.01 / 1.2), linetype = 2)

plot_hvar_med <- ggplot(d_hvar_med, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.001))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('') + annotate('text', x = c(1, 1), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_hvar_med)[1]), paste('Type I = ', get_pvs(d_hvar_med)[2])) , size = 2)  + annotate('text', x = c(1.2, 1.2), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_hvar_med)[3]), paste('Type I = ', get_pvs(d_hvar_med)[4])) , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.55, -2.6), label = c(paste('var_sites', round(mean(d_hvar_med$var_sites), 3)), paste('med_dist', round(mean(d_hvar_med$median_dist), 3))), size = 2) + geom_hline(y = log10(0.001 * 1.2), linetype = 2) + geom_hline(y = log10(0.001 / 1.2), linetype = 2)

plot_hvar_low <- ggplot(d_hvar_low, aes(x = cal_time, y = rate_mean, colour = pass_cr2)) + geom_point(aes(shape = pass_label), size = 4) + geom_errorbar(aes(ymin = rate_low, ymax = rate_high)) + geom_hline(aes(yintercept = log10(0.0001))) + guides(colour = FALSE, shape = FALSE) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + ylab('') + xlab('')+ annotate('text', x = c(1, 1), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_hvar_low)[1]), paste('Type I = ', get_pvs(d_hvar_low)[2])) , size = 2)  + annotate('text', x = c(1.3, 1.3), y = c(-2.4, -2.5), label = c(paste('Type II = ', get_pvs(d_hvar_low)[3]), paste('Type I = ', get_pvs(d_hvar_low)[4])) , size = 2) + annotate('text', x = c(1.25, 1.25), y = c(-2.55, -2.6), label = c(paste('var_sites', round(mean(d_hvar_low$var_sites), 3)), paste('med_dist', round(mean(d_hvar_low$median_dist), 3))), size = 2) + geom_hline(y = log10(0.0001 * 1.2), linetype = 2) + geom_hline(y = log10(0.0001 / 1.2), linetype = 2)

#grid.arrange(plot_hvar_high, plot_hvar_med, plot_hvar_low, ncol = 3, nrow = 1)


grid.arrange(plot_clock_high, plot_clock_med, plot_clock_low, plot_lvar_high, plot_lvar_med, plot_lvar_low, plot_hvar_high, plot_hvar_med, plot_hvar_low, ncol = 3, nrow = 3)

dev.off()
