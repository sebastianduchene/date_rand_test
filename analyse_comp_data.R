

dat <- read.table('comp_II_III.txt', as.is = T)



colnames(dat) <- c("run.name","cal.time","mean.rate","sd.rate","slope","r.sq","real_ucld","real_low_ucld","real_high_ucld","real_root","real_low_root","real_high_root","r1_ucld","r1_low_ucld","r1_high_ucld","r1_root","r1_low_root","r1_high_root","r2_ucld","r2_low_ucld","r2_high_ucld","r2_root","r2_low_root","r2_high_root","r3_ucld","r3_low_ucld","r3_high_ucld","r3_root","r3_low_root","r3_high_root")




median_rate <- vector()
median_root <- vector()

pass_cr1 <- vector()
pass_cr2 <- vector()
pass_cr3 <- vector()
pass_cr4 <- vector()
rate_in <- vector()

dat$mean.rate <- dat$mean.rate 
for(i in 1:nrow(dat)){

      low_rates <- min(unlist(c(dat[i, grep('r._low_ucld', colnames(dat))])))
      high_rates <- max(unlist(c(dat[i, grep('r._high_ucld', colnames(dat))])))

      low_root <- min(unlist(c(dat[i, grep('r._low_root', colnames(dat))])))
      high_root <- max(unlist(c(dat[i, grep('r._high_root', colnames(dat))])))
      
      median_rate[i] <- dat$real_low_ucld[i] + (dat$real_high_ucld[i] - dat$real_low_ucld[i] )
      median_root[i] <- dat$real_low_root[i] + (dat$real_high_root[i] - dat$real_low_root[i] )

      pass_cr1[i] <- (dat$real_ucld[i] > high_rates) || (dat$real_ucld[i] < low_rates)
      pass_cr2[i] <- (median_rate[i] > high_rates) || (median_rate[i] < low_rates)
      pass_cr3[i] <- all(c(dat$real_low_ucld , dat$real_high_ucld) > high_rates) || all(c(dat$real_high_ucld, dat$real_low_ucld)  < low_rates)
      
      pass_cr4[i] <- (median_root[i] > high_root) || (median_root[i] < low_root)

      rate_in[i] <- (dat$mean.rate[i] <= dat$real_high_ucld[i]) && (dat$mean.rate[i] >= dat$real_low_ucld[i])

}



# chek standardising
rate_dif <- (dat$real_ucld )#- dat$mean.rate) / dat$mean.rate
root_dif <- (median_root )#- 100) / 100

dat <- cbind(dat, median_rate, rate_dif, median_root, root_dif, pass_cr1, pass_cr2, pass_cr4, rate_in)

dat <- dat[-(which(dat$median_rate > 1)), ]

#par(bg = 'black')
#par(col = 'white')
#par(col.axis = 'white')
#par(col.lab = 'white')

par(mfrow = c(2, 2))
par(mar = c(4, 4, 0.5, 0.5))

if(F){

cal_time_factor <- as.numeric(cut(dat$cal.time, 3))

plot(jitter(cal_time_factor)[dat$mean.rate == 0.01], dat$rate_dif[dat$mean.rate == 0.01], ylim = c(-20, 60), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = '', xlab = '', xaxt = 'n', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0, 0))
text(x = 1, y = 60, labels = 'rate = 0.01', col = 'white')

plot(jitter(cal_time_factor)[dat$mean.rate == 0.01], dat$root_dif[dat$mean.rate == 0.01], ylim = c(-2, 2), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = '', xlab = '', xaxt = 'n', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0, 0))

plot(jitter(cal_time_factor)[dat$mean.rate == 0.0001], dat$rate_dif[dat$mean.rate == 0.0001], ylim = c(-20, 60), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = 'Error in rate', xlab = 'Calibration time', xaxt = 'n', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0, 0))
axis(1, at = c(1, 2, 3), labels = c(expression(italic(t)<10), expression(paste('10 < ', italic(t), ' > 15')), expression(italic(t)>15)), col = 'white')
text(x = 1, y = 60, labels = 'rate = 0.0001')

plot(jitter(cal_time_factor)[dat$mean.rate == 0.0001], dat$root_dif[dat$mean.rate == 0.0001], ylim = c(-2, 2), col = c('blue', 'red')[as.numeric(dat$pass_cr2)+1], ylab = 'Error in root', xlab = 'Calibration time', xaxt = 'n', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0, 0))
axis(1, at = c(1, 2, 3), labels = c(expression(italic(t)<10), expression(paste('10 < ', italic(t), ' > 15')), expression(italic(t)>15)), col = 'white')

# ! Increase randomisations to 5. increase calibration depth to between 20, 39
stop('Completed plots')


}


# PLOTS FOR ERROR IN THE RATE AND THE ROOT AS A FUNCTION OF THE CALIBRATION TIME
if(T){
plot(dat$cal.time[dat$mean.rate == 0.01], dat$rate_dif[dat$mean.rate == 0.01], ylim = c(0, 0.2), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = '', xlab = '', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0.01, 0.01))
text(x = 5, y = 0.17, labels = 'Error in the rate for a\n simulated rate of  = 0.01', col = 'white')

plot(dat$cal.time[dat$mean.rate == 0.0001], dat$rate_dif[dat$mean.rate == 0.0001], ylim = c(0, 0.02), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = '', xlab = '', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0.0001, 0.0001))
text(x = 5, y = 0.015, labels = 'Error in the rate for a\n simulated rate of  = 0.0001', col = 'white')

plot(dat$cal.time[dat$mean.rate == 0.01], dat$root_dif[dat$mean.rate == 0.01], ylim = c(0, 200), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = '', xlab = '', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(100, 100))
text(x = 5, y = 175, labels = 'Error in the root for a\n simulated rate of  = 0.01', col = 'white')

plot(dat$cal.time[dat$mean.rate == 0.0001], dat$root_dif[dat$mean.rate == 0.0001], ylim = c(0, 200), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = '', xlab = '', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(100, 100))
text(x = 5, y = 175, labels = 'Error in the root for a\n simulated rate of  = 0.0001', col = 'white')

stop('wow')
}

plot(jitter(cal_time_factor)[dat$mean.rate == 0.01], dat$root_dif[dat$mean.rate == 0.01], ylim = c(-2, 2), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = '', xlab = '', xaxt = 'n', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0, 0))

plot(jitter(cal_time_factor)[dat$mean.rate == 0.0001], dat$rate_dif[dat$mean.rate == 0.0001], ylim = c(-20, 60), col = c('blue', 'red')[as.numeric(dat$pass_cr2) + 1], ylab = 'Error in rate', xlab = 'Calibration time', xaxt = 'n', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0, 0))
axis(1, at = c(1, 2, 3), labels = c(expression(italic(t)<10), expression(paste('10 < ', italic(t), ' > 15')), expression(italic(t)>15)), col = 'white')
text(x = 1, y = 60, labels = 'rate = 0.0001')

plot(jitter(cal_time_factor)[dat$mean.rate == 0.0001], dat$root_dif[dat$mean.rate == 0.0001], ylim = c(-2, 2), col = c('blue', 'red')[as.numeric(dat$pass_cr2)+1], ylab = 'Error in root', xlab = 'Calibration time', xaxt = 'n', pch = 20, cex = 2)
lines(x = c(-1, 80), y = c(0, 0))
axis(1, at = c(1, 2, 3), labels = c(expression(italic(t)<10), expression(paste('10 < ', italic(t), ' > 15')), expression(italic(t)>15)), col = 'black')

# ! Increase randomisations to 5. increase calibration depth to between 20, 39
stop('Completed plots')












