dat <- read.table('compiled_data_RUN_II.txt', as.is = T)

colnames(dat) <- c("run.name","cal.time","mean.rate","sd.rate","slope","r.sq","real_ucld","real_low_ucld","real_high_ucld","real_root","real_low_root","real_high_root","r1_ucld","r1_low_ucld","r1_high_ucld","r1_root","r1_low_root","r1_high_root","r2_ucld","r2_low_ucld","r2_high_ucld","r2_root","r2_low_root","r2_high_root","r3_ucld","r3_low_ucld","r3_high_ucld","r3_root","r3_low_root","r3_high_root")


median_rate <- vector()
median_root <- vector()

pass_cr1 <- vector()
pass_cr2 <- vector()
pass_cr3 <- vector()
rate_in <- vector()

dat$mean.rate <- dat$mean.rate 
for(i in 1:nrow(dat)){

      low_rates <- min(unlist(c(dat[i, grep('r._low_ucld', colnames(dat))])))
      high_rates <- max(unlist(c(dat[i, grep('r._high_ucld', colnames(dat))])))
      
      median_rate[i] <- dat$real_low_ucld[i] + (dat$real_high_ucld[i] - dat$real_low_ucld[i] )
      median_root[i] <- dat$real_low_root[i] + (dat$real_high_root[i] - dat$real_low_root[i] )

      pass_cr1[i] <- (dat$real_ucld[i] > high_rates) || (dat$real_ucld[i] < low_rates)
      pass_cr2[i] <- (median_rate[i] > high_rates) || (median_rate[i] < low_rates)
      pass_cr3[i] <- all(c(dat$real_low_ucld , dat$real_high_ucld) > high_rates) || all(c(dat$real_high_ucld, dat$real_low_ucld)  < low_rates)

      rate_in[i] <- (dat$mean.rate[i] <= dat$real_high_ucld[i]) && (dat$mean.rate[i] >= dat$real_low_ucld[i])

}


# chek standardising
rate_dif <- (dat$real_ucld - dat$mean.rate) / dat$mean.rate
root_dif <- (median_root - 100) 

dat <- cbind(dat, median_rate, rate_dif, median_root, root_dif, pass_cr1, pass_cr2, rate_in)

#dat <- dat[dat_comp$rate_dif < 50, ]

dat <- dat[-(which(dat$median_rate > 1)), ]

par(bg = 'black')
par(col = 'white')
par(col.axis = 'white')
par(col.lab = 'white')

plot(dat$rate_dif, dat$root_dif, pch = 20, col = c('red', 'green')[as.numeric(dat$pass_cr1) + 1])

plot(dat$median_rate, dat$root_dif, pch = 20, col = c('red', 'green')[as.numeric(dat$pass_cr1) + 1])

# ? relationship between rate dif and root diff. The y interept it the rate scaling in beast

# What is the error in those that pass it
par(mfrow = c(1, 2))

cal_range <- as.numeric(factor(dat$cal.time < 10))

plot(jitter(as.numeric(dat$pass_cr2)), dat$rate_dif, pch = cal_range, col = c('lightblue', 'orange')[as.numeric(factor(dat$mean.rate))] , ylim = c(-1, 50))
plot(jitter(as.numeric(dat$pass_cr1)), dat$rate_dif, pch = cal_range, col = c('lightblue', 'orange')[as.numeric(factor(dat$mean.rate))] , ylim = c(-1, 50))


plot(jitter(as.numeric(dat$pass_cr2)), dat$root_dif, pch = cal_range, col = c('lightblue', 'orange')[as.numeric(factor(dat$mean.rate))])#, ylim = c(-2, 2))
plot(jitter(as.numeric(dat$pass_cr1)), dat$root_dif, pch = cal_range, col = c('lightblue', 'orange')[as.numeric(factor(dat$mean.rate))]) #, ylim = c(-2, 2))

## !! THE TEST HAS HIGH FALSE POSITIVES AND FALSE NEGATIVES. TEST WITH TREES WITH DIFFERENT STRUCTURES