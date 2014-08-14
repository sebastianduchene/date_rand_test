log_file <- grep('log', dir(), value = T)

for(i in 1:length(log_file)){
 dat <- read.table(log_file[i], head = T)
 rate_est <- mean(as.numeric(dat$rate.mean))
 rate_hpd <- quantile(as.numeric(dat$rate.mean), c(0.025, 0.975))
 root_est <- mean(as.numeric(dat$TreeHeight))
 root_hpd <- quantile(as.numeric(dat$TreeHeight), c(0.025, 0.975))
 cat(paste(log_file[i], rate_est, paste(rate_hpd, collapse = ' '), root_est, paste(root_hpd, collapse = ' '), collapse = ' '), file = 'res_runs.txt', append = T, sep = '\n')

}