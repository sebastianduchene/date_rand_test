library(ggplot2)

true_rate <- log10(as.numeric(read.table('true_dat.log', head = T, as.is = T)$rate.mean)[-c(1:2000)])

rand_1 <- log10(as.numeric(read.table('rand_dat.log', head = T, as.is = T)$rate.mean)[-c(1:2000)])
rand_2 <- log10(as.numeric(read.table('rand_dat_2.log', head = T, as.is = T)$rate.mean)[-c(1:2000)])
rand_3 <- log10(as.numeric(read.table('rand_dat_3.log', head = T, as.is = T)$rate.mean)[-c(1:2000)])
rand_4 <- log10(as.numeric(read.table('rand_dat_4.log', head = T, as.is = T)$rate.mean)[-c(1:2000)])
rand_5 <- log10(as.numeric(read.table('rand_dat_5.log', head = T, as.is = T)$rate.mean)[-c(1:2000)])

block_1 <- log10(as.numeric(read.table('block_rand_1.log', head = T, as.is = T)$rate.mean)[c(1:2000)])
block_2 <- log10(as.numeric(read.table('block_rand_2.log', head = T, as.is = T)$rate.mean)[c(1:2000)])
block_3 <- log10(as.numeric(read.table('block_rand_3.log', head = T, as.is = T)$rate.mean)[c(1:2000)])
block_4 <- log10(as.numeric(read.table('block_rand_4.log', head = T, as.is = T)$rate.mean)[c(1:2000)])
block_5 <- log10(as.numeric(read.table('block_rand_5.log', head = T, as.is = T)$rate.mean)[c(1:2000)])


mean_vals <- c(mean(true_rate), mean(block_1), mean(block_2), mean(block_3),  mean(block_4), mean(block_5), mean(rand_1), mean(rand_2), mean(rand_3), mean(rand_4), mean(rand_5))

max_vals <- c(quantile(true_rate, 0.025), quantile(block_1, 0.025), quantile(block_2, 0.025), quantile(block_3, 0.025),  quantile(block_4, 0.025), quantile(block_5, 0.025), quantile(rand_1, 0.025), quantile(rand_2, 0.025), quantile(rand_3, 0.025), quantile(rand_4, 0.025), quantile(rand_5, 0.025))

min_vals <- c(quantile(true_rate, 0.975), quantile(block_1, 0.975), quantile(block_2, 0.975), quantile(block_3, 0.975),  quantile(block_4, 0.975), quantile(block_5, 0.975), quantile(rand_1, 0.975), quantile(rand_2, 0.975), quantile(rand_3, 0.975), quantile(rand_4, 0.975), quantile(rand_5, 0.975))

loc_vals <- c(1, 2, 2.1, 2.2, 2.3, 2.4, 3, 3.1, 3.2, 3.3, 3.4)
col_vals <- factor(c(2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

dat_all <- data.frame(loc_vals, mean_vals, min_vals, max_vals, col_vals)


plot_dat <- ggplot( dat_all, aes(x = loc_vals, y = mean_vals, colour = col_vals)) + geom_point() + geom_errorbar(aes(ymin = min_vals, ymax = max_vals, width = 0)) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + guides(colour = FALSE) + scale_x_continuous(breaks = c(1.25, 2.25, 3.25)) + geom_hline(y = log10(1e-4), linetype = 1)

pdf('spatial_structure.pdf', width = 5, height = 5, useDingbats = F)
print(plot_dat)
dev.off()









