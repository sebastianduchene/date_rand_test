library(ggplot2)
library(gridExtra)

control_dat1 <- cbind(read.table('res_control_rep1.txt', head = T, as.is = T), cont = c(1.5, 1.4, 1.3, 1.2, 1.1, 1), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
control_dat2 <- cbind(read.table('res_control_rep_2.txt', head = T, as.is = T), cont = c(2.5, 2.4, 2.3, 2.2, 2.1, 2), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
control_dat3 <- cbind(read.table('res_control_rep3.txt', head = T, as.is = T), cont = c(3.5, 3.4, 3.3, 3.2, 3.1, 3), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
control_dat4 <- cbind(read.table('res_control_rep4.txt', head = T, as.is = T), cont = c(4.5, 4.4, 4.3, 4.2, 4.1, 4), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
control_dat5 <- cbind(read.table('res_control_rep5.txt', head = T, as.is = T), cont = c(5.5, 5.4, 5.3, 5.2, 5.1, 5), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
control_all <- rbind(control_dat1, control_dat2, control_dat3, control_dat4, control_dat5)


lvar_dat1 <- cbind(read.table('res_lvar_rep1.txt', head = T, as.is = T), cont = c(1.5, 1.4, 1.3, 1.2, 1.1, 1), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
lvar_dat2 <- cbind(read.table('res_lvar_rep2.txt', head = T, as.is = T), cont = c(2.5, 2.4, 2.3, 2.2, 2.1, 2), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
lvar_dat3 <- cbind(read.table('res_lvar_rep3.txt', head = T, as.is = T), cont = c(3.5, 3.4, 3.3, 3.2, 3.1, 3), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
lvar_dat4 <- cbind(read.table('res_lvar_rep4.txt', head = T, as.is = T), cont = c(4.5, 4.4, 4.3, 4.2, 4.1, 4), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
lvar_dat5 <- cbind(read.table('res_lvar_res5.txt', head = T, as.is = T), cont = c(5.5, 5.4, 5.3, 5.2, 5.1, 5), type = factor(c('rand', 'rand', 'rand', 'rand', 'rand', 'true')))
lvar_all <- rbind(lvar_dat1, lvar_dat2, lvar_dat3, lvar_dat4, lvar_dat5)

control_all$type <- control_all$type == 'true'

control_plot <- ggplot(control_all, aes(x = cont, y = log10(V2), colour = type)) + geom_point() + geom_errorbar(aes(ymin = log10(V3), ymax = log10(V4), width = 0)) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + guides(colour = FALSE) +  xlab('') + ylab(expression(paste('Rate estimate (', log[10], ' subst/site/year)'))) + geom_hline(y = log10(0.0038), linetype = 1) + coord_cartesian(ylim = c(-7, 0))

lvar_plot <- ggplot(lvar_all, aes(x = cont, y = log10(V2), colour = type)) + geom_point() + geom_errorbar(aes(ymin = log10(V3), ymax = log10(V4), width = 0)) + scale_colour_manual(values = c('grey', 'black')) + theme_bw() + guides(colour = FALSE)  +  xlab('') + ylab(expression(paste('Rate estimate (', log[10], ' subst/site/year)'))) + geom_hline(y = log10(0.0038), linetype = 1) + coord_cartesian(ylim = c(-7, 0))

pdf('seq_var_plot.pdf', width = 9, height = 5)
grid.arrange(lvar_plot, control_plot, nrow = 1, ncol = 2)
dev.off()