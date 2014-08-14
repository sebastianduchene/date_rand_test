source('functions.R')
#par(bg = 'black')

mean_rate = 0.01
sd_rate = 0.3
min_cal = 0.5
max_cal = 1.5


for(i in 1:100){

t1 <- get_tree_cal(span_cut = c(1.05, 1.3), max_cal = max_cal, min_cal = min_cal, tr_time = 100, n_tax = 50, print_trees = F)
p1 <- t1$chronogram
p1$edge.length <- p1$edge.length * rlnorm(98, log(mean_rate), sd_rate)
s1 <- as.DNAbin(simSeq(p1, l = 1000))


#stop('standardise span')


# True dates run
xml1 <- make_xml_file(s1, file_name = 'test1', random_dates = F)
cat(xml1, file = 'test1.xml', sep = '\n')

run_1 <- run_beast(file_name = 'test1.xml', beast_path = '~/Desktop/progs/Beast2/bin/')

rate_sim <- mean_rate
root_true <- max(allnode.times(t1$chronogram))
dates_true <- as.numeric(gsub('^.+_', '', t1$chronogram$tip.label))
reg_true <- summary(lm(allnode.times(p1, tipsonly = T) ~ dates_true + 0 ))
slope_true <- reg_true$coefficient[1]
r_true <- reg_true$r.squared
cal_time <- max(dates_true) - min(dates_true)

cat(paste(paste0('run_', i), cal_time, mean_rate, sd_rate, paste(unlist(run_1), collapse = ' '), slope_true, r_true, collapse = ' '), sep = '\n', file = 'SET1.txt', append = T)

#OUT <- paste(paste0('run_', i), cal_time, mean_rate, sd_rate, paste(unlist(run_1), collapse = ' '), slope_true, r_true, collapse = ' ')


stop('wooooooo')
# Randomised dates run
for(k in 1:5){
xml_rand <- make_xml_file(s1, file_name = 'test1', random_dates = T)
cat(xml_rand, file = 'test1.xml', sep = '\n')

run_rand <- run_beast(file_name = 'test1.xml', beast_path = '~/Desktop/progs/Beast2/bin/')

cat(paste(paste0('run_rand_', i, '_', k), cal_time, mean_rate, sd_rate, paste(unlist(run_rand), collapse = ' '), slope_true, r_true, collapse = ' '), sep = '\n', file = 'SET1.txt', append = T)




}


}