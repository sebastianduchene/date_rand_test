source('functions.R')

for(set in 17:24){

if(set == 17){
mean_rate = 0.001
sd_rate = 0.01
min_cal = 1
max_cal = 1.5
set_name <- 'SET17'
}


if(set == 18){
mean_rate = 0.001
sd_rate = 0.01
min_cal = 5
max_cal = 10
set_name = 'SET18'
}


if(set == 19){
mean_rate = 0.001
sd_rate = 0.01
min_cal = 10
max_cal = 20
set_name = 'SET19'
}

if(set == 20){
mean_rate = 0.001
sd_rate = 0.01
min_cal = 20
max_cal = 30
set_name = 'SET20'
}



if(set == 21){
mean_rate = 0.001
sd_rate = 0.3
min_cal = 1
max_cal = 1.5
set_name = 'SET21'
}

if(set == 22){
mean_rate = 0.001
sd_rate = 0.3
min_cal = 5
max_cal = 10
set_name = 'SET22'
}

if(set == 23){
mean_rate = 0.001
sd_rate = 0.3
min_cal = 10
max_cal = 20
set_name = 'SET23'
}

if(set == 24){
mean_rate = 0.01
sd_rate = 0.3
min_cal = 20
max_cal = 30
set_name = 'SET24'
}



#################
#################
#################


if(set == 7){
mean_rate = 0.0001
sd_rate = 0.01
min_cal = 1
max_cal = 1.5
set_name = 'SET7'
}

if(set == 8){
mean_rate = 0.0001
sd_rate = 0.01
min_cal = 5
max_cal = 10
set_name = 'SET8'
}

if(set == 9){
mean_rate = 0.0001
sd_rate = 0.01
min_cal = 10
max_cal = 20
set_name = 'SET9'
}

if(set == 15){
mean_rate = 0.0001
sd_rate = 0.01
min_cal = 20
max_cal = 30
set_name = 'SET15'
}



if(set == 10){
mean_rate = 0.0001
sd_rate = 0.3
min_cal = 1
max_cal = 1.5
set_name = 'SET10'
}

if(set == 11){
mean_rate = 0.0001
sd_rate = 0.3
min_cal = 5
max_cal = 10
set_name = 'SET11'
}

if(set == 12){
mean_rate = 0.0001
sd_rate = 0.3
min_cal = 10
max_cal = 20
set_name = 'SET12'
}

if(set == 16){
mean_rate = 0.0001
sd_rate = 0.3
min_cal = 20
max_cal = 30
set_name = 'SET16'
}



#system('mkdir runs_dat')

# Get into the folder with the data files
setwd('runs_dat')
# Make settings file
system(paste('mkdir', set_name)) 

# Get into the settings file
setwd(set_name)

# loop for replicates
for(i in 1:10){

# make file for the replicate:
system(paste0('mkdir REP_', i))

# Get into the file for the replicate:
setwd(paste0('REP_', i))

par(bg = 'black')
# Generate chronogram and save in memory:
if(max_cal >=10 & max_cal < 20){
span_cut <- c(1.05, 2, 1.3)
}else if(max_cal>=20){
span_cut <- c(1.1, 2, 1.5)
}else{
span_cut <- c(1.05, 1.1, 1.01)
}
t1 <- get_tree_cal(span_cut = span_cut, max_cal = max_cal, min_cal = min_cal, tr_time = 100, n_tax = 50, print_trees = T)
p1 <- t1$chronogram
write.tree(t1$chronogram, file = 'sim_chrono.tree')

# Generate phylogram and sequences
p1$edge.length <- p1$edge.length * rlnorm(98, log(mean_rate), sd_rate)
s1 <- as.DNAbin(simSeq(p1, l = 2000))

# Save in memory simulated rate, age of root, calibration range, regression slope, and r^2
rate_sim <- mean_rate
root_true <- max(allnode.times(t1$chronogram))
dates_true <- as.numeric(gsub('^.+_', '', t1$chronogram$tip.label))
reg_true <- summary(lm(allnode.times(p1, tipsonly = T) ~ dates_true + 0 ))
slope_true <- reg_true$coefficient[1]
r_true <- reg_true$r.squared
cal_time <- max(dates_true) - min(dates_true)

# Generate xml files with true data and with randomisations

xml1 <- make_xml_file(s1, file_name = 'true_dat', random_dates = F)
cat(xml1, file = paste0('true_dat.xml'), sep = '\n')

for(r_rep in 1:5){
 xml_rand <- make_xml_file(s1, file_name = paste0('rand_dat_', r_rep), random_dates = T)
 cat(xml_rand, file = paste0('rand_dat_', r_rep, '.xml'), sep = '\n')
}

# Save simulated data results
cat(paste('sim_data', cal_time, mean_rate, sd_rate, slope_true, r_true, collapse = ' '), sep = '\n', file = 'res_replicate.txt')

# Exit replicate folder

setwd('..')

}

setwd('../..')
#############################




}