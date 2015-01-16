source('functions.R')

mean_rate = 1e-4
sd_rate = 0.05
min_cal = 1
max_cal = 3
set_name = 'SET_MBE_R1'

# Get into the folder with the data files
system(paste('mkdir', set_name)) 

# Get into the settings file
setwd(set_name)

# loop for replicates
for(i in 1:10){

# make file for the replicate:
system(paste0('mkdir r', i))

# Get into the file for the replicate:
setwd(paste0('r', i))

par(bg = 'black')
# Generate chronogram and save in memory:
if(max_cal == 3 ){
  span_cut <- c(1.1, 1.2)
  }else if(max_cal == 15){
    span_cut <- c(1.1, 1.2, 1.3)
  }else if(max_cal == 30){
  span_cut <- c(1.1, 1.5, 2, 2.2)
}

t1 <- get_tree_cal(span_cut = span_cut, max_cal = max_cal, min_cal = min_cal, tr_time = 100, n_tax = 50, print_trees = T)
p1 <- t1$chronogram
write.tree(t1$chronogram, file = 'sim_chrono.tree')

# Generate phylogram and sequences
p1$edge.length <- p1$edge.length * rlnorm(98, log(mean_rate), sd_rate)
s1 <- as.DNAbin(simSeq(p1, l = 2000))

# Generate xml files with true data and with randomisations

xml1 <- make_xml_file(s1, file_name = 'true_dat', random_dates = F)
cat(xml1, file = paste0('true_dat.xml'), sep = '\n')

for(r_rep in 1:20){
 xml_rand <- make_xml_file(s1, file_name = paste0('rand_dat_', r_rep), random_dates = T)
 cat(xml_rand, file = paste0('rand_dat_', r_rep, '.xml'), sep = '\n')
}

setwd('..')

}

