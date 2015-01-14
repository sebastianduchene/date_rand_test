source('../../expe_jar/functions.R')
library(NELSI)
library(ape)
library(phangorn)

sim_trees <- lapply(paste0('sim_trees_', 1:10, '.trees'), function(x) read.nexus(x))
select_trees <- c(3, 6, 2, 5, 3, 2, 2, 2, 2, 2)
sample_trees <- lapply(1:length(sim_trees), function(x) sim_trees[[x]][select_trees[x]])
class(sample_trees) <- 'multiPhylo'

mean_rate = 1e-4
sd_rate = 0.05

for(i in 1:length(sample_trees)){
  system(paste0('mkdir r', i))
  phylo_temp <- sample_trees[[i]][[1]]
  phylo_temp$edge.length <- phylo_temp$edge.length * rlnorm(length(phylo_temp$edge.length), log(mean_rate), sd_rate)
  dat_temp <- as.DNAbin(simSeq(phylo_temp, l = 2000))

  xml_true <- make_xml_file(dat_temp, file_name =  'true_dat', random_dates = F)
  cat(xml_true, file = paste0('r', i, '/true_dat.xml'), sep = '\n')

  for(k in 1:20){
    xml_rand <- make_xml_file(dat_temp, file_name = paste0('rand_dat_', k), random_dates = T)
    cat(xml_rand, file = paste0('r', i, '/rand_dat_', k, '.xml'), sep = '\n') 
  }

}
