library(ape)
source('../functions.R')

taxa_prune <- c('DQ115534_1925', 'DQ115527_1990', 'DQ115529_1990', 'AF235168_2000',  'EF521830_2000',  'EF521848_2000',  'EF521839_2000', 'EF521827_2000', 'EF408186_2005',  'EF408187_2005', 'EF408188_2005', 'EF372272_2006') 



seq_data <- read.dna('cydv_clean.fasta', format = 'fasta')

comp_dates <- gsub('.+_', '', rownames(seq_data))


#GET COMPLETE DATA AND RANDOMISED DATA
comp_data <- make_xml_file(seq_data, file_name = 'complete_true', random_dates = F)
cat(comp_data, file = 'comp_true.xml', sep = '\n')

for(i in 1:5){
      comp_temp <- make_xml_file(seq_data, file_name = paste0('complete_rand_', i), random_dates = T)
      cat(comp_temp, file = paste0('complete_rand_', i, '.xml'), sep = '/n')
}


#GET PRUNED DATA WITHOUT THE DEEPEST CALIBRATIONS
pruned_data <- seq_data[!(rownames(seq_data) %in% taxa_prune), ]

pruned_dates <- gsub('.+_', '', rownames(pruned_data))

prune_true_file <- make_xml_file(pruned_data, file_name = 'pruned_true', random_dates = F)
cat(prune_true_file, file = 'pruned_true.xml', sep = '\n')

for(i in 1:5){
      rand_temp <- make_xml_file(pruned_data, file_name = paste0('pruned_true_rand_', i), random_dates = T)
      cat(rand_temp, file = paste0('pruned_true_rand_', i, '.xml'), sep = '\n')
}


# GET PRUNED ALTERNATIVE  AND RANDOMISED DATA SETS

taxa_alt_prune <- sample(rownames(seq_data)[!(rownames(seq_data) %in% taxa_prune)], length(taxa_prune))

prune_alt_data <- seq_data[!(rownames(seq_data) %in% taxa_alt_prune), ]

prune_alt_file <- make_xml_file(prune_alt_data, file_name = 'prune_alt_true', random_dates = F)
cat(prune_alt_file, file = 'prune_alt_true.xml', sep = '\n')

for(i in 1:5){
      rand_temp <- make_xml_file(prune_alt_data, file_name = paste0('pruned_alt_rand_', i), random_dates = T)
      cat(rand_temp, file = paste0('prune_alt_rand_', i, '.xml'), sep = '\n')
}