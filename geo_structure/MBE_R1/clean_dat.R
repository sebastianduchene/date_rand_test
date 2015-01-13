dat <- readLines('true_dat.xml')

dat_rep <- gsub('\"/>|\"|^.+=', '', dat)

for(i in 1:length(dat_rep)){
      cat('>taxa_', i, '\n', file = 'out.fasta', append = T) 
      cat(dat_rep[i], '\n', file = 'out.fasta', append = T)
      
}