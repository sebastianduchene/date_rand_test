# This is the script to compile all the results from runs in different folders


runs_file <- 'runs_dat'
cudir <- getwd()


true_dat_name <- '/res_replicate.txt'
est_dat_name <- '/res_runs.txt'

# Get complete list of files
set_files_1 <- grep('/.+/' , dir(recursive = T), value = T)


# Get unique file names

set_files <- unique(gsub('/([a-z]|[A-z]|_|[0-9]|[.])+$', '', set_files_1))

for(i in 1:length(set_files)){
      row_name_temp <- set_files[i]

      true_raw <- tryCatch(read.table(paste0(set_files[i], true_dat_name), as.is = T), error = function(x) return('FILE NOT FOUND'))
      est_raw <- tryCatch(read.table(paste0(set_files[i], est_dat_name), as.is = T), error = function(x) return('FILE NOT FOUND'))
 
      if('FILE NOT FOUND' %in% c(true_raw, est_raw)){
      	       print(paste('skipping', row_name_temp))
      	       next	   
      }
      
      true_temp <- true_raw[ ,2:6]

      if(length(grep('true_dat', est_raw[, 1])) > 1){
        stop(paste('Error', row_name_temp, ' has multiple runs'))
      }

      est_temp <- est_raw[grep('true_dat', est_raw[, 1]), 2:7]

      for(rand_rep in grep('rand', est_raw[, 1])){
        est_temp <- cbind(est_temp[1, ], est_raw[rand_rep, 2:7])
      }

      compile_dat <- as.matrix(cbind(row_name_temp, true_temp, est_temp))

#      colnames(compile_dat) <- c('Run.name', 'cal.time', 'mean.rate', 'sd.rate', 'slope', 'r', 'ucld.mean', 'ucld.low', 'ucld.high', 'root.mean', 'root.low', 'root.high', 'r1.ucld.mean', 'r1.ucld.low', 'r1.ucld.high', 'r1.root.mean', 'r1.root.low', 'r1.root.high', 'r2.ucld.mean', 'r2.ucld.low', 'r2.ucld.high', 'r2.root.mean', 'r2.root.low', 'r2.root.high', 'r3.ucld.mean', 'r3.ucld.low', 'r3.ucld.high', 'r3.root.mean', 'r3.root.low', 'r3.root.high')

      cat(paste(as.character(compile_dat), collapse = ' '), file= 'compiled_data.txt', sep = '\n', append = T)
}

