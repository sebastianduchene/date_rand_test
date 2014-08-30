
library(ape)
files_dat <- grep('true_dat.xml$', dir(recursive = T), value = T)


v_sites_mat <- matrix(NA, nrow = length(files_dat), ncol = 2)

for(i in 1:length(files_dat)){
      d_temp <- readLines(files_dat[i])
      s_temp <- gsub('<.+=\\"|\"/> </data> +$|\"/>$| name=.+> | ', '',  grep('<sequence id=', d_temp, value = T))
      s_temp <- strsplit(s_temp, '')
      t_mat <- matrix(NA, nrow = length(s_temp), ncol = length(s_temp[[1]]))

      for(j in 1:length(s_temp)){
      	    t_mat[j, ] <-  s_temp[[j]]
      }

      rownames(t_mat) <- 1:nrow(t_mat)
      t_mat <- as.DNAbin(t_mat)

      print(c(gsub('true_dat.xml', '', files_dat[i]), length(seg.sites(t_mat))))
      v_sites_mat[i, ] <- c(gsub('true_dat.xml', '', files_dat[i]), length(seg.sites(t_mat)) / 2000)

}

