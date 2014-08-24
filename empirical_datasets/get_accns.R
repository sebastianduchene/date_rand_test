tula_virus_accns  <-  c("U95302","U95303","U95304","U95305","U95309","U95310","U95311","U95312","NC_005227","Z30941","Z30942","Z30943","Z30944","Z30945","Z48573","Z48574","Z48741","AF063892","AF063897","AJ223600","AJ223601","Y13979","Y13980")

puumala_virus_accns <- c("AJ888751","AJ888752","PVU95306","AJ277030","AJ277031","AJ277032","AJ277033","AJ277034","AJ238791","AJ278092","AJ278093","AB010730","AB010731","AJ314597","AJ314598","AJ314599","AJ314600","AJ314601","Z21497_1","Z30702_1","Z30703_1","Z30704_1","Z30705_1","Z30706_1","Z30707_1","Z30708_1","Z46942_1","Z69985_1","AJ223368","AJ223369","AJ223371","AJ223374","AJ223375","AJ223376","AJ223377","AJ223380","AF367064","AF367065","AF367066","AF367067","AF367068","AF367069","AF367070","AF367071","AF411447","AF411448","AF411449","AF442613","AJ238788","AJ238789","AJ238790","AJ888731","AJ888732","AJ888733","AJ888734","AJ888735","AJ888736","AJ888738","Z48586")

araquara_G1_accns <-  paste0("EU1701", 62:93)

#puumala 6 -4, tula 2 -2, araquara 8.65 -3


library(ape)

tula_data <- read.GenBank(tula_virus_accns, gene.names = T)

puumala_data <- list()

for(i in 1:length(puumala_virus_accns)){
      print(paste('getting sequence', puumala_virus_accns[i]))
      dat_temp <- tryCatch(read.GenBank(puumala_virus_accns[i], gene.names = T), error = function(x) return('error'))
      if(dat_temp != 'error'){
         puumala_data[[i]] <- dat_temp
      }
      
}

araquara_data <- read.GenBank(araquara_G1_accns)
