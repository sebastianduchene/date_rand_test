

source('functions.R')

par(bg = 'black')
par(mfrow = c(1, 2))

#################
#################
################
################



#t2 <- get_tree_cal(span_cut = c(1, 1.2), max_cal = 5, min_cal = 1, tr_time = 100, n_tax = 50)
#plot(p1, edge.col = 'lightblue', edge.width = 3)
#tiplabels(p1$tip.label)

#plot(t1$chronogram, edge.width = 3, edge.col = 'orange')
#tiplabels(t1$chronogram$tip.label)

#plot(t1$chronogram, edge.width = 3, edge.col = 'lightblue')
#plot(t2$chronogram, edge.width = 3, edge.col = 'lightblue')


## Set rates
#Typical virus high rate 0.001

t1 <- get_tree_cal(span_cut = 1:10, max_cal = 40, min_cal = 35, tr_time = 100, n_tax = 50)
p1 <- t1$chronogram
p1$edge.length <- p1$edge.length * rlnorm(98, log(mean_rate), 0.3)

s1 <- as.DNAbin(simSeq(p1, l = 1000))

xml1 <- make_xml_file(s1, file_name = 'test1', random_dates = F)
cat(xml1, file = 'test1.xml', sep = '\n')

run_1 <- run_beast(file_name = 'test1.xml', beast_path = '~/Desktop/progs/Beast2/bin/')

rate_sim <- mean_rate
root_true <- max(allnode.times(t1$chronogram))
slope_true <- reg_true$coefficient[2]
r_true <- reg_true$r.squared
dates_true <- as.numeric(gsub('^.+_', '', t1$chronogram$tip.label))
reg_true <- summary(lm(allnode.times(p1, tipsonly = T) ~ dates_true ))
cal_time <- max(dates_true) - min(dates_true)




#####
 # Need to write function to run beast and collect relevant data: 
#calibration time, DONE
#mean simulated rate, DONE 
#mean estimated rate, DONE
#estimated rate HPD, DONE
#simulated root age, DONE
#mean estimated root age, DONE 
#estimated root age HPD, DONE
#regression slope for true data , DONE
#R for true data DONE
#estimated data slope DONE
# estimated data R DONE

# Is it necessary to have clock like data? 


## Set the rates, similar to those of virues, with high variation and low variation, and three calibration times
