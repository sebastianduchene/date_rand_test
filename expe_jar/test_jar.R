
source('functions.R')


mean_rate = 0.01
sd_rate = 0.3
min_cal = 0.5
max_cal = 1.5


t1 <- get_tree_cal(span_cut = c(1.05, 1.3), max_cal = max_cal, min_cal = min_cal, tr_time = 100, n_tax = 50, print_trees = F)
p1 <- t1$chronogram
p1$edge.length <- p1$edge.length * rlnorm(98, log(mean_rate), sd_rate)
s1 <- as.DNAbin(simSeq(p1, l = 1000))



# True dates run
xml1 <- make_xml_file(s1, file_name = 'test1', random_dates = F)
cat(xml1, file = 'test1.xml', sep = '\n')

run_1 <- run_beast(file_name = 'test1.xml', beast_path = 'java -jar /Users/sebastianduchene/Desktop/progs/BEAST2.1.3/lib/beast.jar -beagle -overwrite')
