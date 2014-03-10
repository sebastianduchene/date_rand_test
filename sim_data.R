
library(phangorn)
library(geiger)

#simulate k=1
# 32 taxa, Yule, 12 genes, mean = 0.1, sd = 0.05, 2000 nuc/gene, root age = 100, noise: N(0, 0.1)

if(k1 == T){
tr.k1 <- sim.bdtree(b = 1, d = 0, stop = "taxa", n = 32)
rate.k1 <- abs(rnorm(62, 0.001, 0.0005))
chrono.k1 <- tr.k1
chrono.k1$edge.length <- (tr.k1$edge.length / max(branching.times(tr.k1)) ) * 100

phylo.k1 <- chrono.k1
phylo.k1$edge.length <- (chrono.k1$edge.length * rate.k1) 

write.tree(chrono.k1, file = "k1.tree")

par(mfrow = c(1, 2))
k1.list <- list()
for(i in 1:12){
      print(paste("simulating data set", i))
      tr.temp <- phylo.k1
      tr.temp$edge.length <- abs(phylo.k1$edge.length + rnorm(62, 0, 0.001))
      k1.list[[i]] <- as.DNAbin(simSeq(tr.temp, l = 2000))
      plot(tr.temp)
      hist(tr.temp$edge.length)
      print(tr.temp$edge.length)
      system("sleep 2")
      write.dna(k1.list[[i]], file = paste0("k1/k1.gene_", i, ".fasta"), format = "fasta", nbcol = -1, colsep = "")       
}
}

##########################################
#simulate k=2

if(sim2 == T){

tr.k2 <- sim.bdtree(b = 1, d = 0, stop = "taxa", n = 32)
rate.1 <- abs(rnorm(62, 0.001, 0.0005))
rate.2 <- abs(rnorm(62, 0.001, 0.0005))

chrono.k2 <- tr.k2
chrono.k2$edge.length <- (tr.k2$edge.length / max(branching.times(tr.k2)) ) * 100

phylo.k2.1 <- chrono.k2
phylo.k2.1$edge.length <- (chrono.k2$edge.length * rate.1) 

write.tree(chrono.k2, file = "k2.tree")

par(mfrow = c(1, 2))
k2.list <- list()
for(i in 1:6){
      print(paste("simulating data set", i))
      tr.temp <- phylo.k2.1
      tr.temp$edge.length <- abs(phylo.k2.1$edge.length + rnorm(62, 0, 0.001))
      k2.list[[i]] <- as.DNAbin(simSeq(tr.temp, l = 2000))
      plot(tr.temp)
      hist(tr.temp$edge.length)
      print(tr.temp$edge.length)
      system("sleep 2")
      write.dna(k2.list[[i]], file = paste0("k2/k2.gene_", i, ".fasta"), format = "fasta", nbcol = -1, colsep = "")       
}


phylo.k2.2 <- chrono.k2
phylo.k2.2$edge.length <- (chrono.k2$edge.length * rate.2) 


k2.list <- list()
for(i in 1:6){
      print(paste("simulating data set", i))
      tr.temp <- phylo.k2.2
      tr.temp$edge.length <- abs(phylo.k2.2$edge.length + rnorm(62, 0, 0.001))
      k2.list[[i]] <- as.DNAbin(simSeq(tr.temp, l = 2000))
      plot(tr.temp)
      hist(tr.temp$edge.length)
      print(tr.temp$edge.length)
      system("sleep 2")
      write.dna(k2.list[[i]], file = paste0("k2/k2.gene_", i + 6, ".fasta"), format = "fasta", nbcol = -1, colsep = "")       
}
}

##########################################
#simulate k=3
if(sim3 == T){ 
tr.k3 <- sim.bdtree(b = 1, d = 0, stop = "taxa", n = 32)
rate.1 <- abs(rnorm(62, 0.001, 0.0005))
rate.2 <- abs(rnorm(62, 0.001, 0.0005))
rate.3 <- abs(rnorm(62, 0.001, 0.0005))

chrono.k3 <- tr.k3
chrono.k3$edge.length <- (tr.k3$edge.length / max(branching.times(tr.k3)) ) * 100

phylo.k3.1 <- chrono.k3
phylo.k3.1$edge.length <- (chrono.k3$edge.length * rate.1) 

write.tree(chrono.k3, file = "k3.tree")

par(mfrow = c(1, 2))
k3.list <- list()
for(i in 1:4){
      print(paste("simulating data set", i))
      tr.temp <- phylo.k3.1
      tr.temp$edge.length <- abs(phylo.k3.1$edge.length + rnorm(62, 0, 0.001))
      k3.list[[i]] <- as.DNAbin(simSeq(tr.temp, l = 2000))
      plot(tr.temp)
      hist(tr.temp$edge.length)
      print(tr.temp$edge.length)
      system("sleep 2")
      write.dna(k3.list[[i]], file = paste0("k3/k3.gene_", i, ".fasta"), format = "fasta", nbcol = -1, colsep = "")       
}


phylo.k3.2 <- chrono.k3
phylo.k3.2$edge.length <- (chrono.k3$edge.length * rate.2) 

k2.list <- list()
for(i in 1:4){
      print(paste("simulating data set", i))
      tr.temp <- phylo.k3.2
      tr.temp$edge.length <- abs(phylo.k3.2$edge.length + rnorm(62, 0, 0.001))
      k3.list[[i]] <- as.DNAbin(simSeq(tr.temp, l = 2000))
      plot(tr.temp)
      hist(tr.temp$edge.length)
      print(tr.temp$edge.length)
      system("sleep 2")
      write.dna(k3.list[[i]], file = paste0("k3/k3.gene_", i + 4, ".fasta"), format = "fasta", nbcol = -1, colsep = "")       
}

phylo.k3.3 <- chrono.k3
phylo.k3.3$edge.length <- (chrono.k3$edge.length * rate.3) 

k3.list <- list()
for(i in 1:4){
      print(paste("simulating data set", i))
      tr.temp <- phylo.k3.3
      tr.temp$edge.length <- abs(phylo.k3.3$edge.length + rnorm(62, 0, 0.001))
      k3.list[[i]] <- as.DNAbin(simSeq(tr.temp, l = 2000))
      plot(tr.temp)
      hist(tr.temp$edge.length)
      print(tr.temp$edge.length)
      system("sleep 2")
      write.dna(k3.list[[i]], file = paste0("k3/k3.gene_", i + 8, ".fasta"), format = "fasta", nbcol = -1, colsep = "")       
}




}