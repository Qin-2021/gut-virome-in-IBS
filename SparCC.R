library(SpiecEasi)
library(parallel)

rm(list=ls())

amgut1.filt =read.csv("HC_wide.csv", header=T,row.names=1)
amgut1.filt <-t(amgut1.filt)
amgut1.filt <- data.frame(amgut1.filt, check.names = FALSE)

set.seed(123)

amgut1.filt.sparcc <- sparcc(amgut1.filt, iter = 20, inner_iter = 10, th = 0.1)

sparcc0 <- amgut1.filt.sparcc$Cor  
sparcc.cov0 <- amgut1.filt.sparcc$Cov  
sparcc.cov0


colnames(sparcc0) <- colnames(amgut1.filt)
rownames(sparcc0) <- colnames(amgut1.filt)
write.table(sparcc0, 'HC.result/sparcc0.txt', sep = '\t', col.names = NA, quote = FALSE)


set.seed(123)
n = 100


cores <- detectCores()

# parallel running, assign cores
mclapply(1:n, mc.cores=cores, function(i) {
  amgut1.filt.boot <- sample(amgut1.filt, replace = TRUE)  #bootstrap
  amgut1.filt.sparcc_boot <- sparcc(amgut1.filt.boot, iter = 20, inner_iter = 10, th = 0.1)  
  sparcc_boot <- amgut1.filt.sparcc_boot$Cor
  colnames(sparcc_boot) <- colnames(amgut1.filt.boot)
  rownames(sparcc_boot) <- colnames(amgut1.filt.boot)
  write.table(sparcc_boot, paste('HC.result/sparcc_boot', i, '.txt', sep = ''), sep = '\t', col.names = NA, quote = FALSE)  
})
####END

#================================================================
p <- sparcc0
p[p!=0] <- 0

for (i in 1:n) {
  p_boot <- read.delim(paste('HC.result/sparcc_boot', i, '.txt', sep = ''), sep = '\t', row.names = 1)
  p[abs(p_boot)>=abs(sparcc0)] <- p[abs(p_boot)>=abs(sparcc0)] + 1
}

p <- p/n
write.table(p, 'HC.result/pvals.two_sided.txt', sep = '\t', col.names = NA, quote = FALSE)


cor_sparcc <- read.delim('HC.result/sparcc0.txt', row.names = 1, sep = '\t', check.names = FALSE)

pvals <- read.delim('HC.result/pvals.two_sided.txt', row.names = 1, sep = '\t', check.names = FALSE)

# screening out p>=0.05 and absolute sparcc <= 0.15
cor_sparcc[abs(cor_sparcc)<=0.15 | pvals>=0.05] <- 0

diag(cor_sparcc) <- 0

write.table(cor_sparcc, 'HC.result/0.15cor_neetwork.adj.txt', col.names = NA, sep = '\t', quote = FALSE)

#saving node and edge tables=================================================
#install.packages("igraph")
library(igraph)
neetwork_adj <- read.delim('HC.result/cor_neetwork.adj.txt', row.names = 1, sep = '\t', check.names = FALSE)
head(neetwork_adj)[1:6]    
g <- graph_from_adjacency_matrix(as.matrix(neetwork_adj), mode = 'undirected', weighted = TRUE, diag = FALSE)
g

E(g)$sparcc <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)

edge <- data.frame(as_edgelist(g))

edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(g)$weight,
  sparcc = E(g)$sparcc
  
)

head(edge_list)

write.table(edge_list, 'HC.result/cor_network.edge_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

node_list <- data.frame(
  id = V(g)$name,
  label = V(g)$name,
  degree = degree(g)
)
head(node_list)

write.table(node_list, 'HC.result/cor_network.node_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

