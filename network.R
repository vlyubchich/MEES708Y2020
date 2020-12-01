rm(list = ls())
library(dplyr)

#### load data ####
D <- read.csv("./dataraw/level-2.csv")
# remove extra columns
D <- D %>% select(-index, -Full_Name, -Depth, -Cast, -starts_with("Unassign"))

#2do: fix names
tmp <- names(D)
tmp <- strsplit(tmp, "__")
tmp <- sapply(tmp, function(x) x[length(x)])
names(D) <- gsub("\\.", "", tmp) #replace dots with ""

#### correlations ####
#matrix of pairwise Pearson correlations
corPearson <- Hmisc::rcorr(as.matrix(D))
CM <- corPearson$r

#to remove self-loops, set the main diagonal to 0
diag(CM) <- 0

#if needed, adjust by removing non-significant
alpha = 0.05 #significance level for each test
CMsig <- CM
CMsig[corPearson$P > alpha] <- 0
sum(CMsig != 0)

#if needed, adjust for number of the tests (control for multiple testing)
CMsig <- CM
Padj <- corPearson$P
isSymmetric(Padj)
Padj[lower.tri(Padj, diag = FALSE)] <- p.adjust(Padj[lower.tri(Padj, diag = FALSE)],
                                                method = "BH")
Padj[upper.tri(Padj, diag = FALSE)] <- p.adjust(Padj[upper.tri(Padj, diag = FALSE)],
                                                method = "BH")
isSymmetric(Padj)
CMsig[Padj > alpha] <- 0
sum(CMsig != 0)


#### create network ####
library(igraph)
G <- graph_from_adjacency_matrix(CMsig,
                                 mode = "lower",
                                 weighted = TRUE,
                                 diag = FALSE)
plot(G, vertex.size = 10, edge.width = abs(E(G)$weight)*5,
     layout = layout.circle(G)
     ,edge.color = ifelse(CMsig > 0, "blue", "red"))
#2do: try other layouts


#### interactive network ####
Gcluster = cluster_walktrap(G)
plot(Gcluster, G)

library(networkD3)
G_D3 = igraph_to_networkD3(G, group = membership(Gcluster)) #
forceNetwork(Links = G_D3$links, Nodes = G_D3$nodes,
             Source = 'source', Target = 'target', NodeID = 'name',
             Group = 'group', legend = TRUE,
             opacity = 1, fontSize = 16, zoom = FALSE, bounded = TRUE)

#2do: get network statistics (degree distribution, etc.)
?`igraph-package`
degree_distribution(G)
betweenness(G)

