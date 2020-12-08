# Examples of implementing machine learning in R
# All applied to a small dataset `swiss`. Note, however,
# - power of machine learning is usually in analyzing large datasets;
# - the algorithms should be tuned and compared via a formal cross-validation
#(e.g., using a holdout sample for testing that is not used for selecting and training the algorithms).

rm(list = ls())
data(swiss)
set.seed(123)

#### CART ####
library(rpart)
fit_cart <- rpart(Examination ~ ., data = swiss)
fit_cart <- rpart(Examination ~ ., data = swiss,
                  control = rpart.control(cp = 0.001, minbucket = 3)) #default 0.01
fit_cart
plot(fit_cart) #not a good plot, use code below

# The (inverted) tree plot for predicting percentage of draftees receiving highest grade on army examination:
library(rpart.plot)
rpart.plot(fit_cart)

# **Exercise**
# Adjust `rpart` control parameters to obtain a tree that is
# a) smaller;
# b) bigger.

#### Random forest ####
#### using randomForest ####
library(randomForest)
fit_rf <- randomForest(Examination ~ ., data = swiss)
fit_rf <- randomForest(Examination ~ ., data = swiss, mtry = 2, nodesize = 3)
fit_rf
plot(fit_rf)

# **Exercise**
# Adjust `randomForest` parameters to
# a) increase number of variables tried at each split;
# b) decrease terminal node size
# and compare results.


# Importance of predictors (from randomForest)
tmp <- as.vector(fit_rf$importance)
names(tmp) <- rownames(fit_rf$importance)
tmp <- sort(tmp)
par(mar = c(3.3, 5.5, 1, 1), mgp = c(2.0, 0.7, 0))
barplot(tmp,
        beside = TRUE, las = 1,
        xlab = "Importance",
        col = 2, border = NA, cex.names = 0.8,
        horiz = TRUE)

# Partial dependence plots
preds <- rownames(fit_rf$importance)
par(mfrow = c(2, 3))
for (i in seq_along(preds)) { #this should work simply with: i in rownames(fit_rf$importance), but it doesn't
    partialPlot(fit_rf, pred.data = swiss,
                las = 1, main = "", ylab = "Examination",
                x.var = preds[i], xlab = preds[i])
}

# Partial dependence plots with plotmo, including interactions
library(plotmo)
plotmo(fit_rf, pmethod = "partdep")
plotmo(fit_rf, pmethod = "partdep", type2 = "contour")

# **Exercise**
# Use `plotmo` to obtain
# a) contour plots for the interactions;
# b) select only 3 plots of your choice.



#### using ranger ####
# Package ranger is faster and has some extra functions
library(ranger)
fit_ran <- ranger(Examination ~ .,
              importance = 'impurity_corrected',
              data = swiss)
fit_ran

# Importance of predictors (from ranger)
ranimp <- importance_pvalues(fit_ran, method = "altmann",
                             formula = Examination ~ .,
                             data = swiss)
ranimp <- ranimp[order(ranimp[,1]),]
ranimp
tmp <- ranimp[,1]
barplot(tmp,
        beside = TRUE, las = 1, #xlim = c(0, 300),
        xlab = "Importance",
        col = 1, border = NA, cex.names = 0.6,
        horiz = TRUE)

# **Exercise**
# Use `importance_pvalues` to obtain
# a) Altmann's p-values with more permutations;
# b) p-values from another available method;
# and compare results.

# Boruta is another algorithm to test importance of predictors in a random forest.
# library(Boruta)
B = Boruta::Boruta(Examination ~ .,
                   doTrace = 1, maxRuns = 5000,
                   data = swiss)
print(B)
plot(B, horizontal = TRUE,
     las = 1, ylab = "", xlab = "Importance")

# **Exercise**
# Adjust colors of the boxplots in `Boruta` plot.
# https://github.com/vlyubchich/tilefish


#### Clustering ####
# There are 100s of clustering methods.

# Scale data (if variables are measured in different units) to depend less on the scale of the values
D <- scale(swiss) # scales to z-scores (in each column, mean = 0  and variance = 1)

#### hierarchical / agglomerative ####
d <- dist(D, method = "euclidean") # distance matrix between rows of the data
fit_hc <- hclust(d, method = "ward.D2")

# Determine the number of clusters
# https://www.statmethods.net/advstats/cluster.html
# https://uc-r.github.io/hc_clustering
factoextra::fviz_nbclust(D, FUN = factoextra::hcut, method = "wss")
factoextra::fviz_nbclust(D, FUN = factoextra::hcut, method = "silhouette")

K <- 3 #selected number of clusters

plot(fit_hc, las = 1, main = "", xlab = "", sub = "", ylab = "Distance") #display dendrogram
cluster <- cutree(fit_hc, k = K) #cut tree into clusters
cluster
# draw dendrogram with borders around the clusters
rect.hclust(fit_hc, k = K, border = "darkorchid1")

# **Exercise**
# Repeat the clustering analysis and compare results with
# a) different type of distances;
# b) different linkage (agglomeration) method.


#### k-means and k-medoids ####
fit_kmean <- kmeans(D, K, nstart = 25)
fit_kmean$size
fit_kmean$centers
plot(swiss, col = fit_kmean$cluster, pch = 16)

factoextra::fviz_cluster(fit_kmean, swiss, ellipse.type = "norm")

# **Exercise**
# Compare results with kmeans applied with
# a) different number of clusters (K);
# b) different agglomeration algorithm.

library(clue)
fit_kmed <- kmedoids(d, K)
fit_kmed
plot(swiss, col = fit_kmed$cluster, pch = 16)

factoextra::fviz_cluster(fit_kmed, swiss, ellipse.type = "norm")

# **Exercise**
# Trick `fviz_cluster` to plot k-medoids clusters, e.g., by replacing cluster labels in fit_kmean object
# with `fit_kmed$cluster`.

#### dbscan ####
# library(dbscan)
# library(funtimes)

# Use downhill riding approach to select the optimal clustering parameter
# (Huang X, Iliev IR, Lyubchich V, Gel YR (2018). “Riding down the bay: space-time clustering
# of ecological trends.” Environmetrics, 29. doi: 10.1002/env.2455)
MP = 3 #is another clustering parameter that is currently user-defined (not optimized)
eps_opt <- funtimes::DR(t(D), method = "DBSCAN", minPts = MP)$P_opt

#### using dbscan ####
fit_dbs <- dbscan::dbscan(D, eps = eps_opt, minPts = MP)
plot(swiss, col = fit_dbs$cluster + 1L, pch = 16)

#### using fpc ####
fit_dbs_fpc <- fpc::dbscan(D, eps = eps_opt, MinPts = MP)
fit_dbs_fpc
all(fit_dbs$cluster == fit_dbs_fpc$cluster) #check that the packages agree

plot(swiss, col = fit_dbs_fpc$cluster + 1L, pch = 16)
factoextra::fviz_cluster(fit_dbs_fpc, swiss, ellipse.type = "norm")


#### Neural networks ####
library(h2o)

h2o.init()
D_h2o <- as.h2o(swiss)
fit_dl <- h2o.deeplearning(y = "Examination", training_frame = D_h2o, seed = 123456)

# Predictions
pred_dl <- h2o.predict(fit_dl, D_h2o)
cor(swiss$Examination, as.vector(pred_dl))

# **Exercise**
# Compare results when changing the
# a) number of hidden layers
# b) number of neurons per hidden layer
# c) dropout ratios.

# Remember to use cross-validation (was not implemented in the given examples).
