rm(list = ls())
D <- swiss
N = 10000

#compute mean for each column
system.time( {
    for (i in 1:N) {
        colMeans(D)
    }
})
# user  system elapsed
# 0.87    0.04    0.92

system.time( {
    for (i in 1:N) {
        for (j in 1:ncol(D)) {
            mean(D[,j])
        }}
})
# user  system elapsed
# 0.86    0.00    0.86

system.time( {
    for (i in 1:N) {
        apply(D, 2, mean)
    }
})
# user  system elapsed
# 1.62    0.05    1.67


object.size(D)


myfun <- function(x, y)
{
    tmp = lm(x ~ y)
    coef = tmp$coeff
    pv = tmp$pv
    rm(tmp) #remove big object
    # gc()
}


Rprof(tmp <- tempfile())
for (i in 1:N) { colMeans(D) }
Rprof()
summaryRprof(tmp)



library(parallel)
cores <- detectCores()
cl <- parallel::makeCluster(cores)

system.time( {
    for (i in 1:N) {
        parallel::parApply(cl, D, 2, mean)
    }
})

system.time( {
    for (i in 1:N) {
        parallel::parApply(cl, D, 2, function(x) mean(sample(x, 10000, replace = TRUE)))
    }
})
# user  system elapsed
# 18.91    8.00   36.47

system.time( {
    for (i in 1:N) {
        apply(D, 2, function(x) mean(sample(x, 10000, replace = TRUE)))
    }
})
# user  system elapsed
# 58.27    0.09   59.03

parallel::stopCluster(cl) #stop cluster
