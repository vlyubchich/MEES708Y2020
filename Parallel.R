rm(list = ls())
D <- swiss
N = 10000



for (i in 1:N) {
    colMeans(x)
}

object.size()
parallel::parSapply



Rprof(tmp <- tempfile())
for (i in 1:N) { colMeans(D) }
Rprof()
summaryRprof(tmp)
