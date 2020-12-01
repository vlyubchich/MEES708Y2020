rm(list =  ls())

#Get original function and profile it
library(funtimes)
?notrend_test

set.seed(1)

U = swiss[,1]

system.time({
notrend_test(U, B = 1000)
})
# user  system elapsed
# 1.22    0.00    1.22

system.time({
notrend_test(U, B = 1000, test = "WAVK", factor.length = "adaptive.selection")
})


#Hint
#1) do not save all bootstraps
#2) parallelize, e.g., snowboot::boot_dd

# source your new function and apply system.time to it same as above

