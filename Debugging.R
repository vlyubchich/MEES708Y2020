####   R silent ####
tmp <- as.Date(c("2011-04-05", 35))
tmp

tmp = as.matrix(swiss)
col23 = tmp[, 2:3]
is.matrix(col23)

col1 = tmp[,1, drop = FALSE]
is.matrix(col1)

#bad solution, use drop = FALSE
if (is.matrix(col1) ) {
    col1 = as.matrix(col1)
}

####  warning #######
tmp = log(-2)
is.na(tmp)

is.nan(tmp)
is.nan(NA)

####  Error #######
lm(x ~ y)


#### TOOLS #######
traceback()

mean(x)
traceback()

#browser() and debug()
source("MyFunctions2020.R")

truncmean(col1)

debug(truncmean)
truncmean(col1)

debug(lm)
lm(x ~ y)

