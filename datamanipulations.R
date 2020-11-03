rm(list = ls()) #clean the environment

####### birthwt ########
library(MASS)
data(birthwt)

# 1. Code the age of mother in years age as less than 20, 20-35 or greater than 35.
birthwt$Agegroup <- cut(birthwt$age, breaks=c(-Inf,20,35,Inf),
                        label=c("Teen","Adult","Senior"))
table(birthwt$Agegroup)


# 2. Count the number of normal or low birth weight births according to race, coded age, smoking status smoke, and history of hypertension ht.
attach(birthwt)

m = table(low, race, Agegroup, smoke, ht)

# 3. Calculate the percent of each race, age group, smoking status and hypertension according to low or normal birth respectively.
M = prop.table(m) * 100
sum(M)

# 4. Report the data using a flat table, with low as the column variable and the others as row variables
M2 = melt(M)
M2[order(M2$low, M2$Agegroup),]

detach(birthwt)


####### data.table ########
library(data.table)

D = read.csv("./dataraw/10_mL.csv")
# remove all-missing columns
D = D[, !apply(D, 2, function(x) all(is.na(x)))]
# remove all-missing rows
D = D[!is.na(D$Replicate),]
#D = D[, !apply(D, 1, function(x) all(is.na(x)))] #can put extra condition on empty space to get same result

DT = setDT(read.csv("./dataraw/10_mL.csv"))
# remove all-missing rows
DT = DT[!is.na(Replicate),]
# remove all-missing columns
cols2remove = names(DT)[apply(DT, 2, function(x) all(is.na(x)))]
DT = DT[, (cols2remove):=NULL]  # remove columns

CCcols = c("Cell.Count", grep("X", names(DT), value = TRUE ))
DT$CCavg = apply(DT[, ..CCcols], 1, mean)
#DT = DT[, ':='(CCavg = apply(DT[, ..CCcols], 1, mean)  )] #didn't work
DT = DT[, ':='(ratio = CCavg / OD )]

d = DT[, .(CCavg = mean(CCavg)), by = Strain]
setnames(d, "CCavg", "NewCCavg")


####### dplyr ########
library(dplyr)
dplyr::filter()
dplyr::select()
