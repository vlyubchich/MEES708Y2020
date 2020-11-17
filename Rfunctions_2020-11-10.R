swiss
lm(formula = Agriculture ~ Examination, data = swiss)

lm(Agriculture ~ Examination, swiss)

lm(swiss, Agriculture ~ Examination) #error -- entry type does not match expected for those positions

lm(data = swiss, formula = Agriculture ~ Examination)

#VL
lm(Agriculture ~ Examination, data = swiss)

lm(Agriculture ~ Examination, data = swiss, nonsense = TRUE)


#Develop new function
source('MyFunctions2020.R')

x = swiss$Agriculture
cut = 0.05
# hist(x)
# abline(v = c(upr, lwr))

# x = swiss[,1:2]
truncmean(x, stat = "med")
truncmean(x)

truncmean(x, col = "blue", main = "")





