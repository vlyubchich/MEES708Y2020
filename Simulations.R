rm(list = ls())

?rnorm

rnorm(10) # X ~ N(0, 1) #N(0, 1) is the standard (mean = 0, variance = 1)
rnorm(10)

set.seed(123)
rnorm(10)
set.seed(123)
rnorm(15)

###### 1 Simul. from distrib ######
?distribution
rnorm(10, mean = 10)
tmp = rnorm(100000) + 10
mean(tmp)
sd(tmp)

rpois(10, 2)

library(gamlss)
?gamlss.family

library(gamlss.tr)
?gamlss.tr
gen.trun(par = c(0, 100), family = "LOGNO", type = "both") #dLOGNOtr pLOGNOtr qLOGNOtr rLOGNOtr LOGNOtr
gamlss(y ~ x, family = "LOGNOtr")

#### 2 Simulate from the model ####
####2.1 Simple linear regression ####
n <- 1000 #Sample size
x <- rnorm(n) #independent variable
y <- 2*x + rnorm(n) #dependent variable
#DGP: y = 2x + e, e~i.i.d. N(0,1)
out <- lm(y ~ x)
summary(out)

#### 2.2 Generalized linear model -- GLM (Poisson model) ####
beta0 <- 0.1
beta1 <- 0.5
logY <- beta0 + beta1*x
#Y <- exp(logY) #do not get integers
Y <- rpois(n, exp(logY))
plot(x, Y)

out2 <- glm(Y ~ x, family = "poisson")
summary(out2)

#sapply(c(1:(K/2)), function(x) arima.sim(n=n+100, list(order=c(length(phi1),0,0),ar=phi1)))[101:(n+100),]

#### 2.3 ARIMA time series ####
phi <- 0.8
tmp <- arima.sim(n, model = list(order = c(1, 0, 0), ar = phi))
ts.plot(tmp)
plot(tmp)

#Need "burn-in"
tmp <- arima.sim(n+100, model=list(order=c(1,0,0), ar=phi))[101:(n+100)]
ts.plot(tmp)

acf(tmp)

#estimate
ar(tmp)
arima(tmp) #need to specify the model
forecast::auto.arima(tmp)

##### 3. More simulations #####
MC <- 10000 #Number of Monte Carlo simulations
M <- matrix(NA, nrow = n, ncol = MC)
system.time({
    for(mc in 1:MC){
        M[,mc] <- arima.sim(n+100, model=list(order=c(1,0,0), ar=phi))[101:(n+100)]
    }
})
#M
dim(M)

system.time({
    M2 <- sapply(1:MC, function(x) arima.sim(n+100, model=list(order=c(1,0,0), ar=phi))[101:(n+100)])
})
#M2

v = double()
set.seed(123)
system.time({
    for (mc in 1:MC) {
        #do not save all data
        m <- arima.sim(n+100, model=list(order=c(1,0,0), ar=phi))[101:(n+100)]
        #estimate and save only estimates into the vector
        v[mc] = ar(m, order.max = 1)$ar
    }
})
hist(v)
abline(v = phi, col = 2, lwd = 6)
quantile(v, probs = c(0.025, 0.975))


#### Sample and bootstrap ####
set.seed(123)
X <- rexp(20, 0.5) #this is our sample
hist(X, col = "blue")

#Find the mean and get 95% conf. interval
#Use parametric assumption -- mean is distributed N(xbar, sd(x)/sqrt(n))

xbar <- mean(X) #mean of observations
n <- length(X) #sample size
sd_xbar <- sd(X)/sqrt(n) #standard deviation of the mean

qnorm(c(0.025, 0.975), mean=xbar, sd=sd_xbar) #95% CI using normal

xbar + qnorm(c(0.025, 0.975), sd=sd_xbar)

xbar + qnorm(c(0.025, 0.975))*sd_xbar


#If we doubt Normal, use t
xbar + qt(c(0.025, 0.975), df=n-1)*sd_xbar
#small sample, t-distribution gives wider interval than Normal.

#Intro to sample() function
sample(c(1:5)) #permutation
sample(c(1:5), 3) #subsample
sample(c(1:5), 3, replace=TRUE) #subsample with replacement -- used in m-out-of-n bootstrap

sample(c(1:5), replace=T) #resample with replacement -- used in bootstrap

B <- 1000 #number of bootstrap resamples
#Code1
Bmeans <- numeric() #Create an empty vector to store bootstrapped means
for(b in 1:B){
    Bmeans[b] <- mean(sample(X, replace = TRUE))
}
hist(Bmeans, col=2) #distribution of bootstrapped means
quantile(Bmeans, probs = c(0.025, 0.975)) #95% bootstrap confidence interval for the mean

#Code2
Bmeans2 <- sapply(1:B, function(b) mean(sample(X, replace=TRUE)))
hist(Bmeans2, col=2) #distribution of bootstrapped means
quantile(Bmeans2, probs=c(0.025, 0.975))

#Loop functions
l1 <- list(a = c(1,2,3), b = 1:10, c = rnorm(10))
l1
lapply(l1, mean)

sapply(l1, mean)


lapply(l1, quantile, probs=c(0.025, 0.975))
sapply(l1, quantile, probs=c(0.025, 0.975))

apply(M2, 1, sum) #rowsums

M <- array(rnorm(10^3), dim=c(10,10,10))

apply(M, c(1,3), mean)
apply(M, c(1), mean)

mapply(rep, 1:4, 4:1)


F1 <- factor(c(rep("smoke", 10), rep("NoSmoke", 10)))
BP <- rnorm(20, 80)

tapply(BP, F1, mean)
tapply(BP, F1, function(x) sd(x)/sqrt(length(x)) ) #sd for the mean per group










