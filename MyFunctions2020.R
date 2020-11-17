# x = swiss$Agriculture
# cut = 0.05
# hist(x)
# abline(v = c(upr, lwr))
#
# truncmean(x)

truncmean = function(x, cut = 0.05,
                    statistic = c("mean", "median"), ...
                    )
{
    # require(babynames)
    # babynames::applicants
    if (cut >= 1) {stop("cut should be <1")}
    statistic = match.arg(statistic)
    lwr = quantile(x, cut / 2)
    upr = quantile(x, 1 - cut / 2)
    xtruc = x[x > lwr & x < upr]
    hist(x, ...)
    abline(v = c(lwr, upr), lty = 2)
    if (statistic == "mean") {
        print(paste0("Trunctated mean by ", cut*100, "% is:"))
        return(mean(xtruc))
    } else {
        print(paste0("Trunctated median by ", cut*100, "% is:"))
        return(median(xtruc))
    }
}


fpower <- function(x, p = 2){
    x ^ p
}
