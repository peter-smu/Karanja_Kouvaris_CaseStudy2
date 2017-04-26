# install time-series package
install.packages("tseries")

#load the library
library(tseries)

# get historical data
LNNdata <- get.hist.quote('LNN', quote= "Close")
length(LNNdata)

#calculate logs returns
LNNret <- log(lag(LNNdata)) - log(LNNdata)
length(LNNret)

#calculate volatility measure sfor the entire dataset
LNNvol <- sd(LNNret) * sqrt(250) * 100
LNNvol

#Calculate volatility and a continuous lookback window using various decay factors
Vol <- function(d, logrets) {
  var = 0
  lam = 0
  varlist <- c()
  for (r in logrets) {
    lam = lam*(1 - 1/d) +1
    var = (1 - 1/lam) * var + (1/lam) * r^2
    varlist <- c(varlist,var)
  }
  sqrt(varlist)
}

volest <- Vol(10,LNNret)
volest

volest2 <- Vol(30,LNNret)
volest2

volest3 <- Vol(100,LNNret)
volest3

#Plot the results.
plot(volest,type="l")

lines(volest2,type="l”, col="red")

lines(volest3, type = "l", col="blue")