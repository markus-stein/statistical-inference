## CLT for empirical cumulative distribution function (ecdf)                 ##
## Markus Stein
## 15/03/2019
##

## loading packages
library(ggplot2)
library(dplyr)

## Poisson case
# set up
lambda <- 5                 # parameter
x <- -5:20                  # subset of possible x's
Fx <- ppois(x, lambda)      # true cdf
df <- data.frame(x, Fx)     

# true cdf plot
plotcdf <- ggplot(df, aes(x, Fx)) + geom_step() + labs(title="(True) Cumulative \n Distribution Function", y = expression(F[x]), x = "x")
plotcdf

# sampling from poisson
nsim <- 100                 # number of samples to be drawn
n <- 10                     # sample size
xgen <- rpois(n, lambda)
dfgen <- data.frame(xgen)

# empirical cdf plot  
plotecdf <- ggplot(dfgen, aes(xgen)) + stat_ecdf(colour="red", alpha = 0.5) + labs(title="Empirical Cumulative \n Distribution Function", y = expression(F[x]), x = "x")
plotecdf

# plot both true and empirical cdf
plotboth <- ggplot() + geom_step(aes(x, Fx), df) + stat_ecdf(aes(xgen), dfgen, colour = "red", alpha = 0.5) + labs(title="(True) Cumulative \n Distribution Function", y = expression(F[x]), x = "x")
plotboth

# plot one sample incresing n
plot0 <- ggplot() 
xgen <- rpois(n, lambda)
dfgen <- data.frame(xgen, g = 1)
for(i in 1:n){
  # print(plotfixedn + stat_ecdf(aes(xgen), dfgen, colour = "red", alpha = 0.5))
  plotn <- plot0 + stat_ecdf(aes(xgen), dfgen[1:i,], colour = "red", alpha = 0.5)
  print(plotn + addtrue + labs(title=paste("Cumulative Distribution Function - n = ", i), y = expression(F[x]), x = "x"))
}

# plot multiple samples with fixed n
plotfixedn <- ggplot() 
addtrue <- geom_step(aes(x, Fx), df)
xgen <- rpois(n * nsim, lambda)
dfgen <- data.frame(xgen, g = factor(rep(1:nsim, rep(n, nsim))))
for(i in 1:nsim){
  # print(plotfixedn + stat_ecdf(aes(xgen), dfgen, colour = "red", alpha = 0.5))
  plotfixedn <- plotfixedn + stat_ecdf(aes(xgen), dfgen[1:(i*n),], colour = "red", alpha = 0.2)
  print(plotfixedn + addtrue + labs(title=paste("Cumulative Distribution Function \n", i, " samples"), y = expression(F[x]), x = "x"))
}


# ggplotly... gganimate...
