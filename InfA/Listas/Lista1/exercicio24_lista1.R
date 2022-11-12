########## MAT02023 - Inferência A - 2019/1
########## Exercício 24 da lista 1

##### a)
## Poisson case
# set up
lambda <- 5                 # parametro
n <- 10                     # tamanho da amostra
xgen <- rpois(n, lambda)    # amostra gerada
dfgen <- data.frame(xgen)

##### b)
# empirical cdf plot  
plotecdf <- ggplot(dfgen, aes(xgen)) + 
            stat_ecdf(colour="red", alpha = 0.5) + 
            labs(title="Empirical Cumulative \n Distribution Function", 
                 y = expression(F[x]), x = "x")
plotecdf

##### c)
# plot multiple samples with fixed n
n <- 10
nsim <- 10
lambda <- 5
xgen <- rpois(n * nsim, lambda)
dfgen <- data.frame(xgen, g = factor(rep(1:nsim, rep(n, nsim))))
plotfixedn <- ggplot() 
for(i in 1:nsim){
  # print(plotfixedn + stat_ecdf(aes(xgen), dfgen, colour = "red", alpha = 0.5))
  plotfixedn <- plotfixedn + stat_ecdf(aes(xgen), dfgen[dfgen$g==i,], 
                                       colour = "red", alpha = 0.2)
}
plotfixedn + labs(title=paste("Cumulative Distribution Function \n", nsim, 
                              " samples of size ",n), y = expression(F[x]), 
                  x = "x")


##### d)
# funcao para calfular Fn(x) de 'nsim' amostras geradas de tamanho 'n'
Fnx <- function(n, x, nsim){
  # n    - tamanho da amostra
  # x    - particular valor da Poisson
  # nsim - número de simulacoes
  
  Fnxgen <- NULL                # objeto para guardar os Fn(x)'s
  for(i in 1:nsim){
    xgen <- rpois(n, lambda)    # gera amostra
    Fn <- ecdf(xgen)            # calcula CDF
    Fnxgen <- c(Fnxgen, Fn(5))   # armazena Fn(x)
  }
  hist(Fnxgen, main=paste("tamanho amostra n =", n), xlab=paste("Fn(",x,")"))
  abline(v=ppois(5, lambda), col="red")     # F(x) real
}
par(mfrow=c(2,2))
Fnx(10, 5, 100)
Fnx(100, 5, 100)
Fnx(1000, 5, 100)
Fnx(10000, 5, 100)
