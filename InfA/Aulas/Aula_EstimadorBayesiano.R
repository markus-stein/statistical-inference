###################################################################
#                                                                 #
#              MAT02023 - Inferencia A                            #
#  Semestre 2018/1 - Professora Patrecia Klarmann Ziegelmann      #
#                     Aula 14                                     #
#                                                                 #
###################################################################

################### Nucleos de Distribuicoes   ####################
par(mfrow=c(2,2))
teta <- seq(0,1,0.01)

alfa=2
beta=8

Beta= (gamma(alfa+beta)/ (gamma(alfa)*gamma(beta))) * teta^(alfa-1) * (1-teta)^(beta-1)
plot(teta,Beta,type="l")
title("Beta(2,8)")

nucleo=teta^(alfa-1) * (1-teta)^(beta-1)
plot(teta,nucleo,type="l")
title("Nucleo Beta (2,8)")

alfa=8
beta=2

Beta= (gamma(alfa+beta)/ (gamma(alfa)*gamma(beta))) * teta^(alfa-1) * (1-teta)^(beta-1)
plot(teta,Beta,type="l")
title("Beta(8,2)")

nucleo=teta^(alfa-1) * (1-teta)^(beta-1)
plot(teta,nucleo,type="l")
title("Nucleo Beta (8,2)")


############  Verossimilhança Padronizada   #####################
par(mfrow=c(2,2))
teta <- seq(0,5,0.01)

amostra=c(1,2,1,0,2)
n=length(amostra)
soma=sum(amostra)
produto=prod(factorial(amostra))

vero=(exp(-n*teta)*teta^soma)/produto
plot(teta,vero,type="l",col="blue")
title("Verossimilhanca")

verop=dgamma(teta,soma+1,n)
plot(teta,verop,type="l")
title("Verossimilhança Padronizada")

plot(c(0,5),c(0,0.8),type="n")
lines(teta,vero,col="blue")
lines(teta,verop)



######################     Grafico Triplot ######################

#Modelo Poison

par(mfrow=c(2,2))
teta <- seq(0,5,0.01)

#Priori Gama(a,b)
a=2
b=5

priori = b^a/gamma(a) * teta^(a-1) * exp(-b*teta)
plot(teta,priori,type="l",col="blue")
title("Priori Gama(2,5)")


#Verossimilhanca
amostra=c(1,2,1,0,2)
n=length(amostra)
mean(amostra)
soma=sum(amostra)

verop=dgamma(teta,soma+1,n)
plot(teta,verop,type="l")
title("Verop - Média amostra=1,2")


#Posteriori
posteriori=dgamma(teta,soma+a,n+b)
plot(teta,posteriori,type="l",col="red")
title("Posteriori Gama(8,10)")

#TRIPLOT
plot(c(0,5),c(0,2),type="n")
lines(teta,priori,col="blue")
lines(teta,verop)
lines(teta,posteriori,col="red")
title("TRIPLOT")
