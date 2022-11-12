###############################################################################
## MAT02023 - Inferencia A
## Aula 2

## Distribuicao amostral da media
xbarra=vector()
n=5  #tamanho da amostra
Rep=1000 #repeticoes de um experimento 
N=100
# populacao: numeros de 1 a N
# probabilidade P(X=x)=1/N

for(r in 1:Rep){
  x=sample(N,n,replace=TRUE)
  xbarra[r]=mean(x)
}
mean(xbarra) #media do xbarra
var(xbarra)  #Variancia do xbarra
hist(xbarra) #histograma do xbarra
#qual eh a distribuicao do xbarra?


# O que muda com o aumento do tamanho da amostra n?
par(mfrow=c(1,3))


###############################################################################
# Distribuicao amostral da variancia
S2=vector()
n=5  #tamanho da amostra
Rep=10000 #repeticoes de um experimento 
N=100
# populacao: numeros de 1 a N
# probabilidade P(X=x)=1/N

for(r in 1:Rep){
  x=sample(N,n,replace=TRUE)
  S2[r]=var(x)
}
mean(S2) #media do S2
var(S2)  #Variancia do S2
hist(S2) #histograma do S2
#qual eh a distribuicao do S2?




# O que muda com o aumento do tamanho da amostra n?


par(mfrow=c(1,3))