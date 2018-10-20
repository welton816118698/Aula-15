                    #Aula 14 - Quebra Estrural e Bolhas
remove.packages("readxl")
install.packages("readxl", dependencies = T)
install.packages("strucchange")
remove.packages("aTSA")
install.packages("aTSA", dependencies = T)

library(strucchange)
library(readxl)
library(aTSA)
library(tseries)
library("urca") 

setwd("C:/Econometria/15-Modelos_ARCH_GARCH-master")

BITCOIN <- na.omit(read_excel("C:/Econometria/15-Modelos_ARCH_GARCH-master/Bitcoin.xlsx"))

Bitcoin <-  ts(BITCOIN$Close, start = 2017, frequency = 365)

plot(Bitcoin)

#Verificar se a Série é Estacionária

#Criar FAC  e FACP

acf(BITCOIN$Close,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(BITCOIN$Close,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
ur.df(Bitcoin, "none", lags = 1)

#Teste Philips-Perron
pp.test(Bitcoin)

#Teste KPSS
kpss.test(Bitcoin)

#Se não for estacionária, diferenciar a série

IntOrdem1 <- diff(BITCOIN$Close)
IntegradaOrdem1 <- ts(IntOrdem1, start = 2014, frequency = 365)
plot(IntegradaOrdem1)

#Verificar se a Série se tornou Estacionária

#FAC e FACP

acf(IIntOrdem1,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(IntOrdem1,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
ur.df(IntegradaOrdem1, "none", lags = 1)

#Teste Philips-Perron
pp.test(IntegradaOrdem1)

#Teste KPSS
kpss.test(IntegradaOrdem1)


#Verificar quais ordens são as melhores

#Estimando Regressões e Tabelando Resultados

est1 <- data.frame()
for (i in 1:1) {                      #Loop para os AR: ARIMA(i,0,0)
  est1[i,1] <- paste("AR",i)      #Coluna com os nomes do Modelo
  est1[i,2] <- AIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores AIC
  est1[i,3] <- BIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores BIC
}

est2 <- data.frame()                        #Loop para os MA: ARIMA(0,0,i)
for (i in 0:25) {
  est2[i,1] <- paste("MA",i) 
  est2[i,2] <- AIC(arima(IntegradaOrdem1,  order = c(0,1,i)))
  est2[i,3] <- BIC(arima(IntegradaOrdem1,  order = c(0,1,i)))
  
}

est3 <- data.frame()                        #Loop para OS ARMA p=1: ARIMA(1,0,i)
for (i in 1:25) {
  est3[i,1] <- paste("ARMA",1,0,i) 
  est3[i,2] <- AIC(arima(IntegradaOrdem1,  order = c(1,1,i)))
  est3[i,3] <- BIC(arima(IntegradaOrdem1,  order = c(1,1,i)))
  
}


Resultados <- data.frame(rbind(est1,est2,est3,est4))  
colnames(Resultados) <- c("Modelo","AIC","BIC")

#Efetuar teste ARCH-LM para o melhor modelo

arch.test(melhor_modelo)

#Modelando a Variância

