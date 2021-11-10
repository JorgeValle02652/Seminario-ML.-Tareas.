set.seed(800)
setwd("C:/Users/Tyler/Desktop/Seminario ML")

library(readxl)

base<-read_excel("lineal_horm.xlsx", col_names=TRUE)

x<-base$horas
y<-base$monto
#Numero de muestras bootstrap
n<-10000
m<-length(x)

intercept=c()
sdintercept_boot=c()
b1=c()
sdb1_boot=c()
for (i in 0:n){
  x_b<-sample(x, m, replace=TRUE)
  y_b<-sample(y, m, replace=TRUE)
  model<-lm(y_b~x_b)
  
  sdintercept_boot<-c(sdintercept_boot, unname(sqrt(diag(vcov(model))))[1])
  intercept<-c(intercept, unname(model$coefficients)[1])
  sdb1_boot<-c(sdb1_boot, unname(sqrt(diag(vcov(model))))[2])
  b1<-c(b1, unname(model$coefficients)[2])
  
}

par(mfrow=c(1,2))

#Para el histograma de la desviacion estandar del intercepto, notamos una distribucion
#normal:
hist(sdintercept_boot, main=paste("error estandar b0  n= ", n, " m=", m), breaks=100, col="red")

#Para el histograma de la desviacion estandar de b1, notamos una distribucion
#normal:
hist(sdb1_boot, main=paste("error estandar b1  n= ", n, "  m= ",m), breaks=100, col="green")

#Ahora graficamos los histogramas de las betas:

par(mfrow=c(2,2))

######## b0 ########

#Calculamos un intervalo empirico de confianza al 95%, para el intercepto:
q_95<-qnorm(0.95)
left<-mean(intercept)-q_95*mean(sdintercept_boot)
right<-mean(intercept)+q_95*mean(sdintercept_boot)

#Ahora hacemos el histograma del intercepto:

hist(intercept, main=paste("n= ", n, "m= ", m, "  b0"), breaks=100, freq=FALSE, col="orange")
#Calculamos el cuantil de la muestra, al 2.5%, y al 97.5$
p_2_5<-quantile(intercept, probs=c(0.025))
p_97_5<-quantile(intercept, probs=c(0.975))
#comparamos los cuantiles de nuestra muestra, con el intervalo empirico que obtuvimos:
abline(v = p_2_5, col="red", lwd=3, lty=2)
abline(v = left, col="blue", lwd=3, lty=2)
abline(v = p_97_5, col="red", lwd=3, lty=2)
abline(v = right, col="blue", lwd=3, lty=2)

curve(dnorm(x, mean(intercept), mean(sdintercept_boot)), from=18, to=32, add=TRUE, lwd=3)


######## b1 ########
q_95<-qnorm(0.95)
left<-mean(b1)-q_95*mean(sdb1_boot)
right<-mean(b1)+q_95*mean(sdb1_boot)

hist(b1, main=paste("n= ", n, "m= ", m,  " b1"), breaks=100, freq=FALSE, col="skyblue")

p_2_5<-quantile(b1, probs=c(0.025))
p_97_5<-quantile(b1, probs=c(0.975))
#comparamos los cuantiles de nuestra muestra, con el intervalo empirico que obtuvimos:
abline(v = p_2_5, col="red", lwd=3, lty=2)
abline(v = left, col="blue", lwd=3, lty=2)
abline(v = p_97_5, col="red", lwd=3, lty=2)
abline(v = right, col="blue", lwd=3, lty=2)

curve(dnorm(x, mean(b1), mean(sdb1_boot)), from=-0.04, to=0.04, add=TRUE, lwd=3)


#Para concluir, ahora comparemos cuando cada muestreo bootstrap es de m=10:

#Numero de muestras bootstrap
n<-10000
#Tamanio de cada muestra bootstrap
m<-10
intercept=c()
sdintercept_boot=c()
b1=c()
sdb1_boot=c()
for (i in 0:n){
  x_b<-sample(x, m, replace=TRUE)
  y_b<-sample(y, m, replace=TRUE)
  model<-lm(y_b~x_b)
  
  sdintercept_boot<-c(sdintercept_boot, unname(sqrt(diag(vcov(model))))[1])
  intercept<-c(intercept, unname(model$coefficients)[1])
  sdb1_boot<-c(sdb1_boot, unname(sqrt(diag(vcov(model))))[2])
  b1<-c(b1, unname(model$coefficients)[2])
  
}

######## b0 ########

#Calculamos un intervalo empirico de confianza al 95%, para el intercepto:
q_95<-qnorm(0.95)
left<-mean(intercept)-q_95*mean(sdintercept_boot)
right<-mean(intercept)+q_95*mean(sdintercept_boot)

#Ahora hacemos el histograma del intercepto:

hist(intercept, main=paste("n= ", n, " m= ", m, "  b0"), breaks=100, freq=FALSE, col="peachpuff4")
#Calculamos el cuantil de la muestra, al 2.5%, y al 97.5$
p_2_5<-quantile(intercept, probs=c(0.025))
p_97_5<-quantile(intercept, probs=c(0.975))
#comparamos los cuantiles de nuestra muestra, con el intervalo empirico que obtuvimos:
abline(v = p_2_5, col="red", lwd=3, lty=2)
abline(v = left, col="blue", lwd=3, lty=2)
abline(v = p_97_5, col="red", lwd=3, lty=2)
abline(v = right, col="blue", lwd=3, lty=2)

curve(dnorm(x, mean(intercept), mean(sdintercept_boot)), from=10, to=40, add=TRUE, lwd=3)



######## b1 ########

q_95<-qnorm(0.95)
left<-mean(b1)-q_95*mean(sdb1_boot)
right<-mean(b1)+q_95*mean(sdb1_boot)

hist(b1, main=paste("n= ", n, "m= ", m, "  b1"), breaks=100, freq=FALSE)

p_2_5<-quantile(b1, probs=c(0.025))
p_97_5<-quantile(b1, probs=c(0.975))
#comparamos los cuantiles de nuestra muestra, con el intervalo empirico que obtuvimos:
abline(v = p_2_5, col="red", lwd=3, lty=2)
abline(v = left, col="blue", lwd=3, lty=2)
abline(v = p_97_5, col="red", lwd=3, lty=2)
abline(v = right, col="blue", lwd=3, lty=2)

curve(dnorm(x, mean(b1), mean(sdb1_boot)), from=-0.1, to=0.1, add=TRUE, lwd=3)

#La conclusion es que la media de ambas betas no varia, pero si aumenta la varianza, al pasar
#de m=27 (la muestra completa), a m=10.
