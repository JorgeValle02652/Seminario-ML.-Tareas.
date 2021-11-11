#Valle Luna Jorge Alberto. 315076183.

set.seed(100)
setwd("C:/Users/Tyler/Desktop/Seminario ML")

base<-read.csv("base_final.csv", header=TRUE)
base<-base[ , !(names(base) %in% c("X", "X_id_"))]
names(base)
base$comp_interno2[base$comp_interno2=="b'0'"]<-0
base$comp_interno2[base$comp_interno2=="b'1'"]<-1

base$comp_interno2<-as.factor(base$comp_interno2)
#Recordemos que la variable 'categoria1' la interpretamos como la prioridad del cliente para el banco.
#Los del tipo1 son la poblacion mayoritaria de toda la cartera, despues estan los tipo 2, y asi hasta
#los tipo6. Por lo tanto esta variable es ordinal. Este banco prefiere prestarle a alguien de una poblacion
#significativa. Si pierde un cliente de tipo6 por no concederle un prestamo, no pierde gran cosa.
base$categoria1<-as.factor(base$categoria1)
base$categoria1<-relevel(base$categoria1, ref="1")

base$categoria2<-as.factor(base$categoria2)
base$comp_externo4<-as.factor(base$comp_externo4)

str(base)

#Veamos que hay 876 clientes a los que se les niega credito, y 42946 a los que se les concede:

table(base$credito_1si_0no)

# Para entrenar nuestro modelo logit, tomaremos una muestra aleatoria del 70% de 876 observaciones
# para cada uno de los valores de la columna 'credito_1si_0no'.
# De este modo nuestra muestra de entrenamiento contendra la misma cantidad de observaciones
# con prestamo que la cantidad de observaciones sin prestamo.


#Generamos la muestra de entremaniento:

input_ones <- base[which(base$credito_1si_0no == 1), ]  # all 1's
input_zeros <- base[which(base$credito_1si_0no == 0), ]  # all 0's
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)

# El conjunto de test es lo que resta de la base original, despues de obtener la muestra de entrenamiento.
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 


# Investigando al respecto, para regresion logistica binaria, existe un metodo llamado 
#Information Value (IV), que consiste en obtener un numero mayor que 0, que de acuerdo a la escala
#(0, 0.02) <-bad predictor
#[0.02, 0.1)<-weak predictor
#[0.1, 0.3)<-medium predictor
#[0.3, 0.5)<-strong predictor
#[0.5, 1)<- suspicious behaviour

#nos dice que tan buen predictor es una variable explicativa. Esto se hace para saber que
#variables explicativas incluir en el modelo de regresion logistica.

#La informacion del IV la encontre en la pagina:
#https://medium.com/mlearning-ai/weight-of-evidence-woe-and-information-value-iv-how-to-use-it-in-eda-and-model-building-3b3b98efe0e8

#Para obtener el IV de cada variable explicativa, utilizamos el metodo 'smbinning'
# del paquete 'smbinnig':

#install.packages("smbinning")
library(smbinning)

# Separemos variables explicativas por clasificacion: continua o tipo factor

factor_vars <- c("comp_interno2", "categoria1", "categoria2", "comp_externo4")
continuous_vars <- c("comp_interno1", "comp_interno3", "comp_interno4", "comp_externo1", "comp_externo2", "comp_externo3", "comp_mixto1")

#Notamos que este metodo no funciona para algunas variables:

# 1) "categoria2". Este metodo agrupa las observaciones por niveles de la 'categoria2'.
# los niveles de categoria2 son 13. Para el metodo smbinnig, 13 niveles son muchos.
# Como la 'categoria2' es nominal, no hay forma de reducir los niveles.
# Por tanto se elimina.
length(levels(trainingData$categoria2))
factor_var="categoria2"
smbinning.factor(trainingData, y="credito_1si_0no", x=factor_var)

# 2) "comp_interno1". Para variables continuas, el metodo no encuentra una forma significativa
# para partir los datos, de acuerdo a la variable, tal que se observe una diferencia entre 
# buenos (credito_1si_0no=1), y malos (credito_1si_0no=1).
factor_var="comp_interno1"
smbinning(trainingData, y="credito_1si_0no", x=factor_var)


# 3) "comp_interno4". Ocurre lo mismo que para 2).
factor_var="comp_interno4"
smbinning(trainingData, y="credito_1si_0no", x=factor_var)

# 4) "comp_externo1". Ocurre lo mismo que para 3). A veces si hay
# IV, pero no pasa de 0.15. Hay otras veces que devuelve "No significant splits":

# Al extraer el valor IV de la funcion smbinning: smbinning(...)$iv, se regresa
# el error "$ operator is invalid for atomic vectors".
# Esto se debe a que se esta dando la instruccion "No significant splits"$iv.
# Al final, se elimina "comp_externo1".
continuous_var="comp_externo1"

n<-10
for (k in 1:n){
  
  input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  
  input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))
  training_ones <- input_ones[input_ones_training_rows, ]  
  training_zeros <- input_zeros[input_zeros_training_rows, ]
  trainingData <- rbind(training_ones, training_zeros)
  print(smbinning(trainingData, y="credito_1si_0no", x=continuous_var)$iv)
}

# 5) "comp_interno3". Ocurre lo mismo que para 4). A veces si hay
# IV, pero no pasa de 0.15. Hay otras veces que devuelve "No significant splits":

# Al extraer el valor IV de la funcion smbinning: smbinning(...)$iv, se regresa
# el error "$ operator is invalid for atomic vectors".
# Esto se debe a que se esta dando la instruccion "No significant splits"$iv.
# Al final, se elimina "comp_interno3".

continuous_var <- "comp_interno3"
n<-10
for (k in 1:n){
  
  input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  
  input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))
  training_ones <- input_ones[input_ones_training_rows, ]  
  training_zeros <- input_zeros[input_zeros_training_rows, ]
  trainingData <- rbind(training_ones, training_zeros)
  print(smbinning(trainingData, y="credito_1si_0no", x=continuous_var)$iv)
}

# Al final, nuestra clasificacion queda como:

factor_vars <- c("comp_interno2", "categoria1", "comp_externo4")
continuous_vars <- c("comp_externo2", "comp_externo3", "comp_mixto1")


# Calculamos el IV para cada variable explicativa, para 10 trainingData distintos:

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars))
n<-10

for (k in 1:n){
  d<-c()
  input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  
  input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))
  training_ones <- input_ones[input_ones_training_rows, ]  
  training_zeros <- input_zeros[input_zeros_training_rows, ]
  trainingData <- rbind(training_ones, training_zeros)
  
  # compute IV for categoricals
  for(factor_var in factor_vars){
    smb <- smbinning.factor(trainingData, y="credito_1si_0no", x=factor_var)
    if(class(smb) != "character"){
      d <- c(d, smb$iv)
    }
  }
  
  # compute IV for continuous vars
  for(continuous_var in continuous_vars){
    smb <- smbinning(trainingData, y="credito_1si_0no", x=continuous_var) 
    if(class(smb) != "character"){  # any error while calculating scores.
      d <- c(d, smb$iv)
    }
  }
  
  
  iv_df[as.character(k)]<-d
}

iv_df$avg<-rowMeans(iv_df[,-1])



#La ultima columna representa el promedio del valor de IV para cada variable.
#Cuando simule, obtuve:

#"comp_interno2"   0.00147 
#"categoria1"      0.19775
#"comp_externo4"   0.13927
#"comp_externo2"   0.20144
#"comp_externo3"   0.94136
#"comp_mixto1"     0.35602

#Asi, mi modelo lo entrenare con todas las variables, excepto "comp_interno2" y "comp_externo4"


#Con la libreria InformationValue, se obtiene la funcion "misClassError" la cual 
#nos indica la tasa de error de prediccion de nuestro modelo, con respecto a la muestra de test.
library(InformationValue)

#Con la funcion random_split, obtenemos la tasa de error del modelo seleccionado
# variando la muestra de training y la muestra de test:

input_ones <- base[which(base$credito_1si_0no == 1), ]  # all 1's
input_zeros <- base[which(base$credito_1si_0no == 0), ]  # all 0's

random_split<-function(){
  input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  
  input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))
  training_ones <- input_ones[input_ones_training_rows, ]  
  training_zeros <- input_zeros[input_zeros_training_rows, ]
  trainingData <- rbind(training_ones, training_zeros)
  
  # El conjunto de test es lo que resta de la base original, despues de obtener la muestra de entrenamiento.
  test_ones <- input_ones[-input_ones_training_rows, ]
  test_zeros <- input_zeros[-input_zeros_training_rows, ]
  testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's
  
  logitMod <- glm(credito_1si_0no ~ comp_externo3 + comp_mixto1 + comp_externo2 + categoria1, data=trainingData, family=binomial(link="logit"))
  if(length(logitMod$xlevels$categoria1)!=length(levels(testData$categoria1))){
    input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  
    input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))
    training_ones <- input_ones[input_ones_training_rows, ]  
    training_zeros <- input_zeros[input_zeros_training_rows, ]
    trainingData <- rbind(training_ones, training_zeros)
    
    logitMod <- glm(credito_1si_0no ~ comp_externo3 + comp_mixto1 + comp_externo2 + categoria1, data=trainingData, family=binomial(link="logit"))
  }
  
  predicted <- plogis(predict(logitMod, testData))
  
  return(misClassError(testData$credito_1si_0no, predicted, threshold = 0.5))
}

n<-100

p_2_5<-quantile(replicate(n, random_split()), probs=c(0.025))
p_97_5<-quantile(replicate(n, random_split()), probs=c(0.975))

hist(replicate(n, random_split()), main="Tasa de error de prediccion del modelo", col="blue")
abline(v = p_2_5, col="red", lwd=3, lty=2)
abline(v = p_97_5, col="red", lwd=3, lty=2)


#Para un threshold=0.5, es decir que los valores que arroja el modelo por encima de 0.5 se van a 1,
#y los que no, se van a 0, tenemos una tasa de error en el intervalo (28.5%, 36%).

