set.seed(100)


################PASO 1. CARGAMOS Y LIMPIAMOS LA BASE:

setwd("C:/Users/Tyler/Desktop/Seminario ML")

limpieza<-function(){
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
  
  #La base tiene 6 observaciones con missing en la variable comp_externo3:
  which(is.na(base$comp_externo3))
  
  #Al ser solo 6 las eliminamos
  base<-base[-c(6388, 20927, 34053, 35874, 38653, 42935),]
  return(base)
}

base<-limpieza()

str(base)
################PASO 2. Discretizamos todas las variables explicativas de la base de la manera
#más óptima posible según nuestra variable de respuesta "credito_1si_0no":

library(varbin)

#Con esta funcion 'discretizar()'obtenemos para cada observacion,
#los WOES correspondientes a cada variable explicativa.

discretizar<-function(){
  result<-varbin(base, x="comp_interno1", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_interno1")
  
  result<-varbin.factor(base, x="comp_interno2", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_interno2")
  
  result<-varbin(base, x="comp_interno3", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_interno3")
  
  result<-varbin(base, x="comp_interno4", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_interno4")
  
  result<-varbin.factor(base, x="categoria1", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="categoria1")
  
  result<-varbin.factor(base, x="categoria2", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="categoria2")
  
  result<-varbin(base, x="comp_externo1", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_externo1")
  
  result<-varbin(base, x="comp_externo2", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_externo2")
  
  result<-varbin(base, x="comp_externo3", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_externo3")
  
  result<-varbin.factor(base, x="comp_externo4", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_externo4")
  
  result<-varbin(base, x="comp_mixto1", y="credito_1si_0no")
  base<-varbin.convert(base, result, x="comp_mixto1")
  
  return(base[,seq(12,23,1)])
  
}

base<-discretizar()

write.csv(base,"C:/users/Tyler/AppData/Local/Programs/Python/Python38/Scripts/seminario ml/Tarea 2/base_WOES.csv", row.names = FALSE)


