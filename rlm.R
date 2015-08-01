
library(readxl)
dir<-"C:/Users/TOSHIBA/Documents/David/Git Reposotorio R/Regresion-Lineal-Multiple"
setwd(dir)
#2.1.-Leer los archivos
pobla1<-read_excel("poblacion1.xlsx",sheet=1,na="")
pobla2<-read_excel("poblacion2.xlsx",sheet=1,na="")

View(pobla1)
View(pobla2)
#Analizar dimensiones
n1<-dim(pobla1)

n2<-dim(pobla2)
#2.2.-Unir los dos archivos en un objeto llamado poblacion
poblacion<-merge(pobla1,pobla2,by="identificador",suffixes=c("",""))
View(poblacion)
#2.2.-Generar diagramas de cajas e histogramas


for(j in 2:dim(poblacion)[2]){
    if(is.numeric(poblacion[,j])==TRUE){
      boxplot(poblacion[,j])
    }else {
      #if(is.factor(p[,j])==TRUE){
      barplot(table(poblacion[,j]))
    }
}

#2.3.-Escribir codigo que calcule automaticamente media, max,min,frec,cuantile

for(j in 2:dim(poblacion)[2]){
    if(is.numeric(poblacion[,j])==TRUE){
      
      print(names(poblacion)[j])
      print(class(poblacion[,j]))
      
      print(max(poblacion[,j]))
      print(min(poblacion[,j]))
      print(mean(poblacion[,j]))
      print(sd(poblacion[,j]))
      print(quantile(poblacion[,j],probs=seq(0,1,0.25),na.rm = FALSE))
    }else {
      
      print(names(poblacion)[j])
      print(class(poblacion[,j]))
      print(table(poblacion[,j])/dim(poblacion)[1])
    }
    
}


#2.4.- Calcular la correlacion entre la poblacion y las otras variables
for(i in 3:dim(poblacion)[2]){
  if(is.numeric(poblacion[,i])==TRUE){
  correla<-cor(poblacion[,2],poblacion[,i])
  print(correla)
  }
}

#2.5.-Se puede decir que las medias de la varialbe poblacion son distintas en los grupos serv.bas.compl
    #Primero modificamos la data, para transformar a "factor las columnas region y serv.bas..." para poder usar el test t(student)
  servicios<-factor(poblacion[,"serv.bas.compl"],levels=c("SI","NO"),labels=c("si","no"))
  region<-factor(poblacion[,"region"],levels=c("A","B"),labels=c("a","b"))
  poblacion_fac<-data.frame(poblacion[,1:7],region,servicios)

    #Luego hacemos un grafico de cajas para observar como se comporta la "poblacion" en los dos grupos
  plot(poblacion ~ servicios , data = poblacion_fac) 
    #Y finalmente realizamos la prueba de hipotesis para la diferencia de medias
  t.test(poblacion ~ servicios , data = poblacion_fac, conf.level=0.9)

#2.6.- Regresion Linela Multiple, de la varialbe independiente poblacion con los regresores mas significativos
#tomando los tres regresores de mayor correlacion con la poblacion

regre<-lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen,data=poblacion_fac)

#2.7.-Interpretan el R2
summary(regre)["r.squared"]

#2.8.-Analice la significancia de la regresion
summary(aov(regre))

#2.9.-Analice los residuos

residuos<-1:40  #almacenamos los residuos en el vector "resid"
for(i in 1:40){
  residuos[i]<-summary(regre)[["residuals"]][i]
}
    #grafica poblacion vs residuos
plot(poblacion[,"poblacion"],residuos)
    #histograma de residuos
hist(residuos)
    #Comparando con la distribucion normal teorica
qqnorm(residuos)
qqline(residuos)

