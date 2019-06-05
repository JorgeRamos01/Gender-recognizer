rm(list=ls())
library(imager)
setwd("~/ciencia de datos/Proyecto final")
library(Rcpp)
library(RcppArmadillo)
library(devtools)
library(MASS)
sourceCpp("costos.cpp")
source("distancias.R")
source("prediccion.R")

#####Extrayendo las imagenes de hombres
setwd("~/ciencia de datos/Proyecto final/Hombres/Hombres recortadas/train/")
hombres_train <- list.files(path=".", pattern=".jpg",all.files=T, full.names=F, no.. = T) 
men_train = lapply(hombres_train, load.image)

setwd("~/ciencia de datos/Proyecto final/Hombres/Hombres recortadas/test/")
hombres_test <- list.files(path=".", pattern=".jpg",all.files=T, full.names=F, no.. = T) 
men_test = lapply(hombres_test, load.image)

######Extrayendo las imagenes de mujeres
setwd("~/ciencia de datos/Proyecto final/Mujeres/Mujeres recortadas/train")
mujeres_train <- list.files(path=".", pattern=".jpg",all.files=T, full.names=F, no.. = T) 
women_train = lapply(mujeres_train, load.image)

setwd("~/ciencia de datos/Proyecto final/Mujeres/Mujeres recortadas/test")
mujeres_test <- list.files(path=".", pattern=".jpg",all.files=T, full.names=F, no.. = T) 
women_test = lapply(mujeres_test, load.image)




#Transformando las imagenes a un formato imager y en escala de grises
for (i in 1:length(men_train)){
  men_train[[i]]<-grayscale(as.cimg(men_train[[i]]))
}

for (i in 1:length(men_test)){
  men_test[[i]]<-grayscale(as.cimg(men_test[[i]]))
}

for (i in 1:length(women_train)){
  women_train[[i]]<-grayscale(as.cimg(women_train[[i]]))
}

for (i in 1:length(women_test)){
  women_test[[i]]<-grayscale(as.cimg(women_test[[i]]))
}
#Generando las matrices
hombres.train<-matrix(0L,ncol=length(men_train),nrow=dim(men_train[[1]])[1]*dim(men_train[[2]])[1])
for (i in 1:length(men_train)){
  hombres.train[,i]<-c(men_train[[i]])
}

hombres.test<-matrix(0L,ncol=length(men_test),nrow=dim(men_test[[1]])[1]*dim(men_test[[2]])[1])
for (i in 1:length(men_test)){
  hombres.test[,i]<-c(men_test[[i]])
}

mujeres.train<-matrix(0L,ncol=length(women_train),nrow=dim(women_train[[1]])[1]*dim(women_train[[2]])[1])
for (i in 1:length(women_train)){
  mujeres.train[,i]<-c(women_train[[i]])
}

mujeres.test<-matrix(0L,ncol=length(women_test),nrow=dim(women_test[[1]])[1]*dim(women_test[[2]])[1])
for (i in 1:length(women_test)){
  mujeres.test[,i]<-c(women_test[[i]])
}

####Borramos datos innecesarios para liberar memoria
rm(hombres_test)
rm(hombres_train)
rm(mujeres_test)
rm(mujeres_train)
rm(men_test)
rm(men_train)
rm(women_test)
rm(women_train)


grafica<-matrix(0L, ncol=9,nrow=12)
contador<-1
for (i in c(10,20,25,30,35,40,45,50,60)){
  HombresNMF<-nonNegative(hombres.train,i,100)
  MujeresNMF<-nonNegative(mujeres.train,i,100)

  HombresLNMF<-LnonNegative(hombres.train,i,100)
  MujeresLNMF<-LnonNegative(mujeres.train,i,100)

  clasifNMF<-pertenencia(HombresNMF, MujeresNMF, hombres.train,mujeres.train, mujeres.test,distancia = "corr")
  clasifNMF2<-pertenencia(HombresLNMF, MujeresLNMF, hombres.train,mujeres.train, mujeres.test,distancia = "corr")
  clasifPCA<-pertenenciaPCA(hombres.train, mujeres.train, mujeres.test, rango=i, distancia="corr")
  grafica[1,contador]<-sum(clasifNMF=="Mujer")/length(clasifNMF)
  grafica[2,contador]<-sum(clasifNMF2=="Mujer")/length(clasifNMF)
  grafica[3,contador]<-sum(clasifPCA=="Mujer")/length(clasifPCA)
  
  clasifNMF<-pertenencia(HombresNMF, MujeresNMF, hombres.train,mujeres.train, hombres.test,distancia = "corr")
  clasifNMF2<-pertenencia(HombresLNMF, MujeresLNMF, hombres.train,mujeres.train, hombres.test,distancia = "corr")
  clasifPCA<-pertenenciaPCA(hombres.train, mujeres.train, hombres.test, rango=i, distancia="corr")
  grafica[4,contador]<-sum(clasifNMF=="Hombre")/length(clasifNMF)
  grafica[5,contador]<-sum(clasifNMF2=="Hombre")/length(clasifNMF2)
  grafica[6,contador]<-sum(clasifPCA=="Hombre")/length(clasifPCA)
  
  clasifNMF<-pertenencia(HombresNMF, MujeresNMF, hombres.train,mujeres.train, mujeres.train,distancia = "corr")
  clasifNMF2<-pertenencia(HombresLNMF, MujeresLNMF, hombres.train,mujeres.train, mujeres.train,distancia = "corr")
  clasifPCA<-pertenenciaPCA(hombres.train, mujeres.train, mujeres.train, rango=i, distancia="corr")
  grafica[7,contador]<-sum(clasifNMF=="Mujer")/length(clasifNMF)
  grafica[8,contador]<-sum(clasifNMF2=="Mujer")/length(clasifNMF)
  grafica[9,contador]<-sum(clasifPCA=="Mujer")/length(clasifPCA)
  
  clasifNMF<-pertenencia(HombresNMF, MujeresNMF, hombres.train,mujeres.train, hombres.train,distancia = "corr")
  clasifNMF2<-pertenencia(HombresLNMF, MujeresLNMF, hombres.train,mujeres.train, hombres.train,distancia = "corr")
  clasifPCA<-pertenenciaPCA(hombres.train, mujeres.train, hombres.train, rango=i, distancia="corr")
  grafica[10,contador]<-sum(clasifNMF=="Hombre")/length(clasifNMF)
  grafica[11,contador]<-sum(clasifNMF2=="Hombre")/length(clasifNMF2)
  grafica[12,contador]<-sum(clasifPCA=="Hombre")/length(clasifPCA)
  contador<-contador+1
}

plot(c(10,20,25,30,35,40,45,50,60),grafica[1,],type="l", main="Mujeres: Test",sub="Medida: Correlación",col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,xlab="Rango",ylab="Nivel de precisión",ylim=c(min(grafica[1,],grafica[2,],grafica[3,]),max(grafica[1,],grafica[2,],grafica[3,])))
lines(c(10,20,25,30,35,40,45,50,60),grafica[2,],col=rgb(0.9,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b")
lines(c(10,20,25,30,35,40,45,50,60),grafica[3,],col=rgb(0.4,0.7,0.1,0.7) , lwd=3 , pch=19 , type="b")
legend("topleft", 
       legend = c("NMF", "Local NMF","PCA"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.9,0.4,0.1,0.7),
               rgb(0.4,0.4,0.1,0.7)), 
       pch = c(17,19,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.001, 0.001))

plot(c(10,20,25,30,35,40,45,50,60),grafica[4,],type="l", main="Hombres: Test",sub="Medida:Correlación",col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,xlab="Rango",ylab="Nivel de precisión",ylim=c(min(grafica[4,],grafica[5,],grafica[6,]),max(grafica[4,],grafica[5,],grafica[6,])))
lines(c(10,20,25,30,35,40,45,50,60),grafica[5,],col=rgb(0.9,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b")
lines(c(10,20,25,30,35,40,45,50,60),grafica[6,],col=rgb(0.4,0.7,0.1,0.7) , lwd=3 , pch=18 , type="b")
legend("bottomright", 
       legend = c("NMF", "Local NMF","PCA"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.9,0.4,0.1,0.7),
               rgb(0.4,0.7,0.1,0.7)), 
       pch = c(17,19,18), 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.0001, 0.0001))

plot(c(10,20,25,30,35,40,45,50,60),grafica[7,],type="l", main="Mujeres: Train",sub="Medida:Correlación",col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,xlab="Rango",ylab="Nivel de precisión",ylim=c(min(grafica[7,],grafica[8,],grafica[9,]),max(grafica[7,],grafica[8,],grafica[9,])))
lines(c(10,20,25,30,35,40,45,50,60),grafica[8,],col=rgb(0.9,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b")
lines(c(10,20,25,30,35,40,45,50,60),grafica[9,],col=rgb(0.4,0.7,0.1,0.7) , lwd=3 , pch=18 , type="b")
legend("bottomright", 
       legend = c("NMF", "Local NMF","PCA"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.9,0.4,0.1,0.7),
               rgb(0.4,0.7,0.1,0.7)), 
       pch = c(17,19,18), 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.0001, 0.0001))

plot(c(10,20,25,30,35,40,45,50,60),grafica[10,],type="l", main="Hombres: Train",sub="Medida:Correlación",col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,xlab="Rango",ylab="Nivel de precisión",ylim=c(min(grafica[10,],grafica[11,],grafica[12,]),max(grafica[10,],grafica[11,],grafica[12,])))
lines(c(10,20,25,30,35,40,45,50,60),grafica[11,],col=rgb(0.9,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b")
lines(c(10,20,25,30,35,40,45,50,60),grafica[12,],col=rgb(0.4,0.7,0.1,0.7) , lwd=3 , pch=18 , type="b")
legend("bottomright", 
       legend = c("NMF", "Local NMF","PCA"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.9,0.4,0.1,0.7),
               rgb(0.4,0.7,0.1,0.7)), 
       pch = c(17,19,18), 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.0001, 0.0001))


##### Vectores en la base de mujeres con rango=30

MujeresNMF<-nonNegative(mujeres.train,30,100)

media<-apply(mujeres.train,1,mean)
train<-mujeres.train-media
eigenfaces<-svd(train)
eigenfaces<-t(eigenfaces$u[,1:30])

library(imager)
plot(as.cimg(MujeresNMF$W[,1]),axes=FALSE)

plot(as.cimg(t(eigenfaces)[,1]),axes=FALSE)
