###Función que genera la descomposión NMF
nonNegative<-function(V,rango,tol=0.001, max.iter=10,semilla=100){
  set.seed(semilla)
  W<-matrix(sample(seq(1,255,1),dim(V)[1]*rango,replace=TRUE), ncol=rango)
  H<-matrix(sample(seq(1,255,1),dim(V)[2]*rango,replace=TRUE), nrow=rango)
  costoInicial<-costoNMF(V,W,H)
  #print(costoInicial)
  for (i in 1:max.iter){
    resultado=updateNMF(V,W,H)
    #print(costoLNMF(V, resultado[[1]],resultado[[2]]))
    if (abs(costoNMF(V, resultado[[1]],resultado[[2]])-costoInicial)<tol){
      break;
    }
    costoInicial<-costoNMF(V, resultado[[1]],resultado[[2]])
  }
  resultado
}

####Función que genera la descomposión local NMF
LnonNegative<-function(V,rango,tol=0.001, max.iter=10,semilla=100){
  set.seed(semilla)
  W<-matrix(sample(seq(1,255,1),dim(V)[1]*rango,replace=TRUE), ncol=rango)
  H<-matrix(sample(seq(1,255,1),dim(V)[2]*rango,replace=TRUE), nrow=rango)
  costoInicial<-costoLNMF(V,W,H)
  #print(costoInicial)
  for (i in 1:max.iter){
    resultado=updateLNMF(V,W,H)
    #print(costoLNMF(V, resultado[[1]],resultado[[2]]))
    if (abs(costoLNMF(V, resultado[[1]],resultado[[2]])-costoInicial)<tol){
      break;
    }
    costoInicial<-costoLNMF(V, resultado[[1]],resultado[[2]])
  }
  resultado
}

#Funcion que genera las predicciones tanto para NMF como para LNMF con base en alguna
#de las distancias 
predecir<-function(modelo, train, test, distancia="euclid"){
  require(MASS)
  n<-dim(test)[2]
  clasif<-rep(0,n)
  media<-apply(train,1,mean)
  baseMoore<-ginv(modelo[[1]])
  train.red<-baseMoore%*%(train)
  print(dim(train.red))
  test.red<-baseMoore%*%(test)
  if (distancia=="euclid"){
    for (i in 1:n){
      clasif[i]<-distancia(test.red[,i], train.red)
    }
  }
  else if(distancia=="corr"){
    for (i in 1:n){
      clasif[i]<-distCor(test.red[,i], train.red)
    }
  }
  else{
   print("No es una función de distancia valida") 
  }
  clasif
}

#Funcion que genera las predicciones en eigenfaces con base en alguna
#de las distancias 
predecirPCA<-function(train, test, rango, distancia="euclid"){
  n<-dim(test)[2]
  clasif<-rep(0,n)
  media<-apply(train,1,mean)
  train<-train-media
  test<-test-media
  eigenfaces<-svd(train)
  eigenfaces<-t(eigenfaces$u[,1:rango])
  train.red<-eigenfaces%*%(train)
  test.red<-eigenfaces%*%(test)
  if (distancia=="euclid"){
    for (i in 1:n){
      clasif[i]<-distancia(test.red[,i], train.red)
    }
  }
  else if(distancia=="corr"){
    for (i in 1:n){
      clasif[i]<-distCor(test.red[,i], train.red)
    }
  }
  else{
    print("No es una función de distancia valida") 
  }
  clasif
}

#Funcion que permite establecer si la imagen corresponde a un hombre o a una mujer con
#base en alguna de las distancias: euclideana o correlacion, para NMF y LNMF
pertenencia<-function(modelo1, modelo2, train1, train2, test, distancia="euclid"){
  hombres<-predecir(modelo1, train1, test, distancia)
  mujeres<-predecir(modelo2, train2, test, distancia)
  clasif<-rep(1, dim(test)[2])
  for (i in 1:length(clasif)){
    if (distancia=="euclid"){
      if (hombres[i]<mujeres[i]){
      clasif[i]<-"Hombre"
      }
      else if(hombres[i]>mujeres[i]){
        clasif[i]<-"Mujer"
      }
      else{
        clasif[i]<-sample(c("Hombre","Mujer"),1)
      }
    }
    if (distancia=="corr"){
      if (hombres[i]>mujeres[i]){
        clasif[i]<-"Hombre"
      }
      else if(hombres[i]<mujeres[i]){
        clasif[i]<-"Mujer"
      }
      else{
        clasif[i]<-sample(c("Hombre","Mujer"),1)
      }
    }
  }
  clasif
}

#Funcion que permite establecer si la imagen corresponde a un hombre o a una mujer con
#base en alguna de las distancias: euclideana o correlacion, para eigenfaces
pertenenciaPCA<-function(train1,train2, test, rango, distancia="euclid"){
  hombres<-predecirPCA(train1, test, rango, distancia)
  mujeres<-predecirPCA(train2, test, rango, distancia)
  clasif<-rep(1, dim(test)[2])
  for (i in 1:length(clasif)){
    if (distancia=="euclid"){
      if (hombres[i]<mujeres[i]){
        clasif[i]<-"Hombre"
      }
      else if(hombres[i]>mujeres[i]){
        clasif[i]<-"Mujer"
      }
      else{
        clasif[i]<-sample(c("Hombre","Mujer"),1)
      }
    }
    if (distancia=="corr"){
      if (hombres[i]>mujeres[i]){
        clasif[i]<-"Hombre"
      }
      else if(hombres[i]<mujeres[i]){
        clasif[i]<-"Mujer"
      }
      else{
        clasif[i]<-sample(c("Hombre","Mujer"),1)
      }
    }
  }
  clasif
}