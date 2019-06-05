#Funcion que calcula la distancia euclideana entre 2 vectores
distancia<-function(test, matriz){
  n<-length(test)
  medida<-rep(1,n)
  for (i in 1:n){
    medida[i]<-sqrt(sum((test - matriz[,i])^2))
  }
  which.min(medida)
}
#Funcion que normaliza un vector
norm_vec <- function(x) sqrt(sum(x^2))

#Funcion que calcula la correlacion entre 2 vectores
distCor<-function(test,matriz){
  n<-length(test)
  medida<-rep(1,n)
  for (i in 1:n){
    medida[i]<-(t(test)%*%matriz[,i])/(norm_vec(test)*norm_vec(matriz[,i]))
  }
  which.max(medida)
}