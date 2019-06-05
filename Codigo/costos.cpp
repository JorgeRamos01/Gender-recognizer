// NMF
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;  // use the Armadillo library for matrix computations
using namespace Rcpp;

// [[Rcpp::export]]
//Funcion de actualizacion multiplicativa para NMF
List updateNMF(arma::mat V, arma::mat W,arma::mat H) {
  arma::mat temp=W*H;
  float temp1=0;
  for (unsigned int i=0; i<W.n_rows; i++){
    for (unsigned int a=0; a<W.n_cols; a++){
      for (unsigned int u=0; u<V.n_cols; u++){
        temp1+=(V(i,u)/temp(i,u))*H(a,u);
      }
      W(i,a)=W(i,a)*temp1;
      temp1=0;
    }
  }
  arma::rowvec wSum=sum(W);
  for (unsigned int i=0; i<W.n_rows; i++){
    for (unsigned int a=0; a<W.n_cols; a++){
      W(i,a)=W(i,a)/wSum(a);
    }
  }
  temp=W*H;
  temp1=0;
  for (unsigned int a=0; a<H.n_rows; a++){
    for (unsigned int u=0; u<H.n_cols; u++){
      for (unsigned int i=0; i<V.n_rows; i++){
        temp1+=(V(i,u)/temp(i,u))*W(i,a);
      }
      H(a,u)=H(a,u)*temp1;
      temp1=0.0;
    }
  }
  List ret;
  ret["W"]=W;
  ret["H"]=H;
  return ret;
}

// [[Rcpp::export]]
//Funcion de divergencia para NMF
float costoNMF(arma::mat V, arma::mat W,arma::mat H){
  arma::mat temp=W*H;
  float tol=100.0;
  float resultado=0.0;
  float temp1=0.0;
  for (unsigned int i=0; i<V.n_rows; i++){
    for (unsigned int j=0; j<V.n_cols; j++){
      temp1=(V(i,j)*(log10(V(i,j)/temp(i,j))-1)+temp(i,j))/1000;
      if (temp1<tol){
        resultado += temp1;
      }
      else{
        resultado +=tol;
      }
    }
    
  }
  return resultado;
}


// [[Rcpp::export]]
//Funcion de divergencia para local NMF
float costoLNMF(arma::mat X, arma::mat B,arma::mat H){
  arma::mat temp=B*H;
  arma::mat U=B.t()*B;
  arma::mat V=H*H.t();
  float tol=100.0;
  float resultado=0.0;
  float temp1=0.0;
  float temp2=0.0;
  float temp3=0.0;
  for (unsigned int i=0; i<X.n_rows; i++){
    for (unsigned int j=0; j<X.n_cols; j++){
      temp1=(X(i,j)*(log10(X(i,j)/temp(i,j))-1)+temp(i,j))/1000;
      if (temp1<tol){
        resultado += temp1;
      }
      else{
        resultado +=tol;
      }
    }
  }
  for (unsigned int i=0; i<U.n_rows; i++){
    for (unsigned int j=0; j<U.n_cols; j++){
      temp2+=U(i,j)*0.5;
      if (i==j){
        temp3 += V(i,i)*0.5;
      }
    }
  }
  resultado =resultado + temp2 - temp3;
  return resultado;
}

// [[Rcpp::export]]
//Funcion de actualizacion multiplicativa para local NMF
List updateLNMF(arma::mat X, arma::mat B,arma::mat H) {
  arma::mat temp=B*H;
  arma::mat U=B.t()*B;
  arma::mat V=H*H.t();
  float temp1=0.0;
  for (unsigned int k=0; k<H.n_rows; k++){
    for (unsigned int l=0; l<H.n_cols; l++){
      for (unsigned int i=0; i<X.n_rows; i++){
        temp1 += X(i,l)*(B(i,k)/temp(i,l));
      }
      H(k,l)=sqrt(H(k,l)*temp1);
      temp1=0;
    }
  }
  arma::colvec tempSum = sum(temp,1);
  arma::rowvec tempSum2 = sum(H);
  for (unsigned int k=0; k<B.n_rows; k++){
    for (unsigned int l=0; l<B.n_cols; l++){
      for (unsigned int j=0; j<X.n_cols; j++){
        temp1+=X(k,j)*(H(l,j)/tempSum(k));
      }
      B(k,l)= B(k,l)*(temp1/tempSum2(l));
      temp1=0.0;
    }
  }
  arma::colvec tempSum3 = sum(B,1);
  for (unsigned int k=0; k<B.n_rows; k++){
    for (unsigned int l=0; l<B.n_cols; l++){
      B(k,l)=B(k,l)/tempSum3(l);
    }
  }
  
  
  List ret;
  ret["B"]=B;
  ret["H"]=H;
  return ret;
}

