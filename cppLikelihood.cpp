// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;


// [[Rcpp::export]]


double logLike(NumericVector data, NumericVector a12n, NumericVector a32n, NumericVector pop)
{

  double a12 = a12n[0];
  double a32 = a32n[0];
  double like = 0;
  int n = pop[0];
  double pr_old[n+1]={0};
  pr_old[0]=1;
  double sumPr;
  for (int i = 0; i<data.length(); ++i){
    double pr_new[n+1]={0};
    sumPr=0;
    int x0;
    if (i!=0){
      x0=data[i-1];
    }else{
      x0=1;
    }
    int x1=data[i];
    double px= 0;
    for (int r = 0; r<=n; ++r){
      if (pr_old[r]!=0){
        double probr= pr_old[r];
        double pxgr=0;
        for (int h = 0; h<=x0; ++h){
          if (x0 - h <= x1 & x1<=n-r-h){

            NumericVector hvec(1, (double) h);
            double probh=dbinom(hvec, (double)x0, a32, false)[0];
            double dif= x1-x0+h;
            NumericVector cvec(1, dif);
            pxgr = pxgr+ probh*dbinom(cvec,(double)(n-x0-r), a12, false)[0];
            double probPiece = probr*probh;
            sumPr=sumPr+probPiece;
            pr_new[r+h] = pr_new[r+h]+probPiece;
          }
          
        }
        px = px +pxgr*probr;
      }
    }
    like = like+ log(px);
    for (int a = 0; a<=n; ++a){
      pr_old[a]=pr_new[a]/sumPr;
    }
  }
  
  return like;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
