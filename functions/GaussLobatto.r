GaussLobatto<-function(N)
{
  N1<-N
  N<-N-1
  x=matrix(cos(pi*(0:N)/N),ncol=1)
  x=cos(pi*(0:N)/N)
  P<-matrix(0,N1,N1)
  xold<-2
  while (max(abs(x-xold))>2.22044604925031e-16) {
    xold<-x
    P[,1]<-1 
    P[,2]<-x
    
    for (k in 2:N) {
      P[,k+1]=( (2*k-1)*x*P[,k]-(k-1)*P[,k-1] )/k;
    }
    
    x<-xold-( x*P[,N1]-P[,N] )/( N1*P[,N1] )
    
  }
  w<-2./(N*N1*P[,N1]^2);
  ret<-list(x=rev(x),w=w)
  attr(ret,"order")<-N
  ret
}