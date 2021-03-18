#this code is same to the second-version and we need it there
Vn<-function(x,y,n)
{
  x<-as.matrix(x)
  y<-as.matrix(y)
  a=matrix(nrow=n,ncol=n)
  b=matrix(nrow=n,ncol=n)
  A=matrix(nrow=n,ncol=n)
  B=matrix(nrow=n,ncol=n)
  for(i in (1:n))
  {
    a[i,]<-abs(x-x[i])
  }
  for(i in (1:n))
  {
    b[i,]<-abs(y-y[i])
  }
   
  a_row<-rowMeans(a)
  a_col<-colMeans(a)
  a_mean<-mean(a)
  b_row<-rowMeans(b)
  b_col<-colMeans(b)
  b_mean<-mean(b)

  for(i in (1:n))
  {
    A[i,]<-a[i,]-a_row[i]
  }
  for(i in (1:n))
  {
    A[,i]<-A[,i]-a_col[i]
  }
  A<-A+a_mean
  for(i in (1:n))
  {
    B[i,]<-b[i,]-b_row[i]
  }
  for(i in (1:n))
  {
    B[,i]<-B[,i]-b_col[i]
  }
  B<-B+b_mean
  
  Vn2<-matrix(nrow=n,ncol=n)
  Vn2<-A*B
  V<-mean(Vn2)
  
  return(V)
}

Rn<-function(x,y,n)
{
  V1<-Vn(x,x,n)
  V2<-Vn(y,y,n)
  V3<-Vn(x,y,n)
  R<-V3/sqrt(V1*V2)
  return(R)
}
