# the first one which are used to initialize the weight
ini_w<-function(datas,k,a,b)
{
  w<-matrix(nrow=a,ncol=a)
  rn_datas<-matrix(nrow=a,ncol=a)
  for(i in (1:a))
  {
    for(j in (i:a))
    {
      rn_datas[i,j]<-sum(abs(datas[i,]-datas[j,]))
      rn_datas[j,i]<-rn_datas[i,j]
    }
  }
  for(i in (1:a))
  {
    for(j in (1:a))
    {
      w[i,j]<-k_near(rn_datas[i,],j,k)
    }
  }
  
  for(i in (1:a))
  {
    for(j in (i:a))
    {
      if(w[i,j]!=0)
      {
        w[i,j]<-exp(-0.5*(sum((abs(datas[i,]-datas[j,])))**2))
        w[j,i]<-w[i,j]
      }
    }
  }
  
  return(w)
}

#the second one is the updating function
updata_center<-function(data,name,i,k_new,u,n)
{
  name1<-name[i]
  name[i]<-k_new
  u1<-0
  zi<-0
  u2<-0
  zii<-0
  for(t in (1:n))
  {
    if(name[t] == k_new)
    {
      u1<-u1+data[t,]
      zi<-zi+1
    }
    if(name[t] == name1)
    {
      u2<-u2+data[t,]
      zii<-zii+1
    }
  }
  u1<-u1/zi
  if(zii !=0)
  {
    u2<-u2/zii
  }

  for(t in (1:n))
  {
    if(name[t] == k_new)
    {
      u[t,]<-u1
    }
    if(name[t] == name1)
    {
      u[t,]<-u2
    }
  }
  return(u)
}

#the third one is computing the reglar values
regular<-function(center,n,M)
{
  s<-0
  for(i in (1:n))
  {
    for(j in (i:n))
    {
      s<-s+w[i,j]*sum(abs(center[i,]-center[j,]))/M
    }
  }
  return(s)
}

#the forth one is used to compute the goal in the situation we assume
compute_goal<-function(data,i,name,k,gama,u,M,n,b)
{
  u0<-(-data[i,])
  z<-(-1)
  k0<-name[i]
  for(t in (1:n))
  {
    if(name[t] == k0)
    {
      u0<-u0+data[t,]
      z<-z+1
    }
  }
  if(z!=0)
  {
    u0<-u0/z
  }
  
  for(t in (1:n))
  {
    if(name[t] == k0)
    {
      u[t,]<-u0
    }
  }
  
  u1<-data[i,]
  zi<-1
  for(t in (1:n))
  {
    if(name[t] == k)
    {
      u1<-u1+data[t,]
      zi<-zi+1
    }
  }
  u1<-u1/zi
  u[i,]<-u1
  for(t in (1:n))
  {
    if(name[t] == k)
    {
      u[t,]<-u1
    }
  }
  model<-sum_f_all(datas,u,b,n)+gama*regular(u,n,M)
  return(model)
}

#the fifth one include 2 fucnctions which can get the number of clusters and distribute they again 
stat_distr_clusters<-function(name,n)
{
  si<-numeric(n)
  t<-1
  for(i in (1:(n-1)))
  {
    if(si[i]==0)
    {
      for(j in ((i+1):n))
      {
        if(name[i] == name[j])
        {
          name[j]<-(-t)
          si[j]<-1
        }
      }
      name[i]<-(-t)
      si[i]<-1
      t<-t+1
    }
  }
  name=abs(name)
  return(name)
}

stat_cluster<-function(name,n)#count how many clusters at all
{
  si<-numeric(n)
  t<-0
  for(i in (1:n))
  {
    if(si[i]!=1)
    {
      for(j in (i:n))
      {
        if(name[i]==name[j])
        {
          si[j]<-1
        }
      }
      t<-t+1
    }
  }
  return(t)
}

#the sixth one computes one value of our objective function
sum_f_all<-function(data,center,n,n0)
{
  s<-0
  for(i in (1:n0))
  {
    s<-s+(1-Rn(data[i,],center[i,],n))
  }
  return(s)
}

#the seventh one computes the m which is used to zero-one regular some values
max_M<-function(data,n)
{
  A<-matrix(nrow=n,ncol=n)
  for(i in (1:n))
  {
    for(j in (1:n))
    {
      A[i,j]<-sum(abs(data[i,]-data[j,]))
    }
  }
  m<-max(A)
  return(m)
}

#the eighth one just judge the point's k nears
k_near<-function(x,i,k)#x is a vecotr, i is the position
{
  y<-order(x)
  z<-y[1:k]
  t<-0
  for(j in (1:k))
  {
    if(i == z[j])
    {
      t<-1
      break
    }
  }
  
  return(t)
}
