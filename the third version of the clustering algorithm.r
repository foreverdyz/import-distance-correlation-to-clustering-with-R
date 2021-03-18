# It is the third version of the distance correlation clustering algorithm
k_means.new.3<-function(datas,k,n,gama)
{
  a<-nrow(datas)
  b<-ncol(datas)
  name<-1:a
  u<-datas
  u0<-datas
  M<-max_M(datas,a)
  w<-ini_w(datas,k,a,b)
  model<-numeric(a)
  sum_clu<-a
  for(ti in (1:n))
  {
    for(i in (1:a))
    {
      model[name[i]]<-sum_f_all(datas,u,b,a)+gama*regular(u,a,M)
      for(j in (1:sum_clu))
      {
        model[j]<-compute_goal(datas,i,name,j,gama,u,M,a,b)
      }
      k_new<-order(model)[1]
      if(k_new != name[i])
      {
        u<-updata_center(datas,name,i,k_new,u,a)
        u0<-u
        name[i]<-k_new
      }
      name<-stat_distr_clusters(name,a)
      sum_clu<-stat_cluster(name,a)
    }
  }
  return(name)
}
