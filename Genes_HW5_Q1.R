# setwd(...)
# require(...)

########## QUESTION 1 ########## 

# read and normalize the data
xcan<-t(read.table("xcancer.dat"))
ycan<-t(read.table("ycancer.dat"))

xcan<-apply(xcan,2,function(x) pmax(pmin(x,16000),20))
gen_min<-apply(xcan,2,function(x) min(x))
gen_max<-apply(xcan,2,function(x) max(x))
gen_idx<-(gen_max/gen_min>5)&(gen_max-gen_min>500)
xcan<-xcan[,gen_idx]
xcan<-apply(xcan,2,function(x) (x-mean(x))/sd(x))

N<-length(ycan)
M<-length(xcan[1,])
K<-14

# define a Z_mtx function - will be used to compare clustering assignments
Z_mtx <- function(array){
  N<-length(array)
  Z<-matrix(0,nrow=N,ncol=N)
  for (i in 1:(N-1))
  {
    for(j in (i+1):N)
    {
      Z[i,j]=array[i]==array[j]
    }
  }
  return(Z)
}

Y<-Z_mtx(ycan)
I<-matrix(1,N,N)

# compute 14 centers based on true labels 
# and test if they are stable (apply kmeans using these centers)

centers<-matrix(0,K,M)
for (k in 1:K)
{
  centers[k,]<-apply(xcan[ycan==k,],2,mean)
}
kres_stb<-kmeans(xcan, centers, iter.max=10)
klab_stb<-kres_stb$cluster
Y2<-Z_mtx(klab_stb)
rho_stb<-sum(Y*(I-Y2)+(I-Y)*Y2)

# run kmeans 10 times using random starting points
# and find the "best" one
rho_min<-rho_stb # initialize minimal rho with some point
kres_min<kres_stb
for (trial in 1:10)
{
  kres<-kmeans(xcan, 14, iter.max = 10, nstart = 14)
  klab<-kres$cluster
  Z<-Z_mtx(klab)
  rho<-sum(Y*(I-Z)+(I-Y)*Z)
  
  if(rho<rho_min)
  {
    rho_min<-rho
    kres_min<-kres
  }
}

# run kernel kmeans 5 times
rhok_min<-Inf #initialize the best rho error for kernel kmeans
for (trial in 1:5)
{
  kkres<-kkmeans(xcan, 14, kernel = "rbfdot") # run kernel kmeans
  kklab<-kkres@.Data
  Z<-Z_mtx(kklab)
  rho<-sum(Y*(I-Z)+(I-Y)*Z)  
  if(rho<rhok_min)
  {
    rhok_min<-rho
    kkres_min<-kkres
  }
}

# results:
# > rho_min
# [1] 3070
# > rhok_min
# [1] 2932
# conclusion: kernel kmeans has better performance on this data