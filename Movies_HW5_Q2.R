# setwd(...)
# require(...)

########## QUESTION 2: Clustering Movies ########## 

movies <- read.table("u.item", sep = "|", header = FALSE, stringsAsFactors = FALSE, quote="")
movies <- movies[,c(1,2)]
names(movies) <- c("movieid","movie")

rank   <- read.table("u.data", sep = "\t", header = FALSE, stringsAsFactors = FALSE,
                     col.names = c("userid","movieid","rating","ts"))

critics <- merge(movies, rank, by = "movieid")
critics$movie <- NULL
critics$ts <- NULL
names(critics) <- c("movieid","person","rank")
N<-length(critics[,1])
M<-length(movies[,1])

# build dissimilarity matrices manually, using critics
# this can take a little bit of time, so the resulting 
# matrices are attached in a separate data file
dissim_gower<-matrix(NA,M,M)
dissim_eucld<-matrix(NA,M,M)
c<-0
for (i in 1:(M-1))
{
  for (j in (i+1):M) 
  {
    
    idx_i<-critics[,1]==i # select movie i
    idx_j<-critics[,1]==j # select movie j
    sel_cr<-rbind(critics[idx_i,2:3],critics[idx_j,2:3]) # critics that rated either one of two movies and their ratings
    sel_ord<-sel_cr[order(sel_cr[,1]),] # sort selected critics by person id
    same_c<-diff(c(0,sel_ord[,1]))==0 # find critics that rated both movies
    L<-sum(same_c)
    if (L>0)
    {
      same_c_prev<-c(same_c[2:length(same_c)],FALSE)
      dissim_gower[i,j]<-sum(sel_ord[same_c,2]!=sel_ord[same_c_prev,2])/L
      dissim_eucld[i,j]<-sqrt(sum((sel_ord[same_c,2]-sel_ord[same_c_prev,2])^2))
    }
    
    c<-c+1
    if (c %% 10000 ==0)
    {
      cat("Progress: ",as.character(round(c/(M*(M-1)/2)*10000)/100)," percent\n") 
    }
  }
}

# dissimilarities have zero on the diagonal

for (i in 1:M)
{
  dissim_gower[i,i]<-0
  dissim_eucld[i,i]<-0
}

# dissimilarities are symmetric

for (i in 2:M)
{
  for (j in 1:(i-1)) 
  {
    dissim_gower[i,j]<-dissim_gower[j,i]
    dissim_eucld[i,j]<-dissim_eucld[j,i]
  }
}

# > sum(is.na(dissim_gower))/1682/1682
# [1] 0.3043451
# 30% of dissimilarities could not be computed (no overlap between users)
# replace NA dissimilarities with averages
dissim_gower[is.na(dissim_gower)]<-mean(dissim_gower, na.rm=T)
dissim_eucld[is.na(dissim_eucld)]<-mean(dissim_eucld, na.rm=T)

K_clust<-5
# assign movies to K_clust clusters using k-mediods
med_g<-pam(dissim_gower,k=K_clust,diss=T)
clust_g<-med_g$clustering
med_e<-pam(dissim_eucld,k=K_clust,diss=T)
clust_e<-med_e$clustering
# compute multidimensional scaling of movies onto R^3
proj_g<-cmdscale(dissim_gower,3)
proj_e<-cmdscale(dissim_eucld,3)

cols<-451:471 # some colors for plotting

# a trivial function that plots points by cluster - will be used multiple times
plot_2dc <- function(coords,clust,cols,K){
  k<-1
  idx<-clust==k
  plot(coords[idx,],
       xlim=c(min(coords[,1]),max(coords[,1])),
       ylim=c(min(coords[,2]),max(coords[,2])),
       xlab="",
       ylab="",
       col=cols[k])
  for (k in 2:K)
  {
    idx<-clust==k
    points(coords[idx,],col=cols[k])
  }
}

plot_2dc(proj_g[,1:2],clust_g,cols,K_clust)
plot_2dc(proj_e[,1:2],clust_g,cols,K_clust)
plot_2dc(proj_g[,2:3],clust_g,cols,K_clust)
plot_2dc(proj_e[,2:3],clust_g,cols,K_clust)


for (k in 1:K_clust)
{
  idx<-clust==k
  scatterplot3d(coords[idx,],
                xlim=c(min(coords[,1]),max(coords[,1])),
                ylim=c(min(coords[,2]),max(coords[,2])),
                zlim=c(min(coords[,3]),max(coords[,3])),
                color=cols[k])
}

# Interpret clusters
# movies$movie[which(clust_g==1)]
