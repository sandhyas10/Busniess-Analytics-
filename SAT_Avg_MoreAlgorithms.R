setwd("/Users/sandhyasriraman/Downloads/")
colg=read.csv("CollegeData (1).csv")
colg=na.omit(colg)


#Bootstraping sat_avg data

library("boot")
N=nrow(colg)
sam = sample(1:N,N,replace=TRUE)
boot_fn=function(colg,sam) 
  return(coef(lm(SAT_AVG~.-INSTNM,colg,subset=sam)))
set.seed(1)
boot(colg,boot_fn,R=100)

#K-means  clustering to find within cluster variation

colg_scaled=data.frame(scale(colg[,-1]))
wss=rep(0,50)
for (i in 1:50) 
  wss[i]=kmeans(colg_scaled,centers=i,nstart=10)$tot.withinss
plot(wss)
wss 
#Heirarchical clustering to visualize clusters 

colg_sort=colg[order(colg$INSTNM),]
colg_sortscaled=data.frame(scale(colg_sort[,-1]))
colg_sortscaled=head(colg_sortscaled,100)

avg_clustering=hclust(dist(colg_sortscaled),method="average")
plot(avg_clustering)
avg_clustering$labels
clusters_avg = cutree(avg_clustering,4)
(clusters_avg)

cent = function(i, colg_sortscaled, clusters_avg) 
{
  ind = (clusters_avg == i)
  colMeans(colg_sortscaled[ind,])
}
sapply(unique(clusters_avg), cent, colg_sortscaled, clusters_avg)

# Finding Principal Components using PCA
library("pls") 
install.packages("pls")
library("ISLR") 
install.packages("ISLR")



pca=prcomp(colg[,-1],scale=TRUE) 
pca
pve = ((pca$sdev^2)/ sum(pca$sdev^2))*100
plot(cumsum(pve))#pve

# Plotting PCA
biplot(pca,pch=c(25,0.5),col=c("Aquamarine","Red"))


