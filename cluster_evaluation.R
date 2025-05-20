library(factoextra)
library(cluster)
library(ggplot2)
library(stringr)



df<-read.delim("data.csv", row.names = 1,sep=",")

df <- scale(df)
res <- get_clust_tendency(df, 40, graph = TRUE)
res$hopkins_stat

res$plot

set.seed(123)
## Compute the gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 500) 
# Plot the result
fviz_gap_stat(gap_stat)


fviz_nbclust(df, kmeans, method='silhouette')


n_clust<-fviz_nbclust(df, kmeans, method='silhouette')
n_clust<-n_clust$data

max_cluster<-as.numeric(n_clust$clusters[which.max(n_clust$y)])



clara_res <- clara(df,2,samples = 50,pamLike = T)
print(clara_res)
names(clara_res)

fviz_cluster(clara_res,data = df)




