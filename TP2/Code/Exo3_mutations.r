
library(MASS)
library(ggplot2)
library(GGally)
library(ggfortify)
library(ggtern)
library(mclust)
library(factoextra)

mut = read.csv("data/mutations2.csv", header=T, row.names=1)
mut = as.dist(mut, diag=T, upper=T)
summary(mut)


mut.aftd = cmdscale(mut, k=5, eig = TRUE)
mut.aftd 

shepard = Shepard(mut, mut.aftd$points)
plot(shepard, asp =1)
abline(0,1)



N = 100000
var.within = matrix(nrow=N, ncol = 1)
for (i in 1: N)
{
    mutation.kmeans = kmeans(mut.aftd$points, centers=3)
    var.within[i,1] = mutation.kmeans$tot.withinss  
}
min(var.within[,1])
unique(var.within[,1])


mutation.kmeans = kmeans(mut.aftd$points, centers=3)
mat = data.frame(mutation.kmeans$cluster)
rownames(mut.aftd$points)

col.def = c('red','green','blue')
p = ggplot(mut.aftd$points, aes(mut.aftd$points[,1], mut.aftd$points[,2]))+ geom_point()
p + geom_point(aes(colour = factor(mat[,1])))




