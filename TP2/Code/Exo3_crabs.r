
library(MASS)
library(ggplot2)
library(GGally)
library(ggfortify)
library(ggtern)
library(mclust)
library(factoextra)

crabs2 = read.csv("data/crabs2.csv", header=T)
summary(crabs2)
nrow(crabs2)
ncol(crabs2)
#y = interaction(crabs$sp, crabs$sex)

crabs2 = crabs2[,1:4]
crabs2 = cbind(crabs2, y)
ggpairs(crabs2, mapping = aes(color = y), columns = c("FL2","RW2", "CL2", "BD2"))
   


#1. Effectuer plusieurs classifications en K = 2 classes des données Crabs chargées précédemment.
#Les résultats obtenus sont-ils toujours les mêmes ? À quoi correspondent-ils ?
N = 100
var.within = matrix(nrow=N, ncol = 1)
for (i in 1: N)
{
    crabs.kmeans2 = kmeans(crabs2[,1:4], centers=2)
    var.within[i,1] = crabs.kmeans2$tot.withinss  
}
min(var.within[,1])
     
    
#var.within

crabs.kmeans2_1 = kmeans(crabs2[,1:4], centers=2)
crabs.kmeans2_2 = kmeans(crabs2[,1:4], centers=2)
crabs.kmeans2_3 = kmeans(crabs2[,1:4], centers=2)
crabs.kmeans2_4 = kmeans(crabs2[,1:4], centers=2)

table(crabs.kmeans2_1$cluster, crabs2$y)
crabs.kmeans2_1$cluster = as.factor(crabs.kmeans2_1$cluster)

table(crabs.kmeans2_2$cluster, crabs2$y)
crabs.kmeans2_2$cluster = as.factor(crabs.kmeans2_2$cluster)

table(crabs.kmeans2_3$cluster, crabs2$y)
crabs.kmeans2_3 = as.factor(crabs.kmeans2_3$cluster)

table(crabs.kmeans2_4$cluster, crabs2$y)
crabs.kmeans2_4$cluster = as.factor(crabs.kmeans2_4$cluster)

ggplot(crabs2, aes(RW2, BD2 , color= crabs.kmeans2_1$cluster)) + geom_point()
ggplot(crabs2, aes(RW2, BD2 , color= crabs.kmeans2_2$cluster)) + geom_point()
ggplot(crabs2, aes(RW2, BD2 , color= crabs.kmeans2_3$cluster)) + geom_point()
ggplot(crabs2, aes(RW2, BD2 , color= crabs.kmeans2_4$cluster)) + geom_point()




#2. Effectuer une classification en K = 4 classes des données. Comparer à la partition réelle suivant
#l’espèce et le sexe. 
#Que peut-on conclure ?

N = 1000
var.within = matrix(nrow=N, ncol = 1)
for (i in 1: N)
{
    crabs.kmeans4 = kmeans(crabs2[,1:4], centers=4)
    var.within[i,1] = crabs.kmeans4$tot.withinss  
}
unique(var.within[,1])
#9 classifications differentes

min(var.within[,1]) 
#0.10 --> 4 is better
     

crabs.kmeans4 = kmeans(crabs2[,1:4], centers=4)

table(crabs.kmeans4$cluster, crabs2$y)
crabs.kmeans4$cluster = as.factor(crabs.kmeans4$cluster)

ggplot(crabs2, aes(RW2, BD2 , color= crabs.kmeans4$cluster)) + geom_point()

#plot
#merge = interaction(crabs2$y,crabs.kmeans4$cluster)

ggplot(crabs2, aes(RW2, BD2, color= crabs2$y)) + geom_point()

adjustedRandIndex(crabs.kmeans2$cluster, crabs2$y)
adjustedRandIndex(crabs.kmeans4$cluster, crabs2$y)
# 4 est meilleur







