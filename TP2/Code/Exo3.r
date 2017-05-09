#*********************************************#
#                                             #
#                   Libraries                 #
#                                             #
#*********************************************#
library(MASS)
library(ggplot2)
library(GGally)
library(ggfortify)
library(ggtern)
library(mclust)
library(factoextra)


#*****************************************************************#
#                                        				          #
#             					Iris 	    					  #
#                                       					      #
#****************************************************************#

data(iris)
summary(iris)

#*********************************************#
#                                             #
#             Question 1   					  #
#                                             #
#*********************************************#


iris.kmeans2 = kmeans(iris[,1:4], 2)
#png("/Users/zineb/Desktop/Studies/GI05/SY09/SY09_TPs/TP2/Figures/Iris_2/kmeans2.png")
#ggplot(iris, aes(Petal.Length, Sepal.Width, color= iris.kmeans2$cluster)) + geom_point()
fviz_cluster(iris.kmeans2, iris[, -5], , geom = c("point"))
#dev.off()

iris.kmeans3 = kmeans(iris[,1:4], centers=3)
#png("/Users/zineb/Desktop/Studies/GI05/SY09/SY09_TPs/TP2/Figures/Iris_2/kmeans3.png")
#ggplot(iris, aes(Petal.Length, Sepal.Width, color= iris.kmeans3$cluster)) + geom_point()
fviz_cluster(iris.kmeans3, iris[, -5], , geom = c("point"))
#dev.off()

iris.kmeans4 = kmeans(iris[,1:4], centers=4)
#png("/Users/zineb/Desktop/Studies/GI05/SY09/SY09_TPs/TP2/Figures/Iris_2/kmeans4.png")
#ggplot(iris, aes(Petal.Length, Sepal.Width, color= iris.kmeans4$cluster)) + geom_point()
fviz_cluster(iris.kmeans4 , iris[, -5], , geom = c("point"))
#dev.off()




#*********************************************#
#                                             #
#             Question 2   					  #
#                                             #
#*********************************************#

#On cherche à présent à étudier la stabilité du résultat de la partition. Effectuer plusieurs classifications 
#des données en K = 3 classes. 
#Observer les résultats, en termes de partition et d’inertie intra-classes. 
#Ces résultats sont-ils toujours les mêmes ? Commenter et interpréter.

set.seed(10)
iris.kmeans3_1 = kmeans(iris[,1:4], centers=3)
iris.kmeans3_2 = kmeans(iris[,1:4], centers=3)
iris.kmeans3_3 = kmeans(iris[,1:4], centers=3)
iris.kmeans3_4 = kmeans(iris[,1:4], centers=3)

table(iris.kmeans3_1$cluster, iris$Species)
iris.kmeans3_1$cluster = as.factor(iris.kmeans3_1$cluster)

table(iris.kmeans3_2$cluster, iris$Species)
iris.kmeans3_2$cluster = as.factor(iris.kmeans3_2$cluster)

table(iris.kmeans3_3$cluster, iris$Species)
iris.kmeans3_3$cluster = as.factor(iris.kmeans3_3$cluster)

table(iris.kmeans3_4$cluster, iris$Species)
iris.kmeans3_4$cluster = as.factor(iris.kmeans3_4$cluster)

ggplot(iris, aes(Petal.Length, Petal.Width, color= iris.kmeans3_1$cluster)) + geom_point()

ggplot(iris, aes(Petal.Length, Petal.Width, color= iris.kmeans3_2$cluster)) + geom_point()

ggplot(iris, aes(Petal.Length, Petal.Width, color= iris.kmeans3_3$cluster)) + geom_point()

ggplot(iris, aes(Petal.Length, Petal.Width, color= iris.kmeans3_4$cluster)) + geom_point()


#*********************************************#
#                                             #
#             Question 3   					  #
#                                             #
#*********************************************#


#On cherche à déterminer le nombre de classes optimal.
#(a) Effectuer N = 100 classifications en prenant K = 2 classes; puis à nouveau N = 100 classifications en K = 3 
#classes, K = 4 classes, ... jusqu’à K = 10 classes. On pourra faire deux boucles imbriquées pour cela.

N          = 100
K_max      = 10
var.within = matrix(nrow=K_max, ncol = N)


for(k in  2:K_max)
{
    
    for(i in 1:N)
    {
        iris.kmeans = kmeans(iris[,1:4], centers=k)
        var.within[k,i] = iris.kmeans$tot.withinss 
    } 
    
}

A = nrow(iris) * cov.wt(iris[,1:4], method='ML')$cov #ML matrice de covarianc empirique
var.within[1,] = sum(diag(A)) #trace de A
for(i in 2:K_max)
    var.within[i,1] = min(var.within[i,])
    

#png("/Users/zineb/Desktop/Studies/GI05/SY09/SY09_TPs/TP2/Figures/Iris_2/withinvar.png")
plot(var.within[,1], type='ol') #k =3
#dev.off()


#*********************************************#
#                                             #
#             Question 4  					  #
#                                             #
#*********************************************#

#Compare
#indice de Rand poir pouvoir 
#prend toutes les paires de points et voit s il soont classes de la meme maniere dans P1 et P2
#compte le nb de paires de points qui sont classes de la meme maniere dans les 2 partitions ==> on a un indice 
#de concordance dans les 2 Partitions

#USE adjusted Rand index to compare 2 partitions !!

#set.seed(1)
iris.kmeans = kmeans(iris[,1:4], centers=3)
adjustedRandIndex(iris.kmeans$cluster, iris$Species)
#Since the Rand index lies between 0 and 1

table(iris.kmeans$cluster, iris$Species)

#plot
merge = interaction(iris$Species,iris.kmeans$cluster)

#png("/Users/zineb/Desktop/Studies/GI05/SY09/SY09_TPs/TP2/Figures/Iris_2/interaction.png")
ggplot(iris, aes(Petal.Length, Sepal.Width, color= merge)) + geom_point()
#dev.off()


#*****************************************************************#
#                                        				          #
#             					Crabs	    					  #
#                                       					      #
#*****************************************************************#





#*********************************************************************#
#                                           				          #
#             					Mutation	    					  #
#                                       		     			      #
#*********************************************************************#