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

#*****************************************************************#
#                                        				          #
#             					Iris 	    					  #
#                                       					      #
#*****************************************************************#

data(iris)
summary(iris)

ggpairs(iris, mapping = aes(color = Species), columns = c("Sepal.Length",
                                                          "Sepal.Width", "Petal.Length", 
                                                         "Petal.Width"))


#*********************************************#
#                                             #
#             Question 1   					  #
#                                             #
#*********************************************#

iris.scaled = scale(iris[,c(-5)])
iris.pca    = prcomp(iris.scaled)
loadings(iris.pca)

plot(iris.pca)



#Afficher les données dans le premier plan factoriel, tout d’abord sans tenir compte de 
#l’espèce. #Que constatez-vous? 

biplot(iris.pca)




#Combien de groupes de points le jeu de données semble-t-il comporter 
#(par exemple via la couleur ou le symbole des points affichés). Que constatez-vous? 
#--> On ne peut distinguer que 2 classes: le seul critete qui peut les separer est le 
#Sepal Width qui est porte par la deuxieme composante

#Afficher ensuite les données en ajoutant l’information d’espèce
autoplot(iris.pca, data= iris, colour= 'Species', shape= FALSE,  
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=1, y=2)
#--> On a 4 classes mais on ne peut pas les differencier vu qu'elles sont toutes encodees
#dans la premiere composante

 
#À quoi peut-on s’attendre si l’on recherche une partition des données ?


#*****************************************************************#
#                                        				          #
#             					Crabs	    					  #
#                                       					      #
#*****************************************************************#


#Charger le jeu de données Crabs à partir du fichier crabs2.csv disponible sur le site de l’UV. 
crabs2 = read.csv("data/crabs2.csv", header=T)
summary(crabs2)
y = interaction(crabs$sp, crabs$sex)

crabs2 = crabs2[,1:4]
crabs2 = cbind(crabs2, y)
ggpairs(crabs2, mapping = aes(color = y), columns = c("FL2",
                                                          "RW2", "CL2", 
                                                         "BD2"))


#Afficher les données dans le premier plan factoriel, tout d’abord sans tenir compte de 
#l’espèce ou du sexe des crabes. Combien de groupes de points apparaissent ?

crabs2.scaled = scale(crabs2[,1:4])
crabs2.pca    = prcomp(crabs2.scaled)
loadings(crabs2.pca)

biplot(crabs2.pca)

plot(crabs2.pca)


autoplot(crabs2.pca, data= crabs2, colour= 'y', shape= FALSE,  
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=1, y=2)




#Afficher ensuite les données en tenant compte des informations d’espèce et de sexe. 
#Que constatez-vous ?


#*********************************************************************#
#                                           				          #
#             					Mutation	    					  #
#                                       		     			      #
#*********************************************************************#

# Charger les données de Mutations à partir du fichier mutations2.csv et déclarer l
#es données comme tableau de dissimilarités ; on utilisera la commande suivante :
mut = read.csv("data/mutations2.csv", header=T, row.names=1)
mut = as.dist(mut, diag=T, upper=T)
#mut
summary(mut)
#Calculer une représentation euclidienne des données en d = 2 variables par AFTD, et 
#l’afficher; 
#-->takes a set of dissimilarities and returns a set of points such that the distances 
#between the points are approximately equal to the dissimilarities


mut.aftd = cmdscale(mut, eig = TRUE)
mut.aftd 
#2  axes c'est les 2 premieres composantes principales de l'ACP


#calculer la qualité de la représentation, et afficher le diagramme de Shepard. 
#Que peut-on dire ?
#Recommencer avec un nombre de variables d de représentation allant de 3 à 5. 
#Interpréter les résultats.
result = cumsum(abs(mut.aftd$eig)/sum(abs(mut.aftd$eig))) * 100
bp = barplot(result, main="Pourcentage cumulé de l'inertie expliquée")
#suggests that the original distances between the species
#can be represented adequately in 5 dimensions

shepard = Shepard(mut, mut.aftd$points)
plot(shepard, asp =1)
abline(0,1)
#text(shepard$x, shepard$y, labels = rownames(mut.aftd$points))



#shepard prend pour chaque distance de la matrice de distance et calcule 
#la distance qu'on a apres cmscale --> donc normalement on devrait 
#obtenir les meme et donc sur l'axe y=x

#amelioration
mut.aftd = cmdscale(mut, k=3, eig = TRUE)
shepard = Shepard(mut, mut.aftd$points)
plot(shepard, asp =1)
abline(0,1)


mut.aftd = cmdscale(mut, k=4, eig = TRUE)
shepard = Shepard(mut, mut.aftd$points)
plot(shepard, asp =1)
abline(0,1)

mut.aftd = cmdscale(mut, k=5, eig = TRUE)
shepard = Shepard(mut, mut.aftd$points)
plot(shepard, asp =1)
abline(0,1)


mut.aftd = cmdscale(mut, k=7, eig = TRUE)
shepard = Shepard(mut, mut.aftd$points)
plot(shepard, asp =1)
abline(0,1)







