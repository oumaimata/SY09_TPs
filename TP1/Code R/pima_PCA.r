#********************************************#
#                                            #
#                   Pima_PCA                 #
#                                            #
#********************************************#

Pima <- read.csv("Pima.csv", header=T)
Pima$z <- factor(Pima$z)

summary(Pima)
Pimaquant <- Pima[1:7]
Pimaquant = scale(Pimaquant)

#PCA
Pima.pca = princomp(pimaquant)
Pima.pca
loadings(Pima.pca)

#Cummulative Perentage
eigenval = Pima.pca$sd^2
perc.inertie = cumsum(eigenval/sum(eigenval)*100)
bp = barplot(perc.inertie)
text(bp, 0, round(perc.inertie, 1),cex=1,pos=3) 

#Plot variables and population
autoplot(Pima.pca, data= Pimaquant, colour= Pima$z, shape= FALSE,  
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=1, y=2)

