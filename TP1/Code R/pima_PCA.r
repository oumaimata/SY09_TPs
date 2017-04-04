#********************************************#
#                                            #
#                   Pima_PCA                 #
#                                            #
#********************************************#

Pima <- read.csv("Pima.csv", header=T)
Pima$z <- factor(Pima$z)

summary(Pima)

Pimaquant <- Pima[1:7]

pimaquant = scale(Pimaquant)
pima.pca = princomp(pimaquant)
pima.pca
loadings(pima.pca)
plot(pima.pca)

biplot(pima.pca)


load = with(pima.pca, unclass(loadings))
aload = abs(load) 
sweep(aload, 2, colSums(aload), "/")/Users/zineb/Downloads/crabs.r
