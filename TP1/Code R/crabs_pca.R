#*********************************************#
#                                             #
#                   Libraries                 #
#                                             #
#*********************************************#

library(ggplot2)
library(MASS)
library(GGally) #ggpairs
library(reshape2)
library(grid)
library(ggfortify)

#*********************************************#
#                                             #
#                   Data                      #
#                                             #
#*********************************************#
#l’ACP sur crabsquant
data(crabs)
crabsquant = crabs[, 4:8]
n.crabs = nrow(crabs)
p.crabs = ncol(crabs)
summary(crabs)


#*********************************************#
#                                             #
#             Question 1: princomp            #
#                                             #
#*********************************************#
crabs.pca = princomp(crabsquant)
crabs.pca
loadings(crabs.pca)
#png('SY09_TPs/TP1/Figures/Crabs/pca_plot.png')
plot(crabs.pca)
#dev.off()
#png('SY09_TPs/TP1/Figures/Crabs/pca_biplot.png')
biplot(crabs.pca)
#dev.off()

#Composantes portees par l'axe comp1 et 0 sur le deuxieme axe comp2
load = with(crabs.pca, unclass(loadings))
aload = abs(load) ## save absolute values
sweep(aload, 2, colSums(aload), "/")



#*********************************************#
#                                             #
#             Question 2: Solution            #
#                                             #
#*********************************************#

#nouvelle matrice de donnees avec variables non corollees
crabs_decorr = crabs 
rowsum = rowSums(crabs[,4:8])
for(i in 1:n.crabs)
{
    crabs_decorr[i,4:8] = crabs_decorr[i,4:8] / rowsum[i]
    
}

#Column of reponses
y =  matrix(0, nrow=n.crabs, ncol=1)
crabs_decorr = cbind(crabs_decorr, y)
#crabs.new
for(i in 1:n.crabs)
{
    if(crabs_decorr$sp[i]=="O" & crabs_decorr$sex[i]=="M")
        crabs_decorr$y[i] = 'OM'
        
    if(crabs_decorr$sp[i]=="O" & crabs_decorr$sex[i]=="F")
       crabs_decorr$y[i] = 'OF'
        
    if(crabs_decorr$sp[i]=="B" & crabs_decorr$sex[i]=="M")
        crabs_decorr$y[i] = 'BM'
        
    if(crabs_decorr$sp[i]=="B" & crabs_decorr$sex[i]=="F")
        crabs_decorr$y[i] = 'BF'
}

#png('SY09_TPs/TP1/Figures/Crabs/matricial_plot_decorr_classes.png')
ggpairs(crabs_decorr,aes(color = y), columns = c("FL", "RW", "CL", "CW", "BD"))
#dev.off()

crabs_decorr.pca = princomp(crabs_decorr[,4:8])
crabs_decorr.pca
loadings(crabs_decorr.pca)

#png('SY09_TPs/TP1/Figures/Crabs/decorr_pca_plot.png')
plot(crabs_decorr.pca)
#dev.off()
#png('SY09_TPs/TP1/Figures/Crabs/decorr_pca_biplot.png')
autoplot(crabs_decorr.pca, data= crabs_decorr, colour= 'y', shape= FALSE,  
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=1, y=2)
#dev.off()

#Percentage of contribution
load = with(crabs_decorr.pca, unclass(loadings))
aload = abs(load) ## save absolute values
sweep(aload, 2, colSums(aload), "/")
colSums(sweep(aload, 2, colSums(aload), "/"))

screeplot(crabs_decorr.pca)


