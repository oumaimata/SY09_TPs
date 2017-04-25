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
n.crabs    = nrow(crabs)
p.crabs   = ncol(crabs)
summary(crabs)


#*********************************************#
#                                             #
#             Question 1: princomp            #
#                                             #
#*********************************************#
crabs.pca = princomp(crabsquant)
crabs.pca
loadings(crabs.pca)

#Pourcentage d'inertie cumulee
eigenval     = crabs.pca$sd^2
perc.inertie = cumsum(eigenval/sum(eigenval)*100)
bp = barplot(perc.inertie)
text(bp, 0, round(perc.inertie, 1),cex=1,pos=3) 
biplot(crabs.pca)


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
ggpairs(crabs_decorr,aes(color = y), columns = c("FL", "RW", "CL", "CW", "BD"))


#**************************** PCA ******************************
crabs_decorr.pca = princomp(crabs_decorr[,4:8])
crabs_decorr.pca
loadings(crabs_decorr.pca)

eigenval     = crabs_decorr.pca$sd^2
perc.inertie = cumsum(eigenval/sum(eigenval)*100)
bp = barplot(perc.inertie)
text(bp, 0, round(perc.inertie, 1),cex=1,pos=3) 

autoplot(crabs_decorr.pca, data= crabs_decorr, colour= 'y', shape= FALSE,  
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=1, y=2)


