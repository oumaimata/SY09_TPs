#*********************************************#
#                                             #
#                   Libraries                 #
#                                             #
#*********************************************#

library(ggplot2)
library(GGally) #ggpairs
library(reshape2)
library(grid)
library(ggfortify)

#*********************************************#
#                                             #
#                   Data                      #
#                                             #
#*********************************************#

notes             = read.csv("data/sy02-p2016.csv", na.strings="", header=T)

moy.median        = aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) = c("correcteur","moy.median")
std.median        = aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) = c("correcteur","std.median")
median            = merge(moy.median, std.median)

moy.final         = aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final)  = c("correcteur","moy.final")
std.final         = aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final)  = c("correcteur","std.final")
final             = merge(moy.final, std.final)
correcteurs       = merge(median, final, all=T)



#*********************************************#
#                                             #
#                   Question 1                #
#                                             #
#*********************************************#

X.notes = correcteurs[-c(2,8),]
X.notes$correcteur = as.factor(X.notes$correcteur)
X.notes = scale(X.notes[, -c(1)], center = TRUE)

print("Scaled Data:")
X.notes

X.notes.t = t(X.notes)

V = 1/6 * X.notes.t %*% X.notes
print("MAtrice Empirique/ Matrice de Variances")
V

eig.val  = eigen(V)$values
U        = eigen(V)$vectors
colnames(U) = c("v1", "v2", "v3", "v4")
print("Eigen Values:")
eig.val
print("Eigen Vectors:")
U

inertie.perc = matrix(nrow= 1, ncol=4, 0)
result = cumsum(eig.val)/sum(eig.val) * 100
result

#png('SY09_TPs/TP1/Figures/Notes_PCA/pourcentage_inertie.png')
bp = barplot(result, main="Pourcentage cumulé de l'inertie expliquée", 
    col = c('chartreuse3', 'darkgoldenrod1', 'cornflowerblue', 'mediumorchid2'))
text(bp, 0, round(result, 1),cex=1,pos=3) 
#dev.off()


#*********************************************#
#                                             #
#     Question 2.1: Composantes Principales   #
#                                             #
#*********************************************#

C = X.notes %*% U
colnames(C) = c("c1", "c2", "c3", "c4")
colMeans(C)  #Checked! : it is centred!
print("Composantes Principales:")
C

#Verification: Methode 2 obtention des Composantes principales
WDp =  1/6 *corr.acp.scaled %*% corr.acp.scaled.t
eig.vect
eigen(WDp)

test = eigen(WDp)$vectors[,1]*eigen(WDp)$values
test
#Remark: C %*% t(eig.vect) = corr.acp.scaled

#***********************************************#
#                                               #
#     Question 2.2: représentation des quatre   #
#   individus dans le premier plan factoriel    #
#                                               #
#***********************************************#

#Représentation des quatre individus dans le premier plan factoriel.
print("représentation des quatre individus dans le premier plan factoriel:")
names=c("corr1","corr3", "corr4", "corr5", "corr6", "corr7")
col = c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3',
             'mediumorchid2', 'turquoise3')
#png('SY09_TPs/TP1/Figures/Notes_PCA/individus_comp.png')
ggplot(C, aes(C[,1],C[,2])) + geom_point( size = 6, colour = col)+ 
geom_text(aes(label=names)) + xlab("Premiere Compsante") +
ylab("Deuxieme Composante")
#dev.off()



#*********************************************#
#                                             #
#  Question 3: représentation des 4 variables #
#    dans le premier plan factoriel.          #                                            #
#                                             #
#*********************************************#

#Représentation des 4 variables dans le premier plan factoriel.
print("Correlation Matrix: ")
corr.acp = cor(C, X.notes)
corr.acp 

#png('SY09_TPs/TP1/Figures/Notes_PCA/variables_comp.png')
col = c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3')
names = c('moy.median', 'std.median', 'moy.final', 'std.final')
ggplot(corr.acp, aes(C[1,],C[2,])) + 
geom_point(size = 6, colour = col) + 
geom_text(aes(label=names)) + xlab("Premiere Composante") + ylab("Deuxieme Composante")
#|dev.off() 

#*********************************************#
#                                             #
#           Question 4: Calcule               #
#                                             #                                          #
#*********************************************#


#Calculer l’expression k = sum(cu) pour les valeurs k = 1,2 et 3. 
#À quoi correspond cette somme lorsque k = 3 ?
U
C
C%*%t(U)


#*********************************************#
#                                             #
#           Question 5: Calcule               #
#                                             #                                          #
#*********************************************#

#On souhaite représenter les individus initialement écartés de l’ACP. 
#Remplacer chacune de leurs valeurs manquantes par la moyenne de la variable 
#correspondante (imputation par la moyenne), puis représenter ces individus
#dans les deux premiers plans factoriels.

#Montrer comment on peut retrouver tous les résultats alors obtenus 
#(valeurs propres, axes principaux, composantes principales, représentations graphiques, ...).

#On s’intéresse à l’affichage des résultats de la fonction princomp. Qu’affichent les fonctions plot et biplot? Détailler plus particulièrement le fonctionnement de la fonction biplot redéfinie pour la classe princomp (accessible par biplot.princomp)
#et de ses différentes options.

