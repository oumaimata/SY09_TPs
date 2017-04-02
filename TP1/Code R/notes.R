#*********************************************#
#                                             #
#                   Libraries                 #
#                                             #
#*********************************************#
library(gridExtra)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(grid)
library(GGally) #ggpairs


#*********************************************#
#                                             #
# Question 1:Description et Analyse de Data   #
#                                             #
#*********************************************#

dataset = read.csv("data/sy02-p2016.csv", na.strings="", header=T)
n = nrow(dataset)
p = ncol(dataset)
sprintf("Number of individuals= %d",n)
sprintf("Number of Variables= %d",p)


summary(dataset)
describe(dataset)

dataset$nom      = factor(dataset$nom, levels=dataset$nom)
dataset$niveau   = factor(dataset$niveau, ordered=T)
dataset$resultat = factor(dataset$resultat, levels=c("F","Fx","E","D","C","B","A"),ordered=T)

any(is.na(dataset$resultat))
#nrow(dataset[dataset$resultat='NA',])
dataset = dataset[!is.na(dataset$resultat),]
dataset = dataset[dataset$resultat!='ABS',]

#New number of rows
nrow(dataset)
any(is.na(dataset$resultat))

# Normal Distribution??
#jpeg('SY09_TPs/TP1/Figures/Notes/boxplot_exam.jpg')
data = data.frame(dataset$note.median, dataset$note.final, dataset$note.totale)
ggplot(melt(data), aes(variable, value, fill=variable)) + geom_boxplot() + scale_fill_discrete(name="Notes")
#dev.off();

#jpeg('SY09_TPs/TP1/Figures/Notes/multiplot.jpg')
ggpairs(
    dataset, 
    columns = c("specialite", "niveau", "dernier.diplome.obtenu", "correcteur.median")
)
#dev.off();

#jpeg('SY09_TPs/TP1/Figures/Notes/corr_notes.jpg')
df = data.frame(dataset$note.median, dataset$note.final, dataset$note.totale)
ggpairs(df)
#dev.off()
cov(dataset$note.median, dataset$note.final)


#*********************************************#
#                                             #
#      Question 2:Liens statistiques          #
#            entre variables                  #
#                                             #
#*********************************************#

dataset2 = dataset[!is.na(dataset$dernier.diplome.obtenu),]

png('SY09_TPs/TP1/Figures/Notes/specialite_resultat.png')
ggplot(data= dataset2, aes(x= specialite, y= note.totale, fill= specialite))+ geom_boxplot()
dev.off()

png('SY09_TPs/TP1/Figures/Notes/niveau_resultat.png')
ggplot(data= dataset, aes(x= niveau, y= note.totale, fill= niveau))+ geom_boxplot()
dev.off()

png('SY09_TPs/TP1/Figures/Notes/diplome_resultat.png')
ggplot(data= dataset2, aes(x= dernier.diplome.obtenu , y= note.totale, fill= dernier.diplome.obtenu))+ geom_boxplot()
dev.off()
#make sure to take the NA here !

png('SY09_TPs/TP1/Figures/Notes/correcteur_final.png')
ggplot(data= dataset, aes(x= correcteur.final, y= note.final, fill= correcteur.final))+ geom_boxplot()
dev.off()

png('SY09_TPs/TP1/Figures/Notes/correcteur_median.png')
ggplot(data= dataset, aes(x= correcteur.median, y= note.median, fill= correcteur.median))+ geom_boxplot()
dev.off()
