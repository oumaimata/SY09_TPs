#*****************************#

#		   Libraries           #

#******************************#
library(MASS)
library(gridExtra)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(grid)
library(GGally) #ggpairs

#*************************************************************************#

#		   						Notes SY02          					  #

#*************************************************************************#

#*****************************#

#		Question 1            #

#******************************#

notes = read.csv("data/sy02-p2016.csv", na.strings="", header=T)
summary(notes)
describe(notes) #This is more exhaustive than summary
notes$nom      = factor(notes$nom, levels=notes$nom)
notes$niveau   = factor(notes$niveau, ordered=T)
notes$resultat = factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),ordered=T)

n = nrow(notes)
p = ncol(notes)
sprintf("Number of individuals= %d",n)
sprintf("Number of Variables= %d",p)

any(is.na(notes$resultat))

plot(notes[2:11]) 

data = data.frame(notes$note.median, notes$note.final, notes$note.totale)
ggplot(melt(data), aes(variable, value, fill=variable)) + geom_boxplot() + scale_fill_discrete(name="Notes")


#*****************************#

#		Question 2            #

#******************************#

ggplot(notes, aes(x= resultat, fill= dernier.diplome.obtenu))+ geom_bar(position="dodge")
ggplot(notes, aes(x= resultat, fill= niveau))+geom_bar(position="dodge")
ggplot(notes, aes(x= resultat, fill= specialite))+ geom_bar(position="dodge")


vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
plot1 = ggplot(notes, aes(x= correcteur.median, y= note.median ))+ geom_point()
plot2 = ggplot(notes, aes(x= correcteur.final, y= note.final))+ geom_point()
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))



#*************************************************************************#

#		   						Crabs          				         	  #

#*************************************************************************#





#*************************************************************************#

#		   						Pima          				         	  #

#*************************************************************************#

