#*****************************#

#		   Libraries           #

#******************************#
library(MASS)
library(Hmisc) #describe data
library(ggplot2) #plot
library(reshape2)
library(gridExtra)
library(grid)
library(GGally) #ggpairs: matricial plot

#*************************************************************************#

#		   						Notes SY02          					  #

#*************************************************************************#

notes = read.csv("data/sy02-p2016.csv", na.strings="", header=T)

moy.median = aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) = c("correcteur","moy.median")
std.median = aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) = c("correcteur","std.median")
median <- merge(moy.median, std.median)
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) = c("correcteur","moy.final")
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) = c("correcteur","std.final")
final = merge(moy.final, std.final)
correcteurs = merge(median, final, all=T)