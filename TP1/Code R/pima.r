#********************************************#
#                                            #
#                   Libraries                #
#                                            #
#********************************************#
library(GGally)
library(ggplot2)

Pima <- read.csv("Pima.csv", header=T)
Pima$z <- factor(Pima$z)

summary(Pima)

#********************************************#
#                                            #
#                   Question 1               #
#                                            #
#********************************************#

Pimaquant <- Pima[1:7]

boxplot(Pimaquant, main = "les données quantitatives de Pima", name = c("npreg", "glu", "bp", "skin", "bmi", "ped", "age"), outcol="red")

nrow(Pima);
any(is.na(Pima));


boxplot(scale(Pimaquant), outcol="red");

ggpairs(Pimaquant);

#********************************************#
#                                            #
#                   Question 2               #
#                                            #
#********************************************#

boxplot(Pima$npreg[Pima$z == "1"], Pima$npreg[Pima$z == "2"], main = "Étude de la variable NPREG en fonction du diabète",  col = c("blue", "orange"), names = c("1", "2"), ylab = "nombre d'individus", notch = FALSE, outline = FALSE)

boxplot(Pima$glu[Pima$z == "1"], Pima$glu[Pima$z == "2"], main = "Étude de la variable GLU en fonction du diabète",  col = c("blue", "orange"), names = c("1", "2"), ylab = "nombre d'individus", notch = FALSE, outline = FALSE)

boxplot(Pima$bp[Pima$z == "1"], Pima$bp[Pima$z == "2"], main = "Étude de la variable BP en fonction du diabète",  col = c("blue", "orange"), names = c("1", "2"), ylab = "nombre d'individus", notch = FALSE, outline = FALSE)

boxplot(Pima$skin[Pima$z == "1"], Pima$skin[Pima$z == "2"], main = "Étude de la variable SKIN en fonction du diabète",  col = c("blue", "orange"), names = c("1", "2"), ylab = "nombre d'individus", notch = FALSE, outline = FALSE)

boxplot(Pima$bmi[Pima$z == "1"], Pima$bmi[Pima$z == "2"], main = "Étude de la variable BMI en fonction du diabète",  col = c("blue", "orange"), names = c("1", "2"), ylab = "nombre d'individus", notch = FALSE, outline = FALSE)

boxplot(Pima$ped[Pima$z == "1"], Pima$ped[Pima$z == "2"], main = "Étude de la variable PED en fonction du diabète",  col = c("blue", "orange"), names = c("1", "2"), ylab = "nombre d'individus", notch = FALSE, outline = FALSE)

boxplot(Pima$age[Pima$z == "1"], Pima$age[Pima$z == "2"], main = "Étude de la variable AGE en fonction du diabète",  col = c("blue", "orange"), names = c("1", "2"), ylab = "nombre d'individus", notch = FALSE, outline = FALSE)
