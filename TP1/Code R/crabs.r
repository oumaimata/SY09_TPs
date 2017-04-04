#********************************************#
#                                            #
#                   Libraries                #
#                                            #
#********************************************#
library(GGally)
library(ggplot2)
library(MASS)
data(crabs)

summary(crabs)
crabsquant <- crabs[,4:8]

#********************************************#
#                                            #
#                   Question 1               #
#                                            #
#********************************************#
plot(crabsquant)

boxplot(crabsquant, main = "les données de crabsquant", ylab = "nombre de crabes", name = c("FL", "RW", "CL", "CW", "BD"))

plot(crabsquant, col = crabs$sex)

plot(crabsquant, col = crabs$sp)

boxplot(crabs$FL[crabs$sp == "B"], crabs$FL[crabs$sp == "O"], main = "Étude de la variable FL en fonction des espèces",  col = c("blue", "orange"), names = c("B", "O"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$RW[crabs$sp == "B"], crabs$RW[crabs$sp == "O"], main = "Étude de la variable RW en fonction des espèces",  col = c("blue", "orange"), names = c("B", "O"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$CL[crabs$sp == "B"], crabs$CL[crabs$sp == "O"], main = "Étude de la variable CL en fonction des espèces",  col = c("blue", "orange"), names = c("B", "O"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$CW[crabs$sp == "B"], crabs$CW[crabs$sp == "O"], main = "Étude de la variable CW en fonction des espèces",  col = c("blue", "orange"), names = c("B", "O"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$BD[crabs$sp == "B"], crabs$BD[crabs$sp == "O"], main = "Étude de la variable BD en fonction des espèces",  col = c("blue", "orange"), names = c("B", "O"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$FL[crabs$sex == "M"], crabs$FL[crabs$sex == "F"], main = "Étude de la variable FL en fonction du sexe",  col = c("green", "red"), names = c("M", "F"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$RW[crabs$sex == "M"], crabs$RW[crabs$sex == "F"], main = "Étude de la variable RW en fonction du sexe",  col = c("green", "red"), names = c("M", "F"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$CL[crabs$sex == "M"], crabs$CL[crabs$sex == "F"], main = "Étude de la variable CL en fonction du sexe",  col = c("green", "red"), names = c("M", "F"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$CW[crabs$sex == "M"], crabs$CW[crabs$sex == "F"], main = "Étude de la variable CW en fonction du sexe",  col = c("green", "red"), names = c("M", "F"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

boxplot(crabs$BD[crabs$sex == "M"], crabs$BD[crabs$sex == "F"], main = "Étude de la variable BD en fonction du sexe",  col = c("green", "red"), names = c("M", "F"), ylab = "nombre de crabes", notch = FALSE, outline = FALSE)

#********************************************#
#                                            #
#                   Question 1               #
#                                            #
#********************************************#
cor(crabsquant)

print(cor(crabsquant),digits=3)