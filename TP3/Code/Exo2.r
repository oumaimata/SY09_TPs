#*********************************************#
#                                             #
#                   Libraries                 #
#                                             #
#*********************************************#

library(pdist)
library(dplyr)
library(ggplot2)

#*********************************************#
#                                             #
#                   Data.                     #
#                                             #
#*********************************************#

synth40   = read.csv("data/data/Synth1-40.csv", header=T)
synth100  = read.csv("data/data/Synth1-100.csv", header=T)
synth500  = read.csv("data/data/Synth1-500.csv", header=T)
synth1000  = read.csv("data/data/Synth1-1000.csv", header=T)
synth21000 = read.csv("data/data/Synth2-1000.csv", header=T)
pima       = read.csv("data/data/Pima.csv", header=T)
cancer     = read.csv("data/data/Breastcancer.csv", header=T)

#*********************************************#
#                                             #
#    Frontiere Lineaire de La regle de Bayes  #
#                                             #
#*********************************************#

data = synth40
ggplot(data,  aes(V1, V2)) + geom_point(aes(colour = factor(z))) + geom_abline(slope = -2, intercept = -3/2, colour="red")

data = synth100
ggplot(data,  aes(V1, V2)) + geom_point(aes(colour = factor(z))) + geom_abline(slope = -2, intercept = -3/2, colour="red")

data = synth500
ggplot(data,  aes(V1, V2)) + geom_point(aes(colour = factor(z))) + geom_abline(slope = -2, intercept = -3/2, colour="red")

data = synth1000
ggplot(data,  aes(V1, V2)) + geom_point(aes(colour = factor(z))) + geom_abline(slope = -2, intercept = -3/2, colour="red")


#*********************************************#
#                                             #
# Frontiere Quadratique de La regle de Bayes  #
#                                             #
#*********************************************#

data = synth21000
n = nrow(data)
X = data[,1:2]
z = data[,3]

f = function(x) -4*x^2 + 34*x -41+10*log(sqrt(5))   
ggplot(data,  aes(V1, V2)) + geom_point(aes(colour = factor(z))) +
    stat_function(fun=f, colour="red", xlim = c(min(data$V1), max(data$V2))) +
    coord_cartesian(ylim = c(-1,3))

#**********************************************************#
#                                              			   #
# Erreur de la frontiere Quadratique de La regle de Bayes  #
#                                            			   #
#**********************************************************#

 err.bayes2 = function(data)
{
    n = nrow(data)
    z = data[,ncol(data)]
    pred = matrix(nrow = n, ncol =1 )
    
    for(i in 1:n)
    {
        if(data$V1[i]>1.30 &&  data$V1[i] < 7.19 )
            pred[i] = 1
        else
            pred[i] = 2
    }

    return(sum(abs(pred - z))/n)
}
print("Synth2")
err.bayes2(synth21000)
 