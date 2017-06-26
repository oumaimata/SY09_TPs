
#*********************************************#
#                                             #
#                   Data.                     #
#                                             #
#*********************************************#

bcw        = read.csv("data/bcw.csv", header=T)
pima       = read.csv("data/Pima.csv", header=T)
synth140   = read.csv("data/Synth1-40.csv", header = T)
synth1     = read.csv("data/Synth1-1000.csv", header = T)
synth2     = read.csv("data/Synth2-1000.csv", header = T)
synth3     = read.csv("data/Synth3-1000.csv", header=T)

#*********************************************#
#                                             #
#                   Libraries                 #
#                                             #
#*********************************************#
library(abind)
library(MASS)
library(rpart)
library(tree)
library(ggplot2)
library(GGally)



#*********************************************#
#                                             #
#                   Includes                  #
#                                             #
#*********************************************#
source("fonctions/anadisc.r")
source("fonctions/logistic.r")
source("fonctions/mvdnorm.r")
source("fonctions/prob_ad.r")
source("fonctions/prob.log.r")
source("fonctions/prob.log2.r")

#*********************************************#
#                                             #
#              TEST FUNCTION                  #
#                                             #
#*********************************************#

test <- function(data, Nmax)
{
    X  = data[,1:ncol(data)-1]
    z  = data[,ncol(data)]

    p = ncol(X)
    n = nrow(X)

    
    error = matrix(nrow = 6, ncol = Nmax)
    rownames(error) = c("LDA", "QDA", "Naive Bayes", "Log Reg", "Quad Log Reg", "Tree")
    
    for(i in 1:Nmax)
    {
        train   = sample(1:n, round(2*n/3))
        Xapp    = X[train, ]
        zapp    = z[train]

        Xtst  = X[-train, ]
        ztst  = z[-train]


        #*******************************LDA*******************************
        param      = adl.app(Xapp, zapp)
        out        = ad.val(param, Xtst)
        error.lda  = sum(abs(ztst - out$pred))
        error[1,i] = error.lda


        #*******************************QDA*******************************
        param      = adq.app(Xapp, zapp)
        out        = ad.val(param, Xtst)
        error.qda  = sum(abs(ztst - out$pred))
        error[2,i] = error.qda

        #*******************************Naive Bayes*******************************
        param      = nba.app(Xapp, zapp)
        out        = ad.val(param, Xtst)
        error.nba  = sum(abs(ztst - out$pred))
        error[3,i] = error.nba
        
        #*******************************Log Reg*******************************
        param        = log.app(Xapp, zapp, T, 0.001)
        out          = log.val(param$beta, Xtst)
        error.logreg = sum(abs(ztst - out$pred))
        error[4,i]   = error.logreg

        #*******************************Quad Log Reg*******************************
        Xapp2 <- Xapp
        Xtst2 <- Xtst

        for (p in 1:(dim(Xapp)[2]-1))
        {
            for (q in (p+1):dim(Xapp)[2])
            {
                Xapp2 <- cbind(Xapp2, Xapp[,p]*Xapp[,q])
                Xtst2 <- cbind(Xtst2, Xtst[,p]*Xtst[,q])
            }
        }

        for (p in 1:dim(Xapp)[2])
        {
            Xapp2 <- cbind(Xapp2, Xapp[,p]^2)
            Xtst2 <- cbind(Xtst2, Xtst[,p]^2)
        }
        
        
        param            = log.app(Xapp2, zapp, T, 0.001)
        out              = log.val(param$beta, Xtst2)
        error.logreg.qua = sum(abs(ztst - out$pred))
        error[5,i]       = error.logreg.qua
        
         #*******************************Tree*******************************
        zapp <- as.factor(zapp)
        data.tree <- tree(zapp~., data = Xapp)
        #data1.tree
        pred <- predict(data.tree, Xtst)
        zpred <- apply(pred, 1, which.max)
        #out.tree <- ad.val(data.nba, Xtst)
        tree.error <- sum(abs(zpred-ztst))
        error[6,i] = tree.error

        
    }
    
    out = ((rowMeans(error))/nrow(Xtst))*100
    
    out
}

#*********************************************#
#                                             #
#      TEST ON SYNTH1-SYNTH2-SYNTH3           #
#                                             #
#*********************************************#


#On pourra s’appuyer sur les frontières de décision obtenues pour analyser les résultats. 
#Sachant que les données suivent dans chaque classe une loi normale multivariée, 
#comment peut-on interpréter ces résultats ?
print("synth1")
test(synth1, 20)
nrow(synth1)

print("synth2")
test(synth2, 20)

print("synth3")
test(synth3, 20)

#*********************************************#
#                                             #
#             PLOT FRONTIER SYNTH1            #
#                                             #
#*********************************************#
data1 = synth1
X1 <- data1[,1:ncol(data1)-1]
z1 <- data1[,ncol(data1)]

# adq
param = adq.app(X1, z)
out   = ad.val(param, X1)
prob.ad(param, X1, z1, c(0.2, 0.4, 0.6, 0.8),"QDA")


# adl

param = adl.app(X1, z1)
out   = ad.val(param, X1)
prob.ad(param, X1, z1, c(0.2, 0.4, 0.6, 0.8), "LDA")


# nba

param = nba.app(X1, z1)
out   = ad.val(param, X1)
prob.ad(param, X1, z1, c(0.23, 0.43, 0.63, 0.83), "Naive Bayes")

#regression log

out = log.app(X1, z1, T, 0.1)
outB   = out$beta
prob.log(outB , X1, z1, c(0.2, 0.4, 0.6, 0.8), "Logistic Regression")
dev.off();

#regression log quadratique
X2 <- X1

for (p in 1:(dim(X1)[2]-1))
{
    for (q in (p+1):dim(X1)[2])
    {
        X2 <- cbind(X2, X1[,p]*X1[,q])
    }
}

for (p in 1:dim(X1)[2])
{
    X2 <- cbind(X2, X1[,p]^2)
}
X2 <- as.matrix(X2)

out = log.app(X2, z1, T, 0.1)
outB   = out$beta
prob.log2(outB , X2, z1, c(0.2, 0.4, 0.6, 0.8),"Quadratic Logistic Regression")

#*********************************************#
#                                             #
#             PLOT FRONTIER SYNTH2            #
#                                             #
#*********************************************#

data  = synth2
Xapp  = data[,1:ncol(data)-1]
zapp  = data[,ncol(data)]

print("LDA")
param      = adl.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 8.05%", "LDA")


param = adq.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 6.30%", "QDA")


print("NB")
param = nba.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 6.30%", "Naive Bayes")


print("Log Reg")
out = log.app(Xapp, zapp, T, 0.001)  
prob.log(out$beta, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8),"Erreur de test = 7.10%", "Logistic Regression")


Xapp2 <- Xapp

for (p in 1:(dim(Xapp)[2]-1))
{
    for (q in (p+1):dim(Xapp)[2])
    {
        Xapp2 <- cbind(Xapp2, Xapp[,p]*Xapp[,q])
    }
}

for (p in 1:dim(Xapp)[2])
{
    Xapp2 <- cbind(Xapp2, Xapp[,p]^2)
}
Xapp2 <- as.matrix(Xapp2)

print("Log Reg Quad")
param            = log.app(Xapp2, zapp, T, 0.001)
prob.log2(param$beta, Xapp2, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 6.38% ", "Quadratic Logistic Regression")


#*********************************************#
#                                             #
#             PLOT FRONTIER SYNTH3            #
#                                             #
#*********************************************#


data  = synth3
Xapp  = data[,1:ncol(data)-1]
zapp  = data[,ncol(data)]

print("LDA")

param      = adl.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.11%", "LDA")


print("QDA")

param = adq.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.7%", "QDA")



print("NB")

param = nba.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8),"Erreur de test = 4.89%", "Naive Bayes")


print("Log Reg")
out = log.app(Xapp, zapp, T, 0.001)  
prob.log(out$beta, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.14", "Logistic Regression")

Xapp2 <- Xapp

for (p in 1:(dim(Xapp)[2]-1))
{
    for (q in (p+1):dim(Xapp)[2])
    {
        Xapp2 <- cbind(Xapp2, Xapp[,p]*Xapp[,q])
    }
}

for (p in 1:dim(Xapp)[2])
{
    Xapp2 <- cbind(Xapp2, Xapp[,p]^2)
}


Xapp2 = as.matrix(Xapp2)
print("Log Reg Quad")
param            = log.app(Xapp2, zapp, T, 0.001)
prob.log2(param$beta, Xapp2, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.16%", "Quadratic Logistic Regression")
