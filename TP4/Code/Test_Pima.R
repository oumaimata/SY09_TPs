
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
#                   Data.                     #
#                                             #
#*********************************************#

pima        = read.csv("data/pima.csv", header=T)
summary(pima)
nrow(pima)
ncol(pima)

#*********************************************#
#                                             #
#   TEST FUNCTION FOR DISCRIMATION            #
#                                             #
#*********************************************#
 

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

# get errors for pima 
error.pima <- test(pima, 100)
print(error.pima)
#pima.X <- pima[,1:ncol(pima)-1]


pima$z = as.factor(pima$z)
ggpairs(pima, mapping = aes(color = z), columns = 1:7)

