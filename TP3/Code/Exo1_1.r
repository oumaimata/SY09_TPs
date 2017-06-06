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
#                   Includes                  #
#                                             #
#*********************************************#

source("data/fonctions/front.ceuc.R")
source("data/fonctions/front.kppv.R")
source("data/fonctions/separ1.R")
source("data/fonctions/separ2.R")
source("data/fonctions/distXY.R")



#*********************************************#
#                                             #
#            Classifieur Euclidien            #
#                                             #
#*********************************************#


ceuc.app = function(Xapp, zapp)
{
    classes = unique(zapp)
  
    n1 = nrow(table(which(zapp == classes[1])))
    n2 = nrow(table(which(zapp == classes[2])))
    
    nk = c(n1, n2)
    
    mu = rowsum(Xapp, group = zapp)
    mu = mu/ nk;
    
    return (mu);
}

ceuc.val <- function(mu, Xtst)
{
   
    dist = distXY(Xtst, mu)

    #apply minimum by row
    shortest = as.matrix(apply(dist , 1, which.min))
    
    row.names(shortest) = NULL
    return (shortest)
}

#*********************************************#
#                                             #
#                    KPPV                     #
#                                             #
#*********************************************#

kppv.val <- function(Xapp, zapp, K, Xtst)
{ 
    
    dist  = distXY(Xapp, Xtst)
    dist2 = apply(dist, 2, order)
    dist2 = as.matrix(dist2[1:K,])
    
    if (K == 1)
        dist2 = t(dist2)
    
    zneighbors = matrix(nrow=K, ncol=ncol(dist2))

    for(i in 1:nrow(zneighbors))
    {
        for(j in 1:ncol(zneighbors))
        {
            zneighbors[i,j] = zapp[dist2[i,j]]
        }


    }

     #PERFECT IT ALSO RETURN MINIMUM INDEX IF EQUAL!
    pred = as.matrix(apply(zneighbors, 2, function(x) names(which.max(table(x)))))
    pred = as.numeric(as.character(pred))
        
    return(pred)
}

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{

    nbk = length(nppv)
    err = numeric(nbk)
    i = 1
    
    for(k in nppv)
    {
        pred   = kppv.val(Xapp, zapp, k, Xval)
        err[i] = sum(abs(pred - zval))

        i = i+1
    }

    min = apply(as.matrix(err), 2, which.min)

    return (nppv[min])

}

#*********************************************#
#                                             #
#                    TEST                     #
#                                             #
#*********************************************#

data = synth40
X <- data[,1:2]
z <- data[,3]

mu = ceuc.app(X, z)

front.ceuc(X, z, mu, 1000) 
front.kppv(X, z, 3, 1000) 




