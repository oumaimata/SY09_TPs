
#*********************************************#
#                                             #
#                   Data.                     #
#                                             #
#*********************************************#

bcw        = read.csv("data/bcw.csv", header=T)
speam      = read.csv("data/spam.csv", header=T)
pima       = read.csv("data/Pima.csv", header=T)
synth140   = read.csv("data/Synth1-40.csv", header = T)
synth2     = read.csv("data/Synth2-1000.csv", header = T)
synth3     = read.csv("data/Synth3-1000.csv", header=T)

summary(bcw)

#*********************************************#
#                                             #
#                   Libraries                 #
#                                             #
#*********************************************#

library(abind)
library(MASS)



#*********************************************#
#                                             #
#                   Includes                  #
#                                             #
#*********************************************#

source("fonctions/mvdnorm.r")
source("fonctions/prob_ad.r")

# Analyse Disc Quadrqtique
adq.app <- function(Xapp, zapp)
{

    n = dim(Xapp)[1]
    p = dim(Xapp)[2]
    g = max(unique(zapp))

    param <- NULL
    param$MCov = array(0, c(p,p,g))
    param$mean = array(0, c(g,p))
    param$prop = rep(0, g)

    for (k in 1:g)
    {
        #retourne les indices
        indk            = which(zapp==k)
        
        Xapp_g = Xapp[indk,]
        
        #is.array(arr) = TRUE
        param$MCov[,,k] = abind(cov(Xapp_g))
        
        param$mean[k,]  = abind(t(colMeans(Xapp_g)))
        
        param$prop[k]   = length(indk)/n
    }

    param
}



# Analyse Disc Lineaire
adl.app <- function(Xapp, zapp)
{
adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		MCov <- MCov + (cov(Xapp[indk,])* length(indk))
		param$mean[k,] <- apply(Xapp[indk,],2,mean)
		param$prop[k] <- nrow(Xapp[indk,])/nrow(Xapp)
	}
	MCov <- MCov / n
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	}

	param
}
}



# Naive Bayesian Classifier
nba.app <- function(Xapp, zapp)
{
    n <- dim(Xapp)[1]
    p <- dim(Xapp)[2]
    g <- max(unique(zapp))

    param <- NULL
    param$MCov <- array(0, c(p,p,g))
    param$mean <- array(0, c(g,p))
    param$prop <- rep(0, g)

    for (k in 1:g)
    {
        indk <- which(zapp==k)
        Xapp_g = Xapp[indk, ]
        diag(param$MCov[,,k])  = abind(diag(cov(Xapp_g)))
        param$mean[k,]         = abind(t(colMeans(Xapp_g))) 
        param$prop[k]          = abind(length(indk)/n)
    }

    param
}

#Test function
ad.val <- function(param, Xtst)
{
    n <- dim(Xtst)[1]
    p <- dim(Xtst)[2]
    g <- length(param$prop)

    out <- NULL
    prob <- matrix(0, nrow=n, ncol=g)
    
    #mvdnorm <- function(X, mu, Sigma)
    for( k in 1:g)
    {
         prob[,k]= param$prop[k]* mvdnorm(Xtst, param$mean[k,], param$MCov[,,k])
    }
    
    densite.melange = rowSums(prob)
    prob = prob / densite.melange
    
    #returns the index
    pred = max.col(prob)

    out$prob <- prob
    out$pred <- pred

    out
}

#Test QDA
data  = synth140
Xapp  = data[,1:ncol(data)-1]
zapp  = data[,ncol(data)]


param = adq.app(Xapp, zapp)
#param

out   = ad.val(param, Xapp)
#out

prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8))

#Test LDA
data  = synth140
Xapp  = data[,1:ncol(data)-1]
zapp  = data[,ncol(data)]


#Test Naive Bayesian Classifier
data  = synth140
Xapp  = data[,1:ncol(data)-1]
zapp  = data[,ncol(data)]


param = nba.app(Xapp, zapp)
#param

out   = ad.val(param, Xapp)
#out

prob.ad(param, Xapp, zapp, c(0.23,0.43,0.63,0.83))













