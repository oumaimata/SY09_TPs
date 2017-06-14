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


# Navive Bayesian Classfifier
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
        param$prop[k]          =  abind(length(indk)/n)
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

    densite.melange
}
