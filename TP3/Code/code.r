
library(pdist)
library(dplyr)

#Data
synth40 = read.csv("data/data/Synth1-40.csv", header=T)
synth100 = read.csv("data/data/Synth1-100.csv", header=T)
synth500 = read.csv("data/data/Synth1-500.csv", header=T)
synth1000 = read.csv("data/data/Synth1-1000.csv", header=T)
synth21000 = read.csv("data/data/Synth2-1000.csv", header=T)


source("data/fonctions/front.ceuc.R")
source("data/fonctions/front.kppv.R")
source("data/fonctions/separ1.R")
source("data/fonctions/separ2.R")
source("data/fonctions/distXY.R")

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

    #which min returns the indexes
   # pred = t(dist[0,shortest])
 
    #because the classes now are rownames and chars so we have to convert the rownames into a numeric column!
    #pred = as.matrix(transform(rownames(pred), pred = as.numeric(rownames(pred)))[,1])
    
}


kppv.val <- function(Xapp, zapp, K, Xtst)
{
  
    dist = distXY(Xtst, Xapp)
    dist = apply(dist, 2, order) 

    dist = as.matrix(dist[,1:K])

    #PERFECT IT ALSO RETURN MIN INDEX IF EQUAL!
    pred = as.matrix(apply(dist, 1, function(x) names(which.max(table(x)))))
    
    #because the classes now are rownames and chars so we have to convert the rownames into a numeric column!
    pred = as.numeric(as.character(pred))
    #pred = transform(pred, pred = as.numeric(pred))[,2]
    
        
    return((zapp[pred]))
}

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{

    nbk = length(nppv)
    err = numeric(nbk)
    i = 1
    
    for(k in nppv)
    {
        pred   = kppv.val(Xapp, zapp, k, Xtst)
        err[i] = sum(abs(pred - zval))

        i = i+1
    }

    min = apply(as.matrix(err), 2, which.min)

    return (nppv[min])
}

#Test
data = synth40
X <- data[,1:2]
z <- data[,3]

mu = ceuc.app(X, z)
mu

front.ceuc(X, z, mu, 1000) 


nppv = 2*(1:6)-1
nppv
Kopt      = kppv.tune(X, z, X, z, nppv)

#Kopt
front.kppv(X, z, Kopt, 1000) 


#Question1: Estimation de parametres
    
    #data = synth40
    #data = synth100
    #data = synth500
    data = synth1000

    paramtV1    = summarise(group_by(data, z), mean= mean(V1) , sd= sd(V1), cov = cov(V1,V2))
    paramtV1[,3] = paramtV1[,3] * paramtV1[,3] #compute variance from sd

    paramtV2    = summarise(group_by(data, z), mean= mean(V2) , sd= sd(V2), cov = cov(V1,V2))
    paramtV2[,3] = paramtV2[,3] * paramtV2[,3] #compute variance from sd

    paramtV1
    paramtV2

    mu.k1  = c(paramtV1[1,2], paramtV2[1,2])
    mu.k2  = c(paramtV1[2,2], paramtV2[2,2])

    sig.k1 = c(paramtV1[1,3], paramtV2[1,3])
    sig.k2 =  c(paramtV1[2,3], paramtV2[2,3])


    pi1      = length(data$z[data$z==1])/nrow(data)
    pi2      = length(data$z[data$z==2])/nrow(data)

    pi = cbind(pi1, pi2)
    pi
    

#Question2: Erreur Classifieur Euclidien

    data = synth40
    #data = synth100
    #data = synth500
    data = synth1000

    Nmax = 20
    
    n = nrow(data)
    X = data[,1:2]
    z = data[,3]

    err = matrix(nrow=2, ncol= Nmax)

    for(i in 1:Nmax)
    {
        donn.sep <- separ1(X, z)
        Xapp <- donn.sep$Xapp
        zapp <- donn.sep$zapp
        Xtst <- donn.sep$Xtst
        ztst <- donn.sep$ztst

        mu       = ceuc.app(Xapp, zapp)

        predapp  = ceuc.val(mu, Xapp)

        predtst  = ceuc.val(mu, Xtst)

        err[1,i] = sum(abs(predapp - zapp))/(2*n/3)
        err[2,i] = sum(abs(predtst - ztst))/(n/3)  

    }

    mu.app = mean(err[1,])
    mu.tst = mean(err[2,])


    sig.app = var(err[1,])
    sig.tst = var(err[2,])


    #napp = 2n/3
    factor = 0.95*(sig.app/sqrt(2*n/3))
    Ic.app = c(mu.app- factor, mu.app+ factor)

    #ntst = n/3
    factor = 0.95*(sig.tst/sqrt(n/3))
    Ic.tst = c(mu.tst- factor, mu.tst+ factor)


    est = c(mu.app, Ic.app, mu.tst, Ic.tst)
est

#Meme si l erreur est moins , si les intervalles de confiances ne sont pas disjoints cela veut dire que les 2
#peuvent coincider et donc on ne peut pas conclure qu'une methode est meilleure que l'autre

#plus on augmente la taille de Xapp plus on se rapproche de la regle de Bayes
#taux d erreur Bayes de theorique est le taux en dessous duquel les taux d erreur theorique ne peuvent pas descendre
   

#Question 3 et 2 esemble: tune de KNN

        data = synth40
        data = synth40
        #data = synth100
        #data = synth500
        data = synth1000

        n = nrow(data)
        X = data[,1:2]
        z = data[,3]

        donn.sep <- separ1(X, z)
        Xapp <- donn.sep$Xapp
        zapp <- donn.sep$zapp
        Xtst <- donn.sep$Xtst
        ztst <- donn.sep$ztst
        
        nppv = 2*(1:6)-1

        bestk = kppv.tune(Xapp, zapp, Xtst, ztst, nppv)
        bestk

    

#Question 4 : Erreur du KNN
    Nmax = 20

    
     data = synth40
     #data = synth100
     #data = synth500
     #data = synth1000
    n = nrow(data)
    X = data[,1:2]
    z = data[,3]

    nppv = 2*(1:6)-1
    err = numeric(Nmax)  
       
    for(i in 1:Nmax)
    {     
        
        donn.sep <- separ2(X, z)
        Xapp <- donn.sep$Xapp
        zapp <- donn.sep$zapp
        
        Xval <- donn.sep$Xval
        zval <- donn.sep$zval
        
        bestk = kppv.tune(Xapp, zapp, Xtst, ztst, nppv)
        
        Xtst <- donn.sep$Xtst
        ztst <- donn.sep$ztst
        
        predtst  = kppv.val(Xapp, zapp,  bestk, Xtst)

        err[i] = sum(abs(predtst - zval))/(n/4)
        
    }

    
    mu.tst = mean(err)

    #sig.app = var(err[1,])
    sig.tst = var(err)

    #nval = n/4
    #factor = 0.95*(sig.app/sqrt(n/4))
    #Ic.app = c(mu.app- factor, mu.app+ factor)

    #ntst = n/4
    factor = 0.95*(sig.tst/sqrt(n/4))
    Ic.tst = c(mu.tst- factor, mu.tst+ factor)


    est = c( mu.tst, Ic.tst)
est

#Jeu de données Synth2-1000


#Pima


#BreastCancer

#function order to sort, to order distance asc, beware of the return structure !!
#apply( D, 2, order) 
#il faut dire a quoi correspondent les lignes et a quoi correspondent les colonnes pour appl;y avec 
#2 :orders the columns if tests are in columns or 1 :orders the lines
#apply returns a vector of vector 

data = synth40
x1 = 4*data$V1
x2 = 2*data$V2

pi1      = length(data$z[data$z==1])/nrow(data)
pi2      = length(data$z[data$z==2])/nrow(data)
y = 2*log(pi1/pi2) + 3

plot(x1, x2)
abline(0, y, col="red")








Xapp = matrix(  c(2, 4, 3, 1, 5, 7, 6, 8,0, 7, 5,1), nrow=4, ncol=3, byrow = TRUE)
print("Xapp")
Xapp

zapp = matrix(  c(3, 4, 4, 3), nrow=4, ncol=1, byrow = TRUE)
print("zapp")
zapp

Xtst =  matrix(  c(4, 9, 7, 1, 5, 7, 1, 4, 1 ), nrow=3, ncol=3, byrow = TRUE)
print("Xtst")
Xtst

mu = ceuc.app(Xapp, zapp)
print("mu")
mu

ztst = ceuc.val(mu, Xtst)
print("ztst")
ztst
        
      






