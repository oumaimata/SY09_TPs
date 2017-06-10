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
source("Exo1.R")



#*********************************************#
#                                             #
#    Question1: Estimation de parametres      #
#                                             #
#*********************************************#



est.parameters <- function(data)
{ 

    
    paramtV1    = summarise(group_by(data, z), mean= mean(V1) , var= var(V1), cov = cov(V1,V2))

    paramtV2    = summarise(group_by(data, z), mean= mean(V2) , var= var(V2), cov = cov(V1,V2))

    paramtV1
    paramtV2

    mu.k1  = c(paramtV1[1,2], paramtV2[1,2])
    mu.k2  = c(paramtV1[2,2], paramtV2[2,2])

    sig.k1 = c(paramtV1[1,3], paramtV2[1,3])
    sig.k2 =  c(paramtV1[2,3], paramtV2[2,3])


    pi1      = length(data$z[data$z==1])/nrow(data)
    pi2      = length(data$z[data$z==2])/nrow(data)

    pi = cbind(pi1, pi2)
    out = cbind(paramtV1, paramtV2, pi)
    return (out)
    
    
}

data = synth40
est.parameters(data)

data = synth40
est.parameters(data)

data = synth100
est.parameters(data)

data = synth500
est.parameters(data)

data = synth1000
est.parameters(data)

data = synth21000
est.parameters(data)


#*********************************************#
#                                             #
#   Question2: Erreur Classifieur Euclidien   #
#                                             #
#*********************************************#
#
ceuc.err <- function(data)
{ 
    Nmax = 20
    p = ncol(data)
    n = nrow(data)
    X = data[, 1:p-1]
    z = data[, p]

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


    sig.app = sd(err[1,])
    sig.tst = sd(err[2,])


    #napp = 2n/3
    factor = 0.95*(sig.app/sqrt(2*n/3))
    Ic.app = c(mu.app- factor, mu.app+ factor)

    #ntst = n/3
    factor = 0.95*(sig.tst/sqrt(n/3))
    Ic.tst = c(mu.tst- factor, mu.tst+ factor)

    #returns a vector
    est = c(mu.app, Ic.app, mu.tst, Ic.tst)
    return(est)
}
print("Synth40")
data = synth40
ceuc.err(data)

print("Synth100")
data = synth100
ceuc.err(data)

print("Synth500")
data = synth500
ceuc.err(data)

print("Synth1000")
data = synth1000
ceuc.err(data)

print("Synth2")
data = synth21000
ceuc.err(data)

print("Pima")
data = pima
ceuc.err(data)

print("Cancer")
data = cancer
ceuc.err(data)


#*********************************************#
#                                             #
#        Question 3:  Tune KNN                #
#                                             #
#*********************************************#
data = synth40

n = nrow(data)
X = data[,1:2]
z = data[,3]

donn.sep = separ1(X, z)
Xapp     = donn.sep$Xapp
zapp     = donn.sep$zapp
Xtst     = donn.sep$Xtst
ztst     =donn.sep$ztst 
nppv = 2*(1:6)-1

#kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
bestk = kppv.tune(Xapp, zapp, Xapp, zapp, nppv)
bestk
#1

#*********************************************#
#                                             #
#       Question 4:  Erreur du KPPV           #
#                                             #
#*********************************************#
kppv.err(data)
{
    Nmax = 20

    n = nrow(data)
    p = ncol(data)
    X = data[,1:p-1]
    z = data[,p]

    nppv = 2*(1:10)-1
    err = numeric(Nmax)  
       
    for(i in 1:Nmax)
    {     
        
        donn.sep <- separ2(X, z)
        Xapp <- donn.sep$Xapp
        zapp <- donn.sep$zapp
        
        Xval <- donn.sep$Xval
        zval <- donn.sep$zval
        
        bestk = kppv.tune(Xapp, zapp, Xval, zval, nppv)
        
        Xtst <- donn.sep$Xtst
        ztst <- donn.sep$ztst
        
        predtst  = kppv.val(Xapp, zapp,  bestk, Xtst)

        err[i] = sum(abs(predtst - ztst))/(n/4)
        
    }

    mu.tst  = mean(err)
    sig.tst = sd(err)

    #ntst = n/4
    factor = 0.95*(sig.tst/sqrt(n/4))
    Ic.tst = c(mu.tst- factor, mu.tst+ factor)

    est = c( mu.tst, Ic.tst)
    return(est)
}

print("Synth40")
data = synth40
kppv.err(data)

print("Synth100")
data = synth100
kppv.err(data)

print("Synth500")
data = synth500
kppv.err(data)

print("Synth1000")
data = synth1000
kppv.err(data)

print("Synth2")
data = synth21000
kppv.err(data)

print("Pima")
data = pima
kppv.err(data)

print("Cancer")
data = cancer
kppv.err(data)

