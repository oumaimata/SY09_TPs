
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

    
    error = matrix(nrow = 5, ncol = Nmax)
    rownames(error) = c("LDA", "QDA", "Naive Bayes", "Log Reg", "Quad Log Reg")
    
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
test(synth1, 40)
nrow(synth1)

print("synth2")
test(synth2, 40)

print("synth3")
test(synth3, 40)


#*********************************************#
#                                             #
#             PLOT FRONTIER SYNTH2            #
#                                             #
#*********************************************#

data  = synth2
Xapp  = data[,1:ncol(data)-1]
zapp  = data[,ncol(data)]

png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth2_lda.png")
print("LDA")
param      = adl.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 8.05%", "LDA")
dev.off()

#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth2_qda.png")
print("QDA")
param = adq.app(Xapp, zapp)
#prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 6.30%", "QDA")
#dev.off()

#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth2_nb.png")
print("NB")
param = nba.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 6.30%", "Naive Bayes")
#dev.off()

#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth2_logreg.png")
print("Log Reg")
out = log.app(Xapp, zapp, T, 0.001)  
prob.log(out$beta, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8),"Erreur de test = 7.10%", "Logistic Regression")
#dev.off()

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
#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth2_logregquad.png")
print("Log Reg Quad")
param            = log.app(Xapp2, zapp, T, 0.001)
prob.log2(param$beta, Xapp2, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 6.38% ", "Quadratic Logistic Regression")
#dev.off()


#*********************************************#
#                                             #
#             PLOT FRONTIER SYNTH3            #
#                                             #
#*********************************************#


data  = synth3
Xapp  = data[,1:ncol(data)-1]
zapp  = data[,ncol(data)]

print("LDA")
png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth3_lda.png")
param      = adl.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.11%", "LDA")
dev.off()

print("QDA")
#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth3_qda.png")
param = adq.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.7%", "QDA")
#dev.off()


print("NB")
#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth3_nb.png")
param = nba.app(Xapp, zapp)
prob.ad(param, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8),"Erreur de test = 4.89%", "Naive Bayes")
#dev.off()


#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth3_logreg.png")
print("Log Reg")
out = log.app(Xapp, zapp, T, 0.001)  
prob.log(out$beta, Xapp, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.14", "Logistic Regression")
#dev.off()

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

#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/synth3_logregquad.png")
Xapp2 = as.matrix(Xapp2)
print("Log Reg Quad")
param            = log.app(Xapp2, zapp, T, 0.001)
prob.log2(param$beta, Xapp2, zapp, c(0.2, 0.4, 0.6, 0.8), "Erreur de test = 4.16%", "Quadratic Logistic Regression")
#dev.off()



 


test2 <- function(data, Nmax)
{
    X  = data[,1:ncol(data)-1]
    z  = data[,ncol(data)]

    p = ncol(X)
    n = nrow(X)

    
    error = matrix(nrow = 4, ncol = Nmax)
    rownames(error) = c("LDA", "QDA", "Naive Bayes", "Log Reg")
    
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

       
    }
    
     out = ((rowMeans(error))/nrow(Xtst))*100
         out
}



summary(bcw)
nrow(bcw)
ncol(bcw)

test2(bcw, 100)
test2(pima, 100)

bcw$z = as.factor(bcw$z)
ggpairs(bcw, mapping = aes(color = z), columns = 1:4)


#rpart(z~V2+V3+V4+V5+V6+V7+V8+V9+V10, data=bcw, method="class") 
set.seed(1)
X  = bcw[, 1:ncol(bcw)-1]
z  = bcw[, ncol(bcw)]

p = ncol(X)
n = nrow(X)

train   = sample(1:n, round(2*n/3))
Xapp    = X[train, ]
zapp    = z[train]
zapp    = factor(zapp)

Xtst  = X[-train, ]
ztst  = z[-train]
        
        
        
bcw.tree = tree(zapp ~ ., data = Xapp,  method = "class", control=tree.control(nobs=dim(Xapp)[1], mindev = 0.0001))
summary(bcw.tree)
#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/bcw_apptree.png")
plot(bcw.tree)
text(bcw.tree, cex=.65)
#control=tree.control(nobs=dim(Dapp)[1],mindev = 0.0001)
#dev.off()


set.seed(2)

bcw.cv = cv.tree(bcw.tree,  FUN=prune.misclass, K = 10)
#number of missclassed per tree size --> min missclassified is 22 with size 7

#png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/bcw_cvplot.png")
plot(bcw.cv, type='o', col = 'blue', pch = 19 )
#dev.off()

print("Best size")
min.index = which(bcw.cv$dev==min(bcw.cv$dev))
best.size <- bcw.cv$size[min.index] # which size is better?
best.size


# let's refit the tree model (the number of leafs will be no more than best.size)
bcw.cv.pruned = prune.misclass(bcw.tree, best=best.size)
summary(bcw.cv.pruned)

png("/Users/zineb/Desktop/Studies/GI05/SY09/TD1/SY09_TPs/TP4/Figures/bcw_prunedtree.png")
plot(bcw.cv.pruned)
text(bcw.cv.pruned, cex=.85)
dev.off()

bcw.predict = predict(bcw.cv.pruned, Xtst, type="class")
table(ztst, bcw.predict)

tree.pred = predict(bcw.cv.pruned, newdata=Xtst, type = "class")
length(tree.pred)
nrow(Xtst)
nrow(Xapp)
res = with(Xtst, table(tree.pred, ztst))
error.test = (res[1,2] + res[2,1]) / nrow(Xtst)
print(paste("Test Error  ",error.test))

#matrice de oncfusion : quelle est la classe qui semple etre mal classee
#Si performante a identifier ces indivus plutot que ceux la 
