
library(MASS)
library(ggplot2)
library(GGally)
library(ggfortify)
library(ggtern)
library(e1071)
library(class)
library(tree)
library(abind)
library(adabag)
library(randomForest)

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


##################################################
#                                               #
#     Test function  WITH CENTERING             #
#                                               #
#################################################
test2 <- function(data, Nmax)
{
    n = nrow(data)
    p = ncol(data)
    
    X  = data[,-p]
    z  = data[,p]
    
    error = matrix(nrow = 4, ncol = Nmax)
    rownames(error) = c("LDA", "QDA", "Naive Bayes", "Logistic Regression")
    
    for(i in 1:Nmax)
    {
        train   = sample(1:n, round(2*n/3))
        Xapp    = X[train,]
        zapp    = z[train]
        Xtst    = X[-train,]
        ztst    = z[-train]
        
        means = colMeans(Xapp)
        sdev  = apply(Xapp, 2, sd)
        
        for(j in 1:(p-1))
        {
            Xapp[, j] = (as.matrix(Xapp[, j]) -  rep.int(means[j], times=nrow(Xapp)))/ rep.int(sdev[j], times=nrow(Xapp))    
            Xtst[, j] = (as.matrix(Xtst[, j]) -  rep.int(means[j], times=nrow(Xtst)))/ rep.int(sdev[j], times=nrow(Xtst))
        }

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
    
    #error
    out = ((rowMeans(error))/nrow(Xtst))*100
        
    out
}


spam      = read.csv("data/spam.csv", header=T)
data = spam
test2(data, 2)


##################################################
#                                               #
#          CENTER AND REDUCE THE DATA           #
#                                               #
#################################################
#Centrer reduire Xapp total -->  appliquer au Xtst
n = nrow(spam)
train   = sample(1:n, round(2*n/3))

Xapp    = X[train,]
zapp    = z[train]

Xtst    = X[-train,]
ztst    = z[-train]

means = colMeans(Xapp)
sdev  = apply(Xapp, 2, sd)

for(j in 1:(p-1))
{
    Xapp[, j] = (as.matrix(Xapp[, j]) -  rep.int(means[j], times=nrow(Xapp)))/ rep.int(sdev[j], times=nrow(Xapp))    
    Xtst[, j] = (as.matrix(Xtst[, j]) -  rep.int(means[j], times=nrow(Xtst)))/ rep.int(sdev[j], times=nrow(Xtst))
}



##################################################
#                                               #
#  PCA BUT NOT WORKING WELL ON THODE DATA       #
#                                               #
#################################################

spam.pca = princomp(Xapp)
plot(spam.pca)

eigenval     = spam.pca$sd^2
perc.inertie = cumsum(eigenval/sum(eigenval)*100)

bp = barplot(perc.inertie)
#text(bp, 0, round(perc.inertie, 1),cex=1,pos=3) 

autoplot(spam.pca, data= Xapp, colour= zapp, shape= FALSE,  loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=1, y=3)




#################################################
#                                               #
#                KNN WITH CROSS VALIDATION      #
#                                               #
#################################################

#CROSS VALIDATION
zapp = factor(zapp)
spam.knn.cv = tune.knn(x= Xapp, y= zapp, k=1:5, tunecontrol=tune.control(sampling = "cross"), cross=10)

summary(spam.knn.cv)


spam.knn.pred = knn(train = Xapp, test = Xtst, cl = zapp, k =spam.knn.cv$best.parameters)

res = with(Xtst, table(spam.knn.pred, ztst))
error.test = res[1,2] + res[2,1] / nrow(Xtst)
print(paste("Tree Test Error  ",error.test))





#################################################
#                                               #
#                      TREE                     #
#                                               #
#################################################

zapp = factor(zapp)
spam.tree = tree(zapp ~ ., data = Xapp,  method = "class", control=tree.control(nobs=dim(Xapp)[1], mindev = 0.0001))
spam.tree.cv = cv.tree(spam.tree,  FUN=prune.misclass, K = 10)

min.index = which(spam.tree.cv $dev==min(spam.tree.cv$dev))
best.size = spam.tree.cv$size[min.index] # which size is better?


print("Best Tree size = ")
print(best.size)

plot(spam.tree.cv, type='o', col = 'blue', pch = 19 )



spam.cv.pruned = prune.misclass(spam.tree, best=best.size)

spam.tree.pred = predict(spam.cv.pruned, newdata = Xtst, type="class")
res = with(Xtst, table(spam.tree.pred, ztst))
error.test = (res[1,2] + res[2,1]) / nrow(Xtst)

print(paste("Tree Test Error  ",error.test))



#################################################
#                                               #
#                  RANDOM FOREST                #
#                                               #
#################################################


spam.rf = randomForest(zapp~.,data=Xapp)
spam.rf

spam.rf.pred = predict(spam.rf, newdata=Xtst, type="class")
res = with(Xtst, table(spam.rf.pred , ztst))
error.test = (res[1,2] + res[2,1]) / nrow(Xtst)

print(paste("Tree Test Error  ",error.test))



