
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

bcw        = read.csv("data/bcw.csv", header=T)
summary(bcw)
nrow(bcw)
ncol(bcw)

#*********************************************#
#                                             #
#   TEST FUNCTION FOR DISCRIMATION            #
#                                             #
#*********************************************#
 
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

test2(bcw, 100)

bcw$z = as.factor(bcw$z)
ggpairs(bcw, mapping = aes(color = z), columns = 1:4)


#*********************************************#
#                                             #
#                 SEPARATE DATA               #
#                                             #
#*********************************************#




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


#*********************************************#
#                                             #
#                      TREE                   #
#                                             #
#*********************************************#

        
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
