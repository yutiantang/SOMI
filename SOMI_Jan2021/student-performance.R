#################   Student Performance Example   #################
# Nathaniel E. Helwig
#   Adding bias to reduce variance in psychological results: 
#   A tutorial on penalized regression
#   Submitted to The Quantitative Methods for Psychology (TQMP)


#########***#########   LOAD DATA AND PACKAGES   #########***#########

### data availabe from UCI machine learning repository:
### https://archive.ics.uci.edu/ml/datasets/Student+Performance

### define file path and load data
datapath = "~/Desktop/psych-penreg/student-mat.csv"
student = read.table(datapath, sep=";", header=TRUE)

### load MASS library (for ridge)
library(MASS)    # ver 7.3-45

### load glmnet library (for lasso and elastic net)
library(glmnet)  # ver 2.0-5


#########***#########   DATA PREPROCESSING   #########***#########

### create response variable
y = student$G1
n = length(y)

### create model design matrix (without intercept)
X = model.matrix(~., data=student[,1:30])
X = X[,-1]

### check size of X
dim(X)


#########***#########   OLS REGRESSION   #########***#########

### fit linear regression model via OLS
olsmod = lm(y ~ ., data=data.frame(X))

### summarize model results
olsmod.sum = summary(olsmod)
olsmod.sum

### extract model coefficients
olscoef = coef(olsmod)


#########***#########   P = 0.05 SELECTION   #########***#########

### find significant coefficients at 0.05 level
ix = which(olsmod.sum$coefficients[-1,4] < 0.05)

### refit model with significant coefficients
pvalmod.05 = lm(y ~ ., data=data.frame(X[,ix]))

### extract model coefficients
pvalcoef.05 = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(pvalmod.05)), names(olscoef))
pvalcoef.05[ix] = coef(pvalmod.05)
rownames(pvalcoef.05) = names(olscoef)


#########***#########   P = 0.15 SELECTION   #########***#########

### find significant coefficients at 0.15 level
ix = which(olsmod.sum$coefficients[-1,4] < 0.15)

### refit model with significant coefficients
pvalmod.15 = lm(y ~ ., data=data.frame(X[,ix]))

### extract model coefficients
pvalcoef.15 = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(pvalmod.15)), names(olscoef))
pvalcoef.15[ix] = coef(pvalmod.15)
rownames(pvalcoef.15) = names(olscoef)


#########***#########   STEPWISE REGRESSION (AIC)   #########***#########

### AIC selection and direction="both" by default
stepmod.aic = step(olsmod, trace=0)

### get stepwise coefficients
stepcoef.aic = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(stepmod.aic)), names(olscoef))
stepcoef.aic[ix] = coef(stepmod.aic)
rownames(stepcoef.aic) = names(olscoef)


#########***#########   STEPWISE REGRESSION (BIC)   #########***#########

### BIC selection (and direction="both" by default)
stepmod.bic = step(olsmod, trace=0, k=log(n))

### get stepwise coefficients
stepcoef.bic = as(matrix(0, length(olscoef), 1), "dgCMatrix")
ix = match(names(coef(stepmod.bic)), names(olscoef))
stepcoef.bic[ix] = coef(stepmod.bic)
rownames(stepcoef.bic) = names(olscoef)


#########***#########   RIDGE REGRESSION   #########***#########

### fit ridge regression model using GCV to select lambda
lamseq = seq(0,300,length=1000)
ridgemod = lm.ridge(y ~ ., data=data.frame(X), lambda=lamseq)

### plot the ridge trace (to ensure we found minimum)
quartz(width=8,height=4)
plot(ridgemod$lambda, ridgemod$GCV, xlab="Lambda", ylab="GCV")
lines(rep(lamseq[which.min(ridgemod$GCV)],2), range(ridgemod$GCV), lty=3)
dev.copy2pdf(file="~/Desktop/psych-penreg/ridge-gcv.pdf")

### find lambda that minimizes GCV
gcvmin = which.min(ridgemod$GCV)

### extract model coefficients
ridgecoef.min = coef(ridgemod)[gcvmin,]


#########***#########   LASSO REGRESSION   #########***#########

### create fold assignments for 10-fold CV
set.seed(1)
foldid = sample(rep(1:10, length.out=n))

### 10-fold CV to estimate lambda
cvlasso = cv.glmnet(X, y, foldid=foldid, alpha=1)

### plot results
quartz(width=8,height=4)
plot(cvlasso)
dev.copy2pdf(file="~/Desktop/psych-penreg/lasso-mse.pdf")

### get the coefficients
lassocoef.min = coef(cvlasso, s="lambda.min")
lassocoef.1se = coef(cvlasso, s="lambda.1se")


#########***#########   ELASTIC NET REGRESSION   #########***#########

### create fold assignments for 10-fold CV
set.seed(1)
foldid = sample(rep(1:10, length.out=n))

### 10-fold CV to estimate lambda and alpha
alphaseq = seq(0,1,length=21)
cvlist = vector("list",length(alphaseq))
for(k in 1:length(alphaseq)){
  cvlist[[k]] = cv.glmnet(X, y, foldid=foldid, alpha=alphaseq[k])
}

### plot alphaseq vs CV-MSE
quartz(width=8,height=8)
par(mfrow=c(2,1))
mincv = sapply(cvlist, function(x) min(x$cvm))
plot(alphaseq, mincv, xlab="Alpha", ylab="Mean-Squared Error", type="b")

### get the minimum
minid = which.min(mincv)
minid
alphaseq[minid]

### plot results for minimum
plot(cvlist[[minid]])
dev.copy2pdf(file="~/Desktop/psych-penreg/enet-mse.pdf")

### get the coefficients
enetcoef.min = coef(cvlist[[minid]], s="lambda.min")
enetcoef.1se = coef(cvlist[[minid]], s="lambda.1se")


#########***#########   TABLE OF COEFFICIENTS   #########***#########

### unpenalized coefficients (table 3)
utab = round(cbind(olscoef,pvalcoef.05,pvalcoef.15,stepcoef.aic,stepcoef.bic),3)
colnames(utab) = c("ols","p0.05","p0.15","step.aic","step.bic")
utab

### penalized coefficients (table 4)
ptab = round(cbind(ridgecoef.min,lassocoef.min,lassocoef.1se,enetcoef.min,enetcoef.1se),3)
colnames(ptab) = c("ridgecoef","lasso.min","lasso.1se","enet.min","enet.1se")
ptab


#########***#########   MSPE SIMULATION   #########***#########

nrep = 100
methods = factor(c("ols","p0.05","p0.15","step.aic","step.bic",
                   "ridge","lasso.min","lasso.1se","enet.min","enet.1se"),
                 ordered=TRUE, levels=c("ols","p0.05","p0.15","step.aic","step.bic",
                                        "ridge","lasso.min","lasso.1se","enet.min","enet.1se"))
msetab = matrix(0, nrep, length(methods))
lamseq = seq(0, 300, length=1000)
alphaseq = seq(0, 1, length=21)

set.seed(55455)
for(i in 1:nrep){
  
  # print progress
  cat("rep:",i,"\n")
  
  # create training and testing data
  testID = sample.int(n, 95L)
  ytest = y[testID]
  Xtest = X[testID,]
  ytrain = y[-testID]
  Xtrain = X[-testID,]
  
  # ols regression
  olsmod = lm(ytrain ~ ., data=data.frame(Xtrain))
  msetab[i,1] = mean( (ytest - cbind(1,Xtest) %*% coef(olsmod))^2 )
  
  # p = 0.05
  olsmod.sum = summary(olsmod)
  ix = which(olsmod.sum$coefficients[-1,4] < 0.05)
  p05mod = lm(ytrain ~ ., data=data.frame(Xtrain[,ix]))
  msetab[i,2] = mean( (ytest - cbind(1,Xtest[,ix]) %*% coef(p05mod))^2 )
  
  # p = 0.15
  ix = which(olsmod.sum$coefficients[-1,4] < 0.15)
  p15mod = lm(ytrain ~ ., data=data.frame(Xtrain[,ix]))
  msetab[i,3] = mean( (ytest - cbind(1,Xtest[,ix]) %*% coef(p15mod))^2 )
  
  # stepwise regression (aic)
  stepmod = step(olsmod, trace=0)
  ix = match(names(stepmod$coefficients),names(olsmod$coefficients))
  msetab[i,4] = mean( (ytest - cbind(1,Xtest)[,ix] %*% coef(stepmod))^2 )
  
  # stepwise regression (bic)
  stepmod = step(olsmod, trace=0, k=log(length(ytrain)))
  ix = match(names(stepmod$coefficients),names(olsmod$coefficients))
  msetab[i,5] = mean( (ytest - cbind(1,Xtest)[,ix] %*% coef(stepmod))^2 )
  
  # ridge regression
  ridgemod = lm.ridge(ytrain ~ ., data=data.frame(Xtrain), lambda=lamseq)
  gcvmin = which.min(ridgemod$GCV)
  msetab[i,6] = mean( (ytest - cbind(1,Xtest) %*% coef(ridgemod)[gcvmin,])^2 )
  
  # get folds for lasso and elastic net
  foldid = sample(rep(1:10, length.out=length(ytrain)))
  
  # lasso regression
  cvlasso = cv.glmnet(Xtrain, ytrain, foldid=foldid, alpha=1)
  msetab[i,7] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlasso, s="lambda.min"))^2 )
  msetab[i,8] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlasso, s="lambda.1se"))^2 )
  
  # elastic net regression
  cvlist = vector("list",length(alphaseq))
  for(k in 1:length(alphaseq)){
    cvlist[[k]] = cv.glmnet(Xtrain, ytrain, foldid=foldid, alpha=alphaseq[k])
  }
  minid = which.min(sapply(cvlist, function(x) min(x$cvm)))
  msetab[i,9] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlist[[minid]], s="lambda.min"))^2 )
  msetab[i,10] = mean( (ytest - cbind(1,Xtest) %*% coef(cvlist[[minid]], s="lambda.1se"))^2 )
  
}

### box plots of MSPE
library(RColorBrewer)
quartz(width=12, height=5)
Methods = rep(methods, each=nrep)
MSE = c(msetab)
colors = brewer.pal(10, "Set3")
boxplot(MSE ~ Methods, col=colors, xlab="Methods", ylab="Mean-Squared Error", ylim=c(6,12))
for(j in c(7,9,11)) lines(c(0,11), c(j,j), lty=3)
dev.copy2pdf(file="~/Desktop/psych-penreg/box-mse.pdf")

### mean MSPE
meanmse = apply(msetab,2,mean)
names(meanmse) = methods
meanmse

### percent best MSPE
best = apply(msetab, 1, which.min)
prctbest = summary(factor(best))
names(prctbest) = methods
prctbest

### MSPE for first rep
colnames(msetab) = levels(methods)
msetab[1,]
