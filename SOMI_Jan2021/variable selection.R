#SOMI Jan 2021 lecture
#Topic: Variable Selection
#Edit Date: 12/23/2021
#Author: Yutian T. Thompson
#note: This repo is for the SOMI lecture Jan 2021

rm(list=ls(all=TRUE))
# ---- load-sources -----------------------------------------------------------------



#---load-package---------------
library(dplyr)
library(psych)
library(tidyr)
library(ggplot2)
library(MASS)
library(glmnet)
library(grDevices)
library(Matrix)
library(lmridge)

# ---- declare-globals -----------------



#---load-data-----------------
#please specify your data reading path. 
#Data is from http://www.tqmp.org/RegularArticles/vol13-1/p001/
student <- read.table(file = "C:/Users/YTHOMPSO/Dropbox/Work/Teaching/2021SOMI_1/student-mat.csv", sep=";",header = T)


#---tweak-data----------------
ds_use <- student %>% 
  dplyr::select(-G2, -G3) %>% 
  dplyr::rename(y = G1)

y <- ds_use$y
n <- length(y)

X <- model.matrix(~., data=ds_use[, 1:30])
X <- X[,-1]


#------OLS REGRESSION------------------
### fit linear regression model via OLS
olsmod <- lm(y ~ ., data=data.frame(X))

### summarize model results
olsmod.sum <- summary(olsmod)
olsmod.sum

### extract model coefficients
olscoef <- coef(olsmod)


#--------------LASSO REGRESSION-----------
### create fold assignments for 10-fold CV
set.seed(123)
foldid = sample(rep(1:10, length.out=n))

### 10-fold CV to estimate lambda
cvlasso <- cv.glmnet(X, y, type.measure = "mse", foldid=foldid, alpha=1)
#alpha: The elasticnet mixing parameter: alpha=1 is lasso regression, ==0 is ridge regression.
#nlambda: the number of lambda values in the iteration (default=100)

plot(cvlasso)

### get the coefficients
lassocoef.min <- coef(cvlasso, s="lambda.min") #the λ at which the smallest MSE is achieved
lassocoef.1se <- coef(cvlasso, s="lambda.1se") # the largest λ at which the MSE is within one standard error of the smallest MSE




vars <-lassocoef.1se %>% 
  #coef(cvlasso, lambda = cvlasso$lambda.min) %>% 
  as.matrix() %>% 
  as.data.frame() %>%
  setDT(keep.rownames = TRUE) %>% 
  rename(name = rn, coeff = `1`) %>% 
  dplyr::filter(coeff != 0)
  #dplyr::select(-'(Intercept)') %>%
select_var <-c("sex",            
               "Medu",          
               "Fedu",            
               "Mjob",
               "Fjob",
               "reason",
               "studytime",    
               "failures",        
               "schoolsup", 
               "famsup",       
               "higher",    
               "goout"   
)

#use the selected variables to do post-lasso to get the inference infor
ds_use2 <- ds_use %>% 
  dplyr::select(select_var, y)

olsmod2 <- lm(y ~ ., data=ds_use2)
summary(olsmod2)

#--------------RIDGE REGRESSION-------------------------
### fit ridge regression model using GCV to select lambda
lamseq <- seq(0,300,length=1000)
ridgemod <- lm.ridge(y ~ ., data=data.frame(X), lambda=lamseq)

### also fit the ridge using k-fold
ridgemod2 <- cv.glmnet(X, y, type.measure = "mse", foldid=foldid, alpha=0)
ridge_lambda_min2 <- ridgemod2$lambda.min #1.204
ridgecoef.min2 <- coef(ridgemod2, s="lambda.min") 

#if you really want to get the inference infor (p value), you can use the following function:
#But I do not recommend. 
ridgemod3<-linearRidge(y ~ ., data=data.frame(X))
summary(ridgemod3)

### plot the ridge trace (to ensure we found minimum)
#quartz(width=8,height=4)
plot(ridgemod$lambda, ridgemod$GCV, xlab="Lambda", ylab="GCV")
lines(rep(lamseq[which.min(ridgemod$GCV)],2), range(ridgemod$GCV), lty=3)


#plot k-fold results
plot(ridgemod2)

### find lambda that minimizes GCV
gcvmin <- which.min(ridgemod$GCV)


### extract model coefficients
ridgecoef.min <- coef(ridgemod)[gcvmin,]


#save the ridge coefficients to compare with other methods.
ridge_coeff <-ridgecoef.min %>% 
  as.data.frame() %>% 
  setDT(keep.rownames = TRUE) 


#write_csv(ridge_coeff, file = "C:/Users/YTHOMPSO/Dropbox/Work/Teaching/2021SOMI_1/example_ridge.csv")




#--------------ELASTIC NET REGRESSION------------------

### create fold assignments for 10-fold CV
set.seed(123)
foldid = sample(rep(1:10, length.out=n))

### 10-fold CV to estimate lambda and alpha
alphaseq = seq(0,1,length=21)
cvlist = vector("list",length(alphaseq))
for(k in 1:length(alphaseq)){
  cvlist[[k]] = cv.glmnet(X, y, foldid=foldid, alpha=alphaseq[k])
}

### plot alphaseq vs CV-MSE
#quartz(width=8,height=8)
#par(mfrow=c(2,1))
mincv <- sapply(cvlist, function(x) min(x$cvm))
plot(alphaseq, mincv, xlab="Alpha", ylab="Mean-Squared Error", type="b")

### get the minimum
minid <- which.min(mincv)
minid #the second the alpha is corresponding to the min mse
alphaseq[minid]

### plot results for minimum
plot(cvlist[[minid]])
#dev.copy2pdf(file="~/Desktop/psych-penreg/enet-mse.pdf")

### get the coefficients
enetcoef.min = coef(cvlist[[minid]], s="lambda.min")
enetcoef.1se = coef(cvlist[[minid]], s="lambda.1se")



# vars_en <-enetcoef.min %>% 
#   #coef(cvlasso, lambda = cvlasso$lambda.min) %>% 
#   as.matrix() %>% 
#   as.data.frame() %>%
#   setDT(keep.rownames = TRUE) %>% 
#   rename(name = rn, coeff = `1`) %>% 
#   dplyr::filter(coeff != 0)


