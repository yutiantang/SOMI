#Aim: SOMI lecture March--Random Forest
#Author: Yutian T. Thompson
#Date: 03/11/22
#Name: random_forest.r

# ---- load-sources -------------------------------------------------------------------



# ---- load-packages -----------------------------------------------------------
library(rpart)
library(rpart.plot)
library(clValid) #cross validation
library(randomForest) #ml
library(AUC)
library(VSURF)
library(dplyr)
library(kernlab) #for dataset



# ---- data --------------------------------------------------------------------
data("spam", package = "kernlab")
set.seed(9146301)


levels(spam$type) <- c("ok", "spam")
yTable <- table(spam$type)
indApp <- c(sample(1:yTable[2], yTable[2]/2),
              sample((yTable[2] + 1):nrow(spam), yTable[1]/2))
spamApp <- spam[indApp, ]
spamTest <- spam[-indApp, ]



# ---- bagging_RI -----------------------------------------------------------------


bagging <- randomForest(type~ ., data = spamApp, mtry = ncol(spamApp) - 1)
bagging


errTestBagging <- mean(predict(bagging, spamTest) != spamTest$type)
errEmpBagging  <- mean(predict(bagging, spamApp) != spamApp$type)

#find out mtry number
nbvars <- 1:(ncol(spamApp) - 1)
oobsMtry <- sapply(nbvars, function(nbv) {
  RF <- randomForest(type ~ ., spamApp, ntree = 250, mtry = nbv)
  return(RF$err.rate[RF$ntree, "OOB"])
})

oob_table <- as.data.frame(cbind(nbvars, oobsMtry))
plot(oob_table)

mean(replicate(n = 25, randomForest(type~ ., spamApp,
                                    ntree = 250)$err.rate[250, "OOB"]))
#0.05591304

#set the mtry=8
bagStump <- randomForest(type ~ ., spamApp, ntree = 100,
                         mtry = 8, maxnodes = 2)

bagStumpbestvar <- table(bagStump$forest$bestvar[1, ])
names(bagStumpbestvar) <- colnames(spamApp)[
  as.numeric(names(bagStumpbestvar))]
sort(bagStumpbestvar, decreasing = TRUE)





#check about the variable importance;
RFDefImp <- randomForest(type ~ ., data = spamApp,ntree = 500,
                         mtry = 8, maxnodes = 2,
                         importance = TRUE)
varImpPlot(RFDefImp, type = 1, scale = FALSE, cex = 0.8,
           main = "Variable importance")


#use different way to see the Importance variables.
RFStumpImp <- randomForest(type ~., spamApp, maxnodes = 2,  mtry = 8, importance = TRUE)
varImpPlot(RFStumpImp, type = 1, scale = FALSE, cex = 0.8,
           main = "Variable importance")


# ---- model_selection ---------------------------------------------------------
set.seed(923321, kind = "L'Ecuyer-CMRG")
vsurfSpam <- VSURF(type ~ ., spamApp, parallel = TRUE, clusterType = "FORK") #This going to run more than a hour.

#step 1
vsurfSpam$varselect.thres #check about what are the variables that have been selected above the threshold.

#step 2
#for interpretation
vsurfspam_inter <- VSURF_interp(type ~ ., spamApp, vars=vsurfSpam$varselect.interp)



summary(vsurfSpam)
plot(vsurfSpam)
colnames(spamApp[vsurfSpam$varselect.interp])

colnames(spamApp[vsurfSpam$varselect.pred])

#use a different data
# data("toys")
# set.seed(3101318)
# vsurfToys <- VSURF(toys$x, toys$y, mtry = 100)






# abc_example -------------------------------------------------------------

data <- ds_im_complete_class
index <- round(nrow(data)*0.2,digits=0)
test.indices <- sample(1:nrow(data), index)

ds_train <- data[-test.indices,] %>% select( -id, -all_of(outcomes))
ds_test  <- data[test.indices,] %>% select( -id, -all_of(outcomes))


abc_bagging <- randomForest(fsiq_cat~ ., data = ds_train, mtry = ncol(ds_train) - 1)
abc_bagging

nbvars <- 1:(ncol(ds_train) - 1)
oobsMtry <- sapply(nbvars, function(nbv) {
  RF <- randomForest(fsiq_cat ~ ., ds_train, ntree = 500, mtry = nbv)
  return(RF$err.rate[RF$ntree, "OOB"])
})

oob_table <- as.data.frame(cbind(nbvars, oobsMtry))
plot(oob_table)



abc_bagStump <- randomForest(fsiq_cat ~ ., ds_train, ntree = 250,
                         mtry = ncol(ds_train) - 1, maxnodes = 2, importance = TRUE)
varImpPlot(abc_bagStump)



