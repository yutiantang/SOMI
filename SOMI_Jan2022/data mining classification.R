#This is repo is for SOMI Jan 2022, 
#Topic: Introduction of data mining: 

library(dplyr)
library(tidyr)
library(reshape2)
library(ISLR) 
library(ggplot2) 
library(reshape2) 
library(class)
library(caret)
library(gmodels) #crosstable
library(rpart) #decision tree
library(rattle)
library(maptree) #tree plot


ds<-readr::read_rds(file = "S:/CCAN/CCANResEval/PSB Research/Navigator Project/Data/unpublic-data/Analysis use data/ds_navigator_child_family_analysis_use_data.rds")


catogorical_var<-c(
  "screen_used",
  "ref_provided",
  "mh_ref_indicated",
  "no_referral_reason_family_refuse",             
  "no_referral_reason_family_not_support",        
  "no_referral_reason_no_need",                   
  "no_referral_reason_already_participate",       
  "no_referral_reason_other",                     
  "no_referral_reason_not_available",             
  "child_gender",
  "child_ethnicity"
)


ds_use <- ds %>% 
  dplyr::filter(redcap_event_name == "pre_child_data_arm_1") %>% 
  dplyr::mutate(
    id = 1:dplyr::n(),
    child_ethnicity = dplyr::recode(child_ethnicity, "no available"=0L,
                                    "no Hispanic"=1L, 
                                    "Hispanic"=2L,
                                    "missing"=NA_integer_),
    screen_used = dplyr::case_when(screen_used==97~0,
                                   TRUE~screen_used)
    
  )  %>% 
  dplyr::select(
    id,
    child_age,
    #child_ethnicity1,
    all_of(catogorical_var)
  ) %>% 
  na.omit() %>% 
  dplyr::mutate_at(., vars(catogorical_var), as.factor) 
  
  
  
  # purrr::map_dfc(
  #   .x = .[, -which(names(.) %in% c("id", "child_age"))],
  #   .f = ~as.factor(.x),
  #   .id = "id"
  # )
 



# ---- prepare-data ------------------------------------------------------------
set.seed(123)

#create an index to split the data: 80% training and 20% test
index <- round(nrow(ds_use)*0.2,digits=0)
#sample randomly throughout the dataset and keep the total number equal to the value of index
test.indices <- sample(1:nrow(ds_use), index)
#80% training set
ds.train<-ds_use[-test.indices,] %>% select( -id)
#20% test set
ds.test<-ds_use[test.indices,] %>% select( -id)


#Select the training set except the DV
YTrain <- ds.train$screen_used
XTrain <- ds.train %>% select(-screen_used)
# Select the test set except the DV
YTest <- ds.test$screen_used
XTest <- ds.test %>% select(-screen_used)

#define an error rate function and apply it to obtain test/training errors
calc_error_rate <- function(predicted.value, true.value){
  return(mean(true.value!=predicted.value)) 
}



#find the number of K
nfold = 10
set.seed(123)
# cut() divides the range into several intervals
folds <- seq.int(nrow(ds.train)) %>%
  cut(breaks = nfold, labels=FALSE) %>% sample


do.chunk <- function(chunkid, folddef, Xdat, Ydat, k){ 
  train = (folddef!=chunkid)# training index
  Xtr = Xdat[train,] # training set by the index
  Ytr = Ydat[train] # true label in training set
  Xvl = Xdat[!train,] # test set
  Yvl = Ydat[!train] # true label in test set
  predYtr = knn(train = Xtr, test = Xtr, cl = Ytr, k = k) # predict training labels
  predYvl = knn(train = Xtr, test = Xvl, cl = Ytr, k = k) # predict test labels
  data.frame(fold =chunkid, # k folds 
             train.error = calc_error_rate(predYtr, Ytr),#training error per fold 
             val.error = calc_error_rate(predYvl, Yvl)) # test error per fold
}
# set error.folds to save validation errors
error.folds=NULL
# create a sequence of data with an interval of 10
kvec = c(1, seq(10, 50, length.out=5))


set.seed(123)
for (j in kvec){
  tmp = lapply(1:nfold, do.chunk, # apply do.function to each fold
              folddef=folds, Xdat=XTrain, Ydat=YTrain, k=j) # required arguments
  tmp$neighbors = j # track each value of neighbors
  error.folds = rbind(error.folds, tmp) # combine the results 
}

# ds_loop_parameter <- 
#   tidyr::expand_grid(
#     kvec = c(1, seq(10, 50, length.out=5)),
#     chunk_id  = seq_len(nfold)
#   )
# 
# # ds_loop_parameter
# 
# 
# seq_len(nfold) %>% 
#   purrr::map_dfr(
#     function(chunk_id) do.chunk(chunkid = chunk_id, folddef=folds, Xdat=XTrain, Ydat=YTrain, k = 1)
#   )
# 
# l <- as.list(ds_loop_parameter)
# 
# ds_new <- 
#   purrr::map2_dfr(
#     .x = l$chunk_id,
#     .y = l$kvec,
#     .f = function(chunk_id, k) do.chunk(chunkid = chunk_id, folddef=folds, Xdat=XTrain, Ydat=YTrain, k = k)
#   ) %>% 
#   tidyr::pivot_longer(
#     cols = c("train.error", "val.error")
#   )
# 
# 
# 
# 
# for(i in seq_len(nfold) ) {
#   do.chunk(chunkid = i, folddef=folds, Xdat=XTrain, Ydat=YTrain, k = 1)
# }
# 
# temp_list <- list()
# for (row_index in seq_len(nrow(ds_loop_parameter))) {
#   d <- ds_loop_parameter[row_index, ]
#   temp_row <-
#     do.chunk(chunkid = d$chunk_id, folddef=folds, Xdat=XTrain, Ydat=YTrain, k = d$kvec) %>% 
#     dplyr::mutate(
#       k        = d$kvec,
#     )
#   
#   temp_list[[row_index]] <- temp_row
# }
# 
# temp_list %>% 
#   purrr::map_dfr(~.) %>% 
#   tidyr::pivot_longer(
#     cols = c("train.error", "val.error")
#   )
# 
# #melt() in the package reshape2 melts wide-format data into long-format data
# #c("train.error", "val.error")
# pattern <- "^V(\\d{1,2})"




errors1 <- error.folds %>% 
as.data.frame() %>% 
  unnest(.)# %>% 
  # tidyr::pivot_longer(
  #   -neighbors, 
  #   values_drop_na = TRUE,
  #   #names_pattern = pattern,
  #   names_to = "variable", 
  #   values_to = "value"
  # )
 
#c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")

for (i in 1:9) {
 assign(paste0("fold", i), errors1 %>% dplyr::select(paste0("fold", i), neighbors) %>% dplyr::rename(fold=paste0("fold", i)))
  assign(paste0("train.error", i),  errors1 %>% dplyr::select(paste0("train.error", i))%>% dplyr::rename(train.error=paste0("train.error", i)))
  assign(paste0("val.error", i), errors1 %>% dplyr::select(paste0("val.error", i))%>% dplyr::rename(val.error=paste0("val.error", i)))
  
  
}
ds0 <- errors1 %>% dplyr::select(fold, neighbors, train.error, val.error)
ds1<-cbind(fold1, train.error1, val.error1)
ds2<-cbind(fold2, train.error2, val.error2)
ds3<-cbind(fold3, train.error3, val.error3)
ds4<-cbind(fold4, train.error4, val.error4)
ds5<-cbind(fold5, train.error5, val.error5)
ds6<-cbind(fold6, train.error6, val.error6)
ds7<-cbind(fold7, train.error7, val.error7)
ds8<-cbind(fold8, train.error8, val.error8)
ds9<-cbind(fold9, train.error9, val.error9)
ds <- rbind(ds0, ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9) %>% 
  tidyr::pivot_longer(c("train.error", "val.error"),
                      names_to = "variable", 
                      values_to = "error")



# errors <- error.folds %>% 
#   as.data.frame() %>%  
#   tidyr::pivot_longer(
#   -neighbors, 
#   values_drop_na = TRUE,
#   names_pattern = pattern,
#   names_to = "variable", 
#   values_to = "value"
# ) %>% 
#   dplyr::group_by(neighbors, variable) %>% 
#   tidyr::unnest(value) %>% 
#   dplyr::ungroup()
  


val.error.means <- ds %>%
  filter(variable == "val.error" ) %>% 
  group_by(neighbors, fold) %>%
  summarise(ave_error = mean(error)) %>% 
  ungroup() %>% 
  filter(ave_error==min(ave_error))
numneighbor = max(val.error.means$neighbors)
numneighbor

pred.YTtrain<- as.data.frame(pred.YTtrain)

# ---- algorithm_KNN ---------------------------------------------------------------
getK <- train(XTrain, YTrain, method = "knn", preProcess = c("center","scale"))

pred.YTtrain <- knn(train = XTrain, test = XTest, cl = YTrain, k=7)
class_comparison <- data.frame(pred.YTtrain, data.frame(YTest)) %>% 
  dplyr::rename(Predictedy=pred.YTtrain, Observedy=YTest)
 
head(class_comparison)

CrossTable(x = class_comparison$Observedy, y = class_comparison$Predictedy, 
           prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

#evaluation
knnPredict <- predict(getK, newdata = XTest) 
confusionMatrix(class_comparison$Predictedy, class_comparison$Observedy)



#way 3
#training error
set.seed(456)
pred.YTtrain <- knn(train=XTrain, test=XTrain, cl=YTrain, k=10)
knn_traing_error <- calc_error_rate(predicted.value=pred.YTtrain, true.value=YTrain)
knn_traing_error

#0.3359253

#test error
set.seed(789)
pred.YTest = knn(train=XTrain, test=XTest, cl=YTrain, k=7)
knn_test_error <- calc_error_rate(predicted.value=pred.YTest, true.value=YTest)
knn_test_error
#0.4720497

conf.matrix = table(predicted=pred.YTest, true=YTest)

# Test accuracy rate
sum(diag(conf.matrix)/sum(conf.matrix))

# Test error rate
1 - sum(diag(conf.matrix)/sum(conf.matrix))


# ---- algorithm_CART ----------------------------------------------------------

ds_use2<- ds_use %>% 
  dplyr::select(-id) %>% 
  dplyr::mutate(
    screen_used = dplyr::case_when(screen_used=='0'~"NO",screen_used=="1"~"Yes",screen_used=="97"~"No-available"),
    ref_provided =dplyr::case_when(ref_provided=='0'~"NO",ref_provided=="1"~"Yes",ref_provided=="97"~"No-available"),
    mh_ref_indicated = dplyr::case_when(mh_ref_indicated=='0'~"NO",mh_ref_indicated=="1"~"Yes",mh_ref_indicated=="97"~"No-available"),
    no_referral_reason_family_refuse      = dplyr::case_when(no_referral_reason_family_refuse      =='0'~"NO",no_referral_reason_family_refuse      =="1"~"Yes",no_referral_reason_family_refuse      =="97"~"No-available"),
    no_referral_reason_family_not_support = dplyr::case_when(no_referral_reason_family_not_support =='0'~"NO",no_referral_reason_family_not_support =="1"~"Yes",no_referral_reason_family_not_support =="97"~"No-available"),
    no_referral_reason_no_need            = dplyr::case_when(no_referral_reason_no_need            =='0'~"NO",no_referral_reason_no_need            =="1"~"Yes",no_referral_reason_no_need            =="97"~"No-available"),
    no_referral_reason_already_participate= dplyr::case_when(no_referral_reason_already_participate=='0'~"NO",no_referral_reason_already_participate=="1"~"Yes",no_referral_reason_already_participate=="97"~"No-available"),
    no_referral_reason_other              = dplyr::case_when(no_referral_reason_other              =='0'~"NO",no_referral_reason_other              =="1"~"Yes",no_referral_reason_other              =="97"~"No-available"),
    no_referral_reason_not_available      = dplyr::case_when(no_referral_reason_not_available      =='0'~"NO",no_referral_reason_not_available      =="1"~"Yes",no_referral_reason_not_available      =="97"~"No-available"),
    child_gender =dplyr::case_when(child_gender=='0'~"Male",child_gender=="1"~"Female"),
    child_ethnicity = dplyr::case_when(child_ethnicity=='0'~"no available",child_ethnicity=="1"~"no Hispanic", child_ethnicity=="2"~"Hispanic" ),
  ) %>% 
  na.omit() %>% 
  dplyr::mutate_at(., vars(catogorical_var), as.factor) 



ds_use.train<-ds_use2[-test.indices,]
ds_use.test<-ds_use2[test.indices,] 

ds_use.train2<-ds_use[-test.indices,] %>% dplyr::select(-id)
ds_use.test2<-ds_use[test.indices,] %>% dplyr::select(-id)


# Build the model
set.seed(123)
model1 <- rpart(screen_used ~., data = ds_use.train2, method = "class")


fancyRpartPlot(model1, cex=0.75)


# Make predictions on the test data
predicted.classes <- model1 %>% 
  predict(ds_use.test2, type = "class")
#head(predicted.classes)

predict_target <- as.data.frame(predicted.classes)
predict_target2 <- predict_target %>% cbind(., ds_use.test2$screen_used) 
names(predict_target2) <-c("pre", "obs")

#mean(predicted.classes == ds_use.test$screen_used)
#confusionMatrix(predict_target$predicted.classes, as.factor(ds_use.test$screen_used))
confusionMatrix(predict_target2$pre, predict_target2$obs)


roc_result <- pROC::roc(as.numeric(predict_target2$pre), as.numeric(predict_target2$obs))
plot(roc_result)#terriable 

table1 <- matrix(c(107, 5, 43, 6), ncol = 2)
chisq.test(table1)




#prune the tree
set.seed(123)
model2 <- train(
  screen_used ~., data = ds_use.train2, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(model2)



model2$bestTune
fancyRpartPlot(model2$finalModel, cex=0.5)


model1b <- prune(model1,0.067)

#get the prediction values for the model2 or model1b
predicted.classes <- model1b %>% 
  predict(ds_use.test, type = "class")
head(predicted.classes)

predict_target <- as.data.frame(predicted.classes)

mean(predicted.classes == ds_use.test$screen_used)
confusionMatrix(predict_target$predicted.classes, as.factor(ds_use.test$screen_used))
