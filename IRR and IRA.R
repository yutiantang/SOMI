#This is the examples and code for May2020 topic: interrater reliability and agreement
#Author: Yutian T. Thompson

#load_package-----------------------
library(multilevel)
library(psych)
library(dplyr)
library(irr)
library(ICC)


#data----------------------------
df<-data.frame(
  id = 1L:3L,
  item1 = c(1,2,6),
  item2 = c(1,3,6),
  item3 = c(2,4,7),
  item4 = c(2,4,7),
  item5 = c(3,5,8),
  item6 = c(3,5,8),
  item7 = c(4,6,9),
  item8 = c(4,6,9)
  
)

data("lq2002", package = "multilevel") #for awg 

data("diagnoses", package = "irr") #for kappa

sf <- matrix(c(9,    2,   5,    8,
               6,    1,   3,    2,
               8,    4,   6,    8,
               7,    1,   2,    6,
               10,   5,   6,    9,
               6,   2,   4,    7),ncol=4,byrow=TRUE)
colnames(sf) <- paste("J",1:4,sep="")
rownames(sf) <- paste("S",1:6,sep="")
sf  #example from Shrout and Fleiss (1979)

#syntax----------------------------

awg.out<-multilevel::awg(df$item2,df$id,range=c(1,10))
summary(awg.out)


AD.VAL<-ad.m(lq2002[,3:12],lq2002$COMPID)
AD.VAL[1:5,]
summary(AD.VAL)
summary(lq2002[,3:12],lq2002$COMPID,type="mean")


awg.out<-awg(lq2002[,3:13],lq2002$COMPID,range=c(1,5))
summary(awg.out)

#Example for single item measure
summary(ad.m(bhr2000$HRS,bhr2000$GRP))

des<- as.data.frame(psych::describe(lq2002))
 data(lq20)


 #calculate icc by package
 psych::ICC(sf)
irr::icc(sf, model = "oneway", unit = "average")
irr::icc(sf, model = "twoway", type="agreement", unit = "average")
irr::icc(sf, model = "twoway", type="consistency", unit = "average")

 
 
 
 use1<- matrix(c(9,    2,   5,    8,
                 6,    1,   3,    2,
                 8,    4,   6,    8,
                 7,    1,   2,    6,
                 10,   5,   6,    9,
                 6,   2,   4,    7),ncol=1,byrow=TRUE)
 
 #manuelly calculate ICC-->to understand it better
 use2<-matrix(c(
   9,    2,   5,    8,  6,
   6,    1,   3,    2,  3,
   8,    4,   6,    8,  6.5,
   7,    1,   2,    6,  4,
   10,   5,   6,    9,  7.5,
   6,    2,   4,    7, 4.75), ncol=5,byrow=TRUE)
 
 colnames(use2) <- c(paste("J",1:4,sep=""), "mean")
 
 use<-as.data.frame(use2) %>% 
   dplyr::mutate(
     a = (J1-mean)^2,
     b = (J2-mean)^2,
     c = (J3-mean)^2,
     d = (J4-mean)^2
   ) %>% 
   dplyr::summarise(
     a_sum = sum(a),
     b_sum = sum(b),
     c_sum = sum(c),
     d_sum = sum(d)
   )
 
 diagnoses2<-diagnoses %>% 
   dplyr::mutate(
     rater1 = recode(rater1, 
                     `1. Depression`           = 1L,
                     `2. Personality Disorder` = 2L,
                     `3. Schizophrenia`        = 3L,
                     `4. Neurosis`             = 4L,
                     `5. Other`                = 5L),
     rater2 = recode(rater2, 
                     `1. Depression`           = 1L,
                     `2. Personality Disorder` = 2L,
                     `3. Schizophrenia`        = 3L,
                     `4. Neurosis`             = 4L,
                     `5. Other`                = 5L),
     rater3 = recode(rater3, 
                     `1. Depression`           = 1L,
                     `2. Personality Disorder` = 2L,
                     `3. Schizophrenia`        = 3L,
                     `4. Neurosis`             = 4L,
                     `5. Other`                = 5L),
     rater4 = recode(rater4, 
                     `1. Depression`           = 1L,
                     `2. Personality Disorder` = 2L,
                     `3. Schizophrenia`        = 3L,
                     `4. Neurosis`             = 4L,
                     `5. Other`                = 5L),
     rater5 = recode(rater5, 
                     `1. Depression`           = 1L,
                     `2. Personality Disorder` = 2L,
                     `3. Schizophrenia`        = 3L,
                     `4. Neurosis`             = 4L,
                     `5. Other`                = 5L),
     rater6 = recode(rater6, 
                     `1. Depression`           = 1L,
                     `2. Personality Disorder` = 2L,
                     `3. Schizophrenia`        = 3L,
                     `4. Neurosis`             = 4L,
                     `5. Other`                = 5L),
   )
 
 
 
 #kappa
 head(diagnoses[, 1:3])
 #Cohen's kappa
 irr::kappa2(diagnoses[, c("rater1", "rater2")], weight = "unweighted")
 #weighted kappa
 irr::kappa2(diagnoses[, c("rater1", "rater2")], weight = "equal")
 #Fleiss kappa
 irr::kappam.fleiss(diagnoses[, 1:3])
 #Kendall
 irr::kendall(diagnoses, TRUE)
 
 #--save_data--------
 #save the example data to run in spss
 write.csv(diagnoses, file = "C:/Users/YTHOMPSO/Documents/GitHub/SOMI/SOMI_May2020/diagnoses.csv", row.names = F)
 write.csv(diagnoses2, file = "C:/Users/YTHOMPSO/Documents/GitHub/SOMI/SOMI_May2020/diagnoses2.csv", row.names = F)
 
 
 