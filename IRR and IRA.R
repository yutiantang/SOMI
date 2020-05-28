#This is the examples and code for May2020 topic: interrater reliability and agreement
#Author: Yutian T. Thompson

#load_package-----------------------
library(multilevel)
library(psych)
library(dplyr)


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

data("lq2002")

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
 

 #kappa
 
 
 
 