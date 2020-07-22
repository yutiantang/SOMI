#this is for SOMI presentation on July
#Author: Yutian T. Thompson
#Topic: effect size estimates

#----------load-sources--------------------------------------------------------------------

setwd('C:/Users/YTHOMPSO/Documents/GitHub/SOMI/SOMI_July2020')
#----------load-packages--------------------------------------------------------------------
library(stringdist,quietly=TRUE) 
require(dplyr, quietly = TRUE)
require(lubridate, quietly= TRUE)
require(utils, quietly = TRUE)
require(readr, quietly = TRUE)





#-----------load-data-------------------------------------------------------------------
#use example from example 6.10, https://www.statmodel.com/usersguide/chapter6.shtml
#variable names:  y11-y14 x1 x2 a31-a34;
example <- read.table("./ex6.10.dat", header=FALSE)

# ---------tweak-data --------------------------------------------------------------
example <- example %>% 
  dplyr::rename(
    y11 = V1,
    y12 = V2,
    y13 = V3,
    y14 = V4,
    x1  = V5,
    x2  = V6,
    a31 = V7,
    a32 = V8,
    a33 = V9,
    a34 = V10
  )


#---simulate---------------------------------------------------------
#example1, simulate a two independent groups without multiple time points
Alcohol <- rnorm(100, mean = 250, sd = 110)
Control <- rnorm(100, mean = 150, sd = 105)
ds_example1 <- as.data.frame(cbind(Alcohol, Control)) %>% 
  tidyr::gather(group, y) %>% 
  dplyr::mutate(
    group = dplyr::if_else(group=="Alcohol", 1L,0L)
  )


#example2, 
Alcohol1 <- rnorm(100, mean = 250, sd = 110)
Control1 <- rnorm(100, mean = 150, sd = 105)
Alcohol2 <- rnorm(100, mean = 100, sd = 107)
Control2 <- rnorm(100, mean = 150, sd = 103)

ds_example2 <- as.data.frame(cbind(Alcohol1, Control1, Alcohol2, Control2)) %>% 
  tidyr::gather(group, y) %>% 
  dplyr::mutate(
    group = dplyr::if_else(group=="Alcohol", 1L,0L)
  )



#----analysis--------------------------------------------------------
#example1:
psych::describe.by(ds_example1$y, group = ds_example1$group)
res_exa1<-glm(y~group, data=ds_example1)
summary(res_exa1)

sd(ds_example1$y)

a<-var(ds_example1$y[ds_example1$group==1L])
b<-var(ds_example1$y[ds_example1$group==0L])
pool_sd<- ((a+b)/2)^0.5
113.4/pool_sd
#-----save------------------
write.table(ds_example1, "./example1.dat", row.names = F)
