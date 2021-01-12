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


#---load-data-----------------
#please specify your data reading path. 
#Data is from http://www.tqmp.org/RegularArticles/vol13-1/p001/
student <- read.table(file = "C:/Users/YTHOMPSO/Dropbox/Work/Teaching/2021SOMI_1/student-mat.csv", sep=";",header = T)


#---tweak-data----------------


