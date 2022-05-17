#SOMI May 2022
#Author: Yutian T. Thompson
#Topic: covariate adaptive randomization
#Purpose:


library(randomizr) #another randomization package
library(carat) #a good tool for randomization
library(Minirand) # a good tool for randomization
library(stringdist,quietly=TRUE) # needed for approximate string matching of variable names
library(excel.link)
library(cem)
library(BiocManager) #this one is special, please see https://bioconductor.org/install/
library(randPack)
library(sp)
require(dplyr, quietly = TRUE)
require(lubridate, quietly= TRUE)
require(utils, quietly = TRUE)
require(tidyr, quietly = TRUE)
library(readr, quietly = TRUE)
require(openxlsx, quietly = TRUE)
require(zoo, quietly = TRUE)
require(MatchIt, quietly = TRUE)
require(lessR, quietly = TRUE)



#-----------load-data-------------------------------------------------------------------
setwd('S:/CCAN/CCANResEval/PSB Research/Navigator Project/Analysis')


ds_randomization            <- readr::read_rds(file = "./data_for_randomization.rds")
ds_list                     <- read.xlsx("S:/CCAN/CCANResEval/PSB Research/Navigator Project/Year 1 Mixed Method Survey/Original data/Updated Centers Accepted and Opted In to E3 Training for Family Engagement as of 11_22_2019.xlsx",
                                         colNames = TRUE)



# ---- tweak-data --------------------------------------------------------------
#practice randomization strategy
ds_sample <- ds_randomization %>%
  dplyr::select(UniqueID, screen_tool_final, region_final) %>%
  #sample_n( 96) %>%
  dplyr::mutate(UniqueID = as.integer(UniqueID)) %>%
  dplyr::right_join(ds_list, by="UniqueID") %>%
  dplyr::select(screen_tool_final, region_final) %>%
  dplyr::mutate(
    id= 1:dim(.)[1]
  )



ds_sample %>%
  dplyr::group_by(screen_tool_final, region_final) %>%
  dplyr::summarise(
    N=dplyr::n_distinct(id),
    ) %>%
  dplyr::ungroup() %>%
  dplyr::tibble()


#A simple randomisation
set.seed(12345)
index <- round(length(x)*0.5,digits=1)
test.indices <- sample(1:length(x), index)
sr<-randomizr::complete_ra(20, prob = 0.5)
sum(sr)


#practice Pocock-Simon Minimization

trt1 <- res
#Display the number of randomized subjects at covariate factors
balance1 <- randbalance(trt1, covmat, ntrt, trtseq)

ps <- ds_sample %>%
  dplyr::select(-id) %>%
PocSimMIN(., weight = NULL)

ps1_assign <- cbind(ds_sample, ps$assignments) %>%
  dplyr::rename(
    group = `ps$assignments`
  )

ps1_assign %>%
  dplyr::group_by(group, screen_tool_final, region_final) %>%
  dplyr::summarise(
    N=dplyr::n_distinct(id),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(., group) %>%
  flextable()


#way II:
ds_sample2 <- ds_sample %>%
  dplyr::select(-id)
assignments <- rep(NA, dim(ds_sample)[1]) #create an empty vector
group_name <- c(1,2,3) #cannot be the character vector

assignments[1] <- sample(group_name, 1, replace = TRUE, prob = c(1/3, 1/3, 1/3))
for (j in 2:dim(ds_sample)[1]){
  assignments[j] <- Minirand(covmat=ds_sample2, j, covwt=c(1/2, 1/2), ratio=c(1,1,1),
                     ntrt=3, trtseq=group_name, method="Range", result=assignments, p = 0.9)
}

trt1 <- res
#Display the number of randomized subjects at covariate factors
balance1 <- randbalance(assignments, ds_sample2, 3, group_name)


ps2_assign <- cbind(ds_sample, assignments) %>%
  dplyr::rename(
    group = assignments
  ) %>%
  dplyr::mutate(
    group = dplyr::recode(group,"1" = "A",
                          "2" = "B",
                          "3" = "C")
  )

ps2_assign %>%
  dplyr::group_by(group, screen_tool_final, region_final) %>%
  dplyr::summarise(
    N=dplyr::n_distinct(id),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(., group) %>%
  flextable()
