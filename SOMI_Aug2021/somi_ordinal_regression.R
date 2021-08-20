#------------------------------------------------------------------------------
# Author: Yutian T. Thompson
# Date: 2021-08-18
# Filename: somi_ordinal_regression.R
#
#
#

#------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

#----------load-sources--------------------------------------------------------------------

setwd('C:/Users/ythompso/Documents/GitHub/silovsky-navigator-1')
source(file="./analysis/navigator_functions.R")
source(file = "./analysis/navigator_analysis_survey_items.R")
# ---- load-packages -----------------------------------------------------------
library(Hmisc)
library(readxl)
library(miechv3)
library(readxl)
library(table1)
library(corrplot)
library(Hmisc)
library(stats)
library(lares) #devtools::install_github("laresbernardo/lares")
library(mice)
library(miceadds)
require(dplyr, quietly = TRUE)
library(ordinal) #cumulative link model
require(readr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(openintro, quietly = TRUE)
require(purrr, quietly = TRUE)
require(data.table, quietly = TRUE)
require(fastDummies, quietly = TRUE)
#require(purrr, quietly = T)
library(GLMMadaptive)
library(ggpubr)

# ---- declare-globals ---------------------------------------------------------

data_input <- "S:/CCAN/CCANResEval/PSB Research/Navigator Project/Data/unpublic-data/Analysis use data/"
palette_group <-c(
  "A" = "#7a0177",
  "B" = "#1a9850",
  "C" = "#fc8d59"
)


# ---- load-data-------------------------------------------------------------------


ds_mids <- read_rds(file=paste0(data_input, "ds_mids_siteLevel.rds"))
ds_aug_somi <- read_rds(file=paste0(data_input, "ds_full_data_use_siteLevel.rds"))


# ---- tweak-data --------------------------------------------------------------

ds_example <- ds_aug_somi %>%
  #dplyr::filter(wave == 2) %>%
  dplyr::select(site_id, wave, group, screen_use_count, screen_use_percent,screen_use_percent_ordinal,
                mh_screening_percent, mh_screening_percent_ordinal) %>%
  dplyr::mutate(
    screen_use_percent_ordinal1 = as.factor(screen_use_percent_ordinal+1L),
    screen_use_percent_ordinal2 = as.factor(dplyr::case_when(screen_use_percent==0~1L,
                                                   screen_use_percent>0 & screen_use_percent<=20~2L,
                                                   screen_use_percent>20 & screen_use_percent<=80~3L,
                                                   screen_use_percent>80~4L,
                                                   TRUE~NA_integer_)),
    mh_screening_percent_ordinal1 = as.factor(mh_screening_percent_ordinal+1L),
    mh_screening_percent_ordinal2 = as.factor(dplyr::case_when(
      mh_screening_percent==0~1L,
      mh_screening_percent>0 & mh_screening_percent<=20~2L,
      mh_screening_percent>20 & mh_screening_percent<=80~3L,
      mh_screening_percent>80~4L,
      TRUE~NA_integer_
  )),
  group = relevel(group, ref="C"),
  wave1 = dplyr::recode(wave, `1` = -1, `2`=0, `3`=0),
  wave2 = dplyr::recode(wave, `1` = 0, `2`=0, `3`=1),)




# ---- descriptive -------------------------------------------------------------
distribution_plot <- function(data, y, DV){
  data %>%
    ggplot(aes(y = {{y}}))+
    geom_histogram(position = "stack")+
    theme_bw()+
    labs(title=NULL, x=NULL, y=DV)
}


distribution_plot <- function(data, y, DV){
  data %>%
    ggplot(aes(y = {{y}}))+
    geom_histogram(position = "stack")+
    theme_bw()+
    labs(title=NULL, x=NULL, y=DV)
}

#screen use
D1<-descriptive_plot(data=ds_example, y=screen_use_percent, DV="screen use percent")

his1<-distribution_plot(ds_example, screen_use_percent, DV="screen use")
his2<-distribution_plot(ds_example, screen_use_percent_ordinal, DV="screen use")
his3<-distribution_plot(ds_example, as.integer(screen_use_percent_ordinal2), DV="screen use")




# ---- manage-imputation -------------------------------------------------------
ds_imp <- complete(ds_mids,action = "long",include = TRUE) %>%  #orginal
  dplyr::mutate(
    screen_use_percent_ordinal1 = as.factor(screen_use_percent_ordinal+1L),
    #screen_use_percent_cat = dplyr::if_else(screen_use_percent<=40, F, T),
    screen_use_percent_ordinal2 = as.factor(dplyr::case_when(screen_use_percent==0~1L,
                                                  screen_use_percent>0 & screen_use_percent<=20~2L,
                                                  screen_use_percent>20 & screen_use_percent<=80~3L,
                                                  screen_use_percent>80~4L,
                                                  TRUE~NA_integer_)),
    mh_screening_percent_ordinal1 = as.factor(mh_screening_percent_ordinal+1L),
    mh_screening_percent_ordinal2 = as.factor(dplyr::case_when(
      mh_screening_percent==0~1L,
      mh_screening_percent>0 & mh_screening_percent<=20~2L,
      mh_screening_percent>20 & mh_screening_percent<=80~3L,
      mh_screening_percent>80~4L,
      TRUE~NA_integer_
    )),
    # wave1 = dplyr::recode(wave, `1` = -1, `2`=0, `3`=0),
    # wave2 = dplyr::recode(wave, `1` = 0, `2`=0, `3`=1),
    group = relevel(group, ref="C") ,

    # referal_services_not_provide_percent_cate_3a = dplyr::if_else(referal_services_not_provide_percent3a==0, 0L, 1L),
    # referal_services_not_provide_percent_cate_3b = dplyr::if_else(referal_services_not_provide_percent3b==0, 0L, 1L),
    # referal_services_no_available_percent_cate = dplyr::if_else(referal_services_no_available_percent==0, 0L, 1L),
    #
    # mh_ref_indicate_no_available_percent_cate = dplyr::if_else(mh_ref_indicate_no_available_percent==0, 0L, 1L),

  )

ds_mids <-  as.mids(ds_imp)



ds_data_list <- miceadds::mids2datlist(ds_mids)




# ---- analysis ----------------------------------------------------------------
# model1a <- "screen_use_percent_ordinal2~group+wave"
# fit1a<-ordinal::clm(model1a, data=ds_example)
# summary(fit1a)

#test
nominal_test(fit1a)
scale_test(fit1a)


#use lmer
model1a <- "screen_use_percent~group*wave1 +group*wave2+ (1 |site_id)"
fit1a <- with(ds_data_list, exp = lme4::lmer(formula(model1a)))
res1a <- summary(miceadds::lmer_pool(fit1a)) %>% dplyr::tibble()
res1a <-miceadds::lmer_pool(fit1a)


# fit1a_2 <- ordinal::clmm("screen_use_percent_ordinal2~group+wave + (1|site_id)", data=ds_example,  threshold = "equidistant")
# summary(fit1a_2)

fit1b_1 = with(ds_data_list, exp = clmm("screen_use_percent_ordinal2 ~ wave1+wave2 + group + wave1:group + wave2:group+(1|site_id)"))
# fit1b_2 = with(ds_data_list, exp = clmm("screen_use_percent_ordinal2 ~ wave1+wave2 + group + wave1:group + wave2:group+(1|site_id)"),
#              threshold = "equidistant")

fit_check <- clmm("screen_use_percent_ordinal2 ~ wave1+wave2 + group + wave1:group + wave2:group+(1|site_id)",
                 link = "probit", data=ds_example)



res1b <- summary(pool(fit1b_1)) %>% dplyr::tibble()
# res1b_s<- res1b %>% dplyr::tibble() %>%
#   dplyr::mutate_all(., as.numeric)
result1b_1 <- pool(fit1b_1)
result1b_2 <- pool(fit1b_2)

#
# fit1c <- with(ds_data_list, exp = clmm("mh_screening_percent_ordinal1 ~ wave + group + wave:group + (1|site_id)",
#                                        threshold = "equidistant"))
#
# res1c <- summary(pool(fit1c)) %>% dplyr::tibble()

# fit1c_1 <- with(ds_data_list, exp = clmm2(screen_use_percent_ordinal2 ~ wave1 * group+wave2 * group,
#                                           nominal= ~ wave1, random = as.factor(site_id),
#                                           link = "probit", Hess = TRUE))

fit1c_1 <- clmm2(screen_use_percent_ordinal2 ~ wave1*group+wave2*group,
                 nominal= ~wave1+wave2, random = as.factor(site_id),
                 link = "probit", data=ds_example, Hess = TRUE)

fit1c_1 <- clmm2(screen_use_percent_ordinal2 ~ group,
                 nominal= ~wave1+wave2, random = as.factor(site_id),
                 link = "probit", data=ds_example, Hess = TRUE)

summary(fit1c_1)

fit1c_1$coefficients
fit1c_1$Theta
fit1c_1$beta




ds_example1<-ds_imp %>%
  dplyr::mutate(site_id= as.integer(site_id),
                wave = as.numeric(wave),
                screen_use_percent_ordinal2= as.integer(screen_use_percent_ordinal2)
  ) %>%
  dplyr::filter(`.imp`==1)

fit1c_2 <- repolr(screen_use_percent_ordinal2 ~ group, data=ds_example1,subjects = "site_id", times = c(1:3),
                  categories = 4,
                  corr.mod = "uniform")

mod.0 <- repolr(HHSpain~Sex*Time, data=HHSpain, categories=4, subjects="Patient",
                times=c(1,2,5), corr.mod="uniform", alpha=0.5)

model_control <-
clmm.control(method = "nlminb", trace = 0,
             maxIter = 50, gradTol = 1e-4, maxLineIter = 50, useMatrix = FALSE,
             innerCtrl = c("warnOnly", "noWarn", "giveError"),
             checkRanef = c("warn", "error", "message"))

