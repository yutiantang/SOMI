#Ashley Galsky project
#Author: Yutian T. Thompson
#Purpose: CATS study.


rm(list=ls(all=TRUE))

#----------load-sources--------------------------------------------------------------------

base::source(file="C:/Users/ythompso/Documents/GitHub/silovsky-navigator-1/analysis/navigator_functions.R")

#----------load-packages--------------------------------------------------------------------
library(stringdist,quietly=TRUE) # needed for approximate string matching of variable names
library(excel.link)
library(cem)
library(lme4)
library(nlme)
library(mice)

#library(BiocManager) #this one is special, please see https://bioconductor.org/install/
#library(randPack)
library(sp)
library(readxl)
library(ggpubr)
library(data.table)
require(dplyr, quietly = TRUE)
require(lubridate, quietly= TRUE)
require(utils, quietly = TRUE)
require(tidyr, quietly = TRUE)
library(readr, quietly = TRUE)
require(openxlsx, quietly = TRUE)
require(zoo, quietly = TRUE)
require(MatchIt, quietly = TRUE)
require(lessR, quietly = TRUE)
require(haven, quietly = TRUE)
require(stats, quietly = TRUE)
require(rstatix, quietly = TRUE)
require(mitools, quietly = TRUE)
require(sjstats, quietly = TRUE)
require(miceadds, quietly = TRUE)
require(mitml, quietly = TRUE)
library(MachineShop)
library(Amelia)
library(merTools)
library(Zelig)
require(psfmi, quietly = TRUE)
require(ggalluvial, quietly = TRUE)
require(GLMMadaptive, quietly = TRUE) #devtools::install_github("drizopoulos/GLMMadaptive")

# ---- declare-globals ---------------------------------------------------------



#-----------load-data-------------------------------------------------------------------
ds_old <- haven::read_sav("S:/CCAN/CCANResEval/Child Trauma Programs/Research/Active Projects/Retrospective Dataset/Galsky/Galsky_CATS.sav")
ds <- haven::read_sav("S:/CCAN/CCANResEval/Child Trauma Programs/Research/Active Projects/Retrospective Dataset/Galsky/Galsky soper working data set spss version 04 12 2020_b.sav")

#psc17 has multiple sheet, please use the third one not the first two!!!
psc17_wrong <- read_excel("S:/CCAN/CCANResEval/Child Trauma Programs/Research/Active Projects/Retrospective Dataset/Galsky/old/PSC17.xlsx")

psc17 <- read_excel("S:/CCAN/CCANResEval/Child Trauma Programs/Research/Active Projects/Retrospective Dataset/Galsky/old/PSC17.xlsx",
                    sheet = "Sheet5")


# ---- tweak-data --------------------------------------------------------------

CATS <- c("CATS_Time1_Total", "CATS_Time2_Total", "CATS_Time3_Total", "CATS_Time4_Total", "CATS_Time5_Total",
          "CATS_Time6_Total", "CATS_Time7_Total", "CATS_Time8_Total", "CATS_Time9_Total", "CATS_Time10_Total")
cats_last_six <- c("CATS_Time4_Total", "CATS_Time5_Total", "CATS_Time6_Total", "CATS_Time7_Total",
                   "CATS_Time8_Total", "CATS_Time9_Total", "CATS_Time10_Total")

psc17<-psc17 %>%
  dplyr::rename(
    record_id = ID,
    psc17_total = PSC17,
  )

use<- ds %>%
  dplyr::select(ID, Age, Gender, Exp_CATS_Total, Date_Referred, Intake_Date,
                CATS_Time1_Total, CATS_Time2_Total, CATS_Time3_Total, CATS_Time4_Total, CATS_Time5_Total,
                CATS_Time6_Total, CATS_Time7_Total, CATS_Time8_Total, CATS_Time9_Total, CATS_Time10_Total
                ) %>%
  dplyr::mutate(
    Date_Referred     = as.Date(strptime(Date_Referred, "%Y-%m-%d")),
    Intake_Date       = as.Date(strptime(Intake_Date, "%Y-%m-%d")),
    date_diff         = abs(as.numeric(Intake_Date-Date_Referred)), #calculate the date diff between intake and referr
    dose              = apply(.[CATS], 1, FUN = function(x)length(x[!is.na(x)])),
    baseline          = apply(.[CATS], 1, FUN = function(x)first(x[!is.na(x)])),
    last_measurement1  = as.numeric(apply(.[cats_last_six], 1, FUN = function(x)last(x[!is.na(x)]))),
    last_measurement2 = as.numeric(apply(.[CATS], 1, FUN = function(x)last(x[!is.na(x)]))),
    row_mean          = apply(.[cats_last_six], 1, FUN = function(x)mean(x, na.rm=T)),
    row_mean          = dplyr::recode(row_mean, "NaN" = NA_real_),
    dose_cate         = dplyr::if_else(dose>3, 1, 0)
  ) %>%
  dplyr::rename(
    wave_1 = CATS_Time1_Total,
    wave_2 = CATS_Time2_Total,
    wave_3 = CATS_Time3_Total,
    wave_4 = last_measurement1,
    last_measure = last_measurement2
  ) %>%
  dplyr::left_join(psc17, by=c("ID"="record_id"))


ds_use <- use %>%
  dplyr::select(ID, Age, Gender, Exp_CATS_Total, dose, dose_cate, psc17_total, wave_1, wave_2, wave_3, wave_4) %>%
  tidyr::gather("rep", "CATS", -ID, -Age, -Gender, -Exp_CATS_Total, -dose, -dose_cate, -psc17_total) %>%
  dplyr::mutate(
    rep =  as.factor(rep)
  )

ds_use2 <- use %>%
  dplyr::select(ID, Age, Gender, Exp_CATS_Total, dose, psc17_total, wave_1, wave_2, wave_3, wave_4) %>%
  tidyr::gather("rep", "CATS", -ID, -Age, -Gender, -Exp_CATS_Total, -dose, -psc17_total) %>%
  dplyr::mutate(
    rep =  as.factor(rep)
  )

#ds_use3 is made for not imputing psc-17, because this variabe brings us too much trouble
ds_use3 <- use %>%
  dplyr::mutate(
    psc17_total = as.numeric(psc17_total),
    psc17_total = as.integer(sprintf("%2.0f", dplyr::if_else(is.na(psc17_total), mean(psc17_total, na.rm=T), psc17_total) )),
    cats_baseline = wave_1
    ) %>%
  dplyr::select(ID, Age, Gender, Exp_CATS_Total, dose, dose_cate, psc17_total, cats_baseline, wave_1, wave_2, wave_3, wave_4) %>%
  tidyr::gather("rep", "CATS", -ID, -Age, -Gender, -Exp_CATS_Total, -dose, -dose_cate, -psc17_total,-cats_baseline) %>%
  dplyr::mutate(
    rep =  as.factor(rep)
  )


ds_use0 <- ds %>%
  dplyr::select(
    ID, CATS
  ) %>%
  tidyr::gather("rep", "CATS", -ID) %>%
  dplyr::group_by(rep) %>%
  dplyr::summarise(
    total_sample = dplyr::n(),
    non_missing = sum(!is.na(CATS)),
    missing     = sum(is.na(CATS)),
    missing_percent = paste0(sprintf("%4.2f", missing/total_sample*100), "%"),
    nonmissing_percent = paste0(sprintf("%4.2f", non_missing/total_sample*100), "%")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    rep = dplyr::case_when(rep=="CATS_Time1_Total"~"1",
                           rep=="CATS_Time2_Total"~"2",
                           rep=="CATS_Time3_Total"~"3",
                           rep=="CATS_Time4_Total"~"4",
                           rep=="CATS_Time5_Total"~"5",
                           rep=="CATS_Time6_Total"~"6",
                           rep=="CATS_Time7_Total"~"7",
                           rep=="CATS_Time8_Total"~"8",
                           rep=="CATS_Time9_Total"~"9",
                           rep=="CATS_Time10_Total"~"10"),
    rep         = factor(rep, levels =c(
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10"
    )))


#---plots-------------------------
ggplot(ds_use0, aes(x = rep, y = non_missing, label=nonmissing_percent))+
  geom_bar(stat = "identity", position='dodge', alpha=.4, color="#c1a16f", fill="#e07587") +
  geom_text(aes(label = nonmissing_percent),  size = 3,  color="#22353c", vjust=-0.5)+
  #scale_color_manual(values = sek_col, guide = "none") +
  #scale_fill_manual(values =  sek_col) +
  theme_bw()+
  #theme(axis.title.x=element_blank(),
  #      axis.text.x=element_blank(),
  #      axis.ticks.x=element_blank())+
  labs(title = NULL, x="measure time", y="count")

ggplot(ds_use0, aes(x = rep, y = missing, label=missing_percent))+
  geom_bar(stat = "identity", position='dodge', alpha=.4, color="#c1a16f", fill="#e07587") +
  geom_text(aes(label = missing_percent),  size = 3,  color="#22353c", vjust=-0.5)+
  #scale_color_manual(values = sek_col, guide = "none") +
  #scale_fill_manual(values =  sek_col) +
  theme_bw()+
  #theme(axis.title.x=element_blank(),
  #      axis.text.x=element_blank(),
  #      axis.ticks.x=element_blank())+
  labs(title = NULL, x="measure time", y="count")

#---imputation------------
#formating the data from wide to long


#use mice package to impute
# skip_imputation_vars<-c("ID", "rep", "dose", "dose_cate")
# skip_imputation_vars2<-c("ID", "rep", "dose")
#March 2021, we found the new psc-17 variable, if we included in the model --> model would be nonconverged on certain inputed data
# skip_imputation_vars3<-c("ID", "rep", "dose", "dose_cate", "psc17_total")
skip_imputation_vars3<-c("ID", "rep")


design_var_index <- which(names(ds_use3) %in% skip_imputation_vars3)
impute_vars3 <- setdiff(names(ds_use3), skip_imputation_vars3)
impute_var_index3 <- which(names(ds_use3) %in% impute_vars3)

nvars <- ncol(ds_use3)
pmat <- matrix(1,nvars,nvars)
diag(pmat) <- 0
pmat[design_var_index,] <- 0
pmat[,design_var_index] <- 0
pmat[impute_var_index3,impute_var_index3] <- mice::quickpred(ds_use3[impute_var_index3]) #use this to minimize the loggedevents overprediction warnings


meth <- vector()
meth[impute_var_index3] = "pmm"
meth[design_var_index3] = ""

imputed_Data3 <- mice::mice( ds_use3, m=10, maxit = 5, meth = meth, seed = 1127575261, predictorMatrix = pmat)#, remove.collinear=F)
saveRDS(file="S:/CCAN/CCANResEval/Child Trauma Programs/Research/Active Projects/Retrospective Dataset/Galsky/imputed_cat_data.rds", imputed_Data3)

# design_var_index <- which(names(ds_use) %in% skip_imputation_vars)
# nvars <- ncol(ds_use)
# pmat <- matrix(1,nvars,nvars)
# diag(pmat) <- 0
# pmat[design_var_index,] <- 0
# pmat[,design_var_index] <- 0
#
# design_var_index2 <- which(names(ds_use) %in% skip_imputation_vars2)
# nvars2 <- ncol(ds_use2)
# pmat2 <- matrix(1,nvars2,nvars2)
# diag(pmat2) <- 0
# pmat[design_var_index2,] <- 0
# pmat[,design_var_index2] <- 0
#
# design_var_index3 <- which(names(ds_use3) %in% skip_imputation_vars3)
# nvars3 <- ncol(ds_use3)
# pmat3 <- matrix(1,nvars3,nvars3)
# diag(pmat3) <- 0
# pmat[design_var_index3,] <- 0
# pmat[,design_var_index3] <- 0
#
#
# imputed_Data <- mice(data=ds_use , m=10, maxit = 5, seed = 123456789, predictorMatrix = pmat)#, remove.collinear=F)
# imputed_Data2 <- mice(data=ds_use2 , m=10, maxit = 5, seed = 123456789, predictorMatrix = pmat2)#, remove.collinear=F)
# imputed_Data3 <- mice(data=ds_use3 , m=10, maxit = 5, seed = 123456789, predictorMatrix = pmat3) #this imputation is especially for psc-17 variable;
#



#the major difference of imputations based on ds_use and ds_use3 is: the baseline cat scores was calculated on the raw data
#in ds_use3 then impute; While it was calculated in imputed data for ds_imp, which bring the model converge issue;
#Therefore, on March 2021 analysis, we finally decided to use the ds_use3 as well as the corresponding ds_imp3 to do
#the further analysis. Abandon the dis_imp1, ds_use.
# ds_imp <- mice::complete(imputed_Data, action = "long", include = TRUE)
# ds_imp1 <- ds_imp %>%
#   dplyr::group_by(.imp, ID) %>%
#   dplyr::mutate(
#     CATS           = as.numeric(CATS),
#     cat_total      = sum(CATS, na.rm = T),
#     cats_baseline  = CATS[rep=="wave_1"],
#
#     time           = recode(rep, "wave_1" = 1L, "wave_2"=2L, "wave_3"=3L, "wave_4"=4L),
#     psc17_total    = as.integer(sprintf("%2.0f", psc17_total)),
#     delta_cats_since_baseline = cat_total - cats_baseline
# )%>%
#   dplyr::ungroup() %>%
# #dplyr::select(-client_row_index)%>%
#   dplyr::arrange(.imp, .id)
#
# imputed_data_mids <-  as.mids(ds_imp1)
# ds_data_list <- miceadds::mids2datlist(imputed_data_mids)
#~~~~~~~~ds_use imputation management end here




## get imputed list:
## ds_imp <- complete(imputed_Data, action = "long",include = TRUE)
## ds_data_list <- miceadds::mids2datlist(imputed_Data)
## ds_data_list2 <- miceadds::mids2datlist(imputed_Data2)


#manipulate for the ds_use3 imputed data (add on March 2021)
ds_imp3 <- mice::complete(imputed_Data3, action = "long", include = TRUE)
ds_imp3 <- ds_imp3 %>%
  dplyr::group_by(.imp, ID) %>%
  dplyr::mutate(
    CATS           = as.numeric(CATS),
    cat_total      = sum(CATS, na.rm = T),
   # cats_baseline  = CATS[rep=="wave_1"],

    time           = recode(rep, "wave_1" = 1L, "wave_2"=2L, "wave_3"=3L, "wave_4"=4L),
    delta_cats_since_baseline = cat_total - cats_baseline
  )%>%
  dplyr::ungroup() %>%
  dplyr::arrange(.imp, .id)

imputed_data_mids3 <-  as.mids(ds_imp3)
ds_data_list3 <- miceadds::mids2datlist(imputed_data_mids3)




#--visualize------
descriptive<-use %>% #choose the time invarying variables
  dplyr::select(Age, Gender, Exp_CATS_Total, dose, psc17_total) %>%
  psych::describe() %>%
  setDT(keep.rownames = "variable") %>%
  dplyr::mutate(
    mean    = sprintf("%4.3f", mean),
    sd      = sprintf("%4.3f", sd),
    median  = sprintf("%4.3f", median),
    trimmed = sprintf("%4.3f", trimmed),
    mad     = sprintf("%4.3f", mad),
    skew    = sprintf("%4.3f", skew),
    kurtosis= sprintf("%4.3f", kurtosis),
    se      = sprintf("%4.3f", se),
    missing = sprintf("%4.3f", 1-n/75)
  ) %>%
  dplyr::ungroup()

psych::describe.by(ds_use, group = "rep")

descriptive2<-ds_use3 %>%
  dplyr::select(CATS, rep) %>%
  dplyr::group_by(rep) %>%
  dplyr::summarise(
    mean    = mean(CATS, na.rm=TRUE),
    sd      = sd(CATS, na.rm = TRUE),
    #missing = sprintf("%4.3f", 1-n/75)
  ) %>%
  dplyr::ungroup()


descriptive3<-use %>%
  # dplyr::filter(rep=="wave_1"
  # ) %>%
  dplyr::mutate(
    Gender = as.integer(Gender)
  ) %>%
  dplyr::summarise(
    male = sum(Gender[Gender==1L], na.rm=T),
    sample= dplyr::n(),
    female = sample - male
  )

bxp1 <- ds_use3 %>%
  dplyr::select(ID, rep, CATS) %>%
  dplyr::rename(measurement = rep) %>%
  ggboxplot(x = "measurement", y = "CATS", add = "point")


#---preliminary_results----------
#March meeting to get the
#Gender, Age, Session_Frequency, Exp_CATS_Total
#average_dose <- mean(use$dose, na.rm = T) #4.33 times

#use simple repeated ANOVA
ds_ANOVA <- use %>%
  dplyr::select(ID, dose, baseline, last_measure) %>%
  tidyr::gather("rep", "CATS", -ID, -dose) %>%
  dplyr::mutate(
    rep =  as.factor(rep)
  )



ds_cats <- use %>%
  dplyr::select(ID, dose, wave_1, wave_2, wave_3, wave_4, Exp_CATS_Total) %>%
  tidyr::gather("rep", "CATS", -ID, -dose, -Exp_CATS_Total)


#result <- lmer(formula = CATS ~ rep + dose + (1|ID), data = ds_cats)
#summary(result)
#
#result<- nlme::lme(CATS ~ dose, random=~rep|ID, data=ds_cats, method="ML")
#summary(result)

result1<- aov(CATS~factor(rep)+Error(factor(ID)), data = ds_ANOVA)
table1<-anova_stats(result1)



  #summary(result1)
#sjstats::eta_sq(result1)

#result2 <- rstatix::anova_test(dv = CATS, wid=ID, within=rep, data=ds_ANOVA)
#summary(result2)
#sphericity<-rstatix::get_anova_table(res.anova, correction = "GG")
#anova_summary(result1)
#
#design <- factorial_design(dv = CATS, wid=ID, within=rep, data=ds_ANOVA)
#res.anova <- Anova(design$model, idata = design$idata, idesign = design$idesign, type = 3)
#
#
#my.mod <- nlme::lmer(formula = CATS ~ rep + (1|ID), data = ds_ANOVA)
#summary(my.mod)

#just use glm
result <- glm(CATS ~ rep + Age + Gender + dose+Exp_CATS_Total+psc17_total, data=ds_use)
tabl2<- summary(result)




#---outcome_dose-------

fit_dose <- with(ds_data_list, exp=glm(formula(dose ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total)))
res_dose1<- summary(pool(fit_dose))



#dichotomize dose
#fit_dose0 <- with(ds_data_list, exp=lm(formula(dose_cate ~ 1 )), family="binomial")
#res_dose0 <- summary(pool(fit_dose0))

fit_dose2 <- with(ds_data_list, exp=lm(formula(dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total)), family="binomial")
res_dose2 <- summary(pool(fit_dose2)) #corresponding to step1

fit_dose3 <- with(ds_data_list, exp=lm(formula(dose_cate ~ Age + cats_baseline+ Exp_CATS_Total + psc17_total)), family="binomial")
res_dose3 <- summary(pool(fit_dose3))#corresponding to step2

fit_dose4 <- with(ds_data_list, exp=lm(formula(dose_cate ~ Age + Exp_CATS_Total + psc17_total)), family="binomial")
res_dose4 <- summary(pool(fit_dose4)) #corresponding to step3

fit_dose5 <- with(ds_data_list, exp=lm(formula(dose_cate ~ Age + psc17_total)), family="binomial")
res_dose5 <- summary(pool(fit_dose5), conf.int = TRUE, conf.level = 0.95) #corresponding to step4

#fit_dose5b <- with(ds_data_list, exp=lm(formula(dose_cate ~ Age + psc17_total + Age * psc17_total)), family="binomial")
#res_dose5b <- summary(pool(fit_dose5b))

#fit_dose6 <- with(ds_data_list, exp=lm(formula(dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total)), family="binomial")
#res_dose6 <- summary(pool(fit_dose6))



#glm.mids(ds_data_list, )
#D1(fit_dose3, fit_dose2)
#D1(fit_dose4, fit_dose3)
#D1(fit_dose5, fit_dose4)
#D1(fit_dose6, fit_dose5)



#to get the R square
ds_imp2<-ds_imp1 %>%
  dplyr::filter(.imp != 0L)

ds_imp3<-ds_imp1 %>%
  dplyr::filter(.imp == 0L)

fit_dose_b<-D1_logistic(data=ds_imp2, nimp=10, impvar=".imp",
                        formula=dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total,
            names.var=list("Age", "Gender", "cats_baseline", "Exp_CATS_Total", "psc17_total"))

#mivalext_lr(data.val=ds_imp2, nimp=10, impvar=".imp", Outcome="dose_cate",
#            predictors=c("Age", "Gender", "cats_baseline", "Exp_CATS_Total", "psc17_total"),
#            #lp.orig=NULL,
#            lp.orig=c(-0.006, 0.044, -0.030, 0.001, 0.014, -0.010),
#            cal.plot=TRUE, plot.indiv=TRUE, val.check = FALSE)



pool_lr <- psfmi_lr(data=ds_imp2, nimp=10, impvar=".imp",
                    formula=dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total,
                    Outcome="dose_cate",
                    cat.predictors=c("Gender"),
                    #predictors=c("Age", "cats_baseline", "Exp_CATS_Total", "psc17_total"),
                    #keep.predictors = "Smoking",
                    p.crit = 0.05, method="D1", direction="BW")

pool_lr$RR_Model

pool_lr$multiparm




fit_log1 <- lm.mids(dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total, data=ds_data_list)

fit_dose2 <- as.mira(fit_dose2)
fit_dose3 <- as.mira(fit_dose3)
fit_dose4 <- as.mira(fit_dose4)
fit_dose5 <- as.mira(fit_dose5)

#the following r square is for calculating r square from imputation of logistic regression
pool.r.squared(fit_dose2)
pool.r.squared(fit_dose3)
pool.r.squared(fit_dose4)
pool.r.squared(fit_dose5)






bdfile <- system.file("extdata", "vcf1a.bdose", package = "BinaryDosage")
bdinfo <- getbdinfo(bdfiles = bdfile) #use library(BinaryDosage)
snp1 <- getsnp(bdinfo = bdinfo, 1, dosageonly = FALSE)
rsq <- BinaryDosage:::getrsq(snp1$dosage, p2 = snp1$p2)



ds_imp4 <- list("imputation"=ds_imp1)
class(ds_imp4) <- "amelia"
formula1<- dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total

fit_dose4 <- Zelig::zelig(formula1, model="logit", data = ds_imp4$imputation)
summary(fit_dose4)

fit_dose4 <- with(ds_data_list, exp=zelig(formula(dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total, model = "logit", data = swiss, cite = FALSE)))

#---mixed_effect1------------
#repeate anova
fit<-with(ds_data_list, exp = lmer(formula(CATS ~ rep + (1|ID))),
          )
res <- lmer_pool(fit, digits=3)
summary(res)

#model 1a: random intercept
fit0<-with(ds_data_list, exp = lmer(formula(CATS ~ time + (time||ID))))
res0<-lmer_pool(fit)

cmod <- mitools::MIextract(fit0, fun = coef)
vmod <- mitools::MIextract(fit0, fun = vcov)
res <- summary(res0)



mod_table <- cbind(rownames(res), res)%>%dplyr::rename("Variables" = `rownames(res)` )
mod_table1 <- mod_table%>%
  dplyr::mutate(
    results = sprintf("%0.3f", est),
    se = sprintf("%0.3f", se),
    t = sprintf("%0.3f", t),
    p = sprintf("%0.3f", p)#,
  )%>%
  dplyr::select(Variables, results, se, t, p)%>%
  knitr::kable(format = 'html')%>%
  kableExtra::kable_styling(bootstrap_options = "condensed")


#extract AIC and -2LL
#str(pool(fit))
#data.imputed <- pool(fit)
#AIC(fit)
#L_df <- mice::complete(imputed_Data,"long",include = FALSE)
#AIC1<-c()
#logLik1 <- c()
#m <- max(L_df$.imp)
#
#for(i in 1:m){
#  model.imputed1 <- glmer(formula(CATS ~ rep + (1|ID)), data = L_df[which(L_df$.imp == m),])
#  AIC1[i] <- AIC(model.imputed1)
#  logLik1[i] <- logLik(model.imputed1)
#}
#sapply(model.imputed1$analysss, AIC)
#model 1b: random slope hit the error of identification, we do not have enough observations


#model 2: include covariates: no sig


#fit1<-with(ds_data_list, exp = lmer(formula(CATS ~ time +(time||ID)))
#          )

#The following models were specified for the version before March 2021 which used the wrong psc-17 variable;
#we did not applied the random effect on the slope is main for fit5, if the random effect added, we will
#have over-fit model and hit the error of "boundary (singular) fit: see ?isSingular"
fit1<-with(ds_data_list, exp = lmer(formula(CATS ~ time +(1|ID)))
)


fit2<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender +  Exp_CATS_Total + psc17_total+(1|ID))),
         )
#try the different package:
# fit2b<-with(ds_data_list, exp = GLMMadaptive::mixed_model(formula(fixed = CATS ~ time + Age*time + Gender + dose+  Exp_CATS_Total + psc17_total,
#                                                                   random = ~1|ID), family = gaussian()),
# )

fit3<-with(ds_data_list, exp = glmer(formula(CATS ~ time + Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID),
                                             control=glmerControl(optimizer="Nelder_Mead",
                                                                  coptCtrl=list(maxfun=2e10)))),
          )


fit3<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID))),
)

fit4<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age*time + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID))),
           )


# fit5b <- with(ds_data_list, exp = lmer(formula(CATS ~ time + cats_baseline+Age*time + Gender + Exp_CATS_Total + psc17_total+time *cats_baseline +(time|ID))),
# ) #add on Oct 2020, check on march 14th 2021--> not converged

fit5 <- with(ds_data_list, exp = lmer(formula(CATS ~ time + cats_baseline+Age*time + Gender + Exp_CATS_Total + psc17_total+time *cats_baseline +(1|ID))),
)

#res1a <- summary(lmer_pool(fit1_fix))
#res2b <- summary(lmer_pool(fit2b))


res1 <- summary(lmer_pool(fit1))
res2 <- summary(lmer_pool(fit2))
res3 <- summary(lmer_pool(fit3))
res4 <- summary(lmer_pool(fit4))
res5 <- summary(lmer_pool(fit5))
#res5b <- summary(lmer_pool(fit5b))#this one has the random slope but not converged.

#res6 <- summary(lmer_pool(fit6))

#pool.compare(fit1, fit1b, method = "wald") #the null hypothesis is tested that the extra parameters are all zero.
pool.compare(fit2, fit1, method = "wald") #the null hypothesis is tested that the extra parameters are all zero.
pool.compare(fit3, fit2, method = "wald")
#pool.compare(fit2b, fit2, method = "wald")

#pool.compare(fit1_fix, fit1, method = "wald")
#D3(fit6, fit2)

#D3(fit1b, fit1) #no sig--> does not need fit2b

#D3(fit2, fit1) #best model is fit2
#D3(fit3, fit2) #sig
#D3(fit4, fit3) #no sig between fit4 and fit3, therefore, no need this
#D3(fit5, fit4)





cmod <- mitools::MIextract(fit5, fun = coef) #to extract the coefficients
vmod <- mitools::MIextract(fit5, fun = vcov)

mod_table <- cbind(rownames(res5), res5)%>%dplyr::rename("Variables" = `rownames(res5)` )
mod_table2 <- mod_table%>%
  dplyr::mutate(
    results = sprintf("%0.3f", est),
    se = sprintf("%0.3f", se),
    t = sprintf("%0.3f", t),
    p = sprintf("%0.3f", p)#,
    #lwr = sprintf("%0.3f", `lo 95`),
    #upr = sprintf("%0.3f", `hi 95`)
  )%>%
  dplyr::select(Variables, results, se, t, p)%>%
  knitr::kable(format = 'html')%>%
  kableExtra::kable_styling(bootstrap_options = "condensed")


#ds_data_list2<- as.mira(ds_data_list)
#draw plot for
ds_predictions <- jtools::make_new_data(fit5[[1]], pred = "CATS") #If you need more options to appear for variables in your newdata for predictions, add them here
preds <- sapply(fit5, predict, newdata=ds_predictions, allow.new.levels = TRUE)
phats  <- preds[1,]
vw <- (unlist(preds[2,]))^2
pred_vals <- summary(miceadds::pool_mi(phats, vw))%>%
  dplyr::select(fit = results,
                lwr = `(lower`,
                upr = `upper)`)
ds_predictions <- ds_predictions %>% dplyr::bind_cols(pred_vals) #ds_prediction to get the estmiated parameters


cmod_data <- cmod %>%
  map(function(x){as.data.frame(x$ID) %>%
      setDT(keep.rownames = TRUE)%>%
      dplyr::select(rn, `(Intercept)`, `time:cats_baseline`, time)}) %>%
  #unlist() %>%
  as.data.frame() %>%
  dplyr::mutate(
    intercept = rowMeans(.[c("X.Intercept.", "X.Intercept..1", "X.Intercept..2", "X.Intercept..3", "X.Intercept..4",
                             "X.Intercept..5", "X.Intercept..6", "X.Intercept..7", "X.Intercept..8", "X.Intercept..9")]),
    slope      = rowMeans(.[c("time", "time.1", "time.2", "time.3", "time.4",
                              "time.5", "time.6", "time.7", "time.8", "time.9")]),
    slope2     = rowMeans(.[c("time.cats_baseline", "time.cats_baseline.1","time.cats_baseline.2", "time.cats_baseline.3", "time.cats_baseline.4",
                              "time.cats_baseline.5", "time.cats_baseline.6", "time.cats_baseline.7", "time.cats_baseline.8", "time.cats_baseline.9")])
  ) %>%
  dplyr::select(rn, intercept, slope, slope2) %>%
  rename(id = rn)



palette_stage <- c("FK"="#79c8c3"  , "SAU"="#ce8a81", "FK declined"="#9d72a7")
ggplot(data=cmod_data, aes(x=CNTAC_SEQ, y=PSC.TOTAL, group=infk, fill=infk, color=infk))+
  geom_point(size=5, alpha=0.4)+
  geom_abline(intercept=fixef(lwpsct)[1], slope=fixef(lwpsct)[2], linetype="twodash", size=1.5, color="#ce8a81", alpha=0.8)+
  geom_abline(intercept=sum(fixef(lwpsct)[c(1, 3)]), slope=sum(fixef(lwpsct)[c(2, 5)]), linetype="solid", size=1.5, color="#35978f")+
  geom_abline(intercept=sum(fixef(lwpsct)[c(1, 4)]), slope=sum(fixef(lwpsct)[c(2, 6)]), linetype="dashed", size=1.5, color="#9d72a7")+
  scale_x_continuous() +
  coord_cartesian(ylim = c(0, 40)) +
  scale_color_manual(values = palette_stage) +
  scale_fill_manual(values = palette_stage, guide="none") +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(legend.position="none")+
  theme_bw() +
  theme(text=element_text(size=12,  family="sans"))+
  labs(
    title = "Pediatric Symptom Checklist (PSC)\n in repeated measurements",
    x="Wave",
    y="Pediatric Symptom Checklist Scores",
    color="Condition"
  )


install.packages("MAMI", repos="http://R-Forge.R-project.org")
MAMI::mami(ds_data_list2, model="gaussian", outcome = "CATS", id="ID", criterion="AIC")



#-----mixed_effect2---------------------
#This a a new chunck, particularly for the new right psc-17 variable, based on ds_data_list3
#Our goal is to keep the original model setting as much as possible

fit1<-with(ds_data_list3, exp = glmer(formula(CATS ~ time +(1|ID)))
)


fit2<-with(ds_data_list3, exp = glmer(formula(CATS ~ time + Age + Gender +  Exp_CATS_Total + psc17_total+(1|ID))),
)


fit3<-with(ds_data_list3, exp = glmer(formula(CATS ~ time + Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID))),
)


fit4<-with(ds_data_list3, exp = glmer(formula(CATS ~ time + Age*time + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID))),
           )

fit5 <- with(ds_data_list3, exp = glmer(formula(CATS ~ time + cats_baseline+Age*time + Gender + Exp_CATS_Total + psc17_total+time *cats_baseline +(1|ID))),
)




res1 <- summary(lmer_pool(fit1))
res2 <- summary(lmer_pool(fit2))
res3 <- summary(lmer_pool(fit3))
res4 <- summary(lmer_pool(fit4))
res5 <- summary(lmer_pool(fit5))

#get the likelihood ratio test.
mitml::testModels(fit2, fit1, method = "D3", use = "likelihood")
mitml::testModels(fit3, fit2, method = "D3", use = "likelihood")
mitml::testModels(fit4, fit3, method = "D3", use = "likelihood")
mitml::testModels(fit5, fit4, method = "D3", use = "likelihood")



#to get the AIC
#to get AIC
model1 <- "CATS ~ time +(1|ID)"
model2 <- "CATS ~ time + Age + Gender +  Exp_CATS_Total + psc17_total+(1|ID)"
model3 <- "CATS ~ time + Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID)"
model4 <- "CATS ~ time + Age*time + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID)"
model5 <- "CATS ~ time + cats_baseline+Age*time + Gender + Exp_CATS_Total + psc17_total+time *cats_baseline +(1|ID)"

modlist1 <- lmerModList(data=ds_data_list3, model1) #this function is able to pool AIC from imputed data
modlist2 <- lmerModList(data=ds_data_list3, model2)
modlist3 <- lmerModList(data=ds_data_list3, model3)
modlist4 <- lmerModList(data=ds_data_list3, model4)
modlist5 <- lmerModList(data=ds_data_list3, model5)

summary(modlist1)
summary(modlist2)
summary(modlist3)
summary(modlist4)
summary(modlist5)


#---check_assumption-----------------
fit_raw <-glmer(formula(CATS ~ rep + Age + Gender + dose + Exp_CATS_Total + psc17_total+(1|ID)),
                data = ds_use,
                REML=FALSE)
ranef(fit_raw)
#1. check linearity ---> not hold, the residual plot is not that much random with values
residual_raw <- resid(fit_raw)
Plot.Model.Linearity<-plot(resid(fit_raw), fit_raw@frame$CATS)


ds_use2 <- ds_use %>%
  dplyr::mutate(
    na_use =  apply(., 1, FUN = function(row){
      any(is.na(row))})
  ) %>%
  dplyr::filter(na_use==F)


#2. Homogeneity of variance
Model.Res<- residuals(fit_raw) #extracts the residuals and places them in a new column in our original data table
Abs.Model.Res <-abs(Model.Res) #creates a new column with the absolute value of the residuals
Model.Res2 <- Abs.Model.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
model.sdRes <- effectsize::standardize(Model.Res)
ds_use3<-cbind(ds_use2, Model.Res2, model.sdRes)
Levene.Model<- lm(Model.Res2 ~ ID, data=ds_use3) #ANOVA of the squared residuals
anova(Levene.Model) #displays the results

#since the p value is greater than 0.05, we can say that the variance of the residuals is equal
#and therefore the assumption of homoscedasticity is met Not
Plot.Model <- plot(fit_raw) #creates a fitted vs residual plot
Plot.Model

#3. residual normality distribution
qqmath(fit_raw, id=0.05)
#verall the line looks straight and therefore pretty normal and suggests that the assumption is not violated


#--double-check-outlier----
sd_residual <- as.data.frame(fit[[1]]@residuals) %>% dplyr::rename(residual = `fit[[1]]$residuals`) %>%
  dplyr::mutate(sd_residual = standardize(residual)) %>%
  setDT(., keep.rownames = TRUE) %>%
  dplyr::filter(sd_residual<=3 & sd_residual>=-3)


ds_use4<-ds_use3 %>%
  dplyr::filter(model.sdRes<=3 & model.sdRes>=-3)



#no outliers


#-----visual----------------------
baseline <- ds_use2 %>%
  dplyr::filter(rep=="wave_1") %>%
  dplyr::select(ID, CATS) %>%
  dplyr::mutate(
    cate_baseline_level = dplyr::if_else(CATS >= 28, "baseline high", "baseline low")
  ) %>%
  dplyr::select(-CATS)


ds_use_visual <- ds_use %>%
  dplyr::mutate(
    rep = dplyr::recode(rep,"wave_1" = 1,
                        "wave_2" = 2,
                        "wave_3" = 3,
                        "wave_4" = 4),

    CATS = dplyr::if_else(is.na(CATS), 0, CATS)
  ) %>%
  dplyr::left_join(baseline, by="ID") %>%
  dplyr::select(
    cate_baseline_level, rep, CATS
  )


alluvial_ts(dat=ds_use_visual, wave = 0.6, lwd = 0.1,ygap=1.5, alpha = 0.8,
            col = color)



color <-c(
  "baseline high" = "#ff0000",
  "baseline low"  = "#D3D3D3"
)
