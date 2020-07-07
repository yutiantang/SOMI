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


# ---- declare-globals ---------------------------------------------------------



#-----------load-data-------------------------------------------------------------------
ds <- haven::read_sav("S:/CCAN/CCANResEval/Child Trauma Programs/Research/Active Projects/Retrospective Dataset/Galsky/Galsky_CATS.sav")

psc17 <- read_excel("S:/CCAN/CCANResEval/Child Trauma Programs/Research/Active Projects/Retrospective Dataset/Galsky/old/PSC17.xlsx")



# ---- tweak-data --------------------------------------------------------------

CATS <- c("CATS_Time1_Total", "CATS_Time2_Total", "CATS_Time3_Total", "CATS_Time4_Total", "CATS_Time5_Total",
          "CATS_Time6_Total", "CATS_Time7_Total", "CATS_Time8_Total", "CATS_Time9_Total", "CATS_Time10_Total")
cats_last_six <- c("CATS_Time4_Total", "CATS_Time5_Total", "CATS_Time6_Total", "CATS_Time7_Total",
                   "CATS_Time8_Total", "CATS_Time9_Total", "CATS_Time10_Total")

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


#---imputation------------
#formating the data from wide to long


#use mice package to impute
skip_imputation_vars<-c("ID", "rep", "dose", "dose_cate")
skip_imputation_vars2<-c("ID", "rep", "dose")

design_var_index <- which(names(ds_use) %in% skip_imputation_vars)
nvars <- ncol(ds_use)
pmat <- matrix(1,nvars,nvars)
diag(pmat) <- 0
pmat[design_var_index,] <- 0
pmat[,design_var_index] <- 0

design_var_index2 <- which(names(ds_use) %in% skip_imputation_vars2)
nvars2 <- ncol(ds_use2)
pmat2 <- matrix(1,nvars2,nvars2)
diag(pmat2) <- 0
pmat[design_var_index2,] <- 0
pmat[,design_var_index2] <- 0



imputed_Data <- mice(data=ds_use , m=10, maxit = 5, seed = 123456789, predictorMatrix = pmat)#, remove.collinear=F)
imputed_Data2 <- mice(data=ds_use2 , m=10, maxit = 5, seed = 123456789, predictorMatrix = pmat2)#, remove.collinear=F)

ds_imp <- mice::complete(imputed_Data, action = "long", include = TRUE)
ds_imp1 <- ds_imp %>% 
  dplyr::group_by(.imp, ID) %>% 
  dplyr::mutate(
    CATS           = as.numeric(CATS),
    cat_total      = sum(CATS, na.rm = T),
    cats_baseline  = CATS[rep=="wave_1"],
    
    time           = recode(rep, "wave_1" = 1, "wave_2"=2, "wave_3"=3, "wave_4"=4),
    delta_cats_since_baseline = cat_total - cats_baseline
)%>%
  dplyr::ungroup() %>% 
#dplyr::select(-client_row_index)%>%
  dplyr::arrange(.imp, .id) 

imputed_data_mids <-  as.mids(ds_imp1)
ds_data_list <- miceadds::mids2datlist(imputed_data_mids)
#get imputed list:
#ds_imp <- complete(imputed_Data, action = "long",include = TRUE)
#ds_data_list <- miceadds::mids2datlist(imputed_Data)
#ds_data_list2 <- miceadds::mids2datlist(imputed_Data2)



#--visualize------
descriptive<-ds_use %>%
  dplyr::select(Age, Gender, Exp_CATS_Total, dose, psc17_total, CATS) %>%
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
  )



bxp1 <- ds_use %>%
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

fit_dose <- with(ds_data_list, exp=glm(formula(dose ~ Exp_CATS_Total + psc17_total)))
res_dose1<- summary(pool(fit_dose))

#dichotomize dose
fit_dose2 <- with(ds_data_list, exp=glm(formula(dose_cate ~ Exp_CATS_Total + psc17_total)), family="binomial")
res_dose2 <- summary(pool(fit_dose2))


#---mixed_effect------------
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

fit1<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender + Exp_CATS_Total + psc17_total+(time||ID)))
          )


fit2<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age*time + Gender +  Exp_CATS_Total + psc17_total+(time||ID))),
         )


fit3<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender*time +  Exp_CATS_Total + psc17_total+(time||ID))),
          )

fit4<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender +  Exp_CATS_Total*time + psc17_total+(time||ID))),
           )

fit5<- with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender + Exp_CATS_Total + psc17_total*time+ (time||ID))),
            )

res1a <- summary(lmer_pool(fit1_fix))
res1 <- summary(lmer_pool(fit1))
res2 <- summary(lmer_pool(fit2))
res3 <- summary(lmer_pool(fit3))
res4 <- summary(lmer_pool(fit4))
res5 <- summary(lmer_pool(fit5))

pool.compare(fit1, fit0, method = "wald") #the null hypothesis is tested that the extra parameters are all zero.
pool.compare(fit2, fit1, method = "wald") #the null hypothesis is tested that the extra parameters are all zero.
pool.compare(fit3, fit2, method = "wald")

pool.compare(fit1_fix, fit1, method = "wald")
D3(fit1, fit0)



D3(fit2, fit1) #best model is fit2
D3(fit3, fit1) #sig
D3(fit4, fit1) #no sig between fit4 and fit3, therefore, no need this 
D3(fit5, fit1)



cmod <- mitools::MIextract(fit, fun = coef)
vmod <- mitools::MIextract(fit, fun = vcov)
res1 <- summary(pool(fit))
mod_table <- cbind(rownames(res1), res1)%>%dplyr::rename("Variables" = `rownames(res1)` )
mod_table2 <- mod_table%>%
  dplyr::mutate(
    results = sprintf("%0.3f", estimate),
    se = sprintf("%0.3f", std.error),
    t = sprintf("%0.3f", statistic),
    p = sprintf("%0.3f", p.value)#,
    #lwr = sprintf("%0.3f", `lo 95`),
    #upr = sprintf("%0.3f", `hi 95`)
  )%>%
  dplyr::select(Variables, results, se, t, p)%>%
  knitr::kable(format = 'html')%>%
  kableExtra::kable_styling(bootstrap_options = "condensed")


#ds_data_list2<- as.mira(ds_data_list)



#fit_anova <- with(ds_data_list, exp = aov(CATS~factor(rep)+Error(factor(ID))))
#res_anova <- summary(pool(fit_anova))
#cmod <- mitools::MIextract(fit_anova, fun = coef)
#vmod <- mitools::MIextract(fit_anova, fun = vcov)
#an2b <- miceadds::mi.anova(mi.res=ds_imp, formula="CATS~rep+ID", idata=ID)

install.packages("MAMI", repos="http://R-Forge.R-project.org")
MAMI::mami(ds_data_list2, model="gaussian", outcome = "CATS", id="ID", criterion="AIC")





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