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
require(psfmi, quietly = TRUE)

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
  labs(title = NULL, x="measure", y="count")



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
  dplyr::filter(rep=="wave_1"
  ) %>%
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
    missing = sprintf("%4.3f", 1-n/94)
  ) %>% 
  dplyr::ungroup()

psych::describe.by(ds_use, group = "rep") 
 
descriptive2<-ds_use %>%
  dplyr::select(CATS, rep) %>% 
  dplyr::group_by(rep) %>% 
  dplyr::summarise(
    mean    = mean(CATS, na.rm=TRUE),
    sd      = sd(CATS, na.rm = TRUE),
    #missing = sprintf("%4.3f", 1-n/94)
  ) %>% 
  dplyr::ungroup()
  
  
descriptive3<-ds_use %>%
  dplyr::filter(rep=="wave_1"
  ) %>%
  dplyr::mutate(
    Gender = as.integer(Gender)
  ) %>% 
  dplyr::summarise(
    male = sum(Gender[Gender==1L], na.rm=T),
    sample= dplyr::n(),
    female = sample - male
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

fit_dose <- with(ds_data_list, exp=glm(formula(dose ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total)))
res_dose1<- summary(pool(fit_dose))



#dichotomize dose
fit_dose2 <- with(ds_data_list, exp=glm(formula(dose_cate ~ Age )), family="binomial")
res_dose2 <- summary(pool(fit_dose2))

fit_dose3 <- with(ds_data_list, exp=glm(formula(dose_cate ~ Age + Gender)), family="binomial")
res_dose3 <- summary(pool(fit_dose3))

fit_dose4 <- with(ds_data_list, exp=glm(formula(dose_cate ~ Age + Gender + cats_baseline)), family="binomial")
res_dose4 <- summary(pool(fit_dose4))

fit_dose2 <- with(ds_data_list, exp=glm(formula(dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total)), family="binomial")
res_dose2 <- summary(pool(fit_dose2))

#to get the R square
ds_imp2<-ds_imp1 %>% 
  dplyr::filter(.imp != 0L)

ds_imp3<-ds_imp1 %>% 
  dplyr::filter(.imp == 0L)

fit_dose3<-D1_logistic(data=ds_imp2, nimp=10, impvar=".imp",
            fm=dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total,
            names.var=list("Age", "Gender", "cats_baseline", "Exp_CATS_Total", "psc17_total"))

mivalext_lr(data.val=ds_imp2, nimp=10, impvar=".imp", Outcome="dose_cate",
            predictors=c("Age", "Gender", "cats_baseline", "Exp_CATS_Total", "psc17_total"),
            lp.orig=NULL,
            #lp.orig=c(-0.006, 0.044, -0.030, 0.001, 0.014, -0.010),
            cal.plot=TRUE, plot.indiv=TRUE, val.check = FALSE)


ds_imp4 <- list("imputation"=ds_imp1)
class(ds_imp4) <- "amelia"
formula1<- dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total

fit_dose4 <- Zelig::zelig(formula1, model="logit", data = ds_imp4$imputation)
summary(fit_dose4)

fit_dose4 <- with(ds_data_list, exp=zelig(formula(dose_cate ~ Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total, model = "logit", data = swiss, cite = FALSE)))

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


#fit1<-with(ds_data_list, exp = lmer(formula(CATS ~ time +(time||ID)))
#          )

fit1<-with(ds_data_list, exp = lmer(formula(CATS ~ time +(1|ID)))
)

fit2<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender +  Exp_CATS_Total + psc17_total+(1|ID))),
         )

#fit2b<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age*time + Gender + dose+  Exp_CATS_Total + psc17_total+(time||ID))),
#)

fit3<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID))),
          )

fit4<-with(ds_data_list, exp = lmer(formula(CATS ~ time + Age*time + Gender + cats_baseline+ Exp_CATS_Total + psc17_total+(1|ID))),
           )

fit5 <- with(ds_data_list, exp = lmer(formula(CATS ~ time + cats_baseline+Age*time + Gender + Exp_CATS_Total + psc17_total+time *cats_baseline +(1|ID))),
)

#res1a <- summary(lmer_pool(fit1_fix))
#res2b <- summary(lmer_pool(fit2b))


res1 <- summary(lmer_pool(fit1))
res2 <- summary(lmer_pool(fit2))
res3 <- summary(lmer_pool(fit3))
res4 <- summary(lmer_pool(fit4))
res5 <- summary(lmer_pool(fit5))
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

modlist1 <- lmerModList(data=ds_data_list, model1)
modlist2 <- lmerModList(data=ds_data_list, model2)
modlist3 <- lmerModList(data=ds_data_list, model3)
modlist4 <- lmerModList(data=ds_data_list, model4)
modlist5 <- lmerModList(data=ds_data_list, model5)

summary(modlist1)
summary(modlist2)
summary(modlist3)
summary(modlist4)
summary(modlist5)




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