#SOMI Dec 2020 lecture
#Topic: Nonequivalent group designs
#Edit Date: 12/11/2020
#Author: Yutian T. Thompson
#note: This repo only include the instrumental variable and ANCOVA

rm(list=ls(all=TRUE))
# ---- load-sources -----------------------------------------------------------------



#---load-package---------------
library(dplyr)
library(AER)
library(psych)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(parallel)
library(lavaan)
library(AER)
library(brms)
library(rstatix)
library(Hmisc)

#---load-data-----------------
data("CigarettesSW")  # a real data

#I generated
sp <- study_parameters(n1 = 11,
                       n2 = 100,
                       T_end = 10,
                       fixed_intercept = 10,
                       fixed_slope = -.5,
                       icc_pre_subject = 0.8,
                       sigma_error = 3,
                       cohend = 0)


add_confounding <- function(x, confounder, n2, beta_s, c_scale) {
  x$confounder <- x[, confounder]
  x <- x %>%
    mutate(s =  (7 - (confounder - mean(confounder))/sd(confounder) * c_scale) - hidden_confounder*0.5 + rnorm(n2, sd = 1),
           s = ifelse(s < 1, 1, s) * treatment,
           session = s + rnorm(n2, sd = 2),
           session = ifelse(session < 1, 1, session) * treatment,
           yt = yc - beta_s * s,
           y = yt * treatment + yc * (1-treatment),
           id = 1:n2)
  x
}
add_collider <- function(x, confounder, n2, beta_s, c_scale) {
  x$confounder <- x[, confounder]
  x <- x %>%
    mutate(s =  (7 - (confounder - mean(confounder))/sd(confounder) * c_scale) + rnorm(n2, sd = 1),
           s = ifelse(s < 1, 1, s) * treatment,
           session = s + rnorm(n2, sd = 0),
           session = ifelse(session < 1, 1, session) * treatment,
           cred = (session + hidden_confounder) * treatment,
           yt = yc - beta_s * s,
           y = yt * treatment + yc * (1-treatment),
           id = 1:n2)
  x
}



data_generator <- function(pp, confounder, label, beta_s, c_scale,
                           hidden_confounder_sd = 0, measure_error_sd = 0, collider = FALSE) {
  function() {
    d <- simulate_data(pp) # powerlmm::simulate_data
    dpp <- d %>%
      filter(time %in% c(0, 10),
             treatment == 0) %>%
      mutate(time = factor(time, labels = c("pre", "post")),
             treatment = rep(0:1, each = pp$n2)) %>%
      spread(time, y) %>%
      mutate(hidden_confounder = rnorm(2*pp$n2, sd = hidden_confounder_sd),
             yc = post + hidden_confounder)
    if(collider==T) {
      dpp <- add_collider(dpp, confounder = confounder, 2*pp$n2, beta_s, c_scale)
    } else {
      dpp <- add_confounding(dpp, confounder = confounder, 2*pp$n2, beta_s, c_scale)
    }
    dpp <- dpp %>%
      mutate(pre = intercept_subject + rnorm(2*pp$n2, sd = measure_error_sd),
             label = label)
    dpp
  }
}


#a fake data: generating a fake one with sample size == 100
dpp <- data_generator(update(sp, n2 = 100),
                      confounder = "intercept_subject",
                      label = "example",
                      beta_s = 1,
                      c_scale = 3,
                      hidden_confounder_sd = 1,
                      measure_error_sd = 1,
                      collider = T)()


#---tweak-data----------------
ds_descriptive <-CigarettesSW %>%
  dplyr::select(-state, -year) %>%
  psych::describe() %>%
  round(., digits = 3) %>%
  format(., scientific = F)%>%
  knitr::kable()%>%
  kableExtra::kable_styling()


ds_use <-CigarettesSW %>%
  dplyr::mutate(
    rprice = as.numeric(price/cpi),
    salestax= as.numeric((taxs - tax )/cpi)
  )

ds_use_dpp <- dpp %>%
  rename(gender = treatment) %>%
  dplyr::mutate(
    treatment = dplyr::if_else(session >1, T, F)
  )

#fit <- glm(log(packs)~year, data = ds_use)
#summary(fit)
#cor(ds_use$salestax, ds_use$price)

#Y: packs
#instrument: salestax
#T: year (1985 vs 1995)

#---descriptive---------------------
ds_dpp_descriptive <- ds_use_dpp %>%
  dplyr::select(y, pre) %>%
  psych::describe()


TabularManifest::histogram_discrete(ds_use_dpp, "treatment")
TabularManifest::histogram_discrete(ds_use_dpp, "gender")

cor(ds_use_dpp$gender, ds_use_dpp$pre, use = "everything")


res2 <- ds_use_dpp %>%
  dplyr::select(pre, y, gender, treatment) %>%
  cor(., use = "na.or.complete")
res2
cor.test(ds_use_dpp$gender, ds_use_dpp$pre)
cor.test(ds_use_dpp$gender, ds_use_dpp$y)


#---analysis-------------------------
#The following code is to fit the two-stage least square method in a wrong way
cig_s1 <- glm(year ~ salestax, data = ds_use)
lcigp_pred <- cig_s1$fitted.values

coeftest(cig_s1, vcov = vcovHC, type = "HC1")

cig_s2 <- lm(log(ds_use$packs) ~ lcigp_pred)
coeftest(cig_s2, vcov = vcovHC)



#The following code is to fit the same method in the right way
cig_ivreg <- ivreg(log(packs) ~ year | salestax, data = ds_use)
coeftest(cig_ivreg, vcov = vcovHC, type = "HC1") #change is on the se and p value.




#For the fake data
#ANCOVA
#OLS <- lm(post ~ session + pre + treatment, data = dpp)
#summary(OLS)
### Naïve OLS (tx only)
#naive <- dpp %>%
#  #filter(treatment == 1) %>%
#  lm(y ~ session + pre, data = .)
#summary(naive)

naive2 <- ds_use_dpp %>%
  lm(y ~  gender +pre+treatment, data = .)
summary(naive2)

#naive2 <- ds_use_dpp %>%
#anova_test(y ~ gender + treatment+pre)
#get_anova_table(naive2)

#if you want to check about the homogeneity assumption, need to include the interaction
naive3 <- ds_use_dpp %>%
  lm(y ~  gender +pre+treatment+pre*treatment, data = .)
summary(naive3)



IV <- ivreg(y ~ treatment + pre | gender + pre, data = ds_use_dpp)
summary(IV, diagnostics=T)



#check if instrumental variable weak. Borrow the first stage regression

iv_x_regression <-  glm(treatment ~ gender+pre, data = ds_use_dpp)
summary(iv_x_regression)


#--plot-----------
ggscatter(
  ds_use_dpp, x = "pre", y = "y",
  color = "treatment", add = "reg.line")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = treatment)
  )





#---save----------
#save the final usable data for people to practice
ds_use_dpp_save <- ds_use_dpp %>%
  dplyr::select(subject,id, gender, pre, post, treatment,session, hidden_confounder, y)

write.csv(ds_use_dpp_save, file="C:/Users/YTHOMPSO/Documents/GitHub/SOMI/SOMI_Dec2020/practice_data.csv", row.names = F)
