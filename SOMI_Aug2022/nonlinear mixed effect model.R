#SOMI Aug 2022: Nonlinear mixed effect model
#Author: Yutian T. Thompson
#Date: 08/19/2022


rm(list=ls(all=TRUE))



#----------load-sources--------------------------------------------------------------------


#----------load-packages--------------------------------------------------------------------
library(flextable)
library(lmerTest)
library(lme4)
library(dplyr)
library(table1)
require(tidyr, quietly = TRUE)
library(readr, quietly = TRUE)
require(lessR, quietly = T)
library(nlme)
library(stats)

# ---- declare-globals ---------------------------------------------------------


#-----------load-data-------------------------------------------------------------------
hght_long <- read.table('C:/Users/YTHOMPSO/Dropbox/Clare/Book/Growth Modeling/Data/bgs_height_long.dat', na.string='.')


# ---- tweak-data --------------------------------------------------------------
names(hght_long)<-c('id', 'age', 'hght')

table1::table1(~age+hght, data=hght_long)


hght_long <- hght_long %>%
  dplyr::mutate(
    c_age   = age/12,
    cd_age  = (age-18)/12,
    qua_age = cd_age^2
  )




# ---- analysis ----------------------------------------------------------------
#0. prepare for the starting values
#way 1: use nls with only fixed effect
hght.start <- stats::nls(hght~b_1i+b_2i*((age-18)/12)+b_3i*((age-18)/12)^2,
           data=hght_long,
           na.action=na.omit
)
nlscoef <- coef(hght.start)
#summary(hght.start)


#way 2: use lmer with both fixed and random
#ps: here we should put qua_age into random, but the model is not converged
hght.start2<-lmer(hght~1+cd_age+qua_age+(1+cd_age|id), data=hght_long)
lmercoef <- fixef(hght.start2)
names(lmercoef) <- c("b_1i", "b_2i", "b_3i")


hght.quad.nlme0 <- nlme(hght~1+cd_age+qua_age,
                        data=hght_long,
                        fixed=~1+cd_age+qua_age,
                        random~1+cd_age+qua_age,
                        groups=~id,
                        start= list(fixed= nlscoef),
                        # c(30, 10, -3),
                        na.action=na.omit)




#1. Quadratic Growth Model
hght.quad.nlme <- nlme(hght~b_1i+b_2i*((age-18)/12)+b_3i*((age-18)/12)^2,
                       data=hght_long,
                       fixed=b_1i+b_2i+b_3i~1,
                       random=b_1i+b_2i+b_3i~1,
                       groups=~id,
                       start= list(fixed= lmercoef),
                        # c(30, 10, -3),
                       na.action=na.omit)
summary(hght.quad.nlme)


#2. spline growth model
hght.spline.nlme <- nlme(hght~b_1i+b_2i*(pmin(0,age-9))+b_3i*(pmax(0,age-9)),
                         data=hght_long,
                         fixed=b_1i+b_2i+b_3i~1,
                         random=b_1i+b_2i+b_3i~1,
                         groups=~id,
                         start=c(60, 10, 6),
                         na.action=na.omit)

summary(hght.spline.nlme)





#3. Jenss-Bayley Growth Model
#way 1: use nlme
hght.jb.nlme <- nlme(hght~b_1i+b_2i*(c_age)+b_3i*(exp(gamma*(c_age))-1),
                     data=hght_long,
                     fixed=b_1i+b_2i+b_3i+gamma~1,
                     random=b_1i+b_2i+b_3i~1,
                     groups=~id,
                     start=c(50, 10, -18, -2),
                     na.action=na.omit)
summary(hght.jb.nlme)



#way two: use lmer package --> much complicated.
jb_model <- function(age, b_1i, b_2i, b_3i, gamma){
    b_1i+b_2i*(age/12)+b_3i*(exp(gamma*(age/12))-1)}

jb_modelg <- deriv((hght~b_1i+b_2i*(age/12)+b_3i*(exp(gamma*(age/12))-1)),
                   namevec = c("b_1i", "b_2i", "b_3i", "gamma"),
                   function.arg = jb_model)


startsite <- c(b_1i = 50, b_2i = 10, b_3i = -18,
               gamma = -2)

jb.math.nlmer <- nlmer(hght ~ jb_modelg(age, b_1i,
                                        b_2i, b_3i, gamma) ~ (b_1i+b_2i+b_3i|id),
                       data = hght_long,
                       start = startsite)
summary(jb.math.nlmer)


#4. Spline growth models with estimated knot points
hght.spline.nlme <- nlme(hght~b_1i+b_2i*(pmin(0,age-gamma))+b_3i*(pmax(0,age-gamma)),
                         data=hght_long,
                         fixed=b_1i+b_2i+b_3i+gamma~1,
                         random=b_1i+b_2i+b_3i~1,
                         groups=~id,
                         start=c(60, 5, 2, 8),
                         na.action=na.omit)
summary(hght.spline.nlme)



#5. Jenss-Bayley Growth Model with free estimate b4

hght.jb.nlme2 <- nlme(hght~b_1i+b_2i*(c_age)+b_3i*(exp(b_4i*(c_age))-1),
                     data=hght_long,
                     fixed=b_1i+b_2i+b_3i+b_4i~1,
                     random=b_1i+b_2i+b_3i+b_4i~1,
                     groups=~id,
                     start=c(50, 10, -18, -2),
                     na.action=na.omit)

summary(hght.jb.nlme2)


#6. Spline Growth Models with Variability in the Knot Points

hght.jb.nlme.fo <- nlme(hght~(beta_1+d_1i)+(beta_2+d_2i)*(c_age)+(beta_3+d_3i)*(exp(beta_4*(c_age))-1)+d_4i*(beta_3*(c_age)*exp(beta_4*(c_age))),
                        data=hght_long,
                        fixed=beta_1+beta_2+beta_3+beta_4~1,
                        random=d_1i+d_2i+d_3i+d_4i~1,
                        groups=~id,
                        start=c(50, 10, -18, -2),
                        na.action=na.omit)

summary(hght.jb.nlme.fo)
