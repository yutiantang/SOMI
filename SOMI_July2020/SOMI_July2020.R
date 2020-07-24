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
require(effectsize, quietly = TRUE)
require(ANOVAreplication, quietly = TRUE)



#-----------load-data-------------------------------------------------------------------
#use example from example 6.10, https://www.statmodel.com/usersguide/chapter6.shtml
#variable names:  y11-y14 x1 x2 a31-a34;
example <- read.table("./ex6.10.dat", header=FALSE)
example_feingold <- read.csv(file = "./example_Feingold2009.csv")

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


#change example_feingold from wide to long format
#group = -0.5 is control, 0.5 is treatment
example3a <- example_feingold %>% 
  dplyr::select(-T4_T1) %>% 
  dplyr::rename(group = Tx) %>% 
  tidyr::gather("time", "y",-id, -group) %>% 
  dplyr::mutate(
    time = recode(time, "T1" =1L,
                        "T2" =2L,
                        "T3" =3L,
                        "T4" =4L)
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
Control1 <- rnorm(100, mean = 150, sd = 110)
Alcohol2 <- rnorm(100, mean = 100, sd = 110)
Control2 <- rnorm(100, mean = 150, sd = 110)

pre_test <- as.data.frame(cbind(Alcohol1, Control1)) %>% 
  dplyr::mutate(
    id= row_number()
  ) %>% 
  tidyr::gather(group, pre_test, 1:2) %>% 
    dplyr::mutate(
      group = dplyr::if_else(group=="Alcohol1", 0.5,-0.5))
 


ds_example2 <- as.data.frame(cbind(Alcohol1, Control1, Alcohol2, Control2)) %>% 
  dplyr::mutate(
    id = row_number()
  ) %>% 
  tidyr::gather(group, y, 1:4) %>% 
  dplyr::mutate(
    time = dplyr::case_when(group=="Alcohol1"~1L,
                            group=="Control1"~1L,
                            group=="Alcohol2"~2L,
                            group=="Control2"~2L),
    group = dplyr::if_else(group=="Alcohol1"|group=="Alcohol2", 0.5, -0.5) 
  ) %>% 
dplyr::left_join(pre_test, by=c("id", "group"))


ds_treatment <- as.data.frame(cbind(Alcohol1, Alcohol2)) %>%  
  dplyr::rename(pre_test = Alcohol1, post_test = Alcohol2) %>% 
  dplyr::mutate(group=rep(1L),
                y=Alcohol2-Alcohol1) 

ds_control<- as.data.frame(cbind(Control1, Control2)) %>% 
  dplyr::rename(pre_test = Control1, post_test = Control2) %>% 
  dplyr::mutate(group=rep(0L),
                y=Control2 - Control1) 

ds_example2a <- as.data.frame(rbind(ds_treatment, ds_control))


ds_example2b<- as.data.frame(cbind(Alcohol1, Control1, Alcohol2, Control2)) %>% 
  dplyr::mutate(
    id = row_number(),
    treatment_change = Alcohol2 - Alcohol1,
    control_change   = Control2 - Control1
  )


#example3
Alcohol1 <- rnorm(100, mean = 250, sd = 110)
Alcohol2 <- rnorm(100, mean = 100, sd = 110)
Alcohol3 <- rnorm(100, mean = 90, sd = 110)
Alcohol4 <- rnorm(100, mean = 85, sd = 110)

Control1 <- rnorm(100, mean = 150, sd = 110)
Control2 <- rnorm(100, mean = 150, sd = 110)
Control3 <- rnorm(100, mean = 148, sd = 110)
Control4 <- rnorm(100, mean = 145, sd = 110)

ds_example3 <- as.data.frame(cbind(Alcohol1,
                                   Alcohol2,
                                   Alcohol3,
                                   Alcohol4,
                                   Control1,
                                   Control2,
                                   Control3,
                                   Control4)) %>% 
  dplyr::mutate(
    id = row_number()
  ) %>% 
  tidyr::gather(group, y, 1:8) %>% 
  dplyr::mutate(
    time = dplyr::case_when(group=="Alcohol1"~1L,
                            group=="Control1"~1L,
                            
                            group=="Alcohol2"~2L,
                            group=="Control2"~2L,
                            
                            group=="Alcohol3"~3L,
                            group=="Control3"~3L,
                            
                            group=="Alcohol4"~4L,
                            group=="Control4"~4L),
    group = dplyr::case_when(group=="Alcohol1"~0.5,
                             group=="Alcohol2"~0.5,
                             group=="Alcohol3"~0.5,
                             group=="Alcohol4"~0.5,
                             TRUE~-0.5)
  )


#example4 adding sex for two independent group without time
ds_example4 <- ds_example1 %>% 
  dplyr::group_by(group) %>% 
  dplyr::mutate(
    sex = dplyr::if_else(y>=200, 1L, 0L)
  ) %>% 
  dplyr::ungroup()


#example5: three groups without time
ds_example5 <- ds_example3 <- as.data.frame(cbind(Alcohol1,
                                                  Alcohol2,
                                                  Control4)) %>% 
dplyr::rename(TA = Alcohol1,
              TB = Alcohol2,
              control = Control4) %>% 
  tidyr::gather(group, y, 1:3) %>% 
  dplyr::mutate(
    group = as.factor(group)
  )

ds_example5b <- ds_example3 <- as.data.frame(cbind(Alcohol1,
                                                  Alcohol2,
                                                  Control4)) %>% 
  dplyr::rename(TA = Alcohol1,
                TB = Alcohol2,
                control = Control4)


#----analysis--------------------------------------------------------
#example1: two independent groups
psych::describe.by(ds_example1$y, group = ds_example1$group)
res_exa1<-glm(y~group, data=ds_example1)
summary(res_exa1)

sd(ds_example1$y)

a<-var(ds_example1$y[ds_example1$group==1L])
b<-var(ds_example1$y[ds_example1$group==0L])
pool_sd<- ((a+b)/2)^0.5
113.4/pool_sd




#example2: two groups with pre-post designs
psych::describe.by(ds_example2, group = c("group", "time"))
#step1:
res_exa2<-glm(y~group, data=ds_example2)
#res_exa2<-lmer(y~group+1|id, data=ds_example2)
summary(res_exa2)
sd(resid(res_exa2)) #-->128.4534

res_exa2a<-glm(post_test~group, data=ds_example2a)
sd(resid(res_exa2a)) #--118.320

#res_exa2<-glm(y~time+group, data=ds_example2)
res_exa22<-glm(y~pre_test+group, data=ds_example2)
summary(res_exa22)
-33.36087/128.4534
-0.2597118


res_exa22a<-glm(y~time+group+time*group, data=ds_example2)
summary(res_exa22a)

-154.62/128.4534
-1.204


treatment_pre<- ds_example2 %>% dplyr::filter(group==1L) %>% dplyr::filter(time==1L)
treatment_post<- ds_example2 %>% dplyr::filter(group==1L) %>% dplyr::filter(time==2L)
control_pre <- ds_example2 %>% dplyr::filter(group==0L) %>% dplyr::filter(time==1L)
control_post <- ds_example2 %>% dplyr::filter(group==0L) %>% dplyr::filter(time==2L)

mean(treatment_pre$y)
treat_pre_sd<-sd(treatment_pre$y)
mean(treatment_post$y)

mean(control_pre$y)
control_pre_sd<-sd(control_pre$y)
mean(control_post$y)


treat_change_mean <- mean(ds_example2b$treatment_change) #-157.703
treat_change_sd <- sd(ds_example2b$treatment_change) #157.166

control_change_mean<-mean(ds_example2b$control_change) #-1.109
control_change_sd<-sd(ds_example2b$control_change)#148.420


#d <- (treat_change_mean/treat_change_sd) - (control_change_mean/control_change_sd)
d2 <- (treat_change_mean/treat_pre_sd) - (control_change_mean/control_pre_sd)




#example 3
example3b<- example3a %>% 
  dplyr::mutate(
    time2 = dplyr::case_when(time==1L~-3L,
                             time==2L~-1L,
                             time==3L~1L,
                             time==4L~3L)
  )
#res_exa3a <- lmer(y~group*time2+1|time2, data = example3b)


res_exa3a <- lmer(y~time2*group+1|group, data = example3b)


1.116590+1.144176 +0.490630+1.504155+0.004354

res_exa3b <- glm(y~group*time2, data = example3b)
res_exa3a <- glm(y~as.factor(group), data = example3b)
summary(res_exa3b)


sd(residuals.glm(res_exa3b))


sd(resid(res_exa3b))
summary(res_exa3b)

sd(example_feingold$T1[example_feingold$Tx==0.5])
sd(example_feingold$T1[example_feingold$Tx==-0.5])


sd(example3b$y[example3b$group==0.5])
sd(example3b$y[example3b$group==-0.5])
sd(example3b$y)



#example 4;
psych::describe.by(ds_example4, group = c("group", "sex"))
res_exa4 <- glm(y~group+sex+group*sex, data = ds_example4)
summary(res_exa4)


mean(ds_example4$y[ds_example4$group==1])
mean(ds_example4$y[ds_example4$group==0])
group_diff<-243.5568-130.1745
sd<-sd(resid(res_exa4))



#example 5:
psych::describe.by(ds_example5$y, group = ds_example5$group)

ds_example5$group.f = factor(ds_example5$group, labels=c("TA", "TB", "control"))

contrast_matrix <- matrix(c(1/2, -1/2, 0, 1/3, 1/3, -2/3), ncol = 2)
contrasts(ds_example5$group.f)<-contrast_matrix

res_exa5 <- glm(y~group.f, data=ds_example5)
res_exa5b <- glm(y~group, data=ds_example5)


summary(res_exa5)
summary(res_exa5b)



effectsize::sd_pooled(y~group, data = ds_example5)
sd <- ((109.32^2+113.21^2+103.37^2)/3)^0.5
sd <- sd(resid(res_exa5))
d <- ((259.55+113.72)/2-149.51)/sd
d <-90.813/sd

var(ds_example5b$TA)


#-----save------------------
write.table(ds_example1, "./example1.dat", row.names = F)
write.table(ds_example2a, "./example2a.dat", row.names = F, col.names = F)
