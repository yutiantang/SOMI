#SOMI Jan 2025
#Yutian Thompson
#Null hypothesis significance test


library(ggplot2)
library(stats)
library(dplyr)


factorial(24)/(factorial(7)*factorial(17))*0.5^7*0.5^17
fun_prob <- function(N, z){round(factorial(N)/(factorial(z)*factorial(N-z))*0.5^z*0.5^(N-z), 6)}
hypo_popu <- data.frame(
    `p(y)` = c(0.5, 0.5),
     y = c("tall", "head")
  ) %>%
  dplyr::rename(`p(y)`=p.y.)


#----1. given the fixed N-------------------------
z<- c(seq(0, 24))
res1_fix <- matrix(NA, nrow=0, ncol=2) %>%
  as.data.frame() %>%
  dplyr::rename(
    `p(z/N)` = V1,
    z=V2)

res2_fix <- matrix(NA, nrow=0, ncol=2) %>%
  as.data.frame() %>%
  dplyr::rename(
    `p` = V1,
    z=V2)

#calculate the density of sampling distribution
for (z in 0:24){

  sampling_distribution0 <- fun_prob(N=24, z) %>%
    as.data.frame() %>%
    dplyr::rename(`p(z/N)` = ".") %>%
    dplyr::mutate(
      z = z
    ) %>%
    rbind(res1_fix)

  res1_fix <- sampling_distribution0
}

#calculate the distribution function
for (z in 0:24){

  sampling_distribution0 <- round(pbinom(z, size=24, prob = 0.5), 6 )%>%
    as.data.frame() %>%
    dplyr::rename(`p` = ".") %>%
    dplyr::mutate(
      z = z
    ) %>%
    rbind(res2_fix)

  res2_fix <- sampling_distribution0
}

res1_fix %>%
ggplot(aes(x = z, y = `p(z/N)`))+
  geom_bar(stat="identity", position = "dodge", alpha = 0.6, fill = "#003c30") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
        axis.line = element_line(), legend.position = "none")



#print(res1_fix)
print(res2_fix)
hypo_popu %>%
  ggplot(aes(x = y, y = `p(y)`))+
  geom_bar(stat="identity", position = "dodge", alpha = 0.6, fill = "#003c30") +
  scale_y_continuous(limits = c(0, 1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
        axis.line = element_line(), legend.position = "none")


# ---- 2. given the fixed z-----------------------------

N<-c(seq(0, 24))
res3_fix <- matrix(NA, nrow=0, ncol=2) %>%
  as.data.frame() %>%
  dplyr::rename(
    `p(z/N)` = V1,
    z=V2)

res4_fix <- matrix(NA, nrow=0, ncol=2) %>%
  as.data.frame() %>%
  dplyr::rename(
    `p` = V1,
    z=V2)

#calculate the density of sampling distribution
for (N in 0:24){
  sampling_distribution0 <- round(dnbinom(7, size=N, prob = 0.5), 6 )%>%
    #fun_prob2(N, z=7) %>%
    as.data.frame() %>%
    dplyr::rename(`p(z/N)` = ".") %>%
    dplyr::mutate(
      N = N
    ) %>%
    rbind(res3_fix)
  
  res3_fix <- sampling_distribution0
}

res3_fix_use <- res3_fix %>% 
  dplyr::mutate(
    `z/N` = round(7/N, 2)
  ) %>% 
  dplyr::filter(N != 0)

#calculate the distribution function
for (N in 0:24){
  sampling_distribution0 <- round(pnbinom(7, size=N, prob = 0.5), 6 )%>%
    as.data.frame() %>%
    dplyr::rename(`p` = ".") %>%
    dplyr::mutate(
      N = N
    ) %>%
    rbind(res4_fix)
  
  res4_fix <- sampling_distribution0
}

res4_fix_use <- res4_fix %>% 
  dplyr::mutate(
    `z/N` = round(7/N, 2)
  ) %>% 
  dplyr::filter(N != 0)


res3_fix_use %>%
  ggplot(aes(x = `z/N`, y = `p(z/N)`))+
  geom_bar(stat="identity", position = "dodge", alpha = 0.6, fill = "#003c30", width = 0.15) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
        axis.line = element_line(), legend.position = "none")



print(res3_fix_use)
print(res4_fix_use)


# ---- normal-distribution -----------------------------------------------------
#sampling distribution of mean difference
nor_popu1 <- rnorm(n=500, mean=35, sd=5)
nor_popu2 <- rnorm(n=500, mean=35.1, sd=5) 

res5_norm_diff <- data.frame(nor_popu1, nor_popu2) %>% 
  dplyr::mutate(
    mean_diff = nor_popu1-nor_popu2
  )

res5_norm_diff2 <- res5_norm_diff %>% 
  dplyr::select(population1 = nor_popu1, population2=nor_popu2) %>% 
  tidyr::pivot_longer(cols = c("population1", "population2"))

res5_norm_diff2 %>%
  ggplot(aes(x=value, colour=name, fill=name))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,  binwidth=0.5)+
  geom_density(alpha=0.6)+
  scale_color_manual(values=c("#003c30", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#003c30", "#E69F00", "#56B4E9"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
        axis.line = element_line(), legend.position = "none")+
  theme_bw()


res5_norm_diff %>% 
  ggplot(aes(x=mean_diff))+
  geom_histogram( stat = "bin",
                  position = "stack", colour="white", fill="#003c30", binwidth=0.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
        axis.line = element_line(), legend.position = "none")+
  theme_bw()


sample_size <-100000

sample_norm_diff <- as.data.frame(replicate(sample_size, sample(res5_norm_diff$mean_diff, size=100))) 

# aa<- as.matrix(sample_norm_diff)
# kernel_est <- stats::density(aa)
# plot(kernel_est)



sample_norm_diff1 <- sample_norm_diff %>% 
  dplyr::mutate_all(as.numeric) %>% 
  dplyr::summarise(
    across(V1:paste0("V", sample_size), ~ mean(.x, na.rm = TRUE)),
  ) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::select(`mean of difference` = V1)

sample_norm_diff2 <- sample_norm_diff %>% 
  dplyr::mutate_all(as.numeric) %>% 
  dplyr::summarise(
    across(V1:paste0("V", sample_size), ~sd(.x, na.rm = TRUE))
  ) %>% 
  t() %>% 
  as.data.frame()%>% 
  dplyr::select(`sd of difference` = V1)

sample_norm_diff_use <- cbind(sample_norm_diff1, sample_norm_diff2) %>% 
  dplyr::mutate(
    density = rnorm(sample_size, mean = `mean of difference`, sd=`sd of difference`),
  ) 

# sample_norm_diff_use %>% 
#   ggplot(aes(density))+
#   geom_density(alpha = 0.6) 
#   geom_bar(stat="identity", position = "dodge", alpha = 0.6, fill = "#003c30", width = 0.15) +
#   theme_bw()+
#   theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
#         axis.line = element_line(), legend.position = "none")



  
sample_norm_diff_use %>%
ggplot(aes(x=`mean of difference`))+
  geom_histogram( stat = "bin",
                  position = "stack", colour="white", fill="#003c30", binwidth=0.1)+
  
  #geom_histogram(aes(y=..density..), colour="white", fill="#003c30", binwidth=0.1)+
  #geom_density(alpha=.2, fill="#FF6666") +
  theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
        axis.line = element_line(), legend.position = "none")+
  theme_bw()

