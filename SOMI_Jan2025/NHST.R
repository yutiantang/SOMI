#SOMI Jan 2025
#Yutian Thompson
#Null hypothesis significance test


library(ggplot2)


factorial(24)/(factorial(7)*factorial(17))*0.5^7*0.5^17
fun_prob <- function(N, z){round(factorial(N)/(factorial(z)*factorial(N-z))*0.5^z*0.5^(N-z), 6)}
hypo_popu <- data.frame(
    `p(y)` = c(0.5, 0.5),
     y = c("tall", "head")
  ) %>%
  dplyr::rename(`p(y)`=p.y.)

#given the fixed N
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

