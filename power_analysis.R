#SOMI Oct 2023
#Author: Yutian T. Thompson

library(pwrss) #a package for power analysis
library(dplyr)



#t-test
plot_t <- power.t.test(ncp = 1.96, df = 100, alpha = 0.05,
             alternative = "equivalent", plot = TRUE)

#z-test
plot_z <- power.z.test(ncp = 1.96, alpha = 0.05,
                       alternative = "not equal", plot = TRUE)


#compare two means1
design1a <-pwrss.t.2means(mu1 = 30, mu2 = 28, sd1 = 12, sd2 = 12, kappa = 1,
               n2 = 50, alpha = 0.05,
               alternative = "not equal")
plot(design1a)


design1b <-pwrss.t.2means(mu1 = 30, mu2 = 25, sd1 = 12, sd2 = 12, kappa = 1,
                          n2 = 50, alpha = 0.05,
                          alternative = "not equal")
plot(design1b)


design1c <-pwrss.t.2means(mu1 = 30, mu2 = 23, sd1 = 12, sd2 = 12, kappa = 1,
                          n2 = 50, alpha = 0.05,
                          alternative = "not equal")
plot(design1c)





#compare two means2
design2a <-pwrss.t.2means(mu1 = 30, mu2 = 25, sd1 = 12, sd2 = 12, kappa = 1,
                          n2 = 50, alpha = 0.5,
                          alternative = "not equal")
plot(design2a)


design2b <-pwrss.t.2means(mu1 = 30, mu2 = 25, sd1 = 12, sd2 = 12, kappa = 1,
                          n2 = 50, alpha = 0.2,
                          alternative = "not equal")
plot(design2b)


design2c <-pwrss.t.2means(mu1 = 30, mu2 = 25, sd1 = 12, sd2 = 12, kappa = 1,
                          n2 = 50, alpha = 0.05,
                          alternative = "not equal")
plot(design2c)





design3a <- pwrss.t.2means(mu1 = 30, mu2 = 28,  kappa = 1, sd1=12, power=0.8, alpha = 0.05, alternative = "not equal")
plot(design3a)

design3b <- pwrss.t.2means(mu1 = 30, mu2 = 28,  kappa = 1, sd1=12,  power=0.6, alpha = 0.05, alternative = "not equal")
plot(design3b)

design3c <- pwrss.t.2means(mu1 = 30, mu2 = 28,  kappa = 1, sd1=12,  power=0.4, alpha = 0.05, alternative = "not equal")
plot(design3c)



