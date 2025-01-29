# This file is for SOMI 2022 Oct ranking data
# Author: Yutian Thompson


rm(list=ls(all=TRUE))

# ---- load-sources -----------------------------------------------------------------


# ---- load-packages ----------------------------------------------------------------
library(pmr) #for ranking data
library(PlackettLuce)


# ---- load-data ----------------------------------------------------------------
#package examples
data(big4)
data(breasttissue)

# lecture examples
item_a <- c(1, 3)
item_b <- c(2, 2)
item_c <- c(3, 1)
n <- c(2, 1)
ds_eg1 <- data.frame(item_a, item_b, item_c, n)



X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n  <- c(3,5,1,3,2,1)
ds_test <- data.frame(X1,X2,X3,n)


item_a <- c(1, 3, 1)
item_b <- c(2, 2, 2)
item_c <- c(3, 1, 3)
ds_test2 <- data.frame(item_a, item_b, item_c)


X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
ds_test4 <- data.frame(X1,X2,X3,n)

write.csv(ds_test4, file="C:/Users/YTHOMPSO/Dropbox/Work/Teaching/2022SOMI_10/ds_test4.csv", row.names = F)



triples_round_robin <- matrix(c(
  NA, 1, 0, 0,
  1, NA, 1, 0,
  0, 1, NA, 1,
  1, 1, 1, NA),
  4, 4, byrow = TRUE,
  dimnames = list(contest = c("BCD", "ACD", "ABD", "ABC"),
                  winner = c("A", "B", "C", "D"))
)


r1 <- c(1,2,3)
r2 <- c(1,3,2)
r3 <- c(2,1,3)
r4 <- c(2,3,1)
r5 <- c(3,1,2)
r6 <- c(3,2,1)
a1 <- matrix(rep(r1, 3), nrow=3, ncol=3) %>% t()
a2 <- matrix(rep(r2, 5), nrow=3, ncol=5) %>% t()
a3 <- matrix(rep(r3, 1), nrow=3, ncol=1) %>% t()
a4 <- matrix(rep(r4, 3), nrow=3, ncol=3) %>% t()
a5 <- matrix(rep(r5, 2), nrow=3, ncol=2) %>% t()
a6 <- matrix(rep(r6, 1), nrow=3, ncol=1) %>% t()

ds_test4_raw <- rbind(a1, a2, a3, a4, a5, a6)

# ---- descriptive -------------------------------------------------------------
#aggregate the individual raw data into ranking dataset
ds_test3 <- as.data.frame(pmr::rankagg(ds_test2))
#descriptive of ranking data
res_eg1 <- pmr::destat(ds_eg1)
pmr::destat(ds_test)

# the rank mean is basically a weighted mean.
# nitem <- ncol(ds_test) - 1
# meanrank[i] <- weighted.mean(ds_test[, 1], ds_test[, nitem + 1])


#visualize the ranking data
test_2d <- mdpref(ds_test3,rank.vector=T) #for some reason, not working on my computer
pmr::mdpref(ds_test3)
pmr::rankplot(ds_test, label.type=c("1", "2", "3"), circle.bg= "#80cdc1", circle.col="#c7eae5")




# ---- analysis ----------------------------------------------------------------
res_pl <- pl(ds_test4)
coef(res_pl)


ds_test4_ranked <- PlackettLuce::as.rankings(ds_test4_raw)
res_pl1 <-PlackettLuce::PlackettLuce(ds_test4_ranked)
summary(res_pl1, ref=NULL) #do not include the reference.
round(coef(res_pl1, log=F), 3)
res1 <- PlackettLuce::itempar(res_pl1) %>% as.vector()

res_pl_qv1 <- qvcalc(res_pl1)
plot(res_pl_qv1, ylab="worth (log)", main = NULL, cex = 1.5, pch=10)


# ---- class-activity ----------------------------------------------------------
#input rankings from the audience
machine_learning <- c(3,1,3,2,3,1,1)
stats_modeling <- c(2,2,2,1,2,3,2)
exp_design <-c(1,3,1,3,1,2,3)

ds_class_activity <- data.frame(machine_learning, stats_modeling, exp_design)
ds_class_activity_ranked <- PlackettLuce::as.rankings(ds_class_activity)

res_pl2 <-PlackettLuce::PlackettLuce(ds_class_activity_ranked)
summary(res_pl2, ref=NULL)
round(coef(res_pl2, log=F), 3)
res2 <- PlackettLuce::itempar(res_pl2) %>% as.vector()

res_pl_qv2 <- qvcalc(res_pl2)
plot(res_pl_qv2, ylab="worth (log)", main = NULL, cex = 1.5, pch=10)

