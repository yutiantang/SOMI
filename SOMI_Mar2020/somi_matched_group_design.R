#------------------------------------------------------------------------------
# Author: Yutian T. Thompson
# Date: 2020-03-24
# Filename: somi_matched_group_design.R
#
#
#

#------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

#----------load-sources--------------------------------------------------------------------


#----------load-packages--------------------------------------------------------------------
library(stringdist,quietly=TRUE) # needed for approximate string matching of variable names
library(readxl)
library(ggplot2)
library(MatchIt)
library(RItools)
library(Hmisc)
library(Matching)
require(dplyr, quietly = TRUE)


# ---- declare-globals ---------------------------------------------------------
#sciplot: Scientific Graphing Functions for Factorial Designs
#cli: Helpers for Developing Command Line Interfaces
#sbtools: USGS ScienceBase Tools
#ripa: R Image Processing and Analysis
#dams: Dams in the United States from the National Inventory of Dams (NID)
#Home / CRAN / rbounds / R/psens.R
#R/psens.R
#In rbounds: Perform Rosenbaum bounds sensitivity tests for matched and unmatched data.
#Defines functions psens
#Documented in psens
psens <- function(x, y = NULL, Gamma = 6, GammaInc = 1){

  if (class(x)!="Match") {
    trt <- x
    ctrl <- y
  }
  else if(x$est > 0){
    ctrl <-x$mdata$Y[x$mdata$Tr==0]
    trt <- x$mdata$Y[x$mdata$Tr==1]
  } else {
    ctrl <- x$mdata$Y[x$mdata$Tr==1]
    trt <- x$mdata$Y[x$mdata$Tr==0]
  }

  gamma <- seq(1, Gamma, by=GammaInc)
  m <- length(gamma)
  pvals <- matrix(NA, m, 2)
  diff <- trt - ctrl
  S <- length(diff)
  diff <- diff[diff != 0]
  ranks <- rank(abs(diff), ties.method="average")
  psi <- as.numeric(diff > 0)
  T <- sum(psi * ranks)

  for(i in 1:m) {
    p.plus <- gamma[i]/(1 + gamma[i])
    p.minus <- 1/(1+gamma[i])
    E.T.plus <- sum(ranks*p.plus)
    V.T <- sum(ranks^2 * p.plus*(1-p.plus))
    E.T.minus <- sum(ranks*p.minus)

    ## Normal Approximation
    z.plus <- (T - E.T.plus)/sqrt(V.T)
    z.minus <- (T - E.T.minus)/sqrt(V.T)
    p.val.up <- 1 - pnorm(z.plus)
    p.val.low <- 1 - pnorm(z.minus)
    pvals[i,1] <- round(p.val.low, digits=4)
    pvals[i,2] <- round(p.val.up, digits=4)
  }

  pval <- pvals[1,1]
  bounds <- data.frame(gamma, pvals)
  names(bounds) <- c("Gamma", "Lower bound", "Upper bound")

  msg <- "Rosenbaum Sensitivity Test for Wilcoxon Signed Rank P-Value \n"
  note <- "Note: Gamma is Odds of Differential Assignment To
 Treatment Due to Unobserved Factors \n"

  Obj <- list(Gamma = Gamma, GammaInc = GammaInc, pval = pval,
              msg = msg, bounds = bounds, note = note)
  class(Obj) <- c("rbounds", class(Obj))

  Obj
}



#-----------load-data-------------------------------------------------------------------
ds_use <- readr::read_csv(file = "S:/CCAN/CCANResEval/Research Training and Resources/SOMI/group-matched comparison design/ecls.csv")

# ---- tweak-data --------------------------------------------------------------
#1, descriptive information
t.test(c5r2mtsc_std~catholic, data=ds_use)


ds_ecls <- ds_use %>%
  dplyr::group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = as.numeric(sprintf("%5.3f", mean(c5r2mtsc_std))),
            std_error = as.numeric(sprintf("%5.3f", sd(c5r2mtsc_std) / sqrt(n_students)))
  ) %>%
  dplyr::ungroup()

#simple t test to see the difference between two schools
with(ds_use, t.test(c5r2mtsc_std ~ catholic))

ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')
ecls<-ds_use %>%
  dplyr::select(catholic,
                c5r2mtsc_std,
                race_white,
                p5hmage,
                w3income,
                p5numpla,
                w3momed_hsb) %>%
  na.omit()

treated <- (ecls$catholic==1)
cov<-ecls[, 3:7]
std.diff<- apply(cov,2,function(x) 100*(mean(x[treated])-mean(x[!treated]))/
                   (sqrt(0.5*(var(x[treated])+var(x[!treated])))))
abs(std.diff)


#omnibus test
ecls <- ds_use %>%
  mutate(w3income_1k = w3income / 1000) %>%
  na.omit()

xBalance(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
         data=ecls, report=c("chisquare.test"))


#2, get propensity score

m_ps <- glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
            family = binomial(), data = ecls)
summary(m_ps)

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     catholic = m_ps$model$catholic)


#check upon
labs <- paste("Counts of attending:", c("Catholic", "Public"))

prs_df %>%
  mutate(catholic = ifelse(catholic == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white", fill="#01665e") +
  facet_wrap(~catholic) +
  xlab("Probability of going to Catholic school") +
  theme_bw()

#histbackback(split(prs_df$pr_score, prs_df$catholic),
#main="Propensity score before matching", xlab=c("public", "catholic"))
#matching




#3, matching

prs_df2 <- matchit(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
                 data = ecls, method = "nearest", ratio = 1)
summary(prs_df2)

match.data=match.data(prs_df2)
#get plot for matched data'
plot(prs_df2, type="jitter")


##after matching:
#Computing indices of covariate imbalance after matching
ecls_cov <- c('race_white', 'p5hmage', 'w3income_1k', 'p5numpla', 'w3momed_hsb')
treated <- (match.data$catholic==1)
cov<-match.data[,ecls_cov]
std.diff<- apply(cov,2,function(x) 100*(mean(x[treated])-mean(x[!treated]))/
                   (sqrt(0.5*(var(x[treated])+var(x[!treated])))))
abs(std.diff)

xBalance(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
         data=match.data, report=c("chisquare.test"))


#plot
m_ps <- glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
            family = binomial(), data = match.data)
summary(m_ps)

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     catholic = m_ps$model$catholic)

prs_df %>%
  mutate(catholic = ifelse(catholic == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white", fill="#01665e") +
  facet_wrap(~catholic) +
  xlab("Probability of going to Catholic school") +
  theme_bw()


#4, Analyze outcomes

matches<- data.frame(prs_df2$match.matrix)
group1 <- match(row.names(matches), row.names(match.data))
group2 <- match(matches$X1, row.names(match.data))

yt <- match.data$c5r2mtsc_std[group1]
yc <- match.data$c5r2mtsc_std[group2]

matched.cases <- cbind(matches, yt, yc)
after_match_t_test <- t.test(matched.cases$yt, matched.cases$yc)
summary(after_match_t_test)

#5, sensitivity
ecls2 <- ds_use %>%
  mutate(w3income_1k = w3income / 1000) %>%
  na.omit()
Y<- ecls2$c5r2mtsc_std
Tr<- ecls2$catholic
ps<-glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
        family = binomial(), data = ecls2)

Match<- Match(Y=Y, Tr=Tr, X=ps$fitted, replace = F)
psens(Match, Gamma=3, GammaInc=0.1)
