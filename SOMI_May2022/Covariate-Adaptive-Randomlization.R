#SOMI May 2022
#Author: Yutian T. Thompson
#Topic: An introduction of covariate adaptive randomization



library(randomizr) #another randomization package
library(carat) #a good tool for randomization
library(Minirand) # a good tool for randomization
library(stringdist,quietly=TRUE) # needed for approximate string matching of variable names
library(excel.link)
library(cem)
library(BiocManager) #this one is special, please see https://bioconductor.org/install/
library(randPack)
library(sp)
require(dplyr, quietly = TRUE)
require(lubridate, quietly= TRUE)
require(utils, quietly = TRUE)
require(tidyr, quietly = TRUE)
library(readr, quietly = TRUE)
require(openxlsx, quietly = TRUE)
require(zoo, quietly = TRUE)
require(MatchIt, quietly = TRUE)
require(lessR, quietly = TRUE)



#-----------load-data-------------------------------------------------------------------
setwd('S:/CCAN/CCANResEval/PSB Research/Navigator Project/Analysis')


ds_randomization            <- readr::read_rds(file = "./data_for_randomization.rds")
ds_list                     <- read.xlsx("S:/CCAN/CCANResEval/PSB Research/Navigator Project/Year 1 Mixed Method Survey/Original data/Updated Centers Accepted and Opted In to E3 Training for Family Engagement as of 11_22_2019.xlsx",
                                         colNames = TRUE)



# ---- tweak-data --------------------------------------------------------------
#practice randomization strategy
ds_sample <- ds_randomization %>%
  dplyr::select(UniqueID, screen_tool_final, region_final) %>%
  #sample_n( 96) %>%
  dplyr::mutate(UniqueID = as.integer(UniqueID)) %>%
  dplyr::right_join(ds_list, by="UniqueID") %>%
  dplyr::select(screen_tool_final, region_final) %>%
  dplyr::mutate(
    id= 1:dim(.)[1]
  )



ds_sample %>%
  dplyr::group_by(screen_tool_final, region_final) %>%
  dplyr::summarise(
    N=dplyr::n_distinct(id),
    ) %>%
  dplyr::ungroup() %>%
  dplyr::tibble()


#A simple randomization --> #unrestricted randomization
set.seed(12345)
index <- round(length(x)*0.5,digits=1)
test.indices <- sample(1:length(x), index)
sr<-randomizr::complete_ra(20, prob = 0.5)
sum(sr)



#practice permuted block randomization
pbr <- c(1L, 2L, 3L, 4L, 5L, 6L)
sample(pbr, 6, replace  = F)


#historical fun
coin_study <- c("00011101001111101000110101111000100111001000001110
                00101010100100001001100010000111010100010000101101
                01110100001101001010000011111011111001101100101011
                01010000011000111001111101101010110100110110110110
                01111100001110110001010010000010100111111011101011
                10001100011000110001100110100100001000011101111000
                11111110000000001101011010011111011110010010101100
                11101101110010000010001100101100111110100111100010
                00001001101011101010110011111011001000001101011111
                11010001111110010111111001110011111111010000100000
                00001111100101010111100001110111001000110100001111
                11000101001111111101101110110111011010010110110011
                01010011011111110010111000111101111111000001001001
                01001110111011011011111100000101010101010101001001
                11101101110011100000001001101010011001000100001100
                10111100010011010110110111001101001010100000010000
                00001011001101011011111000101100101000011100110011
                11100101011010000110001001100010010001100100001001
                01000011100000011101101111001110011010101101001011
                01000001110110100010001110010011100001010000000010
                10010001011000010010100011111101101111010101010000
                01100010100000100000000010000001100100011011101010
                11011000110111010110010010111000101101101010110110
                00001011011101010101000011100111000110100111011101
                10001101110000010011110001110100001010000111110100
                00111111111111010101001001100010111100101010001111
                11000110101010011010010111110000111011110110011001
                11111010000011101010111101101011100001000101101001
                10011010000101111101111010110011011110000010110010
                00110110101111101011100101001101100100011000011000
                01010011000110100111010000011001100011101011100001
                11010111011110101101101111001111011100011011010000
                01011110100111011001001110001111011000011110011111
                01101011101110011011100011001111001011101010010010
                10100011010111011000111110000011000000010011101011
                10001011101000101111110111000001111111011000000010
                10111111011100010000110000110001111101001110110000
                00001111011100011101010001011000110111010001110111
                10000010000110100000101000010101000101100010111100
                00101110010111010010110010110100011000001110000111")


jek <- as.numeric(coin_study)

jek <- as.numeric(strsplit(as.character(coin_study), "")[[1]]) %>%
  as.data.frame() %>%
  dplyr::rename( result = ".") %>%
  dplyr::filter(!is.na(result)) %>%
  dplyr::mutate(
    times = 1:dplyr::n()
  )

jek %>%
  dplyr::filter(times <= 120) %>% #you can change the number here to check about his experiemntal results
  dplyr::summarise(
    total = sum(result, na.rm=T)
  )

# ---- stratified-block --------------------------------------------------------
ds_sample_strata <- ds_sample %>%
  dplyr::mutate(
    strata = dplyr::case_when(region_final=="Midwestern" & screen_tool_final==TRUE~"strata1",
                               region_final=="Midwestern" & screen_tool_final==FALSE~"strata2",
                              region_final=="Northeast" & screen_tool_final==TRUE~"strata3",
                              region_final=="Northeast" & screen_tool_final==FALSE~"strata4",
                              region_final=="Southern" & screen_tool_final==TRUE~"strata5",
                              region_final=="Southern" & screen_tool_final==FALSE~"strata6",
                              region_final=="Western" & screen_tool_final==TRUE~"strata7",
                              region_final=="Western" & screen_tool_final==FALSE~"strata8"),
  ) %>%
  dplyr::arrange(strata) %>%
  dplyr::mutate(
    id= 1:dplyr::n()
  )

# ds_sample_strata_use <- ds_sample_strata %>%
#   dplyr::select(-UniqueID)

pIDs <- new("PatientID",
            strata = c("strata1", "strata2", "strata3", "strata4", "strata5", "strata6", "strata7", "strata8"),
            start  = c(1L, 15L, 20L, 33L, 37L, 64L, 68L, 77L), #the start and stop point drawn from the data new created "id"
            stop   = c(14L, 19L, 32L, 36L, 63L, 67L, 76L, 81L))

validPID(pIDs)

trts = c( A = 1L, B = 1L, C = 1L)
pbdesc = new("PermutedBlockDesc", treatments = trts, type="PermutedBlock",
             numBlocks=27L)
CE1 <- new("ClinicalExperiment",
           name       = "Navigator Randomization",
           treatments = trts,
           factors    = list(F1 = c("A", "B", "C"), F2 = c("TRUE", "FALSE"),
                             F3 = c("Midwestern", "Northeast", "Southern", "Western")),
           strataFun = function(pDesc) pDesc@strata,
           randomization = list(strata1= list(pbdesc), strata2=list(pbdesc), strata3=list(pbdesc), strata4=list(pbdesc),
                                strata5=list(pbdesc), strata6=list(pbdesc), strata7=list(pbdesc), strata8=list(pbdesc)),
           patientIDs = pIDs)

CT1 <- createTrial(CE1, seed = c(120, 378, 456, 225, 610, 783, 832, 706)) #this is a random seed, set up any values you wish

#table(dempb<- randPack:::.newPBlock(pbdesc))


make_LD = function(data,i){
  new("PatientData", patientID = data$UniqueID[i], date=Sys.Date(),
      covariates=list(screen_tool_final = data$screen_tool_final[i], region_final=data$region_final[i]), strata=data$strata[i])
} #since this function is not generalized enough, I leave it here rather than save into the navigator_function repo


ds_sample_use <- ds_sample_strata %>%
  dplyr::mutate(UniqueID          = as.character(UniqueID),
                screen_tool_final = as.character(screen_tool_final),
                region_final      = as.character(region_final))

pd_list <- list()
for (i in 1:81){
  pd_list[[i]]   <- assign(paste0("pd", i, sep=""), make_LD(ds_sample_use, i))
}


tyt_list <- list()
for (j in 1:81) {
  tyt_list[j] <- getTreatment(CT1, pd_list[[j]])

}

assignment <- do.call(rbind, tyt_list) %>% as.data.frame() %>%
  dplyr::mutate(id = 1:dplyr::n()) %>%
  dplyr::left_join(ds_sample_use, by="id")  %>%
  dplyr::select(-screen_tool_final, -region_final)




#---- Pocock-Simon Minimization-------------------

trt1 <- res
#Display the number of randomized subjects at covariate factors
balance1 <- randbalance(trt1, covmat, ntrt, trtseq)

ps <- ds_sample %>%
  dplyr::select(-id) %>%
PocSimMIN(., weight = NULL)

ps1_assign <- cbind(ds_sample, ps$assignments) %>%
  dplyr::rename(
    group = `ps$assignments`
  )

ps1_assign %>%
  dplyr::group_by(group, screen_tool_final, region_final) %>%
  dplyr::summarise(
    N=dplyr::n_distinct(id),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(., group) %>%
  flextable()


#way II:
ds_sample2 <- ds_sample %>%
  dplyr::select(-id)
assignments <- rep(NA, dim(ds_sample)[1]) #create an empty vector
group_name <- c(1,2,3) #cannot be the character vector

assignments[1] <- sample(group_name, 1, replace = TRUE, prob = c(1/3, 1/3, 1/3))
for (j in 2:dim(ds_sample)[1]){
  assignments[j] <- Minirand(covmat=ds_sample2, j, covwt=c(1/2, 1/2), ratio=c(1,1,1),
                     ntrt=3, trtseq=group_name, method="Range", result=assignments, p = 0.9)
}

trt1 <- res
#Display the number of randomized subjects at covariate factors
balance1 <- randbalance(assignments, ds_sample2, 3, group_name)


ps2_assign <- cbind(ds_sample, assignments) %>%
  dplyr::rename(
    group = assignments
  ) %>%
  dplyr::mutate(
    group = dplyr::recode(group,"1" = "A",
                          "2" = "B",
                          "3" = "C")
  )

ps2_assign %>%
  dplyr::group_by(group, screen_tool_final, region_final) %>%
  dplyr::summarise(
    N=dplyr::n_distinct(id),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(., group) %>%
  flextable()
