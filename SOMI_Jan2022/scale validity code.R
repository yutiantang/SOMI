# SOMI 2022 Jan
# Author: Yutian T. Thompson
# Code for the presentation

rm(list=ls(all=TRUE))


# ---- load-sources -----------------------------------------------------------------
base::source(file= "./analysis/yutian/yutian-gpav-function-add.R") #Load Yutian's personal functions
base::source(file="./manipulation/gpav/gpav-processing-funs.R") #Load imputation and gpav processing functions.
base::source(file="./manipulation/gpav/mirl_function.R") #Load model selection functions.
base::source(file="./analysis/common/display-1.R")              #Load common graphing functions.
base::source(file="./analysis/common/display-2.R")              #Load common graphing functions.
base::source(file="./analysis/common/display-legacy.R")         #Load common graphing functions from MReporting

# ---- load-packages ----------------------------------------------------------------
library(miechv3)
library(magrittr) #Pipes
library(ggplot2)
library(table1)
library(miselect)
library(officer)
library(lares)
library(dplyr)
library(mokken) #non-parametric IRT

requireNamespace("miceadds")
requireNamespace("data.table")
requireNamespace("tidyr")
requireNamespace("OuhscMunge")
requireNamespace("jtools")
requireNamespace('sirt')
requireNamespace('naniar')
requireNamespace('tidyverse')
requireNamespace("lavaanPlot")
requireNamespace("parameters")
requireNamespace("lavaan")
requireNamespace("semTools")
requireNamespace("TabularManifest")
requireNamespace("ggpubr")
requireNamespace("lares")
requireNamespace("rlist")

#remotes::install_github("Melinae/TabularManifest")
# source("manipulation/high-school-flow.R"        , local = new.env())

# ---- declare-globals ----------------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values

path_in                   <- file.path(miechv3::path_middle_school_personal(), "high-school.rds")



# ---- load-data ----------------------------------------------------------------
ds                        <- readr::read_rds(path_in)



# ---- tweak-data ----------------------------------------------------------------


ppp <- c("c_health_consistent_bedtime_cut",
         "c_health_tv_time_cut",
         "c_health_read_to_child_cut",
         "c_health_stories_songs_cut",
         "home_c1_how_often_child_eat_with_mother_and_father_cut",
         "home_c1_how_often_child_get_out_house_cut")




ds1<-ds

ds<-ds1%>%
  dplyr::filter(participant_master_id %in% gpav_ids & source %in% c("gpav 6")) %>% #dp4 started from MIECHV6.0
  dplyr::filter(participant_master_id>=0L) %>%  #the data has asq issues

  gpav_process_phl_ace() %>% # Process aces from gpav 5
  gpav_process_aces() %>%
  dplyr::mutate(dad_interview                = dplyr::coalesce(as.logical(dad_interview), FALSE),
                dp4_general_raw_total        = my_rowSums(.[dp4_raw]),
                c_health_consistent_bedtime_cut = dplyr::case_when(c_health_consistent_bedtime %in% c("Always", "Usually")~1L,
                                                                   c_health_consistent_bedtime %in% c("Sometimes", "Rarely", "Never")~0L,
                                                                   TRUE~NA_integer_), #child_health_138
                c_health_tv_time_cut = dplyr::case_when(c_health_tv_time <= 1L~1L,
                                                        c_health_tv_time <=5L & c_health_tv_time >1L~0L,
                                                        TRUE~NA_integer_),
                c_health_read_to_child_cut = dplyr::case_when(c_health_read_to_child %in% c("4-6 days", "Every day")~1L,
                                                              c_health_read_to_child %in% c("0 days", "1-3 days")~0L,
                                                              TRUE~NA_integer_),
                c_health_stories_songs_cut = dplyr::case_when(c_health_stories_songs %in% c("4-6 days", "Every day")~1L,
                                                              c_health_stories_songs %in% c("0 days", "1-3 days")~0L,
                                                              TRUE~NA_integer_),
                home_c1_how_often_child_eat_with_mother_and_father_cut = dplyr::case_when(home_c1_how_often_child_eat_with_mother_and_father ==1L~1L,
                                                                                          home_c1_how_often_child_eat_with_mother_and_father  == 2L~1L,
                                                                                          home_c1_how_often_child_eat_with_mother_and_father >2L~0L,
                                                                                          TRUE~NA_integer_),
                home_c1_how_often_child_get_out_house_cut = dplyr::case_when(home_c1_how_often_child_get_out_house == 6L~1L,
                                                                             home_c1_how_often_child_get_out_house ==7L~1L,
                                                                             home_c1_how_often_child_get_out_house < 6~0L,
                                                                             TRUE~NA_integer_),
                na_row  = apply(.[dp4_raw], 1, FUN = function(x){all(is.na(x))}), #check if dp4 raw scores are missing
                #high_school = rep(1L), #this is for checking data when left join with transfer scores
                date_taken = as.Date(date_taken, "%Y-%m-%d"),
                # record_id = as.integer(record_id),
  ) %>%
  dplyr::mutate(ppp_total = my_rowSums(.[ppp])) %>% #total ppp scores!
  dplyr::select(record_id,
                date_taken,
                source,
                recruit_source, #2021 new
                participant_master_id,
                survey_iteration,
                dad_interview,
                event_name,
                race_ethnicity_h1,
                income_cat_h1,
                work_status_h1,
                marital_status_h1,
                demo_education_h1,
                participant_gender_h1,
                demo_county_id_h1,
                demo_household_size_h1,
                pregnant_current_h1,
                birth_count_current,
                cdemo_index_child_age_years, #age in year
                cdemo_gender_index_child,
                participant_age,
                phl_ace_score,
                dose_hv_visit_after_t1_count, #new Dec 2021
                dose_hv_visit_after_t1_with_ff_count, #new Dec 2021
                dose_hv_duration_enroll_after_t1_days, #new Dec 2021
                dose_hv_visit_after_t1_count_cut_5, #new Dec 2021
                dose_hv_visit_after_t1_any, #New Dec 2021
                father_engagement_client, #2021 new
                model_hv,
                model_hv_source,
                model_hv_any_gpav,
                model_hv_any_c1,
                model_hv_any_pat,
                model_hv_any_sc,
                drugs_alcohol_frequency_h1,
                ace_c_score,
                dp4_raw, #dp4
                dp4_stan,
                #dp4_general_raw_total,
                General.Development.Score, #dp4 stand scores sum-up
                ppp_total,
                milestones_knowledge_total,
                lts_total_mean,
                # high_school


  ) %>%
  dplyr::group_by(participant_master_id) %>%
  dplyr::arrange(date_taken) %>%
  dplyr::mutate(
    client_row_index             = seq_len(dplyr::n()),


    dp4_phys_stan_total_baseline  = dp4_phys_stan[client_row_index == 1L], #DV
    dp4_behav_stan_total_baseline = dp4_behav_stan[client_row_index== 1L],
    dp4_se_stan_total_baseline    = dp4_se_stan[client_row_index == 1L],
    dp4_cog_stan_total_baseline   = dp4_cog_stan[client_row_index == 1L],
    dp4_com_stan_total_baseline   = dp4_com_stan[client_row_index == 1L],

    dp4_phys_delta_since_baseline  = dplyr::case_when(client_row_index == 1L~NA_integer_, client_row_index != 1L~as.integer(dp4_phys_stan - dp4_phys_stan_total_baseline)), #DV
    dp4_behav_delta_since_baseline = dplyr::case_when(client_row_index == 1L~NA_integer_, client_row_index != 1L~as.integer(dp4_behav_stan- dp4_behav_stan_total_baseline)),
    dp4_se_delta_since_baseline    = dplyr::case_when(client_row_index == 1L~NA_integer_, client_row_index != 1L~as.integer(dp4_se_stan - dp4_se_stan_total_baseline)),
    dp4_cog_delta_since_baseline   = dplyr::case_when(client_row_index == 1L~NA_integer_, client_row_index != 1L~as.integer(dp4_cog_stan - dp4_cog_stan_total_baseline )),
    dp4_com_delta_since_baseline   = dplyr::case_when(client_row_index == 1L~NA_integer_, client_row_index != 1L~as.integer(dp4_com_stan - dp4_com_stan_total_baseline )),



    # father_present             = dplyr::coalesce(father_present, FALSE), #comment
    father_present_freq        = dplyr::case_when(dose_hv_visit_after_t1_with_ff_count == 0 ~ '0 visits',
                                                  dose_hv_visit_after_t1_with_ff_count == 1 ~ '1 visit',
                                                  dose_hv_visit_after_t1_with_ff_count > 1 ~ '2+ visits'),
    # dose_hv_visit_any = dplyr::coalesce(dose_hv_visit_any, 0), #one missing data (2021 Oct)

  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(participant_master_id, dad_interview) %>%
  dplyr::arrange(date_taken) %>%
  dplyr::mutate(
    wave = 1:dplyr::n(), #new: sequence
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-client_row_index) %>%
  gpav_process_demo_vars() %>%   # Process demographic variables - prep for regression
  gpav_process_baseline_hv_after_t1_engagement
