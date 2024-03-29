# MIECHV 5.0 2020
# Outcome: IPVAS
# Author: Yutian T. Thompson
#closeAllConnections()
rm(list=ls(all=TRUE))
#dev.off()

# ---- load-sources -----------------------------------------------------------------
base::source(file= "./analysis/yutian/yutian-gpav-function-add.R") #Load Yutian's functions
base::source(file="./manipulation/gpav/gpav-processing-funs.R") #Load imputation and gpav processing functions.
base::source(file="./analysis/common/display-1.R")              #Load common graphing functions.
base::source(file="./analysis/common/display-2.R")              #Load common graphing functions.
base::source(file="./analysis/common/display-legacy.R")         #Load common graphing functions from MReporting

# ---- load-packages ----------------------------------------------------------------
library(miechv3)
library(magrittr) #Pipes
library(ggplot2)
library(mice)
library(micemd)
library(miceadds)
library(mimtl)
library(VIM)
library(lme4)
library(lmerTest)
library(table1)
library(miselect)
library(officer)
library(mitml)  #install.packages("mitml")
# library(tidyverse)
requireNamespace("miceadds")
requireNamespace("data.table")
requireNamespace("dplyr")
requireNamespace("tidyr")
requireNamespace("OuhscMunge")
# requireNamespace("DescTools")
requireNamespace("jtools")
requireNamespace('sirt')
requireNamespace('naniar')
requireNamespace('tidyverse')
requireNamespace("lavaanPlot")
requireNamespace("parameters")
requireNamespace("lavaan")
requireNamespace("semTools")

# source("manipulation/high-school-flow.R"        , local = new.env())

# ---- declare-globals ----------------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values

path_in                   <- file.path(miechv3::path_middle_school_personal(), "high-school.rds")
path_out                  <- file.path(miechv3::path_middle_school_personal(), "benchmark_ipvas.rds")
path_out_imputation_mom       <- file.path(miechv3::path_middle_school_personal(), "impute_data_ipvas_mom.rds")
path_out_imputation_dad       <- file.path(miechv3::path_middle_school_personal(), "impute_data_ipvas_dad.rds")



outcome_variable <- "ipvas_total_score"
demo_vars <-paste(gpav_demo_vars, collapse = '+')
model_vars <- paste(gpav_model_vars, collapse = "+")


source_to_use <- c('gpav 5', 'gpav 6', 'eto')
palette_group <-c(
  "No HV" = "#7a0177",
  "C1" = "#1a9850",
  "PAT" = "#fc8d59",
  "SC"  = "#0868ac"
)



plot_md_pattern <- function(dat) {
  # escape function if dataset is complete
  total_sample = nrow(dat)
  if (!any(is.na(dat))) {
    return(plot_a_mouse())
  }
  # get md pattern and store additional info
  pat <- mice::md.pattern(dat, plot = FALSE, rotate.names = T)
  vrb <- colnames(pat)[-ncol(pat)]
  colnames(pat) <- c(vrb, "NA_per_pat")
  pat_freq <- as.numeric(rownames(pat))[-nrow(pat)]
  NA_per_pat <- as.numeric(pat[, ncol(pat)])[-nrow(pat)]
  NA_per_vrb <- as.numeric(pat[nrow(pat), ])[-ncol(pat)]
  NA_total <- pat[nrow(pat), ncol(pat)]
  # make the pattern tidy
  long_pat <- pat[-nrow(pat), ] %>%
    cbind(., pat_freq, pat_nr = 1:nrow(.)) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(cols = all_of(vrb),
                        names_to = "vrb",
                        values_to = "obs") %>%

    cbind(., NA_per_vrb) %>%
    dplyr::mutate(mis_percent = paste0(sprintf("%3.1f", (NA_per_vrb/total_sample)*100), "%"),
                  #NA_per_vrb = paste0(NA_per_vrb, " ", "(",mis_percent, ")")
    )
  palette_obs<-c("1" = "#5dd3b0",
                 "0" = "#f12f37")

  # plot the md pattern
  p <- long_pat %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(
      x = vrb,
      y = pat_nr,
      fill = as.factor(obs),
      color=as.factor(obs),
      group = NA_per_pat
    ),
    color = "black") +
    scale_color_manual(values = palette_obs, guide = "none") +
    #scale_fill_manual(values =  palette_obs) +
    # set axes
    ggplot2::scale_x_discrete(
      limits = vrb,
      position = "bottom",
      labels = as.character(NA_per_vrb),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_reverse(
      breaks = 1:max(long_pat$pat_nr),
      labels = as.character(pat_freq),
      expand = c(0, 0),
      sec.axis = ggplot2::dup_axis(labels = as.character(NA_per_pat),
                                   name = "Number of missing entries per pattern")
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    # add labels
    ggplot2::labs(
      x = "Number of missing entries per variable",
      y = "Pattern frequency",
      title = "Missing data pattern",
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = vrb,
        y = -Inf,
        #label = abbreviate(.data[["vrb"]])
        label = paste(.data[["vrb"]], mis_percent, sep=" ")
      ),
      data   = long_pat[1:length(vrb),],
      hjust  = 1.2,
      vjust  = "center",
      angle  =90,
      size   = 2.7,
      colour = "#4d4d4d",
    ) +
    # add styling
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(
        t = 20,
        l = 10,
        b = 10,
        r = 10,
        unit = "pt"
      ),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 10)),
      panel.grid = ggplot2::element_blank()
    )
  return(list(p, pat))
}

unchange <- c(
  "source",
  "event_name",
  "participant_master_id",
  "record_id",
  "dose_hv_duration_enroll_days"    ,
 "dose_hv_duration_last_visit_days",
 "dose_hv_visit_count"             ,
 "dose_hv_visit_count_cut_5"       ,
  "dose_hv_visit_any",
  "model_hv",
  "model_hv_source"  ,
  "model_hv_any_gpav" ,
  "model_hv_primary",
  "model_hv_any_c1"  ,
  "model_hv_any_pat"  ,
  "model_hv_any_sc" ,
  "race_ethnicity_h1",
  "caucasian",
  "hispanic",
  "participant_gender_h1",
  "delta_dose_since_baseline",
  "delta_outcome_since_baseline",
  "father_present",
  "father_present_freq",
  "county_ok",
  "county_tulsa",
  "cdemo_gender_index_child",
  "demo_child_index_male",
  "outcome_baseline",
  "dad_interview",
  "drugs_alcohol_frequency_h1",
  "dose_hv_visit_terminal_count",
  "dose_baseline"  )


# ---- load-data ----------------------------------------------------------------
# To refresh the cache, run high-school-flow file (automatically runs all 3 funnels)
# source("manipulation/high-school-flow.R"        , local = new.env())

ds                        <- readr::read_rds(path_in)
imputed_Data_mom          <- readr::read_rds(path_out_imputation_mom)
imputed_Data_dad          <- readr::read_rds(path_out_imputation_dad)
avp <- read.csv("C:/Users/YTHOMPSO/Dropbox/Work/Teaching/2021SOMI_9/avp.csv", header=FALSE)

# ---- tweak-data ----------------------------------------------------------------
gpav_ids <- ds$participant_master_id[ds$source ==c('gpav 5', 'gpav 6') ]
eto_ids  <- ds$participant_master_id[ds$source == 'eto']

ipvas <- grep('ipvas_', names(ds), value=TRUE) #ipvas has 17 items + total score
ipvas_item <- ipvas[which(ipvas != "ipvas_total_score")]

test<- ds[ipvas_item]
psych::alpha(test, check.keys=TRUE)# 0.83 # 09/07/21

psych::describe.by(ds$ipvas_total_score, group = ds$race_ethnicity_h1)


#ds%<>%
  ds2<-ds %>%
  dplyr::filter(participant_master_id %in% gpav_ids & source %in% c( "gpav 5", "gpav 6")) %>% #, "eto"
  gpav_process_phl_ace() %>% # Process aces from gpav 5
  gpav_process_aces() %>%
  dplyr::mutate(dad_interview = dplyr::coalesce(as.logical(dad_interview), FALSE)) %>%
  gpav_factor_scores_ers() %>%
  gpav_factor_scores_hvp() %>%
  dplyr::select(record_id,
                date_taken,
                source,
                participant_master_id,
                survey_iteration,
                event_name,
                ers_dad_seen_3m,
                ers_mom_seen_3m,
                father_present,
                race_ethnicity_h1, # constant
                income_cat_h1,
                work_status_h1,
                marital_status_h1,
                demo_education_h1,
                participant_gender_h1, # probably constant?
                demo_county_id_h1, # don't impute maybe?
                demo_household_size_h1,
                pregnant_current_h1,
                birth_count_current,
                cdemo_index_child_age_years,
                cdemo_gender_index_child,# probably constant?
                participant_age,
                phl_ace_score, # confirm but probably constant
                dose_hv_duration_enroll_days,
                dose_hv_duration_last_visit_days,
                dose_hv_visit_count,
                dose_hv_visit_count_cut_5,
                dose_hv_visit_any, # once it's true, should always be true
                model_hv,
                model_hv_source,
                model_hv_any_gpav,
                model_hv_any_c1,
                model_hv_any_pat,
                model_hv_any_sc,
                drugs_alcohol_frequency_h1,
                ace_c_score,
                dad_interview,# fake, constant
                # dad_engage_child_ff,
                #ers_emotional_ff,
                #ers_caregive_ff,
                # dad_engage_child_m,
                #ers_emotional_m,
                #ers_caregive_m,
                # dad_engage_hv_ff,
                # dad_attitude_hv_ff,
                # dad_engage_hv_m,
                # dad_attitude_hv_m,
                ipvas_total_score
  ) %>%
  dplyr::group_by(participant_master_id) %>%
  dplyr::arrange(date_taken) %>%
  dplyr::mutate(

    client_row_index             = seq_len(dplyr::n()),
    outcome_baseline             = ipvas_total_score[client_row_index == 1L], #DV
    delta_outcome_since_baseline = dplyr::case_when(client_row_index == 1L ~ NA_integer_,
                                                    client_row_index != 1L ~ as.integer(ipvas_total_score - outcome_baseline)), #DV
    father_present             = dplyr::coalesce(father_present, FALSE),
    father_present_freq        = dplyr::case_when(father_present == 0 ~ '0 visits',
                                                  father_present == 1 ~ '1 visit',
                                                  father_present > 1 ~ '2+ visits'),

    ipvas_total_score        = as.integer(ipvas_total_score) #DV
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-client_row_index) %>%
  gpav_process_demo_vars()%>%   # Process demographic variables - prep for regression
  gpav_process_baseline_hv_engagement

aa<- ds %>% dplyr::select(participant_master_id, record_id, dad_interview, date_taken)


# ---- descriptive -------------------------------------------------------------

descriptive_plot <- function(data, y, DV, group,x){
  data %>%
    ggplot(aes(x ={{x}}, y = {{y}}, color={{group}}, fill={{group}}))+
    #geom_bar(position="fill", stat="identity", alpha = 0.6) +

    geom_bar(stat="identity", position = "dodge", alpha = 0.7) +
    scale_color_manual(values = palette_group, guide = "none") +
    scale_fill_manual(values =  palette_group) +
    theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border=element_blank(),
          axis.line = element_line(), legend.position = "none")+
    theme_bw()+
    labs(title=NULL, x="wave", y=DV)
}

descriptive_plot2 <- function(data, y, DV, x){
  data %>%
    ggplot(aes(x ={{x}}, y = {{y}}, color=model_hv_primary, fill=model_hv_primary))+
    geom_point(position = 'jitter')+
    #geom_smooth(aes(x = wave , y ={{y}}, group=group), method = "loess")+

    #geom_smooth(aes(x ={{x}}, y = {{y}}), se=F, method = "lm")+
    geom_smooth(aes(group=model_hv_primary), method = "lm", formula = y~x, se=T, na.rm = T)+
    scale_color_manual(values = palette_group, guide = "none") +
    scale_fill_manual(values =  palette_group) +
    theme_bw()+
    facet_wrap(~model_hv_primary,scales = "free_x")+
    labs(title=NULL, x="wave", y=DV, fill="program")
}

distribution_plot3 <- function(data, y, DV){
  data %>%
    ggplot(aes(y = {{y}}))+
    geom_histogram(position = "stack")+
    theme_bw()+
    labs(title=NULL, x=NULL, y=DV)
}



ds_mom %>%
  dplyr::filter(!is.na(model_hv_primary)) %>%
descriptive_plot2(y=ipvas_total_score, DV="IPVAS total score",  x= as.factor(wave))
distribution_plot3(ds_mom, ipvas_total_score, DV="IPVAS total score")


  psych::describe.by(ds_mom$ipvas_total_score, group = ds_mom$wave)


  aa<- ds_mi %>% select(participant_master_id,  record_id, dad_interview, source, wave,
                        model_hv,
                        model_hv_primary,
                        model_hv_source,
                        model_hv_any_gpav,
                        model_hv_any_c1,
                        model_hv_any_pat,
                        model_hv_any_sc,
                        dose_hv_visit_any,
  )
# ---- imputation_prepare --------------------------------------------------------------
  ds_wave <- ds %>% #check and found only two waves for both mom and dad
    dplyr::select(
      participant_master_id, record_id, dad_interview,
    ) %>%
    dplyr::group_by(participant_master_id, record_id, dad_interview) %>%
    dplyr::summarise_all(., dplyr::first, na.rm=T) %>%
    dplyr::mutate(
      wave1 = rep(1L),
      wave2 = rep(2L),
    ) %>%
    tidyr::gather(key, wave, -participant_master_id, -record_id, -dad_interview,) %>%
    dplyr::ungroup()




  ds_mi <- ds %>%
    #dplyr::filter(!is.na(phl_ace_score)) %>% #DV needs to be complete?
    dplyr::group_by(participant_master_id, record_id, dad_interview, date_taken) %>%
    dplyr::arrange(date_taken) %>%
    dplyr::summarise_all(., dplyr::first) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(participant_master_id, record_id, dad_interview) %>%
    dplyr::mutate(
      wave = seq_along(record_id)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(ds_wave, by=c("participant_master_id", "record_id", "dad_interview", "wave")) %>%
    dplyr::group_by(participant_master_id, record_id, dad_interview) %>%
    dplyr::mutate_at(unchange, dplyr::first) %>%
    dplyr::ungroup() %>%
    tidyr::separate(record_id, c("first", "second", "third"), remove = F) %>%
    dplyr::mutate(
      third = dplyr::if_else(is.na(third), 0, 1), #warning is from here
      individual_id = as.numeric(paste0(first, second, third))
    ) %>%
      dplyr::select( -c(`Data Source`,
                        `Index Child Age`,
                        `HV Visits`,
                        `Most Recent HV`,
                        participant_gender_h1,
                        marital_status_h1,
                        demo_education_h1,
                        participant_edu_c,
                        participant_edu_college,
                        participant_edu_some_college_vo_tech,
                        participant_edu_some_high_school,
                        participant_edu_less_ninth_grade,
                        participant_male,
                        work_status_h1,
                        participant_edu,
                        county_ok,
                        county_tulsa,
                        outcome_baseline,
                        delta_outcome_since_baseline,
                        first, second, third,
                        event_name,
                        survey_iteration,
                        date_taken,
                        key,
                        demo_county_id_h1,
      ))


  rng_seed <- 1127575261
  skip_imputation_vars <- c("source",
                            #"event_name",
                            "participant_master_id",
                            "individual_id",
                            "Case_number",
                            #"date_taken",
                            #"survey_iteration",
                            "dose_hv_duration_enroll_days",
                            "ers_mom_seen_3m",
                            "model_hv"  ,
                            "model_hv_source"  ,
                            "model_hv_any_gpav" ,
                            "race_ethnicity_h1",
                            "caucasian",
                            "hispanic",
                            "dose_visit_terminal_count",
                            "participant_gender_h1",
                            "wai_total",
                            "hri_total",
                            "wai_categorical",
                            "delta_dose_since_baseline",
                            "delta_outcome_since_baseline",
                            "delta_hri_since_baseline",
                            "delta_wai_since_baseline",
                            "wai_baseline",
                            "hri_baseline",
                            "father_present",
                            "father_present_freq",
                            "county_tulsa",
                            "cdemo_gender_index_child",
                            "demo_child_index_male",
                            "outcome_baseline",
                            "dad_interview",
                            "model_hv_primary",
                            "drugs_alcohol_frequency_h1",
                            "key",
                             "individual_id",
                            "record_id",
                             "wave"
  )

  ds_mi_mom <- ds_mi %>% dplyr::filter(dad_interview == 0) %>% dplyr::select(-c(ers_dad_seen_3m, dad_engage_child_ff, #ers_emotional_ff, ers_caregive_ff,
                                                                                dad_engage_hv_ff, dad_attitude_hv_ff))
  ds_mi_dad <- ds_mi %>% dplyr::filter(dad_interview == 1) %>% dplyr::select(-c(ers_mom_seen_3m, ace_c_score, pregnant_current_h1, dad_engage_child_m, #ers_emotional_m, ers_caregive_m,
                                                                                dad_engage_hv_m, dad_attitude_hv_m))


  pattern<-c(
  "income_cat_h1",
  "demo_household_size_h1",
  "pregnant_current_h1",
  "birth_count_current",
  "cdemo_index_child_age_years",
  "participant_age",
  "phl_ace_score",
  "dose_hv_duration_last_visit_days",
  "dose_hv_visit_count",
  "dose_hv_visit_count_cut_5",
  "dose_hv_visit_any",
  "model_hv_any_c1",
  "model_hv_any_pat",
  "model_hv_any_sc",
  "ace_c_score",
  "dad_engage_child_m",
  "dad_engage_hv_m",
  "dad_attitude_hv_m",
  "ipvas_total_score",
  "married_cohabiting",
  "employed",
  "participant_edu_num",
  "dose_hv_visit_terminal_count",
  "dose_baseline",
  "ipvas_total_score"
  )



  missing_pattern<-ds_mi_mom %>%
    dplyr::select(all_of(pattern)) %>%
    plot_md_pattern() #the function we created

  missing_pattern[[1]]
  nrow(missing_pattern[[2]])
  #aa<- ds_mi_mom %>% dplyr::group_by(individual_id) %>% dplyr::summarise(sample=dplyr::n(), wave_count = dplyr::n_distinct(wave))
  #check ICC
  check_icc <- lmer(ipvas_total_score~1+(1|participant_master_id), data=ds_mi_mom)
summary(check_icc)
19.05/(19.05+26.46)


# ---- imputation --------------------------------------------------------------
#step1: specify matrix
skip_var_index <- which(names(ds_mi_mom) %in% skip_imputation_vars)
cluster_id_index <- which(names(ds_mi_mom) %in% "participant_master_id")
#random_effect_index <- which(names(ds_mi_mom) %in% "wave")
impute_vars <- setdiff(names(ds_mi_mom), skip_imputation_vars)
impute_var_index <- which(names(ds_mi_mom) %in% impute_vars)

#way 1:
nvars <- ncol(ds_mi_mom)
pmat  <- matrix(1,nvars,nvars) #fix effect

pmat[skip_var_index,] <- 0
pmat[,skip_var_index] <- 0
# pmat[, random_effect_index]<- 2
# pmat[random_effect_index,]<- 2
pmat[, cluster_id_index]<- -2
diag(pmat) <- 0


# pmat
# aa<-as.data.frame(pmat)
#way 2:
# temp1<-mice(ds_mi_mom, m=1, maxit=0)
# temp <- temp1$predictorMatrix
# temp1$predictorMatrix[skip_var_index,skip_var_index]<-0
# # temp[,random_effect_index]<- 2
# # temp[random_effect_index,]<- 2
# temp1$predictorMatrix[,cluster_id_index]<- -2


# bb<-as.data.frame(temp1$predictorMatrix)

#step2: specify method
#way 1

meth1<-ds_mi_mom %>%
  purrr::map_chr(class) %>%
  tibble::enframe(
    name = "variable",
    value = "type"
  ) %>%
  #dplyr::select(all_of(impute_vars)) %>%
  # dplyr::summarise_all(., class) %>%
  # #purrr::map_chr(class) #it is another way to summarise the variable's type.
  # t() %>%
  # as.data.frame() %>%
   tibble::rowid_to_column(., var="index") %>%
  # dplyr::rename(type = V1) %>%
  #tibble::rownames_to_column(., "index") %>%
  dplyr::mutate(
    assign_meth = as.character(dplyr::case_when(variable %in% impute_vars & type == "factor"~"2l.jomo",
                                   variable %!in% impute_vars~"",
                                   TRUE~"2l.glm.norm")),
  ) %>%
 # dplyr::filter((assign_meth %!in% c(""))) %>%
  #dplyr::select(-type, -variable, -index) %>%
  dplyr::pull(assign_meth) #pull the vector from the data frame.


# meth2 <- vector()
# meth2[impute_var_index] = meth1$assign_meth
# meth2[skip_var_index] = ""


#way2: use the function to find the method --> not converged sometimes.
method<-find.defaultMethod(ds_mi_mom,cluster_id_index) %>% as.data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::filter(variable %in% impute_vars) %>%
  dplyr::filter(!is.na(`.`))


print(method)

#find the order of variables
pat <- as.data.frame(missing_pattern[[2]])
order <- names(pat)
impute_order <-paste(which(names(ds_mi_mom) %in% order), collapse = ",")


imputed_Data <- mice(ds_mi_mom, predictorMatrix = pmat, method = meth1, m=5, seed = 12335, maxit=100)

plot(imputed_Data,  plot.burnin=TRUE, c("ipvas_total_score"))
plot(imputed_Data,  plot.burnin=TRUE, c("dad_attitude_hv_m"))

#analysis phase
form_mom <-"ipvas_total_score~dose_hv_visit_count+dad_engage_child_m+dad_engage_hv_m+ dad_attitude_hv_m+(1|participant_master_id)"



fit_mom1 <- with(imputed_Data, exp = lme4::lmer(formula(form_mom)))

#pooling
result<-summary(miceadds::lmer_pool(fit_mom1$analyses)) %>%
  knitr::kable(format = 'html', booktabs=T)%>%
  #kable(booktabs = T) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)



# other_paper -------------------------------------------------------------
#use the result from people's paper that comparing the packages
ds_avp <- avp %>%
  tidyr::separate(V1, c("size", "dataset", "missing_perc", "package", "LR", "SVM"), sep=" ",remove = T) %>%
  dplyr::filter(package %in% c("MICE", "AMELIA-II"))


ds_avp_use <-  ds_avp %>%
  dplyr::filter(dataset == "BNG") %>%
  dplyr::mutate(
    size = dplyr::recode(size, "10,000"=10000L, "15,000"=15000L),
    package = as.factor(package),
    missing_perc = as.integer(missing_perc),
    LR = as.numeric(LR),
    SVM = as.numeric(SVM)
  )




palette_package   <- c("MICE"="#0000FF", "AMELIA-II" = "#ff0000")
line_type_outcome <- c("MICE" = "dotdash", "AMELIA-II" = "solid")

ds_avp_use %>%
  dplyr::filter(size==10000L) %>%
ggplot(aes(x = missing_perc, y = LR, linetype=package)) +
  geom_point(aes(y = LR)) +
  geom_line(aes(y = LR, group = package, colour=package), stat = "identity",
            position = "identity", size=1.5) +
  coord_cartesian(ylim = c(0, 0.15)) +
  scale_color_manual(values = palette_package) +
  scale_fill_manual(values = palette_package) +
  scale_linetype_manual(values=line_type_outcome, guide = "none") +
  theme_bw()+
  #theme(legend.position="none")+
  #theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.border = element_blank())+
  #theme(plot.margin = unit(c(1,0.1,0.1,0.1), "lines"))+
  theme(text=element_text(size=12,  family="sans"))+
  labs(title="N=10,000, LR", x="missing percent", y="Accuracy variance percentage")
