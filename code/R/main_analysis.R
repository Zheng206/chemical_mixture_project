library(dplyr)
library(stringr)
library(tidyverse)
library(glue)
library(readxl)
library(ggplot2)
library(rlang)
library(DT)
library(broom)
library(ggfortify)
library(kableExtra)
library(corrplot)
library(car)
library(geepack)
library(qgcomp)
library("gplots")
library("RColorBrewer")
library("lattice")
library(haven)
library(parallel)
library(mediation)
source("./code/R/useful_function.R")

# Data Management
dv = read_csv("./Data/OPEs and Covariates/opfr.csv")
cov_df = read_csv("./Data/OPEs and Covariates/covariates_full.csv")
cov_col = c("sex", "race", "home_score_total", "mom_edu")
cov_df = cov_df %>% dplyr::select(1,cov_col) 
cov_df$race = sapply(cov_df$race, function(x){if (x %in% c("Asian, Pacific Islander, or Native Hawaiian", "Other/Multiracial", "Black or African American")){return("Non-white")}else{return(x)}}, USE.NAMES = FALSE)
cov_df$mom_edu = sapply(cov_df$mom_edu, function(x){if(x %in% c("8th grade or less", "Some high School (Grades 9, 10, 11, 12)", "Technical or trade school", "High school diploma (completed grade 12)")){return("Before college")}else if (x %in% c("GED", "Graduate or professional school")){return("Graduate or professional school")}else{return(x)}}, USE.NAMES = FALSE)
dv[sapply(dv, is.character)] = lapply(dv[sapply(dv, is.character)], as.factor)
dv_wide = dv %>% pivot_wider(names_from = analyte, values_from = sg_adj_read)
dv_wide = dv_wide[c(1,3,6:9)]
cotinine_16w = read_csv("./Data/Covariates/cotinine_16w.csv")
cotinine_26w = read_csv("./Data/Covariates/cotinine_26w.csv")
cotinine_birth = read_csv("./Data/Covariates/cotinine_birth.csv")
cotinine_m12 = read_csv("./Data/Covariates/cotinine_m12.csv")
cotinine_m24 = read_csv("./Data/Covariates/cotinine_m24.csv")
cotinine_m36 = read_csv("./Data/Covariates/cotinine_m36.csv")
cotinine_m48 = read_csv("./Data/Covariates/cotinine_m48.csv")
cotinine_m60 = cotinine_m48
cotinine_m60$visit = "m60"
cotinine_p3 = cotinine_m48
cotinine_p3$visit = "p3"
cotinine = rbind(cotinine_16w, cotinine_26w, cotinine_birth, cotinine_m12, cotinine_m24, cotinine_m36, cotinine_m60, cotinine_p3) %>% dplyr::select("subject_id", "visit", "result.res")
colnames(cotinine) = c("subject_id", "visit", "cotinine")
cotinine = cotinine %>% distinct()
covariates = c("cotinine", "sex", "race", "home_score_total","mom_edu" )

cov_df[c(2:3,5)] = lapply(cov_df[c(2:3,5)], as.factor)

window_name = c("prenatal", "birth", "3y", "5-8y", "8y", "3y-pre_diff", "5y-pre_diff","child-pre_diff")
Subcortical = read_csv("./Data/HOME_Study_FreeSurfer-CSV/asegstats.csv")
ICV = Subcortical[c(1,67)]

## Brain Metabolites
metabolites_df = read_excel("./Data/MRS/HOME_Study_MRS_Concentrations_AiminPenn_DeID.xlsx")
metabolites = data_gen(metabolites_df, dv_wide, cotinine)
metabolites_df_name = paste0(names(metabolites), "_metabolites")
for (i in 1:length(names(metabolites))){
  assign(metabolites_df_name[i], metabolites[[names(metabolites)[i]]])
} 
metabolites_outcomes = c("mI", "NAA", "Cr", "Cho", "Glu", "Glx")

## Cortical Volume (CV)
aparc_lh_CV = read_csv("./Data/Volume/aparc_lh_volume.csv")
aparc_rh_CV = read_csv("./Data/Volume/aparc_rh_volume.csv")
aparc_lh_CV[,c(2:35)] = aparc_lh_CV[,c(2:35)]/1000
aparc_rh_CV[,c(2:35)] = aparc_rh_CV[,c(2:35)]/1000
combined_df_CV = gen_com_df(type = "CV", aparc_lh = aparc_lh_CV, aparc_rh = aparc_rh_CV, Subcortical)[["combined_df"]]
CV_outcomes = gen_com_df(type = "CV", aparc_lh = aparc_lh_CV, aparc_rh = aparc_rh_CV, Subcortical)[["outcomes"]]
CV = data_gen(combined_df_CV, dv_wide, cotinine)
CV_df_name = paste0(names(CV), "_CV")
for (i in 1:length(names(CV))){
  assign(CV_df_name[i], CV[[names(CV)[i]]])
} 
outlier_ind_CV =c(1,4,8,11,12,14,15,19:23,27:30,32:36,38)

### Separate Parstriangularis Volumes from hemispheres
roi_lh = aparc_lh_CV[c("SubjectID", "lh_parstriangularis_volume", "lh_paracentral_volume")]
colnames(roi_lh) = c("SubjectID", "lh_parstriangularis", "lh_paracentral")
roi_lh = roi_lh %>% left_join(ICV, by = "SubjectID")
roi_rh = aparc_rh_CV[c("SubjectID", "rh_parstriangularis_volume", "rh_paracentral_volume")]
colnames(roi_rh) = c("SubjectID", "rh_parstriangularis", "rh_paracentral")
roi_rh = roi_rh %>% left_join(ICV, by = "SubjectID")
CV_lh = data_gen(roi_lh, dv_wide, cotinine)
CV_rh = data_gen(roi_rh, dv_wide, cotinine)
CV_lh_name = paste0(names(CV_lh), "_lhCV")
for (i in 1:length(names(CV_lh))){
  assign(CV_lh_name[i], CV_lh[[names(CV_lh)[i]]])
} 
CV_rh_name = paste0(names(CV_rh), "_rhCV")
for (i in 1:length(names(CV_rh))){
  assign(CV_rh_name[i], CV_rh[[names(CV_rh)[i]]])
} 

## Cortical Thickness (CT)
aparc_lh_CT = read_csv("./Data/HOME_Study_FreeSurfer-CSV/aparc_lh_thickness.csv")
aparc_rh_CT = read_csv("./Data/HOME_Study_FreeSurfer-CSV/aparc_rh_thickness.csv")
combined_df_CT = gen_com_df(type = "CT", aparc_lh = aparc_lh_CT, aparc_rh = aparc_rh_CT, Subcortical)[["combined_df"]]
CT_outcomes = gen_com_df(type = "CT", aparc_lh = aparc_lh_CT, aparc_rh = aparc_rh_CT, Subcortical)[["outcomes"]]
CT = data_gen(combined_df_CT, dv_wide, cotinine)
CT_df_name = paste0(names(CT), "_CT")
for (i in 1:length(names(CT))){
  assign(CT_df_name[i], CT[[names(CT)[i]]])
} 
outlier_ind_CT =c(3,4,7,14)

## Subcortical ROI
aparc_lh_area = read_csv("./Data/HOME_Study_FreeSurfer-CSV/aparc_lh_area.csv")
aparc_rh_area = read_csv("./Data/HOME_Study_FreeSurfer-CSV/aparc_rh_area.csv")
combined_df_Sub = gen_com_df(type = "subroi", aparc_lh = aparc_lh_area, aparc_rh = aparc_rh_area, Subcortical)[["combined_df"]]
Sub_outcomes = gen_com_df(type = "subroi", aparc_lh = aparc_lh_area, aparc_rh = aparc_rh_area, Subcortical)[["outcomes"]]
Sub = data_gen(combined_df_Sub, dv_wide, cotinine)
Sub_df_name = paste0(names(Sub), "_Sub")
for (i in 1:length(names(Sub))){
  assign(Sub_df_name[i], Sub[[names(Sub)[i]]])
} 
outlier_ind_Sub = c(1:2,4,5,6,9,11,13:20,23,30:32,33,34)

## Neurobehavior 
data = read_sas("./Data/neurobehavior/outcomes_add.sas7bdat")
Neuro_outcomes = c("ChAMP_Index_Verbal", "ChAMP_Index_Visual", "ChAMP_Index_Immediate", "ChAMP_Index_Delayed", "ChAMP_Index_Total_Memory",
             "Pegboard_DH_Seconds", "Pegboard_DH_Drops", "Pegboard_NDH_Seconds", "Pegboard_NDH_Drops", "BRIEF_Child_BRI",
             "BRIEF_Child_ERI", "BRIEF_Child_CRI", "BRIEF_Child_GEC", "BASC_Child_FII_GC_T", "BASC_Child_ESI_GC_T",
             "BASC_Child_IHI_GC_T", "BASC_Child_INZ_GC_T", "BASC_Child_PAI_GC_T", "BASC_Child_SPI_GC_T")
neurobh = data[c("SubjectID", Neuro_outcomes)]
neurobh = data_gen(neurobh, dv_wide, cotinine)
neurobh_df_name = paste0(names(neurobh), "_neurobh")
for (i in 1:length(names(neurobh))){
  assign(neurobh_df_name[i], neurobh[[names(neurobh)[i]]])
} 
outlier_ind_nb =c(6,8, 14:15,17:19)
glm_ind_nb = c(7,9)

# Multiple Informant Model
metabolites_mi = MI_all_results(metabolites_outcomes, gee_df = gee_df_metabolites, out.dir = "./Results/Before_MC", name = "brain_metabolites")
metabolites_mi$coef_df_main = metabolites_mi$coef_df_main %>% mutate(type = "metabolites") %>% add_column(window = "5-8y", .after = "term")
metabolites_mi$coef_df_total = metabolites_mi$coef_df_total %>% mutate(type = "metabolites")
metabolites_mi$coef_df_con = rbind(metabolites_mi$coef_df_main, metabolites_mi$coef_df_total)
CV_mi = MI_all_results(CV_outcomes, gee_df = gee_df_CV, out.dir = "./Results/Before_MC", name = "cortical_volume", outlier_ind = outlier_ind_CV, oc = "brain_region")
CV_mi$coef_df_main = CV_mi$coef_df_main %>% mutate(type = "CV") %>% add_column(window = "5-8y", .after = "term")
CV_mi$coef_df_total = CV_mi$coef_df_total %>% mutate(type = "CV")
CV_mi$coef_df_con = rbind(CV_mi$coef_df_main, CV_mi$coef_df_total)
CT_mi = MI_all_results(CT_outcomes, gee_df = gee_df_CT, out.dir = "./Results/Before_MC", name = "cortical_thickness", outlier_ind = outlier_ind_CT, oc = "brain_region")
CT_mi$coef_df_main = CT_mi$coef_df_main %>% mutate(type = "CT") %>% add_column(window = "5-8y", .after = "term")
CT_mi$coef_df_total = CT_mi$coef_df_total %>% mutate(type = "CT")
CT_mi$coef_df_con = rbind(CT_mi$coef_df_main, CT_mi$coef_df_total)
Sub_mi = MI_all_results(Sub_outcomes, gee_df = gee_df_Sub, out.dir = "./Results/Before_MC", name = "subcortical_roi", outlier_ind = outlier_ind_Sub, oc = "brain_region")
Sub_mi$coef_df_main = Sub_mi$coef_df_main %>% mutate(type = "Sub") %>% add_column(window = "5-8y", .after = "term")
Sub_mi$coef_df_total = Sub_mi$coef_df_total %>% mutate(type = "Sub")
Sub_mi$coef_df_con = rbind(Sub_mi$coef_df_main, Sub_mi$coef_df_total)
Neuro_mi = MI_all_results(Neuro_outcomes, gee_df = gee_df_neurobh, out.dir = "./Results/Before_MC", name = "neurobehavior", outlier_ind = outlier_ind_nb, glm_ind = glm_ind_nb)
Neuro_mi$coef_df_main = Neuro_mi$coef_df_main %>% mutate(type = "Neuro") %>% add_column(window = "5-8y", .after = "term")
Neuro_mi$coef_df_total = Neuro_mi$coef_df_total %>% mutate(type = "Neuro")
Neuro_mi$coef_df_con = rbind(Neuro_mi$coef_df_main, Neuro_mi$coef_df_total)
all_mi = rbind(metabolites_mi$coef_df_main, CV_mi$coef_df_main, CT_mi$coef_df_main, Sub_mi$coef_df_main, Neuro_mi$coef_df_main, metabolites_mi$coef_df_total, CV_mi$coef_df_total, CT_mi$coef_df_total, Sub_mi$coef_df_total, Neuro_mi$coef_df_total)

# Quantile g-Computation Model
metabolites_qg = qg_all_results(metabolites_df_name, metabolites_outcomes, out_type = "metabolites", oc = "other", out.dir = "./Results/Before_MC", name = "brain_metabolites") %>% mutate(type = "metabolites")
CV_qg = qg_all_results(CV_df_name, CV_outcomes, out_type = "brain_region", oc = "brain_region", out.dir = "./Results/Before_MC", name = "cortical_volume") %>% mutate(type = "CV")
CT_qg = qg_all_results(CT_df_name, CT_outcomes, out_type = "brain_region", oc = "brain_region", out.dir = "./Results/Before_MC", name = "cortical_thickness") %>% mutate(type = "CT")
Sub_qg = qg_all_results(Sub_df_name, Sub_outcomes, out_type = "brain_region", oc = "brain_region", out.dir = "./Results/Before_MC", name = "subcortical_roi") %>% mutate(type = "Sub")
Neuro_qg = qg_all_results(neurobh_df_name, Neuro_outcomes, out_type = "neurobehavior", glm_ind = glm_ind_nb, oc = "other", out.dir = "./Results/Before_MC", name = "neurobehavior") %>% mutate(type = "Neuro")
all_qg = rbind(metabolites_qg, CV_qg, CT_qg, Sub_qg, Neuro_qg)
all = rbind(all_mi, all_qg)

# Model Connection
## Match by Outcome
metabolites_agreed = agreed_results(metabolites_mi$coef_df_con, metabolites_qg)[[1]]
metabolites_agreed_df = agreed_results(metabolites_mi$coef_df_con, metabolites_qg)[[2]] %>% mutate(type = "metabolites") %>% ungroup()
CV_agreed = agreed_results(CV_mi$coef_df_con, CV_qg)[[1]]
CV_agreed_df = agreed_results(CV_mi$coef_df_con, CV_qg)[[2]] %>% mutate(type = "CV") %>% ungroup()
CT_agreed = agreed_results(CT_mi$coef_df_con, CT_qg)[[1]]
CT_agreed_df = agreed_results(CT_mi$coef_df_con, CT_qg)[[2]] %>% mutate(type = "CT") %>% ungroup()
Sub_agreed = agreed_results(Sub_mi$coef_df_con, Sub_qg)[[1]]
Sub_agreed_df = agreed_results(Sub_mi$coef_df_con, Sub_qg)[[2]] %>% mutate(type = "Sub") %>% ungroup()
Neuro_agreed = agreed_results(Neuro_mi$coef_df_con, Neuro_qg)[[1]]
Neuro_agreed_df = agreed_results(Neuro_mi$coef_df_con, Neuro_qg)[[2]] %>% mutate(type = "Neuro") %>% ungroup()
agreed_df_com = rbind(metabolites_agreed_df, CV_agreed_df, CT_agreed_df, Sub_agreed_df, Neuro_agreed_df)

agree_df_input = agreed_df_com %>% dplyr::select(outcome, type) %>% distinct()
agreed_windows = lapply(1:nrow(agree_df_input), function(i){
  sub_df = agreed_df_com %>% filter(outcome == agree_df_input[[i, "outcome"]], type == agree_df_input[[i, "type"]])
  qg_window = sub_df %>% filter(term == "OPE") %>% pull(window)
  #qg_window[qg_window=="3y"]= "three_year"
  mi_window = sub_df %>% filter(term != "OPE") %>% pull(window)
  int = intersect(qg_window, mi_window)
  if(length(int) > 0){
    lst = data.frame(cbind(agree_df_input[[i, "outcome"]], agree_df_input[[i, "type"]], int))
    colnames(lst) = c("outcome", "type", "window")
  }else{lst = NULL}
  return(lst)
}) %>% bind_rows() %>% left_join(agreed_df_com %>% dplyr::select(outcome, window, type, term), by = c("outcome", "window", "type"))
agreed_windows = agreed_windows %>% left_join(all, by = c("term", "window", "outcome", "type"))
write_csv(agreed_windows, "./Results/model_agreement/agreed_results.csv")

# Create Brain Function Map (move to manuscript code script)
#region = agreed_windows %>% filter(type %in% c("CV", "CT")) %>% pull(outcome) %>% unique()
#anatomy = c("Frontal Lobe", "Occipital Lobe", "Frontal Lobe", "Parietal Lobe", "Frontal Lobe", "Occipital Lobe", "Limbic Lobe", "Insular cortex", "Limbic Lobe", "Frontal Lobe", "Frontal Lobe")
#func = c("Visual", "Visual", "Cognition", "Movement", "Language", "Visual", "Cognition", "Cognition", "Cognition", "Movement", "Cognition")
#brain_df = data.frame(cbind(region, anatomy, func))
#brain_df = agreed_windows %>% filter(type %in% c("CV", "CT")) %>% left_join(brain_df, by = c("outcome" = "region"))
#
#ggplot(brain_df, aes(x = as.factor(window), y = as.numeric(estimate), color = as.factor(anatomy))) +
#  geom_point() +
#  labs(x = "Window", y = "Estimate", color = "Anatomy")
#
#ggplot(brain_df, aes(x = as.factor(window), y = as.numeric(estimate), color = as.factor(func))) +
#  geom_point() +
#  labs(x = "Window", y = "Estimate", color = "Brain Function")

# Mediation
## First-step Mediation Result
sim_files = list.files("./Results/simulation/previous", recursive = TRUE, full.names = TRUE) 
sim_result = lapply(sim_files, function(x) read_csv(x)) %>% bind_rows() %>% add_column(type = c("CV", "CV", "CV"), .after = "mediator") %>% 
  add_column(model = c("mi", "qg", "qg"), .after = "sig") %>% mutate(OPE = case_when(is.na(OPE) ~ "OPE",
                                                                                           .default = toupper(OPE)),
                                                                           ind = c(3, 3, 3)) 


df_map = list("prenatal" = "^pre_df_*",
              "birth" = "^birth_df_*",
              "three_year" = "^three_y_df_*",
              "3y" = "^three_y_df_*",
              "5-8y" = "^five_eight_y_df_*",
              "8y" = "^eight_y_df_*")
df_names = c("metabolites_df_name", "CT_df_name", "CV_df_name", "neurobh_df_name", "Sub_df_name")
### Adjust CI from 90% to 95%
sim_result_update = mclapply(1:nrow(sim_result), function(i){
  m_window = sim_result[[i, "window"]]
  pattern = df_map[[m_window]]
  ind = sim_result[[i, "ind"]]
  df1 = eval(parse(text = eval(parse(text = df_names[ind]))[which(grepl(pattern, eval(parse(text = df_names[ind]))))]))
  df2 = eval(parse(text = eval(parse(text = df_names[4]))[which(grepl(pattern, eval(parse(text = df_names[4]))))]))
  y = sim_result[[i, "y"]]
  m = sim_result[[i, "mediator"]]
  ope = tolower(sim_result[[i, "OPE"]])
  model = sim_result[[i, "model"]]
  if(model == "mi"){return(mi_mediate_gen(df1, df2, y, m, type = "brain_region", 10000, window = m_window, ope))}else{
    return(qg_mediate_gen(df1, df2, y, m, type = "brain_region", 10000, window = m_window))
  }
}, mc.cores = detectCores()) %>% bind_rows() %>% dplyr::select(indirect_effect_CI, sig, total_effect_CI)
sim_result = sim_result %>% dplyr::select(y, mediator, type, OPE, window, model, ind) %>% cbind(sim_result_update)
#sim_result$sig = sapply(1:nrow(sim_result), function(i){
#  a = as.numeric(gsub("\\(", "", str_split(sim_result$CI[i], ",")[[1]][1]))
#  b = as.numeric(gsub("\\)", "", str_split(sim_result$CI[i], ",")[[1]][2]))
#  if(a*b>0){return("*")}else{return(NA)}
#}, USE.NAMES = FALSE)
sim_result = sim_result %>% filter(!is.na(sig))

med_result = lapply(1:nrow(sim_result), function(i){
  m_window = sim_result[[i, "window"]]
  pattern = df_map[[m_window]]
  ind = sim_result[[i, "ind"]]
  df1 = eval(parse(text = eval(parse(text = df_names[ind]))[which(grepl(pattern, eval(parse(text = df_names[ind]))))]))
  df2 = eval(parse(text = eval(parse(text = df_names[4]))[which(grepl(pattern, eval(parse(text = df_names[4]))))]))
  y = sim_result[[i, "y"]]
  m = sim_result[[i, "mediator"]]
  ope = tolower(sim_result[[i, "OPE"]])
  model = sim_result[[i, "model"]]
  mediator_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$mediator
  mediator_effect_CI = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$mediator_CI
  indirect_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$indirect
  direct_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$direct
  direct_effect_CI = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$direct_CI
  total_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$total
  
  med_df = data.frame(cbind(mediator_effect, mediator_effect_CI, indirect_effect, direct_effect, direct_effect_CI, total_effect))
  return(med_df)
}) %>% bind_rows()

sim_result = cbind(sim_result, med_result)
sim_result = sim_result[c("y", "mediator", "type", "OPE", "window", "model", "indirect_effect", "indirect_effect_CI", "mediator_effect", "mediator_effect_CI", "direct_effect", "direct_effect_CI", "total_effect", "total_effect_CI")]

sim_result$agreed = sapply(1:nrow(sim_result), function(i){
  if(sim_result[[i, "y"]] %in% unique(agreed_windows$outcome)){
    return("agreed")
  }else{return("not-agreed")}
}, USE.NAMES = FALSE)

write_csv(sim_result, "./Results/mediation/mediation_result_pre.csv")

## Post-hoc Mediation
sim_files = list.files("./Results/simulation/post", recursive = TRUE, full.names = TRUE) 
sim_result = lapply(sim_files, function(x) read_csv(x)) %>% bind_rows() %>% na.omit() %>% add_column(type = c("CV", "CV", "CV"), .after = "mediator") %>% 
  add_column(model = c("mi", "qg", "qg"), .after = "sig") %>% mutate(OPE = case_when(OPE == "OPE" ~ "OPE",
                                                                                     .default = toupper(OPE)),
                                                                     ind = c(3, 3, 3)) 

df_names = c("metabolites_df_name", "CT_df_name", "CV_rh_name", "neurobh_df_name", "Sub_df_name")

### Adjust CI from 90% to 95%
sim_result_update = mclapply(1:nrow(sim_result), function(i){
  m_window = sim_result[[i, "window"]]
  pattern = df_map[[m_window]]
  ind = sim_result[[i, "ind"]]
  df1 = eval(parse(text = eval(parse(text = df_names[ind]))[which(grepl(pattern, eval(parse(text = df_names[ind]))))]))
  df2 = eval(parse(text = eval(parse(text = df_names[4]))[which(grepl(pattern, eval(parse(text = df_names[4]))))]))
  y = sim_result[[i, "y"]]
  m = sim_result[[i, "mediator"]]
  ope = tolower(sim_result[[i, "OPE"]])
  model = sim_result[[i, "model"]]
  if(model == "mi"){return(mi_mediate_gen(df1, df2, y, m, type = "brain_region", 10000, window = m_window, ope))}else{
    return(qg_mediate_gen(df1, df2, y, m, type = "brain_region", 10000, window = m_window))
  }
}, mc.cores = detectCores()) %>% bind_rows() %>% dplyr::select(indirect_effect_CI, sig, total_effect_CI)
sim_result = sim_result %>% dplyr::select(y, mediator, type, OPE, window, model, ind) %>% cbind(sim_result_update)
sim_result = sim_result %>% filter(!is.na(sig))

med_result = lapply(1:nrow(sim_result), function(i){
  m_window = sim_result[[i, "window"]]
  pattern = df_map[[m_window]]
  ind = sim_result[[i, "ind"]]
  df1 = eval(parse(text = eval(parse(text = df_names[ind]))[which(grepl(pattern, eval(parse(text = df_names[ind]))))]))
  df2 = eval(parse(text = eval(parse(text = df_names[4]))[which(grepl(pattern, eval(parse(text = df_names[4]))))]))
  y = sim_result[[i, "y"]]
  m = sim_result[[i, "mediator"]]
  ope = tolower(sim_result[[i, "OPE"]])
  model = sim_result[[i, "model"]]
  mediator_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$mediator
  mediator_effect_CI = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$mediator_CI
  indirect_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$indirect
  direct_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$direct
  direct_effect_CI = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$direct_CI
  total_effect = mediation_analysis(df1, df2, y = y, m = m, type = "brain_region", model = model, ope = ope)$total
  
  med_df = data.frame(cbind(mediator_effect, mediator_effect_CI, indirect_effect, direct_effect, direct_effect_CI, total_effect))
  return(med_df)
}) %>% bind_rows()

sim_result = cbind(sim_result, med_result)
sim_result = sim_result[c("y", "mediator", "type", "OPE", "window", "model", "indirect_effect", "indirect_effect_CI", "mediator_effect", "mediator_effect_CI", "direct_effect", "direct_effect_CI", "total_effect", "total_effect_CI")]

write_csv(sim_result, "./Results/mediation/mediation_result_post.csv")

