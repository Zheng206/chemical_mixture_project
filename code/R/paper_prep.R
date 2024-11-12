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
library(gridExtra)
source("./code/R/useful_function.R")

# Data Check
dv = read_csv("./Data/OPEs and Covariates/opfr.csv")
cov_df = read_csv("./Data/OPEs and Covariates/covariates_full.csv")
cov_col = c("sex", "race", "home_score_total", "mom_edu")
cov_df = cov_df %>% dplyr::select(1,cov_col) %>% na.omit()
cov_df$race = sapply(cov_df$race, function(x){if (x %in% c("Asian, Pacific Islander, or Native Hawaiian", "Other/Multiracial", "Black or African American")){return("Non-white")}else{return(x)}}, USE.NAMES = FALSE)
cov_df$mom_edu = sapply(cov_df$mom_edu, function(x){if(x %in% c("8th grade or less", "Some high School (Grades 9, 10, 11, 12)", "Technical or trade school", "High school diploma (completed grade 12)")){return("Before college")}else if (x %in% c("GED", "Graduate or professional school")){return("Graduate or professional school")}else{return(x)}}, USE.NAMES = FALSE)
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
cotinine = cotinine %>% distinct() %>% na.omit()

# Outcome df
metabolites_df = read_excel("./Data/MRS/HOME_Study_MRS_Concentrations_AiminPenn_DeID.xlsx") %>% na.omit()
metabolites_id = unique(metabolites_df$SubjectID)
aparc_lh_CV = read_csv("./Data/Volume/aparc_lh_volume.csv") %>% na.omit()
brain_id = unique(aparc_lh_CV$SubjectID)
data = read_sas("./Data/neurobehavior/outcomes_add.sas7bdat")
Neuro_outcomes = c("ChAMP_Index_Verbal", "ChAMP_Index_Visual", "ChAMP_Index_Immediate", "ChAMP_Index_Delayed", "ChAMP_Index_Total_Memory",
                   "Pegboard_DH_Seconds", "Pegboard_DH_Drops", "Pegboard_NDH_Seconds", "Pegboard_NDH_Drops", 
                   "BRIEF_Child_BRI", "BRIEF_Child_ERI", "BRIEF_Child_CRI", "BRIEF_Child_GEC", 
                   "BASC_Child_FII_GC_T", "BASC_Child_ESI_GC_T", "BASC_Child_IHI_GC_T", "BASC_Child_INZ_GC_T", "BASC_Child_PAI_GC_T", "BASC_Child_SPI_GC_T")
neurobh = data[c("SubjectID", Neuro_outcomes)] %>% na.omit()
neurobh_id = unique(neurobh$SubjectID)
cov_id = cov_df %>% pull(subject_id) %>% unique()

## Separate neurobehavior test type
neurobh_champ_id = data[c("SubjectID", "ChAMP_Index_Verbal", "ChAMP_Index_Visual", "ChAMP_Index_Immediate", "ChAMP_Index_Delayed", "ChAMP_Index_Total_Memory")] %>% na.omit() %>% pull(SubjectID)
neurobh_pegboard_id = data[c("SubjectID", "Pegboard_DH_Seconds", "Pegboard_DH_Drops", "Pegboard_NDH_Seconds", "Pegboard_NDH_Drops")] %>% na.omit() %>% pull(SubjectID)
neurobh_brief_id = data[c("SubjectID", "BRIEF_Child_BRI", "BRIEF_Child_ERI", "BRIEF_Child_CRI", "BRIEF_Child_GEC")] %>% na.omit() %>% pull(SubjectID)
neurobh_basc_id = data[c("SubjectID", "BASC_Child_FII_GC_T", "BASC_Child_ESI_GC_T", "BASC_Child_IHI_GC_T", "BASC_Child_INZ_GC_T", "BASC_Child_PAI_GC_T", "BASC_Child_SPI_GC_T")] %>% na.omit() %>% pull(SubjectID)

## demographic table
subid_meta = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(metabolites_df$SubjectID)), unique(neurobh$SubjectID))
subid_meta_champ = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(metabolites_df$SubjectID)), neurobh_champ_id)
subid_meta_pegboard = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(metabolites_df$SubjectID)), neurobh_pegboard_id)
subid_meta_brief = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(metabolites_df$SubjectID)), neurobh_brief_id)
subid_meta_basc = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(metabolites_df$SubjectID)), neurobh_basc_id)

subid_brain = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), unique(neurobh$SubjectID))
subid_brain_champ = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), neurobh_champ_id)
subid_brain_pegboard = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), neurobh_pegboard_id)
subid_brain_brief = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), neurobh_brief_id)
subid_brain_basc = intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), neurobh_basc_id)


subid_combine = intersect(intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), unique(metabolites_df$SubjectID)), unique(neurobh$SubjectID))
subid_combine_champ = intersect(intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), unique(metabolites_df$SubjectID)), neurobh_champ_id)
subid_combine_pegboard = intersect(intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), unique(metabolites_df$SubjectID)), neurobh_pegboard_id)
subid_combine_brief = intersect(intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), unique(metabolites_df$SubjectID)), neurobh_brief_id)
subid_combine_basc = intersect(intersect(intersect(intersect(unique(cov_df$subject_id), unique(cotinine$subject_id)), unique(aparc_lh_CV$SubjectID)), unique(metabolites_df$SubjectID)), neurobh_basc_id)




ids = list(metabolites_id, brain_id, neurobh_id, neurobh_champ_id, neurobh_pegboard_id, neurobh_brief_id, neurobh_basc_id, 
           subid_meta, subid_meta_champ, subid_meta_pegboard, subid_meta_brief, subid_meta_basc,
           subid_brain, subid_brain_champ, subid_brain_pegboard, subid_brain_brief, subid_brain_basc,
           subid_combine, subid_combine_champ, subid_combine_pegboard, subid_combine_brief, subid_combine_basc)

demo_gen = function(id, cov_df, cotinine){
  sub_cov = cov_df %>% filter(subject_id %in% id) 
  n = nrow(sub_cov)
  child_sex = NA
  male = paste0(table(sub_cov$sex)[["Male"]], "(", round(100*table(sub_cov$sex)[["Male"]]/length(sub_cov$sex)), ")")
  female = paste0(table(sub_cov$sex)[["Female"]], "(", round(100*table(sub_cov$sex)[["Female"]]/length(sub_cov$sex)), ")")
  race = NA
  non_white = paste0(table(sub_cov$race)[["Non-white"]], "(", round(100*table(sub_cov$race)[["Non-white"]]/length(sub_cov$race)), ")")
  white = paste0(table(sub_cov$race)[["White or Caucasian"]], "(", round(100*table(sub_cov$race)[["White or Caucasian"]]/length(sub_cov$race)), ")")
  homescore = paste0(round(mean(sub_cov$home_score_total), 2), "(", round(sd(sub_cov$home_score_total), 2), ")")
  mom_edu = NA
  bachelor = paste0(table(sub_cov$mom_edu)[["Bachelor's degree"]], "(", round(100*table(sub_cov$mom_edu)[["Bachelor's degree"]]/length(sub_cov$mom_edu)), ")")
  before_college = paste0(table(sub_cov$mom_edu)[["Before college"]], "(", round(100*table(sub_cov$mom_edu)[["Before college"]]/length(sub_cov$mom_edu)), ")")
  graduate = paste0(table(sub_cov$mom_edu)[["Graduate or professional school"]], "(", round(100*table(sub_cov$mom_edu)[["Graduate or professional school"]]/length(sub_cov$mom_edu)), ")")
  some_college = paste0(table(sub_cov$mom_edu)[["Some college or 2 year degree"]], "(", round(100*table(sub_cov$mom_edu)[["Some college or 2 year degree"]]/length(sub_cov$mom_edu)), ")")
  demo_df = data.frame(rbind(n, child_sex, male, female, race, non_white, white, homescore, mom_edu, before_college, some_college, bachelor, graduate))
  return(demo_df)
}

demo_df = lapply(1:length(ids), function(i){
  sub_df = demo_gen(ids[[i]], cov_df, cotinine)
  }) %>% bind_cols()
colnames(demo_df) = c("metabolites", "brain", "neurobehavior", "neurobehavior_champ", "neurobehavior_pegboard", "neurobehavior_brief", "neurobehavior_basc",
                      "metabolites_mediation", "metabolites_mediation_champ", "metabolites_mediation_pegboard", "metabolites_mediation_brief", "metabolites_mediation_basc",
                      "brain_mediation", "brain_mediation_champ", "brain_mediation_pegboard", "brain_mediation_brief", "brain_mediation_basc",
                      "combine_mediation", "combine_mediation_champ", "combine_mediation_pegboard", "combine_mediation_brief", "combine_mediation_basc")
demo_df = demo_df %>% add_column(Variable = rownames(demo_df), .before = "metabolites")
write_csv(demo_df, "./Results/demographic/demo_table.csv")

## Missing data investigation 
meta_id_df = data.frame(cbind(metabolites_id, rep("MRS", length(metabolites_id))))
colnames(meta_id_df) = c("subject_id", "subset_MRS")
brain_id_df = data.frame(cbind(brain_id, rep("Morphometric", length(brain_id))))
colnames(brain_id_df) = c("subject_id", "subset_Morphometric")
neurobh_champ_id_df = data.frame(cbind(neurobh_champ_id, rep("Neurobehavior_ChAMP", length(neurobh_champ_id))))
colnames(neurobh_champ_id_df) = c("subject_id", "subset_Neurobehavior_ChAMP")
neurobh_pegboard_id_df = data.frame(cbind(neurobh_pegboard_id, rep("Neurobehavior_Pegboard", length(neurobh_pegboard_id))))
colnames(neurobh_pegboard_id_df) = c("subject_id", "subset_Neurobehavior_Pegboard")
neurobh_brief_id_df = data.frame(cbind(neurobh_brief_id, rep("Neurobehavior_BRIEF", length(neurobh_brief_id))))
colnames(neurobh_brief_id_df) = c("subject_id", "subset_Neurobehavior_BRIEF")
neurobh_basc_id_df = data.frame(cbind(neurobh_basc_id, rep("Neurobehavior_BASC", length(neurobh_basc_id))))
colnames(neurobh_basc_id_df) = c("subject_id", "subset_Neurobehavior_BASC")
cov_id_df = data.frame(cbind(cov_id, rep("Covariates", length(cov_id))))
colnames(cov_id_df) = c("subject_id", "subset_Covariates")
subset_df = meta_id_df %>% full_join(brain_id_df, by = "subject_id") %>% full_join(neurobh_champ_id_df, by = "subject_id") %>% full_join(neurobh_pegboard_id_df, by = "subject_id") %>% full_join(neurobh_brief_id_df, by = "subject_id") %>% 
  full_join(neurobh_basc_id_df, by = "subject_id") %>% left_join(cov_id_df, by = "subject_id")
write_csv(subset_df, "./Results/demographic/subset_id.csv")

# Sensitive Analysis with Covariate interpolation
library(nnet)
used_id = unique(c(metabolites_id, brain_id, neurobh_champ_id, neurobh_pegboard_id, neurobh_brief_id, neurobh_basc_id))
cov_df = read_csv("./Data/OPEs and Covariates/covariates_full.csv")
cov_col = c("sex", "race", "home_score_total", "mom_edu")
cov_df = cov_df %>% dplyr::select(1,cov_col) %>% filter(subject_id %in% used_id)
miss_df = cov_df %>% filter(is.na(home_score_total) | is.na(mom_edu))
# Use regression model to do interpolation
score_model = lm(home_score_total ~ sex + race, data = cov_df)
miss_df$home_score_total = sapply(1:nrow(miss_df), function(i) {
  if(is.na(miss_df$home_score_total[i])) {predict(score_model, newdata = miss_df[i,])}else{miss_df$home_score_total[i]}
}, USE.NAMES = FALSE)
edu_model = multinom(mom_edu ~ sex + race + home_score_total, data = cov_df)
miss_df$mom_edu = sapply(1:nrow(miss_df), function(i) {
  if(is.na(miss_df$mom_edu[i])) {colnames(edu_model$fitted.values)[as.numeric(predict(edu_model, newdata = miss_df[i,]))]}else{miss_df$mom_edu[i]}
}, USE.NAMES = FALSE)
cov_df = rbind(cov_df %>% na.omit(), miss_df)
write_csv(cov_df, "./Results/sensitivity_analysis/cov_df.csv")
###############################################################################################################################

# Visualization of results
## Step 1 Exploratory Analysis
### MI
ex = c("third_Ventricle", "Left_vessel", "Optic_Chiasm", "Left_Inf_Lat_Vent", "Lateral_Ventricle", "CSF")
mi_files = list.files("./Results/Before_MC", pattern = "*_main.csv|*_total.csv", recursive = TRUE, full.names = TRUE)
mi_sen_files = list.files("./Results/sensitivity_analysis/Before_MC", pattern = "*_main.csv|*_total.csv", recursive = TRUE, full.names = TRUE)
mi_df = lapply(mi_files, function(x){
  sub_df = read_csv(x)
  if(!"window" %in% colnames(sub_df)){
    sub_df = sub_df %>% mutate(window = "5-8y")
  }
  sub_df = sub_df[c("term", "window", "estimate", "p.value", "outcome")]
  return(sub_df)
  }) %>% bind_rows() %>% filter(!outcome %in% ex)
mi_df$term = sapply(mi_df$term, function(x){
  if(x == "BCEtP"){return("BCEP")}else if(x == "BDCPP"){
    return("BDCIPP")}else if(x == "DBuP"){
      return("DNBP")}else if(x == "DPHP"){return(x)}
}, USE.NAMES = FALSE)
mi_sen_df = lapply(mi_sen_files, function(x){
  sub_df = read_csv(x)
  if(!"window" %in% colnames(sub_df)){
    sub_df = sub_df %>% mutate(window = "5-8y")
  }
  sub_df = sub_df[c("term", "window", "estimate", "p.value", "outcome")]
  return(sub_df)
}) %>% bind_rows() %>% filter(!outcome %in% ex)

mi_sen_df$term = sapply(mi_sen_df$term, function(x){
  if(x == "BCEtP"){return("BCEP")}else if(x == "BDCPP"){
    return("BDCIPP")}else if(x == "DBuP"){
      return("DNBP")}else if(x == "DPHP"){return(x)}
}, USE.NAMES = FALSE)

mi_count = mi_df %>% filter(p.value < 0.1) %>% group_by(window) %>% summarize(count = n()) %>% mutate(window = factor(window, levels = c("prenatal", "birth", "3y", "5-8y")))
ggplot(mi_count, aes(x = window, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = 1.7, color = "white") + 
  labs(x = "Window", y = "Number of Significant Results", title = "Multiple Informant") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

mi_sen_count = mi_sen_df %>% filter(p.value < 0.1) %>% group_by(window) %>% summarize(count = n()) %>% mutate(window = factor(window, levels = c("prenatal", "birth", "3y", "5-8y")))
ggplot(mi_sen_count, aes(x = window, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = 1.7, color = "white") + 
  labs(x = "Window", y = "Number of Significant Results", title = "Multiple Informant") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

mi_heat = mi_df %>% filter(p.value < 0.1) %>% dplyr::select(term, window) %>% table()
col = colorRampPalette(brewer.pal(8, "Blues"))(25)
colnames(mi_heat)[colnames(mi_heat) == "prenatal"] = "pre"
col_reorder = c("pre", "birth", "3y", "5-8y")
mi_heat = mi_heat[, col_reorder]
heatmap(mi_heat, Colv = NA, Rowv = NA, col = col, scale = "none", add.expr = {
  for (i in 1:nrow(mi_heat)) {
    for (j in 1:ncol(mi_heat)) {
      text(j, i, mi_heat[i, j], cex = 1.5)
    }
  }
})

mi_sen_heat = mi_sen_df %>% filter(p.value < 0.1) %>% dplyr::select(term, window) %>% table()
col = colorRampPalette(brewer.pal(8, "Blues"))(25)
colnames(mi_sen_heat)[colnames(mi_sen_heat) == "prenatal"] = "pre"
mi_sen_heat = mi_sen_heat[, col_reorder]
heatmap(mi_sen_heat, Colv = NA, Rowv = NA, col = col, scale = "none", add.expr = {
  for (i in 1:nrow(mi_heat)) {
    for (j in 1:ncol(mi_heat)) {
      text(j, i, mi_heat[i, j], cex = 1.5)
    }
  }
})

### QG
qg_files = list.files("./Results/Before_MC", pattern = "*_combined_effect.csv", recursive = TRUE, full.names = TRUE)
qg_sen_files = list.files("./Results/sensitivity_analysis/Before_MC", pattern = "*_combined_effect.csv", recursive = TRUE, full.names = TRUE)
qg_df = lapply(qg_files, function(x) read_csv(x)) %>% bind_rows() %>% filter(!outcome %in% ex)
qg_sen_df = lapply(qg_sen_files, function(x) read_csv(x)) %>% bind_rows() %>% filter(!outcome %in% ex)

qg_count = qg_df %>% filter(p.value < 0.1, window != "8y") %>% group_by(window) %>% summarize(count = n()) %>% mutate(window = factor(window, levels = c("prenatal", "birth", "3y", "5-8y")))
ggplot(qg_count, aes(x = window, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = 1.7, color = "white") + 
  labs(x = "Window", y = "Number of Significant Results", title = "Quantile g-computation") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

qg_sen_count = qg_sen_df %>% filter(p.value < 0.1, window != "8y") %>% group_by(window) %>% summarize(count = n()) %>% mutate(window = factor(window, levels = c("prenatal", "birth", "3y", "5-8y")))
ggplot(qg_sen_count, aes(x = window, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = 1.7, color = "white") + 
  labs(x = "Window", y = "Number of Significant Results", title = "Quantile g-computation") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

## Step 2 Model Agreement
agreed_df = read_csv("./Results/model_agreement/agreed_results.csv") %>% filter(!outcome %in% ex)
agreed_df = agreed_df %>% mutate(type = case_when(type == "metabolites" ~ "Neurochem", 
                                                  type == "Neuro" ~ "Behavior",
                                                  .default = type))
agreed_df$window = factor(agreed_df$window, levels = c("prenatal", "birth", "3y", "5-8y"))
agreed_df$term = sapply(agreed_df$term, function(x){
  if(x == "BCEtP"){return("BCEP")}else if(x == "BDCPP"){
    return("BDCIPP")}else if(x == "DBuP"){
      return("DNBP")}else{return(x)}
}, USE.NAMES = FALSE)
agreed_sen_df = read_csv("./Results/sensitivity_analysis/model_agreement/agreed_results.csv") %>% filter(!outcome %in% ex)
agreed_sen_df = agreed_sen_df %>% mutate(type = case_when(type == "metabolites" ~ "Neurochem", 
                                                          type == "Neuro" ~ "Behavior",
                                                          .default = type))
agreed_sen_df$window = factor(agreed_sen_df$window, levels = c("prenatal", "birth", "3y", "5-8y"))
agreed_sen_df$term = sapply(agreed_sen_df$term, function(x){
  if(x == "BCEtP"){return("BCEP")}else if(x == "BDCPP"){
    return("BDCIPP")}else if(x == "DBuP"){
      return("DNBP")}else{return(x)}
}, USE.NAMES = FALSE)
agreed_count = agreed_df %>% filter(term == "OPE") %>% group_by(window, type) %>% summarize(count = n()) %>% mutate(window = factor(window, levels = c("prenatal", "birth", "3y", "5-8y"))) 
agreed_sen_count = agreed_sen_df %>% filter(term == "OPE") %>% group_by(window, type) %>% summarize(count = n()) %>% mutate(window = factor(window, levels = c("prenatal", "birth", "3y", "5-8y"))) 
agreed_name_df = agreed_df %>% filter(term == "OPE") %>% dplyr::select(outcome, window)
agreed_name_sen_df = agreed_sen_df %>% filter(term == "OPE") %>% dplyr::select(outcome, window)
overlap = agreed_name_df %>% semi_join(agreed_name_sen_df)
ggplot(agreed_count, aes(x = window, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black", size = 3) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Window", y = "Number of Significant Outcomes", fill = "Type", title = "Model Agreement") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

agreed_sen_count = agreed_sen_df %>% filter(term == "OPE") %>% group_by(window, type) %>% summarize(count = n()) %>% mutate(window = factor(window, levels = c("prenatal", "birth", "3y", "5-8y")))
ggplot(agreed_sen_count, aes(x = window, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black", size = 3) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Window", y = "Number of Significant Outcomes", fill = "Type", title = "Model Agreement") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

### Visualization of Estimates and CIs with agreed results
my_colors <- c("BCEP" = "coral", "BDCIPP" = "springgreen", "DPHP" = "cyan3", "DNBP" = "purple", "OPE" = "black")

#### All

p = ggplot(agreed_df, aes(x = outcome, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Outcome", y = "Estimate", color = "OPE Type") +
  facet_wrap( type ~ window, scales = "free", ncol = 3) +
  #theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  scale_color_manual(values = my_colors) +
  #facet_wrap(~type, scales = "free", nrow = 5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title.x = element_text(face = "bold", size = 11),
        axis.title.y = element_text(face = "bold", size = 11))
ggsave("/Users/zhengren/Desktop/PennSIVE/Project/chemical_mixture/paper/figure/manuscript/figure 6/coefficients.png", plot = p, width = 21, height = 15, units = "in")

p_sen = ggplot(agreed_sen_df, aes(x = outcome, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Outcome", y = "Estimate", color = "OPE Type") +
  facet_wrap( type ~ window, scales = "free", ncol = 3) +
  #theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  scale_color_manual(values = my_colors) +
  #facet_wrap(~type, scales = "free", nrow = 5) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title.x = element_text(face = "bold", size = 11),
        axis.title.y = element_text(face = "bold", size = 11))
ggsave("/Users/zhengren/Desktop/PennSIVE/Project/chemical_mixture/paper/figure/supplementary/figure 4/coefficients_sen.png", plot = p_sen, width = 21, height = 15, units = "in")

### Grouping ROIs into Anatomy and Functional Region
region = agreed_df %>% filter(type %in% c("CV", "CT")) %>% pull(outcome) %>% unique()
anatomy = c("Frontal Lobe", "Occipital Lobe", "Frontal Lobe", "Parietal Lobe", "Frontal Lobe", "Occipital Lobe", "Limbic Lobe", "Insular Cortex", "Limbic Lobe", "Frontal Lobe", "Frontal Lobe")
func = c("Motor", "Vision", "Cognition", "Motor", "Language", "Vision", "Cognition", "Cognition", "Cognition", "Motor", "Cognition")
brain_df = data.frame(cbind(region, anatomy, func))
brain_df = agreed_df %>% filter(type %in% c("CV", "CT")) %>% left_join(brain_df, by = c("outcome" = "region"))


colors_ant <- c("Frontal Lobe" = "coral", "Occipital Lobe" = "cyan2", "Parietal Lobe" = "chartreuse", "Limbic Lobe" = "blueviolet", "Insular Cortex" = "cornflowerblue")
shapes_ant <- c("Frontal Lobe" = 15, "Occipital Lobe" = 16, "Parietal Lobe" = 17, "Limbic Lobe" = 4, "Insular Cortex" = 3)
colors_fun <- c("Motor" = "coral", "Vision" = "cyan2", "Cognition" = "chartreuse", "Language" = "blueviolet")
shapes_fun <- c("Motor" = 15, "Vision" = 16, "Cognition" = 17, "Language" = 4)
ggplot(brain_df, aes(x = as.factor(window), y = as.numeric(estimate), color = as.factor(anatomy), shape = as.factor(anatomy))) +
  geom_point(position = position_dodge(width = 0.4)) +
  labs(x = "Window", y = "OPE's Effect on Brain Anatomical Regions (mm³)", color = "Anatomical Region", shape = "Anatomical Region") +
  scale_color_manual(values = colors_ant) +
  scale_shape_manual(values = shapes_ant) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))
  

ggplot(brain_df, aes(x = as.factor(window), y = as.numeric(estimate), color = func, shape = func)) +
  geom_point(position = position_dodge(width = 0.4)) +
  labs(x = "Window", y = "OPE's Effect on Brain Functional Regions (mm³)", color = "Functional Region", shape = "Functional Region") +
  scale_color_manual(values = colors_fun) +
  scale_shape_manual(values = shapes_fun) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

region = agreed_sen_df %>% filter(type %in% c("CV", "CT")) %>% pull(outcome) %>% unique()
anatomy = c("Frontal Lobe", "Parietal Lobe", "Frontal Lobe", "Frontal Lobe", "Limbic Lobe", "Temporal Lobe", "Limbic Lobe", "Frontal Lobe")
func = c("Cognition", "Motor", "Language", "Motor", "Cognition", "Auditory", "Cognition", "Cognition")
brain_sen_df = data.frame(cbind(region, anatomy, func))
brain_sen_df = agreed_sen_df %>% filter(type %in% c("CV", "CT")) %>% left_join(brain_sen_df, by = c("outcome" = "region"))

colors_ant <- c("Frontal Lobe" = "coral", "Occipital Lobe" = "cyan3", "Parietal Lobe" = "chartreuse", "Limbic Lobe" = "blueviolet", "Insular Cortex" = "cornflowerblue", "Temporal Lobe" = "pink")
shapes_ant <- c("Frontal Lobe" = 15, "Occipital Lobe" = 16, "Parietal Lobe" = 17, "Limbic Lobe" = 4, "Insular Cortex" = 3, "Temporal Lobe" = 8)
colors_fun <- c("Motor" = "coral", "Vision" = "cyan3", "Cognition" = "chartreuse", "Language" = "blueviolet", "Auditory" = "pink")
shapes_fun <- c("Motor" = 15, "Vision" = 16, "Cognition" = 17, "Language" = 4, "Auditory" = 3)
ggplot(brain_sen_df, aes(x = as.factor(window), y = as.numeric(estimate), color = as.factor(anatomy), shape = as.factor(anatomy))) +
  geom_point(position = position_dodge(width = 0.4)) +
  labs(x = "Window", y = "OPE's Effect on Brain Anatomical Regions (mm³)", color = "Anatomical Region", shape = "Anatomical Region") +
  scale_color_manual(values = colors_ant) +
  scale_shape_manual(values = shapes_ant) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

ggplot(brain_sen_df, aes(x = as.factor(window), y = as.numeric(estimate), color = as.factor(func), shape = as.factor(func))) +
  geom_point(position = position_dodge(width = 0.4)) +
  labs(x = "Window", y = "OPE's Effect on Brain Functional Regions (mm³)", color = "Functional Region", shape = "Functional Region") +
  scale_color_manual(values = colors_fun) +
  scale_shape_manual(values = shapes_fun) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

## Mediation Coefficients Interpretation
md_result = read_csv("./Results/mediation/mediation_result_pre.csv")
md_result_post = read_csv("./Results/mediation/mediation_result_post.csv")

mediation_inter = function(i, md_result){
  type = md_result[[i, "type"]]; model = md_result[[i, "model"]]
  m = md_result[[i, "mediator"]]; ope = md_result[[i, "OPE"]]; outcome = md_result[[i, "y"]]
  unit = ifelse(type == "CV", "volume", "thickness")
  if(model == "qg"){
    qg_mediator = md_result[[i, "mediator_effect"]]
    qg_mediator_CI = md_result[[i, "mediator_effect_CI"]]
    change = ifelse(qg_mediator > 0,  "increase", "decrease")
    qg_indirect = 100 * (exp(md_result[[i, "indirect_effect"]]) - 1)
    qg_indirect_CI = paste0("(", 100 * (exp(as.numeric(gsub("\\(", "", str_split(md_result[[i, "indirect_effect_CI"]], ",")[[1]][1]))) - 1), " , ", 100 * (exp(as.numeric(gsub("\\)", "", str_split(md_result[[i, "indirect_effect_CI"]], ",")[[1]][2]))) - 1), ")")
    change_ind = ifelse(qg_indirect > 0,  "increase", "decrease")
    print(paste0("One quantile increases of OPEs were associated with a ", sprintf("%.3f", abs(qg_mediator)), " mm3 ", change, " ", qg_mediator_CI, " of ", m, " ", unit, ", which consequently contributed to a ", sprintf("%.2f", abs(qg_indirect)), "% ",
                 change_ind, " ", qg_indirect_CI, " of ", outcome))
  }else{
    mi_mediator = md_result[[i, "mediator_effect"]]*log(1.1) # 10 percent increase of ope
    mi_mediator_CI = paste0("(", as.numeric(gsub("\\(", "", str_split(md_result[[i, "mediator_effect_CI"]], ",")[[1]][1]))*log(1.1), " , ", as.numeric(gsub("\\)", "", str_split(md_result[[i, "mediator_effect_CI"]], ",")[[1]][2]))*log(1.1), ")")
    change = ifelse(mi_mediator > 0,  "increase", "decrease")
    mi_indirect = 100 * (exp(md_result[[i, "indirect_effect"]]*log(1.1)) - 1)
    mi_indirect_CI = paste0("(", 100 * (exp(as.numeric(gsub("\\(", "", str_split(md_result[[i, "indirect_effect_CI"]], ",")[[1]][1]))*log(1.1)) - 1), " , ", 100 * (exp(as.numeric(gsub("\\)", "", str_split(md_result[[i, "indirect_effect_CI"]], ",")[[1]][2]))*log(1.1)) - 1), ")")
    change_ind = ifelse(mi_indirect > 0,  "increase", "decrease")
    print(paste0("Ten percent increases of ", ope, " was associated with a ", sprintf("%.3f", abs(mi_mediator)), " mm3 ", change, " ", mi_mediator_CI, " of ", m, " ", unit, ", which consequently contributed to a ", sprintf("%.2f", abs(mi_indirect)), "% ",
                 change_ind, " ", mi_indirect_CI, " of ", outcome))
  }
}

mediation_inter(1, md_result)
mediation_inter(2, md_result)

mediation_inter(1, md_result_post)
mediation_inter(2, md_result_post)

unit_change = function(result){
  result$indirect_effect = sapply(1:nrow(result), function(i){
    if(result$model[i] == "mi"){return(paste0(round(100 * (exp(result$indirect_effect[i]*log(1.1)) - 1), 2), "%"))}else{return(paste0(round(100 * (exp(result$indirect_effect[i]) - 1), 2), "%"))}
  }, USE.NAMES = FALSE)
  result$indirect_effect_CI = sapply(1:nrow(result), function(i){
    a = as.numeric(gsub("\\(", "", str_split(result$indirect_effect_CI[i], ",")[[1]][1]))
    b = as.numeric(gsub("\\)", "", str_split(result$indirect_effect_CI[i], ",")[[1]][2]))
    if(result$model[i] == "mi"){
      p = paste0("(", round(100 * (exp(a*log(1.1)) - 1), 2), "% , ", round(100 * (exp(b*log(1.1)) - 1), 2), "%)")
      return(p)}else{
        p = paste0("(", round(100 * (exp(a) - 1), 2), "% , ", round(100 * (exp(b) - 1), 2), "%)")
        return(p)}
  }, USE.NAMES = FALSE)
  result$direct_effect = sapply(1:nrow(result), function(i){
    if(result$model[i] == "mi"){return(paste0(round(100 * (exp(result$direct_effect[i]*log(1.1)) - 1), 2), "%"))}else{return(paste0(round(100 * (exp(result$direct_effect[i]) - 1), 2), "%"))}
  }, USE.NAMES = FALSE)
  result$direct_effect_CI = sapply(1:nrow(result), function(i){
    a = as.numeric(gsub("\\(", "", str_split(result$direct_effect_CI[i], ",")[[1]][1]))
    b = as.numeric(gsub("\\)", "", str_split(result$direct_effect_CI[i], ",")[[1]][2]))
    if(result$model[i] == "mi"){
      p = paste0("(", round(100 * (exp(a*log(1.1)) - 1), 2), "% , ", round(100 * (exp(b*log(1.1)) - 1), 2), "%)")
      return(p)}else{
        p = paste0("(", round(100 * (exp(a) - 1), 2), "% , ", round(100 * (exp(b) - 1), 2), "%)")
        return(p)}
  }, USE.NAMES = FALSE)
  result$total_effect = sapply(1:nrow(result), function(i){
    if(result$model[i] == "mi"){return(paste0(round(100 * (exp(result$total_effect[i]*log(1.1)) - 1), 2), "%"))}else{return(paste0(round(100 * (exp(result$total_effect[i]) - 1), 2), "%"))}
  }, USE.NAMES = FALSE)
  result$total_effect_CI = sapply(1:nrow(result), function(i){
    a = as.numeric(gsub("\\(", "", str_split(result$total_effect_CI[i], ",")[[1]][1]))
    b = as.numeric(gsub("\\)", "", str_split(result$total_effect_CI[i], ",")[[1]][2]))
    if(result$model[i] == "mi"){
      p = paste0("(", round(100 * (exp(a*log(1.1)) - 1), 2), "% , ", round(100 * (exp(b*log(1.1)) - 1), 2), "%)")
      return(p)}else{
        p = paste0("(", round(100 * (exp(a) - 1), 2), "% , ", round(100 * (exp(b) - 1), 2), "%)")
        return(p)}
  }, USE.NAMES = FALSE)
  result$mediator_effect = sapply(1:nrow(result), function(i){
    if(result$model[i] == "mi"){return(round(result$mediator_effect[i]*log(1.1) , 2))}else{return(round(result$mediator_effect[i] , 2))}
  }, USE.NAMES = FALSE)
  result$mediator_effect_CI = sapply(1:nrow(result), function(i){
    a = as.numeric(gsub("\\(", "", str_split(result$mediator_effect_CI[i], ",")[[1]][1]))
    b = as.numeric(gsub("\\)", "", str_split(result$mediator_effect_CI[i], ",")[[1]][2]))
    if(result$model[i] == "mi"){
      p = paste0("(", round(a*log(1.1), 2), " , ", round(b*log(1.1), 2), ")")
      return(p)}else{
        p = paste0("(", round(a, 2), " , ", round(b, 2), ")")
        return(p)}
  }, USE.NAMES = FALSE)
  return(result)
}
md_result_new = unit_change(md_result)
md_result_post_new = unit_change(md_result_post)
write_csv(md_result_new, "./Results/mediation/mediation_result_pre_table.csv")
write_csv(md_result_post_new, "./Results/mediation/mediation_result_post_table.csv")


