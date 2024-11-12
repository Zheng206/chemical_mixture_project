library(tidyr)
library(dplyr)
library(network)
library(stringr)
library(igraph)
library(visNetwork)
library(shiny)
library(shinythemes)
library(tidyverse)
source("./func.R")

# Data Wrangling
## Read in Brain Metabolite data
nt_df = rbind(
  './network/brain_metabolites_interaction.csv' %>% 
    read.csv() %>% 
    tibble::add_column(effect = 'interaction') %>% 
    tibble::add_column(outcome.type = 'Brain Metabolite', .after  = 'outcome'),
  './network/brain_metabolites_main.csv' %>% 
    read.csv() %>% 
    tibble::add_column(window = 'five_eight_year', .after = 'term') %>% 
    tibble::add_column(effect = 'main') %>% 
    tibble::add_column(outcome.type = 'Brain Metabolite', .after  = 'outcome')   
)
  
nt_df_total = rbind('./network/brain_metabolites_total.csv' %>% read.csv() %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Brain Metabolite', .after  = 'outcome'),
                    './network/brain_metabolites_main.csv' %>% read.csv() %>% tibble::add_column(window = 'five_eight_year', .after = 'term') %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Brain Metabolite', .after  = 'outcome'))

nt_df_ce = read.csv('./network/brain_metabolites_combined_effect.csv') %>% 
  tibble::add_column(outcome.type = 'Brain Metabolite', .after  = 'outcome') %>% 
  tibble::add_column(effect = 'combined', .after  = 'outcome.type')

## Read in cortical thickness data
thick_df = rbind(
  './network/cortical_thickness_interaction.csv' %>% 
    read.csv() %>%
    tibble::add_column(effect = 'interaction'),
  './network/cortical_thickness_main.csv' %>% 
    read.csv() %>%
    tibble::add_column(window = 'five_eight_year', .after = 'term') %>% 
    tibble::add_column(effect = 'main')
)  %>% 
  tibble::add_column(outcome.type = 'Cortical Thickness', .after  = 'outcome')

thick_df_total = rbind('./network/cortical_thickness_total.csv' %>% read.csv() %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Cortical Thickness', .after  = 'outcome'), 
                       './network/cortical_thickness_main.csv' %>% read.csv() %>% tibble::add_column(window = 'five_eight_year', .after = 'term') %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Cortical Thickness', .after  = 'outcome'))

thick_df_ce = read.csv('./network/cortical_thickness_combined_effect.csv') %>% 
  tibble::add_column(outcome.type = 'Cortical Thickness', .after  = 'outcome') %>% 
  tibble::add_column(effect = 'combined', .after  = 'outcome.type') 

## Read in volumes data
vol_df = rbind(
  './network/cortical_volume_interaction.csv' %>% 
    read.csv() %>%
    tibble::add_column(effect = 'interaction'),
  './network/cortical_volume_main.csv' %>% 
    read.csv() %>%
    tibble::add_column(window = 'five_eight_year', .after = 'term') %>% 
    tibble::add_column(effect = 'main')
) %>% 
  tibble::add_column(outcome.type = 'Volume', .after  = 'outcome')

vol_df_total = rbind('./network/cortical_volume_total.csv' %>% read.csv() %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Volume', .after  = 'outcome'),
                     './network/cortical_volume_main.csv' %>% read.csv() %>% tibble::add_column(window = 'five_eight_year', .after = 'term') %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Volume', .after  = 'outcome'))

vol_df_ce = read.csv('./network/cortical_volume_combined_effect.csv') %>% 
  tibble::add_column(outcome.type = 'Volume', .after  = 'outcome') %>% 
  tibble::add_column(effect = 'combined', .after  = 'outcome.type')

## Read in subcortical volumes data
subcort_df = rbind(
  './network/subcortical_roi_interaction.csv' %>% 
    read.csv() %>%
    tibble::add_column(effect = 'interaction'),
  './network/subcortical_roi_main.csv' %>% 
    read.csv() %>%
    tibble::add_column(window = 'five_eight_year', .after = 'term') %>% 
    tibble::add_column(effect = 'main')
) %>% 
  tibble::add_column(outcome.type = 'Subcortical Volume', .after  = 'outcome')

subcort_df_total = rbind('./network/subcortical_roi_total.csv' %>% read.csv() %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Subcortical Volume', .after  = 'outcome'),
                         './network/subcortical_roi_main.csv' %>% read.csv() %>% tibble::add_column(window = 'five_eight_year', .after = 'term') %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Subcortical Volume', .after  = 'outcome')) 

subcort_df_ce = read.csv('./network/subcortical_roi_combined_effect.csv') %>% 
  tibble::add_column(outcome.type = 'Subcortical Volume', .after  = 'outcome') %>% 
  tibble::add_column(effect = 'combined', .after  = 'outcome.type')

## Read in neurobehavior data
neuro_df = rbind(
  './network/neurobehavior_interaction.csv' %>% 
    read.csv() %>%
    tibble::add_column(effect = 'interaction'),
  './network/neurobehavior_main.csv' %>% 
    read.csv() %>%
    tibble::add_column(window = 'five_eight_year', .after = 'term') %>% 
    tibble::add_column(effect = 'main')
) %>% 
  tibble::add_column(outcome.type = 'Neurobehavior', .after  = 'outcome')

neuro_df_total = rbind('./network/neurobehavior_total.csv' %>% read.csv() %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Neurobehavior', .after  = 'outcome'),
                       './network/neurobehavior_main.csv' %>% read.csv() %>% tibble::add_column(window = 'five_eight_year', .after = 'term') %>% tibble::add_column(effect = 'total') %>% tibble::add_column(outcome.type = 'Neurobehavior', .after  = 'outcome')) 

neuro_df_ce = read.csv('./network/neurobehavior_combined_effect.csv') %>% 
  tibble::add_column(outcome.type = 'Neurobehavior', .after  = 'outcome') %>% 
  tibble::add_column(effect = 'combined', .after  = 'outcome.type')

## Join all data.frames
ex = c("third_Ventricle", "Left_vessel", "Optic_Chiasm", "Left_Inf_Lat_Vent", "Lateral_Ventricle", "CSF")
df = rbind(nt_df, 
           thick_df, 
           vol_df,
           subcort_df,
           neuro_df) %>% filter(p.value < 0.1) %>% filter(!outcome %in% ex) %>% mutate(
             term = case_when(term == "BCEtP" ~ "BCEP",
                              term == "DBuP" ~ "DNBP",
                              term == "BDCPP" ~ "BDCIPP",
                              .default = term),
             window = case_when(window == "three_year" ~ "3y",
                                window == "five_eight_year" ~ "5-8y",
                                .default = window)
           )

df_total = rbind(nt_df_total, 
                 thick_df_total, 
                 vol_df_total,
                 subcort_df_total,
                 neuro_df_total) %>% distinct() %>% filter(p.value < 0.1) %>% 
  mutate(window = case_when(window == "five_eight_year" ~ "5-8y",
                            .default = window),
         term = case_when(
           term == "DBUP" ~ "DBuP",
           .default = term
         )) %>% filter(!outcome %in% ex) %>% mutate(
           term = case_when(term == "BCEtP" ~ "BCEP",
                            term == "DBuP" ~ "DNBP",
                            term == "BDCPP" ~ "BDCIPP",
                            .default = term)
         ) 

df_ce = rbind(nt_df_ce, 
              thick_df_ce, 
              vol_df_ce,
              subcort_df_ce,
              neuro_df_ce) %>% filter(p.value < 0.1) %>% filter(!outcome %in% ex) %>% mutate(
                term = case_when(term == "BCEtP" ~ "BCEP",
                                 term == "DBuP" ~ "DNBP",
                                 term == "BDCPP" ~ "BDCIPP",
                                 .default = term)
              ) 
df_ce = df_ce %>% filter(window != "8y")
agree_df = read_csv("./network/agreed_results.csv") %>% filter(!outcome %in% ex) %>% mutate(term = case_when(term == "BCEtP" ~ "BCEP",
                                                                                term == "DBuP" ~ "DNBP",
                                                                                term == "BDCPP" ~ "BDCIPP",
                                                                                .default = term),
                                                               type = case_when(type == "metabolites" ~ "Brain Metabolite",
                                                                                type == "CV" ~ "Volume",
                                                                                type == "CT" ~ "Cortical Thickness",
                                                                                type == "Sub" ~ "Subcortical Volume",
                                                                                type == "Neuro" ~ "Neurobehavior"))

# Multiple Informant
## create nodes 
nodes = dplyr::union(df %>% dplyr::select(term, outcome.type, window) %>% rename(variable=term), 
                     df %>% dplyr::select(outcome, outcome.type, window) %>% rename(variable=outcome)) %>%  
  mutate(outcome.type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP"), 'Chem. Mix.', outcome.type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(outcome.type == 'Cortical Thickness', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(outcome.type == 'Volume', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = outcome.type)

links = df %>% 
  mutate(outcome = ifelse(outcome.type == 'Cortical Thickness', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(outcome.type == 'Volume', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes),
         from = purrr::map_int(from, id_node, nodes = nodes), 
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y','5-8y')))

nodes_total = dplyr::union(df_total %>% dplyr::select(term, outcome.type, window) %>% rename(variable=term), 
                           df_total %>% dplyr::select(outcome, outcome.type, window) %>% rename(variable=outcome)) %>%  
  mutate(outcome.type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP"), 'Chem. Mix.', outcome.type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(outcome.type == 'Cortical Thickness', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(outcome.type == 'Volume', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = outcome.type)

links_total = df_total %>% 
  mutate(outcome = ifelse(outcome.type == 'Cortical Thickness', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(outcome.type == 'Volume', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_total),
         from = purrr::map_int(from, id_node, nodes = nodes_total), 
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y', "5-8y")))

# Quantile g-computation
## create nodes 
nodes_ce = dplyr::union(df_ce %>% dplyr::select(term, outcome.type, window) %>% rename(variable=term), 
                        df_ce %>% dplyr::select(outcome, outcome.type, window) %>% rename(variable=outcome)) %>%  
  mutate(outcome.type = ifelse(variable == "OPE", 'Chem. Mix.', outcome.type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(outcome.type == 'Cortical Thickness', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(outcome.type == 'Volume', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = outcome.type)

links_ce = df_ce %>% 
  mutate(outcome = ifelse(outcome.type == 'Cortical Thickness', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(outcome.type == 'Volume', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_ce),
         from = purrr::map_int(from, id_node, nodes = nodes_ce), 
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y', '5-8y')))

# Separate Global Measurements
global = c("TotalGrayVol", "BrainSegVol", "SubCortGrayVol", "SupraTentorialVol", "Lateral_Ventricle", "Cerebellum_White_Matter", "Cerebellum_Cortex", "CortexVol", "CerebralWhiteMatterVol", "surface_area", "total_brain", "MeanThickness (CT)")

nodes$group = sapply(1:nrow(nodes), function(i){
  case_when(nodes[[i, "variable"]] %in% global ~ "Global",
            .default = nodes[[i, "group"]])
}, USE.NAMES = FALSE)

nodes_total$group = sapply(1:nrow(nodes_total), function(i){
  case_when(nodes_total[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_total[[i, "group"]])
}, USE.NAMES = FALSE)

nodes_ce$group = sapply(1:nrow(nodes_ce), function(i){
  case_when(nodes_ce[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_ce[[i, "group"]])
}, USE.NAMES = FALSE)

df$outcome.type = sapply(1:nrow(df), function(i){
  case_when(df[[i, "outcome"]] %in% global ~ "Global",
            .default = df[[i, "outcome.type"]])
}, USE.NAMES = FALSE)

df_total$outcome.type = sapply(1:nrow(df_total), function(i){
  case_when(df_total[[i, "outcome"]] %in% global ~ "Global",
            .default = df_total[[i, "outcome.type"]])
}, USE.NAMES = FALSE)

df_ce$outcome.type = sapply(1:nrow(df_ce), function(i){
  case_when(df_ce[[i, "outcome"]] %in% global ~ "Global",
            .default = df_ce[[i, "outcome.type"]])
}, USE.NAMES = FALSE)

df = df %>% mutate(
  sig = case_when(p.value < 0.1 & p.value >= 0.05 ~ ".",
                  p.value <0.05 & p.value >= 0.01 ~ "*",
                  p.value < 0.01 & p.value >= 0.001 ~ "**", 
                  p.value < 0.001 ~ "***"),
  conf.low = sprintf("%.3f", conf.low),
  estimate = sprintf("%.3f", estimate),
  conf.high = sprintf("%.3f", conf.high),
  p.value = sprintf("%.3f", p.value)) %>% arrange(as.numeric(p.value))

df_total= df_total %>% mutate(
  sig = case_when(p.value < 0.1 & p.value >= 0.05 ~ ".",
                  p.value <0.05 & p.value >= 0.01 ~ "*",
                  p.value < 0.01 & p.value >= 0.001 ~ "**", 
                  p.value < 0.001 ~ "***"),
  conf.low = sprintf("%.3f", conf.low),
  estimate = sprintf("%.3f", estimate),
  conf.high = sprintf("%.3f", conf.high),
  p.value = sprintf("%.3f", p.value)) %>% arrange(as.numeric(p.value))

df_ce = df_ce %>% mutate(
  sig = case_when(p.value < 0.1 & p.value >= 0.05 ~ ".",
                  p.value <0.05 & p.value >= 0.01 ~ "*",
                  p.value < 0.01 & p.value >= 0.001 ~ "**", 
                  p.value < 0.001 ~ "***"),
  conf.low = sprintf("%.3f", conf.low),
  estimate = sprintf("%.3f", estimate),
  conf.high = sprintf("%.3f", conf.high),
  p.value = sprintf("%.3f", p.value)) %>% arrange(as.numeric(p.value))

agree_df = agree_df %>% mutate(
  sig = case_when(p.value < 0.1 & p.value >= 0.05 ~ ".",
                  p.value <0.05 & p.value >= 0.01 ~ "*",
                  p.value < 0.01 & p.value >= 0.001 ~ "**", 
                  p.value < 0.001 ~ "***"),
  conf.low = sprintf("%.3f", conf.low),
  estimate = sprintf("%.3f", estimate),
  conf.high = sprintf("%.3f", conf.high),
  p.value = sprintf("%.3f", p.value)) %>% arrange(as.numeric(p.value))

## Brain Related Nodes Identification
nodes_list = rbind(nodes %>% dplyr::select(variable, group) %>% filter(group %in% c("Cortical Thickness", "Global", "Volume", "Subcortical Volume")) %>% unique(),
      nodes_ce %>% dplyr::select(variable, group) %>% filter(group %in% c("Cortical Thickness", "Global", "Volume", "Subcortical Volume")) %>% unique(),
      nodes_total %>% dplyr::select(variable, group) %>% filter(group %in% c("Cortical Thickness", "Global", "Volume", "Subcortical Volume")) %>% unique()) %>% distinct()
#write_csv(nodes_list, "/Users/zhengren/Desktop/Chemical_Mixture/Results/network/brain_nodes.csv")

## Model Agreement
nodes_mi = read_csv("./network/nodes_mi.csv")
nodes_qg = read_csv("./network/nodes_qg.csv")
nodes_brain_qg = read_csv("./network/nodes_brain_qg.csv")
nodes_brain_mi = read_csv("./network/nodes_brain_mi.csv")
nodes_func_qg = read_csv("./network/nodes_func_qg.csv")
nodes_func_mi = read_csv("./network/nodes_func_mi.csv")
links_mi = read_csv("./network/links_mi.csv")
links_qg = read_csv("./network/links_qg.csv")
links_brain_qg = read_csv("./network/links_brain_qg.csv")
links_brain_mi = read_csv("./network/links_brain_mi.csv")
links_func_qg = read_csv("./network/links_func_qg.csv")
links_func_mi = read_csv("./network/links_func_mi.csv")

## Mediation Analysis
nodes_mi_me = read_csv("./network/nodes_mi_me.csv")
nodes_qg_me = read_csv("./network/nodes_qg_me.csv")
links_mi_me = read_csv("./network/links_mi_me.csv")
links_qg_me = read_csv("./network/links_qg_me.csv")
CI_fun = function(x, type = "raw"){
  if(type == "raw"){
    a = as.numeric(gsub("\\(", "", str_split(x, ",")[[1]][1]))
    b = as.numeric(gsub("\\)", "", str_split(x, ",")[[1]][2]))
    return(paste0("(", sprintf("%.3f", a), " , ", sprintf("%.3f", b), ")"))
  }else{
    a = as.numeric(gsub("%", "", gsub("\\(", "", str_split(x, ",")[[1]][1])))
    b = as.numeric(gsub("%", "", gsub("\\)", "", str_split(x, ",")[[1]][2])))
    return(paste0("(", sprintf("%.2f", a), "% , ", sprintf("%.2f", b), "%)"))
  }
}
me_pre = read_csv("./network/mediation_result_pre.csv") %>% mutate(type = case_when(type == "CV" ~ "Volume")) %>% mutate(indirect_effect_CI = sapply(indirect_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                         mediator_effect_CI = sapply(mediator_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                         direct_effect_CI = sapply(direct_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                         total_effect_CI = sapply(total_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                         indirect_effect = sapply(indirect_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                         mediator_effect = sapply(mediator_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                         direct_effect = sapply(direct_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                         total_effect = sapply(total_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE))
me_pre_table = read_csv("./network/mediation_result_pre_table.csv") %>% mutate(type = case_when(type == "CV" ~ "Volume")) %>% mutate(indirect_effect_CI = sapply(indirect_effect_CI, CI_fun, type ="int", USE.NAMES = FALSE),
                                                                                                                                     mediator_effect_CI = sapply(mediator_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                                     direct_effect_CI = sapply(direct_effect_CI, CI_fun, type ="int", USE.NAMES = FALSE),
                                                                                                                                     total_effect_CI = sapply(total_effect_CI, CI_fun, type ="int", USE.NAMES = FALSE),
                                                                                                                                     indirect_effect = sapply(indirect_effect, function(x) paste0(sprintf("%.2f", as.numeric(gsub("%", "", x))), "%"), USE.NAMES = FALSE),
                                                                                                                                     mediator_effect = sapply(mediator_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                                     direct_effect = sapply(direct_effect, function(x) paste0(sprintf("%.2f", as.numeric(gsub("%", "", x))), "%"), USE.NAMES = FALSE),
                                                                                                                                     total_effect = sapply(total_effect, function(x) paste0(sprintf("%.2f", as.numeric(gsub("%", "", x))), "%"), USE.NAMES = FALSE))
me_post = read_csv("./network/mediation_result_post.csv") %>% mutate(type = case_when(type == "CV" ~ "Volume")) %>% mutate(indirect_effect_CI = sapply(indirect_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                           mediator_effect_CI = sapply(mediator_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                           direct_effect_CI = sapply(direct_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                           total_effect_CI = sapply(total_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                           indirect_effect = sapply(indirect_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                           mediator_effect = sapply(mediator_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                           direct_effect = sapply(direct_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                           total_effect = sapply(total_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE))
me_post_table = read_csv("./network/mediation_result_post_table.csv") %>% mutate(type = case_when(type == "CV" ~ "Volume")) %>% mutate(indirect_effect_CI = sapply(indirect_effect_CI, CI_fun, type ="int", USE.NAMES = FALSE),
                                                                                                                                       mediator_effect_CI = sapply(mediator_effect_CI, CI_fun, USE.NAMES = FALSE),
                                                                                                                                       direct_effect_CI = sapply(direct_effect_CI, CI_fun, type ="int", USE.NAMES = FALSE),
                                                                                                                                       total_effect_CI = sapply(total_effect_CI, CI_fun, type ="int", USE.NAMES = FALSE),
                                                                                                                                       indirect_effect = sapply(indirect_effect, function(x) paste0(sprintf("%.2f", as.numeric(gsub("%", "", x))), "%"), USE.NAMES = FALSE),
                                                                                                                                       mediator_effect = sapply(mediator_effect, function(x) sprintf("%.3f", as.numeric(x)), USE.NAMES = FALSE),
                                                                                                                                       direct_effect = sapply(direct_effect, function(x) paste0(sprintf("%.2f", as.numeric(gsub("%", "", x))), "%"), USE.NAMES = FALSE),
                                                                                                                                       total_effect = sapply(total_effect, function(x) paste0(sprintf("%.2f", as.numeric(gsub("%", "", x))), "%"), USE.NAMES = FALSE))

### Stop here, need modification of the shiny app layout.....

function(input, output, session){
  output$effect_type = renderUI({
    if(input$model == "Multiple Informant Model"){
      radioButtons("effect_type", "Select the type of effects", choices = c("main & interaction", "total"), selected = "total")
    }
  })
  output$window_option = renderUI({
    if(input$model == "Multiple Informant Model"){
      if(!is.null(input$effect_type) && input$effect_type == "total"){
        radioButtons("window_total", "Select the window of interest", choices = c("prenatal", "birth", "3y", "5-8y (reference window)"), selected = "prenatal")
      }else if(!is.null(input$effect_type) && input$effect_type == "main & interaction"){
        radioButtons("window_main", "Select the window of interest", choices = c("prenatal", "birth", "3y", "5-8y (reference window)"), selected = "prenatal")
      }
    }else{
      radioButtons("com_window", "Select the window of interest", choices = c("prenatal", "birth", "3y", "5-8y"), selected = "prenatal")
    }
  })
  output$vis_output = renderUI({
    if(input$model == "Multiple Informant Model"){
      visNetworkOutput("MI_plot", height = 600)
    }else{
      visNetworkOutput("qg_plot", height = 600)
    }
  })
  
  output$explore_table = DT::renderDT({
    if(input$model == "Multiple Informant Model"){
      if(!is.null(input$effect_type) && input$effect_type == "main & interaction"){
        if(!is.null(input$window_main)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
          selected_name = nodes %>% filter(id == selected_ids) %>% pull(variable)
          selected_type = nodes %>% filter(id == selected_ids) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          }
          if(input$window_main == "5-8y (reference window)"){
            if (length(selected_ids) > 0) {
              df %>% filter(window == "5-8y", (term %in% selected_name | (outcome %in% selected_name & outcome.type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                  targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                    'sig',
                                                                                                    target = 'row',
                                                                                                    backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }else{
              df %>% filter(window == "5-8y") %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                           targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                             'sig',
                                                                                                             target = 'row',
                                                                                                             backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }
          }else{
            if (length(selected_ids) > 0) {
              df %>% filter((window == input$window_main | window == "5-8y"), (term %in% selected_name | (outcome %in% selected_name & outcome.type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                             targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                               'sig',
                                                                                                               target = 'row',
                                                                                                               backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }else{
              df %>% filter(window == input$window_main | window == "5-8y") %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                         targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                           'sig',
                                                                                                           target = 'row',
                                                                                                           backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }
          }
        }
      }else if(!is.null(input$effect_type) && input$effect_type == "total"){
        if(!is.null(input$window_total)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
            selected_name = nodes_total %>% filter(id == selected_ids) %>% pull(variable)
            selected_type = nodes_total %>% filter(id == selected_ids) %>% pull(group)
            selected_name = sapply(selected_name, function(x){
              return(gsub(" \\(.*\\)", "", x))
            }, USE.NAMES = FALSE)
          }
          if(input$window_total == "5-8y (reference window)"){
            if (length(selected_ids) > 0) {
              df_total %>% filter(window == "5-8y", (term %in% selected_name | (outcome %in% selected_name & outcome.type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                        targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                          'sig',
                                                                                                          target = 'row',
                                                                                                          backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }else{
              df_total %>% filter(window == "5-8y") %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                  targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                    'sig',
                                                                                                                    target = 'row',
                                                                                                                    backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }
          }else{
            if (length(selected_ids) > 0) {
              df_total %>% filter(window == input$window_total, (term %in% selected_name | (outcome %in% selected_name & outcome.type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                    targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                      'sig',
                                                                                                                      target = 'row',
                                                                                                                      backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }else{
              df_total %>% filter(window == input$window_total) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                  'sig',
                                                                                                                  target = 'row',
                                                                                                                  backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
            }
          }
        }
      }
    }else{
      if(!is.null(input$com_window)){
      selected_ids = input$network_selected
      if (length(selected_ids) > 0){
        selected_name = nodes_ce %>% filter(id == selected_ids) %>% pull(variable)
        selected_type = nodes_ce %>% filter(id == selected_ids) %>% pull(group)
        selected_name = sapply(selected_name, function(x){
          return(gsub(" \\(.*\\)", "", x))
        }, USE.NAMES = FALSE)
        df_ce%>% filter(window == input$com_window, (term %in% selected_name | (outcome %in% selected_name & outcome.type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                    targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                      'sig',
                                                                                                                                                                                                      target = 'row',
                                                                                                                                                                                                      backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
      }else{
        df_ce %>% filter(window == input$com_window) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                              targets = "_all")))) %>% formatStyle(column = as.numeric(c(3:5)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                'sig',
                                                                                                                target = 'row',
                                                                                                                backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
       }
      }
    }
  })
  output$MI_plot = renderVisNetwork({
    if(!is.null(input$effect_type) && input$effect_type == "main & interaction"){
      if(!is.null(input$window_main) && input$window_main == "prenatal"){
        vizNetwork(vis.nodes = nodes %>% filter(prenatal == 1 | `5-8y` == 1),
                   vis.links = links %>% filter(window %in% c('prenatal', '5-8y'))) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else if(!is.null(input$window_main) && input$window_main == "birth"){
        vizNetwork(vis.nodes = nodes %>% filter(birth == 1 | `5-8y` == 1),
                   vis.links = links %>% filter(window %in% c('birth', '5-8y'))) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else if(!is.null(input$window_main) && input$window_main == "3y"){
        vizNetwork(vis.nodes = nodes %>% filter(`3y` == 1 | `5-8y` == 1),
                   vis.links = links %>% filter(window %in% c('3y', '5-8y'))) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else if(!is.null(input$window_main) && input$window_main == "5-8y (reference window)"){
        vizNetwork(vis.nodes = nodes %>% filter(`5-8y` == 1),
                   vis.links = links %>% filter(window  == '5-8y')) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }
    }else if(!is.null(input$effect_type) && input$effect_type == "total"){
      if(!is.null(input$window_total) && input$window_total == "prenatal"){
        vizNetwork(vis.nodes = nodes_total %>% filter(prenatal == 1),
                   vis.links = links_total %>% filter(window %in% c('prenatal'))) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else if(!is.null(input$window_total) && input$window_total == "birth"){
        vizNetwork(vis.nodes = nodes_total %>% filter(birth == 1),
                   vis.links = links_total %>% filter(window %in% c('birth'))) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else if(!is.null(input$window_total) && input$window_total == "3y"){
        vizNetwork(vis.nodes = nodes_total %>% filter(`3y` == 1),
                   vis.links = links_total %>% filter(window %in% c('3y'))) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else if(!is.null(input$window_total) && input$window_total == "5-8y (reference window)"){
        vizNetwork(vis.nodes = nodes_total %>% filter(`5-8y` == 1),
                   vis.links = links_total %>% filter(window %in% c('5-8y'))) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }
    }
  })
  
  output$qg_plot = renderVisNetwork({
    if(input$com_window == "prenatal"){
      vizNetwork(vis.nodes = nodes_ce %>% filter(prenatal == 1),
                 vis.links = links_ce %>% filter(window %in% c('prenatal'))) %>%
        visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
    }else if(input$com_window == "birth"){
      vizNetwork(vis.nodes = nodes_ce %>% filter(birth == 1),
                 vis.links = links_ce %>% filter(window %in% c('birth'))) %>%
        visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
    }else if(input$com_window == "3y"){
      vizNetwork(vis.nodes = nodes_ce %>% filter(`3y` == 1),
                 vis.links = links_ce %>% filter(window %in% c('3y'))) %>%
        visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
    }else if(input$com_window == "5-8y"){
      vizNetwork(vis.nodes = nodes_ce %>% filter(`5-8y` == 1),
                 vis.links = links_ce %>% filter(window %in% c('5-8y'))) %>%
        visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
    }
  })
  
  output$agree_table = DT::renderDT({
    if(input$brain_me == "individual"){
      if(input$agree_effect == "Single OPE Effect"){
        if(!is.null(input$window_me)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
            selected_name = nodes_mi %>% filter(id == selected_ids) %>% pull(variable)
            selected_type = nodes_mi %>% filter(id == selected_ids) %>% pull(group)
            selected_name = sapply(selected_name, function(x){
              return(gsub(" \\(.*\\)", "", x))
            }, USE.NAMES = FALSE)
            agree_df %>% filter(window == input$window_me, (term %in% selected_name | (outcome %in% selected_name & type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                        targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                          'sig',
                                                                                                                                                                                                          target = 'row',
                                                                                                                                                                                                          backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }else{
            agree_df %>% filter(window == input$window_me) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                             targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                               'sig',
                                                                                                               target = 'row',
                                                                                                               backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }
        }
      }else{
        if(!is.null(input$window_me)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
            selected_name = nodes_qg %>% filter(id == selected_ids) %>% pull(variable)
            selected_type = nodes_qg %>% filter(id == selected_ids) %>% pull(group)
            selected_name = sapply(selected_name, function(x){
              return(gsub(" \\(.*\\)", "", x))
            }, USE.NAMES = FALSE)
            agree_df %>% filter(window == input$window_me, (term %in% selected_name | (outcome %in% selected_name & type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                           targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                             'sig',
                                                                                                                                                                                                             target = 'row',
                                                                                                                                                                                                             backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }else{
            agree_df %>% filter(window == input$window_me) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                               targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                 'sig',
                                                                                                                 target = 'row',
                                                                                                                 backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }
        }
      }
    }else if(input$brain_me == "anatomy"){
      if(input$agree_effect == "Single OPE Effect"){
        if(!is.null(input$window_me)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
            selected_name = nodes_brain_mi %>% filter(id == selected_ids) %>% pull(variable)
            selected_type = nodes_mi %>% filter(variable == selected_name) %>% pull(group)
            selected_name = sapply(selected_name, function(x){
              return(gsub(" \\(.*\\)", "", x))
            }, USE.NAMES = FALSE)
            agree_df %>% filter(window == input$window_me, (term %in% selected_name | (outcome %in% selected_name & type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                           targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                             'sig',
                                                                                                                                                                                                             target = 'row',
                                                                                                                                                                                                             backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }else{
            agree_df %>% filter(window == input$window_me) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                               targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                 'sig',
                                                                                                                 target = 'row',
                                                                                                                 backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }
        }
      }else{
        if(!is.null(input$window_me)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
            selected_name = nodes_brain_qg %>% filter(id == selected_ids) %>% pull(variable)
            selected_type = nodes_qg %>% filter(variable == selected_name) %>% pull(group)
            selected_name = sapply(selected_name, function(x){
              return(gsub(" \\(.*\\)", "", x))
            }, USE.NAMES = FALSE)
            agree_df %>% filter(window == input$window_me, (term %in% selected_name | (outcome %in% selected_name & type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                           targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                             'sig',
                                                                                                                                                                                                             target = 'row',
                                                                                                                                                                                                             backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }else{
            agree_df %>% filter(window == input$window_me) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                               targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                 'sig',
                                                                                                                 target = 'row',
                                                                                                                 backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }
        }
      }
    }else{
      if(input$agree_effect == "Single OPE Effect"){
        if(!is.null(input$window_me)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
            selected_name = nodes_func_mi %>% filter(id == selected_ids) %>% pull(variable)
            selected_type = nodes_mi %>% filter(variable == selected_name) %>% pull(group)
            selected_name = sapply(selected_name, function(x){
              return(gsub(" \\(.*\\)", "", x))
            }, USE.NAMES = FALSE)
            agree_df %>% filter(window == input$window_me, (term %in% selected_name | (outcome %in% selected_name & type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                           targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                             'sig',
                                                                                                                                                                                                             target = 'row',
                                                                                                                                                                                                             backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }else{
            agree_df %>% filter(window == input$window_me) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                               targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                 'sig',
                                                                                                                 target = 'row',
                                                                                                                 backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }
        }
      }else{
        if(!is.null(input$window_me)){
          selected_ids = input$network_selected
          if (length(selected_ids) > 0){
            selected_name = nodes_func_qg %>% filter(id == selected_ids) %>% pull(variable)
            selected_type = nodes_qg %>% filter(variable == selected_name) %>% pull(group)
            selected_name = sapply(selected_name, function(x){
              return(gsub(" \\(.*\\)", "", x))
            }, USE.NAMES = FALSE)
            agree_df %>% filter(window == input$window_me, (term %in% selected_name | (outcome %in% selected_name & type %in% selected_type))) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                           targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                             'sig',
                                                                                                                                                                                                             target = 'row',
                                                                                                                                                                                                             backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }else{
            agree_df %>% filter(window == input$window_me) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                               targets = "_all")))) %>% formatStyle(column = as.numeric(c(5:7)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                 'sig',
                                                                                                                 target = 'row',
                                                                                                                 backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
          }
        }
      }
    }
  })
  
  output$agree_plot = renderVisNetwork({
    if(input$brain_me == "individual"){
      if(input$agree_effect == "Single OPE Effect"){
        if(input$window_me == "prenatal"){
          vizNetwork(vis.nodes = nodes_mi %>% filter(prenatal == 1),
                   vis.links = links_mi %>% filter(window == "prenatal")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "birth"){
          vizNetwork(vis.nodes = nodes_mi %>% filter(birth == 1),
                     vis.links = links_mi %>% filter(window == "birth")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "3y"){
          vizNetwork(vis.nodes = nodes_mi %>% filter(`3y` == 1),
                     vis.links = links_mi %>% filter(window == "3y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "5-8y"){
          vizNetwork(vis.nodes = nodes_mi %>% filter(`5-8y` == 1),
                     vis.links = links_mi %>% filter(window == "5-8y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }
      }else{
        if(input$window_me == "prenatal"){
          vizNetwork(vis.nodes = nodes_qg %>% filter(prenatal == 1),
                     vis.links = links_qg %>% filter(window == "prenatal")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "birth"){
          vizNetwork(vis.nodes = nodes_qg %>% filter(birth == 1),
                     vis.links = links_qg %>% filter(window == "birth")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "3y"){
          vizNetwork(vis.nodes = nodes_qg %>% filter(`3y` == 1),
                     vis.links = links_qg %>% filter(window == "3y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "5-8y"){
          vizNetwork(vis.nodes = nodes_qg %>% filter(`5-8y` == 1),
                     vis.links = links_qg %>% filter(window == "5-8y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }
      }
    }else if(input$brain_me == "anatomy"){
      if(input$agree_effect == "Single OPE Effect"){
        if(input$window_me == "prenatal"){
          vizNetwork(vis.nodes = nodes_brain_mi %>% filter(prenatal == 1),
                     vis.links = links_brain_mi %>% filter(window == "prenatal")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "birth"){
          vizNetwork(vis.nodes = nodes_brain_mi %>% filter(birth == 1),
                     vis.links = links_brain_mi %>% filter(window == "birth")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "3y"){
          vizNetwork(vis.nodes = nodes_brain_mi %>% filter(`3y` == 1),
                     vis.links = links_brain_mi %>% filter(window == "3y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "5-8y"){
          vizNetwork(vis.nodes = nodes_brain_mi %>% filter(`5-8y` == 1),
                     vis.links = links_brain_mi %>% filter(window == "5-8y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }
      }else{
        if(input$window_me == "prenatal"){
          vizNetwork(vis.nodes = nodes_brain_qg %>% filter(prenatal == 1),
                     vis.links = links_brain_qg %>% filter(window == "prenatal")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "birth"){
          vizNetwork(vis.nodes = nodes_brain_qg %>% filter(birth == 1),
                     vis.links = links_brain_qg %>% filter(window == "birth")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "3y"){
          vizNetwork(vis.nodes = nodes_brain_qg %>% filter(`3y` == 1),
                     vis.links = links_brain_qg %>% filter(window == "3y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "5-8y"){
          vizNetwork(vis.nodes = nodes_brain_qg %>% filter(`5-8y` == 1),
                     vis.links = links_brain_qg %>% filter(window == "5-8y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }
      }
    }else{
      if(input$agree_effect == "Single OPE Effect"){
        if(input$window_me == "prenatal"){
          vizNetwork(vis.nodes = nodes_func_mi %>% filter(prenatal == 1),
                     vis.links = links_func_mi %>% filter(window == "prenatal")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "birth"){
          vizNetwork(vis.nodes = nodes_func_mi %>% filter(birth == 1),
                     vis.links = links_func_mi %>% filter(window == "birth")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "3y"){
          vizNetwork(vis.nodes = nodes_func_mi %>% filter(`3y` == 1),
                     vis.links = links_func_mi %>% filter(window == "3y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "5-8y"){
          vizNetwork(vis.nodes = nodes_func_mi %>% filter(`5-8y` == 1),
                     vis.links = links_func_mi %>% filter(window == "5-8y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }
      }else{
        if(input$window_me == "prenatal"){
          vizNetwork(vis.nodes = nodes_func_qg %>% filter(prenatal == 1),
                     vis.links = links_func_qg %>% filter(window == "prenatal")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "birth"){
          vizNetwork(vis.nodes = nodes_func_qg %>% filter(birth == 1),
                     vis.links = links_func_qg %>% filter(window == "birth")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "3y"){
          vizNetwork(vis.nodes = nodes_func_qg %>% filter(`3y` == 1),
                     vis.links = links_func_qg %>% filter(window == "3y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }else if(input$window_me == "5-8y"){
          vizNetwork(vis.nodes = nodes_func_qg %>% filter(`5-8y` == 1),
                     vis.links = links_func_qg %>% filter(window == "5-8y")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
        }
      }
    }
  })
  
  output$mediation_table = DT::renderDT({
    if(input$agree_effect_me == "Single OPE Effect"){
      if(input$mediation_selection == "pre"){
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_mi_me %>% filter(status == "pre", id == selected_ids) %>% pull(variable)
          selected_type = nodes_mi_me %>% filter(status == "pre", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_mi_me %>% filter(status == "pre", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_mi_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          to = sapply(to, function(x) nodes_mi_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                          estimate = case_when(effect == "mediation" ~ me_pre %>% filter(model == "mi") %>% pull(mediator_effect),
                                                                                                                               effect == "indirect" ~ me_pre %>% filter(model == "mi") %>% pull(indirect_effect),
                                                                                                                               effect == "direct" ~ me_pre %>% filter(model == "mi") %>% pull(direct_effect),
                                                                                                                               effect == "total" ~ me_pre %>% filter(model == "mi") %>% pull(total_effect)),
                                                                                                          CI.95 = case_when(effect == "mediation" ~ me_pre %>% filter(model == "mi") %>% pull(mediator_effect_CI),
                                                                                                                            effect == "indirect" ~ me_pre %>% filter(model == "mi") %>% pull(indirect_effect_CI),
                                                                                                                            effect == "direct" ~ me_pre %>% filter(model == "mi") %>% pull(direct_effect_CI),
                                                                                                                            effect == "total" ~ me_pre %>% filter(model == "mi") %>% pull(total_effect_CI)),
                                                                                                          sig = sapply(CI.95, function(x){
                                                                                                            a = as.numeric(gsub("\\(", "", str_split(x, ",")[[1]][1]))
                                                                                                            b = as.numeric(gsub("\\)", "", str_split(x, ",")[[1]][2]))
                                                                                                            if(a*b >= 0){return("*")}else{return("")}
                                                                                                          }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                      targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                        'sig',
                                                                                                                                                                                                                                                                        target = 'row',
                                                                                                                                                                                                                                                                        backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_pre %>% filter(model == "mi") %>% dplyr::select(-agreed) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                             targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red"))) 
        }
      }else{
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_mi_me %>% filter(status == "post", id == selected_ids) %>% pull(variable)
          selected_type = nodes_mi_me %>% filter(status == "post", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_mi_me %>% filter(status == "post", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_mi_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          to = sapply(to, function(x) nodes_mi_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                          estimate = case_when(effect == "mediation" ~ me_post %>% filter(model == "mi") %>% pull(mediator_effect),
                                                                                                                               effect == "indirect" ~ me_post %>% filter(model == "mi") %>% pull(indirect_effect),
                                                                                                                               effect == "direct" ~ me_post %>% filter(model == "mi") %>% pull(direct_effect),
                                                                                                                               effect == "total" ~ me_post %>% filter(model == "mi") %>% pull(total_effect)),
                                                                                                          CI.95 = case_when(effect == "mediation" ~ me_post %>% filter(model == "mi") %>% pull(mediator_effect_CI),
                                                                                                                            effect == "indirect" ~ me_post %>% filter(model == "mi") %>% pull(indirect_effect_CI),
                                                                                                                            effect == "direct" ~ me_post %>% filter(model == "mi") %>% pull(direct_effect_CI),
                                                                                                                            effect == "total" ~ me_post %>% filter(model == "mi") %>% pull(total_effect_CI)),
                                                                                                          sig = sapply(CI.95, function(x){
                                                                                                            a = as.numeric(gsub("\\(", "", str_split(x, ",")[[1]][1]))
                                                                                                            b = as.numeric(gsub("\\)", "", str_split(x, ",")[[1]][2]))
                                                                                                            if(a*b >= 0){return("*")}else{return("")}
                                                                                                          }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                  targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                    'sig',
                                                                                                                                                                                                                                                                    target = 'row',
                                                                                                                                                                                                                                                                    backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_post %>% filter(model == "mi") %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                        targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red"))) 
        }
      }
    }else{
      if(input$mediation_selection == "pre"){
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_qg_me %>% filter(status == "pre", id == selected_ids) %>% pull(variable)
          selected_type = nodes_qg_me %>% filter(status == "pre", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_qg_me %>% filter(status == "pre", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_qg_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          to = sapply(to, function(x) nodes_qg_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                          estimate = case_when(effect == "mediation" ~ me_pre %>% filter(model == "qg") %>% pull(mediator_effect),
                                                                                                                               effect == "indirect" ~ me_pre %>% filter(model == "qg") %>% pull(indirect_effect),
                                                                                                                               effect == "direct" ~ me_pre %>% filter(model == "qg") %>% pull(direct_effect),
                                                                                                                               effect == "total" ~ me_pre %>% filter(model == "qg") %>% pull(total_effect)),
                                                                                                          CI.95 = case_when(effect == "mediation" ~ me_pre %>% filter(model == "qg") %>% pull(mediator_effect_CI),
                                                                                                                            effect == "indirect" ~ me_pre %>% filter(model == "qg") %>% pull(indirect_effect_CI),
                                                                                                                            effect == "direct" ~ me_pre %>% filter(model == "qg") %>% pull(direct_effect_CI),
                                                                                                                            effect == "total" ~ me_pre %>% filter(model == "qg") %>% pull(total_effect_CI)),
                                                                                                          sig = sapply(CI.95, function(x){
                                                                                                            a = as.numeric(gsub("\\(", "", str_split(x, ",")[[1]][1]))
                                                                                                            b = as.numeric(gsub("\\)", "", str_split(x, ",")[[1]][2]))
                                                                                                            if(a*b >= 0){return("*")}else{return("")}
                                                                                                          }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                  targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                    'sig',
                                                                                                                                                                                                                                                                    target = 'row',
                                                                                                                                                                                                                                                                    backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_pre %>% filter(model == "qg") %>% dplyr::select(-agreed) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                        targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red"))) 
        }
      }else{
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_qg_me %>% filter(status == "post", id == selected_ids) %>% pull(variable)
          selected_type = nodes_qg_me %>% filter(status == "post", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_qg_me %>% filter(status == "post", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_qg_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          to = sapply(to, function(x) nodes_qg_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                          estimate = case_when(effect == "mediation" ~ me_post %>% filter(model == "qg") %>% pull(mediator_effect),
                                                                                                                               effect == "indirect" ~ me_post %>% filter(model == "qg") %>% pull(indirect_effect),
                                                                                                                               effect == "direct" ~ me_post %>% filter(model == "qg") %>% pull(direct_effect),
                                                                                                                               effect == "total" ~ me_post %>% filter(model == "qg") %>% pull(total_effect)),
                                                                                                          CI.95 = case_when(effect == "mediation" ~ me_post %>% filter(model == "qg") %>% pull(mediator_effect_CI),
                                                                                                                            effect == "indirect" ~ me_post %>% filter(model == "qg") %>% pull(indirect_effect_CI),
                                                                                                                            effect == "direct" ~ me_post %>% filter(model == "qg") %>% pull(direct_effect_CI),
                                                                                                                            effect == "total" ~ me_post %>% filter(model == "qg") %>% pull(total_effect_CI)),
                                                                                                          sig = sapply(CI.95, function(x){
                                                                                                            a = as.numeric(gsub("\\(", "", str_split(x, ",")[[1]][1]))
                                                                                                            b = as.numeric(gsub("\\)", "", str_split(x, ",")[[1]][2]))
                                                                                                            if(a*b >= 0){return("*")}else{return("")}
                                                                                                          }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                  targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                    'sig',
                                                                                                                                                                                                                                                                    target = 'row',
                                                                                                                                                                                                                                                                    backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_post %>% filter(model == "qg") %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                         targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red"))) 
        }
      }
    }
  })
  
  output$better_interpretation = renderUI({
    if(input$agree_effect_me == "Single OPE Effect"){
      HTML("<strong>Note</strong>: To better interpret the results, we converted the units of the coefficients, allowing us to interpret the results as follows: <br>", "A <strong>10 percent</strong> increase in <strong>the OPE</strong> is associated with a change of xx mm³ in the mediator, resulting in a change of yy drops in the Pegboard Test. <br>")
    }else{
      HTML("<strong>Note</strong>: To better interpret the results, we converted the units of the coefficients, allowing us to interpret the results as follows: <br>", "Every <strong>10th percentile</strong> increase in the <strong>OPE mixture</strong> is associated with a change of xx mm³ in the mediator, resulting in a change of yy drops in the Pegboard Test. <br>")
    }
  })
  
  output$mediation_table_int = DT::renderDT({
    if(input$agree_effect_me == "Single OPE Effect"){
      if(input$mediation_selection == "pre"){
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_mi_me %>% filter(status == "pre", id == selected_ids) %>% pull(variable)
          selected_type = nodes_mi_me %>% filter(status == "pre", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_mi_me %>% filter(status == "pre", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_mi_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          to = sapply(to, function(x) nodes_mi_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                          estimate = case_when(effect == "mediation" ~ me_pre_table %>% filter(model == "mi") %>% pull(mediator_effect) %>% as.character(),
                                                                                                                               effect == "indirect" ~ me_pre_table %>% filter(model == "mi") %>% pull(indirect_effect),
                                                                                                                               effect == "direct" ~ me_pre_table %>% filter(model == "mi") %>% pull(direct_effect),
                                                                                                                               effect == "total" ~ me_pre_table %>% filter(model == "mi") %>% pull(total_effect)),
                                                                                                          CI.95 = case_when(effect == "mediation" ~ me_pre_table %>% filter(model == "mi") %>% pull(mediator_effect_CI),
                                                                                                                            effect == "indirect" ~ me_pre_table %>% filter(model == "mi") %>% pull(indirect_effect_CI),
                                                                                                                            effect == "direct" ~ me_pre_table %>% filter(model == "mi") %>% pull(direct_effect_CI),
                                                                                                                            effect == "total" ~ me_pre_table %>% filter(model == "mi") %>% pull(total_effect_CI)),
                                                                                                          sig = sapply(CI.95, function(x){
                                                                                                            a = as.numeric(gsub("%", "", gsub("\\(", "", str_split(x, ",")[[1]][1])))
                                                                                                            b = as.numeric(gsub("%", "", gsub("\\)", "", str_split(x, ",")[[1]][2])))
                                                                                                            if(a*b >= 0){return("*")}else{return("")}
                                                                                                          }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                  targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                    'sig',
                                                                                                                                                                                                                                                                    target = 'row',
                                                                                                                                                                                                                                                                    backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_pre_table %>% filter(model == "mi") %>% dplyr::select(-agreed) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                        targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red"))) 
        }
      }else{
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_mi_me %>% filter(status == "post", id == selected_ids) %>% pull(variable)
          selected_type = nodes_mi_me %>% filter(status == "post", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_mi_me %>% filter(status == "post", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_mi_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                           to = sapply(to, function(x) nodes_mi_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                           line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                           estimate = case_when(effect == "mediation" ~ me_post_table %>% filter(model == "mi") %>% pull(mediator_effect) %>% as.character(),
                                                                                                                                effect == "indirect" ~ me_post_table %>% filter(model == "mi") %>% pull(indirect_effect),
                                                                                                                                effect == "direct" ~ me_post_table %>% filter(model == "mi") %>% pull(direct_effect),
                                                                                                                                effect == "total" ~ me_post_table %>% filter(model == "mi") %>% pull(total_effect)),
                                                                                                           CI.95 = case_when(effect == "mediation" ~ me_post_table %>% filter(model == "mi") %>% pull(mediator_effect_CI),
                                                                                                                             effect == "indirect" ~ me_post_table %>% filter(model == "mi") %>% pull(indirect_effect_CI),
                                                                                                                             effect == "direct" ~ me_post_table %>% filter(model == "mi") %>% pull(direct_effect_CI),
                                                                                                                             effect == "total" ~ me_post_table %>% filter(model == "mi") %>% pull(total_effect_CI)),
                                                                                                           sig = sapply(CI.95, function(x){
                                                                                                             a = as.numeric(gsub("%", "", gsub("\\(", "", str_split(x, ",")[[1]][1])))
                                                                                                             b = as.numeric(gsub("%", "", gsub("\\)", "", str_split(x, ",")[[1]][2])))
                                                                                                             if(a*b >= 0){return("*")}else{return("")}
                                                                                                           }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                   targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                     'sig',
                                                                                                                                                                                                                                                                     target = 'row',
                                                                                                                                                                                                                                                                     backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_post_table %>% filter(model == "mi") %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                         targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red"))) 
        }
      }
    }else{
      if(input$mediation_selection == "pre"){
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_qg_me %>% filter(status == "pre", id == selected_ids) %>% pull(variable)
          selected_type = nodes_qg_me %>% filter(status == "pre", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_qg_me %>% filter(status == "pre", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_qg_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          to = sapply(to, function(x) nodes_qg_me %>% filter(status == "pre", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                          line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                          estimate = case_when(effect == "mediation" ~ me_pre_table %>% filter(model == "qg") %>% pull(mediator_effect) %>% as.character(),
                                                                                                                               effect == "indirect" ~ me_pre_table %>% filter(model == "qg") %>% pull(indirect_effect),
                                                                                                                               effect == "direct" ~ me_pre_table %>% filter(model == "qg") %>% pull(direct_effect),
                                                                                                                               effect == "total" ~ me_pre_table %>% filter(model == "qg") %>% pull(total_effect)),
                                                                                                          CI.95 = case_when(effect == "mediation" ~ me_pre_table %>% filter(model == "qg") %>% pull(mediator_effect_CI),
                                                                                                                            effect == "indirect" ~ me_pre_table %>% filter(model == "qg") %>% pull(indirect_effect_CI),
                                                                                                                            effect == "direct" ~ me_pre_table %>% filter(model == "qg") %>% pull(direct_effect_CI),
                                                                                                                            effect == "total" ~ me_pre_table %>% filter(model == "qg") %>% pull(total_effect_CI)),
                                                                                                          sig = sapply(CI.95, function(x){
                                                                                                            a = as.numeric(gsub("%", "", gsub("\\(", "", str_split(x, ",")[[1]][1])))
                                                                                                            b = as.numeric(gsub("%", "", gsub("\\)", "", str_split(x, ",")[[1]][2])))
                                                                                                            if(a*b >= 0){return("*")}else{return("")}
                                                                                                          }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                  targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                    'sig',
                                                                                                                                                                                                                                                                    target = 'row',
                                                                                                                                                                                                                                                                    backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_pre_table %>% filter(model == "qg") %>% dplyr::select(-agreed) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                        targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red")))
        }
      }else{
        selected_ids = input$network_selected
        if (length(selected_ids) > 0){
          selected_name = nodes_qg_me %>% filter(status == "post", id == selected_ids) %>% pull(variable)
          selected_type = nodes_qg_me %>% filter(status == "post", variable == selected_name) %>% pull(group)
          selected_name = sapply(selected_name, function(x){
            return(gsub(" \\(.*\\)", "", x))
          }, USE.NAMES = FALSE)
          links_qg_me %>% filter(status == "post", (from == selected_ids | to == selected_ids)) %>% mutate(from = sapply(from, function(x) nodes_qg_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                           to = sapply(to, function(x) nodes_qg_me %>% filter(status == "post", id == x) %>% pull(variable), USE.NAMES = FALSE),
                                                                                                           line_type = case_when(dashes == TRUE ~ "dash", .default = "solid"),
                                                                                                           estimate = case_when(effect == "mediation" ~ me_post_table %>% filter(model == "qg") %>% pull(mediator_effect) %>% as.character(),
                                                                                                                                effect == "indirect" ~ me_post_table %>% filter(model == "qg") %>% pull(indirect_effect),
                                                                                                                                effect == "direct" ~ me_post_table %>% filter(model == "qg") %>% pull(direct_effect),
                                                                                                                                effect == "total" ~ me_post_table %>% filter(model == "qg") %>% pull(total_effect)),
                                                                                                           CI.95 = case_when(effect == "mediation" ~ me_post_table %>% filter(model == "qg") %>% pull(mediator_effect_CI),
                                                                                                                             effect == "indirect" ~ me_post_table %>% filter(model == "qg") %>% pull(indirect_effect_CI),
                                                                                                                             effect == "direct" ~ me_post_table %>% filter(model == "qg") %>% pull(direct_effect_CI),
                                                                                                                             effect == "total" ~ me_post_table %>% filter(model == "qg") %>% pull(total_effect_CI)),
                                                                                                           sig = sapply(CI.95, function(x){
                                                                                                             a = as.numeric(gsub("%", "", gsub("\\(", "", str_split(x, ",")[[1]][1])))
                                                                                                             b = as.numeric(gsub("%", "", gsub("\\)", "", str_split(x, ",")[[1]][2])))
                                                                                                             if(a*b >= 0){return("*")}else{return("")}
                                                                                                           }, USE.NAMES = FALSE)) %>% dplyr::select(from, to, effect, estimate, CI.95, sig, line_type, status) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                                                                                                                                                                   targets = "_all")))) %>% formatStyle(column = as.numeric(c(4)),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                                                                                                                                                                                                                     'sig',
                                                                                                                                                                                                                                                                     target = 'row',
                                                                                                                                                                                                                                                                     backgroundColor = styleEqual(c("*","**","***"), "lightyellow"))
        }else{
          me_post_table %>% filter(model == "qg") %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                                                                         targets = "_all")))) %>% formatStyle(column = as.numeric(c(7, 9, 11, 13)),color = styleInterval(c(-0.00001), c("green", "red"))) 
        }
      }
    }
  })
  
  output$mediation_plot = renderVisNetwork({
    if(input$agree_effect_me == "Single OPE Effect"){
      if(input$mediation_selection == "pre"){
        vizNetwork(vis.nodes = nodes_mi_me %>% filter(status == "pre"),
                   vis.links = links_mi_me %>% filter(status == "pre")) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else{
        vizNetwork(vis.nodes = nodes_mi_me %>% filter(status == "post"),
                 vis.links = links_mi_me %>% filter(status == "post")) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }
    }else{
      if(input$mediation_selection == "pre"){
        vizNetwork(vis.nodes = nodes_qg_me %>% filter(status == "pre"),
                   vis.links = links_qg_me %>% filter(status == "pre")) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }else{
        vizNetwork(vis.nodes = nodes_qg_me %>% filter(status == "post"),
                 vis.links = links_qg_me %>% filter(status == "post")) %>%
          visEvents(select = "function(nodes) { Shiny.onInputChange('network_selected', nodes.nodes); }")
      }
    }
  })
  #output$note = renderText({
  #  print("Note: For prenatal, birth and three year window, total effect is the significant window specific effect. For the reference window, main & interaction effect shows the significant reference window specific effect, total effect shows the significant effect regardless of windows.")
  #})
}