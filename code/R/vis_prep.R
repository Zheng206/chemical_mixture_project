library(tidyr)
library(dplyr)
library(network)
library(stringr)
library(igraph)
library(visNetwork)
library(shiny)
library(tidyverse)
source("./code/network_app/func.R")

# Individual Results
source_dir = "./Results/Before_MC"
source_dir_agree = "./Results/model_agreement"
source_dir_mediation = "./Results/mediation"
destination_dir = "./code/network_app/network"

# List all files in the source directory
files_to_copy = list.files(source_dir, full.names = TRUE)
files_to_copy_agree = list.files(source_dir_agree, full.names = TRUE)
files_to_copy_mediation = list.files(source_dir_mediation, full.names = TRUE)
# Copy each file to the destination directory
file.copy(files_to_copy, file.path(destination_dir, basename(files_to_copy)), overwrite = TRUE)
file.copy(files_to_copy_agree, file.path(destination_dir, basename(files_to_copy_agree)), overwrite = TRUE)
file.copy(files_to_copy_mediation, file.path(destination_dir, basename(files_to_copy_mediation)), overwrite = TRUE)

# Agreed Models
## create nodes 
df = read_csv("./Results/model_agreement/agreed_results.csv")
### exclude useless regions
ex = c("third_Ventricle", "Left_vessel", "Optic_Chiasm", "Left_Inf_Lat_Vent", "Lateral_Ventricle", "CSF")

df = df %>% filter(!outcome %in% ex)
df$term = sapply(df$term, function(x){
  if(x == "BCEtP"){return("BCEP")}else if(x == "BDCPP"){
    return("BDCIPP")}else if(x == "DBuP"){
      return("DNBP")}else if(x == "DPHP"){return(x)}else{return(x)}
}, USE.NAMES = FALSE)
region = df %>% filter(type %in% c("CV", "CT")) %>% pull(outcome) %>% unique()
anatomy = c("Frontal Lobe", "Occipital Lobe", "Frontal Lobe", "Parietal Lobe", "Frontal Lobe", "Occipital Lobe", "Limbic Lobe", "Insular cortex", "Limbic Lobe", "Frontal Lobe", "Frontal Lobe")
func = c("Motor", "Vision", "Cognition", "Motor", "Language", "Vision", "Cognition", "Cognition", "Cognition", "Motor", "Cognition")
brain_df = data.frame(cbind(region, anatomy, func))
brain_df = df %>% filter(type %in% c("CV", "CT")) %>% left_join(brain_df, by = c("outcome" = "region")) %>% mutate(effect = "total")


df = df %>% mutate(type = case_when(type == "metabolites" ~ "Brain Metabolite",
                                    type == "CV" ~ "Volume",
                                    type == "CT" ~ "Cortical Thickness",
                                    type == "Sub" ~ "Subcortical Volume",
                                    type == "Neuro" ~ "Neurobehavior",
                                    .default = type),
                   effect = "total")

df_qg = df %>% filter(term == "OPE")
df_mi = df %>% filter(term != "OPE")
df_brain_qg = brain_df %>% filter(term == "OPE")
df_brain_mi = brain_df %>% filter(term != "OPE")

### Individual ROIs
nodes_qg = dplyr::union(df_qg %>% dplyr::select(term, type, window) %>% rename(variable=term), 
                        df_qg %>% dplyr::select(outcome, type, window) %>% rename(variable=outcome)) %>%  
  mutate(type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(type == 'Cortical Thickness', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(type == 'Volume', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = type)

links_qg = df_qg %>% 
  mutate(outcome = ifelse(type == 'Cortical Thickness', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(type == 'Volume', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_qg),
         from = purrr::map_int(from, id_node, nodes = nodes_qg), 
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y','5-8y')))

nodes_mi = dplyr::union(df_mi %>% dplyr::select(term, type, window) %>% rename(variable=term), 
                        df_mi %>% dplyr::select(outcome, type, window) %>% rename(variable=outcome)) %>%  
  mutate(type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(type == 'Cortical Thickness', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(type == 'Volume', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = type)

links_mi = df_mi %>% 
  mutate(outcome = ifelse(type == 'Cortical Thickness', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(type == 'Volume', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_mi),
         from = purrr::map_int(from, id_node, nodes = nodes_mi), 
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y','5-8y')))

### anatomy
nodes_brain_qg = dplyr::union(df_brain_qg %>% dplyr::select(term, anatomy, window, type) %>% rename(variable=term), 
                              df_brain_qg %>% dplyr::select(outcome, anatomy, window, type) %>% rename(variable=outcome)) %>%  
  mutate(anatomy = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', anatomy),
         type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(type == 'CT', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(type == 'CV', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = anatomy) %>% dplyr::select(id, variable, group, birth, `3y`, prenatal, `5-8y`)

links_brain_qg = df_brain_qg %>% 
  mutate(outcome = ifelse(type == 'CT', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(type == 'CV', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_brain_qg),
         from = purrr::map_int(from, id_node, nodes = nodes_brain_qg),
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y','5-8y')))

nodes_brain_mi = dplyr::union(df_brain_mi %>% dplyr::select(term, anatomy, window, type) %>% rename(variable=term), 
                              df_brain_mi %>% dplyr::select(outcome, anatomy, window, type) %>% rename(variable=outcome)) %>%  
  mutate(anatomy = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', anatomy),
         type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(type == 'CT', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(type == 'CV', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = anatomy) %>% dplyr::select(id, variable, group, birth, `3y`, prenatal, `5-8y`)

links_brain_mi = df_brain_mi %>% 
  mutate(outcome = ifelse(type == 'CT', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(type == 'CV', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_brain_mi),
         from = purrr::map_int(from, id_node, nodes = nodes_brain_mi), 
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y','5-8y')))

### function
nodes_func_qg = dplyr::union(df_brain_qg %>% dplyr::select(term, func, window, type) %>% rename(variable=term), 
                              df_brain_qg %>% dplyr::select(outcome, func, window, type) %>% rename(variable=outcome)) %>%  
  mutate(func = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', func),
         type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(type == 'CT', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(type == 'CV', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = func) %>% dplyr::select(id, variable, group, birth, `3y`, prenatal, `5-8y`)

links_func_qg = df_brain_qg %>% 
  mutate(outcome = ifelse(type == 'CT', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(type == 'CV', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_func_qg),
         from = purrr::map_int(from, id_node, nodes = nodes_func_qg),
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y','5-8y')))

nodes_func_mi = dplyr::union(df_brain_mi %>% dplyr::select(term, func, window, type) %>% rename(variable=term), 
                              df_brain_mi %>% dplyr::select(outcome, func, window, type) %>% rename(variable=outcome)) %>%  
  mutate(func = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', func),
         type = ifelse(variable %in% c("BCEP", "BDCIPP", "DNBP", "DPHP", "OPE"), 'Chem. Mix.', type)) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = 'window', values_from = 'value', values_fill = 0) %>% 
  mutate(variable = ifelse(type == 'CT', paste(variable, '(CT)'), variable)) %>% 
  mutate(variable = ifelse(type == 'CV', paste(variable, '(Vol.)'), variable)) %>% 
  mutate(id = seq_len(nrow(.)), 
         .before = 'variable') %>% 
  rename(group = func) %>% dplyr::select(id, variable, group, birth, `3y`, prenatal, `5-8y`)

links_func_mi = df_brain_mi %>% 
  mutate(outcome = ifelse(type == 'CT', paste(outcome, '(CT)'), outcome)) %>% 
  mutate(outcome = ifelse(type == 'CV', paste(outcome, '(Vol.)'), outcome)) %>% 
  rename(from = term,to = outcome) %>% 
  mutate(to = purrr::map_int(to, id_node, nodes = nodes_func_mi),
         from = purrr::map_int(from, id_node, nodes = nodes_func_mi), 
         dashes = effect == 'interaction', 
         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
  dplyr::select(from, to, window, effect, dashes, estimate) %>% 
  arrange(factor(window, levels=c('prenatal', 'birth', '3y','5-8y')))


global = c("TotalGrayVol", "BrainSegVol", "SubCortGrayVol", "SupraTentorialVol", "Lateral_Ventricle", "Cerebellum_White_Matter", "Cerebellum_Cortex", "CortexVol", "CerebralWhiteMatterVol", "surface_area", "total_brain", "MeanThickness (CT)")

nodes_qg$group = sapply(1:nrow(nodes_qg), function(i){
  case_when(nodes_qg[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_qg[[i, "group"]])
}, USE.NAMES = FALSE)

nodes_mi$group = sapply(1:nrow(nodes_mi), function(i){
  case_when(nodes_mi[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_mi[[i, "group"]])
}, USE.NAMES = FALSE)

nodes_brain_qg$group = sapply(1:nrow(nodes_brain_qg), function(i){
  case_when(nodes_brain_qg[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_brain_qg[[i, "group"]])
}, USE.NAMES = FALSE)

nodes_brain_mi$group = sapply(1:nrow(nodes_brain_mi), function(i){
  case_when(nodes_brain_mi[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_brain_mi[[i, "group"]])
}, USE.NAMES = FALSE)

nodes_func_qg$group = sapply(1:nrow(nodes_func_qg), function(i){
  case_when(nodes_func_qg[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_func_qg[[i, "group"]])
}, USE.NAMES = FALSE)

nodes_func_mi$group = sapply(1:nrow(nodes_func_mi), function(i){
  case_when(nodes_func_mi[[i, "variable"]] %in% global ~ "Global",
            .default = nodes_func_mi[[i, "group"]])
}, USE.NAMES = FALSE)

write_csv(nodes_mi, "./code/network_app/network/nodes_mi.csv")
write_csv(nodes_qg, "./code/network_app/network/nodes_qg.csv")
write_csv(nodes_brain_qg, "./code/network_app/network/nodes_brain_qg.csv")
write_csv(nodes_brain_mi, "./code/network_app/network/nodes_brain_mi.csv")
write_csv(nodes_func_qg, "./code/network_app/network/nodes_func_qg.csv")
write_csv(nodes_func_mi, "./code/network_app/network/nodes_func_mi.csv")
write_csv(links_mi, "./code/network_app/network/links_mi.csv")
write_csv(links_qg, "./code/network_app/network/links_qg.csv")
write_csv(links_brain_qg, "./code/network_app/network/links_brain_qg.csv")
write_csv(links_brain_mi, "./code/network_app/network/links_brain_mi.csv")
write_csv(links_func_qg, "./code/network_app/network/links_func_qg.csv")
write_csv(links_func_mi, "./code/network_app/network/links_func_mi.csv")


# Mediation Analysis
mediation_df_pre = read_csv("./Results/mediation/mediation_result_pre.csv") %>% mutate(type = case_when(
  type == "metabolites" ~ "Brain Metabolite",
  type == "CV" ~ "Volume",
  type == "CT" ~ "Cortical Thickness",
  type == "Sub" ~ "Subcortical Volume",
  type == "Neuro" ~ "Neurobehavior",
  .default = type 
)) %>% dplyr::select(-agreed) %>% mutate(status = "pre")

mediation_df_post = read_csv("./Results/mediation/mediation_result_post.csv") %>% mutate(type = case_when(
  type == "metabolites" ~ "Brain Metabolite",
  type == "CV" ~ "Volume",
  type == "CT" ~ "Cortical Thickness",
  type == "Sub" ~ "Subcortical Volume",
  type == "Neuro" ~ "Neurobehavior",
  .default = type 
)) %>% mutate(status = "post")

mediation_df = rbind(mediation_df_pre, mediation_df_post) %>% mutate(OPE = case_when(OPE == "BCETP" ~ "BCEP",
                                                                                     .default = OPE))

mediation_node_link_pre = function(mediation_df, ope){
  ope_mediator = mediation_df %>% filter(OPE %in% ope) %>% dplyr::select(OPE, mediator, type, mediator_effect) %>% 
    add_column(from_type = "Chem. Mix.", .after = "OPE") %>% rename("to_type" = "type")
  ope_y = mediation_df %>% filter(OPE %in% ope) %>% dplyr::select(OPE, y, direct_effect) %>% add_column(from_type = "Chem. Mix.", .after = "OPE") %>% 
    add_column(to_type = "Neurobehavior", .after = "y")
  mediator_y = mediation_df %>% filter(OPE %in% ope) %>% dplyr::select(mediator, type, y, indirect_effect) %>% rename("from_type" = "type") %>% 
    add_column(to_type = "Neurobehavior", .after = "y")
  ope_y_total = mediation_df %>% filter(OPE %in% ope) %>% dplyr::select(OPE, y, total_effect) %>% add_column(from_type = "Chem. Mix.", .after = "OPE") %>% 
    add_column(to_type = "Neurobehavior", .after = "y")
  
  nodes = rbind(ope_mediator %>% dplyr::select(OPE, from_type) %>% rename("variable" = "OPE", "type" = "from_type"),
                ope_mediator %>% dplyr::select(mediator, to_type) %>% rename("variable" = "mediator", "type" = "to_type"),
                ope_y %>% dplyr::select(OPE, from_type) %>% rename("variable" = "OPE", "type" = "from_type"),
                ope_y %>% dplyr::select(y, to_type) %>% rename("variable" = "y", "type" = "to_type"),
                mediator_y %>% dplyr::select(mediator, from_type) %>% rename("variable" = "mediator", "type" = "from_type"),
                mediator_y %>% dplyr::select(y, to_type) %>% rename("variable" = "y", "type" = "to_type"),
                ope_y_total %>% dplyr::select(OPE, from_type) %>% rename("variable" = "OPE", "type" = "from_type"),
                ope_y_total %>% dplyr::select(y, to_type) %>% rename("variable" = "y", "type" = "to_type")) %>% distinct() %>% 
    mutate(variable = ifelse(type == 'Cortical Thickness', paste(variable, '(CT)'), variable)) %>% 
    mutate(variable = ifelse(type == 'Volume', paste(variable, '(Vol.)'), variable)) %>% 
    mutate(id = seq_len(nrow(.)), 
           .before = 'variable') %>% 
    rename(group = type)
  
  links = rbind(ope_mediator %>% mutate(effect = "mediation") %>% 
                  mutate(mediator = ifelse(to_type == 'Cortical Thickness', paste(mediator, '(CT)'), mediator)) %>% 
                  mutate(mediator = ifelse(to_type == 'Volume', paste(mediator, '(Vol.)'), mediator)) %>% 
                  rename(from = OPE,to = mediator, estimate = mediator_effect) %>% 
                  mutate(to = purrr::map_int(to, id_node, nodes = nodes),
                         from = purrr::map_int(from, id_node, nodes = nodes), 
                         dashes = TRUE, 
                         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
                  dplyr::select(from, to, effect, dashes, estimate),
                ope_y %>% mutate(effect = "direct") %>% 
                  rename(from = OPE,to = y, estimate = direct_effect) %>% 
                  mutate(to = purrr::map_int(to, id_node, nodes = nodes),
                         from = purrr::map_int(from, id_node, nodes = nodes), 
                         dashes = TRUE, 
                         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
                  dplyr::select(from, to, effect, dashes, estimate),
                mediator_y %>% mutate(effect = "indirect") %>% 
                  mutate(mediator = ifelse(from_type == 'Cortical Thickness', paste(mediator, '(CT)'), mediator)) %>% 
                  mutate(mediator = ifelse(from_type == 'Volume', paste(mediator, '(Vol.)'), mediator)) %>% 
                  rename(from = mediator,to = y, estimate = indirect_effect) %>% 
                  mutate(to = purrr::map_int(to, id_node, nodes = nodes),
                         from = purrr::map_int(from, id_node, nodes = nodes), 
                         dashes = TRUE, 
                         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
                  dplyr::select(from, to, effect, dashes, estimate),
                ope_y_total %>% mutate(effect = "total") %>% 
                  rename(from = OPE,to = y, estimate = total_effect) %>% 
                  mutate(to = purrr::map_int(to, id_node, nodes = nodes),
                         from = purrr::map_int(from, id_node, nodes = nodes), 
                         dashes = FALSE, 
                         estimate = ifelse(sign(estimate) == 1, 'positive', 'negative')) %>% 
                  dplyr::select(from, to, effect, dashes, estimate))
  return(list("nodes" = nodes, "links" = links))
}


nodes_mi_me = lapply(unique(mediation_df$status), function(x){
  return(mediation_node_link_pre(mediation_df %>% filter(status == x), "BCEP")$nodes %>% mutate(status = x))
}) %>% bind_rows()
  
links_mi_me = lapply(unique(mediation_df$status), function(x){
  return(mediation_node_link_pre(mediation_df %>% filter(status == x), "BCEP")$links %>% mutate(status = x))
}) %>% bind_rows()

nodes_qg_me = lapply(unique(mediation_df$status), function(x){
  return(mediation_node_link_pre(mediation_df %>% filter(status == x), "OPE")$nodes %>% mutate(status = x))
}) %>% bind_rows()

links_qg_me = lapply(unique(mediation_df$status), function(x){
  return(mediation_node_link_pre(mediation_df %>% filter(status == x), "OPE")$links %>% mutate(status = x))
}) %>% bind_rows()

write_csv(nodes_mi_me, "./code/network_app/network/nodes_mi_me.csv")
write_csv(links_mi_me, "./code/network_app/network/links_mi_me.csv")
write_csv(nodes_qg_me, "./code/network_app/network/nodes_qg_me.csv")
write_csv(links_qg_me, "./code/network_app/network/links_qg_me.csv")

