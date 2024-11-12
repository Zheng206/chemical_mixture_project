# checked
lrh_correlation = function(feature, name, aparc_lh, aparc_rh){
  lh_feature = paste0("lh_", feature, name)
  rh_feature = paste0("rh_", feature,name)
  correlation = cor(x = aparc_lh[[lh_feature]], y = aparc_rh[[rh_feature]])
  return(correlation)
}

# checked
lrh_diff = function(feature, name, aparc_lh, aparc_rh){
  lh_feature = paste0("lh_", feature, name)
  rh_feature = paste0("rh_", feature, name)
  lh = cbind(aparc_lh[[lh_feature]],rep("left",length(aparc_lh[[lh_feature]])))
  rh = cbind(aparc_rh[[rh_feature]],rep("right",length(aparc_rh[[rh_feature]])))
  df = data.frame(rbind(lh, rh))
  colnames(df) = c(feature, "hemisphere")
  df[[feature]] = as.numeric(df[[feature]])
  return(df)
}

# checked
combine_lrh = function(feature, uncor, name, aparc_lh, aparc_rh){
  lh_feature = paste0("lh_", feature, name)
  rh_feature = paste0("rh_", feature, name)
  df = data.frame()
  df = cbind(aparc_lh[lh_feature], aparc_rh[rh_feature])
  if (! feature %in% uncor){
    df[feature] = (df[lh_feature] + df[rh_feature])/2
    return(df[feature])
  }else{
    return(df)
  }
}

gen_com_df = function(type, aparc_lh, aparc_rh, Subcortical){
  if(type == "CV"){
    features = sapply(colnames(aparc_lh)[-1], function(x) str_split(x, "_")[[1]][2], USE.NAMES = FALSE)
    correlation = sapply(features, lrh_correlation, name = "_volume", aparc_lh = aparc_lh, aparc_rh = aparc_rh, USE.NAMES = FALSE)
    cor_df = data.frame(cbind(features, correlation))
    cor_df$correlation = as.numeric(cor_df$correlation)
    uncor_features = cor_df %>% filter(correlation < 0.5) %>% pull(features)
    cor_features = cor_df %>% filter(correlation >= 0.5) %>% pull(features)
    combined_df = lapply(features, combine_lrh, uncor = uncor_features, name = "_volume", aparc_lh = aparc_lh, aparc_rh = aparc_rh) %>% bind_cols()
    combined_df = cbind(combined_df,aparc_lh$SubjectID)
    combined_df = combined_df[,c(39,1:38)]
    colnames(combined_df) = c("SubjectID", colnames(combined_df)[2:39])
    combined_df = combined_df %>% left_join(ICV, by = c("SubjectID"))
    combined_df = combined_df %>% rename("lh_bankssts" = "lh_bankssts_volume",
                                         "rh_bankssts" = "rh_bankssts_volume",
                                         "lh_caudalanteriorcingulate" = "lh_caudalanteriorcingulate_volume",
                                         "rh_caudalanteriorcingulate" = "rh_caudalanteriorcingulate_volume",
                                         "lh_frontalpole" = "lh_frontalpole_volume",
                                         "rh_frontalpole" = "rh_frontalpole_volume",
                                         "lh_temporalpole" = "lh_temporalpole_volume",
                                         "rh_temporalpole" = "rh_temporalpole_volume")
    outcomes = c("lh_bankssts","rh_bankssts","lh_caudalanteriorcingulate","rh_caudalanteriorcingulate","lh_frontalpole","rh_frontalpole","lh_temporalpole", "rh_temporalpole", cor_features)
    }else if(type == "CT"){
    features = sapply(colnames(aparc_lh)[-1], function(x) str_split(x, "_")[[1]][2], USE.NAMES = FALSE)
    correlation = sapply(features, lrh_correlation, name = "_thickness", aparc_lh = aparc_lh, aparc_rh = aparc_rh, USE.NAMES = FALSE)
    cor_df = data.frame(cbind(features, correlation))
    cor_df$correlation = as.numeric(cor_df$correlation)
    uncor_features = cor_df %>% filter(correlation < 0.5) %>% pull(features)
    cor_features = cor_df %>% filter(correlation >= 0.5) %>% pull(features)
    combined_df = lapply(features, combine_lrh, uncor = uncor_features, name = "_thickness", aparc_lh = aparc_lh, aparc_rh = aparc_rh) %>% bind_cols()
    combined_df = cbind(combined_df,aparc_lh$SubjectID)
    combined_df = combined_df[,c(45,1:44)]
    colnames(combined_df) = c("SubjectID", colnames(combined_df)[2:45])
    colnames(combined_df)[which(grepl("_thickness", colnames(combined_df)))] = sapply(colnames(combined_df)[which(grepl("_thickness", colnames(combined_df)))], function(x) paste0(str_split(x, "_")[[1]][1],"_",str_split(x, "_")[[1]][2]), USE.NAMES = FALSE)
    combined_df = combined_df %>% left_join(ICV, by = c("SubjectID"))
    outlier_ind =c(3,4,7,14)
    uncor_labels = c(paste0("lh_",uncor_features), paste0("rh_",uncor_features))
    uncor_labels = uncor_labels[order(str_replace(uncor_labels, 'lh_|rh_', ''))]
    outcomes = c(uncor_labels, cor_features)
  }else if(type == "subroi"){
    combined_df= Subcortical
    lh_area = aparc_lh[36]
    rh_area = aparc_rh[36]
    area = (lh_area + rh_area)/2
    colnames(area) = "surface_area"
    ICV = combined_df[67]
    include = combined_df[c(1,10:12, 15, 34, 41:47, 56:58)]
    left = colnames(combined_df)[which(grepl("Left-*|^lh*", colnames(combined_df)))]
    left = left[which(!grepl("*-WM-hypointensities$|*SurfaceHoles$", left))]
    right = colnames(combined_df)[which(grepl("Right-*|^rh*", colnames(combined_df)))]
    right = right[which(!grepl("*-WM-hypointensities$|*SurfaceHoles$", right))]
    roi_name = sapply(left, function(x){
      if(grepl("Left-*", x)){return(gsub("Left-","",x))}else{return(gsub("lh", "", x))}
    }, USE.NAMES = FALSE)
    corr = sapply(1:length(roi_name), function(i) cor(x = combined_df[[left[i]]], y = combined_df[[right[i]]]), USE.NAMES = FALSE)
    cor_df = data.frame(cbind(roi_name, corr))
    uncor_name = cor_df %>% filter(corr < 0.5) %>% pull(roi_name)
    uncor_name = c(left[which(roi_name %in% uncor_name)], right[which(roi_name %in% uncor_name)])
    cor_name = cor_df %>% filter(corr >= 0.5) %>% pull(roi_name)
    cor_roi = lapply(cor_name, function(x){
      roi = combined_df %>% dplyr::select(matches(x)) %>% mutate(x = rowMeans(.[c(1:2)])) %>% dplyr::select(x) 
      colnames(roi) = x
      return(roi)
    }) %>% bind_cols()
    combined_df = cbind(include, combined_df[uncor_name], cor_roi)
    colnames(combined_df) = sapply(colnames(combined_df), function(x) gsub("-", "_", x), USE.NAMES = FALSE)
    colnames(combined_df)[2] = "third_Ventricle"
    colnames(combined_df)[3] = "fourth_Ventricle"
    colnames(combined_df)[6] = "fifth_Ventricle"
    combined_df = combined_df[-6]
    combined_df = cbind(combined_df, area)
    combined_df$total_brain = combined_df$BrainSegVol - combined_df$Brain_Stem
    features = colnames(combined_df)[-1]
    outcomes = features
    combined_df = cbind(combined_df, ICV)
  }
  return(list("combined_df" = combined_df, "outcomes" = outcomes))
}

transform = function(x){
  if (x == 0){
    return("0.000")
  }else if(x == 1){
    return("1.000")
  }else{return(sprintf("%.3f", x))}
}

# checked
mean_w_na = function(i,df,t){
  a = df[i,t]
  a[is.na(a) | a == "Inf"] = NA
  return(mean(a[!is.na(a)])) 
}

# checked
data_period_average = function(data, chemical, t, new_name){
  data = data %>% dplyr::select('subject_id', 'visit', chemical) 
  data_wide = data %>% filter(visit %in% t) %>% na.omit() %>% pivot_wider(names_from = visit, values_from = chemical)
  data_wide['average'] = sapply(1:nrow(data_wide), mean_w_na, df = data_wide, t = t, USE.NAMES = FALSE)
  data_wide = data_wide %>% dplyr::select(subject_id, average) 
  colnames(data_wide) = c('subject_id', new_name)
  return(data_wide)
}

# checked
data_selection = function(t, period, outcome_df, dv_wide, cotinine){
  bcetp = data_period_average(dv_wide, 'bcetp',t, new_name = 'bcetp')
  bdcpp = data_period_average(dv_wide, 'bdcpp',t, new_name = 'bdcpp')
  dbup = data_period_average(dv_wide, 'dbup',t, new_name = 'dbup')
  dphp = data_period_average(dv_wide, 'dphp',t, new_name = 'dphp')
  cotinine = cotinine %>% filter(visit %in% t) %>% group_by(subject_id) %>% summarize(cotinine = mean(cotinine))
  co_df = bcetp %>% left_join(bdcpp, by = "subject_id") %>% left_join(dbup, by = "subject_id") %>% left_join(dphp, by = "subject_id") %>% left_join(cotinine, by = "subject_id") %>% left_join(outcome_df, by = c("subject_id" = "SubjectID"))
  co_df["period"] = period
  return(co_df)
}

# generate dataframes
data_gen = function(outcome_df, ...){
  ## 5 windows
  pre_df = data_selection(c('16w','26w'), "prenatal", outcome_df, ...)
  pre_df = pre_df %>% left_join(cov_df, by = "subject_id")
  three_y_df = data_selection(c("m12","m24", "m36"), "three_year", outcome_df, ...)
  three_y_df = three_y_df %>% left_join(cov_df, by = "subject_id")
  five_eight_y_df = data_selection(c("m60", "p3"), "five_to_eight_year", outcome_df, ...)
  five_eight_y_df = five_eight_y_df %>% left_join(cov_df, by = "subject_id")
  eight_y_df = data_selection(c("m12","m24", "m36", "m60", "p3"), "eight_year", outcome_df, ...)
  eight_y_df = eight_y_df %>% left_join(cov_df, by = "subject_id")
  birth_df = data_selection(c("birth"), "birth", outcome_df, ...)
  birth_df = birth_df %>% left_join(cov_df, by = "subject_id")
  ## 3 window differences
  three_y_diff = pre_df %>% left_join(three_y_df[c(1:6)], by = "subject_id", suffix = c("_pre", "_3y")) %>% mutate(bcetp = bcetp_3y - bcetp_pre,
                                                                                                                   bdcpp = bdcpp_3y - bdcpp_pre,
                                                                                                                   dbup = dbup_3y - dbup_pre,
                                                                                                                   dphp = dphp_3y - dphp_pre,
                                                                                                                   cotinine = cotinine_3y - cotinine_pre) %>% dplyr::select(names(pre_df)[c(1:12)])
  
  three_y_diff = three_y_diff %>% left_join(cov_df, by = "subject_id")
  five_y_diff = pre_df %>% left_join(five_eight_y_df[c(1:6)], by = "subject_id", suffix = c("_pre", "_5y")) %>% mutate(bcetp = bcetp_5y - bcetp_pre,
                                                                                                                       bdcpp = bdcpp_5y - bdcpp_pre,
                                                                                                                       dbup = dbup_5y - dbup_pre,
                                                                                                                       dphp = dphp_5y - dphp_pre,
                                                                                                                       cotinine = cotinine_5y - cotinine_pre) %>% dplyr::select(names(pre_df)[c(1:12)])
  five_y_diff = five_y_diff %>% left_join(cov_df, by = "subject_id")
  pre_child_diff = pre_df %>% left_join(eight_y_df[c(1:6)], by = "subject_id", suffix = c("_pre", "_child")) %>% mutate(bcetp = bcetp_child - bcetp_pre,
                                                                                                                        bdcpp = bdcpp_child - bdcpp_pre,
                                                                                                                        dbup = dbup_child - dbup_pre,
                                                                                                                        dphp = dphp_child - dphp_pre,
                                                                                                                        cotinine = cotinine_child - cotinine_pre) %>% dplyr::select(names(pre_df)[c(1:12)])
  pre_child_diff = pre_child_diff %>% left_join(cov_df, by = "subject_id")
  gee_df = rbind(pre_df, birth_df, three_y_df, five_eight_y_df)
  gee_df$period = as.factor(gee_df$period)
  gee_df$period = relevel(gee_df$period, ref = 'five_to_eight_year')
  return(list("pre_df" = pre_df, "three_y_df" = three_y_df, "five_eight_y_df" = five_eight_y_df, "eight_y_df" = eight_y_df, "birth_df" = birth_df, "three_y_diff" = three_y_diff, "five_y_diff" = five_y_diff, "pre_child_diff" = pre_child_diff, "gee_df" = gee_df))
}

uni_model = function(outcome, OPE, rm_outliers, df, plot = FALSE, po = FALSE, type = "lm", ...){
  if(type == "lm"){
    if(! po){
      if(rm_outliers){
        model = remove_outliers(outcome,  paste0(" ~ ", OPE), df)[[1]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ ", OPE))
        model = lm(fml, data = df)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }
    }else{
      if(rm_outliers){
        model = remove_outliers(outcome,  paste0(" ~ ", OPE, "+ I(", OPE, "^2)"), df)[[1]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ ", OPE, "+ I(", OPE, "^2)"))
        model = lm(fml, data = df)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }
    }
  }else if(type == "glm"){
    if(rm_outliers){
        model = remove_outliers(outcome,  paste0(" ~ ", OPE), df, type = "glm", ...)[[1]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ ", OPE))
        model = glm(fml, data = df, ...)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }
  }
    
  if(plot == TRUE){
    print(autoplot(model, which = 1:4))
  }
  return(list(p, term, model))
}


linear_model = function(outcome, md, rm_outliers, df, plot = FALSE, type = "lm", oc = "other", ...){
  if (type == "lm") {
    if (md == 'linear') {
      if(rm_outliers){
        model = remove_outliers(outcome,  "~ bcetp + bdcpp + dbup + dphp + cotinine + sex + race + home_score_total + mom_edu", df)[[1]]
        df_new = remove_outliers(outcome,  "~ bcetp + bdcpp + dbup + dphp + cotinine + sex + race + home_score_total + mom_edu", df)[[3]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ bcetp + bdcpp + dbup + dphp + cotinine + sex + race + home_score_total + mom_edu"))
        model = lm(fml, data = df)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }
    }else if (md == 'log'){
      if(rm_outliers){
        model = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + cotinine + sex + race + home_score_total + mom_edu", df)[[1]]
        df_new = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + cotinine + sex + race + home_score_total + mom_edu", df)[[3]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + cotinine + sex + race + home_score_total + mom_edu"))
        model = lm(fml, data = df)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)}
    }else if(md == "log_quadratic"){
      if(rm_outliers){
        model = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + I(log(bcetp)^2) + I(log(bdcpp)^2) + I(log(dbup)^2) + I(log(dphp)^2) + cotinine + sex + race + home_score_total + mom_edu", df)[[1]]
        df_new = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + I(log(bcetp)^2) + I(log(bdcpp)^2) + I(log(dbup)^2) + I(log(dphp)^2) + cotinine + sex + race + home_score_total + mom_edu", df)[[3]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + I(log(bcetp)^2) + I(log(bdcpp)^2) + I(log(dbup)^2) + I(log(dphp)^2)  + cotinine + sex + race + home_score_total + mom_edu"))
        model = lm(fml, data = df)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)}
    }
  }else if(type == "glm"){
    if (md == 'linear') {
      if(rm_outliers){
        model = remove_outliers(outcome,  "~ bcetp + bdcpp + dbup + dphp + cotinine + sex + race + home_score_total + mom_edu", df, type = "glm", ...)[[1]]
        df_new = remove_outliers(outcome,  "~ bcetp + bdcpp + dbup + dphp + cotinine + sex + race + home_score_total + mom_edu", df, type = "glm", ...)[[3]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ bcetp + bdcpp + dbup + dphp + cotinine + sex + race + home_score_total + mom_edu"))
        model = glm(fml, data = df, ...)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }
    }else if (md == 'log'){
      if(rm_outliers){
        model = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + cotinine + sex + race + home_score_total + mom_edu", df, type = "glm", ...)[[1]]
        df_new = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + cotinine + sex + race + home_score_total + mom_edu", df, type = "glm", ...)[[3]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + cotinine + sex + race + home_score_total + mom_edu"))
        model = glm(fml, data = df, ...)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)}
    }else if(md == "log_quadratic"){
      if(rm_outliers){
        model = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + I(log(bcetp)^2) + I(log(bdcpp)^2) + I(log(dbup)^2) + I(log(dphp)^2) + cotinine + sex + race + home_score_total + mom_edu", df, type = "glm", ...)[[1]]
        df_new = remove_outliers(outcome,  " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + I(log(bcetp)^2) + I(log(bdcpp)^2) + I(log(dbup)^2) + I(log(dphp)^2) + cotinine + sex + race + home_score_total + mom_edu", df, type = "glm", ...)[[3]]
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)
      }else{
        fml = as.formula(paste0(outcome, " ~ log(bcetp) + log(bdcpp) + log(dbup) + log(dphp) + I(log(bcetp)^2) + I(log(bdcpp)^2) + I(log(dbup)^2) + I(log(dphp)^2)  + cotinine + sex + race + home_score_total + mom_edu"))
        model = glm(fml, data = df, ...)
        p = model %>% tidy() %>% pull(p.value) %>% round(3)
        term = model %>% tidy() %>% pull(term)}
    }
  }

  if(plot == TRUE) {
    print(autoplot(model, which = 1:4))
  }

  if(oc == "brain_region"){
    model = update(model, . ~ . + EstimatedTotalIntraCranialVol)
    p = model %>% tidy() %>% pull(p.value) %>% round(3)
    term = model %>% tidy() %>% pull(term)
  }
  return(list(p, term, model))
}


IQR_outliers = function(x){
  a = na.omit(x) 
  Q1 = quantile(a, .25)
  Q3 = quantile(a, .75)
  IQR = IQR(a)
  upper = Q3 + 1.5*IQR
  lower = Q1 - 1.5*IQR
  return(list(lower, upper))
  #which(a < lower | a > upper)
}

outlier_pct = function(outcome, df){
  df_new = df %>% filter(df[[outcome]] >= IQR_outliers(df[[outcome]])[[1]] & df[[outcome]] <= IQR_outliers(df[[outcome]])[[2]])
  outliers = df %>% filter(df[[outcome]] < IQR_outliers(df[[outcome]])[[1]] | df[[outcome]] > IQR_outliers(df[[outcome]])[[2]])
  n_out = nrow(outliers)/nrow(na.omit(df[outcome]))
  return(n_out)
}

remove_outliers = function(outcome, v_str, df, type = "lm", ...){
  df_new = df %>% filter(df[[outcome]] >= IQR_outliers(df[[outcome]])[[1]] & df[[outcome]] <= IQR_outliers(df[[outcome]])[[2]])
  outliers = df %>% filter(df[[outcome]] < IQR_outliers(df[[outcome]])[[1]] | df[[outcome]] > IQR_outliers(df[[outcome]])[[2]])
  fml = as.formula(paste0(outcome, v_str))
  if(type == "lm"){
    model = lm(fml, data = df_new)}else if(type == "glm"){
    model = glm(fml, data = df_new, ...)  
    }
  return(list(model, outliers, df_new))
}

find_outliers = function(outliers, df_long, outcome){
  outlier_index = which(df_long[[outcome]] %in% outliers[[outcome]])
  sjd = df_long %>% filter(df_long[[outcome]] %in% outliers[[outcome]]) %>% pull(subject_id)
  n_outliers = nrow(outliers)
  df_long["outcomes"] = sapply(df_long$subject_id, function(x){
    if (x %in% sjd){return("outlier")}else{return("normal value")}}, USE.NAMES = FALSE)
  return(list(df_long, n_outliers))
}

p_adjust = function(i, p_table, ...){
  p = unlist(as.vector(p_table[-1][i,]))# removing  the term column
  p_adjusted = p.adjust(p,...)
  return(p_adjusted)
}

# checked
cov_name = function(x){
  if(grepl("sex",x)){return(gsub("sex","sex | ", x))}else if(grepl("race",x)){return(gsub("race","race | ", x))}else if(grepl("mom_edu",x)){return(gsub("mom_edu","mom_edu | ", x))}else{return(x)}
}

font_color = function(x, thre){
  if(is.na(x)){return('white')}else if(x == "<0.001"){return("red")}else{
    if (as.numeric(x) < thre){return("red")}else{return("black")}
  }
}

# checked
gee_model = function(ope, outcome, rm_outliers = FALSE, oc = "other", gee_df, ...){
  if(rm_outliers){
    gee_df_new = gee_df %>% filter(gee_df[[outcome]] >= IQR_outliers(gee_df[[outcome]])[[1]] & gee_df[[outcome]] <= IQR_outliers(gee_df[[outcome]])[[2]])
    formula = as.formula(paste0(outcome, " ~  log(",ope, ")*period + cotinine + sex + race + home_score_total + mom_edu"))
    model = geeglm(formula, data = na.omit(gee_df_new), id = subject_id, corstr="independence", ...)
    stats = model %>% tidy() %>% filter(grepl('log', term)) %>% dplyr::select("term", "p.value") %>% filter(!term == paste0("log(",ope, ")"))
    stats_ope = model %>% tidy() %>% filter(term == paste0("log(",ope, ")")) %>% dplyr::select("term", "p.value")
    colnames(stats) = c("term", outcome)
    colnames(stats_ope) = c("term", outcome)
  }else{
    formula = as.formula(paste0(outcome, " ~  log(",ope, ")*period + cotinine + sex + race + home_score_total + mom_edu"))
    model = geeglm(formula, data = na.omit(gee_df), id = subject_id, corstr="independence", ...)
    stats = model %>% tidy() %>% filter(grepl('log', term)) %>% dplyr::select("term", "p.value") %>% filter(!term == paste0("log(",ope, ")"))
    stats_ope = model %>% tidy() %>% filter(term == paste0("log(",ope, ")")) %>% dplyr::select("term", "p.value")
    colnames(stats) = c("term", outcome)
    colnames(stats_ope) = c("term", outcome)}

  if(oc == "brain_region"){
    model = update(model, . ~ . + EstimatedTotalIntraCranialVol)
    stats = model %>% tidy() %>% filter(grepl('log', term)) %>% dplyr::select("term", "p.value") %>% filter(!term == paste0("log(",ope, ")"))
    stats_ope = model %>% tidy() %>% filter(term == paste0("log(",ope, ")")) %>% dplyr::select("term", "p.value")
    colnames(stats) = c("term", outcome)
    colnames(stats_ope) = c("term", outcome)
  }

  return(list(stats,stats_ope,model))
  }

# checked
gee_coef = function(x, period){
  ope = x %>% tidy() %>% pull(term) 
  ope = ope[2]
  coef_df = x %>% tidy(conf.int = TRUE, conf.level = 0.90) %>% dplyr::select(term, conf.low, estimate, conf.high, p.value, std.error) %>% filter(grepl('log', term)) %>% filter(term == paste0(ope, ":period",period) | term == ope)
  interactive_p = coef_df %>% pull(p.value)
  if(interactive_p[2] < 0.1){
    result = coef_df %>% mutate(conf.low = sum(estimate) - qt(0.95, glance(x)$n.clusters - 1)*sqrt(coef_df$std.error[1]^2 + coef_df$std.error[2]^2), estimate = sum(estimate), conf.high = estimate + qt(0.95, glance(x)$n.clusters - 1)*sqrt(coef_df$std.error[1]^2 + coef_df$std.error[2]^2)) %>% filter(term == ope) %>% mutate(p.value = case_when(
      interactive_p[2] < 0.1 & interactive_p[2] >= 0.05 ~ "*",
      interactive_p[2] < 0.05 & interactive_p[2] >= 0.01 ~ "**",
      interactive_p[2] < 0.01 ~ "***"
    )) %>% mutate("window" = period) %>% dplyr::select(term, conf.low, estimate, conf.high, p.value, window)
    colnames(result) = c("OPE","95%CI.low","Estimate","95%CI.high", "interaction.sig","window")
    result = result[c(1,6,2:5)]
    p_total = 2*pt(-abs(result$Estimate/sqrt(coef_df$std.error[1]^2 + coef_df$std.error[2]^2)), df = glance(x)$n.clusters - 1)
    result = result %>% mutate(total.sig = case_when(p_total < 0.05 & p_total >= 0.01 ~ "*",
                                                     p_total < 0.01 & p_total >= 0.001 ~ "**",
                                                     p_total < 0.001 ~ "***"), p_total = p_total) 
    result = result %>% mutate(`95%CI.low` = sprintf("%.3f", `95%CI.low`),
                               Estimate = sprintf("%.3f", Estimate),
                               `95%CI.high` = sprintf("%.3f", `95%CI.high`))
    return(result)
  }else{
    result = coef_df %>% filter(term == ope) %>% mutate(p.value = NA) %>% mutate("window" = period) %>% dplyr::select(term, conf.low, estimate, conf.high, p.value, window)
    colnames(result) = c("OPE","95%CI.low","Estimate","95%CI.high", "interaction.sig","window")
    result = result[c(1,6,2:5)]
    p_total = interactive_p[1]
    result = result %>% mutate(total.sig = case_when(p_total < 0.05 & p_total >= 0.01 ~ "*",
                                                     p_total < 0.01 & p_total >= 0.001 ~ "**",
                                                     p_total < 0.001 ~ "***"), p_total = p_total) 
    result = result %>% mutate(`95%CI.low` = sprintf("%.3f", `95%CI.low`),
                               Estimate = sprintf("%.3f", Estimate),
                               `95%CI.high` = sprintf("%.3f", `95%CI.high`))
    return(result)
  }
}

gee_coef_run = function(gee_table, outcomes, coeff_list, period_name){
  ind_sig_c = c()
  total_sig_c = c()
  outcome_c = c()
  coef_df_list = data.frame()
  for (outcome in outcomes){
    ind = which(gee_table["outcomes"]== outcome)
    coef_table = expand_grid(ind, period_name)
    coef_df = lapply(1:nrow(coef_table), function(i) gee_coef(coeff_list[[coef_table$ind[i]]], coef_table$period_name[i])) %>% bind_rows()
    coef_df_c = coef_df %>% mutate(outcome = outcome) %>% mutate(term = case_when(OPE == "log(bcetp)" ~ "BCEtP",
                                                                                  OPE == "log(dbup)" ~ "DBuP",
                                                                                  OPE == "log(dphp)" ~ "DPHP",
                                                                                  OPE == "log(bdcpp)" ~ "BDCPP")) %>% dplyr::select(term, window, `95%CI.low`, Estimate, `95%CI.high`, p_total, outcome) %>% rename("estimate" = "Estimate", "p.value" = "p_total", "conf.low" = "95%CI.low", "conf.high" = "95%CI.high") 
    coef_df_list = rbind(coef_df_list, coef_df_c)
  }
    return(coef_df_list)
}

qgcomp_model = function(outcome, window, df, return_index, oc = "other", type = "lm", ...){
  if(oc == "brain_region"){
    formula = paste0(outcome, " ~ cotinine + sex + race + home_score_total + mom_edu + EstimatedTotalIntraCranialVol + bcetp + bdcpp + dbup + dphp")
  }else{
    formula = paste0(outcome, " ~ cotinine + sex + race + home_score_total + mom_edu + bcetp + bdcpp + dbup + dphp")}
  gcomp_fit = qgcomp.noboot(as.formula(formula), data = df, expnms = c("bcetp", "bdcpp", "dbup", "dphp"), q = 10, ...)
  p_value = as.vector(gcomp_fit %>% tidy(conf.int = TRUE, conf.level = 0.90) %>% pull("Pr(>|z|)"))[2]
  comb_effect = gcomp_fit %>% tidy(conf.int = TRUE, conf.level = 0.90)
  comb_effect = comb_effect %>% dplyr::select("Lower CI","estimate","Upper CI","Pr(>|z|)")
  comb_effect = unname(unlist(comb_effect[2,]))
  gcomp_fit$qx[outcome] = gcomp_fit$fit$data[outcome]
  if(oc == "brain_region"){
    covariates = c("cotinine", "sex", "race", "home_score_total", "mom_edu", "EstimatedTotalIntraCranialVol")
  }else{
    covariates = c("cotinine", "sex", "race", "home_score_total", "mom_edu")}
  gcomp_fit$qx[covariates] = gcomp_fit$fit$data[covariates]
  new_formula = as.formula(paste0(outcome, "~ ."))
  if(type == "lm"){
    new_fit = lm(new_formula, data = gcomp_fit$qx)}else if(type == "glm"){
      new_fit = glm(new_formula, data = gcomp_fit$qx, ...)
    }
  summary_df = new_fit %>% tidy(conf.int = TRUE, conf.level = 0.90) %>% dplyr::select("term", "conf.low","estimate","conf.high","p.value") %>% filter(term %in% c("bcetp_q", "bdcpp_q", "dbup_q", "dphp_q"))
  summary_df = rbind(summary_df,c("Combined Effect", comb_effect))
  summary_df = summary_df %>% mutate(conf.low = sprintf("%.3f",as.numeric(conf.low)),
                                     estimate = sprintf("%.3f",as.numeric(estimate)),
                                     conf.high = sprintf("%.3f", as.numeric(conf.high)), p.value = case_when(as.numeric(p.value)<0.05 ~ "*",
                                                                                                             .default = NA))
  colnames(summary_df) = c("OPE", "95%CI.low","Estimate","95%CI.high","sig")
  summary_df$brain_region = outcome
  summary_df$window = window
  summary_df = summary_df[, c(6:7, 1:5)]
  if (return_index == 1){return(sprintf("%.3f", p_value))}else if(return_index == 2){return(summary_df)}
}

# checked
linear_model_sig = function(md, window, term, outlier_ind = NULL, glm_ind = NULL, quar_ind = NULL, ...){
  if(md == "full"){
    p_table_unadjusted = data.frame(term)
    for (outcome in outcomes){
      if(outcome %in% outcomes[outlier_ind]){
            if(outcome %in% outcomes[glm_ind]){
              p_values = linear_model(outcome = outcome, md = 'log', rm_outliers = TRUE, df = windows_df[[window]], type = "glm", ...)
            }else if(outcome %in% outcomes[quar_ind]){
              p_values = linear_model(outcome = outcome, md = "log_quadratic", rm_outliers = TRUE, df = windows_df[[window]], ...)
            }else{p_values = linear_model(outcome = outcome, md = 'log', rm_outliers = TRUE, df = windows_df[[window]], ...)}
          }else{
            if(outcome %in% outcomes[glm_ind]){
              p_values = linear_model(outcome = outcome, md = 'log', rm_outliers = FALSE, df = windows_df[[window]], type = "glm", ...)
            }else if(outcome %in% outcomes[quar_ind]){
              p_values = linear_model(outcome = outcome, md = "log_quadratic", rm_outliers = FALSE, df = windows_df[[window]], ...)
            }else{p_values = linear_model(outcome = outcome, md = 'log', rm_outliers = FALSE, df = windows_df[[window]], ...)}
          }
      p_table = data.frame(cbind(p_values[[2]], p_values[[1]]))
      colnames(p_table) = c("term", outcome)
      p_table_unadjusted = p_table_unadjusted %>% left_join(p_table, by = "term")
    }
    
    colnames(p_table_unadjusted) = c("term", outcomes)
    p_table_unadjusted$term = sapply(p_table_unadjusted$term, cov_name, USE.NAMES = FALSE)
    sig_df = p_table_unadjusted %>% filter(grepl("log", term)) %>% pivot_longer(c(2:(length(outcomes) + 1)), names_to = "brain_region", values_to = "p.values") %>% filter(p.values < 0.05) %>% mutate(window = window)
    return(sig_df)}else if(md == "uni"){
      OPEs = c("log(bcetp)","log(bdcpp)","log(dbup)","log(dphp)")
      OPE_p = data.frame(OPEs)
      outcome_uni = rep(NA,4)
      for (outcome in outcomes){
        for (i in 1:length(OPEs)){
          if(outcome %in% outcomes[outlier_ind]){
            if(outcome %in% outcomes[glm_ind]){
              p_values = uni_model(outcome = outcome, OPE = OPEs[i], rm_outliers = TRUE, df = windows_df[[window]], type = "glm", ...)
            }else if(outcome %in% outcomes[quar_ind]){
              p_values = uni_model(outcome = outcome, OPE = OPEs[i], rm_outliers = TRUE, df = windows_df[[window]], po = TRUE, ...)
            }else{p_values = uni_model(outcome = outcome, OPE = OPEs[i], rm_outliers = TRUE, df = windows_df[[window]], ...)}
          }else{
            if(outcome %in% outcomes[glm_ind]){
              p_values = uni_model(outcome = outcome, OPE = OPEs[i], rm_outliers = FALSE, df = windows_df[[window]], type = "glm", ...)
            }else if(outcome %in% outcomes[quar_ind]){
              p_values = uni_model(outcome = outcome, OPE = OPEs[i], rm_outliers = FALSE, df = windows_df[[window]], po = TRUE, ...)
            }else{p_values = uni_model(outcome = outcome, OPE = OPEs[i], rm_outliers = FALSE, df = windows_df[[window]], ...)}
          }
          outcome_uni[i] = p_values[[1]][2]}
        OPE_p = cbind(OPE_p, outcome_uni)}
      colnames(OPE_p) = c("OPE", outcomes) 
      uni_sig_df = OPE_p %>% pivot_longer(c(2:(length(outcomes) + 1)), names_to = "brain_region", values_to = "p.values") %>% filter(p.values < 0.05) %>% mutate(window = window)
      return(uni_sig_df)
    }
}

# Network Visualization Data Extraction
MI_get_coef = function(x, period, type){
  ope = x %>% tidy() %>% pull(term) 
  ope = ope[2]
  coef_df = x %>% tidy(conf.int = TRUE, conf.level = 0.90) %>% dplyr::select(term, conf.low, estimate, conf.high, p.value) %>% filter(grepl('log', term)) %>% filter(term == paste0(ope, ":period",period) | term == ope)
  if (type == "main"){
    coef_df = coef_df %>% filter(term == ope)
  }else if (type == "interaction"){
    coef_df = coef_df %>% filter(grepl("period*", term))
    coef_df$window = str_sub(str_split(coef_df$term[1], ":")[[1]][2], 7)
    coef_df$term = ope
    coef_df = coef_df[c(1,6,2:5)]
  }
  return(coef_df)
}

MI_coef_extraction = function(gee_table, coeff_list, out.dir, name, filter = TRUE){
  ind = 1:nrow(gee_table)
  period_name = c("birth", "prenatal", "three_year")
  coef_table = expand_grid(ind, period_name)
  coef_df_main = lapply(1:nrow(coef_table), function(i) MI_get_coef(coeff_list[[coef_table$ind[i]]], coef_table$period_name[i], type = "main")) %>% bind_rows()
  coef_df_main["outcome"] = sapply(1:nrow(coef_table), function(x) gee_table$outcomes[coef_table$ind[x]], USE.NAMES = FALSE)
  coef_df_main = unique(coef_df_main)
  if(filter == TRUE){
    coef_df_main = coef_df_main %>% filter(p.value < 0.05)
    }
  coef_df_main$term = sapply(coef_df_main$term, function(x){
    case_when(x == "log(bcetp)" ~ "BCEtP",
              x == "log(bdcpp)" ~ "BDCPP",
              x == "log(dbup)" ~ "DBuP",
              x == "log(dphp)" ~ "DPHP")
  }, USE.NAMES = FALSE)
  #coef_df_main = coef_df_main %>% mutate(window = case_when(window == "three_year" ~ "3y",
  #                                                        .default = window))
  write_csv(coef_df_main, paste0(out.dir,"/", name, "_main.csv"))
  
  coef_df_int = lapply(1:nrow(coef_table), function(i) MI_get_coef(coeff_list[[coef_table$ind[i]]], coef_table$period_name[i], type = "interaction")) %>% bind_rows()
  coef_df_int["outcome"] = sapply(1:nrow(coef_table), function(x) gee_table$outcomes[coef_table$ind[x]], USE.NAMES = FALSE)
  coef_df_int = unique(coef_df_int)
  if(filter == TRUE){
    coef_df_int = coef_df_int %>% filter(p.value < 0.1)
  }
  coef_df_int$term = sapply(coef_df_int$term, function(x){
    case_when(x == "log(bcetp)" ~ "BCEtP",
              x == "log(bdcpp)" ~ "BDCPP",
              x == "log(dbup)" ~ "DBuP",
              x == "log(dphp)" ~ "DPHP")
  }, USE.NAMES = FALSE)
  #coef_df_int = coef_df_int %>% mutate(window = case_when(window == "three_year" ~ "3y",
  #                                                        .default = window))
  write_csv(coef_df_int, paste0(out.dir,"/", name, "_interaction.csv"))
  return(list("coef_df_main" = coef_df_main, "coef_df_int" = coef_df_int))
}

MI_all_results = function(outcomes, out.dir, name, outlier_ind = NULL, glm_ind = NULL, ...){
  outcomes = outcomes
  ope_name = c("bcetp", "bdcpp", "dbup", "dphp")
  period_name = c("birth", "prenatal", "three_year")
  gee_table = expand_grid(ope_name, outcomes)
  data_list = vector("list", nrow(gee_table)) # Interaction Effect
  ope_sig= vector("list", nrow(gee_table)) # Main Effect
  coeff_list = vector("list", nrow(gee_table)) # Model
  for (i in 1:nrow(gee_table)){
    if(gee_table$outcomes[i] %in% outcomes[outlier_ind]){
      data_list[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i], rm_outliers = TRUE, family = "gaussian", ...)[[1]]
      ope_sig[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i],rm_outliers = TRUE, family = "gaussian", ...)[[2]]
      coeff_list[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i], rm_outliers = TRUE, family = "gaussian", ...)[[3]]
    }else if (gee_table$outcomes[i] %in% outcomes[glm_ind]){
      data_list[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i], rm_outliers = FALSE, family = "poisson", ...)[[1]]
      ope_sig[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i],rm_outliers = FALSE, family = "poisson", ...)[[2]]
      coeff_list[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i], rm_outliers = FALSE, family = "poisson", ...)[[3]]
    }else{
      data_list[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i], family = "gaussian", ...)[[1]]
      ope_sig[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i], family = "gaussian", ...)[[2]]
      coeff_list[[i]] = gee_model(gee_table$ope_name[i], gee_table$outcomes[i], family = "gaussian", ...)[[3]]
    }
  }
  results = MI_coef_extraction(gee_table, coeff_list, filter = FALSE, out.dir, name)
  coef_df_main = results[["coef_df_main"]]
  coef_df_int = results[["coef_df_int"]]
  coef_df_total = gee_coef_run(gee_table, outcomes, coeff_list, period_name) 
  coef_df_total$window = sapply(coef_df_total$window, function(x) if(x == "three_year") {return("3y")}else{return(x)},
                                                                     USE.NAMES = FALSE)
 #coef_df_total_out = coef_df_total %>% group_by(term, estimate, p.value, outcome) %>% summarize(count = n()) %>% 
 #  mutate(type = case_when(count > 1 ~ "all windows",
 #                          .default = "windows of interest")) %>% left_join(coef_df_total, by = c("term", "estimate", "p.value", "outcome")) %>% 
 #  mutate(window = case_when(type == "all windows" ~ "all windows",
 #                            .default = window)) %>% dplyr::select(term, window, estimate, p.value, outcome) %>% distinct() %>% ungroup()
 #  
  write_csv(coef_df_total, paste0(out.dir,"/", name, "_total.csv"))
  return(list("coef_df_main" = coef_df_main, "coef_df_int" = coef_df_int, "coef_df_total" = coef_df_total))
}

qg_coef_extraction = function(gcomp_list, gcomp_table, out.dir, name){
  df = gcomp_list %>% filter(OPE == "Combined Effect") %>% left_join(gcomp_table, by = c("window" = "windows_interest", "brain_region" = colnames(gcomp_table)[2])) %>% 
    dplyr::select(OPE, window, `95%CI.low`, Estimate, `95%CI.high`, p.values, brain_region) %>% mutate(OPE = "OPE")
  colnames(df) = c("term", "window", "conf.low", "estimate", "conf.high", "p.value", "outcome")
  write_csv(df, paste0(out.dir,"/", name, "_combined_effect.csv"))
  return(df)
}

qg_all_results = function(df_name, outcomes, glm_ind = NULL, out_type, oc, ...){
  windows_interest = c("prenatal", "birth", "3y", "5-8y", "8y")
  pre_df = eval(parse(text = df_name[which(grepl("^pre_df*", df_name))]))
  birth_df = eval(parse(text = df_name[which(grepl("^birth_df*", df_name))]))
  three_y_df = eval(parse(text = df_name[which(grepl("^three_y_df_*", df_name))]))    
  five_eight_y_df = eval(parse(text = df_name[which(grepl("^five_eight_y_df_*", df_name))]))    
  eight_y_df = eval(parse(text = df_name[which(grepl("^eight_y_df_*", df_name))]))    
  windows_df = list("prenatal" = pre_df, "birth" = birth_df, "3y" = three_y_df, "5-8y" = five_eight_y_df, "8y" = eight_y_df)
  outcomes = outcomes
  gcomp_table = expand_grid(windows_interest, outcomes)
  g_type = sapply(gcomp_table$outcomes, function(x){
    if (x %in% glm_ind){return("glm")}else{return("lm")}
  }, USE.NAMES = FALSE)
  g_family = lapply(gcomp_table$outcomes, function(x){
    if (x %in% glm_ind){return(eval(parse(text = "poisson()")))}else{return(eval(parse(text = "gaussian()")))}
  })
  gcomp_list = vector("list", nrow(gcomp_table))
  gcomp_list = mapply(qgcomp_model, outcome = gcomp_table$outcomes, window = gcomp_table$windows_interest, df = windows_df[gcomp_table$windows_interest], return_index = rep(2, nrow(gcomp_table)), oc = oc, type = g_type, family = g_family, USE.NAMES = FALSE, SIMPLIFY = FALSE) %>% bind_rows()
  gcomp_table$p_value = mapply(qgcomp_model, outcome = gcomp_table$outcomes, window = gcomp_table$windows_interest, df = windows_df[gcomp_table$windows_interest], return_index = rep(1, nrow(gcomp_table)), oc = oc, type = g_type, family = g_family, USE.NAMES = FALSE)
  colnames(gcomp_table) = c("windows_interest", out_type, "p.values")
  qg_df = qg_coef_extraction(gcomp_list, gcomp_table, ...)
  return(qg_df)
}

# Multiple Comparison
BH_multiple_comparison_run = function(total, combined, type, out.dir, name){
  if(type == "MI"){
    outcomes = unique(total$outcome)
    window = unique(total$window)
    ope = unique(total$term)
    input = expand_grid(window, ope)
    df_corrected = lapply(1: nrow(input), function(i) BH_correction_MI(total, input[[i, "window"]], input[[i, "ope"]])) %>% bind_rows()
    df_corrected = df_corrected %>% group_by(term, estimate, p.value, outcome) %>% summarize(count = n()) %>% 
      mutate(type = case_when(count > 1 ~ "all windows",
                              .default = "windows of interest")) %>% left_join(df_corrected, by = c("term", "estimate", "p.value", "outcome")) %>% 
      mutate(window = case_when(type == "all windows" ~ "all windows",
                                .default = window)) %>% dplyr::select(term, window, estimate, p.value, outcome) %>% distinct() %>% ungroup()
    
    write_csv(df_corrected, paste0(out.dir, "/", name, "_total.csv"))
  }else if (type == "QG"){
    outcomes = unique(combined$outcome)
    window = unique(combined$window)
    df_corrected = lapply(window, function(w) BH_correction_QG(combined, w)) %>% bind_rows()
    write_csv(df_corrected, paste0(out.dir, "/", name, "_combined_effect.csv"))
  }
  return(df_corrected)
}

BH_correction_MI = function(total, w, ope){
  sub_df = total %>% filter(window == w, term == ope)
  sub_df$p.value = p.adjust(sub_df$p.value, method = "BH")
  return(sub_df)
}

BH_correction_QG = function(combined, w){
  sub_df = combined %>% filter(window == w)
  sub_df$p.value = p.adjust(sub_df$p.value, method = "BH")
  return(sub_df)
}

# Model Connection

agreed_results = function(mi, qg){
  #if(type == "before"){
  #  mi = mi %>% group_by(term, estimate, p.value, outcome) %>% summarize(count = n()) %>% 
  #    mutate(type = case_when(count > 1 ~ "all windows",
  #                            .default = "windows of interest")) %>% left_join(mi, by = c("term", "estimate", "p.value", "outcome")) %>% 
  #    mutate(window = case_when(type == "all windows" ~ "all windows",
  #                              .default = window)) %>% dplyr::select(term, window, estimate, p.value, outcome) %>% distinct() %>% ungroup()
  #}
  df = rbind(mi, qg) %>% filter(p.value < 0.1) %>% group_by(outcome, window, term) %>% summarize(ope = n()) %>% ungroup()
  qg_sig = df %>% filter(term == "OPE") %>% pull(outcome) %>% unique()
  mi_sig = df %>% filter(term != "OPE") %>% pull(outcome) %>% unique()
  agreed_sig = qg_sig[qg_sig %in% mi_sig]
  df = df %>% filter(outcome %in% agreed_sig)
  return(list(agreed_sig, df))
}

# Mediation Analysis
mi_mediate = function(df_com, y, m, variables, ope, rt = "int"){
  y_form = paste0(y, " ~ ", paste(variables[variables !="subject_id"], collapse = " + "), " + ", m)
  y_form = gsub(ope, paste0("log(", ope, ")"), y_form)
  if(str_split(y, "_")[[1]][length(str_split(y, "_")[[1]])] == "Drops"){
    y_type = "count"
  }else{y_type = "continuous"}
  if(y_type == "count"){
    mi.y = glm(as.formula(y_form), data = df_com, family = poisson())
  }else{
    mi.y = lm(as.formula(y_form), data = df_com)
  }
  b = mi.y$coefficients[[m]]
  m_form = paste0(m, " ~ ", paste(variables[variables !="subject_id"], collapse = " + "))
  m_form = gsub(ope, paste0("log(", ope, ")"), m_form)
  mi.m = lm(as.formula(m_form), data = df_com)
  a = mi.m$coefficients[[paste0("log(", ope, ")")]]
  indirect = a*b
  if(rt == "int"){
    d = mi.y$coefficients[[paste0("log(", ope, ")")]]
    total = d + indirect
    return(list("indirect" = indirect, "total" = total))
  }else if(rt == "ls"){
    d = mi.y$coefficients[[paste0("log(", ope, ")")]]
    d_conf = paste0("(", paste0(round(unname(confint(mi.y, level = 0.95)[paste0("log(", ope, ")"), ]), 3), collapse = " , "), ")")
    m_conf = paste0("(", paste0(round(unname(confint(mi.m, level = 0.95)[paste0("log(", ope, ")"), ]), 3), collapse = " , "), ")")
    return(list("direct" = round(d, 3), "direct_CI" = d_conf, "indirect" = round(indirect,3), "total" = round((d+indirect), 3), "mediator" = round(a, 3), "mediator_CI" = m_conf, "model" = mi.y))
  }
}

mi_mediate_gen = function(df1, df2, y, m, type, n, window, ope){
  if(type == "brain_region"){
    variables = c("subject_id", "cotinine",
                  "EstimatedTotalIntraCranialVol", "sex",
                  "home_score_total", "mom_edu", "race", ope)
  }else{
    variables = c("subject_id", "cotinine",
                  "sex", "home_score_total", "mom_edu", "race", ope)
  }
  
  df_com = df1 %>% dplyr::select(c(m, variables)) %>% left_join(df2 %>% dplyr::select(c(y, "subject_id")), by = c("subject_id")) %>% na.omit()
  set.seed(123)
  indirect_dist = replicate(
    n,
    expr = mi_mediate(df_com %>% slice_sample(prop = 1, replace = TRUE), y, m, variables = variables, ope = ope)
  )
  lower.ci = round(quantile(unlist(indirect_dist["indirect", ]), probs = 0.025), 3)
  higher.ci = round(quantile(unlist(indirect_dist["indirect", ]), probs = 0.975), 3)
  if(0< lower.ci | 0 >higher.ci){sig = "*"}else{sig = NA}
  lower.ci.total = round(quantile(unlist(indirect_dist["total", ]), probs = 0.025), 3)
  higher.ci.total = round(quantile(unlist(indirect_dist["total", ]), probs = 0.975), 3)
  sig_df = data.frame("y" = y, "mediator" = m, "OPE" = ope, "indirect_effect_CI" = paste0("(", lower.ci, " , ", higher.ci, ")"), "window" = window, "sig" = sig, "total_effect_CI" = paste0("(", lower.ci.total, " , ", higher.ci.total, ")"))
  return(sig_df)
}

qg_mediate = function(df_com, y, m, variables, rt = "int"){
  y_form = paste0(y, " ~ ", paste(variables[variables !="subject_id"], collapse = " + "), " + ", m)
  d_form = paste0(y, " ~ ", paste(variables[variables !="subject_id"], collapse = " + "))
  if(str_split(y, "_")[[1]][length(str_split(y, "_")[[1]])] == "Drops"){
    y_type = "count"
  }else{y_type = "continuous"}
  if(y_type == "count"){
    gcomp_y = qgcomp.noboot(as.formula(y_form), data = df_com, expnms = c("bcetp", "bdcpp", "dbup", "dphp"), q = 10, family = poisson())
  }else{
    gcomp_y = qgcomp.noboot(as.formula(y_form), data = df_com, expnms = c("bcetp", "bdcpp", "dbup", "dphp"), q = 10, family = gaussian())
  }
  b = gcomp_y$fit$coefficients[[m]]
  m_form = paste0(m, " ~ ", paste(variables[variables !="subject_id"], collapse = " + "))
  gcomp_m = qgcomp.noboot(as.formula(m_form), data = df_com, expnms = c("bcetp", "bdcpp", "dbup", "dphp"), q = 10, family = gaussian())
  a = unname(gcomp_m$psi)
  indirect = a*b
  if(rt == "int"){
    d = unname(gcomp_y$psi)
    total = d + indirect
    return(list("indirect" = indirect, "total" = total))
  }else if(rt == "ls"){
    d = unname(gcomp_y$psi)
    d_conf = paste0("(", paste0(round(unname(confint(gcomp_y)[2,]), 3), collapse = " , "),  ")")
    m_conf = paste0("(", paste0(round(unname(confint(gcomp_m)[2,]), 3), collapse = " , "), ")")
    return(list("direct" = round(d, 3), "direct_CI" = d_conf, "indirect" = round(indirect, 3), "total" = round((d+indirect), 3), "mediator" = round(a, 3), "mediator_CI" = m_conf, "model" = gcomp_y))
  }
}

qg_mediate_gen = function(df1, df2, y, m, type, n, window){
  if(type == "brain_region"){
    variables = c("subject_id", "bcetp", "bdcpp", "dbup", "dphp", "cotinine",
                  "EstimatedTotalIntraCranialVol", "sex",
                  "home_score_total", "mom_edu", "race")
  }else{
    variables = c("subject_id", "bcetp", "bdcpp", "dbup", "dphp", "cotinine",
                  "sex", "home_score_total", "mom_edu", "race")
  }
  df_com = df1 %>% dplyr::select(c(m, variables)) %>% left_join(df2 %>% dplyr::select(c(y, "subject_id")), by = c("subject_id")) %>% na.omit()
  set.seed(123)
  indirect_dist = replicate(
    n,
    expr = qg_mediate(df_com %>% slice_sample(prop = 1, replace = TRUE), y, m, variables = variables)
  )
  lower.ci = round(quantile(unlist(indirect_dist["indirect", ]), probs = 0.025), 3)
  higher.ci = round(quantile(unlist(indirect_dist["indirect", ]), probs = 0.975), 3)
  if(0< lower.ci | 0 >higher.ci){sig = "*"}else{sig = NA}
  lower.ci.total = round(quantile(unlist(indirect_dist["total", ]), probs = 0.025), 3)
  higher.ci.total = round(quantile(unlist(indirect_dist["total", ]), probs = 0.975), 3)
  sig_df = data.frame("y" = y, "mediator" = m, "indirect_effect_CI" = paste0("(", lower.ci, " , ", higher.ci, ")"), "window" = window, "sig" = sig, "total_effect_CI" = paste0("(", lower.ci.total, " , ", higher.ci.total, ")"))
  return(sig_df)
}

mediation_analysis = function(df1, df2, y, m, type, model, ope = "OPE"){
  if (model == "mi"){
    if(type == "brain_region"){
      variables = c("subject_id", "cotinine",
                    "EstimatedTotalIntraCranialVol", "sex",
                    "home_score_total", "mom_edu", "race", ope)
    }else{
      variables = c("subject_id", "cotinine",
                    "sex", "home_score_total", "mom_edu", "race", ope)
    }
    df_com = df1 %>% dplyr::select(c(m, variables)) %>% left_join(df2 %>% dplyr::select(c(y, "subject_id")), by = c("subject_id")) %>% na.omit()
    result = mi_mediate(df_com, y, m, variables = variables, ope = ope, rt = "ls")
  }else{
    if(type == "brain_region"){
      variables = c("subject_id", "bcetp", "bdcpp", "dbup", "dphp", "cotinine",
                    "EstimatedTotalIntraCranialVol", "sex",
                    "home_score_total", "mom_edu", "race")
    }else{
      variables = c("subject_id", "bcetp", "bdcpp", "dbup", "dphp", "cotinine",
                    "sex", "home_score_total", "mom_edu", "race")
    }
    df_com = df1 %>% dplyr::select(c(m, variables)) %>% left_join(df2 %>% dplyr::select(c(y, "subject_id")), by = c("subject_id")) %>% na.omit()
    result = qg_mediate(df_com, y, m, variables = variables, rt = "ls")
  }
  return(result)
}

