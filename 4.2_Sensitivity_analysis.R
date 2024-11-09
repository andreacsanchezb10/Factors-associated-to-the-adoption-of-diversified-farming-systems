library(readxl)
library(dplyr)
#-----------------------------------------------------------
#######################################################
########## SENSITIVITY ANALYSIS #######################
######################################################
############ OVERALL META-ANALYSIS WITH Log-Odds ratio -----
factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")
factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$logor_unit,")", sep="")

comparison<-read.csv("results/comparison_best_model.csv",header = TRUE, sep = ",")
sort(unique(comparison$best_model))

pcc_data<-read.csv("data/pcc_data.csv",header = TRUE, sep = ",")
names(pcc_data)

logor_factor_class_unit<-factors_metric_assessed%>%
  filter(!is.na(logor_unit))%>%
  select(factor_sub_class,logor_factor_unit)
sort(unique(logor_factor_class_unit$logor_factor_unit))

logor_factor_class_unit<-unique(logor_factor_class_unit$logor_factor_unit)

######## THREE-LEVEL META-ANALYSIS -------------- 
logor_data_3level<- pcc_data%>%
  left_join(comparison, by= "pcc_factor_unit")%>%
  filter(best_model == "Three-level" )%>%
  filter(model_method_recla== "logit" |
           model_method_recla=="probit")%>%
  filter(!is.na(logor_unit))

names(logor_data_3level)
sort(unique(logor_data_3level$factor_metric))
sort(unique(logor_data_3level$model_method_recla))
length(unique(logor_data_3level$study_id))
sort(unique(logor_data_3level$model_method_recla))

write.csv(logor_data_3level,"data/logor_data_3levels.csv", row.names=FALSE)

#### Estimate the overall effect by fitting an intercept-only model ----
logor_overall_3level <- function(data, metric_unit) {
  overal_model <- rma.mv(b_logOR, v_logOR, 
                         random = list(~ 1 | ES_ID, ~ 1 | study_id),
                         data = data,
                         method = "REML", 
                         test = "t",
                         dfs="contain",
                         subset = (logor_factor_unit == metric_unit))
  
  return(summary(overal_model, digits = 3))
  
}
sort(unique(logor_data_3level$logor_factor_unit))
# Vector of factor_metric_unit levels
factor_metric_units <- unique(logor_data_3level$logor_factor_unit)

# List to store the results of all models
logor_overall_3level_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- logor_overall_3level(data = logor_data_3level, metric_unit = unit)
  logor_overall_3level_list[[unit]] <- result
}

# Combine overall results into one table
logor_overall_3level_results_list<- do.call(rbind, logor_overall_3level_list)

logor_overall_3level_results <- as.data.frame(logor_overall_3level_results_list)%>%
  rownames_to_column(., var = "logor_factor_unit")%>%
  mutate(ci.lb = sapply(ci.lb, as.numeric),
         ci.ub = sapply(ci.ub, as.numeric))%>%
  mutate(beta = as.numeric(beta))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval>=0.1,"","",
                                                        "")))))%>%
  #mutate(significance1= if_else(pval>0.05&pval<=0.1,"\u0A76",""))
  select(logor_factor_unit, beta,se, ci.lb, ci.ub,zval,pval,significance,sigma2,QEdf,
         QE,QEp,s.nlevels)%>%
  mutate(sigma2=substr(sigma2, 3, nchar(sigma2) - 1))%>%
  mutate(sigma2.1= as.numeric(str_extract(sigma2, ".*(?=\\,)")))%>%
  mutate(sigma2.2= as.numeric(str_extract(sigma2, "(?<=, ).*")))%>%
  mutate(s.nlevels=substr(s.nlevels, 3, nchar(s.nlevels) - 1))%>%
  mutate(n_ES= as.numeric(str_extract(s.nlevels, ".*(?=\\,)")))%>%
  mutate(n_studies= as.numeric(str_extract(s.nlevels, "(?<=, ).*")))%>%
  mutate_at(2:7, as.numeric)%>%
  mutate_at(8:9,as.character)%>%
  mutate_at(10:12, as.numeric)%>%
  mutate_at(15:17, as.numeric)%>%
  #mutate(across(where(is.numeric), ~ round(., 3)))%>%
  mutate(QEp= as.character(QEp))%>%
  mutate(QEp= if_else(QEp==0, "<0.001", QEp))%>%
  select(logor_factor_unit, beta,se, ci.lb, ci.ub,zval,pval,significance,n_ES, n_studies,sigma2.1,sigma2.2,QEdf,QE,QEp)%>%
  #Transform back Odds-ratio
  mutate(or.beta= exp(beta))%>%
  mutate(or.ci.lb= exp(ci.lb))%>%
  mutate(or.ci.ub= exp(ci.ub))

sapply(logor_overall_3level_results, class)

write.csv(logor_overall_3level_results,"results/logor_overall_results_3levels.csv", row.names=FALSE)
sort(unique(logor_overall_3level_results$logor_factor_unit))
######## TWO-LEVEL META-ANALYSIS -------------- 
logor_data_2level<- pcc_data%>%
  left_join(comparison, by= "pcc_factor_unit")%>%
  filter(best_model == "Two-level" )%>%
  filter(model_method_recla== "logit" |
           model_method_recla=="probit")%>%
  filter(!is.na(logor_unit))%>%
  filter(!is.na(v_logOR))%>%
  filter(se_logOR!=Inf)

names(logor_data_2level)
sort(unique(logor_data_2level$logor_factor_unit))
sort(unique(logor_data_2level$factor_metric))
sort(unique(logor_data_2level$model_method_recla))
sort(unique(logor_data_2level$logor_factor_unit))
sort(unique(logor_data_2level$factor_sub_class.x))
sort(unique(logor_data_2level$factor_metric))

write.csv(logor_data_2level,"data/logor_data_2levels.csv", row.names=FALSE)

#### Estimate the overall effect by fitting an intercept-only model ----
logor_overall_2level <- function(data, metric_unit) {
  overal_model <- rma(b_logOR, v_logOR, 
                      data = data,
                      method = "REML", 
                      test = "knha",
                      subset = (logor_factor_unit == metric_unit))
  
  return(summary(overal_model, digits = 3))
  
}

# Vector of factor_metric_unit levels
sort(unique(logor_data_2level$logor_factor_unit))
factor_metric_units <- unique(logor_data_2level$logor_factor_unit)

# List to store the results of all models
logor_overall_2level_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- logor_overall_2level(data = logor_data_2level, metric_unit = unit)
  logor_overall_2level_list[[unit]] <- result
}

# Combine overall results into one table
logor_overall_2level_results_list<- do.call(rbind, logor_overall_2level_list)

logor_overall_2level_results <- as.data.frame(logor_overall_2level_results_list)%>%
  tibble::rownames_to_column(., var = "logor_factor_unit")%>%
  mutate(ci.lb = sapply(ci.lb, as.numeric),
         ci.ub = sapply(ci.ub, as.numeric))%>%
  mutate(beta = as.numeric(beta))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval>=0.1,"","",
                                                        "")))))%>%
  dplyr::rename("n_ES"="k")%>%
  select(logor_factor_unit, beta,ci.lb,ci.ub,
         zval, pval,
         significance,n_ES,
         tau2,se.tau2, I2, QE,dfs, QEp)%>%
  #separate(fit.stats, into = c("ll.ML", "dev.ML", "AIC.ML","BIC.ML","AICc.ML",
  #                            "ll.REML", "dev.REML", "AIC.REML","BIC.REML","AICc.REML"), sep = ", ")%>%
  mutate_at(2:6, as.numeric)%>%
  mutate_at(7:8, as.character)%>%
  mutate_at(9:14, as.numeric)%>%
  mutate(or.beta= exp(beta))%>%
  mutate(or.ci.lb= exp(ci.lb))%>%
  mutate(or.ci.ub= exp(ci.ub))

write.csv(logor_overall_2level_results,"results/logor_overall_results_2levels.csv", row.names=FALSE)
sort(unique(logor_overall_2level_results$logor_factor_unit))


logor_data<- rbind(logor_data_3level, logor_data_2level)
length(unique(logor_data$study_id))
sort(unique(logor_data$logor_factor_unit))
