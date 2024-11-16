library(readxl)
library(tidyr)
library(stringr)
library(dplyr)
library(metafor)
library(tibble)

data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_metric_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)


factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$pcc_unit,")", sep="")

factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$logor_unit,")", sep="")

pcc_data<-read.csv("data/pcc_data.csv",header = TRUE, sep = ",")
names(pcc_data)

comparison<-read.csv("results/comparison_best_model.csv",header = TRUE, sep = ",")
sort(unique(comparison$best_model))

############ OVERALL META-ANALYSIS WITH FISHERS' Z -----
pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_category,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

######## THREE-LEVEL META-ANALYSIS -------------- 
pcc_data_3level<- pcc_data%>%
  left_join(comparison, by= c("pcc_factor_unit","factor_category"))%>%
  filter(best_model == "Three-level" )

sort(unique(pcc_data_3level$pcc_factor_unit))
sort(unique(pcc_data_3level$factor_category))

write.csv(pcc_data_3level,"data/pcc_data_3levels.csv", row.names=FALSE)

#### Estimate the overall effect by fitting an intercept-only model ----
overall_3level <- function(data, metric_unit) {
  overal_model <- rma.mv(fis.yi, fis.vi, 
                         random = list(~ 1 | ES_ID, ~ 1 | study_id),
                         data = data,
                         method = "REML", 
                         test = "t",
                         dfs="contain",
                         subset = (pcc_factor_unit == metric_unit))
  
  return(summary(overal_model, digits = 3))
  
}

# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data_3level$pcc_factor_unit)

# List to store the results of all models
overall_3level_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- overall_3level(data = pcc_data_3level, metric_unit = unit)
  overall_3level_list[[unit]] <- result
}

# Combine overall results into one table
overall_3level_results_list<- do.call(rbind, overall_3level_list)

overall_3level_results <- as.data.frame(overall_3level_results_list)%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(ci.lb = sapply(ci.lb, as.numeric),
         ci.ub = sapply(ci.ub, as.numeric))%>%
  mutate(beta = as.numeric(beta))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval>=0.1,"","",
                                                        "")))))%>%
  mutate(significance1= if_else(pval>0.05&pval<=0.1,"\u0A76",""))%>%
  select(pcc_factor_unit, beta,se, ci.lb, ci.ub,zval,pval,significance,significance1,sigma2,QEdf,
         QE,QEp,s.nlevels)%>%
  mutate(sigma2=substr(sigma2, 3, nchar(sigma2) - 1))%>%
  mutate(sigma2.1= as.numeric(str_extract(sigma2, ".*(?=\\,)")))%>%
  mutate(sigma2.2= as.numeric(str_extract(sigma2, "(?<=, ).*")))%>%
  mutate(s.nlevels=substr(s.nlevels, 3, nchar(s.nlevels) - 1))%>%
  mutate(n_ES= as.numeric(str_extract(s.nlevels, ".*(?=\\,)")))%>%
  mutate(n_studies= as.numeric(str_extract(s.nlevels, "(?<=, ).*")))%>%
  mutate_at(2:7, as.numeric)%>%
  mutate_at(8:9,as.character)%>%
  mutate_at(11:12, as.numeric)%>%
  mutate_at(15:17, as.numeric)%>%
  #mutate(across(where(is.numeric), ~ round(., 3)))%>%
  mutate(QEp= as.character(QEp))%>%
  mutate(QEp= if_else(QEp==0, "<0.001", QEp))%>%
  select(pcc_factor_unit, beta,se, ci.lb, ci.ub,zval,pval,significance,significance1,n_ES, n_studies,sigma2.1,sigma2.2,QEdf,QE,QEp)%>%
  #Transform back fisher's z to PCC
  mutate(pcc.beta= transf.ztor(beta))%>%
  mutate(pcc.ci.lb= transf.ztor(ci.lb))%>%
  mutate(pcc.ci.ub= transf.ztor(ci.ub))


write.csv(overall_3level_results,"results/overall_results_3levels.csv", row.names=FALSE)

#### D istribution of the variance over the three levels of the meta-analytic model ----
#Equation: Cheung (2014) Formula to calculate the estimate sampling variance (formula 14)
#v= vi
estimated.sampling.variance.func <- function (v) {  
  result<- ((length(v)-1) * sum(1/v))/ (((sum(1/v))^2)-(sum(1/(v^2))))
  return(result)
}

overall_3level_sampling_variance<- pcc_data_3level%>%
  group_by(pcc_factor_unit)%>%
  mutate(sampling.variance= estimated.sampling.variance.func(fis.vi))%>%
  group_by(pcc_factor_unit,sampling.variance)%>%
  tally()%>%
  left_join(overall_3level_results, by = ("pcc_factor_unit"))%>%
  mutate(heterogeneity_test= paste("Q(df = ", QEdf,") = ",QE,", p ", QEp, sep = ""))


## Each of the three variance components (I2_1, I2_2, I2_3) is divided by the total amount of variance
# Sampling variance (Amount of variance at level 1)
overall_3level_sampling_variance$I2_1<-((overall_3level_sampling_variance$sampling.variance)/(overall_3level_sampling_variance$sigma2.1+overall_3level_sampling_variance$sigma2.2+overall_3level_sampling_variance$sampling.variance))*100

# Within-study variance (Amount of variance at level 2)
overall_3level_sampling_variance$I2_2<-((overall_3level_sampling_variance$sigma2.1) / (overall_3level_sampling_variance$sigma2.1 + overall_3level_sampling_variance$sigma2.2 + overall_3level_sampling_variance$sampling.variance))*100

# Between-study variance (Amount of variance at level 3)
overall_3level_sampling_variance$I2_3<-((overall_3level_sampling_variance$sigma2.2) / (overall_3level_sampling_variance$sigma2.1 + overall_3level_sampling_variance$sigma2.2 + overall_3level_sampling_variance$sampling.variance))*100


overall_3level_sampling_variance<-overall_3level_sampling_variance%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  select("factor_category","pcc_factor_unit", "sigma2.1", "sigma2.2",
         "heterogeneity_test", "I2_1",
         "I2_2", "I2_3")%>%
  mutate(across(where(is.numeric), ~ round(., 5)))


write.csv(overall_3level_sampling_variance,"results/heterogeneity_3levels.csv", row.names=FALSE)


######## TWO-LEVEL META-ANALYSIS -------------- 
pcc_data_2level<- pcc_data%>%
  left_join(comparison, by= c("pcc_factor_unit","factor_category"))%>%
  filter(best_model == "Two-level" )

names(pcc_data_2level)
sort(unique(pcc_data_2level$pcc_factor_unit))

write.csv(pcc_data_2level,"data/pcc_data_2levels.csv", row.names=FALSE)

#### Estimate the overall effect by fitting an intercept-only model ----
overall_2level <- function(data, metric_unit) {
  overal_model <- rma(fis.yi, fis.vi, 
                         data = data,
                         method = "REML", 
                         test = "knha",
                         subset = (pcc_factor_unit == metric_unit))
  
  return(summary(overal_model, digits = 3))
  
}

# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data_2level$pcc_factor_unit)
factor_metric_units
# List to store the results of all models
overall_2level_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- overall_2level(data = pcc_data_2level, metric_unit = unit)
  overall_2level_list[[unit]] <- result
}

# Combine overall results into one table
overall_2level_results_list<- do.call(rbind, overall_2level_list)

overall_2level_results <- as.data.frame(overall_2level_results_list)%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(ci.lb = sapply(ci.lb, as.numeric),
         ci.ub = sapply(ci.ub, as.numeric))%>%
  mutate(beta = as.numeric(beta))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval>=0.1,"","",
                                                                "")))))%>%
  mutate(significance1= if_else(pval>0.05&pval<=0.1,"\u0A76",""))%>%
  dplyr::rename("n_ES"="k")%>%
  select(pcc_factor_unit, beta,ci.lb,ci.ub,
         zval, pval,
         significance,significance1,n_ES,
         tau2,se.tau2, I2, QE,dfs, QEp)%>%
  #separate(fit.stats, into = c("ll.ML", "dev.ML", "AIC.ML","BIC.ML","AICc.ML",
   #                            "ll.REML", "dev.REML", "AIC.REML","BIC.REML","AICc.REML"), sep = ", ")%>%
  mutate_at(2:6, as.numeric)%>%
  mutate_at(7:8, as.character)%>%
  mutate_at(9:15, as.numeric)%>%
  mutate(pcc.beta= transf.ztor(beta))%>%
  mutate(pcc.ci.lb= transf.ztor(ci.lb))%>%
  mutate(pcc.ci.ub= transf.ztor(ci.ub))
  
write.csv(overall_2level_results,"results/overall_results_2levels.csv", row.names=FALSE)

## Distribution of the variance
overall_2level_sampling_variance<-  overall_2level_results%>%
  select(pcc_factor_unit,tau2,se.tau2, I2, QE, dfs, QEp)%>%
  mutate_at(2:7, as.numeric)%>%
  mutate(across(where(is.numeric), ~ round(., 3)))%>%
  mutate(QEp= as.character(QEp))%>%
  mutate(QEp= if_else(QEp==0, "<0.001", paste("= ",QEp,sep = "")))%>%
  mutate(heterogeneity_test= paste("Q(df = ", dfs,") = ",QE,", p ", QEp, sep = ""))%>%
  mutate(tau2_se= paste(tau2," (",se.tau2,")",sep=""))%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  select(factor_category,pcc_factor_unit,tau2_se, heterogeneity_test, I2)
  
write.csv(overall_2level_sampling_variance,"results/heterogeneity_2levels.csv", row.names=FALSE)