library(tibble)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(metafor)

factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed_2")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_data<-read.csv( "data/pcc_data.csv", header = TRUE, sep = ",")%>%
  mutate(pcc_factor_unit= as.factor(pcc_factor_unit))

levels(pcc_data$pcc_factor_unit) #64
names(pcc_data)
######## COMPARISON between 2-level and 3-level model structure -------------- 

#### Heterogeneity of within-study variance (level 2) ---
## Build a two-level model without within-study variance 
#modelnovar2 <- rma.mv(y, v, random = list(~ 1 | effectsizeID, ~ 1 | studyID),
#                     sigma2=c(0,NA), tdist=TRUE, data=dataset)

modelnovar2_model <- function(data, metric_unit) {
  overal_model <- rma.mv(fis.yi, fis.vi, 
                         random = list(~ 1 | ES_ID, ~ 1 | article_id),
                         data = data,
                         method = "REML", 
                         test = "t",
                         dfs="contain",
                         subset = (pcc_factor_unit == metric_unit))
  
  summary(overal_model, digits = 3)
  
  modelnovar2 <- rma.mv(fis.yi, fis.vi, 
                        random = list(~ 1 | ES_ID, ~ 1 | article_id),
                        data = data,
                        method = "REML", 
                        test = "t",
                        dfs="contain", 
                        sigma2 = c(0, NA),
                        subset = (pcc_factor_unit == metric_unit))
  
  summary(modelnovar2, digits = 3)
  
  anova_result <- anova(overal_model, modelnovar2)
  return(anova_result)
  
}

# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data$pcc_factor_unit)

data_levels <- levels(pcc_data$pcc_factor_unit)

setdiff(data_levels, factor_metric_units)


# List to store the results of all models
modelnovar2_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in levels(pcc_data$pcc_factor_unit)) {
  result <- modelnovar2_model(data = pcc_data, metric_unit = unit)
  modelnovar2_list[[unit]] <- result
}


# Combine overall results into one table
modelnovar2_results_list <- do.call(rbind, modelnovar2_list)

modelnovar2_results<-as.data.frame(modelnovar2_results_list)%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  separate(fit.stats.f, into = c("ll.f", "dev.f", "AIC.f","BIC.f","AICc.f"), sep = ", ")%>%
  separate(fit.stats.r, into = c("ll.r", "dev.r", "AIC.r","BIC.r","AICc.r"), sep = ", ")%>%
  select("pcc_factor_unit",     
         "AIC.f" , "AIC.r",
         "LRT" ,"pval")%>%
  mutate_all(~ gsub("AIC = ", "", .))%>%
  dplyr::rename("AIC.three_level" = "AIC.f",
         "AIC.within" = "AIC.r",
         "LRT.within" = "LRT",
         "pval.within"= "pval")%>%
  mutate_at(2:5, as.numeric)%>%
  mutate_at(2:5, ~round(.,4))

#### Heterogeneity of between-study variance (level 3) ---
# Build a two-level model without between-study variance;
# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
#modelnovar3 <- rma.mv(y, v, random = list(~ 1 | effectsizeID, ~ 1 | studyID),
#                    sigma2=c(NA,0), tdist=TRUE, data=dataset)
#anova(overall,modelnovar3)
modelnovar3_model <- function(data, metric_unit) {
    overal_model <- rma.mv(fis.yi, fis.vi, 
                           random = list(~ 1 | ES_ID, ~ 1 | article_id),
                           data = data,
                           method = "REML", 
                           test = "t",
                           dfs="contain",
                           subset = (pcc_factor_unit == metric_unit))
    
    summary(overal_model, digits = 3)
    
    modelnovar3 <- rma.mv(fis.yi, fis.vi, 
                          random = list(~ 1 | ES_ID, ~ 1 | article_id),
                          data = data,
                          method = "REML", 
                          test = "t",
                          dfs="contain", 
                          sigma2 = c(NA, 0),
                          subset = (pcc_factor_unit == metric_unit))
    
    summary(modelnovar3, digits = 3)
    
    anova_result <- anova(overal_model, modelnovar3)
    return(anova_result)
    
  }
  
# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data$pcc_factor_unit)
  
# List to store the results of all models
modelnovar3_list <- list()
  
# Loop over all factor_metric_unit levels and run the models
for (unit in levels(pcc_data$pcc_factor_unit)) {
    result <- modelnovar3_model(data = pcc_data, metric_unit = unit)
    modelnovar3_list[[unit]] <- result
  }
  
# Combine overall results into one table
modelnovar3_results_list <- do.call(rbind, modelnovar3_list)
  
modelnovar3_results <- as.data.frame(modelnovar3_results_list)%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  separate(fit.stats.f, into = c("ll.f", "dev.f", "AIC.f","BIC.f","AICc.f"), sep = ", ")%>%
  separate(fit.stats.r, into = c("ll.r", "dev.r", "AIC.r","BIC.r","AICc.r"), sep = ", ")%>%
  select("pcc_factor_unit",     
         "AIC.r",
         "LRT" ,"pval")%>%
  mutate_all(~ gsub("AIC = ", "", .))%>%
  dplyr::rename("AIC.between" = "AIC.r",
         "LRT.between" = "LRT",
         "pval.between"= "pval")%>%
  mutate_at(2:4, as.numeric)%>%
  mutate_at(2:4, ~round(.,4))
names(modelnovar3_results)
### Table S6.	Results of the comparison between the three-level and two-level model 
#structures for each determinant factor, based on the Akaike Information Criterion (AIC), 
#Likelihood Ratio Test (LRT) and p-value. The three-level model was chosen as the best model
#when its AIC was lower and the LRT statistically significant comparing to both two-level models. 
pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

comparison<- modelnovar2_results%>%
  left_join(modelnovar3_results, by= "pcc_factor_unit")%>%
  mutate(best_model= if_else(pval.within<=0.05 & pval.between<=0.05, "Three-level",
                                  "Two-level"))%>%
  mutate(pval.within= as.character(pval.within),
         pval.between= as.character(pval.between))%>%
  mutate(pval.within = if_else(pval.within==0, "< 0.0001", pval.within),
         pval.between= if_else(pval.within==0, "< 0.0001", pval.between))%>%
  mutate(LRT.pval.within = paste("LRT = ", LRT.within,", p = ",pval.within, sep = ""),
         LRT.pval.between = paste("LRT = ", LRT.between,", p = ",pval.between, sep = ""))%>%
  mutate_all(~ gsub(" = <", " <", .))%>%
  left_join(pcc_factor_class_unit, by= "pcc_factor_unit")%>%
  select("factor_sub_class","pcc_factor_unit",
         "AIC.three_level", "AIC.within",   "AIC.between",
         "LRT.pval.within", "LRT.pval.between",
         "best_model")

length((comparison$best_model[comparison$best_model %in% "Three-level"])) #13
length((comparison$best_model[comparison$best_model %in% "Two-level"])) #54


write.csv(comparison, "results/comparison_best_model.csv", row.names=FALSE)

