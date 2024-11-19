library(dplyr)
library(metafor)
library(tibble)
library(purrr)
library(readxl)
library(stringr)

################# FACTOR CLASS ----------------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_metric_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$logor_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_category,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

################################  PUBLICATION BIAS ###################################################################3----------------------------------------------------#
#### THREE-LEVEL DATA
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(fis.vi),
         pcc_precision = (1/pcc_se))
names(pcc_data_3level)

#### TWO-LEVEL DATA
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(fis.vi),
         pcc_precision = (1/pcc_se))
  
sort(unique(pcc_data_2level$pcc_factor_unit))
names(pcc_data_2level)
##--------- EGGER REGRESSION TEST----
# Define a function to extract and format model results
extract_model_results <- function(result, unit, source) {
  data.frame(
    pcc_factor_unit = unit,
    moderator = source,
    Estimate = result[1],
    SE = result[2],
    tval = result[3],
    CI_Upper = result[4],
    CI_Lower = result[5]
  )
}

#### THREE-LEVEL DATA----
# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data_3level$pcc_factor_unit)

# List to store the results of all models
egger_3level_list <- lapply(factor_metric_units, function(unit) {
  # Fit the model
  egger_model <- rma.mv(fis.yi, fis.vi, 
                        random = list(~ 1 | ES_ID, ~ 1 | study_id),
                        data = pcc_data_3level,
                        mod = ~pcc_precision,
                        method = "REML", 
                        test = "t",
                        dfs = "contain",
                        subset = (pcc_factor_unit == unit))
  
  # Extract model results
  result <- coef(summary(egger_model))
  intercept_result <- result[1, ]
  precision_result <- result[2, ]
  
  # Combine results into a list
  rbind(extract_model_results(intercept_result, unit, "Intercept"),
        extract_model_results(precision_result, unit, "Precision"))
})

# Combine all results into one data frame
egger_3level_results <- do.call(rbind, egger_3level_list)


#### TWO-LEVEL DATA----
unit_counts <- pcc_data_2level %>%
  group_by(pcc_factor_unit) %>%
  dplyr::summarize(n = n())

# Filter out levels with insufficient data
selected_units <- unit_counts %>%
  filter(n >= 3) %>%
  pull(pcc_factor_unit)

sort(unique(pcc_data_2level$pcc_factor_unit))    

all_units <- unique(pcc_data_2level$pcc_factor_unit)

# Get the levels that are missed
missed_units <- setdiff(all_units, selected_units)

# Print the missed levels
print(missed_units)

# Fit the model only for selected levels of pcc_factor_unit
egger_2level_list <- lapply(selected_units, function(unit) {
  egger_model2 <- rma.uni(fis.yi, fis.vi, 
                          data = pcc_data_2level %>% filter(pcc_factor_unit == unit),
                          mod = ~pcc_precision,
                          method = "REML", 
                          test = "knha")
  
  # Extract model results
  result <- coef(summary(egger_model2))
  intercept_result <- result[1, ]
  precision_result <- result[2, ]
  
  # Combine results into a list
  rbind(extract_model_results(intercept_result, unit, "Intercept"),
        extract_model_results(precision_result, unit, "Precision"))
})
# Combine all results into one data frame
egger_2level_results <- do.call(rbind, egger_2level_list)

### COMBINE THREE AND TWO LEVEL RESULTS

egger_test<- rbind(egger_3level_results,egger_2level_results)%>%
  mutate(pval= round(pval,4),
         tval=round(tval,4))%>%
  mutate(significance = if_else(pval <=0.001,paste(pval,"***",sep=""),
                                if_else(pval>0.001&pval<0.01,paste(pval,"**",sep=""),
                                        if_else(pval>0.01&pval<=0.05,paste(pval,"*",sep=""),
                                                        paste(pval)))))%>%
  mutate(significance= if_else(significance=="0***","<0.0001***",significance))%>%
  left_join(pcc_factor_class_unit, by= "pcc_factor_unit")%>%
  dplyr::select("factor_category","pcc_factor_unit","moderator","estimate" ,"se" ,"tval", "significance")
  
write.csv(egger_test,"results/egger_test.csv", row.names=FALSE)

##########################################################################################
##--------- Funnel plot-----
#https://rpubs.com/dylanjcraven/metaforr
### THREE-LEVEL DATA----
funnel_3level <- function(factor_units, pcc_data) {
  # Generate unique folder name based on current timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  folder_path <- paste0("plots_", timestamp, "/")
  
  # Create the folder
  dir.create(folder_path)
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    funnel.model <- rma.mv(fis.yi, fis.vi,
                           random = list(~ 1 | ES_ID, ~ 1 | study_id),
                           test = "t",
                           dfs="contain",
                           data = factor_unit_subset,
                           method = "REML")
    summary(funnel.model, digits = 3)
    
    #trimandfill <- trimfill(funnel.model)
    
    funnel.plots <- funnel(funnel.model, yaxis="seinv", refline=0 ,ylab="Precision (1/SE)" ,
                           xlab="Residual value", main = paste(factor_unit),level= c(90,95,99),
                           shade=c("white", "gray", "darkgray"),back="white"
    )
    
    # Generate file name
    plot_filename <- paste0("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/figures/GFS_submission_2/funnel_plots/", "plot_", gsub(" ", "_", factor_unit), ".png")
    
    # Save plot
    png(plot_filename)
    print(funnel.plots)
    dev.off()
  }
}

# Example usage for multiple factor units
factor_metric_units3 <- unique(pcc_data_3level$pcc_factor_unit)
factor_metric_units3

#error in "Access to information (Access to information)"                                                   

funnel_3level(factor_metric_units3, pcc_data_3level)
#850x580
                     

### TWO-LEVEL DATA-----
funnel_2level <- function(factor_units, pcc_data) {
  # Generate unique folder name based on current timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  folder_path <- paste0("plots_", timestamp, "/")
  
  # Create the folder
  dir.create(folder_path)
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    funnel.model <- rma(fis.yi, fis.vi,
                           test = "knha",
                           data = factor_unit_subset,
                           method = "REML")
    summary(funnel.model, digits = 3)
    
    trimandfill <- trimfill(funnel.model)
    
    funnel.plots <- funnel(trimandfill, yaxis="seinv", refline=0 ,ylab="Precision (1/SE)" ,
                           xlab="Residual value", main = paste(factor_unit),level= c(90,95,99),
                           shade=c("white", "gray", "darkgray"),back="white"
    )
    
    # Generate file name
    plot_filename <- paste0("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/figures/GFS_submission_2/funnel_plots/", "plot_", gsub(" ", "_", gsub("/", "_", factor_unit)), ".png")
    
    # Save plot
    png(plot_filename)
    print(funnel.plots)
    dev.off()
  }
}

# Example usage for multiple factor units
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(fis.vi),
         pcc_precision = (1/pcc_se))
pcc_data_2level<-pcc_data_2level%>%
  filter(pcc_factor_unit==  "Awareness (Climate change)")
factor_metric_units2 <- unique(pcc_data_2level$pcc_factor_unit)
factor_metric_units2
funnel_2level(factor_metric_units2, pcc_data_2level)

#850x580
                         
# error in "Perceived benefit from practice (Soil erosion reduction)"                                               


##--------- TRIM AND FILL METHOD ----
### TWO-LEVEL DATA-----
sort(unique(pcc_data_2level$pcc_factor_unit))
trimfill_data<-pcc_data_2level%>%
  filter(
    pcc_factor_unit=="Soil slope (Flat)"|
      pcc_factor_unit== "Fertilizer use (Organic or manure)"|
      pcc_factor_unit== "Perceived production constraint (Soil erosion)"|
      pcc_factor_unit=="Non-farm income (Amount)" |
      pcc_factor_unit== "Total income (Total income)"|
      pcc_factor_unit=="Household head (Farming experience)"|
      pcc_factor_unit=="Hired labour (Number of workers)"|
      pcc_factor_unit=="Access to irrigation (Access to irrigation)"|
      pcc_factor_unit=="Access to training (Access to training)"|
      pcc_factor_unit=="Land tenure security (Secure tenure)"|
      pcc_factor_unit=="Association membership (Association membership)"|
      pcc_factor_unit=="Communicate with other farmers (Communicate with other farmers)"|
      pcc_factor_unit=="Relatives and friends (Relatives and friends)")
         
trimfill_2level <- function(data, metric_unit) {
  overal_model <- rma(fis.yi, fis.vi, 
                      data = data,
                      method = "REML", 
                      test = "knha",
                      subset = (pcc_factor_unit == metric_unit))
  

  return(trimfill(overal_model))
  
}

# Vector of factor_metric_unit levels
factor_metric_units <- unique(trimfill_data$pcc_factor_unit)

# List to store the results of all models
trimfill_2level_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- trimfill_2level(data = trimfill_data, metric_unit = unit)
  trimfill_2level_list[[unit]] <- result
}

# Combine overall results into one table
trimfill_2level_results_list<- do.call(rbind, trimfill_2level_list)

trimfill_2level_results <- as.data.frame(trimfill_2level_results_list)%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate_at(2:8, as.numeric)%>%
  mutate(across(where(is.numeric), ~ round(., 3)))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval>=0.1,"","",
                                                        "")))))%>%
  mutate(results= paste(b, " [",ci.lb,", ", ci.ub,"]",significance, sep=""))%>%
  select(pcc_factor_unit,results)
  
write.csv(trimfill_2level_results,"results/trimfill_2level_results.csv", row.names=FALSE)

