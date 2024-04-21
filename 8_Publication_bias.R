library(dplyr)
library(metafor)
library(tibble)
library(purrr)
library(readxl)
library(stringr)

################# FACTOR CLASS ----------------
factors_metric_assessed <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
  sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
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

##--------- EGGER REGRESSION TEST
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

#### THREE-LEVEL DATA
# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data_3level$pcc_factor_unit)

# List to store the results of all models
egger_3level_list <- lapply(factor_metric_units, function(unit) {
  # Fit the model
  egger_model <- rma.mv(fis.yi, fis.vi, 
                        random = list(~ 1 | ES_ID, ~ 1 | article_id),
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


#### TWO-LEVEL DATA
unit_counts <- pcc_data_2level %>%
  group_by(pcc_factor_unit) %>%
  summarize(n = n())

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
  dplyr::select("factor_sub_class","pcc_factor_unit","moderator","estimate" ,"se" ,"tval", "significance")
  
write.csv(egger_test,"results/egger_test.csv", row.names=FALSE)

##########################################################################################
##--------- Funnel plot
#https://rpubs.com/dylanjcraven/metaforr
### THREE-LEVEL DATA
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
                           random = list(~ 1 | ES_ID, ~ 1 | article_id),
                           test = "t",
                           dfs="contain",
                           data = factor_unit_subset,
                           method = "REML")
    summary(funnel.model, digits = 3)
    
    funnel.plots <- funnel(funnel.model, yaxis="seinv", refline=0 ,ylab="Precision (1/SE)" ,
                           xlab="Residual value", main = paste(factor_unit),level= c(90,95,99),
                           shade=c("white", "gray", "darkgray"),back="white"
    )
    
    # Generate file name
    plot_filename <- paste0("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/figures/funnel_plots/", "plot_", gsub(" ", "_", factor_unit), ".png")
    
    # Save plot
    png(plot_filename)
    print(funnel.plots)
    dev.off()
  }
}

# Example usage for multiple factor units
factor_metric_units3 <- unique(pcc_data_3level$pcc_factor_unit)

funnel_3level(factor_metric_units3, pcc_data_3level)



### TWO-LEVEL DATA
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
    
    funnel.plots <- funnel(funnel.model, yaxis="seinv", refline=0 ,ylab="Precision (1/SE)" ,
                           xlab="Residual value", main = paste(factor_unit),level= c(90,95,99),
                           shade=c("white", "gray", "darkgray"),back="white"
    )
    
    # Generate file name
    plot_filename <- paste0("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/figures/funnel_plots/", "plot_", gsub(" ", "_", gsub("/", "_", factor_unit)), ".png")
    
    # Save plot
    png(plot_filename)
    print(funnel.plots)
    dev.off()
  }
}

# Example usage for multiple factor units
factor_metric_units2 <- unique(pcc_data_2level$pcc_factor_unit)

funnel_2level(factor_metric_units2, pcc_data_2level)

850x580