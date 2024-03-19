library(dplyr)
library(metafor)
library(tibble)
library(purrr)
library(readxl)
library(stringr)

################# FACTOR CLASSES ----------------
factors_metric_assessed <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
  sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

################################  QQ PLOT: RESIDUALS ###################################################################3----------------------------------------------------#
#### THREE-LEVEL DATA
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(vi),
         pcc_precision = (1/pcc_se))
names(pcc_data_3level)

qqplot_3level <- function(factor_units, pcc_data) {
  num_cols <- 6
  num_rows <- ceiling(length(factor_units) / num_cols)
  par(mfrow = c(num_rows, num_cols), mar = c(3, 3, 1, 1))  # Adjust margins
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    overall_model3 <- rma.mv(yi, vi,
                            random = list(~ 1 | ES_ID, ~ 1 | article_id),
                            test = "t",
                            dfs="contain"
                            data = factor_unit_subset,
                            method = "REML")
    summary(overall_model3, digits = 3)
    
    plot_index <- i %% num_cols
    if (plot_index == 0) plot_index <- num_cols
    
    qqnorm(residuals(overall_model3, type = "rstandard"), 
           main = paste(factor_unit))
    qqline(residuals(overall_model3, type = "rstandard"), col = "red")
  }
  
  par(mfrow = c(1, 1))  # Reset to default plotting layout
}

# Example usage for multiple factor units
factor_metric_units3 <- unique(pcc_data_3level$pcc_factor_unit)

qqplot_3level(factor_metric_units3, pcc_data_3level)


#### TWO-LEVEL DATA
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(vi),
         pcc_precision = (1/pcc_se))

qqplot_2level <- function(factor_units, pcc_data) {
  num_cols <- 6
  num_rows <- ceiling(length(factor_units) / num_cols)
  par(mfrow = c(num_rows, num_cols), mar = c(3, 3, 1, 1))  # Adjust margins
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    overall_model2 <- rma.uni(yi, vi,
                             test = "knha",
                             data = factor_unit_subset,
                             method = "REML")
    

    summary(overall_model2, digits = 3)
    
    plot_index <- i %% num_cols
    if (plot_index == 0) plot_index <- num_cols
    
    qqnorm(residuals(overall_model2, type = "rstandard"), 
           main = paste(factor_unit))
    qqline(residuals(overall_model2, type = "rstandard"), col = "red")
  }
  
  par(mfrow = c(1, 1))  # Reset to default plotting layout
}

# Example usage for multiple factor units
factor_metric_units2 <- unique(pcc_data_2level$pcc_factor_unit)

qqplot_2level(factor_metric_units2, pcc_data_2level)
