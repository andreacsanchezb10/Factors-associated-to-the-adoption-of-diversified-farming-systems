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
  mutate(pcc_se = sqrt(fis.vi),
         pcc_precision = (1/pcc_se))
names(pcc_data_3level)

qqplot_3level <- function(factor_units, pcc_data) {
  # Generate unique folder name based on current timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  folder_path <- paste0("plots_", timestamp, "/")
  
  # Create the folder
  dir.create(folder_path)
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    overall_model3 <- rma.mv(fis.yi, fis.vi,
                              random = list(~ 1 | ES_ID, ~ 1 | article_id),
                              test = "t",
                              dfs="contain",
                              data = factor_unit_subset,
                              method = "REML")
    summary(overall_model3, digits = 3)
    
    qqplot<- qqnorm(residuals(overall_model3, type = "rstandard"), 
                    main = paste(factor_unit),level= overall_model3$level)
      qqline(residuals(overall_model3, type = "rstandard"), col = "red")
    
    
    # Generate file name
    plot_filename <- paste0("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/figures/funnel_plots/", "plot_", gsub(" ", "_", factor_unit), ".png")
    
    # Save plot
    png(plot_filename)
    print(qqplot)
    dev.off()
  }
}

# Example usage for multiple factor units
factor_metric_units3 <- unique(pcc_data_3level$pcc_factor_unit)

qqplot_3level(factor_metric_units3, pcc_data_3level)


#### TWO-LEVEL DATA
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(fis.vi),
         pcc_precision = (1/pcc_se))%>%
  filter(pcc_factor_unit==
      
         
           
            "Perceived erosion reduction benefit (1= yes, 0= others)" )

sort(unique(pcc_data_2level$pcc_factor_unit))
               
qqplot_2level <- function(factor_units, pcc_data) {
  # Generate unique folder name based on current timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  folder_path <- paste0("plots_", timestamp, "/")
  
  # Create the folder
  dir.create(folder_path)
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    overall_model2 <- rma(fis.yi, fis.vi,
                        test = "knha",
                        data = factor_unit_subset,
                        method = "REML")
    summary(overall_model2, digits = 3)
    
    qqplot<- qqnorm(residuals(overall_model2, type = "rstandard"), 
           main = paste(factor_unit),envelope=TRUE)
    qqline(residuals(overall_model2, type = "rstandard"), col = "red")
    
    # Generate file name
    plot_filename <- paste0("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/figures/funnel_plots/", "plot_", gsub(" ", "_", gsub("/", "_", factor_unit)), ".png")
    
    # Save plot
    png(plot_filename)
    print(qqplot)
    dev.off()
  }
}


# Example usage for multiple factor units
factor_metric_units2 <- unique(pcc_data_2level$pcc_factor_unit)

qqplot_2level(factor_metric_units2, pcc_data_2level)

