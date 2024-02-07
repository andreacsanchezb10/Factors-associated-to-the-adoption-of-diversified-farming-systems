library(dplyr)
library(metafor)
library(tibble)
library(purrr)
library(readxl)
library(stringr)

################# META-REGRESSION ----------------
factors_metric_assessed <- read_excel("C:/Users/AndreaSanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/Meta_data_2024.01.25.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

#### THREE-LEVEL META-ANALYSIS
#Data
pcc_data_3level<- read.csv(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/pcc_data_3levels.csv",
                           header = TRUE, sep = ",")%>%
  mutate_at(vars(m_mean_farm_size_ha,n_samples_num,n_predictors_num ), as.numeric)%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)

sort(unique(pcc_data_3level$pcc_factor_unit))
sort(unique(pcc_data_3level$n_articles))

#Heterogeneity
heterogeneity_3level<- read.csv("heterogeneity_3levels.csv",header = TRUE, sep = ",")

sort(unique(heterogeneity_3level$pcc_factor_unit))

# List of moderators
moderators <- c("m_region", "m_sub_region","m_intervention_recla2","m_mean_farm_size_ha",
                "m_model_method","m_random_sample","m_exact_variance_value","m_type_data",
                "n_samples_num","n_predictors_num")

# List of pcc_factor_unit
pcc_factor_units <- unique(pcc_data_3level$pcc_factor_unit)

# Create an empty list to store results
results_list <- list()

for (moderator in moderators) {
  results <- pcc_factor_units %>% 
    map_df(~ {
      # Create subset for the current pcc_factor_unit
      subset_data <- subset(pcc_data_3level, pcc_factor_unit == .x)
      
      # Check if the current pcc_factor_unit has more than one level for the moderator
      if (length(unique(subset_data[[moderator]])) > 1) {
        # Determine whether to include "-1" in the formula
        formula_suffix <- ifelse(grepl("m_region|m_sub_region|m_intervention_recla2", moderator), "-1", "")
        
        # Run the analysis
        extension <- rma.mv(yi, vi, 
                            random = list(~ 1 | ES_ID, ~ 1 | article_id),
                            mods = as.formula(paste("~", moderator, formula_suffix)),
                            data = subset_data,
                            method = "REML", 
                            test = "t",
                            dfs = "contain")
        
        # Extract relevant information from the summary
        summary_data <- data.frame(
          pcc_factor_unit = as.character(.x),
          moderator = as.character(moderator),
          rownames_to_column(coef(summary(extension)), var = "moderator_class"),
          beta = coef(summary(extension))[, "estimate"],
          se = coef(summary(extension))[, "se"],
          ci.lb = coef(summary(extension))[, "ci.lb"],
          ci.ub = coef(summary(extension))[, "ci.ub"],
          tval = coef(summary(extension))[, "tval"],
          pval = coef(summary(extension))[, "pval"],
          df = coef(summary(extension))[, "df"],
          stringsAsFactors = FALSE
        )
        
        return(summary_data)
      } else {
        # If less than 2 levels, return NULL
        return(NULL)
      }
    })
  
  # Filter out NULL results and store the non-empty results in the list
  results_list[[moderator]] <- results[!sapply(results, is.null)]
}

# Combine results from the list into a single data frame
meta_regression_3levels_df <- bind_rows(results_list)%>%
  left_join(pcc_factor_class_unit, by= "pcc_factor_unit")%>%
  mutate(significance2 = if_else(beta >0 & pval <=0.05, "significant_positive",
                                 if_else(beta <0 & pval <=0.05, "significant_negative",
                                         if_else(beta>0&pval>0.05,"no_significant_positive",
                                                 "no_significant_negative"))))%>%
  mutate(moderator=str_replace_all(moderator, "~", ""))%>%
  mutate(moderator=str_replace_all(moderator, "-1", ""))%>%
  mutate(moderator_class= str_replace(.$moderator_class, paste0(".*", .$moderator), ""))

sort(unique(meta_regression_3levels_df$moderator))
sort(unique(meta_regression_3levels_df$pcc_factor_unit))

write.csv(meta_regression_3levels_df,"meta_regression_3levels.csv", row.names=FALSE)

##########################################################################################
########### TWO-LEVEL META-ANALYSIS ############################################
#Data
pcc_data_2level<- read.csv("pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(m_mean_farm_size_ha= as.numeric(m_mean_farm_size_ha))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)
sort(unique(pcc_data_2level$pcc_factor_unit))    
          
#Heterogeneity
heterogeneity_2level<- read.csv("heterogeneity_2levels.csv",header = TRUE, sep = ",")%>%
  filter(I2>=75)

sort(unique(heterogeneity_2level$pcc_factor_unit))

#pcc database after removing factors with <10 studies;
#factors with sampling variance > 25% 
m_pcc_data_2level<- pcc_data_2level%>%
  dplyr::left_join(heterogeneity_2level, by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  filter(!is.na(I2))

sort(unique(m_pcc_data_2level$pcc_factor_unit))

# List of moderators
moderators <- c("m_region", "m_sub_region","m_intervention_recla2","m_mean_farm_size_ha",
                "m_model_method","m_random_sample","m_exact_variance_value","m_type_data",
                "n_samples_num","n_predictors_num")
# List of pcc_factor_unit
pcc_factor_units <- unique(m_pcc_data_2level$pcc_factor_unit)

# Create an empty list to store results
results_list2 <- list()

for (moderator in moderators) {
  results <- pcc_factor_units %>% 
    map_df(~ {
      # Create subset for the current pcc_factor_unit
      subset_data <- subset(m_pcc_data_2level, pcc_factor_unit == .x)
      
      # Check if the current pcc_factor_unit has more than one level for the moderator
      if (length(unique(subset_data[[moderator]])) > 1) {
        # Determine whether to include "-1" in the formula
        formula_suffix <- ifelse(grepl("m_region|m_sub_region|m_intervention_recla2", moderator), "-1", "")
        
        # Run the analysis
        extension <- rma.uni(yi, vi,
                             mods = as.formula(paste("~", moderator, formula_suffix)),
                             data = subset_data,
                             method = "REML", 
                             test = "knha")
        
        # Extract relevant information from the summary
        summary_data <- data.frame(
          pcc_factor_unit = as.character(.x),
          moderator = as.character(moderator),
          rownames_to_column(coef(summary(extension)), var = "moderator_class"),
          beta = coef(summary(extension))[, "estimate"],
          se = coef(summary(extension))[, "se"],
          ci.lb = coef(summary(extension))[, "ci.lb"],
          ci.ub = coef(summary(extension))[, "ci.ub"],
          tval = coef(summary(extension))[, "tval"],
          pval = coef(summary(extension))[, "pval"],
          df = coef(summary(extension))[, "df"],
          stringsAsFactors = FALSE
        )
        
        return(summary_data)
      } else {
        # If less than 2 levels, return NULL
        return(NULL)
      }
    })
  
  # Filter out NULL results and store the non-empty results in the list
  results_list2[[moderator]] <- results[!sapply(results, is.null)]
}


# Combine results from the list into a single data frame
meta_regression_2levels_df <- bind_rows(results_list2)%>%
  left_join(pcc_factor_class_unit, by= "pcc_factor_unit")%>%
  mutate(significance2 = if_else(beta >0 & pval <=0.05, "significant_positive",
                                 if_else(beta <0 & pval <=0.05, "significant_negative",
                                         if_else(beta>0&pval>0.05,"no_significant_positive",
                                                 "no_significant_negative"))))%>%
  mutate(moderator=str_replace_all(moderator, "~", ""))%>%
  mutate(moderator=str_replace_all(moderator, "-1", ""))%>%
  mutate(moderator_class= str_replace(.$moderator_class, paste0(".*", .$moderator), ""))

sort(unique(meta_regression_2levels_df$moderator))
sort(unique(meta_regression_2levels_df$pcc_factor_unit))

write.csv(meta_regression_2levels_df,"meta_regression_2levels.csv", row.names=FALSE)
