library(dplyr)
library(metafor)
library(tibble)
library(purrr)
library(readxl)
library(stringr)

################# META-REGRESSION ----------------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_metric_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$logor_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_category,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

#### THREE-LEVEL META-ANALYSIS
#Data
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate_at(vars(m_mean_farm_size_ha,n_samples,n_factors,m_education_years ), as.numeric)%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_studies = n_distinct(study_id))%>%
  dplyr::mutate(n_es = n_distinct(ES_ID))%>%
  #filter(n_articles>9)
  filter(n_es>9)
    
sort(unique(pcc_data_3level$pcc_factor_unit))
sort(unique(pcc_data_3level$model_method_recla))
names(pcc_data_3level)

#Heterogeneity
heterogeneity_3level<- read.csv("results/heterogeneity_3levels.csv",header = TRUE, sep = ",")%>%
  filter(I2_1<=25)

sort(unique(heterogeneity_3level$pcc_factor_unit))

# List of moderators
moderators <- c("m_dp_recla",
                "m_un_region", "m_un_subregion",
                "m_mean_farm_size_ha","m_education_years",
                "n_factors","m_av_year_assessment",
                "m_sampling_unit","m_random_sample",
                "m_type_data","model_method_recla")

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
      if (length(unique(subset_data[[moderator]])) > 1 && !all(is.na(subset_data[[moderator]]))) {
        # Determine whether to include "-1" in the formula
        formula_suffix <- ifelse(grepl(
          "m_un_region|m_un_subregion|m_dp_recla|model_method_recla",
          moderator), "-1", "")
        
        # Check if there are more than one level for the moderator in this subset
        if (length(unique(subset_data[[moderator]])) > 1) {
          tryCatch({
            # Run the analysis
            extension <- rma.mv(fis.yi, fis.vi, 
                                random = list(~ 1 | ES_ID, ~ 1 | study_id),
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
              QM = extension$QM,
              QMdf1 = extension$QMdf[1],
              QMdf2 =extension$QMdf[2],
              QMp= extension$QMp,
              stringsAsFactors = FALSE)
            
            return(summary_data)
          }, error = function(e) {
            # Print information when an error occurs
            cat("Error occurred for pcc_factor_unit:", .x, "and moderator:", moderator, "\n")
            print(e)
            return(NULL)
          })
        } else {
          # If less than 2 levels, return NULL
          return(NULL)
        }
      } else {
        # If less than 2 levels or all NAs, return NULL
        return(NULL)
      }
    })
  
  results_list[[moderator]] <- results[!sapply(results, is.null)]
}
  

# Combine results from the list into a single data frame
meta_regression_3levels_df <- bind_rows(results_list)%>%
  #rename("beta"="estimate")%>%
  left_join(pcc_factor_class_unit, by= "pcc_factor_unit")%>%
  mutate(moderator=str_replace_all(moderator, "~", ""))%>%
  mutate(moderator=str_replace_all(moderator, "-1", ""))%>%
  mutate(moderator_class= str_replace(.$moderator_class, paste0(".*", .$moderator), ""))%>%
  #mutate_at(c("estimate","se","tval","pval" ,"ci.lb","ci.ub",
   #           "QM", "QMp"),  ~round(.,4))%>%
  mutate(significance2 = if_else(estimate >0 & pval<=0.05,"positive5",
                                 if_else(estimate <0 & pval <=0.05, "negative5",
                                         if_else(estimate >0 &pval>0.05&pval<=0.1, "non_significant",
                                                 if_else(estimate <0 &pval>0.05&pval<=0.1, "non_significant",
                                                         "non_significant")))))%>%
  mutate(f_test= paste("QM (", QMdf1,", ",QMdf2, ") = ",QM, ", p = ",QMp, sep = ""))%>%
  select("moderator","factor_category","pcc_factor_unit","moderator_class",
         "estimate","se","ci.lb","ci.ub","tval","df","pval" ,
         "f_test","significance2","QMp")%>%
  #Transform back fisher's z to PCC
  mutate(pcc.estimate= transf.ztor(estimate))%>%
  mutate(pcc.ci.lb= transf.ztor(ci.lb))%>%
  mutate(pcc.ci.ub= transf.ztor(ci.ub))%>%
  mutate_at(c("pcc.estimate","pcc.ci.lb","pcc.ci.ub"),  ~round(.,2))


names(meta_regression_3levels_df)
sort(unique(meta_regression_3levels_df$moderator))
sort(unique(meta_regression_3levels_df$pcc_factor_unit))

write.csv(meta_regression_3levels_df,"results/meta_regression_3levels.csv", row.names=FALSE)

##########################################################################################
########### TWO-LEVEL META-ANALYSIS ############################################
#Data
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(m_mean_farm_size_ha= as.numeric(m_mean_farm_size_ha))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_studies = n_distinct(study_id))%>%
  dplyr::mutate(n_es = n_distinct(ES_ID))%>%
  filter(n_es>9)

sort(unique(pcc_data_2level$pcc_factor_unit))    
table(pcc_data_2level$pcc_factor_unit,pcc_data_2level$n_es)          
#Heterogeneity
heterogeneity_2level<- read.csv("results/heterogeneity_2levels.csv",header = TRUE, sep = ",")%>%
  filter(I2>=75)

sort(unique(heterogeneity_2level$pcc_factor_unit))

#pcc database after removing factors with <10 studies;
#factors with sampling variance > 25% 
m_pcc_data_2level<- pcc_data_2level%>%
  dplyr::left_join(heterogeneity_2level, by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  filter(!is.na(I2))

sort(unique(m_pcc_data_2level$pcc_factor_unit))

# List of pcc_factor_unit
pcc_factor_units <- unique(m_pcc_data_2level$pcc_factor_unit)

# Create an empty list to store results
results_list2 <- list()

for (moderator in moderators) {
  results <- pcc_factor_units %>% 
    map_df(~ {
      # Create subset for the current pcc_factor_unit
      subset_data <- subset(pcc_data_2level, pcc_factor_unit == .x)
      
      # Check if the current pcc_factor_unit has more than one level for the moderator
      if (length(unique(subset_data[[moderator]])) > 1 && !all(is.na(subset_data[[moderator]]))) {
        # Determine whether to include "-1" in the formula
        formula_suffix <- ifelse(grepl(
          "m_un_region|m_un_subregion|m_dp_recla|model_method_recla",
          moderator), "-1", "")
        
        # Check if there are more than one level for the moderator in this subset
        if (length(unique(subset_data[[moderator]])) > 1) {
          tryCatch({
            # Run the analysis
            extension <- rma.uni(fis.yi, fis.vi,
                                 mods = as.formula(paste("~", moderator, formula_suffix)),
                                 data = subset_data,
                                 method = "REML", 
                                 test = "knha")
            
            # Extract relevant information from the summary
            summary_data <- data.frame(
              pcc_factor_unit = as.character(.x),
              moderator = as.character(moderator),
              rownames_to_column(coef(summary(extension)), var = "moderator_class"),
              QM = extension$QM,
              QMdf1 = extension$QMdf[1],
              QMdf2 =extension$QMdf[2],
              QMp= extension$QMp,
              stringsAsFactors = FALSE
            )
            
            return(summary_data)
          }, error = function(e) {
            # Print information when an error occurs
            cat("Error occurred for pcc_factor_unit:", .x, "and moderator:", moderator, "\n")
            print(e)
            return(NULL)
          })
        } else {
          # If less than 2 levels, return NULL
          return(NULL)
        }
      } else {
        # If less than 2 levels or all NAs, return NULL
        return(NULL)
      }
    })
  
  results_list2[[moderator]] <- results[!sapply(results, is.null)]
}


# Combine results from the list into a single data frame
meta_regression_2levels_df <- bind_rows(results_list2)%>%
  left_join(pcc_factor_class_unit, by= "pcc_factor_unit")%>%
  mutate(moderator=str_replace_all(moderator, "~", ""))%>%
  mutate(moderator=str_replace_all(moderator, "-1", ""))%>%
  mutate(moderator_class= str_replace(.$moderator_class, paste0(".*", .$moderator), ""))%>%
  #mutate_at(c("estimate","se","tval","pval","ci.lb","ci.ub",
   #           "QM", "QMp"),  ~round(.,4))%>%
  mutate(significance2 = if_else(estimate >0 & pval<=0.05,"positive5",
                                 if_else(estimate <0 & pval <=0.05, "negative5",
                                         if_else(estimate >0 &pval>0.05&pval<=0.1, "non_significant",
                                                 if_else(estimate <0 &pval>0.05&pval<=0.1, "non_significant",
                                                         "non_significant")))))%>%
  mutate(f_test= paste("QM (", QMdf1,", ",QMdf2, ") = ",QM, ", p = ",QMp, sep = ""))%>%
  select("moderator","factor_category","pcc_factor_unit","moderator_class",
         "estimate","se","ci.lb","ci.ub","tval","df","pval" ,
         "f_test","significance2","QMp")%>%
  #Transform back fisher's z to PCC
  mutate(pcc.estimate= transf.ztor(estimate))%>%
  mutate(pcc.ci.lb= transf.ztor(ci.lb))%>%
  mutate(pcc.ci.ub= transf.ztor(ci.ub))%>%
  mutate_at(c("pcc.estimate","pcc.ci.lb","pcc.ci.ub"),  ~round(.,2))
  

sort(unique(meta_regression_2levels_df$moderator))
sort(unique(meta_regression_2levels_df$pcc_factor_unit))

write.csv(meta_regression_2levels_df,"results/meta_regression_2levels.csv", row.names=FALSE)

meta_regression_df<- rbind(meta_regression_3levels_df,meta_regression_2levels_df)%>%
  mutate(significance= format(pval, scientific = TRUE))%>%
  mutate(significance = if_else(pval <=0.001,paste(significance,"***",sep=""),
                                if_else(pval>0.001&pval<0.01,paste(significance,"**",sep=""),
                                        if_else(pval>0.01&pval<=0.05,paste(significance,"*",sep=""),
                                                if_else(pval>0.05&pval<=0.1,paste(significance,"\u2020",sep=""),
                                                paste(significance))))))
sort(unique(meta_regression_df$pcc_factor_unit))

write.csv(meta_regression_df,"results/meta_regression.csv", row.names=FALSE)
