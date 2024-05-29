library(metaforest)
library(dplyr)
### THREE-LEVEL META-ANALYSIS
#Data
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate_at(vars(m_mean_farm_size_ha,n_samples_num,n_predictors_num,m_education_years ), as.numeric)%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(pcc_factor_unit== "Education (continuous)")%>%
  select("ES_ID",pcc_factor_unit,fis.yi, "fis.vi" ,
         "m_intervention_recla2",
         "m_region", "m_sub_region",
         "m_mean_farm_size_ha","m_education_years",
         "n_predictors_num","m_av_year_assessment",
         "m_sampling_unit","m_random_sample","m_exact_variance_value",
         "m_type_data","m_model_method")%>%
  na.omit()%>%
  rename("yi"="fis.yi",
         "vi"="fis.vi")


sort(unique(pcc_data_3level$pcc_factor_unit))
sort(unique(pcc_data_3level$m_model_method))
sort(unique(pcc_data_3level$ES_ID))

names(pcc_data_3level)


#Heterogeneity
heterogeneity_3level<- read.csv("results/heterogeneity_3levels.csv",header = TRUE, sep = ",")%>%
  filter(I2_1<=25)

sort(unique(heterogeneity_3level$pcc_factor_unit))


# Run model with many trees to check convergence
check_conv <- metaforest::MetaForest(yi~.,
                                     #vi= "fis.vi",
                         data = pcc_data_3level,
                         study = "ES_ID",
                         whichweights = "random",
                         method = "REML",
                         num.trees = 20000)
plot(check_conv)
# Model with 10000 trees for replication
mf_rep <- MetaForest(yi~.,
                     #vi= "fis.vi",
                     data = pcc_data_3level,
                     study = "ES_ID",
                     whichweights = "random",
                     method = "REML",
                     num.trees = 10000)
# Recursive preselection
preselected <- preselect(mf_rep,
                         replications = 100,
                         algorithm = "recursive")
# Plot results
plot(preselected)

retain_mods <- preselect_vars(preselected, cutoff = .5)
plot(retain_mods)

library(caret)
# Set up 10-fold clustered CV
grouped_cv <- trainControl(method = "cv",
                           index = groupKFold(pcc_data_3level$ES_ID, k = 10))


# Set up a tuning grid
tuning_grid <- expand.grid(whichweights = c("random", "ﬁxed", "unif"),
                           mtry = 2:6,
                           min.node.size = 2:6)

# X should contain only retained moderators, clustering variable, and vi
X <- pcc_data_3level[, c("ES_ID", "vi", retain_mods)]

# Train the model
mf_cv <- train(y = pcc_data_3level$yi,
               x = X,
               study = "ES_ID", # Name of the clustering variable
               method = ModelInfo_mf(),
               trControl = grouped_cv,
               tuneGrid = tuning_grid,
               num.trees = 7500)

# Extract R^2_cvVan Lissa,
r2_cv <- mf_cv$results$Rsquared[which.min(mf_cv$results$RMSE)]

final <- mf_cv$finalModel

# Extract R^2_oob from the ﬁnal model
r2_oob <- final$forest$r.squared
# Plot convergence
plot(final)

# Plot variable importance
VarImpPlot(final)
# Sort the variable names by importance, so that the
# partial dependence plots will be ranked by importance
ordered_vars <- names(final$forest$variable.importance)[
  order(final$forest$variable.importance, decreasing = TRUE)]
# Plot partial dependence
PartialDependence(final, vars = ordered_vars,
                  rawdata = TRUE, pi = .95)

  
  
# List of moderators
moderators <- c("m_intervention_recla2","m_intervention_system_components",
                "m_region", "m_sub_region",
                "m_mean_farm_size_ha","m_education_years",
                "n_samples_num","n_predictors_num","m_av_year_assessment",
                "m_sampling_unit","m_random_sample","m_exact_variance_value",
                "m_type_data","m_model_method",
                "m_endogeneity_correction",
                "m_exposure_correction")

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
          "m_region|m_sub_region|m_intervention_recla2|m_model_method|m_intervention_system_components",
          moderator), "-1", "")
        
        # Check if there are more than one level for the moderator in this subset
        if (length(unique(subset_data[[moderator]])) > 1) {
          tryCatch({
            # Run the analysis
            extension <- rma.mv(fis.yi, fis.vi, 
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
  mutate_at(c("estimate","se","tval","pval" ,"ci.lb","ci.ub",
              "QM", "QMp"),  ~round(.,4))%>%
  mutate(significance2 = if_else(estimate >0 & pval<=0.05,"positive5",
                                 if_else(estimate <0 & pval <=0.05, "negative5",
                                         if_else(estimate >0 &pval>0.05&pval<=0.1, "non_significant",
                                                 if_else(estimate <0 &pval>0.05&pval<=0.1, "non_significant",
                                                         "non_significant")))))%>%
  mutate(f_test= paste("QM (", QMdf1,", ",QMdf2, ") = ",QM, ", p = ",QMp, sep = ""))%>%
  select("moderator","factor_sub_class","pcc_factor_unit","moderator_class",
         "estimate","ci.lb","ci.ub","tval","df","pval" ,
         "f_test","significance2","QMp")%>%
  #Transform back fisher's z to PCC
  mutate(pcc.estimate= transf.ztor(estimate))%>%
  mutate(pcc.ci.lb= transf.ztor(ci.lb))%>%
  mutate(pcc.ci.ub= transf.ztor(ci.ub))