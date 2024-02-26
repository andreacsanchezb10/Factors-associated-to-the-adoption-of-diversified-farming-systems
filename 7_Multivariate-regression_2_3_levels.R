
#__________MULTIVARIATE REGRESSION MODEL
#https://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin
library(MuMIn)
library(tibble)
library(stringr)
library(metafor)
library(purrr)
library(glmulti)


moderators.formula <- as.formula("~m_intervention_recla2+m_intervention_system_components+m_region+
                                 m_mean_farm_size_ha+m_education_years+
                                 n_samples_num+n_predictors_num+
                                 m_av_year_assessment+m_sampling_unit+
                                 m_random_sample+m_exact_variance_value+
                                 m_type_data+m_model_method+
                                 m_endogeneity_correction+m_exposure_correction")
                                 
variables_to_exclude <- c("m_intervention_recla2","m_intervention_system_components",
                                 "m_region",
                                 "m_mean_farm_size_ha","m_education_years",
                                 "n_samples_num","n_predictors_num","m_av_year_assessment",
                                 "m_sampling_unit","m_random_sample","m_exact_variance_value",
                                 "m_type_data","m_model_method",
                                 "m_endogeneity_correction","m_exposure_correction")


eval(metafor:::.MuMIn)

##########################################################################################################################################################
#################### TWO-LEVEL META-ANALYSIS ##########################################################################################
##########################################################################################################################################################
#Data
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(m_mean_farm_size_ha= as.numeric(m_mean_farm_size_ha))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)
sort(unique(pcc_data_2level$pcc_factor_unit))    

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

unique_units <- unique(m_pcc_data_2level$pcc_factor_unit)
importance_list <- list()


for (unit in unique_units) {
  subset_data <- m_pcc_data_2level %>%
    filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Check if any of the specified variables has 0 articles for the current unit
  if (any(sapply(variables_to_exclude, function(var) sum(subset_data[[var]] == 1) > 0))) {
    # Check if any of the specified variables has <2 levels
    if (any(sapply(variables_to_exclude, function(var) length(unique(subset_data[[var]])) < 2))) {
      print("Excluding variables due to <2 levels:")
      print(variables_to_exclude[sapply(variables_to_exclude, function(var) length(unique(subset_data[[var]])) < 2)])
      # If there are articles for at least one variable and <2 levels for all variables, include all variables in the moderators.formula
      current_formula <- moderators.formula
    } else {
      # If there are 0 articles for all variables or >=2 levels for any variable, exclude them from the moderators.formula
      print("Excluding variables due to 0 articles or >=2 levels:")
      print(variables_to_exclude)
      current_formula <- as.formula(gsub(paste(variables_to_exclude, collapse = "+"), "", as.character(moderators.formula)))
    }
  } else {
    # If there are 0 articles for all variables, exclude them from the moderators.formula
    print("Excluding variables due to 0 articles:")
    print(variables_to_exclude)
    current_formula <- as.formula(gsub(paste(variables_to_exclude, collapse = "+"), "", as.character(moderators.formula)))
  }
  
  # Perform analysis
  full <- rma(yi, vi, mods = current_formula, data = subset_data, method = "ML")
  
  # Store weightable results in the list
  res <- dredge(full, trace = 2)
  importance_list[[unit]] <- sw(res)
}


# Convert the list to a data.frame
importance_df_2levels <- do.call(rbind, lapply(importance_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit.moderator")%>%
  mutate(moderator= str_extract(pcc_factor_unit.moderator, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.moderator, '^[^.]+'))%>%
  rename("Importance"="X[[i]]")%>%
  #filter(Importance>=0.8)%>%
  select(pcc_factor_unit,moderator,Importance)

sort(unique(importance_df_2levels$pcc_factor_unit))
subset(res, delta <= 2, recalc.weights=FALSE)

##########################################################################################################################################################
########################  THREE-LEVEL META-ANALYSIS ##########################################################################################################################################################
##########################################################################################################################################################
#Data
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate_at(vars(m_mean_farm_size_ha,n_samples_num,n_predictors_num,m_education_years ), as.numeric)%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)%>%
  filter(pcc_factor_unit=="Association member (1= yes)" )

sort(unique(pcc_data_3level$pcc_factor_unit))
sort(unique(pcc_data_3level$m_model_method))
sort(unique(pcc_data_3level$m_sub_region))


sort(unique(pcc_data_3level$pcc_factor_unit))

unique_units <- unique(pcc_data_3level$pcc_factor_unit)
importance_list <- list()


for (unit in unique_units) {
  subset_data <- pcc_data_3level %>%
    filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Check if any of the specified variables has 0 articles for the current unit
  if (any(sapply(variables_to_exclude, function(var) sum(subset_data[[var]] == 1) > 0))) {
    # Check if any of the specified variables has <2 levels
    if (any(sapply(variables_to_exclude, function(var) length(unique(subset_data[[var]])) < 2))) {
      print("Excluding variables due to <2 levels:")
      print(variables_to_exclude[sapply(variables_to_exclude, function(var) length(unique(subset_data[[var]])) < 2)])
      # If there are articles for at least one variable and <2 levels for all variables, include all variables in the moderators.formula
      current_formula <- moderators.formula
    } else {
      # If there are 0 articles for all variables or >=2 levels for any variable, exclude them from the moderators.formula
      print("Excluding variables due to 0 articles or >=2 levels:")
      print(variables_to_exclude)
      current_formula <- as.formula(gsub(paste(variables_to_exclude, collapse = "+"), "", as.character(moderators.formula)))
    }
  } else {
    # If there are 0 articles for all variables, exclude them from the moderators.formula
    print("Excluding variables due to 0 articles:")
    print(variables_to_exclude)
    current_formula <- as.formula(gsub(paste(variables_to_exclude, collapse = "+"), "", as.character(moderators.formula)))
  }
  
  # Perform analysis
  full <- rma.mv(yi, vi, 
                 mods = current_formula,
                 random = list(~ 1 | ES_ID, ~ 1 | article_id),
                 data=subset_data, 
                 method="ML")
  
  # Store weightable results in the list
  res <- dredge(full, trace = 2)
  importance_list[[unit]] <- sw(res)
}

# Convert the list to a data.frame
importance_df_3levels <- do.call(rbind, lapply(importance_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit.moderator")%>%
  mutate(moderator= str_extract(pcc_factor_unit.moderator, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.moderator, '^[^.]+'))%>%
  rename("Importance"="X[[i]]")%>%
  #filter(Importance>=0.8)%>%
  select(pcc_factor_unit,moderator,Importance)

sort(unique(importance_df_3levels$pcc_factor_unit))
subset(res, delta <= 2, recalc.weights=FALSE)










########################################################################################







### MODEL SELECTION

rma.glmulti <- function(formula, data, ...) {
  rma(formula, vi, data=data, method="ML", ...)
}

# Get unique values of pcc_factor_unit
pcc_factor_units <- unique(m_pcc_data_2level$pcc_factor_unit)

# Initialize a list to store results
weigh_list <- list()

for (unit in unique_units) {
  # Create subset
  subset_data <- m_pcc_data_2level %>%
    dplyr::filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Perform analysis
  res <- glmulti(moderators.formula, 
                 data=subset_data,
                 level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=32)
  
  # Store weightable results in the list
  weigh_list[[unit]] <- weightable(res)
  
}

# Convert the list to a data.frame
weigh_df_2levels <- do.call(rbind, lapply(weigh_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit.model")
  mutate(n_model= str_extract(pcc_factor_unit.model, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.model, '^[^.]+'))%>%
  group_by(pcc_factor_unit)%>%
  mutate(min.aicc= min(aicc)+2)%>%
  filter(aicc <min.aicc)




# Convert the list to a data.frame
weigh_df_2levels <- do.call(rbind, lapply(weigh_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit.model")%>%
  mutate(n_model= str_extract(pcc_factor_unit.model, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.model, '^[^.]+'))%>%
  group_by(pcc_factor_unit)%>%
  mutate(min.aicc= min(aicc)+2)%>%
  filter(aicc <min.aicc)




names(pcc_data_2level)
# Initialize a list to store results
importance_list <- list()

# IMPORTANCE
# Iterate over each unique unit
for (unit in unique_units) {
  # Create subset
  subset_data <- pcc_data_2level %>%
    filter(unique_units == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Perform analysis
  full <- rma(yi, vi, 
              mods = moderators,
              data=subset_data, 
              method="ML")
  
  # Store weightable results in the list
  res <- dredge(full, trace=2)
  importance_list[[unit]] <- sw(res)
  
}

# Convert the list to a data.frame
importance_df_2levels <- do.call(rbind, lapply(importance_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit.moderator")%>%
  mutate(moderator= str_extract(pcc_factor_unit.moderator, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.moderator, '^[^.]+'))%>%
  rename("Importance"="X[[i]]")%>%
  #filter(Importance>=0.8)%>%
  select(pcc_factor_unit,moderator,Importance)

subset(res, delta <= 2, recalc.weights=FALSE)


#WEIGHT


#### THREE-LEVEL META-ANALYSIS
library(ape)

names(pcc_data_3level)
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  dplyr::filter(pcc_factor_unit=="Access to extension (1= yes)"|
           pcc_factor_unit=="Access to information (1= yes)" |
           pcc_factor_unit=="Association member (1= yes)")

sort(unique(pcc_data_3level$pcc_factor_unit))
# Get unique values of pcc_factor_unit
unique_units <- unique(pcc_data_3level$pcc_factor_unit)

# Initialize a list to store results
importance_list <- list()

# IMPORTANCE
# Iterate over each unique unit
for (unit in unique_units) {
  # Create subset
  subset_data <- pcc_data_3level %>%
    filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Perform analysis
  full <- rma.mv(yi, vi, 
                 mods = moderators,
                 random = list(~ 1 | ES_ID, ~ 1 | article_id),
                 data=subset_data, 
                 method="ML")
  
  
  
  # Store weightable results in the list
  res <- dredge(full, trace=2)
  importance_list[[unit]] <- sw(res)
  
}

# Convert the list to a data.frame
importance_df_3levels <- do.call(rbind, lapply(importance_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit.moderator")%>%
  mutate(moderator= str_extract(pcc_factor_unit.moderator, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.moderator, '^[^.]+'))%>%
  rename("Importance"="X[[i]]")%>%
  #filter(Importance>=0.8)%>%
  select(pcc_factor_unit,moderator,Importance)

subset(res, delta <= 2, recalc.weights=FALSE)


#WEIGHT

rma.glmulti <- function(formula, data, ...) {
  rma.mv(formula, vi,
         random = list(~ 1 | ES_ID, ~ 1 | article_id),
         data=data, method="ML", ...)
}


# Get unique values of pcc_factor_unit
unique_units <- unique(pcc_data_3level$pcc_factor_unit)

# Initialize a list to store results
weigh_list <- list()

# Iterate over each unique unit
for (unit in unique_units) {
  # Create subset
  subset_data <- pcc_data_3level %>%
    dplyr::filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Perform analysis
  res <- glmulti(moderators, data=subset_data,
                 level=2, 
                 fitfunction=rma.glmulti, crit="aicc", confsetsize=32)
  
  # Store weightable results in the list
  weigh_list[[unit]] <- weightable(res)
  
}

# Convert the list to a data.frame
weigh_df_3levels <- do.call(rbind, lapply(weigh_list, as.data.frame))
  rownames_to_column(., var = "pcc_factor_unit.model")%>%
  mutate(n_model= str_extract(pcc_factor_unit.model, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.model, '^[^.]+'))%>%
  group_by(pcc_factor_unit)%>%
  mutate(min.aicc= min(aicc)+2)%>%
  filter(aicc <min.aicc)