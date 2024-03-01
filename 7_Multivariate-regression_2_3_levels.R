
#__________MULTIVARIATE REGRESSION MODEL
#https://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin
#library(MuMIn)
library(tibble)
library(stringr)
library(metafor)
library(purrr)
library(glmulti)
library(dplyr)


variables_to_exclude <- c("m_intervention_recla2","m_intervention_system_components", "m_region","m_education_years", "m_mean_farm_size_ha")
                          
("m_sub_region","m_av_year_assessment","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")


eval(metafor:::.MuMIn)

#Function to calculate the importance of moderators
table.glmulti <- function(x, type = "p", ...) {
  if (type == "s") {
    ww <- exp(-(x@crits - x@crits[1])/2)
    ww <- ww / sum(ww)
    
    # Handle synonymies for interactions
    clartou <- function(x) {
      sort(strsplit(x, ":")[[1]]) -> pieces
      if (length(pieces) > 1) paste(pieces[1], ":", pieces[2], sep = "")
      else x
    }
    
    # List terms in models
    tet <- lapply(x@formulas, function(x) sapply(attr(delete.response(terms(x)), "term.labels"), clartou))
    
    # All unique terms
    unique(unlist(tet)) -> allt
    
    # Importances
    imp <- sapply(allt, function(x) sum(ww[sapply(tet, function(t) x %in% t)]))
    
    # Create a data frame
    importance_df <- data.frame(Term = allt, Importance = imp)
    importance_df <- importance_df[order(imp, decreasing = TRUE), ]
    
    return(importance_df)
  } else {
    warning("plot: Invalid type argument for plotting glmulti objects.")
  }
}

##########################################################################################################################################################
#################### TWO-LEVEL META-ANALYSIS ##########################################################################################
##########################################################################################################################################################
#Data
#Data
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(m_mean_farm_size_ha= as.numeric(m_mean_farm_size_ha))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)%>%
  ungroup()
sort(unique(pcc_data_2level$pcc_factor_unit))    

#Heterogeneity
heterogeneity_2level<- read.csv("results/heterogeneity_2levels.csv",header = TRUE, sep = ",")%>%
  filter(I2>=75) 

sort(unique(heterogeneity_2level$pcc_factor_unit))

#pcc database after removing factors with <10 studies;
#factors with sampling variance > 25% 
m_pcc_data_2level<- pcc_data_2level%>%
  dplyr::left_join(heterogeneity_2level, by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  filter(!is.na(I2))%>%
  filter(pcc_factor_unit!= "Relatives and friends (number)")%>%
  
  filter(pcc_factor_unit!= "Livestock owned (1= yes)")%>%
  filter(pcc_factor_unit!= "Soil fertility (1= high)")%>%
  filter(pcc_factor_unit!= "Extension frequency (number of contacts)")

filter(pcc_factor_unit== "Marital status (1= married)")
filter(pcc_factor_unit== "Non-farm income (continuous)")

sort(unique(m_pcc_data_2level$pcc_factor_unit))
filter(pcc_factor_unit== "Receive support for conservation (1= yes)")

filter(pcc_factor_unit== "Farming experience (continuous)")
filter(pcc_factor_unit== "Land tenure (1= owned)")
filter(pcc_factor_unit== "Access to irrigation (1= yes)")
filter(pcc_factor_unit== "Age (continuous)")
filter(pcc_factor_unit== "Communicate with other farmers (1= yes)")
filter(pcc_factor_unit== "Plot size (continuous)")
filter(pcc_factor_unit== "Soil fertility (1= moderate)")
filter(pcc_factor_unit== "Household is native (1= yes)")
"Access to non-farm income (1= yes)"
"Age (continuous)"
"Education (continuous)"  
"Gender (1= male)" 
filter(pcc_factor_unit== "Access to credit (1= yes)")
filter(pcc_factor_unit== "Access to training (1= yes)")
filter(pcc_factor_unit== "Use of DFS (1= yes)")



rma.glmulti <- function(formula, data, ...)
  rma(formula, vi, data=data, method="ML", ...)

unique_units <- unique(m_pcc_data_2level$pcc_factor_unit)
importance_list <- list()

for (unit in unique(m_pcc_data_2level$pcc_factor_unit)) {
  
  # Subset the data for the current pcc_factor_unit
  subset_data <- m_pcc_data_2level %>%
    filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Check if there are enough data points for modeling
  if (nrow(subset_data) > length(coef(subset_data)) + 2) {
    
    # List of variables to include in the model
    variables_to_include <- c("m_intervention_recla2", "m_intervention_system_components")
    
    # Include "m_mean_farm_size_ha" only if it has data for >= 60% of rows
    if (sum(!is.na(subset_data$m_mean_farm_size_ha)) / nrow(subset_data) >= 0.5) {
      variables_to_include <- c(variables_to_include, "m_mean_farm_size_ha")
    }
    
    # Include "m_education_years" only if it has data for >= 60% of rows
    if (sum(!is.na(subset_data$m_education_years)) / nrow(subset_data) >= 0.5) {
      variables_to_include <- c(variables_to_include, "m_education_years")
    }
    
    # Check if "m_region" has more than 1 category level
    if (length(unique(subset_data$m_region)) > 1) {
      variables_to_include <- c(variables_to_include, "m_region")
    } else {
      cat("Only one category level for m_region in unit:", unit, ". Removing from the analysis.\n")
    }
    
    # Check if there are enough variables for modeling
    if (length(variables_to_include) > 1) {
      
      # Check if the number of parameters is smaller than the number of observations
      if (length(variables_to_include) <= nrow(subset_data)) {
        
        formula_string <- paste("yi ~", paste(variables_to_include, collapse = "+"))
        
        # Perform glmulti analysis
        res <- glmulti(formula_string, data=subset_data,
                       level=2, marginality=TRUE, method="g", fitfunction=rma.glmulti, crit="aicc")
        
        # Store weightable results in the list
        importance_list[[unit]] <- table.glmulti(res, type = "s")
        
      } else {
        cat("Number of parameters exceeds the number of observations in unit:", unit, "\n")
      }
      
    } else {
      cat("Not enough data points or variables for modeling in unit:", unit, "\n")
    }
    
  } else {
    cat("Not enough data points for modeling in unit:", unit, "\n")
  }
}

importance_df_2levels <- do.call(rbind, lapply(importance_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(pcc_factor_unit= sub("\\..*", "", pcc_factor_unit))%>%
  group_by(pcc_factor_unit) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 5) %>%
  ungroup()
sort(unique(importance_df_2levels$pcc_factor_unit))


##########################################################################################################################################################
########################  THREE-LEVEL META-ANALYSIS ##########################################################################################################################################################
##########################################################################################################################################################
#Data
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate_at(vars(m_mean_farm_size_ha,n_samples_num,n_predictors_num,m_education_years ), as.numeric)%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)%>%
  ungroup()%>%
  filter(pcc_factor_unit!="Awareness of practice (1= yes)")

sort(unique(pcc_data_3level$pcc_factor_unit))

rma.glmulti1 <- function(formula, data, ...) {
  rma.mv(formula, vi,
         random = list(~ 1 | ES_ID, ~ 1 | article_id),
         data=data, method="ML", ...)
}

unique_units <- unique(pcc_data_3level$pcc_factor_unit)
importance_list <- list()

for (unit in unique(pcc_data_3level$pcc_factor_unit)) {
  
  # Subset the data for the current pcc_factor_unit
  subset_data <- pcc_data_3level %>%
    filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Check if there are enough data points for modeling
  if (nrow(subset_data) > length(coef(subset_data)) + 2) {
    
    # List of variables to include in the model
    variables_to_include <- c("m_intervention_recla2", "m_intervention_system_components", "m_region")
    
    # Include "m_mean_farm_size_ha" only if it has data for >= 50% of rows
    if (sum(!is.na(subset_data$m_mean_farm_size_ha)) / nrow(subset_data) >= 0.5) {
      variables_to_include <- c(variables_to_include, "m_mean_farm_size_ha")
    }
    
    # Include "m_education_years" only if it has data for >= 50% of rows
    if (sum(!is.na(subset_data$m_education_years)) / nrow(subset_data) >= 0.5) {
      variables_to_include <- c(variables_to_include, "m_education_years")
    }
    
    # Check if there are enough variables for modeling
    if (length(variables_to_include) > 0) {
      
      formula_string <- paste("yi ~", paste(variables_to_include, collapse = "+"))
      
      # Perform glmulti analysis
      res1 <- glmulti(formula_string, data=subset_data, level=2, marginality=TRUE, fitfunction=rma.glmulti1,
                      crit="aicc", plotty=FALSE, method="g")

      
      # Store weightable results in the list
      importance_list[[unit]] <- table.glmulti(res1, type = "s")
      
    } else {
      cat("Not enough data points for modeling in unit:", unit, "\n")
    }
    
  } else {
    cat("Not enough data points for modeling in unit:", unit, "\n")
  }
}

# Convert the list to a data.frame
importance_factors_3levels <- do.call(rbind, lapply(importance_list, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(pcc_factor_unit= sub("\\..*", "", pcc_factor_unit))%>%
  group_by(pcc_factor_unit) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 5) %>%
  ungroup()



##################################################################
## PLOT IMPORTANT MODERATORS
############################################################################
sort(unique(importance_factors_3levels$pcc_factor_unit))

importance_factors<-rbind(importance_factors_3levels,importance_factors_3levels)%>%
  mutate(colours= if_else(Importance>=0.5,"important","not_important"))


importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_education_years")] <- "Education (years)"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_intervention_recla2")] <- "Diversification practice"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_intervention_system_components")] <- "Diversification practice\ncomponents"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_region")] <- "Region"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_mean_farm_size_ha")] <- "Farm size (ha)"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_education_years:m_intervention_system_components")] <- "Education (years):\nDiversification systems\ncomponents"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_intervention_recla2:m_intervention_system_components")] <- "Diversification practice:\nDiversification practice\ncomponents"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_intervention_system_components:m_region")] <- "Diversification systems\ncomponents:Regions"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_intervention_recla2:m_region")] <- "Diversification practice:\nRegion"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_mean_farm_size_ha:m_region")] <- "Farm size (ha):Region"
importance_factors_3levels$Term[importance_factors_3levels$Term %in% c("m_intervention_system_components:m_mean_farm_size_ha")] <- "Diversification systems\ncomponents:Farm size (ha)"



  
library(reshape2)
library(ggh4x)
library(readxl)

factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")
pcc_factor_class_unit<-factors_metric_assessed%>%
  dplyr::select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

importance_factors<-importance_factors_3levels%>%
  mutate(colours= if_else(Importance>=0.5,"important","not_important"))%>%
  left_join(pcc_factor_class_unit,by="pcc_factor_unit")


fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#297d7d")


overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = fills),
  text_y = elem_list_text(size= 12,colour= "black",angle = 90),
  by_layer_y = FALSE
)


ggplot(data = importance_factors, aes(Term, pcc_factor_unit, fill = as.factor(colours), group="factor_sub_class"))+
  geom_tile(color = "black")+
  geom_text(aes(Term, pcc_factor_unit, label= round(Importance, 3)), color = "black", size = 4) +
  scale_x_discrete(position = "top", expand=c(0,0)) +
  scale_y_discrete( expand=c(0,0)) +
  
  xlab("Moderators importance")+
  scale_fill_manual(values = c("important" = "#ff9248","not_important"= "white"))+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",strip = overall_strips)+
  theme(strip.placement.y = "outside",
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y =element_text(color="black",size=12, family = "sans"),
        axis.text.x =element_text(color="black",size=10, family = "sans"))



sort(unique(importance_factors_3levels$Term))

importance_models_3levels<- do.call(rbind, lapply(importance_list, as.data.frame))%>%
  mutate(important_moderators= if_else(Importance>=0.5,Term, NA))%>%
  group_by(pcc_factor_unit)%>%
  summarise(important_moderators1 = paste(important_moderators, collapse = "+ "))


  




