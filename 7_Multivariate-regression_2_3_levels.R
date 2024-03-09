
#__________MULTIVARIATE REGRESSION MODEL
#https://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin
#library(MuMIn)
library(tibble)
library(stringr)
library(metafor)
library(purrr)
library(glmulti)
library(dplyr)


 c("m_intervention_recla2","m_intervention_system_components", "m_region","m_education_years", "m_mean_farm_size_ha")
                          
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
  filter(
    pcc_factor_unit== "Livestock owned (1= yes)"|
    pcc_factor_unit== "Non-farm income (continuous)")
    pcc_factor_unit=="Plot size (continuous)"|
    pcc_factor_unit=="Extension frequency (number of contacts)"|
    pcc_factor_unit=="Soil fertility (1= high)"|
    pcc_factor_unit ==  "Soil fertility (1= moderate)"|
    pcc_factor_unit =="Relatives and friends (number)"|
    pcc_factor_unit =="Marital status (1= married)"|
    pcc_factor_unit =="Education (continuous)"|
    pcc_factor_unit =="Access to credit (1= yes)"|
      pcc_factor_unit =="Access to irrigation (1= yes)")
      pcc_factor_unit =="Access to training (1= yes)")
      pcc_factor_unit =="Farming experience (continuous)")
        pcc_factor_unit ==  "Household is native (1= yes)")
      pcc_factor_unit =="Use of DFS (1= yes)")
        
      pcc_factor_unit =="Access to non-farm income (1= yes)"|
      pcc_factor_unit =="Age (continuous)"|
      pcc_factor_unit =="Communicate with other farmers (1= yes)"|
      pcc_factor_unit =="Gender (1= male)"|
      pcc_factor_unit =="Land tenure (1= owned)"|
      pcc_factor_unit == "Receive support for conservation (1= yes)" )

  filter(pcc_factor_unit=="Access to non-farm income (1= yes)" )
  select(m_mean_farm_size_ha)
sort(unique(m_pcc_data_2level$pcc_factor_unit))

rma.glmulti <- function(formula, data, ...)
  rma(formula, vi, data=data, method="ML", ...)


res <- glmulti( yi ~m_intervention_recla2+m_intervention_system_components+m_region+m_mean_farm_size_ha+m_education_years, 
                data=m_pcc_data_2level, 
               level=2, marginality=TRUE, fitfunction=rma.glmulti,
               crit="aicc", method="g", plotty=FALSE)

# Most moderators
unique_units <- unique(m_pcc_data_2level$pcc_factor_unit)
importance_list2 <- list()
#esta funcion no funciona para non-farm income 

for (unit in unique(m_pcc_data_2level$pcc_factor_unit)) {
  
  # Subset the data for the current pcc_factor_unit
  subset_data <- m_pcc_data_2level %>%
    filter(pcc_factor_unit == unit)
  subset_data <- as.data.frame(subset_data)
  
  # Check if there are enough data points for modeling
  if (nrow(subset_data) > length(coef(subset_data)) + 2) {
    # Include specific variables based on pcc_factor_unit
    if (unit == "Livestock owned (1= yes)") {
      variables_to_include <- c( "m_intervention_recla2","m_intervention_system_components","m_sub_region", "m_mean_farm_size_ha")} 
    else if ( unit =="Non-farm income (continuous)")  {
      variables_to_include <- c("m_intervention_recla2","m_intervention_system_components","m_mean_farm_size_ha")} 
    #else if ( unit =="Plot size (continuous)") {
     #variables_to_include <- c("m_intervention_recla2","m_intervention_system_components","m_mean_farm_size_ha","m_education_years") }
     #else if ( unit =="Extension frequency (number of contacts)"||
      #      unit == "Soil fertility (1= high)"||
     #        unit ==  "Soil fertility (1= moderate)") {
    #  variables_to_include <- c("m_intervention_recla2","m_intervention_system_components","m_education_years", "m_mean_farm_size_ha")
    #}
    #else if ( unit =="Relatives and friends (number)" ||
      #       unit =="Marital status (1= married)" ) {
     #variables_to_include <- c("m_intervention_recla2","m_intervention_system_components","m_region","m_sub_region","m_education_years", "m_mean_farm_size_ha") }
     #else if (unit == "Education (continuous)" ||
      #       unit ==  "Access to credit (1= yes)") {
     # variables_to_include <- c("m_intervention_recla2","m_intervention_system_components","m_region","m_education_years", "m_mean_farm_size_ha") }
    #else if (unit =="Access to irrigation (1= yes)")
             #unit ==  "Access to training (1= yes)")
             #unit =="Farming experience (continuous)")
             #unit =="Household is native (1= yes)"||
             #unit== "Use of DFS (1= yes)") {
    #{variables_to_include <- c("m_intervention_recla2","m_intervention_system_components","m_region", "m_mean_farm_size_ha")}
    
    # else if (unit =="Access to non-farm income (1= yes)"|
    #        unit =="Age (continuous)"||
    #        unit =="Communicate with other farmers (1= yes)"||
    #        unit == "Gender (1= male)"||
    #        unit == "Land tenure (1= owned)"||
    #        unit ==  "Receive support for conservation (1= yes)" 
             #        ) {
    # variables_to_include <- c("m_intervention_recla2","m_intervention_system_components","m_region", "m_mean_farm_size_ha")
    #}
    
    # Check if there are enough variables for modeling
    if (length(variables_to_include) > 0) {
      
      formula_string <- paste("yi ~", paste(variables_to_include, collapse = "+"))
      
      # Perform glmulti analysis
      res <- glmulti(formula_string, data=subset_data, 
                      level=2, marginality=TRUE, fitfunction=rma.glmulti,
                      crit="aicc", confsetsize=1000, plotty=FALSE)
      
      # Store weightable results in the list
      importance_list2[[unit]] <- table.glmulti(res, type = "s")
      
    } else {
      cat("Not enough data points for modeling in unit:", unit, "\n")
    }
    
  } else {
    cat("Not enough data points for modeling in unit:", unit, "\n")
  }
}

importance_df_2levels <- do.call(rbind, lapply(importance_list2, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(pcc_factor_unit= sub("\\..*", "", pcc_factor_unit))
  rbind(extension_frequency_i, livestock_owned_i,relatives_i,soil_fertility_h_i)%>%
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
  ungroup()

sort(unique(pcc_data_3level$pcc_factor_unit))

rma.glmulti1 <- function(formula, data, ...) {
  rma.mv(formula, vi,
         random = list(~ 1 | ES_ID, ~ 1 | article_id),
         data=data, method="ML", ...)
}

unique_units <- unique(pcc_data_3level$pcc_factor_unit)
importance_list3 <- list()

for (unit in unique(pcc_data_3level$pcc_factor_unit)) {
  
  # Subset the data for the current pcc_factor_unit
  subset_data <- pcc_data_3level %>%
    filter(pcc_factor_unit == unit)
  
  subset_data <- as.data.frame(subset_data)
  
  # Check if there are enough data points for modeling
  if (nrow(subset_data) > length(coef(subset_data)) + 2) {
    
    # List of variables to include in the model
    #variables_to_include <- c("m_intervention_recla2", "m_intervention_system_components", "m_region")
    
    # Include specific variables based on pcc_factor_unit
    if (unit == "Awareness of practice (1= yes)") {
      variables_to_include <- c( "m_intervention_recla2", "m_region", "m_mean_farm_size_ha")
    } 
    else if ( unit == "Distance farm-house (continuous)") {
      variables_to_include <- c("m_intervention_recla2", "m_intervention_system_components", "m_region", "m_sub_region", "m_mean_farm_size_ha", "m_education_years")
      
    } else if (unit == "Access to information (1= yes)") {
      variables_to_include <- c("m_intervention_recla2", "m_intervention_system_components", "m_region", "m_mean_farm_size_ha")
      
    } else if (unit == "Association member (1= yes)") {
      variables_to_include <- c("m_intervention_recla2", "m_intervention_system_components", "m_region", "m_mean_farm_size_ha", "m_education_years")
      
    } else if (unit == "Access to extension (1= yes)"||
               unit == "Adults in household (continuous)" || 
               unit == "Distance market (continuous)" || 
               unit == "Farm size (continuous)" || 
               unit == "Household size (continuous)" || 
               unit == "Livestock units (continuous)" || 
               unit == "On-farm income (continuous)") {
      variables_to_include <- c("m_intervention_recla2", "m_intervention_system_components", "m_region", "m_sub_region", "m_mean_farm_size_ha")
    }
    
    # Check if there are enough variables for modeling
    if (length(variables_to_include) > 0) {
      
      formula_string <- paste("yi ~", paste(variables_to_include, collapse = "+"))
      
      # Perform glmulti analysis
      res1 <- glmulti(formula_string, data=subset_data, 
                      level=2, marginality=TRUE, fitfunction=rma.glmulti1,
                      crit="aicc", confsetsize=1000, plotty=FALSE)
      
      # Store weightable results in the list
      importance_list3[[unit]] <- table.glmulti(res1, type = "s")
      
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
  mutate(pcc_factor_unit= sub("\\..*", "", pcc_factor_unit))
  rbind(awareness_i)%>%
  group_by(pcc_factor_unit) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 5) %>%
  ungroup()

sort(unique(importance_factors_3levels$pcc_factor_unit))
##################################################################
## PLOT IMPORTANT MODERATORS
############################################################################
sort(unique(importance_factors_3levels$pcc_factor_unit))

importance_factors<-
  #importance_df_2levels%>%
  rbind(importance_df_2levels,importance_factors_3levels)%>%
  mutate(colours= if_else(Importance>=0.5,"important","not_important"))


importance_factors$Term[importance_factors$Term %in% c("m_education_years")] <- "Education (years)"
importance_factors$Term[importance_factors$Term %in% c("m_intervention_recla2")] <- "Diversification practice"
importance_factors$Term[importance_factors$Term %in% c("m_intervention_system_components")] <- "Diversification practice\ncomponents"
importance_factors$Term[importance_factors$Term %in% c("m_region")] <- "Region"
importance_factors$Term[importance_factors$Term %in% c("m_sub_region")] <- "Sub-region"

importance_factors$Term[importance_factors$Term %in% c("m_mean_farm_size_ha")] <- "Farm size (ha)"
importance_factors$Term[importance_factors$Term %in% c("m_education_years:m_intervention_recla2")] <- "Years of educationn (years):\nDiversification practice"

importance_factors$Term[importance_factors$Term %in% c("m_education_years:m_intervention_system_components")] <- "Education (years):\nDiversification practice\ncomponents"
importance_factors$Term[importance_factors$Term %in% c("m_intervention_recla2:m_intervention_system_components")] <- "Diversification practice:\nDiversification practice\ncomponents"
importance_factors$Term[importance_factors$Term %in% c("m_intervention_system_components:m_region")] <- "Diversification systems\ncomponents:Regions"
importance_factors$Term[importance_factors$Term %in% c("m_intervention_recla2:m_region")] <- "Diversification practice:\nRegion"
importance_factors$Term[importance_factors$Term %in% c("m_mean_farm_size_ha:m_region")] <- "Farm size (ha):Region"
importance_factors$Term[importance_factors$Term %in% c("m_intervention_system_components:m_mean_farm_size_ha")] <- "Diversification practice\ncomponents:Farm size (ha)"

unique_category1 <- unique(importance_factors$pcc_factor_unit)
unique_category2 <- unique(importance_factors$Term)

# Create all possible combinations
all_combinations <- expand.grid(
  pcc_factor_unit = unique_category1,
  Term = unique_category2
)

# Merge with the original data frame to get missing combinations
result <- merge(all_combinations, importance_factors, all.x = TRUE)


sort(unique(importance_factors$Term))

  
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

importance_factors<-result%>%
  dplyr::mutate(colours= if_else(Importance>=0.5,"Important",
                                 "Not important"))%>%
  dplyr::mutate(colours= if_else(is.na(Importance),"NA",colours))%>%
  left_join(pcc_factor_class_unit,by="pcc_factor_unit")%>%
  mutate(factor_sub_class = toupper(factor_sub_class))%>%
  mutate(factor_sub_class=if_else(factor_sub_class=="ACCESSIBILITY","ACCES\nSIBILITY",
                                  if_else(factor_sub_class=="BIOPHYSICAL","BIO\nPHYSICAL",
                                          factor_sub_class)))



sort(unique(importance_factors$Importance))


fills <- c("#f0c602", "#ea6044","#d896ff",  "#87CEEB", "#496491", "#92c46d", "#297d7d")


overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = fills),
  text_y = elem_list_text(size= 12,colour= "white",angle = 90,face="bold"),
  by_layer_y = FALSE
)


ggplot(data = importance_factors, aes(Term, pcc_factor_unit, fill = as.factor(colours), group="factor_sub_class"))+
  geom_tile(color = "black")+
  geom_text(aes(Term, pcc_factor_unit, label= round(Importance, 3)), color = "black", size = 6) +
  scale_x_discrete(position = "top", expand=c(0,0)) +
  scale_y_discrete( expand=c(0,0)) +
  scale_fill_manual(values = c("Important" = "#ff9248","Not important"= "white","NA"="grey75"),
                    name="Moderators\nimportance")+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",strip = overall_strips)+
  theme(strip.placement.y = "outside",
        legend.position = "bottom",
        axis.title = element_blank(),
        plot.margin = unit(c(t=0.5,r=1,b=0.5,l=0.5), "cm"),
        legend.title =  element_text(color="black",size=14, family = "sans",face="bold"),
        legend.text = element_text(color="black",size=12, family = "sans"),
        axis.text.y =element_text(color="black",size=12, family = "sans"),
        axis.text.x =element_text(color="black",size=14, family = "sans",angle=45,
                                  vjust = 0.5, hjust=0))



############################################################################################################################################################################################
###### MULTIVARIATE META-REGRESSION ANALYSIS
############################################################################################################################################################################################
sort(unique(pcc_data_3level$pcc_factor_unit))
"Association member (1= yes)"

association_member<- rma.mv(yi, vi, 
           random = list(~ 1 | ES_ID, ~ 1 | article_id),
           mods = ~m_education_years+m_intervention_system_components,
           data = pcc_data_3level,
           subset=pcc_factor_unit=="Association member (1= yes)",
           method = "REML", 
           test = "t", dfs = "contain")
summary(association_member)

sort(unique(m_pcc_data_2level$pcc_factor_unit))
"Access to credit (1= yes)"
"Relatives and friends (number)"
"Extension frequency (number of contacts)" 

credit <- rma.uni(yi, vi,
                     mods =~  m_education_years*m_intervention_recla2+m_intervention_system_components,
                     data = m_pcc_data_2level,
                     subset=pcc_factor_unit=="Access to credit (1= yes)",
                     method = "REML", 
                     test = "knha")

summary(credit)

relatives_friends <- rma.uni(yi, vi,
                  mods =~  m_intervention_recla2+m_mean_farm_size_ha,
                  data = m_pcc_data_2level,
                  subset=pcc_factor_unit=="Relatives and friends (number)",
                  method = "REML",  test = "knha")

summary(relatives_friends)

extensionfrequency <- rma.uni(yi, vi,
                             mods =~   m_education_years*m_intervention_recla2-1,
                             data = m_pcc_data_2level,
                             subset=pcc_factor_unit=="Extension frequency (number of contacts)",
                             method = "REML",  test = "knha")

summary(extensionfrequency)

extensionfrequency <- rma.uni(yi, vi,
                              mods =~   m_education_years*m_intervention_system_components-1,
                              data = m_pcc_data_2level,
                              subset=pcc_factor_unit=="Extension frequency (number of contacts)",
                              method = "REML",  test = "knha")

summary(extensionfrequency)
