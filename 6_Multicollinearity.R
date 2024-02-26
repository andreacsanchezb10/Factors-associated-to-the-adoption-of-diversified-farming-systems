#install.packages("glmulti")
library(data.table)
library(dplyr)
library(vcd)
library(stringr)
library(readxl)
library(ggh4x)


factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)
##########################################################################################################################################################
#################### DATA ##########################################################################################
##########################################################################################################################################################
## Two-level meta-analysis
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)%>%
  ungroup()


#Heterogeneity
heterogeneity_2level<- read.csv("results/heterogeneity_2levels.csv",header = TRUE, sep = ",")%>%
  filter(I2>=75)

sort(unique(heterogeneity_2level$pcc_factor_unit))

#pcc database after removing factors with <10 studies;
#factors with sampling variance > 25% 
m_pcc_data_2level<- pcc_data_2level%>%
  dplyr::left_join(heterogeneity_2level, by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  filter(!is.na(I2))%>%
  dplyr::select(pcc_factor_unit,
                m_region,
                m_sub_region,
                m_intervention_recla2,
                m_intervention_system_components,
                m_model_method,
                #binary moderators
                m_random_sample,
                m_exact_variance_value,
                m_sampling_unit,
                m_type_data,
                m_endogeneity_correction,
                m_exposure_correction,
                #continuous moderators
                m_mean_farm_size_ha,
                n_samples_num,
                n_predictors_num,
                m_av_year_assessment,
                m_education_years)

## Three-level meta-analysis
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_articles = n_distinct(article_id))%>%
  filter(n_articles>9)%>%
  ungroup()%>%
  dplyr::select(pcc_factor_unit,
                m_region,
                m_sub_region,
                m_intervention_recla2,
                m_intervention_system_components,
                m_model_method,
                #binary moderators
                m_random_sample,
                m_exact_variance_value,
                m_sampling_unit,
                m_type_data,
                m_endogeneity_correction,
                m_exposure_correction,
                #continuous moderators
                m_mean_farm_size_ha,
                n_samples_num,
                n_predictors_num,
                m_av_year_assessment,
                m_education_years)


pcc_data_meta_regression<-rbind(pcc_data_3level,m_pcc_data_2level)%>%
  mutate_at(vars(
    #categorical moderators
    pcc_factor_unit,m_region,
    m_sub_region,
    m_intervention_recla2,
    m_intervention_system_components,
    m_model_method,
    #binary moderators
    m_random_sample,
    m_exact_variance_value,
    m_sampling_unit,
    m_type_data,
    m_endogeneity_correction,
    m_exposure_correction), as.factor)%>%
  #continuous moderators
  mutate_at(vars(m_mean_farm_size_ha,
                 n_samples_num,
                 n_predictors_num,
                 m_av_year_assessment,
                 m_education_years),as.numeric)
 
pcc_data_meta_regression <- na.omit(pcc_data_meta_regression)

sort(unique(pcc_data_meta_regression$pcc_factor_unit))
  
str(pcc_data_meta_regression$m_mean_farm_size_ha)
sort(unique(pcc_data_meta_regression$pcc_factor_unit))
table(pcc_data_meta_regression$pcc_factor_unit,pcc_data_meta_regression$n_articles)
#categorical
sort(unique(pcc_data_meta_regression$m_region))
sort(unique(pcc_data_meta_regression$m_sub_region))
sort(unique(pcc_data_meta_regression$m_intervention_recla2))
sort(unique(pcc_data_meta_regression$m_model_method))
#Binary
sort(unique(pcc_data_meta_regression$m_random_sample))
sort(unique(pcc_data_meta_regression$m_exact_variance_value))
#Continuous
sort(unique(pcc_data_meta_regression$m_mean_farm_size_ha))
sort(unique(pcc_data_meta_regression$n_samples_num))
sort(unique(pcc_data_meta_regression$n_predictors_num))

sort(unique(pcc_data_meta_regression$m_type_data))



############ CHECK FOR MULTICOLLINEARITY BETWEEN EXPLANATORY VARIABLES --------
#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
########################################################################################################################################
#### Correlation between categorical variables
old_column<-c("V1",  "V2",  "V3",  "V4",  "V5",  "V6",  "V7",  "V8","V9","V10","V11")
new_column<- c(
  #categorical moderators
  "Region",
  "Sub-region",
  "Diversification practice",
  "Diversification practice components",
  "Model type",
  #binary moderators
  "Random sampling",
  "Exact variance value",
  "Household sampling unit",
  "Primary data",
  "Endogeneity analysis",
  "Exposure correction")

# Function to calculate Cramer's V correlations for a specific factor unit
calculate_cramer_for_unit <- function(data, factor_unit) {
  cramer_data <- data %>%
    dplyr::select(pcc_factor_unit,
                  m_region,
                  m_sub_region,
                  m_intervention_recla2,
                  m_intervention_system_components,
                  m_model_method,
                  #binary moderators
                  m_random_sample,
                  m_exact_variance_value,
                  m_sampling_unit,
                  m_type_data,
                  m_endogeneity_correction,
                  m_exposure_correction)

  unit_data <- cramer_data %>%
    filter(pcc_factor_unit == factor_unit) %>%
    dplyr::select(-pcc_factor_unit)
  unit_data<-as.data.frame(unit_data)
  
  cor_cramer <- function(x) {
    nc <- ncol(x)
    v <- expand.grid(1:nc, 1:nc)
    matrix(mapply(function(i1, i2) assocstats(table(x[, i1], x[, i2]))$cramer, v[, 1], v[, 2]), nc, nc)
  }
  
  result <- as.data.frame(cor_cramer(unit_data))
  return(result)
}

# List to store results for each factor unit
cramer_results_list <- list()

# List of unique factor units
unique_units <- unique(pcc_data_meta_regression$pcc_factor_unit)

# Iterate over each factor unit and calculate Cramer's V correlations
for (unit in unique_units) {
  unit_result <- calculate_cramer_for_unit(pcc_data_meta_regression, unit)
  cramer_results_list[[unit]] <- unit_result
}


# Combine the results into a single data frame
cramer_results_df <- do.call(rbind, cramer_results_list)%>%
  tibble::rownames_to_column(., var = "pcc_factor_unit.moderator")%>%
  mutate(moderator1= str_extract(pcc_factor_unit.moderator, '\\b\\w+$'))%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.moderator, '^[^.]+'))%>%
  select(pcc_factor_unit,moderator1,V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11)%>%
  setnames(., old_column, new_column)%>%
  tidyr::gather(key = "moderator2", value = "correlation", new_column, -pcc_factor_unit)%>%
  mutate(significance= if_else(correlation>=0.7,"*",""))%>%
  mutate(moderator1 = new_column[as.numeric(moderator1)])%>%
  left_join(pcc_factor_class_unit,by="pcc_factor_unit")
  
ggplot(cramer_results_df, aes(moderator1, moderator2, fill= correlation)) + 
  geom_tile(color = "black")+
  scale_fill_gradient(low="white", high="blue") +
  facet_wrap2(vars(pcc_factor_unit),nrow = 8, ncol = 4 )+
  theme(axis.text.x =element_text(color="black",size=10,
                                  family = "sans",angle=90,
                                  vjust = 0.5, hjust = 1),
        axis.text.y =element_text(color="black",size=10,
                                  family = "sans"),
        axis.title= element_blank())+
  labs(fill = "Cramer's V\ncorrelation")
  
########################################################################################################################################
##### Correlation between continuous and categorical variables
########################################################################################################################################
# Function to perform Kruskal-Wallis tests for a specific factor unit
kruskal_data<-pcc_data_meta_regression 
  select(pcc_factor_unit,
    m_region, m_sub_region, m_intervention_recla2, m_model_method,
    m_random_sample, m_exact_variance_value,
    #continuous
    m_mean_farm_size_ha, n_samples_num, n_predictors_num)


kruskal_function <- function(data, factor_unit) {
  kruskal_data <- data %>%
    dplyr::select(pcc_factor_unit,
                  m_region, m_sub_region, m_intervention_recla2, m_model_method,
                  m_intervention_system_components,
                  m_random_sample, m_exact_variance_value,m_sampling_unit,
                  m_type_data,m_endogeneity_correction,m_exposure_correction,
                  #continuous
                  m_mean_farm_size_ha,n_samples_num,n_predictors_num,
                  m_av_year_assessment,m_education_years)%>%
    filter(pcc_factor_unit == factor_unit) %>%
    dplyr::select(-pcc_factor_unit)
  
  # Extract categorical and continuous variable names
  categorical_vars <- names(kruskal_data)[sapply(kruskal_data, is.factor)]
  continuous_vars <- names(kruskal_data)[sapply(kruskal_data, is.numeric)]
  
  # Initialize a list to store results
  result_list <- list()
  
  # Perform Kruskal-Wallis tests for all combinations
  for (cat_var in categorical_vars) {
    for (cont_var in continuous_vars) {
      print(paste("Testing:", cat_var, "_vs_", cont_var))
      tryCatch({
        test_result <- kruskal.test(get(cont_var) ~ get(cat_var), data = kruskal_data)
        result_list[[paste(factor_unit, "/", cat_var, "_vs_", cont_var)]] <- test_result
      }, error = function(e) {
        print(paste("Error in:", cat_var, "_vs_", cont_var))
        print(e)
      })
    }
  }
  
  # Combine the results into a data frame
  result_df <- do.call(rbind, result_list)
  
  return(result_df)
}

# List to store results for each factor unit
kruskal_results_list <- list()

# List of unique factor units
unique_units <- unique(pcc_data_meta_regression$pcc_factor_unit)

# Iterate over each factor unit and perform Kruskal-Wallis tests
for (unit in unique_units) {
  unit_result <- kruskal_function(pcc_data_meta_regression, unit)
  kruskal_results_list[[unit]] <- unit_result
}

# Combine the results into a single data frame
kruskal_results_df <- as.data.frame(do.call(rbind, kruskal_results_list))%>%
  tibble::rownames_to_column(., var = "pcc_factor_unit.moderator")%>%
  mutate(pcc_factor_unit= str_extract(pcc_factor_unit.moderator, ".*(?= /)"))%>%
  mutate( moderator1= str_match(pcc_factor_unit.moderator, "/ (.*?) _vs")[, 2])%>%
  mutate(moderator2= str_extract(pcc_factor_unit.moderator, "(?<=vs_ ).*"))%>%
  mutate(chi_sq = str_extract(statistic, "(?<== ).*?(?=\\))"),
         chi_sq= round(as.numeric(chi_sq), 0),
         df = str_extract(parameter, "(?<== ).*?(?=\\))"),
         p.value= as.numeric(p.value),
         correlation= paste("H(", df,") = ",chi_sq,", p = ",p.value, sep=""),
         significance = if_else(p.value<=0.05,"*",""))%>%
  left_join(pcc_factor_class_unit,by="pcc_factor_unit")%>%
  mutate(moderator2= if_else(moderator2=="m_av_year_assessment","Years of assessment",
                             if_else(moderator2=="m_education_years","Years of formal education",
                                     if_else(moderator2=="m_mean_farm_size_ha","Land size (ha)",
                                             if_else(moderator2=="n_predictors_num","Years of formal education",
                                                     if_else(moderator2=="n_samples_num","Number of samples",moderator2))))))%>%
  mutate(moderator1= if_else(moderator1=="m_endogeneity_correction","Endogeneity analysis",
                             if_else(moderator1=="m_exact_variance_value","Exact variance value",
                                     if_else(moderator1=="m_exposure_correction","Exposure correction",
                                             if_else(moderator1=="m_intervention_recla2","Diversification practice",
                                                     if_else(moderator1=="m_intervention_system_components","Diversification practice components",
                                                             if_else(moderator1=="m_model_method","Model type",
                                                                     if_else(moderator1=="m_random_sample","Random sampling",
                                                                             if_else(moderator1=="m_region","Region",
                                                                                     if_else(moderator1=="m_sampling_unit","Household sampling unit",
                                                                                             if_else(moderator1=="m_sub_region","Sub-region",
                                                                                                     if_else(moderator1=="m_type_data","Primary data",
                                                                                                             moderator1))))))))))))
                                                                                                             
sort(unique(kruskal_results_df$moderator1))

ggplot(kruskal_results_df, aes(moderator1, moderator2, fill= p.value)) + 
  geom_tile(color = "black")+
  scale_fill_gradient(high="white", low ="blue") +
  facet_wrap2(vars(pcc_factor_unit),nrow = 8, ncol = 4 )+
  theme(axis.text.x =element_text(color="black",size=10,
                                  family = "sans",angle=90,
                                  vjust = 0.5, hjust = 1),
        axis.text.y =element_text(color="black",size=10,
                                  family = "sans"),
        axis.title= element_blank())+
  labs(fill = "Kruskal Wallis\np-value")


##### Correlation between continuous and categorical variables
# Function to perform Pearson correlation for a specific factor unit
perform_correlation_tests <- function(pcc_factor_unit, data) {
  subset_data <- data %>%
    filter(pcc_factor_unit == pcc_factor_unit) %>%
    select(pcc_factor_unit,
           m_mean_farm_size_ha,
           n_samples_num,
           n_predictors_num)
  
  # Perform correlation tests for all pairwise combinations
  combinations <- combn(c("m_mean_farm_size_ha", "n_samples_num", "n_predictors_num"), 2, simplify = TRUE)
  
  results_list <- lapply(1:ncol(combinations), function(i) {
    correlation_test <- cor.test(subset_data[, combinations[1, i]], subset_data[, combinations[2, i]], method = "pearson")
    
    return(data.frame(
      pcc_factor_unit = pcc_factor_unit,
      variable1 = combinations[1, i],
      variable2 = combinations[2, i],
      correlation_coefficient = correlation_test$estimate,
      p_value = correlation_test$p.value
    ))
  })
  
  # Combine the results into a data frame
  result_df <- do.call(rbind, results_list)
  
  return(result_df)
}

# List of pcc_factor_unit values
pcc_factor_units <- unique(pcc_data_2level$pcc_factor_unit)

# Initialize a list to store results
correlation_results <- list()

# Perform correlation tests for each pcc_factor_unit
for (unit in pcc_factor_units) {
  result <- perform_correlation_tests(unit, pcc_data_2level)
  correlation_results[[unit]] <- result
}

# Combine the results into a single data frame
pearson_result_df <- do.call(rbind, correlation_results)%>%
  rownames_to_column(., var = "pcc_factor_unit.moderator")%>%
  rename("moderator1"="variable1")%>%
  rename("moderator2"="variable2")%>%
  mutate_at(vars(correlation_coefficient,p_value),~ round(., 3))%>%
  mutate(correlation = paste("r = ", correlation_coefficient, " , pval = ", p_value, sep=""),
         significance= if_else(p_value <= 0.05,"*",""))%>%
  select(pcc_factor_unit,moderator1,moderator2, correlation,significance)

multi_collinearity<- rbind(cramer_results_df,
                           kruskal_results_df,
                           pearson_result_df)%>%
  select(pcc_factor_unit,moderator1,moderator2, correlation)%>%
  pivot_wider(names_from = moderator2, values_from = correlation)%>%
  group_by(pcc_factor_unit,moderator1)%>%
  summarize(across(c("UN_Regions", "UN_sub_region", "m_intervention_recla2", "model_method",
                     "m_random_sample", "m_exact_variance_value",
                     "m_mean_farm_size_ha","n_samples_num","n_predictors_num"),
                   ~ paste(na.omit(.), collapse = ", ")))

write.csv(multi_collinearity, "C:/Users/Andrea/Documents/Bioversity/Cost_benefits_analysis/Meta-analysis/Results_2022.06.21/kruskal_test.csv")

