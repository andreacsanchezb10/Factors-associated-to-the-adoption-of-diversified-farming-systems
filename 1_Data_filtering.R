#install.packages("Rtools")
library(readxl)
library(dplyr)

# Set the file path and name of the .xlsx file -------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

# Use the read.csv() function to read the clean evidence_map-data into a data frame
data <- read.csv(paste0(data_path,"evidence_map_data_clean.csv"),header = TRUE, sep = ",")

length(sort(unique(data$study_id))) # Number of articles 193
sort(unique(data$y_metric_recla)) #28
sort(unique(data$y_metric_recla_2)) #5 dependent variables
sort(unique(data$limitation_of_use_obs))
sort(unique(data$m_dp_recla))

table(data$y_metric_recla)
table(data$y_metric_recla_2)
#[1] "adoption"    "awareness"   "disadoption" "intensity of adoption" "interest"   
table(data$y_metric_recla, data$y_metric_recla_2)

### ---- Filter Adoption studies ----
#filter only studies analyzing adoption as a dependent variable
adoption<- data%>%
  dplyr::filter(y_metric_recla_2=="adoption")

length(sort(unique(adoption$study_id))) # 159 studies
table(adoption$y_metric_recla) #Number of rows 3969
sort(unique(adoption$country)) #Countries 45
length(sort(unique(adoption$x_metric_recla))) # Unique factors 210

### DEPENDENT VARIABLE: ADOPTION BINARY (1= yes, 0= no)----
# Select only necessary columns 
adoption_binary<- adoption%>%
  filter(y_metric_recla=="diversity adoption (1=yes, 0=no)")%>%
  #Convert to numeric the necessary columns
  mutate(coefficient_value= as.numeric(coefficient_value),
         variance_value = as.numeric(variance_value),
         variance_ci_l = as.numeric(variance_ci_l),
         variance_ci_u = as.numeric(variance_ci_u),
         z_t_value= as.numeric(z_t_value),
         p_value = as.numeric(p_value),
         n_factors= as.numeric(n_factors),
         n_samples= as.numeric(n_samples),
         x_mean_value_num =as.numeric(x_mean_value),
         transformation_coefficient_num = as.numeric(transformation_coefficient),
         transformation_variance_num = as.numeric(transformation_variance),
         country = as.character(country))%>%
  #Select only the columns that we are going to use
  mutate(factor_metric= paste(x_metric_recla, " (", x_metric_unit_recla, ")", sep=""))%>%
  dplyr::select(study_id,model_id,main_crop, 
                country,m_un_region,m_un_subregion,
                year_assessment_start, year_assessment_end,
                #Population
                dp_raw, dp_raw_details, dp_detail_1,dp_detail_2,dp_detail_3,dp_detail_4,
                dp_recla,m_dp_recla,
                #Dependent variable
                y_metric_raw,y_metric_recla,
                
                #Independent variable
                x_metric_raw,x_metric_recla,
                x_metric_unit_raw,
                transformation_coefficient,transformation_variance,
                x_metric_unit_recla, x_type,limitation_of_use_obs,factor_metric,
                
                # Outcome
                model_method_raw,model_method_recla,
                endogeneity_correction, exposure_correction,
                
                coefficient_type,coefficient_value,
                variance_type,variance_value,
                
                variance_ci_l,  variance_ci_u,
                z_t_value, p_value, 
                n_factors,n_samples, df_original,
                sampling_unit, type_data,
                #Moderators
                m_mean_farm_size_ha, m_random_sample,
                x_mean_value
                
                )%>%
  filter(study_id!="338")%>%
  filter(study_id!="1056")%>%
  filter(study_id!="856")

table(adoption_binary$y_metric_recla) #Number of rows 3919
length(sort(unique(adoption_binary$study_id))) # 154 studies
sort(unique(adoption_binary$study_id))
sort(unique(adoption_binary$limitation_of_use_obs))
sort(unique(adoption_binary$factor_metric))
length(sort(unique(adoption_binary$x_metric_recla))) # Unique factors 208
sort(unique(adoption_binary$m_dp_recla)) #10 diversified practices
table(adoption_binary$m_dp_recla)
sort(unique(adoption_binary$country)) #Countries 44


m_education_years<- adoption_binary%>%
  filter(factor_metric == "hh education (years)")%>%
  select(study_id, model_id, x_mean_value)%>%
  dplyr::rename("m_education_years"="x_mean_value")


adoption_binary_clean<- adoption_binary%>%
  left_join(m_education_years, by=c("study_id"="study_id",
                                    "model_id"="model_id"))%>%
  mutate(m_sampling_unit= if_else(sampling_unit== "farmers" |
                                    sampling_unit=="household"|
                                    sampling_unit=="household data collection"|
                                    sampling_unit=="households"|
                                    sampling_unit=="landholders"|
                                    sampling_unit=="managers", 1, 0))%>%
  mutate(m_type_data = if_else(type_data== "primary and secondary data" |
                                 type_data==  "primary data",1,0  ))%>%
  mutate_at(vars("year_assessment_start", "year_assessment_end", "m_education_years"), as.numeric)%>%
  mutate(m_av_year_assessment= if_else(is.na(year_assessment_end),
                                       year_assessment_start,
                                       ((year_assessment_start+year_assessment_end)/2)))%>%
  mutate(m_av_year_assessment= round(m_av_year_assessment,0))%>%
  select(!x_mean_value)

names(adoption_binary_clean)

write.csv(adoption_binary_clean,"data/data_adoption_binary.csv", row.names=FALSE)
