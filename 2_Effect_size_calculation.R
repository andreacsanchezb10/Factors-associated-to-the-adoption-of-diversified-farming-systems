library(dplyr)
library(readxl)


####### FACTORS -------
factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")
data<-read.csv("data/binary_adoption_clean_data.csv",header = TRUE, sep = ",")%>%
  select( "article_id","model_id", "main_crop" ,"country",                        
          "intervention_recla","intervention_recla_detail_1" ,   
          "intervention_recla_detail_2", "intervention_recla_detail_3",    "intervention_recla_detail_4",     "y_metric_recla" ,                
          "x_metric_raw"   ,                 "x_metric_recla"         ,         "x_metric_unit_raw"   ,            "x_metric_unit_recla" ,           
          "x_data_type"   ,                  "transformation_coefficient" ,            
          "transformation_variance_num",     "model_analysis_raw"  ,            "m_model_method"    ,                "coefficient_type"  ,             
         "coefficient"         ,            "coefficient_num"    ,             "variance_metric"   ,              "variance_value",                 
          "variance_value_num"   ,           "variance_ci_l"    ,               "variance_ci_l_num" ,              "variance_ci_u",                  
          "variance_ci_u_num" ,              "z_t_value"  ,                     "z_t_value_num"    ,               "p_value"  ,                      
          "p_value_num"  ,                   "df_original" ,                    "n_predictors"  ,                  "n_predictors_num" ,              
         "n_samples"   ,                    "n_samples_num"      ,             "limitation_of_use_obs",           "factor_metric"    ,              
          "coefficient_variance_type" ,      "model_coefficient_variance_type", "t_value_pcc"  ,                  
          "b_logOR"        ,                 "se_logOR"    ,"m_region"    ,"m_sub_region",                 
          "Developed_Developing" ,         
         "m_intervention_recla2" ,"m_exact_variance_value","m_random_sample", "m_mean_farm_size_ha",
        "m_sampling_unit", "m_type_data", "m_av_year_assessment", "m_education_years", "m_intervention_system_components",
        "m_endogeneity_correction","m_exposure_correction")%>%
  left_join(factors_metric_assessed, by= c("factor_metric"))

sort(unique(data$factor_metric))
names(data)    
data$ES_ID <- as.numeric(1:nrow(data)) #add a new column with the effect size ID number


names(data)
length(unique(data$article_id)) #153 articles for PCC analysis
sort(unique(data$article_id))  
sort(unique(data$pcc_factor_unit))  
length(unique(data$pcc_factor_unit)) #70

length(unique(data$m_intervention_recla2)) #10 systems
sort(unique(data$country)) #44
sort(unique(data$limitation_of_use_obs))

#######  Included FACTORS -------
length(unique(data$x_metric_recla)) #48
sort(unique(data$x_metric_recla))
length(unique(data$x_metric_recla2))#49

str(data)
sort(unique(data$factor_metric))
table(data$factor_metric)

factors_metric_unit<-data%>%
  group_by(factor_sub_class,   pcc_factor_unit ) %>%
  summarise(n_articles = n_distinct(article_id),
            n_ES = n_distinct(ES_ID))

sort(unique(factors_metric_unit$pcc_factor_unit)) # 135 factor_metric

write.csv(factors_metric_unit, "data/pcc_factors_metric_unit_articles.csv", row.names=FALSE)

####################### EFFECT SIZE calculation---------------------------------
library(metafor)
######### PCC effect size---------------------------------

### Compute Partial Correlation Coefficient effect size
#https://wviechtb.github.io/metadat/reference/dat.aloe2013.html
#Formulas: 
#rp <- tval/sqrt(tval^2+df)
#v <- (1-rp^2)/df # Variance Stanley & Doucouliagos (2012)
pcc_data<-escalc(measure="PCOR", ti= t_value_pcc, ni=n_samples_num, mi=n_predictors_num, data=data,
                 var.names=c("pcc.yi","pcc.vi"))


sort(unique(pcc_data$pcc_factor_unit))
names(pcc_data)

check_meta<- pcc_data%>%
  filter(is.na(yi))

sort(unique(pcc_data$pcc_unit))

#write.csv(pcc_data, "data/pcc_data.csv", row.names=FALSE)

######### Fisher Z transformation ---------------------------------
#https://osf.io/ubqfg
### Compute Fisher's z transformed partial correlations
#fis <- 0.5*log((1+rp)/(1-rp))
#v_fis <- rep(1/(n-3-1), k) # Variance Fisher's z transformed partial correlations

fis_data<- pcc_data%>%
  mutate(fis.yi= 0.5*log((1+pcc.yi)/(1-pcc.yi)))%>%
  mutate(fis.vi= (1/(n_samples_num-3-(n_predictors_num-1))))

names(fis_data)
write.csv(fis_data, "data/pcc_data.csv", row.names=FALSE)

escalc(measure="PCOR", ti= 0.7071, ni=50, mi=n_predictors_num, data=data,
       var.names=c("pcc.yi","pcc.vi"))

0.5*log((1+0.7071)/(1-0.7071))
  
0.5*log((1+0.7071)/(1-0.7071))


### Select condition
(1-(0.3162^2))/(25-2-1)


rep(1/(25-3-1), 50)
(1/(25-3-1))
