library(dplyr)
library(readxl)


####### FACTORS -------
factors_metric_assessed <- read_excel("C:/Users/AndreaSanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/Meta_data_2024.01.25.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")


data<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/binary_adoption_clean_data.csv",
               header = TRUE, sep = ",")%>%
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
        "m_sampling_unit", "m_type_data", "m_av_year_assessment")%>%
  left_join(factors_metric_assessed, by= c("factor_metric"))

sort(unique(data$factor_metric))
names(data)    
data$ES_ID <- as.numeric(1:nrow(data)) #add a new column with the effect size ID number


names(data)
length(unique(data$article_id)) #153 articles for PCC analysis
sort(unique(data$article_id))  
sort(unique(data$pcc_factor_unit))  
length(unique(data$pcc_factor_unit)) #68

length(unique(data$m_intervention_recla2)) #10 systems
sort(unique(data$country)) #44
sort(unique(data$limitation_of_use_obs))

#######  Included FACTORS -------
length(unique(data$x_metric_recla)) #46
sort(unique(data$x_metric_recla))
length(unique(data$x_metric_recla2))#49
sort(unique(data$x_metric_recla2))#47

[1] "access to agricultural extension"                           "access to agricultural information"                        
[3] "access to agricultural training"                            "access to credit"                                          
[5] "access to credit is a constraint"                           "access to irrigation"                                      
[7] "access to off-farm income"                                  "agricultural extension frequency"                          
[9] "awareness of SFP or DFS"                                    "distance farm-house"                                       
[11] "distance to input market"                                   "distance to market"                                        
[13] "distance to output market"                                  "family members and friends living in and out the community"
[15] "farm labour force (hired)"                                  "farm labour force (non-hired)"                             
[17] "farm size"                                                  "h adult members"                                           
[19] "h income"                                                   "h off-farm income"                                         
[21] "h on-farm income"                                           "h size"                                                    
[23] "hh age"                                                     "hh association member"                                     
[25] "hh comunicate with other farmers"                           "hh education"                                              
[27] "hh farming experience"                                      "hh gender"                                                 
[29] "hh is native"                                               "hh marital status"                                         
[31] "hh perceive benefits of SFP or DFS"                         "hh risk attitude"                                          
[33] "land tenure security"                                       "limitations to implement SFT or DFS"                       
[35] "livestock owned"                                            "number of agricultural plots"                              
[37] "plot size"                                                  "precipitation"                                             
[39] "production constraints"                                     "receive incentive for conservation"                        
[41] "soil depth"                                                 "soil fertility"                                            
[43] "soil slope"                                                 "temperature"                                               
[45] "units of livestock"  

str(data)
sort(unique(data$factor_metric))
table(data$factor_metric)

factors_metric_unit<-data%>%
  group_by(factor_sub_class,   pcc_factor_unit ) %>%
  summarise(n_articles = n_distinct(article_id),
            n_ES = n_distinct(ES_ID))

sort(unique(factors_metric_unit$pcc_factor_unit)) # 135 factor_metric

write.csv(factors_metric_unit, "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/pcc_factors_metric_unit_articles.csv", row.names=FALSE)

####################### EFFECT SIZE calculation---------------------------------
library(metafor)
######### PCC effect size---------------------------------

### Compute Partial Correlation Coefficient effect size
#https://wviechtb.github.io/metadat/reference/dat.aloe2013.html
#Formulas: 
#rp <- tval/sqrt(tval^2+df)
#v <- (1-rp^2)/df # Variance Stanley & Doucouliagos (2012)
pcc_data<-escalc(measure="PCOR", ti= t_value_pcc, ni=n_samples_num, mi=n_predictors_num, data=data)


sort(unique(pcc_data$pcc_factor_unit))
names(pcc_data)

check_meta<- pcc_data%>%
  filter(is.na(yi))

sort(unique(pcc_data$pcc_unit))

write.csv(pcc_data, "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/pcc_data.csv", row.names=FALSE)

