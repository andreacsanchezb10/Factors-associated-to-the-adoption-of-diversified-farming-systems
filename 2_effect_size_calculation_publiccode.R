library(dplyr)
library(readxl)


####### FACTORS -------
factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$logor_unit,")", sep="")
names(data)
data<-read.csv("data/binary_adoption_clean_data.csv",header = TRUE, sep = ",")%>%
  select( "study_id","model_id", "main_crop" ,
          "country","m_un_region"    ,"m_un_subregion",
          "dp_raw", "dp_raw_details",
          "dp_recla","dp_detail_1" ,
          "y_metric_raw","y_metric_recla" ,
          "x_metric_raw"   ,"x_metric_recla",
          "x_metric_unit_raw","transformation_coefficient" ,"transformation_variance_num", "x_metric_unit_recla" , 
          "x_type",
          "limitation_of_use_obs",
          "model_method_raw"  ,            "model_method_recla" ,    
          "endogeneity_correction","exposure_correction",
          "coefficient_type","coefficient_value",
          "variance_metric"   ,"variance_value",                 
          "variance_ci_l"    ,"variance_ci_u", 
          "z_t_value"  ,"p_value"  ,                      
          "n_factors"  ,                               
          "n_samples"   ,             
          
          "factor_metric"    ,              
          "coefficient_variance_type" ,      "model_coefficient_variance_type", "t_value_pcc"  ,                  
          "b_logOR"        ,                 "se_logOR"    ,                 
                                      
          "m_dp_recla" ,"m_exact_variance_value","m_random_sample", "m_mean_farm_size_ha",
          "m_sampling_unit", "m_type_data", "m_av_year_assessment", "m_education_years"
          )%>%
  left_join(factors_metric_assessed, by= c("factor_metric"))

sort(unique(data$pcc_factor_unit))
sort(unique(data$factor_metric))

names(data)    
data$ES_ID <- as.numeric(1:nrow(data)) #add a new column with the effect size ID number


names(data)
length(unique(data$study_id)) #154 articles for PCC analysis
sort(unique(data$study_id))  
sort(unique(data$pcc_factor_unit))  #69
length(unique(data$pcc_factor_unit)) #70

length(unique(data$m_dp_recla)) #10 systems
sort(unique(data$country)) #45
sort(unique(data$limitation_of_use_obs))

#######  Included FACTORS -------
length(unique(data$x_metric_recla)) #50
sort(unique(data$x_metric_recla))
length(unique(data$x_metric_recla2))#39

str(data)
sort(unique(data$factor_metric))
table(data$factor_metric)

factors_metric_unit<-data%>%
  group_by(factor_sub_class,   pcc_factor_unit ) %>%
  summarise(n_articles = n_distinct(study_id),
            n_ES = n_distinct(ES_ID))

sort(unique(factors_metric_unit$pcc_factor_unit)) # 66 factor_metric

write.csv(factors_metric_unit, "data/pcc_factors_metric_unit_articles.csv", row.names=FALSE)

####################### EFFECT SIZE calculation---------------------------------
library(metafor)
######### PCC effect size---------------------------------

### Compute Partial Correlation Coefficient effect size
#https://wviechtb.github.io/metadat/reference/dat.aloe2013.html
#Formulas: 
#rp <- tval/sqrt(tval^2+df)
#v <- (1-rp^2)/df # Variance Stanley & Doucouliagos (2012)
pcc_data<-escalc(measure="PCOR", ti= t_value_pcc, ni=n_samples, mi=n_factors, data=data,
                 var.names=c("pcc.yi","pcc.vi"))


sort(unique(pcc_data$pcc_factor_unit))
names(pcc_data)

######### Fisher Z transformation ---------------------------------
#https://osf.io/ubqfg
### Compute Fisher's z transformed partial correlations
#fis <- 0.5*log((1+rp)/(1-rp))
#v_fis <- rep(1/(n-3-1), k) # Variance Fisher's z transformed partial correlations

fis_data<- pcc_data%>%
  mutate(fis.yi= 0.5*log((1+pcc.yi)/(1-pcc.yi)))%>%
  mutate(fis.vi= (1/(n_samples-3-(n_factors-1))))%>%
  filter(pcc_factor_unit!="Attitude toward practice (positive continuous)")


sort(unique(fis_data$pcc_factor_unit))
sort(unique(fis_data$logor_factor_unit))
sort(unique(fis_data$study_id))
sort(unique(fis_data$main_crop))
sort(unique(fis_data$m_dp_recla))

######### Log-odds ratio ---------------------------------
# Calculate log-odds ratio variance from standard error
#Borenstain et al (2009) 37p
logor_data<- fis_data%>%
  mutate(v_logOR= (se_logOR)^2)

names(fis_data)

write.csv(logor_data, "data/pcc_data.csv", row.names=FALSE)

