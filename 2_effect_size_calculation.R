library(dplyr)
library(readxl)


####### FACTORS -------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_metric_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$logor_unit,")", sep="")

data<-read.csv("data/binary_adoption_clean_data.csv",header = TRUE, sep = ",")%>%
  left_join(factors_metric_assessed, by= c("factor_metric"))%>%
  mutate(study_model_id=paste(study_id, model_id, sep="_"))

length(unique(data$study_model_id)) #245

factors_models<- data%>%
  group_by( study_model_id)%>%
  dplyr::summarise(n_models = n_distinct(pcc_factor_unit))

sort(unique(data$pcc_factor_unit))
sort(unique(data$factor_metric))
sort(unique(data$factor_metric))

names(data)    
data$ES_ID <- as.numeric(1:nrow(data)) #add a new column with the effect size ID number


names(data)
length(unique(data$study_id)) #154 articles for PCC analysis
sort(unique(data$study_id))  
sort(unique(data$pcc_factor_unit))  #71
length(unique(data$pcc_factor_unit)) #72

length(unique(data$m_dp_recla)) #10 systems
sort(unique(data$country)) #44
sort(unique(data$limitation_of_use_obs))

#######  Included FACTORS -------
length(unique(data$x_metric_recla)) #52
sort(unique(data$x_metric_recla))
length(unique(data$x_metric_recla2))#41

str(data)
sort(unique(data$factor_metric))
table(data$factor_metric)

factors_metric_unit<-data%>%
  group_by(factor_category,   pcc_factor_unit ) %>%
  summarise(n_articles = n_distinct(study_id),
            n_ES = n_distinct(ES_ID))

sort(unique(factors_metric_unit$pcc_factor_unit)) # 71 factor_metric

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

length(unique(logor_data$study_id)) #154
