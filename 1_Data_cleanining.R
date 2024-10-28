#install.packages("Rtools")
library(readxl)
library(dplyr)

# Set the file path and name of the .xlsx file -------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/Meta_data_2024.02.15.xlsx"

# Use the read_excel() function to read the data into a data frame
data <- read_excel(data_path, sheet = "meta_PCC")
data <- data[-1,]
data <- data[-1,]

length(sort(unique(data$study_id))) # Number of articles 194
sort(unique(data$y_metric_recla)) #28
sort(unique(data$y_metric_recla_2))
sort(unique(data$limitation_of_use_obs))

table(data$y_metric_recla)
table(data$y_metric_recla_2)
#[1] "adoption"    "awareness"   "disadoption" "interest"   
table(data$y_metric_recla, data$y_metric_recla_2)
names(data)

### ---- Filter Adoption studies ----
adoption<- data%>%
  dplyr::filter(y_metric_recla_2=="adoption")

length(sort(unique(adoption$study_id))) # Number of articles 160
table(adoption$y_metric_recla) #Number of rows 3975
sort(unique(adoption$country)) #Countries 46
length(sort(unique(adoption$x_metric_recla))) # Unique factors 206

names(adoption)
### Select only necessary columns ----
adoption_clean<- adoption%>%
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
  dplyr::select(study_id,model_id,main_crop, country, 
                year_assessment_start, year_assessment_end,
                intervention_recla,intervention_recla_detail_1,
                intervention_recla_detail_2,intervention_recla_detail_3,
                intervention_recla_detail_4,
                y_metric_recla,x_metric_raw,x_metric_recla, x_metric_unit_raw,
                x_metric_unit_recla,x_type,
                transformation_coefficient,	transformation_coefficient_num,
                transformation_variance,
                transformation_variance_num,
                x_mean_value,x_mean_value_num,
                model_method_raw,model_method_recla,coefficient_type, 
                coefficient_value,
                variance_metric,variance_value,
                variance_ci_l,  variance_ci_u,
                z_t_value, p_value, df_original, n_factors,
                n_samples, 
                limitation_of_use_obs,m_exact_variance_value,m_random_sample, m_mean_farm_size_ha, 
                sampling_unit,type_data, endogeneity_correction, exposure_correction,
                x_sample_yes_dummy_binary3,x_sample_no_dummy_binary4)%>%
  
  mutate(factor_metric= paste(x_metric_recla, " (", x_metric_unit_recla, ")", sep=""))


length(sort(unique(adoption_clean$study_id))) # Number of articles 160
sort(unique(adoption_clean$study_id))
sort(unique(adoption_clean$limitation_of_use_obs))
sort(unique(adoption_clean$factor_metric))

####### Diversified farming systems -------
## m_intervention_recla2 = by system
sort(unique(adoption_clean$intervention_recla))
table(adoption_clean$intervention_recla)

adoption_clean$m_intervention_recla2<- stringr::str_to_sentence(adoption_clean$intervention_recla)
sort(unique(adoption_clean$m_intervention_recla2))

adoption_clean$m_intervention_recla2[adoption_clean$m_intervention_recla2 %in% c("Agroforestry and fallow",
                                                                             "Crop rotation and cover crops",
                                                                             "Crop rotation and intercropping",
                                                                             "Land with temporary fallow and cover crops"
                                                                             )]<- "Combined systems"
adoption_clean$m_intervention_recla2[adoption_clean$m_intervention_recla2 %in% c("Integrated aquaculture-agriculture")]<- "Agro-aquaculture"
adoption_clean$m_intervention_recla2[adoption_clean$m_intervention_recla2 %in% c("Grazing cut and carry",
                                                                             "Integrated crop-livestock",
                                                                             "Silvopasture")]<- "Agro-silvopasture"
adoption_clean$m_intervention_recla2[adoption_clean$m_intervention_recla2 %in% c("Embedded seminatural infrastructures")]<- "Embedded seminatural habitats"
adoption_clean$m_intervention_recla2[adoption_clean$m_intervention_recla2 %in% c("Land with temporary fallow")]<- "Fallow"

sort(unique(adoption_clean$m_intervention_recla2)) #10 systems
sort(unique(adoption_clean$intervention_recla_detail_1)) #10 systems

table(adoption_clean$m_intervention_recla2)

##m_intervention_system_components
sort(unique(adoption_clean$m_intervention_system_components))

table(adoption_clean$intervention_recla, adoption_clean$m_intervention_system_components)

sort(unique(prueba$study_id))


####### Prepare data for the analysis -------

## Simplify the name of coefficient_type
sort(unique(adoption_clean$coefficient_type))

adoption_clean$coefficient_type[adoption_clean$coefficient_type %in% "average marginal effect"] <- "AME"
adoption_clean$coefficient_type[adoption_clean$coefficient_type %in% "coefficient value"] <- "B"
adoption_clean$coefficient_type[adoption_clean$coefficient_type %in% "marginal effect"] <- "ME"
adoption_clean$coefficient_type[adoption_clean$coefficient_type %in% "odd ratio"] <- "OR"
sort(unique(adoption_clean$coefficient_type))

## Transform SD to SE: SE = SD/sqrt(sample size)
SD_SE <- function (sd,n) {  
  result<- (sd/sqrt(n))
  return(result)
}

adoption_clean$variance_value[adoption_clean$variance_metric %in% c("standard deviation")] <-  
  SD_SE(adoption_clean$variance_value[adoption_clean$variance_metric %in% c("standard deviation")],
        adoption_clean$n_samples[adoption_clean$variance_metric %in% c("standard deviation")])

## Simplify the name of variance_metric
sort(unique(adoption_clean$variance_metric))

adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("90% confidence intervals")] <- "90% CI"
adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("95% confidence intervals")] <- "95% CI"
adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("chi-square statistic")] <- "X2"
adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("p value")] <- "P"
adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("robust standard error","standard error",
                                                                       "standard deviation")] <- "SE"
adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("t value", "t ratio")] <- "T"
adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("wald statistic")] <- "WS"
adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("z value")] <- "Z"

sort(unique(adoption_clean$variance_metric))

table(adoption_clean$variance_metric,adoption_clean$model_method_recla )

## Transform 90% conﬁdence intervals to SE(b)
#coefficient_type == "OR"
#model_method_recla == "logit"
CI90_SE <- function (or,ci_l) {  
  result<- (log(or)-log(ci_l))/(1.645)
  return(result)
}

adoption_clean$variance_value[adoption_clean$variance_metric %in% c("90% CI")] <-  
  CI90_SE(adoption_clean$coefficient_value[adoption_clean$variance_metric %in% c("90% CI")],
          adoption_clean$variance_ci_l[adoption_clean$variance_metric %in% c("90% CI")])

## Transform 95% conﬁdence intervals to SE(b)
#coefficient_type == "OR"
#model_method_recla == "logit"
CI95_SE <- function (or,ci_u) {  
  result<- (log(or)+log(ci_u))/(1.96)
  return(result)
}

adoption_clean$variance_value[adoption_clean$variance_metric %in% c("95% CI")] <-  
  CI95_SE(adoption_clean$coefficient_value[adoption_clean$variance_metric %in% c("95% CI")],
          adoption_clean$variance_ci_u[adoption_clean$variance_metric %in% c("95% CI")])

## Transform OR to B
#coefficient_type == "OR"
#model_method_recla == "logit"
#variance_metric == c("90% CI", "95% CI")
OR_CI90_95_B <- function (or) {  
  result<- log(or)
  return(result)
}

adoption_clean$coefficient_value[adoption_clean$variance_metric %in% c("90% CI", "95% CI")] <-  
  OR_CI90_95_B(adoption_clean$coefficient_value[adoption_clean$variance_metric %in% c("90% CI","95% CI")])

adoption_clean$coefficient_type[adoption_clean$variance_metric %in% c("90% CI","95% CI")] <- "B"

adoption_clean$variance_metric[adoption_clean$variance_metric %in% c("90% CI","95% CI")] <- "SE"

sort(unique(adoption_clean$variance_metric))

## Combine coefficient type with variance metric
adoption_clean$coefficient_variance_type<- paste(adoption_clean$coefficient_type, adoption_clean$variance_metric,sep = "_")
sort(unique(adoption_clean$coefficient_variance_type))

# Combine model type, coefficient type and variance metric
adoption_clean$model_coefficient_variance_type<-paste(adoption_clean$model_method_recla, adoption_clean$coefficient_variance_type,sep = "_")

sort(unique(adoption_clean$model_coefficient_variance_type))
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla)

## ATTENTION: I need to contact the authors of these articles to ask for SE or t-z value
contact_authors_1<- adoption_clean%>%
  filter(is.na(variance_value))%>%
  filter(y_metric_recla=="diversity adoption (1=yes, 0=no)")

length(unique(contact_authors_1$study_id)) #23 articles
sort(unique(contact_authors_1$study_id))
sort(unique(contact_authors_1$y_metric_recla))


# ATENTION: I NEED TO CONTACT THE AUTHORS OF THE ARTICLES THAT DON'T PROVIDE THE EXACT P VALUE OR SE.
# Replace P = NA for 0.3 (Greenberg et al. 2003)
# Stanley and Doucouliagos recommend to use 0.1 OR 0.5
# Greenberg et al. (2003). A meta-analysis of government-sponsored training programs. use 0.3, as this is the midpoint between 0.10 and 0.5
#Stanley and Doucouliagos said that the best thing to do is to omit these results.
adoption_clean$variance_value<- ifelse(adoption_clean$variance_metric %in% "P" &
                                                is.na(adoption_clean$variance_value),0.3,adoption_clean$variance_value)

# Replace SE == 0 by 0.0001
adoption_clean$variance_value<- ifelse(adoption_clean$variance_metric %in% "SE" &
                                             adoption_clean$variance_value %in% 0 ,0.0001,adoption_clean$variance_value)


####### Calculate t value or z value -------
sort(unique(adoption_clean$coefficient_variance_type))
sort(unique(adoption_clean$model_method_recla))
#[1] "logit"  "OLS"    "other"  "probit" "tobit" 
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )

# model_method_recla == any model
# coefficient_variance_type == c("B_SE", "ME_SE", "AME_SE")
# z= B/SE, t= B/SE
# Cochrane 6.3.1 Obtaining standard errors from confidence intervals and P values: absolute (difference) measures#section-6-3-1
#https://training.cochrane.org/handbook/archive/v6/chapter-06
# and (2) From t statistic to standard error
#https://training.cochrane.org/handbook/archive/v6/chapter-06#_Ref190821230
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )
t_z_B_SE <- function (b, se) {  
  result<- (b/se)
  return(result)
}

adoption_clean$t_value_pcc[adoption_clean$coefficient_variance_type %in% c("B_SE","ME_SE","AME_SE")] <-  
  t_z_B_SE(adoption_clean$coefficient_value[adoption_clean$coefficient_variance_type %in% c("B_SE","ME_SE","AME_SE")],
           adoption_clean$variance_value[adoption_clean$coefficient_variance_type %in% c("B_SE","ME_SE","AME_SE")])

# model_method_recla == "logit" and "probit"
# coefficient_variance_type == c("B_P", "ME_P")
# CHECK: Ref available: Kleinbaum, D. G., & Klein, M. (2010). Logistic regression: a self-learning text (3rd ed.). Springer Science & Business Media.
# SE = B/z; z=  Φ^−1(p/2) ∗ sign(B)
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )
t_z_probit_logit_B_P <- function (b,p) {  
  result<- sign(b)* (abs(qnorm(p/2)))
  return(result)
}

adoption_clean$t_value_pcc[adoption_clean$model_coefficient_variance_type %in% c("logit_B_P","probit_B_P","logit_ME_P","probit_ME_P" )] <- 
  t_z_probit_logit_B_P(adoption_clean$coefficient_value[adoption_clean$model_coefficient_variance_type %in%c("logit_B_P","probit_B_P","logit_ME_P","probit_ME_P")],
                       adoption_clean$variance_value[adoption_clean$model_coefficient_variance_type %in%c("logit_B_P","probit_B_P","logit_ME_P","probit_ME_P")])

# model_coefficient_variance_type == c("probit_B_X2", "logit_B_WS")
# z=  sqrt(X2)
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )

t_z_probit_logit_B_X<- function (b,x) {  
  result<-   sign(b) * sqrt(abs(x))
  return(result)
}

adoption_clean$t_value_pcc[adoption_clean$model_coefficient_variance_type %in%  c("probit_B_X2", "logit_B_WS")] <- 
  t_z_probit_logit_B_X(adoption_clean$coefficient_value[adoption_clean$model_coefficient_variance_type %in% c("probit_B_X2", "logit_B_WS")],
                       adoption_clean$variance_value[adoption_clean$model_coefficient_variance_type %in% c("probit_B_X2", "logit_B_WS")])
  

# model_method_recla == c("logit")
# coefficient_variance_type == "OR_SE"
# (log(OR)*OR)/SE
#https://www.youtube.com/watch?v=RDY5MFVbRQE
#https://libguides.princeton.edu/logit
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )

t_z_logit_OR_SE<- function (or, se) {  
  result<-   (log(or)*or)/se
  return(result)
}

adoption_clean$t_value_pcc[adoption_clean$model_coefficient_variance_type %in% c("logit_OR_SE")] <- 
  t_z_logit_OR_SE(adoption_clean$coefficient_value[adoption_clean$model_coefficient_variance_type %in%c("logit_OR_SE")],
                       adoption_clean$variance_value[adoption_clean$model_coefficient_variance_type %in%c("logit_OR_SE")])

# model_method_recla == "logit" 
# coefficient_variance_type == c("OR_P")
# CHECK: Ref available: Kleinbaum, D. G., & Klein, M. (2010). Logistic regression: a self-learning text (3rd ed.). Springer Science & Business Media.
# SE = B/z; z=  Φ^−1(p/2) ∗ sign(OR)
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )
t_z_probit_logit_OR_P <- function (or,p) {  
  result<- sign(log(or))* (abs(qnorm(p/2)))
  return(result)
}

adoption_clean$t_value_pcc[adoption_clean$model_coefficient_variance_type %in% c("logit_OR_P")] <- 
  t_z_probit_logit_OR_P(adoption_clean$coefficient_value[adoption_clean$model_coefficient_variance_type %in%c("logit_OR_P")],
                       adoption_clean$variance_value[adoption_clean$model_coefficient_variance_type %in%c("logit_OR_P")])

# TO CHECK: I need to verify this formula
# model_method_recla == c("OLS", "tobit")
# coefficient_variance_type == c("B_P", "ME_P")
#Formula from Ruzzante et al supp info
# t_z= t = Ft^−1 (p/2, df) ∗ sign(b)
# coefficient_value > 0
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )
t_z_tobit_OLS_B_P <- function(p,n,k) {
  t <- qt(p/2, (n-k-1))
  return(t)
}

adoption_clean$t_value_pcc[adoption_clean$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")] <- 
  t_z_tobit_OLS_B_P(adoption_clean$variance_value[adoption_clean$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")],
                    adoption_clean$n_samples[adoption_clean$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")],
                    adoption_clean$n_factors[adoption_clean$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")])


# model_method_recla == any model
# coefficient_variance_type == c("B_T", "B_Z","ME_T", "ME_Z","OR_Z")
# t_z= t OR z
table(adoption_clean$coefficient_variance_type,adoption_clean$model_method_recla )

t_z_ANY <- function (t_z) {  
  result<- t_z
  return(result)
}

adoption_clean$t_value_pcc[adoption_clean$coefficient_variance_type %in%  c("B_T", "B_Z","ME_T", "ME_Z","OR_Z")] <- 
  t_z_ANY(adoption_clean$variance_value[adoption_clean$coefficient_variance_type %in%  c("B_T", "B_Z","ME_T", "ME_Z","OR_Z")])


## ATTENTION: I need to contact the authors of these articles to ask for SE or t-z value
contact_authors_2<- adoption_clean%>%
  filter(is.na(t_value_pcc))

length(unique(contact_authors_2$study_id)) #9 articles
sort(unique(contact_authors_2$study_id))
table(contact_authors_2$coefficient_variance_type,contact_authors_2$model_method_recla )

# Formula to get t value from GLM_B_P
# Formula to get t value from other_B_P
# Formula to get t value from nd_ME_P
# Check if #737 should be included, it reports negative SE values.
# Check if z_t_value_recal has the same sign than coefficient_value

####### DEPENDENT ADOPTION INTENSITY -------
adoption_intensity<-adoption_clean%>%
  filter(y_metric_recla!="diversity adoption (1=yes, 0=no)")

length(sort(unique(adoption_intensity$study_id))) # Number of articles 32
table(adoption_intensity$y_metric_recla) #Number of rows 755
sort(unique(adoption_intensity$country)) #Countries 18
length(sort(unique(adoption_intensity$x_metric_recla))) # Unique factors 99
sort(unique(adoption_intensity$study_id))

####### DEPENDENT ADOPTION BINARY (1= yes, 0= no) -------
adoption_binary<-adoption_clean%>%
  filter(y_metric_recla=="diversity adoption (1=yes, 0=no)")%>%
  ## Remove the rows with t_value_pcc == NA
  filter(!is.na(t_value_pcc))

sort(unique(adoption_binary$model_coefficient_variance_type))
  
length(sort(unique(adoption_binary$study_id))) # Number of articles 154
table(adoption_binary$y_metric_recla) #Number of rows 3905
sort(unique(adoption_binary$country)) #Countries 44
length(sort(unique(adoption_binary$x_metric_recla))) # Unique factors 163
sort(unique(adoption_binary$study_id))

table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla)

### Calculate LOG-ODDS RATIO (for Logit and Probit models)---------

#model_method_recla == "logit"
#coefficient_type == ("OR")
## Convert OR (odds ratio) to B (log-odds ratio)
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
B_logit_OR <- function(or) {
  b <- log(or)
  return(b)
}
adoption_binary$b_logOR[adoption_binary$model_method_recla %in% c("logit") & adoption_binary$coefficient_type %in%c("OR")]<- 
  B_logit_OR(adoption_binary$coefficient_value[adoption_binary$model_method_recla %in% c("logit") &
                                               adoption_binary$coefficient_type %in%c("OR")])


#model_method_recla == ("logit")
#coefficient_type== ("B")
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
B_logit_B <- function (b) {  
  result<- b
  return(result)
}

adoption_binary$b_logOR[adoption_binary$model_method_recla %in% c("logit") & 
                          adoption_binary$coefficient_type %in%c("B")] <- 
  B_logit_B(adoption_binary$coefficient_value[adoption_binary$model_method_recla %in% c("logit") & 
                                              adoption_binary$coefficient_type %in%c("B")])

#model_method_recla == ("probit")
#coefficient_type== ("B")
#CHECK: Amemiya (1981) said multiply by 1.6 (used by Ruzzante)
B_probit_B <- function (b) {  
  result<- b*1.6
  return(result)
}

adoption_binary$b_logOR[adoption_binary$model_method_recla %in% c("probit") & 
                          adoption_binary$coefficient_type %in%c("B")] <- 
  B_probit_B(adoption_binary$coefficient_value[adoption_binary$model_method_recla %in% c("probit") & 
                                              adoption_binary$coefficient_type %in%c("B")])


### Calculate SE of LOG-ODDS RATIO (for Logit and Probit models) ----------
#model_coefficient_variance_type == ("logit_OR_SE")
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
OR_SE_logit_SE <- function (or, se) {  
  result<- or/se
  return(result)
}

adoption_binary$se_logOR[adoption_binary$model_coefficient_variance_type %in%c("logit_OR_SE")] <- 
  OR_SE_logit_SE(adoption_binary$coefficient_value[adoption_binary$model_coefficient_variance_type %in% c("logit_OR_SE")],
                 adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in% c("logit_OR_SE")])

#model_coefficient_variance_type == ("logit_B_SE")
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
B_SE_logit_SE <- function (se) {  
  result<- se
  return(result)
}

adoption_binary$se_logOR[adoption_binary$model_coefficient_variance_type %in%c("logit_B_SE")] <- 
  B_SE_logit_SE(adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in% c("logit_B_SE")])



#model_coefficient_variance_type == ("logit_B_P","logit_B_T","logit_B_WS","logit_B_Z","logit_OR_P","logit_OR_Z")
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
OR_P_logit_SE <- function (b, t) {  
  result<- b/t
  return(result)
}

adoption_binary$se_logOR[adoption_binary$model_coefficient_variance_type %in%
                           c("logit_B_P","logit_B_T","logit_B_WS", "logit_B_Z","logit_OR_P","logit_OR_Z")] <- 
  OR_P_logit_SE(adoption_binary$b_logOR[adoption_binary$model_coefficient_variance_type %in% 
                                          c("logit_B_P","logit_B_T","logit_B_WS", "logit_B_Z","logit_OR_P","logit_OR_Z")],
                 adoption_binary$t_value_pcc[adoption_binary$model_coefficient_variance_type %in% 
                                               c("logit_B_P","logit_B_T","logit_B_WS", "logit_B_Z","logit_OR_P","logit_OR_Z")])


#model_coefficient_variance_type == ("probit_B_SE")
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
B_SE_probit_SE <- function (se) {  
  result<- se*1.6
  return(result)
}

adoption_binary$se_logOR[adoption_binary$model_coefficient_variance_type %in%c("probit_B_SE")] <- 
  B_SE_probit_SE(adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in% c("probit_B_SE")])


#model_coefficient_variance_type == ("probit_B_P","probit_B_T","probit_B_X2","probit_B_Z")
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
B_T_probit_SE <- function (b,t) {  
  result<- (b/t)*1.6
  return(result)
}

adoption_binary$se_logOR[adoption_binary$model_coefficient_variance_type %in%c("probit_B_P","probit_B_T","probit_B_X2","probit_B_Z")] <- 
  B_T_probit_SE(adoption_binary$coefficient_value[adoption_binary$model_coefficient_variance_type %in% c("probit_B_P","probit_B_T","probit_B_X2","probit_B_Z")],
                 adoption_binary$t_value_pcc[adoption_binary$model_coefficient_variance_type %in% c("probit_B_P","probit_B_T","probit_B_X2","probit_B_Z")])

### Check 
check<-adoption_binary%>%
  filter(is.na(se_logOR))

sort(unique(check$model_coefficient_variance_type))
length(sort(unique(check$study_id))) # Number of articles 16
sort(unique(check$country)) #Countries 12
sort(unique(check$study_id))

### Convert the coefficient and variance values to the same metric (e.g., acres, hours, miles, etc.)--------
# Transforms the t_value_pcc
adoption_binary$t_value_pcc[adoption_binary$transformation_coefficient_num %in% -1] <- 
  adoption_binary$t_value_pcc[adoption_binary$transformation_coefficient_num %in% -1]*
  adoption_binary$transformation_coefficient_num[adoption_binary$transformation_coefficient_num %in% -1]   

# Transforms the b_logOR
adoption_binary$b_logOR[!is.na(adoption_binary$transformation_coefficient_num)] <- 
  adoption_binary$b_logOR[!is.na(adoption_binary$transformation_coefficient_num)]*
  adoption_binary$transformation_coefficient_num[!is.na(adoption_binary$transformation_coefficient_num)]   

#Transform the se_logOR
adoption_binary$se_logOR[!is.na(adoption_binary$transformation_variance_num)] <- 
  adoption_binary$se_logOR[!is.na(adoption_binary$transformation_variance_num)]*
  adoption_binary$transformation_variance_num[!is.na(adoption_binary$transformation_variance_num)]   


length(unique(adoption_binary$study_id)) # Number of PCC studies 154
length(unique(adoption_binary$study_id[!is.na(adoption_binary$b_logOR)])) #137
length(unique(adoption_binary$x_metric_recla)) #164 factors
sort(unique(adoption_binary$study_id))


factors_articles_count <- adoption_binary %>%
  group_by(x_metric_recla,x_metric_unit_recla) %>%
  summarise(n_articles = n_distinct(study_id))

names(adoption_binary)

write.csv(factors_articles_count, "data/binary_adoption_factors_articles1.csv", row.names=FALSE)


#### Filter only the factors I'm going to study ------
data_adoption_binary<-adoption_binary%>%
  filter(
    # Biophysical context
    x_metric_recla=="precipitation"|
    x_metric_recla=="soil depth"|  
    x_metric_recla=="soil fertility"|
    x_metric_recla=="soil slope"|
    x_metric_recla=="temperature"|
    
    # Farmers behaviour
    x_metric_recla=="hh perceive benefits of SFP or DFS"| #to check
    x_metric_recla=="hh risk attitude"| #to check
    x_metric_recla=="limitations to implement SFT or DFS"| #to check
    x_metric_recla=="production constraints"| #to check
    #  x_metric_recla=="hh perception of precipitation"|
      
      # Financial capital
  x_metric_recla=="access to off-farm income"|
  x_metric_recla=="h income"|
  x_metric_recla=="h off-farm income"| 
  x_metric_recla=="h on-farm income"|
  x_metric_recla=="livestock owned"|
  x_metric_recla=="units of livestock"|
  
  # Human capital
  x_metric_recla=="h size"|
  x_metric_recla=="hh age"|
  x_metric_recla=="hh education"|
  x_metric_recla=="hh gender"|
  #x_metric_recla=="hh is native"|
  x_metric_recla=="hh marital status"|
  x_metric_recla=="farm labour force (non-hired)"|
  x_metric_recla=="farm labour force (hired)"|
  x_metric_recla=="h adult members"|
    x_metric_recla=="hh farming experience" |
    
  #Natural capital
  x_metric_recla=="farm size"|
  x_metric_recla=="number of agricultural plots"|
  x_metric_recla=="plot size"|
  
  # Physical capital 
  x_metric_recla=="access to irrigation"|
  x_metric_recla=="distance farm-house" |           
  x_metric_recla=="distance to market"| 
  x_metric_recla=="distance to input market"| 
  x_metric_recla=="distance to output market"|
  x_metric_recla=="distance to road"|
  
  # Social capital
  x_metric_recla=="family members and friends living in and out the community"|
  x_metric_recla=="hh association member" |
  x_metric_recla=="hh comunicate with other farmers"|
  x_metric_recla=="hh perception of extension services"|
  

# Political and institutional context
  x_metric_recla=="land tenure security"| 
  
  x_metric_recla== "access to agricultural extension"|
  x_metric_recla=="access to agricultural information"|
  x_metric_recla=="access to agricultural training"|
  x_metric_recla=="agricultural extension frequency"|
  x_metric_recla== "awareness of SFP or DFS"| #to check
  
  x_metric_recla=="receive incentive for conservation"|
  x_metric_recla=="access to credit"|  
  x_metric_recla=="access to credit is a constraint"  |
  x_metric_recla=="hh perception of climate change" ) # to check


sort(unique(data_adoption_binary$limitation_of_use_obs))

####### Countries -------
sort(unique(data_adoption_binary$country)) #44

UN_region <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/1_chapter_effect_size/UN_region.xlsx", sheet = "UN_subregion")%>%
  mutate(Country_Name= if_else(Country_Name == "United States of America (The)","USA",
                               if_else(Country_Name == "Democratic Rep. of the Congo (The)","Democratic Republic of the Congo",
                                       if_else(Country_Name =="United Republic of Tanzania (The)","Tanzania",
                                       if_else(Country_Name =="Bolivia (Plurinational State of)","Bolivia",
                                               if_else(Country_Name == "Sudan (The)", "Sudan",
                                                       if_else(Country_Name=="Republic of Moldova (The)", "Moldova",
                                                               if_else(Country_Name=="Philippines (The)", "Philippines",
                                                                       if_else(Country_Name=="Viet Nam", "Vietnam",
                                                                               if_else(Country_Name == "Iran (Islamic Republic of)", "Iran",
                                                                                       if_else(Country_Name == "Niger (The)","Niger",
                                                                               Country_Name)))))))))))%>%
  dplyr::select(Country_Name, UN_Regions, UN_sub_region,Developed_Developing)%>%
  dplyr::rename("un_region"="UN_Regions",
                "un_subregion"="UN_sub_region")

sort(unique(UN_region$Country_Name))

data_adoption_binary<- data_adoption_binary%>%
  left_join(UN_region, by=c("country" ="Country_Name"))
names(data_adoption_binary)

data_adoption_binary$un_region[data_adoption_binary$country %in% "Vietnam, Thailand"] <-"Asia"
data_adoption_binary$un_subregion[data_adoption_binary$country %in% "Vietnam, Thailand"] <-"South-eastern Asia"
data_adoption_binary$un_region[
  data_adoption_binary$country %in% "Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia"] <-"Africa"


sort(unique(data_adoption_binary$country[is.na(data_adoption_binary$un_subregion)])) #1
sort(unique(data_adoption_binary$country)) #44
sort(unique(data_adoption_binary$un_region)) #5
sort(unique(data_adoption_binary$un_subregion)) #14

table(data_adoption_binary$country,data_adoption_binary$un_region)
length(unique(data_adoption_binary$study_id)) #154 articles 
sort(unique(data_adoption_binary$study_id))
sort(unique(data_adoption_binary$country[data_adoption_binary$un_subregion %in% c("Central America")]))

table(data_adoption_binary$country,data_adoption_binary$un_subregion)


####### Factors classification -------

factors_assessed <- read_excel(data_path, sheet = "FACTORS_metric_assessed")


####### CHECK NO LIMITATION DATA -------
factors<-data_adoption_binary%>%
  mutate(factor_metric=paste(x_metric_recla," (",x_metric_unit_recla,")",sep=""))%>%
  left_join(factors_assessed, by="factor_metric")%>%
  #mutate(x_metric_recla3= paste(x_metric_recla2,pcc_unit,sep="_"))%>%
  filter(limitation_of_use_obs== "no limitation")%>%
  group_by(factor_sub_class, x_metric_recla2 ) %>%
  summarise(n_articles = n_distinct(study_id))%>%
  ungroup()
  
sort(unique(factors$factor_metric))
  
sort(unique(factors$factor_sub_class))


####### Remove !=no limitation data AND reclassify MODERATORS-------
m_education_years<- data_adoption_binary%>%
  filter(factor_metric == "hh education (years)")%>%
  select(study_id, model_id, x_mean_value)%>%
  dplyr::rename("m_education_years"="x_mean_value")

m_age_years<-data_adoption_binary%>%
  filter(factor_metric == "hh age (years)")%>%
  select(study_id, model_id, x_mean_value)%>%
  dplyr::rename("m_age_years"="x_mean_value")

m_age_years<-data_adoption_binary%>%
  filter(factor_metric == "hh age (years)")%>%
  select(study_id, model_id, x_mean_value)%>%
  dplyr::rename("m_age_years"="x_mean_value")

names(data_adoption_binary)

m_gender_percent<-data_adoption_binary%>%
  filter(x_metric_recla == "hh gender")%>%
  filter(x_metric_unit_raw=="1= female, 0= male"|
         x_metric_unit_raw=="1= male, 0= female"|
         x_metric_unit_raw=="1= male, 0= otherwise"|
         x_metric_unit_raw=="1= male, 2= female")%>%
  mutate_at(vars(x_sample_yes_dummy_binary3,x_sample_no_dummy_binary4),as.numeric)%>%
  mutate(m_male_percent= if_else(x_metric_unit_raw=="1= female, 0= male",
                               (100-x_sample_yes_dummy_binary3), x_sample_yes_dummy_binary3))%>%
  select(study_id, model_id,m_male_percent)
    
sort(unique(m_gender_percent$x_metric_unit_raw))

pcc_data_adoption_binary<- data_adoption_binary%>%
  filter(!is.na(t_value_pcc))%>%
  filter(limitation_of_use_obs== "no limitation")%>%
  # MODERATORS
  dplyr::rename("m_model_method"= "model_method_raw")%>%
  left_join(m_education_years, by=c("study_id"="study_id",
                                    "model_id"="model_id"))%>%
  left_join(m_age_years, by=c("study_id"="study_id",
                                    "model_id"="model_id"))%>%
  left_join(m_gender_percent,by=c("study_id"="study_id",
                                  "model_id"="model_id"))%>%
  
  mutate(m_sampling_unit= if_else(sampling_unit== "farmers" |
                                    sampling_unit=="household"|
                                    sampling_unit=="household data collection"|
                                    sampling_unit=="households"|
                                    sampling_unit=="landholders"|
                                    sampling_unit=="managers", 1, 0))%>%
  mutate(m_type_data = if_else(type_data== "primary and secondary data" |
                                 type_data==  "primary data",1,0  ))%>%
  mutate_at(vars("year_assessment_start", "year_assessment_end", "m_education_years","m_age_years"), as.numeric)%>%
  mutate(m_av_year_assessment= if_else(is.na(year_assessment_end),
                                       year_assessment_start,
                                       ((year_assessment_start+year_assessment_end)/2)))%>%
  mutate(m_av_year_assessment= round(m_av_year_assessment,0))%>%
  #Endogeneity analysis by primary articles yes/no
  mutate(m_endogeneity_correction= if_else(m_endogeneity_correction=="no endogeneity correction",0,1))%>%
  #Exposure correction analysis by primary articles yes/no
  mutate(m_exposure_correction= if_else(m_exposure_correction=="no exposure correction",0,1))
  





names(pcc_data_adoption_binary)
sort(unique(pcc_data_adoption_binary$m_av_year_assessment)) # 1988 - 2023
sort(unique(pcc_data_adoption_binary$m_sampling_unit)) # 

length(unique(pcc_data_adoption_binary$study_id)) #153 articles for PCC analysis
sort(unique(pcc_data_adoption_binary$study_id)) #153 articles 
length(unique(pcc_data_adoption_binary$m_intervention_recla2)) #10 systems
length(unique(pcc_data_adoption_binary$x_metric_recla)) #47
sort(unique(pcc_data_adoption_binary$country)) #44
sort(unique(pcc_data_adoption_binary$limitation_of_use_obs)) 

### Moderators ---
names(pcc_data_adoption_binary)

#verificar cuantos articulos para log-odds ratio y cuantos para pcc

write.csv(pcc_data_adoption_binary,"data/binary_adoption_clean_data.csv", row.names=FALSE)

