#install.packages("Rtools")
library(readxl)
library(dplyr)

# Set the file path and name of the .xlsx file -------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

# Use the read.csv() function to read the clean meta-data into a data frame
data <- read.csv(paste0(data_path,"meta_data_clean.csv"),header = TRUE, sep = ",")

length(sort(unique(data$study_id))) # Number of articles 194
sort(unique(data$y_metric_recla)) #28
sort(unique(data$y_metric_recla_2)) #5 dependent variables
sort(unique(data$limitation_of_use_obs))
sort(unique(data$m_dp_recla))

table(data$y_metric_recla)
table(data$y_metric_recla_2)
#[1] "adoption"    "awareness"   "disadoption" "interest"   
table(data$y_metric_recla, data$y_metric_recla_2)
names(data)

### ---- Filter Adoption studies ----
#filter only studies analyzing adoption as a dependent variable
adoption<- data%>%
  dplyr::filter(y_metric_recla_2=="adoption")

length(sort(unique(adoption$study_id))) # 160 studies
table(adoption$y_metric_recla) #Number of rows 3974
sort(unique(adoption$country)) #Countries 46
length(sort(unique(adoption$x_metric_recla))) # Unique factors 205

names(adoption)
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
  dplyr::select(study_id,model_id,main_crop, country, 
                year_assessment_start, year_assessment_end,
                dp_raw, dp_details,dp_recla,dp_detail_1,
                dp_detail_2,dp_detail_3,
                dp_detail_4,y_metric_raw,
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
                x_sample_yes_dummy_binary3,x_sample_no_dummy_binary4,
                m_dp_recla,m_un_region,m_un_subregion)%>%
  
  mutate(factor_metric= paste(x_metric_recla, " (", x_metric_unit_recla, ")", sep=""))

sort(unique(adoption_binary$y_metric_recla))
table(adoption_binary$y_metric_recla) #Number of rows 3949
length(sort(unique(adoption_binary$study_id))) # 158 studies
sort(unique(adoption_binary$study_id))
sort(unique(adoption_binary$limitation_of_use_obs))
sort(unique(adoption_binary$factor_metric))
length(sort(unique(adoption_binary$x_metric_recla))) # Unique factors 205
sort(unique(adoption_binary$m_dp_recla)) #10 diversified practices
table(adoption_binary$m_dp_recla)
sort(unique(adoption_binary$country)) #Countries 45

####### Prepare data for the analysis -------

## Simplify the name of coefficient_type
sort(unique(adoption_binary$coefficient_type))
adoption_binary$coefficient_type[adoption_binary$coefficient_type %in% "coefficient value"] <- "B"
adoption_binary$coefficient_type[adoption_binary$coefficient_type %in% "marginal effect"] <- "ME"
adoption_binary$coefficient_type[adoption_binary$coefficient_type %in% "odd ratio"] <- "OR"
sort(unique(adoption_binary$coefficient_type))

## Convert SD to SE: SE = SD/sqrt(sample size)
SD_SE <- function (sd,n) {  
  result<- (sd/sqrt(n))
  return(result)
}

adoption_binary$variance_value[adoption_binary$variance_metric %in% c("standard deviation")] <-  
  SD_SE(adoption_binary$variance_value[adoption_binary$variance_metric %in% c("standard deviation")],
        adoption_binary$n_samples[adoption_binary$variance_metric %in% c("standard deviation")])

## Simplify the name of variance_metric
sort(unique(adoption_binary$variance_metric))

adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("90% confidence intervals")] <- "90% CI"
adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("95% confidence intervals")] <- "95% CI"
adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("chi-square statistic")] <- "X2"
adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("p value")] <- "P"
adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("robust standard error","standard error",
                                                                       "standard deviation")] <- "SE"
adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("t value", "t ratio")] <- "T"
adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("wald statistic")] <- "WS"
adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("z value")] <- "Z"

sort(unique(adoption_binary$variance_metric))

table(adoption_binary$variance_metric,adoption_binary$model_method_recla )

## Transform 90% conﬁdence intervals to SE(b)
#coefficient_type == "OR"
#model_method_recla == "logit"
CI90_SE <- function (or,ci_l) {  
  result<- (log(or)-log(ci_l))/(1.645)
  return(result)
}

adoption_binary$variance_value[adoption_binary$variance_metric %in% c("90% CI")] <-  
  CI90_SE(adoption_binary$coefficient_value[adoption_binary$variance_metric %in% c("90% CI")],
          adoption_binary$variance_ci_l[adoption_binary$variance_metric %in% c("90% CI")])

## Transform 95% conﬁdence intervals to SE(b)
#coefficient_type == "OR"
#model_method_recla == "logit"
CI95_SE <- function (or,ci_u) {  
  result<- (log(or)+log(ci_u))/(1.96)
  return(result)
}

adoption_binary$variance_value[adoption_binary$variance_metric %in% c("95% CI")] <-  
  CI95_SE(adoption_binary$coefficient_value[adoption_binary$variance_metric %in% c("95% CI")],
          adoption_binary$variance_ci_u[adoption_binary$variance_metric %in% c("95% CI")])

## Transform OR to B
#coefficient_type == "OR"
#model_method_recla == "logit"
#variance_metric == c("90% CI", "95% CI")
OR_CI90_95_B <- function (or) {  
  result<- log(or)
  return(result)
}

adoption_binary$coefficient_value[adoption_binary$variance_metric %in% c("90% CI", "95% CI")] <-  
  OR_CI90_95_B(adoption_binary$coefficient_value[adoption_binary$variance_metric %in% c("90% CI","95% CI")])

adoption_binary$coefficient_type[adoption_binary$variance_metric %in% c("90% CI","95% CI")] <- "B"

adoption_binary$variance_metric[adoption_binary$variance_metric %in% c("90% CI","95% CI")] <- "SE"

sort(unique(adoption_binary$variance_metric))

## Combine coefficient type with variance metric
adoption_binary$coefficient_variance_type<- paste(adoption_binary$coefficient_type, adoption_binary$variance_metric,sep = "_")
sort(unique(adoption_binary$coefficient_variance_type))

# Combine model type, coefficient type and variance metric
adoption_binary$model_coefficient_variance_type<-paste(adoption_binary$model_method_recla, adoption_binary$coefficient_variance_type,sep = "_")

sort(unique(adoption_binary$model_coefficient_variance_type))
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla)

## ATTENTION: I need to contact the authors of these articles to ask for SE or t-z value
contact_authors_1<- adoption_binary%>%
  filter(is.na(variance_value))%>%
  filter(y_metric_recla=="diversity adoption (1=yes, 0=no)")

length(unique(contact_authors_1$study_id)) #22 articles
sort(unique(contact_authors_1$study_id))
sort(unique(contact_authors_1$y_metric_recla))


# ATENTION: I NEED TO CONTACT THE AUTHORS OF THE ARTICLES THAT DON'T PROVIDE THE EXACT P VALUE OR SE.
# Replace P = NA for 0.3 (Greenberg et al. 2003)
# Stanley and Doucouliagos recommend to use 0.1 OR 0.5
# Greenberg et al. (2003). A meta-analysis of government-sponsored training programs. use 0.3, as this is the midpoint between 0.10 and 0.5
#Stanley and Doucouliagos said that the best thing to do is to omit these results.
adoption_binary$variance_value<- ifelse(adoption_binary$variance_metric %in% "P" &
                                                is.na(adoption_binary$variance_value),0.3,adoption_binary$variance_value)

# Replace SE == 0 by 0.0001
adoption_binary$variance_value<- ifelse(adoption_binary$variance_metric %in% "SE" &
                                             adoption_binary$variance_value %in% 0 ,0.0001,adoption_binary$variance_value)


####### Calculate t value or z value -------
sort(unique(adoption_binary$coefficient_variance_type))
sort(unique(adoption_binary$model_method_recla))
#[1] "logit"  "OLS"    "other"  "probit" "tobit" 
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )

# model_method_recla == any model
# coefficient_variance_type == c("B_SE", "ME_SE", "AME_SE")
# z= B/SE, t= B/SE
# Cochrane 6.3.1 Obtaining standard errors from confidence intervals and P values: absolute (difference) measures#section-6-3-1
#https://training.cochrane.org/handbook/archive/v6/chapter-06
# and (2) From t statistic to standard error
#https://training.cochrane.org/handbook/archive/v6/chapter-06#_Ref190821230
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
t_z_B_SE <- function (b, se) {  
  result<- (b/se)
  return(result)
}

adoption_binary$t_value_pcc[adoption_binary$coefficient_variance_type %in% c("B_SE","ME_SE","AME_SE")] <-  
  t_z_B_SE(adoption_binary$coefficient_value[adoption_binary$coefficient_variance_type %in% c("B_SE","ME_SE","AME_SE")],
           adoption_binary$variance_value[adoption_binary$coefficient_variance_type %in% c("B_SE","ME_SE","AME_SE")])

# model_method_recla == "logit" and "probit"
# coefficient_variance_type == c("B_P", "ME_P")
# CHECK: Ref available: Kleinbaum, D. G., & Klein, M. (2010). Logistic regression: a self-learning text (3rd ed.). Springer Science & Business Media.
# SE = B/z; z=  Φ^−1(p/2) ∗ sign(B)
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
t_z_probit_logit_B_P <- function (b,p) {  
  result<- sign(b)* (abs(qnorm(p/2)))
  return(result)
}

adoption_binary$t_value_pcc[adoption_binary$model_coefficient_variance_type %in% c("logit_B_P","probit_B_P","logit_ME_P","probit_ME_P" )] <- 
  t_z_probit_logit_B_P(adoption_binary$coefficient_value[adoption_binary$model_coefficient_variance_type %in%c("logit_B_P","probit_B_P","logit_ME_P","probit_ME_P")],
                       adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in%c("logit_B_P","probit_B_P","logit_ME_P","probit_ME_P")])

# model_coefficient_variance_type == c("probit_B_X2", "logit_B_WS")
# z=  sqrt(X2)
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )

t_z_probit_logit_B_X<- function (b,x) {  
  result<-   sign(b) * sqrt(abs(x))
  return(result)
}

adoption_binary$t_value_pcc[adoption_binary$model_coefficient_variance_type %in%  c("probit_B_X2", "logit_B_WS")] <- 
  t_z_probit_logit_B_X(adoption_binary$coefficient_value[adoption_binary$model_coefficient_variance_type %in% c("probit_B_X2", "logit_B_WS")],
                       adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in% c("probit_B_X2", "logit_B_WS")])
  

# model_method_recla == c("logit")
# coefficient_variance_type == "OR_SE"
# (log(OR)*OR)/SE
#https://www.youtube.com/watch?v=RDY5MFVbRQE
#https://libguides.princeton.edu/logit
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )

t_z_logit_OR_SE<- function (or, se) {  
  result<-   (log(or)*or)/se
  return(result)
}

adoption_binary$t_value_pcc[adoption_binary$model_coefficient_variance_type %in% c("logit_OR_SE")] <- 
  t_z_logit_OR_SE(adoption_binary$coefficient_value[adoption_binary$model_coefficient_variance_type %in%c("logit_OR_SE")],
                       adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in%c("logit_OR_SE")])

# model_method_recla == "logit" 
# coefficient_variance_type == c("OR_P")
# CHECK: Ref available: Kleinbaum, D. G., & Klein, M. (2010). Logistic regression: a self-learning text (3rd ed.). Springer Science & Business Media.
# SE = B/z; z=  Φ^−1(p/2) ∗ sign(OR)
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
t_z_probit_logit_OR_P <- function (or,p) {  
  result<- sign(log(or))* (abs(qnorm(p/2)))
  return(result)
}

adoption_binary$t_value_pcc[adoption_binary$model_coefficient_variance_type %in% c("logit_OR_P")] <- 
  t_z_probit_logit_OR_P(adoption_binary$coefficient_value[adoption_binary$model_coefficient_variance_type %in%c("logit_OR_P")],
                       adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in%c("logit_OR_P")])

# TO CHECK: I need to verify this formula
# model_method_recla == c("OLS", "tobit")
# coefficient_variance_type == c("B_P", "ME_P")
#Formula from Ruzzante et al supp info
# t_z= t = Ft^−1 (p/2, df) ∗ sign(b)
# coefficient_value > 0
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )
t_z_tobit_OLS_B_P <- function(p,n,k) {
  t <- qt(p/2, (n-k-1))
  return(t)
}

adoption_binary$t_value_pcc[adoption_binary$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")] <- 
  t_z_tobit_OLS_B_P(adoption_binary$variance_value[adoption_binary$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")],
                    adoption_binary$n_samples[adoption_binary$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")],
                    adoption_binary$n_factors[adoption_binary$model_coefficient_variance_type %in%  c("tobit_B_P","OLS_B_P","tobit_ME_P")])


# model_method_recla == any model
# coefficient_variance_type == c("B_T", "B_Z","ME_T", "ME_Z","OR_Z")
# t_z= t OR z
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla )

t_z_ANY <- function (t_z) {  
  result<- t_z
  return(result)
}

adoption_binary$t_value_pcc[adoption_binary$coefficient_variance_type %in%  c("B_T", "B_Z","ME_T", "ME_Z","OR_Z")] <- 
  t_z_ANY(adoption_binary$variance_value[adoption_binary$coefficient_variance_type %in%  c("B_T", "B_Z","ME_T", "ME_Z","OR_Z")])


## ATTENTION: I need to contact the authors of these articles to ask for SE or t-z value
contact_authors_2<- adoption_binary%>%
  filter(is.na(t_value_pcc))

length(unique(contact_authors_2$study_id)) #3 articles
sort(unique(contact_authors_2$study_id)) #338  856 1021
table(contact_authors_2$coefficient_variance_type,contact_authors_2$model_method_recla )

######## Remove the rows with t_value_pcc == NA
adoption_binary<-adoption_binary%>%
  filter(!is.na(t_value_pcc))

  
length(sort(unique(adoption_binary$study_id))) # Number of articles 156
table(adoption_binary$y_metric_recla) #Number of rows 3932
sort(unique(adoption_binary$country)) #Countries 45
length(sort(unique(adoption_binary$x_metric_recla))) # Unique factors 205
sort(unique(adoption_binary$study_id))
sort(unique(adoption_binary$model_coefficient_variance_type))
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
sort(unique(check$country)) #Countries 11
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


length(unique(adoption_binary$study_id)) # Number of PCC studies 156
length(unique(adoption_binary$study_id[!is.na(adoption_binary$b_logOR)])) #140
length(unique(adoption_binary$x_metric_recla)) #205 factors
sort(unique(adoption_binary$study_id))


factors_articles_count <- adoption_binary %>%
  group_by(x_metric_recla,x_metric_unit_recla) %>%
  summarise(n_articles = n_distinct(study_id))

names(adoption_binary)

write.csv(factors_articles_count, "data/binary_adoption_factors_articles1.csv", row.names=FALSE)


#### Filter only the most frequently analysed factors with comparable units ------
adoption_binary_clean<-adoption_binary%>%
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
    x_metric_recla=="production constraints"| #to check |
      x_metric_recla=="awareness of climate change" |
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
  x_metric_recla=="trust in extension services"|
  

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
  
  #Farm management
  x_metric_recla=="organic fertilizer use"
   ) # to check


length(sort(unique(adoption_binary_clean$study_id))) #156
sort(unique(adoption_binary_clean$limitation_of_use_obs))
sort(unique(adoption_binary_clean$country[is.na(adoption_binary_clean$m_un_subregion)])) #1
sort(unique(adoption_binary_clean$country)) #45
sort(unique(adoption_binary_clean$m_un_region)) #5 regions
sort(unique(adoption_binary_clean$m_un_subregion)) #14 subregions
sort(unique(adoption_binary_clean$country[adoption_binary_clean$m_un_subregion %in% c("Central America")]))


####### Factors classification -------
factors_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")


####### CHECK NO LIMITATION DATA -------
factors<-adoption_binary_clean%>%
  mutate(factor_metric=paste(x_metric_recla," (",x_metric_unit_recla,")",sep=""))%>%
  left_join(factors_assessed, by="factor_metric")%>%
  #mutate(x_metric_recla3= paste(x_metric_recla2,pcc_unit,sep="_"))%>%
  filter(limitation_of_use_obs== "no limitation")%>%
  group_by(factor_sub_class, x_metric_recla2 ) %>%
  summarise(n_articles = n_distinct(study_id))%>%
  ungroup()
  
sort(unique(factors$factor_sub_class))


####### Remove !=no limitation data AND reclassify MODERATORS-------
m_education_years<- adoption_binary_clean%>%
  filter(factor_metric == "hh education (years)")%>%
  select(study_id, model_id, x_mean_value)%>%
  dplyr::rename("m_education_years"="x_mean_value")

m_age_years<-adoption_binary_clean%>%
  filter(factor_metric == "hh age (years)")%>%
  select(study_id, model_id, x_mean_value)%>%
  dplyr::rename("m_age_years"="x_mean_value")

m_age_years<-adoption_binary_clean%>%
  filter(factor_metric == "hh age (years)")%>%
  select(study_id, model_id, x_mean_value)%>%
  dplyr::rename("m_age_years"="x_mean_value")

names(adoption_binary_clean)

m_gender_percent<-adoption_binary_clean%>%
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

pcc_adoption_binary_clean<- adoption_binary_clean%>%
  filter(!is.na(t_value_pcc))%>%
  filter(limitation_of_use_obs== "no limitation")%>%
  # MODERATORS
  mutate(m_model_method_recla= model_method_recla)%>%
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
  mutate(m_endogeneity_correction= if_else(endogeneity_correction=="no endogeneity correction",0,1))%>%
  #Exposure correction analysis by primary articles yes/no
  mutate(m_exposure_correction= if_else(exposure_correction=="no exposure correction",0,1))
  

names(pcc_adoption_binary_clean)
sort(unique(pcc_adoption_binary_clean$m_av_year_assessment)) # 1988 - 2023
sort(unique(pcc_adoption_binary_clean$m_sampling_unit)) # 

length(unique(pcc_adoption_binary_clean$study_id)) #155 studies for PCC analysis
sort(unique(pcc_adoption_binary_clean$study_id)) #155 studies
length(unique(pcc_adoption_binary_clean$m_dp_recla)) #10 practices
length(unique(pcc_adoption_binary_clean$x_metric_recla)) #47
sort(unique(pcc_adoption_binary_clean$x_metric_recla)) #47
sort(unique(pcc_adoption_binary_clean$country)) #45
sort(unique(pcc_adoption_binary_clean$limitation_of_use_obs)) 

### Moderators ---
names(pcc_adoption_binary_clean)

#verificar cuantos articulos para log-odds ratio y cuantos para pcc

write.csv(pcc_adoption_binary_clean,"data/binary_adoption_clean_data.csv", row.names=FALSE)

