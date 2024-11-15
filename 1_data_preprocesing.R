#install.packages("Rtools")
library(readxl)
library(dplyr)

# Use the read.csv() function to read the clean evidence_map-data into a data frame
adoption_binary <- read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis_2024.02.04/Factors-associated-to-the-adoption-of-diversified-farming-systems/data/data_adoption_binary.csv",header = TRUE, sep = ",")
               
   
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

adoption_binary$variance_value[adoption_binary$variance_type %in% c("standard deviation")] <-  
  SD_SE(adoption_binary$variance_value[adoption_binary$variance_type %in% c("standard deviation")],
        adoption_binary$n_samples[adoption_binary$variance_type %in% c("standard deviation")])

## Simplify the name of variance_type
sort(unique(adoption_binary$variance_type))

adoption_binary$variance_type[adoption_binary$variance_type %in% c("90% confidence intervals")] <- "90% CI"
adoption_binary$variance_type[adoption_binary$variance_type %in% c("95% confidence intervals")] <- "95% CI"
adoption_binary$variance_type[adoption_binary$variance_type %in% c("chi-square statistic")] <- "X2"
adoption_binary$variance_type[adoption_binary$variance_type %in% c("p value")] <- "P"
adoption_binary$variance_type[adoption_binary$variance_type %in% c("robust standard error","standard error",
                                                                       "standard deviation")] <- "SE"
adoption_binary$variance_type[adoption_binary$variance_type %in% c("t value", "t ratio")] <- "T"
adoption_binary$variance_type[adoption_binary$variance_type %in% c("wald statistic")] <- "WS"
adoption_binary$variance_type[adoption_binary$variance_type %in% c("z value")] <- "Z"

sort(unique(adoption_binary$variance_type))

table(adoption_binary$variance_type,adoption_binary$model_method_recla )

## Transform 90% conﬁdence intervals to SE(b)
#coefficient_type == "OR"
#model_method_recla == "logit"
CI90_SE <- function (or,ci_l) {  
  result<- (log(or)-log(ci_l))/(1.645)
  return(result)
}

adoption_binary$variance_value[adoption_binary$variance_type %in% c("90% CI")] <-  
  CI90_SE(adoption_binary$coefficient_value[adoption_binary$variance_type %in% c("90% CI")],
          adoption_binary$variance_ci_l[adoption_binary$variance_type %in% c("90% CI")])

## Transform 95% conﬁdence intervals to SE(b)
#coefficient_type == "OR"
#model_method_recla == "logit"
CI95_SE <- function (or,ci_u) {  
  result<- (log(or)+log(ci_u))/(1.96)
  return(result)
}

adoption_binary$variance_value[adoption_binary$variance_type %in% c("95% CI")] <-  
  CI95_SE(adoption_binary$coefficient_value[adoption_binary$variance_type %in% c("95% CI")],
          adoption_binary$variance_ci_u[adoption_binary$variance_type %in% c("95% CI")])

## Transform OR to B
#coefficient_type == "OR"
#model_method_recla == "logit"
#variance_type == c("90% CI", "95% CI")
OR_CI90_95_B <- function (or) {  
  result<- log(or)
  return(result)
}

adoption_binary$coefficient_value[adoption_binary$variance_type %in% c("90% CI", "95% CI")] <-  
  OR_CI90_95_B(adoption_binary$coefficient_value[adoption_binary$variance_type %in% c("90% CI","95% CI")])

adoption_binary$coefficient_type[adoption_binary$variance_type %in% c("90% CI","95% CI")] <- "B"

adoption_binary$variance_type[adoption_binary$variance_type %in% c("90% CI","95% CI")] <- "SE"

sort(unique(adoption_binary$variance_type))

## Combine coefficient type with variance metric
adoption_binary$coefficient_variance_type<- paste(adoption_binary$coefficient_type, adoption_binary$variance_type,sep = "_")
sort(unique(adoption_binary$coefficient_variance_type))

# Combine model type, coefficient type and variance metric
adoption_binary$model_coefficient_variance_type<-paste(adoption_binary$model_method_recla, adoption_binary$coefficient_variance_type,sep = "_")

sort(unique(adoption_binary$model_coefficient_variance_type))
table(adoption_binary$coefficient_variance_type,adoption_binary$model_method_recla)

## ATTENTION: I need to contact the authors of these articles to ask for SE or t-z value
contact_authors_1<- adoption_binary%>%
  filter(is.na(variance_value))%>%
  filter(y_metric_recla=="diversity adoption (1=yes, 0=no)")

length(unique(contact_authors_1$study_id)) #21 articles
sort(unique(contact_authors_1$study_id))
sort(unique(contact_authors_1$y_metric_recla))

# IMPORTANT:  WE CONTACTED THE AUTHORS OF THE STUDIES THAT DID NOT PROVIDE THE EXACT P VALUE OR SE, IF THEY DID NOT REPLY
# Replace P = NA for 0.3, as this is the midpoint between 0.10 and 0.5 (Greenberg et al. 2003)
# Greenberg et al. (2003). A meta-analysis of government-sponsored training programs. use 0.3
adoption_binary$variance_value<- ifelse(adoption_binary$variance_type %in% "P" &
                                                is.na(adoption_binary$variance_value),0.3,adoption_binary$variance_value)

# Replace SE == 0 by 0.0001
adoption_binary$variance_value<- ifelse(adoption_binary$variance_type %in% "SE" &
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

length(unique(contact_authors_2$study_id)) #1 articles
sort(unique(contact_authors_2$study_id)) #1021
table(contact_authors_2$coefficient_variance_type,contact_authors_2$model_method_recla )

######## Remove the rows with t_value_pcc == NA
adoption_binary<-adoption_binary%>%
  filter(!is.na(t_value_pcc))

  
length(sort(unique(adoption_binary$study_id))) # Number of articles 154
table(adoption_binary$y_metric_recla) #Number of rows 3918
sort(unique(adoption_binary$country)) #Countries 44
length(sort(unique(adoption_binary$x_metric_recla))) # Unique factors 208
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
length(sort(unique(check$study_id))) # Number of articles 15
sort(unique(check$country)) #Countries 11
sort(unique(check$study_id))
#[1]  190  348  638  674  709  895 1124 1297 1328 1382 1435 1881 2005 2031 2177

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
length(unique(adoption_binary$study_id[!is.na(adoption_binary$b_logOR)])) #139
length(unique(adoption_binary$x_metric_recla)) #208 factors
sort(unique(adoption_binary$study_id))


factors_articles_count <- adoption_binary %>%
  dplyr::group_by(x_metric_recla,x_metric_unit_recla) %>%
  dplyr::summarise(n_articles = n_distinct(study_id))%>%
  ungroup()
head(factors_articles_count)
names(adoption_binary)

write.csv(factors_articles_count, "data/binary_adoption_factors_articles.csv", row.names=FALSE)

length(unique(adoption_binary$study_id))

#### Filter only the most frequently analysed factors with comparable units ------
adoption_binary_clean<-adoption_binary%>%
  filter(
    # Biophysical context
    x_metric_recla=="precipitation"|
    x_metric_recla=="soil depth"|  
    x_metric_recla=="soil fertility"|
    x_metric_recla=="soil slope"|
    x_metric_recla=="temperature"|
    
    # Farmers' attitudes
      x_metric_recla=="hh risk attitude"| 
      x_metric_recla=="attitude toward SFP or DFP"|
      x_metric_recla=="environmental attitude"|
      x_metric_recla=="productivist attitude"|
      x_metric_recla=="awareness of climate change" |
      x_metric_recla== "awareness of SFP or DFS"| 
      x_metric_recla=="hh perceive benefits of SFP or DFS"|
      x_metric_recla=="perceived limitations to implement SFT or DFS"| 
      x_metric_recla=="production constraints"| 

      # Financial capital
  x_metric_recla=="access to non-farm income"|
  x_metric_recla=="h income"|
  x_metric_recla=="h non-farm income"| 
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
  x_metric_recla=="hh communicate with other farmers"|
  x_metric_recla=="trust in extension services"|
  

## Political and institutional context
  #Land tenure
  x_metric_recla=="land tenure security"| 
  x_metric_recla=="land tenure status"| 
  
  #Knowledge access
  x_metric_recla== "access to agricultural extension"|
  x_metric_recla=="access to agricultural information"|
  x_metric_recla=="access to agricultural training"|
  x_metric_recla=="agricultural extension frequency"|
  
  #Risk management strategies
  x_metric_recla=="receive incentive for conservation"|
  x_metric_recla=="access to credit"|  
  x_metric_recla=="access to credit is a constraint"  |
  
  #Farm management characteristics
  x_metric_recla=="fertilizer use"
   ) 


length(sort(unique(adoption_binary_clean$study_id))) #154
sort(unique(adoption_binary_clean$limitation_of_use_obs))
sort(unique(adoption_binary_clean$country[is.na(adoption_binary_clean$m_un_subregion)])) #1
sort(unique(adoption_binary_clean$country)) #44
sort(unique(adoption_binary_clean$m_un_region)) #5 regions
sort(unique(adoption_binary_clean$m_un_subregion)) #14 subregions
sort(unique(adoption_binary_clean$country[adoption_binary_clean$m_un_subregion %in% c("Central America")]))


####### Factors classification -------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)


####### CHECK NO LIMITATION DATA -------
factors<-adoption_binary_clean%>%
  left_join(factors_assessed, by="factor_metric")%>%
  filter(limitation_of_use_obs== "no limitation")%>%
  group_by(factor_category, factor_subcategory,factor_metric ) %>%
  dplyr::summarise(n_articles = n_distinct(study_id))%>%
  ungroup()
  
sort(unique(factors$factor_category))

pcc_adoption_binary_clean<- adoption_binary_clean%>%
  filter(limitation_of_use_obs== "no limitation")

names(pcc_adoption_binary_clean)
sort(unique(pcc_adoption_binary_clean$m_av_year_assessment)) # 1988 - 2023
sort(unique(pcc_adoption_binary_clean$m_sampling_unit)) # 

length(unique(pcc_adoption_binary_clean$study_id)) #154 studies for PCC analysis
sort(unique(pcc_adoption_binary_clean$study_id)) 
length(unique(pcc_adoption_binary_clean$m_dp_recla)) #10 practices
length(unique(pcc_adoption_binary_clean$x_metric_recla)) #52
sort(unique(pcc_adoption_binary_clean$x_metric_recla)) #51
sort(unique(pcc_adoption_binary_clean$country)) #44
sort(unique(pcc_adoption_binary_clean$limitation_of_use_obs)) 

### Moderators ---
names(pcc_adoption_binary_clean)


write.csv(pcc_adoption_binary_clean,"data/binary_adoption_clean_data.csv", row.names=FALSE)
