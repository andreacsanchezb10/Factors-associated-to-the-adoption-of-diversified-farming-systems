
#__________MULTIVARIATE REGRESSION MODEL
#https://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin
#library(MuMIn)
library(tibble)
library(stringr)
library(metafor)
library(purrr)
library(glmulti)
library(dplyr)

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
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(m_mean_farm_size_ha= as.numeric(m_mean_farm_size_ha))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_es = n_distinct(ES_ID))%>%
  filter(n_es>9)%>%
  ungroup()
  names(pcc_data_2level)
sort(unique(pcc_data_2level$pcc_factor_unit))    

#Heterogeneity
heterogeneity_2level<- read.csv("results/heterogeneity_2levels.csv",header = TRUE, sep = ",")%>%
  filter(I2>=75) 

sort(unique(heterogeneity_2level$pcc_factor_unit))

#pcc database after removing factors with <10 studies;
#factors with sampling variance > 25% 
m_pcc_data_2level<- pcc_data_2level%>%
  dplyr::left_join(heterogeneity_2level, by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  filter(!is.na(I2))
sort(unique(m_pcc_data_2level$pcc_factor_unit))    


rma.glmulti <- function(formula, data, ...)
  rma(formula, fis.vi, data=data, method="ML", ...)

# Most moderators
unique_units <- unique(m_pcc_data_2level$pcc_factor_unit)
importance_list2 <- list()


for (unit in unique(m_pcc_data_2level$pcc_factor_unit)) {
  
  # Subset the data for the current pcc_factor_unit
  subset_data <- m_pcc_data_2level %>%
    filter(pcc_factor_unit == unit)
  subset_data <- as.data.frame(subset_data)
    # Include specific variables based on pcc_factor_unit
    if (unit == "Plot size (continuous)"||
        unit =="Perceived pest as production constraint (1= yes, 0= no)"||
        unit =="Temperature (Celsius)"){
      variables_to_include <- c( "m_intervention_recla2","m_region","m_sub_region", "m_mean_farm_size_ha","m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
    } else if (unit =="Access to credit (1= yes, 0= no)"||
               unit =="Access to irrigation (1= yes, 0= no)"||
               unit== "Access to non-farm income (1= yes, 0= no)"||
               unit=="Association member (1= yes, 0= no)"||
               unit=="Communicate with other farmers (1= yes, 0= no)"||
               unit =="Extension frequency (number of contacts)"||
               unit =="Farm size (continuous)"||
               unit =="Gender (1= male, 0= female)"||
               unit =="Household is native (1= yes, 0= no)"){
      variables_to_include <- c("m_intervention_recla2","m_region","m_sub_region","m_mean_farm_size_ha","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
    } else if (unit =="Farming experience (continuous)"){
      variables_to_include <- c("m_intervention_recla2","m_region","m_sub_region","m_mean_farm_size_ha","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
    }else if (unit== "Relatives and friends (continuous)"||
              unit=="Married (1= yes, 0= others)"||
              unit== "High soil fertility (1= yes, 0= others)"){
      variables_to_include <- c("m_intervention_recla2","m_region","m_sub_region","m_mean_farm_size_ha","m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
      }else if (unit=="Moderate soil fertility (1= yes, 0= others)"){
      variables_to_include <- c("m_intervention_recla2","m_mean_farm_size_ha","m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method","m_av_year_assessment")
      } else if (unit =="Livestock owned (1= yes, 0= no)"||
                 unit =="Age (continuous)"||
                 unit =="Precipitation (mm/year)"){
        variables_to_include <- c("m_intervention_recla2","m_region","m_sub_region","m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method","m_av_year_assessment")
      } else if (unit =="Access to training (1= yes, 0= no)"){
        variables_to_include <- c("m_intervention_recla2","m_region","m_sub_region","m_education_years","n_predictors_num","m_sampling_unit","m_type_data","m_model_method")
      } else if (unit =="Receive incentive for conservation (1= yes, 0= no)"){
        variables_to_include <- c("m_intervention_recla2","m_region","m_sub_region","m_mean_farm_size_ha","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
        }  
  # Check if there are enough variables for modeling
    if (length(variables_to_include) > 0) {
      formula_string2 <- paste("fis.yi ~", paste(variables_to_include, collapse = "+"))
      
      # Perform glmulti analysis
      res2 <- glmulti(formula_string2, data=subset_data, 
                      level=1, marginality=TRUE, fitfunction=rma.glmulti,
                      crit="aicc", confsetsize=100, plotty=FALSE)
      
      # Store weightable results in the list
      importance_list2[[unit]] <- table.glmulti(res2, type = "s")
    } else {
      cat("Not enough data points for modeling in unit:", unit, "\n")
    }
    
}

importance_df_2levels <- do.call(rbind, lapply(importance_list2, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(pcc_factor_unit= sub("\\..*", "", pcc_factor_unit))%>%
  filter(Importance>= 0.5)

sort(unique(importance_df_2levels$pcc_factor_unit))

write.csv(importance_df_2levels,"results/moderators_importance_akaike_2levels.csv", row.names=FALSE)

##########################################################################################################################################################
########################  THREE-LEVEL META-ANALYSIS ##########################################################################################################################################################
##########################################################################################################################################################
#Data
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate_at(vars(m_mean_farm_size_ha,n_samples_num,n_predictors_num,m_education_years ), as.numeric)%>%
  group_by( pcc_factor_unit)%>%
  dplyr::mutate(n_es = n_distinct(ES_ID))%>%
  filter(n_es>9)%>%
  ungroup()
  filter(pcc_factor_unit=="Steep slope (1= yes, 0= others)")
  select(m_education_years)
    pcc_factor_unit=="Access to extension (1= yes, 0= no)"|
           pcc_factor_unit=="Access to information (1= yes, 0= no)"|
             pcc_factor_unit=="Adults in household (continuous)"|
           pcc_factor_unit=="Awareness of practice (1= yes, 0= no)"|
           pcc_factor_unit=="Distance to farm-house (continuous)"|
           pcc_factor_unit=="Distance to market (continuous)"|
           pcc_factor_unit=="Education (continuous)"|
           )

sort(unique(pcc_data_3level$pcc_factor_unit))

rma.glmulti1 <- function(formula, data, ...) {
  rma.mv(formula, fis.vi,
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
  if (nrow(subset_data) > length(coef(subset_data)) + 1) {
    
    # Include specific variables based on pcc_factor_unit
    if (unit == "Awareness of practice (1= yes, 0= no)"||
        unit =="Access to extension (1= yes, 0= no)") {
      variables_to_include <- c( "m_intervention_recla2", "m_region","m_sub_region", "m_mean_farm_size_ha","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method","m_av_year_assessment")
      } else if (unit =="Education (continuous)"|
                 unit =="Land tenure security (1= yes, 0= no)") {
      variables_to_include <- c("m_intervention_recla2", "m_region","m_sub_region", "m_mean_farm_size_ha", "m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
      } else if ( unit == "Access to information (1= yes, 0= no)") { 
        variables_to_include <- c("m_intervention_recla2", "m_region", "m_mean_farm_size_ha", "m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
        } else if (unit == "Distance to market (continuous)"|| 
                   unit == "Household size (continuous)" ){
          variables_to_include <- c("m_intervention_recla2", "m_region", "m_sub_region", "m_mean_farm_size_ha","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data","m_model_method")
          }  else if (unit== "On-farm income (continuous)"||
                      unit==  "Non-farm income (continuous)"||
                      unit== "Livestock units (continuous)"||
                      unit=="Adults in household (continuous)"){
            variables_to_include <- c("m_intervention_recla2","m_region","m_sub_region","m_mean_farm_size_ha","m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_model_method")
            } else if ( unit == "Distance to farm-house (continuous)"){
              variables_to_include <- c("m_intervention_recla2", "m_region","m_sub_region", "m_mean_farm_size_ha", "m_education_years","n_predictors_num","m_sampling_unit","m_random_sample","m_type_data")
            } else if ( unit == "Steep slope (1= yes, 0= others)"){
              variables_to_include <- c("m_intervention_recla2", "m_region","m_sub_region", "m_mean_farm_size_ha", "n_predictors_num","m_random_sample","m_type_data")
            } 
    # Check if there are enough variables for modeling
    if (length(variables_to_include) > 0) {
      
      formula_string <- paste("fis.yi ~", paste(variables_to_include, collapse = "+"))
      
      # Perform glmulti analysis
      res1 <- glmulti(formula_string, data=subset_data, 
                      level=1, marginality=TRUE, fitfunction=rma.glmulti1,
                      crit="aicc", confsetsize=100, plotty=FALSE)
      
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
importance_factors_3levels <- do.call(rbind, lapply(importance_list3, as.data.frame))%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(pcc_factor_unit= sub("\\..*", "", pcc_factor_unit))%>%
  filter(Importance>= 0.5)


sort(unique(importance_factors_3levels$pcc_factor_unit))

write.csv(importance_factors_3levels,"results/moderators_importance_akaike_3levels.csv", row.names=FALSE)


##################################################################
## PLOT IMPORTANT MODERATORS
############################################################################
library(reshape2)
library(ggh4x)
library(readxl)

factors_metric_assessed <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
  sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

pcc_data<-read.csv("data/pcc_data.csv",header = TRUE, sep = ",")
names(pcc_data)

importance_factors1<-
  rbind(importance_df_2levels,importance_factors_3levels)%>%
  dplyr::rename("moderator"="Term")%>%
  left_join(pcc_factor_class_unit,by="pcc_factor_unit")%>%
  group_by(factor_sub_class)%>%
  summarise(total = n_distinct(pcc_factor_unit))%>%
  ungroup()%>%
  mutate(total= if_else(factor_sub_class=="Human capital", 8,total))

importance_factors<-
    rbind(importance_df_2levels,importance_factors_3levels)%>%
    dplyr::rename("moderator"="Term")%>%
    left_join(pcc_factor_class_unit,by="pcc_factor_unit")

sort(unique(importance_factors$pcc_factor_unit))
sort(unique(importance_factors$factor_sub_class))

sort(unique(importance_factors$moderator))
importance_factors$moderator[importance_factors$moderator %in% c("m_intervention_recla2")] <- "Diversification\npractice"
importance_factors$moderator[importance_factors$moderator %in% c("m_education_years")] <- "Education\n(years)"
importance_factors$moderator[importance_factors$moderator %in% c("m_mean_farm_size_ha")] <- "Farm size (ha)"
importance_factors$moderator[importance_factors$moderator %in% c("m_av_year_assessment")] <- "Year of\nassessment"
importance_factors$moderator[importance_factors$moderator %in% c("m_model_method")] <- "Model type"
importance_factors$moderator[importance_factors$moderator %in% c("m_random_sample")] <- "Random\nsampling"
importance_factors$moderator[importance_factors$moderator %in% c("m_sampling_unit")] <- "Household\nsampling unit"
importance_factors$moderator[importance_factors$moderator %in% c("m_type_data")] <- "Primary data"
importance_factors$moderator[importance_factors$moderator %in% c("n_predictors_num")] <- "Number of\npredictors"
importance_factors$moderator[importance_factors$moderator %in% c("m_region")] <- "Region"
importance_factors$moderator[importance_factors$moderator %in% c("m_sub_region")] <- "Sub-region"

sort(unique(importance_factors$moderator))
sort(unique(importance_factors$factor_sub_class))

importance_factors2<-as.data.frame(table(importance_factors$factor_sub_class, importance_factors$moderator))%>%
  dplyr::rename("factor_sub_class"="Var1",
                "moderator"= "Var2")%>%
  left_join(importance_factors1, by= "factor_sub_class")%>%
  mutate(percentage= round((Freq/total)*100,0))%>%
  mutate(analysis="AICc")
  

importance_factors2$factor_sub_class[importance_factors2$factor_sub_class %in% c("Land tenure")] <- "Political context_3"
importance_factors2$factor_sub_class[importance_factors2$factor_sub_class %in% c( "Financial risk-mechanisms")] <- "Political context_1"
importance_factors2$factor_sub_class[importance_factors2$factor_sub_class %in% c( "Knowledge access")] <- "Political context_2"
sort(unique(importance_factors2$factor_sub_class))

importance_factors2<- importance_factors2%>%
  group_by(factor_sub_class) %>%
  mutate(dense_rank = dense_rank(-percentage)) %>%
  mutate(Important = ifelse(dense_rank <= 2, "Yes", "No")) %>%
  select(-dense_rank)%>%
  mutate(Important=if_else(percentage==0, "No", Important) )

#Meta-regression results
meta_regression<- read.csv("results/meta_regression.csv",header = TRUE, sep = ",")%>%
  dplyr::select(moderator, factor_sub_class, pcc_factor_unit,QMp)%>%
  filter(moderator!="m_intervention_system_components")%>%
  filter(moderator!="m_endogeneity_correction")%>%
  filter(moderator!="m_exact_variance_value")%>%
  filter(moderator!="m_exposure_correction")%>%
  filter(moderator!="n_samples_num" )%>%
  filter(QMp<=0.05)
meta_regression<-unique(meta_regression)

meta_regression$moderator[meta_regression$moderator %in% c("m_intervention_recla2")] <- "Diversification\npractice"
meta_regression$moderator[meta_regression$moderator %in% c("m_education_years")] <- "Education\n(years)"
meta_regression$moderator[meta_regression$moderator %in% c("m_mean_farm_size_ha")] <- "Farm size (ha)"
meta_regression$moderator[meta_regression$moderator %in% c("m_av_year_assessment")] <- "Year of\nassessment"
meta_regression$moderator[meta_regression$moderator %in% c("m_model_method")] <- "Model type"
meta_regression$moderator[meta_regression$moderator %in% c("m_random_sample")] <- "Random\nsampling"
meta_regression$moderator[meta_regression$moderator %in% c("m_sampling_unit")] <- "Household\nsampling unit"
meta_regression$moderator[meta_regression$moderator %in% c("m_type_data")] <- "Primary data"
meta_regression$moderator[meta_regression$moderator %in% c("n_predictors_num")] <- "Number of\npredictors"
meta_regression$moderator[meta_regression$moderator %in% c("m_region")] <- "Region"
meta_regression$moderator[meta_regression$moderator %in% c("m_sub_region")] <- "Sub-region"

sort(unique(meta_regression$moderator))

meta_regression2<-as.data.frame(table(meta_regression$factor_sub_class, meta_regression$moderator))%>%
  dplyr::rename("factor_sub_class"="Var1",
                "moderator"= "Var2")%>%
  left_join(importance_factors1, by= "factor_sub_class")%>%
  mutate(percentage= round((Freq/total)*100,0))%>%
  mutate(analysis= "F-test")
meta_regression2$factor_sub_class[meta_regression2$factor_sub_class %in% c("Land tenure")] <- "Political context_3"
meta_regression2$factor_sub_class[meta_regression2$factor_sub_class %in% c( "Financial risk-mechanisms")] <- "Political context_1"
meta_regression2$factor_sub_class[meta_regression2$factor_sub_class %in% c( "Knowledge access")] <- "Political context_2"

landtenure<- meta_regression2%>%
  filter(factor_sub_class=="Social capital")%>%
  mutate(factor_sub_class="Political context_3",
         Freq= 0,total = 1,percentage= 0)

naturalcapital<- meta_regression2%>%
  filter(factor_sub_class=="Social capital")%>%
  mutate(factor_sub_class="Natural capital",
         Freq= 0,total = 2,percentage= 0)

meta_regression2<-rbind(meta_regression2, landtenure,naturalcapital)%>%
  group_by(factor_sub_class) %>%
  mutate(dense_rank = dense_rank(-percentage)) %>%
  mutate(Important = ifelse(dense_rank <= 2, "Yes", "No")) %>%
  select(-dense_rank)%>%
  mutate(Important=if_else(percentage==0, "No", Important) )

sort(unique(meta_regression2$factor_sub_class))
sort(unique(meta_regression2$moderator))


importance_factors3<-rbind(importance_factors2,meta_regression2)
sort(unique(importance_factors3$moderator))


fills <- c("#f0c602","#ea6044","#d896ff","#6a57b8","#87CEEB","#496491","#92c46d","#92c46d","#92c46d","#297d7d")

overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(fill = fills),
  text_y = elem_list_text(size= 12,colour= fills,angle = 90,face="bold"),
  text_x = elem_list_text(size= 11,colour= "black",angle = 0,face="bold",
                          label= c("Diversified\npractices")),
  by_layer_y = FALSE
)

importanceplot<- ggplot(data=importance_factors3, aes(y= factor_sub_class, x=analysis)) +
  geom_tile(color = "black",aes(fill=Important))+
  scale_fill_manual(values = c("white","green"))+
  geom_text(data=importance_factors3,aes(label=paste(percentage,"%",sep=""),
                                         x=analysis, y=factor_sub_class), 
            vjust=0.5, hjust=0.5,size=4,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  facet_grid2(vars(factor_sub_class),vars(moderator),
              scales= "free", space='free_y', switch = "y",strip = overall_strips)+
  scale_x_discrete(position = "top") +
  theme(strip.placement.y = "inside",
        strip.placement.x = "outside",
        legend.position = "bottom",
        axis.title = element_blank(),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=3.5), "cm"),
        legend.title =  element_text(color="black",size=14, family = "sans",face="bold"),
        legend.text = element_text(color="black",size=12, family = "sans"),
        axis.text.y =element_blank(),
        axis.text.x =element_text(color="black",size=11, family = "sans",face="bold"),
        panel.background = element_blank(),
        axis.ticks.y=element_blank())

importanceplot  

overall_distribution_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = "white"),
  text_y = elem_list_text(size= 0.1,colour= "white",angle = 90),
  by_layer_y = FALSE
)

total<- ggplot(subset(importance_factors3, analysis=="AICc"), aes(y= factor_sub_class, x=analysis)) +
  geom_tile(color = "black",fill="grey")+
  geom_text(aes(label=total,x=analysis, y=factor_sub_class), 
            vjust=0.5, hjust=0.5,size=4,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",strip = overall_distribution_strips)+
  scale_x_discrete(position = "top", label="Total number\nof factors") +
  theme(strip.placement.y = "inside",
        strip.placement.x = "outside",
        legend.position = "bottom",
        axis.title = element_blank(),
        plot.margin = unit(c(t=1.2,r=1,b=1.8,l=0), "cm"),
        legend.title =  element_text(color="black",size=14, family = "sans",face="bold"),
        legend.text = element_text(color="black",size=12, family = "sans"),
        axis.text.y =element_blank(),
        axis.text.x =element_text(color="black",size=11, family = "sans",face="bold"),
        panel.background = element_blank(),
        axis.ticks.y=element_blank())
total

library(ggpubr)

importance.total<-ggarrange(importanceplot,total,ncol = 2,widths = c(1, 0.10))

importance.total


