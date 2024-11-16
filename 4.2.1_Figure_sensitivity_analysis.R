library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(forcats)

data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_metric_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$logor_unit,")", sep="")


#### PCC data ----
pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_category,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

pcc_data<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))

#### Overall results
#Two-level
pcc_2level<-read.csv("data/pcc_data_2levels.csv",
                     header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_category,pcc_factor_unit) %>%
  dplyr::summarise(n_studies = n_distinct(study_id))

overall_2level_results<-read.csv("results/overall_results_2levels.csv",
                                 header = TRUE, sep = ",")%>%
  left_join(pcc_2level,by="pcc_factor_unit")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_studies",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")

sort(unique(overall_2level_results$pcc_factor_unit))

#Three-level
overall_3level_results<-read.csv("results/overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_studies",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")


sort(unique(overall_3level_results$pcc_factor_unit))

overal_results<- overall_3level_results%>%
  rbind(overall_2level_results)%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  arrange(factor_category,desc(pcc.beta))%>%
  mutate_at(vars("n_ES","n_studies"),as.numeric)%>%
  mutate(significance2 = if_else(pcc.beta >0 & pval <=0.05, "significant_positive",
                                 if_else(pcc.beta <0 & pval <=0.05, "significant_negative",
                                         if_else(pcc.beta>0&pval>0.05,"no_significant_positive",
                                                 "no_significant_negative"))))%>%
  mutate(pcc.ci.lb_l = ifelse(pcc.ci.lb < -0.27, -0.27, NA),
         pcc.ci.ub_l = ifelse(pcc.ci.ub > 0.75, 0.75, NA))%>%
  
  mutate(pcc.ci.ub_l1= ifelse(pcc_factor_unit=="Soil slope (Steep)", pcc.ci.ub,
                              ifelse(pcc_factor_unit=="Perceived benefit from practice (Soil erosion reduction)",pcc.ci.ub,
                                     ifelse(pcc_factor_unit=="Plot size (Plot size)",pcc.ci.ub,
                                            ifelse(pcc_factor_unit=="Access to irrigation (Access to irrigation)",pcc.ci.ub,
                                                   NA)))))%>%
  mutate(pcc.ci.lb_l1= ifelse(pcc_factor_unit=="Plot size (Plot size)", pcc.ci.lb,
                              if_else(pcc_factor_unit=="Land tenure security (Secure tenure)", pcc.ci.lb,
                                      NA)))%>%
  mutate(factor_category= if_else(factor_category=="Financial risk-mechanisms","Political_1",
                                   if_else(factor_category=="Knowledge access","Political_2",
                                           if_else(factor_category=="Land tenure","Political_3",
                                                   factor_category))))%>%
  #mutate(significance1= if_else(pval>0.05&pval<=0.1,"†",""))%>%
  mutate(pcc_factor_unit2= seq(71, 1 ))%>%
  mutate(label= paste("(",n_studies,"|",n_ES,")",sep=""))

sort(unique(overal_results$pcc_factor_unit))
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil depth (Shallow)"] <- 71
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil depth (Moderate)"] <- 70
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil depth (Deep)"] <- 69
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil fertility (High)" ] <- 68
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil fertility (Moderate)"  ] <- 67
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil fertility (Poor)"] <- 66
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil slope (Moderate)" ] <- 65
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Soil slope (Steep)"] <- 64
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Soil slope (Flat)" ] <- 63
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Climate (Precipitation)"  ] <- 62
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Climate (Temperature)"  ] <- 61

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Fertilizer use (Chemical)"   ] <- 60
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Fertilizer use (Organic or manure)"  ] <- 59

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Awareness (Practice)"  ] <- 58
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Awareness (Climate change)"  ] <- 57
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived production constraint (Soil fertility)" ] <- 56
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived production constraint (Drought)"  ] <- 55
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived production constraint (Soil erosion)" ] <- 54
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived production constraint (Pest)"  ] <- 53

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Environmental attitude (Environmental attitude)"  ] <- 52
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Productivist attitude (Productivist attitude)"  ] <- 51
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Positive attitude toward practice (Positive attitude toward practice)"   ] <- 50
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived benefit from practice (Environmental)"  ] <- 49
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived benefit from practice (Financial)"  ] <- 48
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Perceived benefit from practice (Production)"   ] <- 47
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived benefit from practice (Soil erosion reduction)"   ] <- 46
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived benefit from practice (Soil fertility)"   ] <- 45
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Perceived limitation to implement sustainable practices (Financial)"   ] <- 44
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Attitude to risk (Risk-aversion)" ] <- 43

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Non-farm income (Amount)"  ] <- 42
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Non-farm income (Availability)"  ] <- 41
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "On-farm income (On-farm income)"  ] <- 40
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Total income (Total income)"    ] <- 39
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Livestock (Units)"  ] <- 38
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Livestock (Ownership)"   ] <- 37


overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Household head (Farming experience)" ] <- 36
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Household head (Education)"   ] <- 35
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Household head (Literacy)"    ] <- 34
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Household head (Age)"    ] <- 33
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Household head (Married)"   ] <- 32
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Household head (Male)"      ] <- 31
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Household (Number of adults)"    ] <- 30
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Household (Number of people)"    ] <- 29
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Hired labour (Number of workers)"    ] <- 28
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Hired labour (Presence)"   ] <- 27
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Farm labour (Number of workers)"   ] <- 26

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Plot size (Plot size)"    ] <- 25
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Farm size (Farm size)"     ] <- 24
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Number of plots (Number of plots)"    ] <- 23
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Distance to market (General market)" ] <- 22
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Distance to market (Output market)"    ] <- 21
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Distance to market (Input market)"   ] <- 20
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Distance to road (Distance to road)"   ] <- 19
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Distance to farm-house (Distance to farm-house)"   ] <- 18
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Access to irrigation (Access to irrigation)"    ] <- 17

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Receive incentive for conservation (Receive incentive for conservation)"    ] <- 16
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Access to credit is a constraint (Access to credit is a constraint)"    ] <- 15
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Access to credit (Access to credit)"    ] <- 14

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Extension services (Frequency)"    ] <- 13
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Extension services (Access)"    ] <- 12
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Access to information (Access to information)"   ] <- 11
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Access to training (Access to training)"] <- 10

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Land tenure security (Secure tenure)"  ] <- 9
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Land tenure status (Land ownership)" ] <- 8
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Land tenure status (Owned land area)" ] <- 7
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Land tenure status (Rented land)"    ] <- 6
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Land tenure status (Rented land area)"  ] <- 5

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Association membership (Association membership)"   ] <- 4
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Communicate with other farmers (Communicate with other farmers)"   ] <- 3
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Relatives and friends (Relatives and friends)"   ] <- 2
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%  "Trust in extension services (Trust in extension services)"   ] <- 1

sort(unique(overal_results$factor_category))
sort(unique(overal_results$significance1))
sort(unique(overal_results$label))

#overal_results$factor_category <- toupper(overal_results$factor_category)

#overal_results$ID <- as.numeric(seq(71, 1, by = -1)) #add a new column with the effect size ID number

#### Log-Odds Ratio data ----
logor_factor_class_unit<-factors_metric_assessed%>%
  select(factor_category,logor_factor_unit)
logor_factor_class_unit<-unique(logor_factor_class_unit)

logor_data<- read.csv("data/logor_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/logor_data_2levels.csv",header = TRUE, sep = ","))

length(unique(logor_data$study_id)) #151
length(unique(logor_data$study_model_id)) #241

#### Overall results
#Two-level
logor_2level<-read.csv("data/logor_data_2levels.csv", header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_category.x,logor_factor_unit) %>%
  dplyr::summarise(n_studies = n_distinct(study_id))

logor_overall_2level_results<-read.csv("results/logor_overall_results_2levels.csv",
                                       header = TRUE, sep = ",")%>%
  left_join(logor_2level,by="logor_factor_unit")%>%
  select("logor_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_studies",
         "or.beta","or.ci.lb","or.ci.ub")

sort(unique(logor_overall_2level_results$logor_factor_unit))

#Three-level
logor_overall_3level_results<-read.csv("results/logor_overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("logor_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_studies",
         "or.beta","or.ci.lb","or.ci.ub")

sort(unique(logor_overall_3level_results$logor_factor_unit))

logor_overal_results<- logor_overall_3level_results%>%
  rbind(logor_overall_2level_results)%>%
  left_join(logor_factor_class_unit, by="logor_factor_unit")%>%
  arrange(factor_category,desc(or.beta))%>%
  mutate_at(vars("n_ES","n_studies"),as.numeric)%>%
  mutate(significance2 = if_else(or.beta >0 & pval <=0.05, "significant_positive",
                                 if_else(or.beta <0 & pval <=0.05, "significant_negative",
                                         if_else(or.beta>0&pval>0.05,"no_significant_positive",
                                                 "no_significant_negative"))))%>%
  mutate(or.ci.lb_l = ifelse(or.ci.ub > 3, or.ci.lb, NA),
         or.ci.ub_l = ifelse(or.ci.ub > 3.5, 3.5, NA))%>%
  mutate(or.beta_l= ifelse(logor_factor_unit=="Environmental attitude (1= environmental attitude, 0= others)", 3.3,
                              ifelse(logor_factor_unit=="Positive attitude toward practice (1= positive attitude, 0= others)",3.3,
                                     ifelse(logor_factor_unit== "Awareness (1= practice awareness, 0= others)",3.3,
                                     NA))))%>%
  mutate(factor_category= if_else(factor_category=="Financial risk-mechanisms","Political_1",
                                   if_else(factor_category=="Knowledge access","Political_2",
                                           if_else(factor_category=="Land tenure","Political_3",
                                                   factor_category))))%>%
  mutate(logor_factor_unit2= seq(76, 1 ))%>%
  mutate(label= paste("(",n_studies,"|",n_ES,")",sep=""),
         label2= ifelse(or.ci.ub > 3.5, label, NA),
         significance3= ifelse(or.ci.ub > 3.5, significance, NA))%>%
  mutate(logor_factor_unit3= if_else(factor_category=="Biophysical context"|
                                       factor_category=="Farmers behaviour"|
                                       factor_category=="Political_1"|
                                       factor_category=="Political_2"|
                                       factor_category=="Social capital","",logor_factor_unit ))

sort(logor_overal_results$logor_factor_unit)

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil depth (1= shallow depth, 0= others)"] <- 76
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil depth (1= moderate depth, 0= others)" ] <- 75
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil depth (1= deep depth, 0= others)"] <- 74

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil fertility (1= high fertility, 0= others)" ] <- 73
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil fertility (1= moderate fertility, 0= others)"   ] <- 72
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil fertility (1= poor fertility, 0= others)" ] <- 71

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil slope (1= moderate slope, 0= others)"  ] <- 70
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Soil slope (1= steep slope, 0= others)" ] <- 69                                                  
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Soil slope (1= flat slope, 0= others)" ] <- 68

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Climate (mm/year)"  ] <- 67
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Climate (Celsius)"  ] <- 66

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Awareness (1= practice awareness, 0= others)"  ] <- 65
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Awareness (1= climate change awereness, 0= others)"   ] <- 64

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Perceived production constraint (1= soil fertility constraints, 0= others)"] <- 63
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Perceived production constraint (1= drought constraints, 0= others)"  ] <- 62
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Perceived production constraint (1= pest constraints, 0= others)" ] <- 61
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Environmental attitude (1= environmental attitude, 0= others)"] <- 60
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Positive attitude toward practice (1= positive attitude, 0= others)"] <- 59

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Perceived benefit from practice (1= environmental benefits, 0= others)"] <- 58
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Perceived benefit from practice (1= financial benefits, 0= others)"] <- 57
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Perceived benefit from practice (1= production benefits, 0= others)"] <- 56
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Perceived benefit from practice (1= soil erosion reduction benefits, 0= others)"] <- 55
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Perceived benefit from practice (1= soil fertility benefits, 0= others)"] <- 54
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Perceived limitation to implement sustainable practices (1= financial limitation, 0= others)"] <- 53
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Attitude to risk (1= risk-averse, 0= others)"] <- 52


logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Non-farm income (USD)" ] <- 51
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Non-farm income (1= yes, 0= no)" ] <- 50
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "On-farm income (percentage)" ] <- 49
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "On-farm income (USD)"] <- 48
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Total income (USD)"] <- 47
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Livestock (TLU)"  ] <- 46
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Livestock (1= owned, 0= no)"  ] <- 45

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Household head (Farming experience (years))" ] <- 44
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Household head (Education (years))"] <- 43
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Household head (1= literate, 0= illiterate)"  ] <- 42
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Household head (Age (years))" ] <- 41
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Household head (1= married, 0= others)"  ] <- 40
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Household head (1= male, 0= female)" ] <- 39
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Household (Number of adults)" ] <- 38
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Household (number of people)"] <- 37
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Hired labour (number of people)" ] <-36
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Hired labour (1= hired labour, 0= no)"] <- 35
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Farm labour (number of people)"] <- 34
  

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Plot size (ha)"]<-33
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Farm size (ha)"]<-32
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Number of plots (number of plots)"]<-31


logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Distance to market (general market (km))" ]<-30
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Distance to market (general market (minutes))"]<-29
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Distance to market (output market (minutes))"]<-28
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Distance to market (Input market (km))"]<-27
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Distance to market (Input market (minutes))"]<-26
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to road (km)"]<-25
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to road (minutes)"]<-24
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Distance to farm-house (km)" ]<-23
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%  "Distance to farm-house (minutes)" ]<-22
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Access to irrigation (1= yes, 0= others)"]<-21

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Receive incentive for conservation (1= receive incentive, 0= no)"]<-18
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Access to credit (1= access to credit, 0= no)"]<-17
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Access to credit is a constraint (1= access to credit is a constraint, 0= no)"]<-16

   

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Extension services (number of contacts)" ]<-15
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Extension services (1= access to extension, 0= no)"]<-14
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Access to information (1= access to training, 0= no)" ]<-13
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Access to training (1= access to training, 0= no)"]<-12

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Land tenure security (1= secure land tenure, 0= others)" ]<-11
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Land tenure status (1= landownership, 0= others)"]<-10
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Land tenure status (ha of owned land)" ]<-9
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Land tenure status (percentage of owned land)" ]<-8
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Land tenure status (1= rented land, 0= others)" ]<-7
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Land tenure status (ha of rented land)" ]<-6
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Land tenure status (percentage of rented land)" ]<-5

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Association membership (1= yes, 0= no)"  ]<-4
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Communicate with other farmers (1= yes, 0= no)"]<-3
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Relatives and friends (number)"]<-2
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Trust in extension services (1= yes, 0= others)"  ]<-1

sort(unique(logor_overal_results$factor_category))
sort(unique(logor_overal_results$significance1))


#logor_overal_results$ID <- as.numeric(seq(72, 1, by = -1)) #add a new column with the effect size ID number

########################################################################################################
############# OVERALL RESULTS ONLY  ########################################################################################################
########################################################################################################
## Overall results for the most studied factors
fills <- c("#f0c602","#F09319", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")


overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = fills),
  text_y = elem_list_text(size= 1,colour= fills,angle = 90),
  by_layer_y = FALSE
)

overall_distribution_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = "white"),
  text_y = elem_list_text(size= 0.1,colour= "white",angle = 90),
  by_layer_y = FALSE
)

theme_overall<-theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(color="black",size=13, family = "sans", face = "bold",vjust = -1),
  axis.text.x =element_text(color="black",size=12, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.background = element_blank(),
  panel.grid.major  = element_line(color = "grey85",size = 0.6),
  axis.line = element_line(colour = "black"))

# Compare PCC results vs Log-OR results
overall_effect<-
  ggplot(overal_results,
            aes(y=reorder(pcc_factor_unit, pcc_factor_unit2),x=pcc.beta,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,
             colour = factor(factor_category) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_text(aes(label=label, x=pcc.ci.ub+0.08, group=pcc_factor_unit,fontface = "bold"), 
            vjust=0.5, hjust=-0.005,size=4, 
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l1),show.legend = F,size=1)+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l1),show.legend = F,size=1)+
  scale_colour_manual(values = fills)+
  facet_grid2(vars(factor_category),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.50,-0.25,0,0.25,0.50,0.75),
                     labels = c("-0.50","-0.25","0","0.25","0.50","0.75"))+
  geom_text(aes(label=label, x=pcc.ci.ub+0.08, group=pcc_factor_unit,fontface = "bold"), 
            vjust=0.5, hjust=-0.005,size=4, 
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=9, family = "sans"))
overall_effect
#20*10 portrait

logor_overall_strips<- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = "white"),
  text_y = elem_list_text(size= 1,colour= "white",angle = 90),
  by_layer_y = FALSE
)


sort(unique(logor_overal_results$factor_category))
logor_overall_effect<-
ggplot(logor_overal_results,
     aes(y=reorder(logor_factor_unit, logor_factor_unit2),x=or.beta,
           xmin=or.ci.lb, xmax=or.ci.ub,
           colour = factor(factor_category) ))+
  geom_vline(xintercept=1, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=or.ci.ub+0.01, group=logor_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_text(aes(label=label, x=or.ci.ub+0.2, group=logor_factor_unit,fontface = "bold"), 
            vjust=0.35, hjust=-0.005,size=4, 
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_segment(aes(y = reorder(logor_factor_unit, logor_factor_unit2),
                   yend = reorder(logor_factor_unit, logor_factor_unit2),
                   x=or.beta, xend = or.ci.ub_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(logor_factor_unit, logor_factor_unit2),
                              yend = reorder(logor_factor_unit, logor_factor_unit2),
                              x=or.beta, xend = or.ci.lb),show.legend = F,size=1)+
  geom_point(aes(y=reorder(logor_factor_unit, logor_factor_unit2),x=or.beta_l),
                 size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_segment(aes(y = reorder(logor_factor_unit, logor_factor_unit2),
                   yend = reorder(logor_factor_unit, logor_factor_unit2),
                   x=or.beta_l, xend = or.ci.ub_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(logor_factor_unit, logor_factor_unit2),
                   yend = reorder(logor_factor_unit, logor_factor_unit2),
                   x=or.beta_l, xend = or.ci.lb),show.legend = F,size=1)+
  
  geom_text(aes(label=significance3, x=or.ci.ub_l+0.01, group=logor_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
  geom_text(aes(label=label2, x=or.ci.ub_l+0.25, group=logor_factor_unit,fontface = "bold"), 
            vjust=0.35, hjust=-0.005,size=4, 
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
scale_colour_manual(values = fills)+
  facet_grid2(vars(factor_category),
              scales= "free", space='free_y', switch = "y",
              strip = logor_overall_strips)+
  scale_x_continuous(limit = c(0,3.8),expand = c(0.01,0.16),
             breaks = c(0, 0.5,1,1.5,2,2.5,3,3.5),
            labels = c("0","0.5","1","1.5","2","2.5","3","3.5"))+
  xlab("")+
  scale_y_discrete(position = "right")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0), "cm"),
        axis.text.y =element_text(color="black",size=9, family = "sans")
        )
logor_overall_effect

#20*10 portrait
sensitivity.plot<-ggarrange(overall_effect,logor_overall_effect,ncol = 2,widths = c(1, 0.5))

sensitivity.plot
