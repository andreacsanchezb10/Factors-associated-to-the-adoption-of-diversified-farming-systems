library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(forcats)

factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$logor_unit,")", sep="")

#### PCC data ----
pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

pcc_data<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))

#### Overall results
#Two-level
pcc_2level<-read.csv("data/pcc_data_2levels.csv",
                     header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_sub_class.x,pcc_factor_unit) %>%
  dplyr::summarise(n_articles = n_distinct(article_id))

overall_2level_results<-read.csv("results/overall_results_2levels.csv",
                                 header = TRUE, sep = ",")%>%
  left_join(pcc_2level,by="pcc_factor_unit")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_articles",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")%>%
  filter(pcc_factor_unit!="Attitude toward practice (positive continuous)" )

sort(unique(overall_2level_results$pcc_factor_unit))


#Three-level
overall_3level_results<-read.csv("results/overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_articles",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")


sort(unique(overall_3level_results$pcc_factor_unit))

overal_results<- overall_3level_results%>%
  rbind(overall_2level_results)%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  arrange(factor_sub_class,desc(pcc.beta))%>%
  mutate_at(vars("n_ES","n_articles"),as.numeric)%>%
  mutate(significance2 = if_else(pcc.beta >0 & pval <=0.05, "significant_positive",
                                 if_else(pcc.beta <0 & pval <=0.05, "significant_negative",
                                         if_else(pcc.beta>0&pval>0.05,"no_significant_positive",
                                                 "no_significant_negative"))))%>%
  mutate(pcc.ci.lb_l = ifelse(pcc.ci.lb < -0.27, -0.27, NA),
         pcc.ci.ub_l = ifelse(pcc.ci.ub > 0.75, 0.75, NA))%>%
  mutate(pcc.ci.ub_l1= ifelse(pcc_factor_unit=="Steep slope (1= yes, 0= others)", pcc.ci.ub,
                              ifelse(pcc_factor_unit=="Perceived erosion reduction benefit (1= yes, 0= others)",pcc.ci.ub,
                                     ifelse(pcc_factor_unit=="Plot size (continuous)",pcc.ci.ub,
                                            ifelse(pcc_factor_unit=="Access to irrigation (1= yes, 0= others)",pcc.ci.ub,NA)))))%>%
  mutate(pcc.ci.lb_l1= ifelse(pcc_factor_unit=="Plot size (continuous)", pcc.ci.lb,NA))%>%
  mutate(factor_sub_class= if_else(factor_sub_class=="Financial risk-mechanisms","Political_1",
                                   if_else(factor_sub_class=="Knowledge access","Political_2",
                                           if_else(factor_sub_class=="Land tenure","Political_3",
                                                   factor_sub_class))))%>%
  #mutate(significance1= if_else(pval>0.05&pval<=0.1,"†",""))%>%
  mutate(pcc_factor_unit2= seq(67, 1 ))%>%
  mutate(label= paste("(",n_articles,"|",n_ES,")",sep=""))

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in% "Deep soil (1= yes, 0= others)"] <- 65
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"High soil fertility (1= yes, 0= others)"] <- 64
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Moderate soil fertility (1= yes, 0= others)"] <-63
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Poor soil fertility (1= yes, 0= others)"] <-62
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Moderate slope (1= yes, 0= others)"] <-61
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Flat slope (1= yes, 0= others)"] <-60
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Steep slope (1= yes, 0= others)"] <-59
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Precipitation (mm/year)"] <-58
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Temperature (Celsius)"] <-57

overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Negative attitude toward practice (1= yes, 0= others)"]<-55
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived environmental benefit (1= yes, 0= others)"]<-54
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived financial benefit (1= yes, 0= others)"]<-53
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived soil fertility benefit (1= yes, 0= others)"]<-52
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived erosion reduction benefit (1= yes, 0= others)"]<-51
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Awareness of practice (1= yes, 0= no)"]<-50
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Awareness of climate change (1= yes, 0= no)"]<-49
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived financial constraint (1= yes, 0= others)"]<-48
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Risk-aversion (1= yes, 0= others)"]<-47
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived soil fertility as production constraint (1= yes, 0= others)"]<-46
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived drought as production constraint (1= yes, 0= others)"]<-45
overal_results$pcc_factor_unit2[overal_results$pcc_factor_unit %in%"Perceived pest as production constraint (1= yes, 0= others)"]<-44

sort(unique(overal_results$factor_sub_class))
sort(unique(overal_results$significance1))
sort(unique(overal_results$label))

#overal_results$factor_sub_class <- toupper(overal_results$factor_sub_class)

overal_results$ID <- as.numeric(seq(67, 1, by = -1)) #add a new column with the effect size ID number

#### Log-Odds Ratio data ----
logor_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,logor_factor_unit)
logor_factor_class_unit<-unique(logor_factor_class_unit)

logor_data<- read.csv("data/logor_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/logor_data_2levels.csv",header = TRUE, sep = ","))

#### Overall results
#Two-level
logor_2level<-read.csv("data/logor_data_2levels.csv",
                       header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_sub_class.x,logor_factor_unit) %>%
  dplyr::summarise(n_articles = n_distinct(article_id))

logor_overall_2level_results<-read.csv("results/logor_overall_results_2levels.csv",
                                       header = TRUE, sep = ",")%>%
  left_join(logor_2level,by="logor_factor_unit")%>%
  select("logor_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_articles",
         "or.beta","or.ci.lb","or.ci.ub")
filter(pcc_factor_unit!="Attitude toward practice (positive continuous)" )

sort(unique(logor_overall_2level_results$logor_factor_unit))

#Three-level
logor_overall_3level_results<-read.csv("results/logor_overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("logor_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_articles",
         "or.beta","or.ci.lb","or.ci.ub")

sort(unique(logor_overall_3level_results$logor_factor_unit))

logor_overal_results<- logor_overall_3level_results%>%
  rbind(logor_overall_2level_results)%>%
  left_join(logor_factor_class_unit, by="logor_factor_unit")%>%
  arrange(factor_sub_class,desc(or.beta))%>%
  mutate_at(vars("n_ES","n_articles"),as.numeric)%>%
  mutate(significance2 = if_else(or.beta >0 & pval <=0.05, "significant_positive",
                                 if_else(or.beta <0 & pval <=0.05, "significant_negative",
                                         if_else(or.beta>0&pval>0.05,"no_significant_positive",
                                                 "no_significant_negative"))))%>%
  mutate(or.ci.lb_l = ifelse(or.ci.ub > 3, or.ci.lb, NA),
         or.ci.ub_l = ifelse(or.ci.ub > 3.5, 3.5, NA))%>%
  mutate(or.beta_l= ifelse(logor_factor_unit=="Positive attitude toward practice (1= yes, 0= others)", 3.3,
                              ifelse(logor_factor_unit=="Awareness of practice (1= yes, 0= no)",3.2,NA)))%>%
  mutate(factor_sub_class= if_else(factor_sub_class=="Financial risk-mechanisms","Political_1",
                                   if_else(factor_sub_class=="Knowledge access","Political_2",
                                           if_else(factor_sub_class=="Land tenure","Political_3",
                                                   factor_sub_class))))%>%
  mutate(logor_factor_unit2= seq(72, 1 ))%>%
  mutate(label= paste("(",n_articles,"|",n_ES,")",sep=""),
         label2= ifelse(or.ci.ub > 3.5, label, NA),
         significance3= ifelse(or.ci.ub > 3.5, significance, NA))%>%
  mutate(logor_factor_unit3= if_else(factor_sub_class=="Biophysical context"|
                                       factor_sub_class=="Farmers behaviour"|
                                       factor_sub_class=="Political_1"|
                                       factor_sub_class=="Political_2"|
                                       factor_sub_class=="Social capital","",logor_factor_unit ))

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in% "Deep soil (1= yes, 0= others)"] <- 70
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"High soil fertility (1= yes, 0= others)"] <- 69
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Moderate soil fertility (1= yes, 0= others)"] <-68
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Poor soil fertility (1= yes, 0= others)"] <-67
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Moderate slope (1= yes, 0= others)"] <-66
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Flat slope (1= yes, 0= others)"] <-65
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Steep slope (1= yes, 0= others)"] <-64
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Precipitation (mm/year)"] <-63
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Temperature (Celsius)"] <-62

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Positive attitude toward practice (1= yes, 0= others)"]<-61
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Negative attitude toward practice (1= yes, 0= others)"]<-59
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived environmental benefit (1= yes, 0= others)"]<-58
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived financial benefit (1= yes, 0= others)"]<-57
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived soil fertility benefit (1= yes, 0= others)"]<-56
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived erosion reduction benefit (1= yes, 0= others)"]<-55
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Awareness of practice (1= yes, 0= no)"]<-54
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Awareness of climate change (1= yes, 0= no)"]<-53
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived financial constraint (1= yes, 0= others)"]<-52
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Risk-aversion (1= yes, 0= others)"]<-51
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived soil fertility as production constraint (1= yes, 0= others)"]<-50
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived drought as production constraint (1= yes, 0= others)"]<-49
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Perceived pest as production constraint (1= yes, 0= others)"]<-48

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Non-farm income (USD)"]<-47
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"On-farm income (percentage)"]<-46
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"On-farm income (USD)"]<-45
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Total income (USD)"]<-44
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Access to non-farm income (1= yes, 0= no)"]<-43
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Livestock units (TLU)"]<-42
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Livestock owned (1= yes, 0= no)"]<-41

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Farming experience (years)"]<-40
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Education (years)"]<-39
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Adults in household (number of people)"]<-38
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Household size (number of people)"]<-37
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Literate farmer (1= literate, 0= illiterate)"]<-36
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Primary education (1= yes, 0= no)"]<-35
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Hired labour (number of people)"]<-34
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Post-secondary education (1= yes, 0= no)"]<-33
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Age (years)"]<-32
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Non-hired labour (number of people)"]<-31
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Married (1= yes, 0= others)"]<-30
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Household is native (1= yes, 0= no)"]<-29
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"High school education (1= yes, 0= no)"]<-28
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Gender (1= male, 0= female)"]<-27
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Hired labour (1= yes, 0= no)"]<-26

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Plot size (ha)"]<-25
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Farm size (ha)"]<-24
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Number of plots (number)"]<-23

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to market (km)"]<-22
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to market (minutes)"]<-21
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to output market (minutes)"]<-20
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to road (km)"]<-19
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to road (minutes)"]<-18
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to input market (km)"]<-17
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to input market (minutes)"]<-16
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to farm-house (km)"]<-15
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Distance to farm-house (minutes)"]<-14
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Access to irrigation (1= yes, 0= others)"]<-13



logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Association member (1= yes, 0= no)"]<-4
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Communicate with other farmers (1= yes, 0= no)"]<-3

logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Extension frequency (number of contacts)"]<-23
logor_overal_results$logor_factor_unit2[logor_overal_results$logor_factor_unit %in%"Access to training (1= yes, 0= no)"]<-20

sort(unique(logor_overal_results$factor_sub_class))
sort(unique(logor_overal_results$significance1))


#logor_overal_results$ID <- as.numeric(seq(72, 1, by = -1)) #add a new column with the effect size ID number

########################################################################################################
############# OVERALL RESULTS ONLY  ########################################################################################################
########################################################################################################
## Overall results for the most studied factors
fills <- c("#f0c602", "#ea6044","#6a57b8",  "#87CEEB",  "#92c46d", "#92c46d","#92c46d","#297d7d")

fills <- c("#d896ff", "#496491")

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
  ggplot(subset(overal_results, factor_sub_class %in%
                  # c("Biophysical context","Farmers behaviour",
                  #"Human capital","Natural capital","Political_1",
                  #"Political_2","Political_3" ,"Social capital"   )),
         c("Financial capital","Physical capital")),
         
         aes(y=reorder(pcc_factor_unit, pcc_factor_unit2),x=pcc.beta,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,
             colour = factor(factor_sub_class) ))+
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
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.50,-0.25,0,0.25,0.50,0.75),
                     labels = c("-0.50","-0.25","0","0.25","0.50","0.75"))+
  geom_text(aes(label=label, x=pcc.ci.ub+0.08, group=pcc_factor_unit,fontface = "bold"), 
            vjust=0.5, hjust=-0.005,size=4, 
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
  xlab("")+
  #xlab(bquote(bold("Partial correlation coefficient (" *italic(r)[p]*")")))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=3.5), "cm"),
        #axis.text.x =element_blank(),
        #axis.line.x = element_blank())
        axis.text.y =element_text(color="black",size=12, family = "sans"))
overall_effect
#1000 1600
#1000 500

logor_overall_strips<- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = "white"),
  text_y = elem_list_text(size= 1,colour= "white",angle = 90),
  by_layer_y = FALSE
)


sort(unique(logor_overal_results$factor_sub_class))
logor_overall_effect<-
ggplot(subset(logor_overal_results, factor_sub_class %in%  
                #  c("Biophysical context","Farmers behaviour",
                #  "Human capital" ,"Natural capital","Political_1",
              #  "Political_2","Political_3","Social capital")),
                c("Financial capital","Physical capital")),
       aes(y=reorder(logor_factor_unit, logor_factor_unit2),x=or.beta,
           xmin=or.ci.lb, xmax=or.ci.ub,
           colour = factor(factor_sub_class) ))+
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
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",
              strip = logor_overall_strips)+
  scale_x_continuous(limit = c(0,3.8),expand = c(0.05,0.16),
             breaks = c(0, 0.5,1,1.5,2,2.5,3,3.5),
            labels = c("0","0.5","1","1.5","2","2.5","3","3.5"))+
  xlab("")+
  scale_y_discrete(position = "right")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0), "cm"),
       #axis.text.y =element_blank(),
       # axis.text.x =element_blank(),
       #axis.line.x = element_blank(),
       #axis.ticks = element_blank(),
       
        axis.text.y =element_text(color="black",size=12, family = "sans")
        )
logor_overall_effect

#1050 1600
#1050 500
sensitivity.plot<-ggarrange(overall_effect,logor_overall_effect,ncol = 2,widths = c(1, 0.5))

sensitivity.plot
