##################################################################
## PLOT IMPORTANT MODERATORS
############################################################################
library(reshape2)
library(ggh4x)
library(readxl)

factors_metric_assessed <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/Meta_data_2024.02.15.xlsx",
  sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

pcc_data<-read.csv("data/pcc_data.csv",header = TRUE, sep = ",")
names(pcc_data)

# F-Test moderators
#Meta-regression results
sort(unique(meta_regression$moderator))

meta_regression<- read.csv("results/meta_regression.csv",header = TRUE, sep = ",")%>%
  dplyr::select(moderator, factor_sub_class, pcc_factor_unit,QMp)%>%
  filter(moderator!="m_exact_variance_value")%>%
  filter(moderator!="n_samples" )
meta_regression$factor_sub_class[meta_regression$factor_sub_class %in% c("Land tenure")] <- "Political context"
meta_regression$factor_sub_class[meta_regression$factor_sub_class %in% c( "Financial risk-mechanisms")] <- "Political context"
meta_regression$factor_sub_class[meta_regression$factor_sub_class %in% c( "Knowledge access")] <- "Political context"
meta_regression$moderator[meta_regression$moderator %in% c("m_dp_recla")] <- "Diversification\npractice"
meta_regression$moderator[meta_regression$moderator %in% c("m_education_years")] <- "Education\n(years)"
meta_regression$moderator[meta_regression$moderator %in% c("m_mean_farm_size_ha")] <- "Farm size (ha)"
meta_regression$moderator[meta_regression$moderator %in% c("m_av_year_assessment")] <- "Year of\nassessment"
meta_regression$moderator[meta_regression$moderator %in% c("model_method_recla")] <- "Model type"
meta_regression$moderator[meta_regression$moderator %in% c("m_random_sample")] <- "Random\nsampling"
meta_regression$moderator[meta_regression$moderator %in% c("m_sampling_unit")] <- "Household\nsampling unit"
meta_regression$moderator[meta_regression$moderator %in% c("m_type_data")] <- "Primary data"
meta_regression$moderator[meta_regression$moderator %in% c("n_factors")] <- "Number of\npredictors"
meta_regression$moderator[meta_regression$moderator %in% c("m_un_region")] <- "Region"
meta_regression$moderator[meta_regression$moderator %in% c("m_un_subregion")] <- "Sub-region"
sort(unique(meta_regression$factor_sub_class))

factors<-meta_regression%>%
  group_by(factor_sub_class)%>%
  summarise(total.factor_sub_class = n_distinct(pcc_factor_unit))%>%
  ungroup()%>%
  mutate(total.factor_sub_class= if_else(factor_sub_class=="Human capital", 8,total.factor_sub_class))
sort(unique(factors$factor_sub_class))

factors2<-meta_regression%>%
  select(factor_sub_class,moderator)
factors2<-unique(factors2)
sort(unique(factors2$factor_sub_class))

meta_regression<-meta_regression%>%
  filter(QMp<=0.05)
meta_regression<-unique(meta_regression)

sort(unique(meta_regression$moderator))
sort(unique(meta_regression$factor_sub_class))

meta_regression2<-as.data.frame(table(meta_regression$factor_sub_class, meta_regression$moderator))%>%
  dplyr::rename("factor_sub_class"="Var1",
                "moderator"= "Var2")%>%
  right_join(factors2, by= c("factor_sub_class","moderator"))%>%
  mutate(Freq= if_else(is.na(Freq),0,Freq))%>%
  left_join(factors, by= "factor_sub_class")%>%
  #mutate(factor_sub_class= if_else(Freq==0, "No important",factor_sub_class ))%>%
  #group_by(factor_sub_class,moderator)%>%
  #summarise(Freq= sum(Freq),
  #        total.factor_sub_class= sum(total.factor_sub_class))%>%
  # ungroup()%>%
  mutate(total= 35)

important_ftest<- meta_regression2%>%
  mutate(percentage= round((Freq/total)*100,3))%>%
  filter(percentage!=0)
important_ftest$importance[important_ftest$factor_sub_class %in% c("Biophysical context")] <- 8
important_ftest$importance[important_ftest$factor_sub_class %in% c("Farmers behaviour")] <- 7
important_ftest$importance[important_ftest$factor_sub_class %in% c("Financial capital")] <- 6
important_ftest$importance[important_ftest$factor_sub_class %in% c("Human capital")] <- 5
important_ftest$importance[important_ftest$factor_sub_class %in% c("Physical capital")] <- 3
important_ftest$importance[important_ftest$factor_sub_class %in% c("Political context")] <- 2
important_ftest$importance[important_ftest$factor_sub_class %in% c("Social capital")] <- 1
important_ftest<-important_ftest%>%
  mutate(importance= paste("2important/",importance,sep=""))

sort(unique(important_ftest$factor_sub_class))
  
non_important_ftest<- meta_regression2%>%
  mutate(percentage= round(((total.factor_sub_class-Freq)/total)*100,3))%>%
  filter(percentage!=0)
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Biophysical context")] <- 8
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Farmers behaviour")] <- 7
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Financial capital")] <- 6
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Human capital")] <- 5
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Natural capital")] <- 4
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Physical capital")] <- 3
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Political context")] <- 2
non_important_ftest$importance[non_important_ftest$factor_sub_class %in% c("Social capital")] <- 1
non_important_ftest<-non_important_ftest%>%
  mutate(importance= paste("1non-important/",importance,sep=""))      

important_total_ftest<- rbind(important_ftest,non_important_ftest)%>%
  arrange(desc(importance)) 

#Akaike AICc
moderators_importance_akaike_2levels<- read.csv("results/moderators_importance_akaike_2levels.csv",header = TRUE, sep = ",")
moderators_importance_akaike_3levels<-read.csv("results/moderators_importance_akaike_3levels.csv",header = TRUE, sep = ",")
aicc<- rbind(moderators_importance_akaike_2levels,moderators_importance_akaike_3levels)%>%
  rename("moderator"="Term")%>%
  left_join(pcc_factor_class_unit,by="pcc_factor_unit")
  
sort(unique(aicc$pcc_factor_unit))       
aicc$factor_sub_class[aicc$factor_sub_class %in% c("Land tenure")] <- "Political context"
aicc$factor_sub_class[aicc$factor_sub_class %in% c( "Financial risk-mechanisms")] <- "Political context"
aicc$factor_sub_class[aicc$factor_sub_class %in% c( "Knowledge access")] <- "Political context"
aicc$moderator[aicc$moderator %in% c("m_intervention_recla2")] <- "Diversification\npractice"
aicc$moderator[aicc$moderator %in% c("m_education_years")] <- "Education\n(years)"
aicc$moderator[aicc$moderator %in% c("m_mean_farm_size_ha")] <- "Farm size (ha)"
aicc$moderator[aicc$moderator %in% c("m_av_year_assessment")] <- "Year of\nassessment"
aicc$moderator[aicc$moderator %in% c("model_method_recla")] <- "Model type"
aicc$moderator[aicc$moderator %in% c("m_random_sample")] <- "Random\nsampling"
aicc$moderator[aicc$moderator %in% c("m_sampling_unit")] <- "Household\nsampling unit"
aicc$moderator[aicc$moderator %in% c("m_type_data")] <- "Primary data"
aicc$moderator[aicc$moderator %in% c("n_factors")] <- "Number of\npredictors"
aicc$moderator[aicc$moderator %in% c("m_un_region")] <- "Region"
aicc$moderator[aicc$moderator %in% c("m_un_subregion")] <- "Sub-region"
sort(unique(aicc$pcc_factor_unit))       

aicc2<-as.data.frame(table(aicc$factor_sub_class, aicc$moderator))%>%
  dplyr::rename("factor_sub_class"="Var1",
                "moderator"= "Var2")%>%
  left_join(factors, by= "factor_sub_class")%>%
  mutate(total= 35)

important_aicc<- aicc2%>%
  mutate(percentage= round((Freq/total)*100,4))%>%
  filter(percentage!=0)
important_aicc$importance[important_aicc$factor_sub_class %in% c("Biophysical context")] <- 8
important_aicc$importance[important_aicc$factor_sub_class %in% c("Farmers behaviour")] <- 7
important_aicc$importance[important_aicc$factor_sub_class %in% c("Financial capital")] <- 6
important_aicc$importance[important_aicc$factor_sub_class %in% c("Human capital")] <- 5
important_aicc$importance[important_aicc$factor_sub_class %in% c("Natural capital")] <- 3
important_aicc$importance[important_aicc$factor_sub_class %in% c("Physical capital")] <- 3
important_aicc$importance[important_aicc$factor_sub_class %in% c("Political context")] <- 2
important_aicc$importance[important_aicc$factor_sub_class %in% c("Social capital")] <- 1
important_aicc<-important_aicc%>%
  mutate(importance= paste("2important/",importance,sep=""))

sort(unique(important_aicc$factor_sub_class))

non_important_aicc<- aicc2%>%
  mutate(percentage= round(((total.factor_sub_class-Freq)/total)*100,4))%>%
  filter(percentage!=0)
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Biophysical context")] <- 8
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Farmers behaviour")] <- 7
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Financial capital")] <- 6
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Human capital")] <- 5
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Natural capital")] <- 4
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Physical capital")] <- 3
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Political context")] <- 2
non_important_aicc$importance[non_important_aicc$factor_sub_class %in% c("Social capital")] <- 1
non_important_aicc<-non_important_aicc%>%
  mutate(importance= paste("1non-important/",importance,sep=""))      
sort(unique(non_important_aicc$factor_sub_class))


important_total_aicc<- rbind(important_aicc,non_important_aicc)%>%
  arrange(desc(importance))

##################################################################################################################
############################## PLOT ##########################################################################################
##################################################################################################################
library(ggpubr)

moderator_ftest<- c("Household\nsampling unit","Year of\nassessment","Primary data","Number of\npredictors",
              "Farm size (ha)","Education\n(years)" ,"Random\nsampling","Region","Model type", "Sub-region",
               "Diversification\npractice")

importance_ftest<- ggplot(important_total_ftest, aes(y= moderator, x=percentage,colour= importance,fill=importance)) +
  geom_bar(stat="identity", show.legend = F)+
  scale_fill_manual(values = c("white","white","white","white","white","white","white","white",
                               "#297d7d","#92c46d","#496491","#6a57b8","#d896ff","#ea6044","#f0c602"))+
  scale_colour_manual(values = c("#297d7d","#92c46d","#496491","#87CEEB","#6a57b8","#d896ff","#ea6044","#f0c602",
                                 "#297d7d", "#92c46d","#496491","#6a57b8","#d896ff","#ea6044","#f0c602"))+
  scale_y_discrete(limits = moderator_ftest)+
  scale_x_continuous(limit = c(0,102),expand = c(0, 0))+
  ggtitle("Importance: Omnibus test based on F-distribution")+
  xlab("Percentage")+
  ylab("Moderators")+
  theme(axis.text.x =element_text(color="black",size=10, family = "sans"),
        axis.title = element_text(color="black",size=11, family = "sans",face="bold"),
        axis.ticks.y=element_blank(),
        axis.line.x = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "grey",size = 0.6),
        plot.title = element_text(color="black",size=12, family = "sans",face="bold",hjust = 0.5))
importance_ftest
moderator_aicc<- c("Year of\nassessment",
                    "Diversification\npractice",
                   "Sub-region","Number of\npredictors","Household\nsampling unit","Region","Model type",
                   "Random\nsampling","Education\n(years)" ,"Primary data","Farm size (ha)")

importance_aicc<- ggplot(important_total_aicc, aes(y= moderator, x=percentage,colour= importance,fill=importance)) +
  geom_bar(stat="identity", show.legend = F)+
  scale_fill_manual(values = c("white","white","white","white","white","white","white","white",
                               "#297d7d","#92c46d","#496491","#6a57b8","#d896ff","#ea6044","#f0c602"))+
  scale_colour_manual(values = c("#297d7d","#92c46d","#496491","#87CEEB","#6a57b8","#d896ff","#ea6044","#f0c602",
                                 "#297d7d", "#92c46d","#496491","#6a57b8","#d896ff","#ea6044","#f0c602"))+
  scale_y_discrete(limits = moderator_aicc)+
  scale_x_continuous(limit = c(0,102),expand = c(0, 0))+
  xlab("Percentage")+
  ylab("Moderators")+
  ggtitle("Importance: Akaike's information criterion (AICc)")+
  
  theme(axis.text.x =element_text(color="black",size=10, family = "sans"),
        axis.title = element_text(color="black",size=11, family = "sans",face="bold"),
        axis.ticks.y=element_blank(),
        axis.line.x = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "grey",size = 0.6),
        plot.title = element_text(color="black",size=12, family = "sans",face="bold",hjust = 0.5))
importance_aicc

importance.plot<-ggarrange(importance_ftest,importance_aicc,nrow = 2,widths = c(1, 1))
importance.plot
#1000 1500