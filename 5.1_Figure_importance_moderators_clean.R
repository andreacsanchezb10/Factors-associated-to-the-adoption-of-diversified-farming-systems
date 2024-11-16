##################################################################
## PLOT IMPORTANT MODERATORS
############################################################################
library(reshape2)
library(ggh4x)
library(readxl)
library(dplyr)

data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_metric_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$logor_unit,")", sep="")


pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_category,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

pcc_data<-read.csv("data/pcc_data.csv",header = TRUE, sep = ",")
names(pcc_data)

# F-Test moderators
#Meta-regression results
meta_regression<- read.csv("results/meta_regression.csv",header = TRUE, sep = ",")%>%
  dplyr::select(moderator, factor_category, pcc_factor_unit,QMp)
meta_regression$factor_category[meta_regression$factor_category %in% c("Land tenure")] <- "Political context"
meta_regression$factor_category[meta_regression$factor_category %in% c( "Financial risk-mechanisms")] <- "Political context"
meta_regression$factor_category[meta_regression$factor_category %in% c( "Knowledge access")] <- "Political context"
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
sort(unique(meta_regression$factor_category))

factors<-meta_regression%>%
  group_by(factor_category)%>%
  dplyr::summarise(total.factor_category = n_distinct(pcc_factor_unit))%>%
  ungroup()
sort(unique(factors$factor_category))
sum(factors$total.factor_category)

factors2<-meta_regression%>%
  select(factor_category,moderator)
factors2<-unique(factors2)
sort(unique(factors2$factor_category))

meta_regression<-meta_regression%>%
  filter(QMp<=0.05)
meta_regression<-unique(meta_regression)

sort(unique(meta_regression$moderator))
sort(unique(meta_regression$factor_category))

meta_regression2<-as.data.frame(table(meta_regression$factor_category, meta_regression$moderator))%>%
  dplyr::rename("factor_category"="Var1",
                "moderator"= "Var2")%>%
  right_join(factors2, by= c("factor_category","moderator"))%>%
  mutate(Freq= if_else(is.na(Freq),0,Freq))%>%
  left_join(factors, by= "factor_category")%>%
   mutate(total= 38)

important_ftest<- meta_regression2%>%
  mutate(percentage= round((Freq/total)*100,3))%>%
  filter(percentage!=0)
important_ftest$importance[important_ftest$factor_category %in% c("Biophysical context")] <- 8
important_ftest$importance[important_ftest$factor_category %in% c("Farmers attitudes")] <- 7
important_ftest$importance[important_ftest$factor_category %in% c("Financial capital")] <- 6
important_ftest$importance[important_ftest$factor_category %in% c("Human capital")] <- 5
important_ftest$importance[important_ftest$factor_category %in% c("Physical capital")] <- 3
important_ftest$importance[important_ftest$factor_category %in% c("Political context")] <- 2
important_ftest$importance[important_ftest$factor_category %in% c("Social capital")] <- 1
important_ftest<-important_ftest%>%
  mutate(importance= paste("2important/",importance,sep=""))

sort(unique(important_ftest$factor_category))
  
non_important_ftest<- meta_regression2%>%
  mutate(percentage= round(((total.factor_category-Freq)/total)*100,3))%>%
  filter(percentage!=0)
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Biophysical context")] <- 8
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Farmers attitudes")] <- 7
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Financial capital")] <- 6
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Human capital")] <- 5
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Natural capital")] <- 4
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Physical capital")] <- 3
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Political context")] <- 2
non_important_ftest$importance[non_important_ftest$factor_category %in% c("Social capital")] <- 1
non_important_ftest<-non_important_ftest%>%
  mutate(importance= paste("1non-important/",importance,sep=""))      

important_total_ftest<- rbind(important_ftest,non_important_ftest)%>%
  arrange(desc(importance)) 

##Figure A.2. Importance of moderators in explaining variance in overall effects for the 38
#factors included in the meta-regression analyses, based on the F-distribution omnibus test.
library(ggpubr)

moderator_ftest<- c("Year of\nassessment","Household\nsampling unit","Primary data",
              "Farm size (ha)","Education\n(years)" ,"Random\nsampling","Number of\npredictors","Model type",
              "Region",
               "Diversification\npractice", "Sub-region")

importance_ftest<- ggplot(important_total_ftest, aes(y= moderator, x=percentage,colour= importance,fill=importance)) +
  geom_bar(stat="identity", show.legend = F)+
  scale_fill_manual(values = c("white","white","white","white","white","white","white","white",
                               "#297d7d","#92c46d","#496491","#6a57b8","#d896ff","#ea6044","#f0c602"))+
  scale_colour_manual(values = c("#297d7d","#92c46d","#496491","#87CEEB","#6a57b8","#d896ff","#ea6044","#f0c602",
                                 "#297d7d", "#92c46d","#496491","#6a57b8","#d896ff","#ea6044","#f0c602"))+
  scale_y_discrete(limits = moderator_ftest)+
  scale_x_continuous(limit = c(0,102),expand = c(0, 0))+
  ggtitle("")+
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
#pdf: 10.05*8.21
#landscape