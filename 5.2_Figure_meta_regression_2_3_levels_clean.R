library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(forcats)
library("xlsx")

####################################################################################################################################
################ META-REGRESSION FIGURES ############################################################################################
##############################################################################################################################
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/"

factors_metric_assessed <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "FACTORS_metric_assessed")%>%
  select(factor_category, factor_subcategory,factor_metric, pcc_unit, logor_unit)

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$factor_subcategory," (",factors_metric_assessed$logor_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_category,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

#### PCC data 
pcc_data<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))


#### Meta-regression results
meta_regression<- read.csv("results/meta_regression.csv",header = TRUE, sep = ",")%>%
  select(factor_category,pcc_factor_unit, moderator, moderator_class,estimate,ci.lb, ci.ub,tval, pval, f_test, significance,significance2,
         pcc.estimate, pcc.ci.lb,pcc.ci.ub)%>%
  mutate(factor_category= if_else(factor_category=="Financial risk-mechanisms","Political_1",
                                   if_else(factor_category=="Knowledge access","Political_2",
                                           if_else(factor_category=="Land tenure","Political_3",
                                                   factor_category))))%>%
  arrange(factor_category, moderator, 
          moderator_class)%>%
  dplyr::mutate(pcc.estimate2= ifelse(pcc.estimate > 0.33, "large",
                                  ifelse(pcc.estimate >= 0.17, "moderate",
                                         ifelse(pcc.estimate > -0.17, "small",
                                                ifelse(pcc.estimate >= -0.33, "moderate", "large")))))%>%
  mutate(estimate2_significance2= paste(pcc.estimate2,significance2,sep = "_"))%>%
  mutate(significance3 = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",""))))
                                             

sort(unique(meta_regression$factor_category))
sort(unique(meta_regression$estimate2_significance2))
sort(unique(meta_regression$pcc_factor_unit))


#Moderator: farm size------
m_farm_size_distribution<-pcc_data%>%
  filter(!is.na(m_mean_farm_size_ha))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))

m_farm_size<- meta_regression%>%
  filter(moderator== "m_mean_farm_size_ha")%>%
  mutate(moderator_class= if_else(moderator_class=="","Farm size (ha)","Intercept"))%>%
  mutate(f_test= if_else(moderator_class=="Farm size (ha)", "", f_test))%>%
  select("factor_category", "pcc_factor_unit","moderator_class",  "estimate" ,
         "ci.lb", "ci.ub","tval", "significance","f_test",
         pcc.estimate, pcc.ci.lb,pcc.ci.ub)

write.xlsx(m_farm_size, "results/meta_regression_farm_size.xlsx", 
           sheetName = "farm_size", col.names = TRUE, row.names = TRUE, append = FALSE)

m_farm_size<- meta_regression%>%
  filter(moderator== "m_mean_farm_size_ha")%>%
  filter(moderator_class!="intrcpt")%>%
  left_join(m_farm_size_distribution, by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))

m_farm_size$ID <- as.numeric(seq(1, 38, by = 1))

#Moderator: education------
m_education_distribution<-pcc_data%>%
  filter(!is.na(m_education_years))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))


m_education<- meta_regression%>%
  filter(moderator== "m_education_years")%>%
  mutate(moderator_class= if_else(moderator_class=="","Education (years)","Intercept"))%>%
  mutate(f_test= if_else(moderator_class=="Education (years)", "", f_test))%>%
  select("factor_category", "pcc_factor_unit","moderator_class",  "estimate" ,
         "ci.lb", "ci.ub","tval", "significance","f_test",
         pcc.estimate, pcc.ci.lb,pcc.ci.ub)

write.xlsx(m_education, "results/meta_regression_education.xlsx", 
           sheetName = "education", col.names = TRUE, row.names = TRUE, append = FALSE)

m_education<- meta_regression%>%
  filter(moderator== "m_education_years")%>%
  filter(moderator_class!="intrcpt")%>%
  left_join(m_education_distribution, by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))

#Moderator: methodological characteristics------
sort(unique(meta_regression$moderator))
m_methods<- meta_regression%>%
  filter(moderator== "model_method_recla"|
           moderator== "m_random_sample"|
           moderator== "m_sampling_unit"|
           moderator== "m_type_data"|
           moderator== "n_factors"|
           moderator=="m_av_year_assessment")%>%
  select("factor_category", "pcc_factor_unit","moderator","f_test")%>%
  distinct(., .keep_all = TRUE)%>%
  tidyr::pivot_wider(., names_from = moderator, values_from = f_test)

write.xlsx(m_methods, "results/meta_regression_methods.xlsx", 
           sheetName = "m_methods", col.names = TRUE, row.names = TRUE, append = FALSE)

#Moderator: diversified farming systems------
m_dfs_distribution<-pcc_data%>%
  group_by(pcc_factor_unit, m_dp_recla)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_dp_recla")
names(pcc_data)

m_dfs<- meta_regression%>%
  filter(moderator== "m_dp_recla")%>%
  left_join(m_dfs_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                     "moderator_class"="moderator_class"))%>%
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))

#Moderator: Region------
m_region_distribution<-pcc_data%>%
  group_by( pcc_factor_unit, m_un_region)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_un_region")

m_region<- meta_regression%>%
  filter(moderator== "m_un_region")%>%
  left_join(m_region_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                     "moderator_class"="moderator_class"))%>%
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))%>%
  rbind(c(factor_category = "Biophysical context",
             pcc_factor_unit = "Soil fertility (1= moderate)",
          moderator= "m_un_region",
             moderator_class = "Africa", 
             rep(NA, ncol(.) - 4)))

sort(unique(m_region$moderator_class))
  
#Moderator: Region------
m_subregion_distribution<-pcc_data%>%
  group_by( pcc_factor_unit, m_un_subregion)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_un_subregion")

m_subregion<- meta_regression%>%
  filter(moderator== "m_un_subregion")%>%
  left_join(m_subregion_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                        "moderator_class"="moderator_class"))%>%
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))

sort(unique(m_subregion$moderator_class))


#https://stackoverflow.com/questions/15420621/reproduce-table-and-plot-from-journal -----
## SYNTHESIZE SOME PLOT DATA 
## OVERALL RESULTS
overall<-m_farm_size%>%
  select(ID, pcc_factor_unit)

#___________________________------
## Overall results for the most studied factors
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")
significance <- c("#F7ADA4","#FF4933","#D3D3D3","#BAF2C4","#256C32")

overall<-m_farm_size%>%
  select(ID, pcc_factor_unit)


m_dfs$ID <- overall$ID[match(m_dfs$pcc_factor_unit, overall$pcc_factor_unit)]

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
  axis.title.x = element_blank(),

  axis.text.x =element_text(color="black",size=12, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.background = element_blank(),
  panel.grid  =  element_blank(),
  
  #panel.grid.major  = element_line(color = "grey85",size = 0.6),
  axis.line = element_line(colour = "grey45"))

sort(unique(m_dfs$estimate2_significance2))
dfs<- ggplot(m_dfs, aes(x=moderator_class ,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(estimate2_significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(estimate2_significance2),
                 colour= factor(estimate2_significance2)), shape = 21, 
             show.legend=F) +
  scale_fill_manual(values = c("#8F1D1E","#D3D3D3","#184620",
                               "#FF4933","#D3D3D3","#329244",
                               "#F7ADA4","#D3D3D3","#BAF2C4"))+
  #,"#FF4933","#D3D3D3","#BAF2C4",))+
  scale_colour_manual(values = c("#8F1D1E","#D3D3D3","#184620",
                                 "#FF4933","#D3D3D3","#329244",
                                 "#F7ADA4","#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_category),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_discrete(expand =c(0,0),position = "top")+
  scale_y_discrete(expand =c(0,0))+
  
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0.1,b=0.5,l=1.5), "cm"),
        axis.text.y =element_text(color="black",size=11, family = "sans"))
dfs
  
overall_distribution_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = "white"),
  text_y = elem_list_text(size= 0.1,colour= "white",angle = 90),
  by_layer_y = FALSE)

m_region$ID <- overall$ID[match(m_region$pcc_factor_unit, overall$pcc_factor_unit)]

sort(unique(m_region$estimate2_significance2))
region<-
ggplot(m_region, aes(x=moderator_class,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(estimate2_significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(estimate2_significance2),
                 colour= factor(estimate2_significance2)), shape = 21, 
             show.legend=F) +
  scale_fill_manual(values = c("#D3D3D3","#184620",
                               "#D3D3D3","#329244",
                               "#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#D3D3D3","#184620",
                                 "#D3D3D3","#329244",
                                 "#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_category),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_discrete(expand =c(0,0),position = "top")+
  scale_y_discrete(expand =c(0,0))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0.1,b=0.5,l=1.5), "cm"),
        axis.text.y =element_text(color="black",size=11, family = "sans"))

region 

regression.plot<-ggarrange(dfs,region,ncol = 2,widths = c(1, 0.30))

regression.plot

#19x23

#18x20
m_subregion$ID <- overall$ID[match(m_subregion$pcc_factor_unit, overall$pcc_factor_unit)]
sort(unique(m_subregion$estimate2_significance2))
sort(unique(m_subregion$moderator_class))

subregion<-ggplot(m_subregion, aes(x=moderator_class,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(estimate2_significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(estimate2_significance2),
                 colour= factor(estimate2_significance2)), shape = 21,show.legend=F) +
  scale_fill_manual(values = c("#8F1D1E","#D3D3D3","#184620",
                                "#FF4933","#D3D3D3","#329244",
                               "#F7ADA4","#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#8F1D1E","#D3D3D3","#184620",
                                "#FF4933","#D3D3D3","#329244",
                                "#F7ADA4","#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_category),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_discrete(expand =c(0,0),position = "top")+
  scale_y_discrete(expand =c(0,0))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"),
        axis.text.x =element_text(color="black",size=9,angle=45, family = "sans"),
        axis.text.y =element_text(color="black",size=9, family = "sans"))

subregion 


m_education$ID <- overall$ID[match(m_education$pcc_factor_unit, overall$pcc_factor_unit)]
sort(unique(m_education$estimate2_significance2))
sort(unique(m_education$moderator_class))

education<-
ggplot(m_education, aes(x=moderator_class,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(estimate2_significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(estimate2_significance2),
                 colour= factor(estimate2_significance2)), shape = 21,show.legend=F) +
  scale_fill_manual(values = c("#D3D3D3","#F7ADA4","#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#D3D3D3","#F7ADA4","#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_category),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_discrete(expand =c(0,0),position = "top")+
  scale_y_discrete(expand =c(0,0))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0.1,b=0.5,l=1.5), "cm"),
        axis.text.y =element_text(color="black",size=11, family = "sans"))

education 

m_farm_size$ID <- overall$ID[match(m_farm_size$pcc_factor_unit, overall$pcc_factor_unit)]
sort(unique(m_farm_size$estimate2_significance2))
sort(unique(m_farm_size$moderator_class))

farm_size<-ggplot(m_farm_size, aes(x=moderator_class,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(estimate2_significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(estimate2_significance2),
                 colour= factor(estimate2_significance2)), shape = 21,show.legend=F) +
  scale_fill_manual(values = c("#D3D3D3",
                               "#D3D3D3",
                               "#F7ADA4", "#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#D3D3D3",
                                 "#D3D3D3",
                                 "#F7ADA4", "#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(11))+
  
  facet_grid2(vars(factor_category),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_discrete(expand =c(0,0),position = "top")+
  scale_y_discrete(expand =c(0,0))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0.1,b=0.5,l=1.5), "cm"),
        axis.text.y =element_text(color="black",size=11, family = "sans"))

farm_size 
regression.plot<-ggarrange(education,farm_size,ncol = 2,widths = c(1, 1))
regression.plot


## Figures main paper
#Diversified practices
library(stringr)
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")
sort(unique(m_dfs$pcc_factor_unit))

# Figure 5 ----
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")
m_dfs_significant<- m_dfs%>%
  filter(!str_detect(estimate2_significance2,"non_significant"))

m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Cover crops"] <-"1_Cover crops"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Agroforestry"] <-"2_Agroforestry"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Intercropping"] <-"3_Intercropping"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Embedded seminatural habitats"] <-"4_Embedded seminatural"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Crop rotation"] <-"5_Crop rotation"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Fallow"] <-"6_Fallow"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Combined practices"] <-"7_Combined practices"

m_dfs_significant<-m_dfs_significant%>%
  arrange(moderator_class, factor_category, desc(estimate))%>%
  mutate(ID= seq(1, 38 ))%>%
  mutate(pcc_factor_unit= paste(ID,pcc_factor_unit,sep = "  "))
  
overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = "white"),
  text_y = elem_list_text(size= 6,colour= "white",angle = 90),
  by_layer_y = FALSE
)

theme_overall<-theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(color="black",size=13, family = "sans", face = "bold",vjust = -1),
  axis.text.x =element_text(color="black",size=12, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.background = element_blank(),
  panel.grid.major.x  = element_line(color = "grey85",size = 0.6),
  axis.line = element_line(colour = "black"),
  axis.ticks.y=element_line(colour = "grey30"))

dfs_significant<-
ggplot(m_dfs_significant, 
         aes(y=reorder(pcc_factor_unit, desc(ID)),x=pcc.estimate,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,group=pcc_factor_unit,
             colour = factor(factor_category) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1.3, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3.5, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance3, x=pcc.ci.ub+0.02, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
  scale_colour_manual(values = fills)+
  facet_grid2(vars(moderator_class),
             scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-1,1.25),expand = c(0, 0),
                     breaks = c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1),
                     labels = c("-1","-0.75","-0.5","-0.25","0","0.25","0.5","0.75","1"))+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=5, family = "sans"))

dfs_significant

dfs_significant_distribution<-
        ggplot(m_dfs_significant, 
         aes(x=n_studies, y=reorder(pcc_factor_unit, desc(ID)),
             fill = factor(factor_category))) +
  geom_bar(stat="identity",show.legend = F,width = 0.7)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 4, alpha=0.6,
                show.legend = F)+ 
  scale_fill_manual(values = fills)+
  facet_grid2(vars(moderator_class),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0), "cm"))+
  scale_x_continuous(
    limit = c(0,50),expand = c(0,0),
    breaks = c(0,10,20,30,40,50),
    labels= c("0","10","20","30","40","50"))
dfs_significant_distribution
overall.plot<-ggarrange(dfs_significant,dfs_significant_distribution,ncol = 2,widths = c(1, 0.3))
overall.plot
#15x19

# Figure 6 -----
m_region_nonsignificant<- m_region%>%
  filter(str_detect(estimate2_significance2,"non_significant"))
sort(unique(m_region_nonsignificant$pcc_factor_unit))

m_subregion_nonsignificant<- m_subregion%>%
  filter(str_detect(estimate2_significance2,"non_significant"))
sort(unique(m_subregion_nonsignificant$pcc_factor_unit))

m_region_significant<- m_region%>%
  filter(!str_detect(estimate2_significance2,"non_significant"))%>%
 mutate(pcc.estimate = as.numeric(pcc.estimate),
        pcc.ci.lb = as.numeric(pcc.ci.lb),
        pcc.ci.ub = as.numeric(pcc.ci.ub),
        factor_category = as.factor(factor_category),
        n_studies = as.numeric(n_studies),
        n_ES = as.numeric(n_ES) )%>%
  arrange(moderator_class, factor_category, desc(pcc.estimate))%>%
  mutate(ID= seq(1, 14 ))%>%
  mutate(pcc_factor_unit= paste(ID,pcc_factor_unit,sep = "  "))

m_region_significant$moderator_class[m_region_significant$moderator_class %in%"Latin America and the Caribbean"] <-"1_Latin America"
m_region_significant$moderator_class[m_region_significant$moderator_class %in%"Africa"] <-"2_Africa"
m_region_significant$moderator_class[m_region_significant$moderator_class %in%"Asia"] <-"2_Asia"
m_region_significant$moderator_class[m_region_significant$moderator_class %in%"Europe"] <-"3_Europe"


fills <- c("#f0c602", "#ea6044","#6a57b8", "#92c46d", "#92c46d","#92c46d","#297d7d")

theme_overall<-theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(color="black",size=13, family = "sans", face = "bold",vjust = -1),
  axis.text.x =element_text(color="black",size=12, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.grid.major.x  = element_line(color = "grey85",size = 0.6),
  axis.ticks.y=element_line(colour = "grey30"),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"))

region_significant<-
  ggplot(m_region_significant, 
         aes(y=reorder(pcc_factor_unit, desc(ID)),x=pcc.estimate,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,group=pcc_factor_unit,
             colour = factor(factor_category) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  
  geom_errorbar(width=0,size=1.3, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3.5, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance3, x=pcc.ci.ub+0.02, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
  scale_colour_manual(values = fills)+
  facet_grid2(vars(moderator_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.25,1.1),expand = c(0, 0),
                     breaks = c(-0.25,0,0.25,0.5,0.75,1),
                     labels = c("-0.25","0","0.25","0.5","0.75","1"))+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=5, family = "sans"))

region_significant

region_significant_distribution<-
ggplot(m_region_significant, 
       aes(x=n_studies, y=reorder(pcc_factor_unit, desc(ID)),
           fill = factor(factor_category))) +
  geom_bar(stat="identity",show.legend = F,width = 0.7)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 4,
                show.legend = F, alpha=0.6)+
  scale_fill_manual(values = fills)+
  facet_grid2(vars(moderator_class),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("")+
  #xlab("Number")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y=element_line(colour = "grey"),
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0), "cm"))+
  scale_x_continuous(
    limit = c(0,50),expand = c(0,0),
    breaks = c(0,10,20,30,40,50),
    labels= c("0","10","20","30","40","50"))
region_significant_distribution
overall.region.plot<-ggarrange(region_significant,region_significant_distribution,ncol = 2,widths = c(1, 0.3))
overall.region.plot
#15x6
#15*9
#1000 1500
