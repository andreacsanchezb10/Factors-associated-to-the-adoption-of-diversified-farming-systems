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
factors_metric_assessed <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

#### PCC data 
pcc_data<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))


#### Meta-regression results
meta_regression<- read.csv("results/meta_regression.csv",header = TRUE, sep = ",")%>%
  select(factor_sub_class,pcc_factor_unit, moderator, moderator_class,estimate,ci.lb, ci.ub,tval, pval, f_test, significance,significance2)%>%
  
  mutate(factor_sub_class= if_else(factor_sub_class=="Financial risk-mechanisms","Political_1",
                                   if_else(factor_sub_class=="Knowledge access","Political_2",
                                           if_else(factor_sub_class=="Land tenure","Political_3",
                                                   factor_sub_class))))%>%
  arrange(factor_sub_class, moderator, 
          moderator_class)%>%
  dplyr::mutate(estimate2= ifelse(estimate > 0.33, "large",
                                  ifelse(estimate >= 0.17, "moderate",
                                         ifelse(estimate > -0.17, "small",
                                                ifelse(estimate >= -0.33, "moderate", "large")))))%>%
  mutate(estimate2_significance2= paste(estimate2,significance2,sep = "_"))%>%
  mutate(significance3 = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",""))))
                                             

sort(unique(meta_regression$factor_sub_class))
sort(unique(meta_regression$estimate2_significance2))


#Moderator: diversification practices components ------
m_intervention_system_components<- meta_regression%>%
  filter(moderator== "m_intervention_system_components")%>%
  select("factor_sub_class", "pcc_factor_unit","moderator_class",  "estimate" ,
         "ci.lb", "ci.ub","tval", "significance","f_test")%>%
  mutate(moderator_class = paste0(toupper(substr(moderator_class, 1, 1)), substr(moderator_class, 2, nchar(moderator_class))))


write.xlsx(m_intervention_system_components, "results/meta_regression_intervention_components.xlsx", 
           sheetName = "farm_size", col.names = TRUE, row.names = TRUE, append = FALSE)


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
  select("factor_sub_class", "pcc_factor_unit","moderator_class",  "estimate" ,
         "ci.lb", "ci.ub","tval", "significance","f_test")

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
  select("factor_sub_class", "pcc_factor_unit","moderator_class",  "estimate" ,
         "ci.lb", "ci.ub","tval", "significance","f_test")

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
  filter(moderator== "m_endogeneity_correction"|
           moderator== "m_exact_variance_value"|
           moderator== "m_exposure_correction"|
           moderator== "model_method_recla"|
           moderator== "m_random_sample"|
           moderator== "m_sampling_unit"|
           moderator== "m_type_data"|
           moderator== "n_factors"|
           moderator== "n_samples"|
           moderator=="m_av_year_assessment")%>%
  select("factor_sub_class", "pcc_factor_unit","moderator","f_test")%>%
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
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))
  rbind(c(factor_sub_class = "Biophysical context",
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

#___________________________
## Overall results for the most studied factors
fills <- c("#f0c602", "#F09319","#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")
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
  
  facet_grid2(vars(factor_sub_class),
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
region<-ggplot(m_region, aes(x=moderator_class,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(estimate2_significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(estimate2_significance2),
                 colour= factor(estimate2_significance2)), shape = 21, 
             show.legend=F) +
  scale_fill_manual(values = c("#8F1D1E","#D3D3D3","#184620",
                               "#D3D3D3","#329244",
                               "#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#8F1D1E","#D3D3D3","#184620",
                                 "#D3D3D3","#329244",
                                 "#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_sub_class),
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
                               "#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#8F1D1E","#D3D3D3","#184620",
                                 "#FF4933","#D3D3D3","#329244",
                                 "#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_discrete(expand =c(0,0),position = "top")+
  scale_y_discrete(expand =c(0,0))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0.1,b=0.5,l=1.5), "cm"),
        axis.text.y =element_text(color="black",size=11, family = "sans"))

subregion 


m_education$ID <- overall$ID[match(m_education$pcc_factor_unit, overall$pcc_factor_unit)]
sort(unique(m_education$estimate2_significance2))
sort(unique(m_education$moderator_class))

education<-ggplot(m_education, aes(x=moderator_class,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(estimate2_significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(estimate2_significance2),
                 colour= factor(estimate2_significance2)), shape = 21,show.legend=F) +
  scale_fill_manual(values = c("#F7ADA4","#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#F7ADA4","#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_sub_class),
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
                               "#F7ADA4", "#D3D3D3","#BAF2C4"))+
  scale_colour_manual(values = c("#D3D3D3",
                                 "#F7ADA4", "#D3D3D3","#BAF2C4"))+
  scale_size_manual(values=c(11))+
  
  facet_grid2(vars(factor_sub_class),
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

m_dfs_significant<- m_dfs%>%
  filter(!str_detect(estimate2_significance2,"non_significant"))%>%
  mutate(ci.ub_l = ifelse(ci.ub > 3, 3, NA),
         ci.lb_l=ifelse(ci.ub > 3, ci.lb, NA))
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Cover crops"] <-"1_Cover crops"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Agroforestry"] <-"2_Agroforestry"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Intercropping"] <-"3_Intercropping"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Embedded seminatural habitats"] <-"4_Embedded seminatural"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Crop rotation"] <-"5_Crop rotation"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Fallow"] <-"6_Fallow"
m_dfs_significant$moderator_class[m_dfs_significant$moderator_class %in%"Combined systems"] <-"7_Combined systems"

m_dfs_significant<-m_dfs_significant%>%
  arrange(moderator_class, factor_sub_class, desc(estimate))%>%
  mutate(ID= seq(1, 33 ))%>%
  mutate(pcc_factor_unit= paste(ID,pcc_factor_unit,sep = "  "))
  
  
  
#m_dfs_significant$ID <- overall$ID[match(m_dfs_significant$pcc_factor_unit, overall$pcc_factor_unit)]

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
  panel.grid.major  = element_line(color = "grey85",size = 0.6),
  axis.line = element_line(colour = "black"))

dfs_significant<-
  #ggplot(subset(m_dfs_significant,moderator_class=="5_Crop rotation"),
            ggplot(m_dfs_significant, 
         aes(y=reorder(pcc_factor_unit, desc(ID)),x=estimate,
             xmin=ci.lb, xmax=ci.ub,group=pcc_factor_unit,
             colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3.5, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance3, x=ci.ub+0.01, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
  scale_colour_manual(values = fills)+
  facet_grid2(vars(moderator_class),
             scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-2.5,3.5),expand = c(0.01, 0.01),
                     breaks = c(-2,-1,0,1,2,3),
                     labels = c("-2","-1","0","1","2","3"))+
  xlab("")+
  #xlab(bquote(bold("Partial correlation coefficient (" *italic(r)[p]*")")))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))

dfs_significant

dfs_significant_distribution<-
        ggplot(m_dfs_significant, 
         aes(x=n_articles, y=reorder(pcc_factor_unit, desc(ID)),
             fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F)+ 
  geom_point(aes(x=n_ES, y=reorder(pcc_factor_unit, estimate),
                 fill = factor(factor_sub_class)),
             shape=18,size=2, position = (position_dodge(width = -0.2)),
             show.legend = F)+
  scale_fill_manual(values = fills)+
  facet_grid2(vars(moderator_class),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("")+
  #xlab("Number")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))+
  scale_x_continuous(
    limit = c(0,40),expand = c(0,0),
    breaks = c(0,10,20,30,40,50),
    labels= c("0","10","20","30","40","50"))
dfs_significant_distribution
overall.plot<-ggarrange(dfs_significant,dfs_significant_distribution,ncol = 2,widths = c(1, 0.2))
overall.plot
#19x23
#1000 1500

#Regions -----
m_region_significant<- m_region%>%
  filter(!str_detect(estimate2_significance2,"non_significant"))%>%
 mutate(estimate = as.numeric(estimate),
        ci.lb = as.numeric(ci.lb),
        ci.ub = as.numeric(ci.ub),
        factor_sub_class = as.factor(factor_sub_class),
        n_articles = as.numeric(n_articles),
        n_ES = as.numeric(n_ES) )%>%
  arrange(moderator_class, factor_sub_class, desc(estimate))%>%
  mutate(ID= seq(1, 9 ))%>%
  mutate(pcc_factor_unit= paste(ID,pcc_factor_unit,sep = "  "))

fills <- c( "#ea6044","#d896ff", "#92c46d", "#92c46d","#297d7d")

overall_regions <- strip_themed(
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
  panel.grid.major.x  = element_line(color = "grey85",size = 0.6),
  
  axis.ticks.y=element_line(colour = "grey"),
  
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"))

region_significant<-
  ggplot(m_region_significant, 
         aes(y=reorder(pcc_factor_unit, desc(ID)),x=estimate,
             xmin=ci.lb, xmax=ci.ub,group=pcc_factor_unit,
             colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3.5, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance3, x=ci.ub+0.01, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  
  scale_colour_manual(values = fills)+
  facet_grid2(vars(moderator_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.7,2),expand = c(0.01, 0.01),
                     breaks = c(-0.5,0,0.5,1,1.5,2),
                     labels = c("-0.5","0","0.5","1","1.5","2"))+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))

region_significant

region_significant_distribution<-
ggplot(m_region_significant, 
       aes(x=n_articles, y=reorder(pcc_factor_unit, desc(ID)),
           fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F)+
  geom_point(aes(x=n_ES, y=reorder(pcc_factor_unit, estimate),
                 fill = factor(factor_sub_class)),
             shape=15,size=2, position = (position_dodge(width = -0.2)),
             show.legend = F)+
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
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))+
  scale_x_continuous(
    limit = c(0,40),expand = c(0,0),
    breaks = c(0,10,20,30,40,50),
    labels= c("0","10","20","30","40","50"))
region_significant_distribution
overall.region.plot<-ggarrange(region_significant,region_significant_distribution,ncol = 2,widths = c(1, 0.2))
overall.region.plot
#15x6
#1000 1500
