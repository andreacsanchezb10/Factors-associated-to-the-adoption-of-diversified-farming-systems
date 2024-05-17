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
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
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
  mutate(estimate2_significance2= paste(estimate2,significance2,sep = "_"))

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
m_farm_size<- meta_regression%>%
  filter(moderator== "m_mean_farm_size_ha")%>%
  mutate(moderator_class= if_else(moderator_class=="","Farm size (ha)","Intercept"))%>%
  mutate(f_test= if_else(moderator_class=="Farm size (ha)", "", f_test))%>%
  select("factor_sub_class", "pcc_factor_unit","moderator_class",  "estimate" ,
         "ci.lb", "ci.ub","tval", "significance","f_test")

write.xlsx(m_farm_size, "results/meta_regression_farm_size.xlsx", 
           sheetName = "farm_size", col.names = TRUE, row.names = TRUE, append = FALSE)
m_farm_size<-m_farm_size%>%
  filter(moderator_class=="Farm size (ha)")%>%
  group_by(factor_sub_class)%>%
  group_modify(~.x %>%
                 arrange(factor(pcc_factor_unit, levels = c(sort(pcc_factor_unit), 
                                   decreasing = TRUE))))

m_farm_size$ID <- as.numeric(seq(1, 35, by = 1))

#Moderator: education------
m_education<- meta_regression%>%
  filter(moderator== "m_education_years")%>%
  mutate(moderator_class= if_else(moderator_class=="","Education (years)","Intercept"))%>%
  mutate(f_test= if_else(moderator_class=="Education (years)", "", f_test))%>%
  select("factor_sub_class", "pcc_factor_unit","moderator_class",  "estimate" ,
         "ci.lb", "ci.ub","tval", "significance","f_test")

write.xlsx(m_education, "results/meta_regression_education.xlsx", 
           sheetName = "education", col.names = TRUE, row.names = TRUE, append = FALSE)

  
#Moderator: methodological characteristics------
sort(unique(meta_regression$moderator))
m_methods<- meta_regression%>%
  filter(moderator== "m_endogeneity_correction"|
           moderator== "m_exact_variance_value"|
           moderator== "m_exposure_correction"|
           moderator== "m_model_method"|
           moderator== "m_random_sample"|
           moderator== "m_sampling_unit"|
           moderator== "m_type_data"|
           moderator== "n_predictors_num"|
           moderator== "n_samples_num"|
           moderator=="m_av_year_assessment")%>%
  select("factor_sub_class", "pcc_factor_unit","moderator","f_test")%>%
  distinct(., .keep_all = TRUE)%>%
  tidyr::pivot_wider(., names_from = moderator, values_from = f_test)

write.xlsx(m_methods, "results/meta_regression_methods.xlsx", 
           sheetName = "m_methods", col.names = TRUE, row.names = TRUE, append = FALSE)

#Moderator: diversified farming systems------
m_dfs_distribution<-pcc_data%>%
  group_by(pcc_factor_unit, m_intervention_recla2)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_intervention_recla2")
names(pcc_data)

m_dfs<- meta_regression%>%
  filter(moderator== "m_intervention_recla2")%>%
  left_join(m_dfs_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                     "moderator_class"="moderator_class"))%>%
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))

#Moderator: Region------
m_region_distribution<-pcc_data%>%
  group_by( pcc_factor_unit, m_region)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_region")

m_region<- meta_regression%>%
  filter(moderator== "m_region")%>%
  left_join(m_region_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                     "moderator_class"="moderator_class"))%>%
  mutate(icon_n_es= if_else(n_ES>=10,"more10.png","less10.png" ))%>%
  rbind(c(factor_sub_class = "Biophysical context",
             pcc_factor_unit = "Soil fertility (1= moderate)",
          moderator= "m_region",
             moderator_class = "Africa", 
             rep(NA, ncol(.) - 4)))

sort(unique(m_region$moderator_class))
  

#https://stackoverflow.com/questions/15420621/reproduce-table-and-plot-from-journal -----
## SYNTHESIZE SOME PLOT DATA 
## OVERALL RESULTS
overall<-m_farm_size%>%
  select(ID, pcc_factor_unit)

#___________________________
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
  scale_colour_manual(values = c("#8F1D1E","#D3D3D3","#15320C",
                                 "#FF4933","#D3D3D3","#256C32",
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


region<-ggplot(m_region, aes(x=moderator_class,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_es), fill=factor(significance2),
                 colour= factor(significance2)), shape = 21, 
             show.legend=F) +
  scale_fill_manual(values = c("#FF4933","#D3D3D3","#BAF2C4","#256C32"))+
  scale_colour_manual(values = c("#FF4933","#D3D3D3","#BAF2C4","#256C32"))+
  scale_size_manual(values=c(5,11))+
  
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  scale_x_discrete(expand =c(0,0),position = "top")+
  scale_y_discrete(expand =c(0,0))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())
region 

regression.plot<-ggarrange(dfs,region,ncol = 2,widths = c(1, 0.30))

regression.plot

#19x23

#18x20