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
          moderator_class)

sort(unique(meta_regression$factor_sub_class))
sort(unique(meta_regression$pcc_factor_unit))

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
  filter(moderator_class=="Farm size (ha)")
m_farm_size$ID <- as.numeric(seq(1, 40, by = 1))

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
  mutate(
    y = case_when(
      moderator_class == "Agro-aquaculture" ~ 0 + (0.5 *1),
      moderator_class == "Agro-silvopasture" ~ 0 + (0.5 *3),
      moderator_class == "Agroforestry" ~ 0 + (0.5 *5),
      moderator_class == "Combined systems" ~ 0 + (0.5 * 7),
      moderator_class == "Cover crops" ~ 0 + (0.5 * 9),
      moderator_class == "Crop rotation" ~ 0 + (0.5 * 11),
      moderator_class == "Embedded seminatural habitats" ~ 0 + (0.5 * 13),
      moderator_class == "Fallow" ~ 0 + (0.5 * 15),
      moderator_class == "Intercropping" ~ 0 + (0.5 * 17),
      moderator_class == "Rotational grazing" ~ 0 + (0.5 * 19),
      TRUE ~ NA_real_))%>%
  #tidyr::complete(., pcc_factor_unit, moderator_class, fill = list(estimate = NA))%>%
  left_join(m_dfs_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                     "moderator_class"="moderator_class"))%>%
  mutate(icon_n_articles= if_else(n_ES>=10,
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/more10.png",
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/less10.png" ))

#Moderator: Region------
m_region_distribution<-pcc_data%>%
  group_by( pcc_factor_unit, m_region)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_region")

m_region<- meta_regression%>%
  filter(moderator== "m_region")%>%
  mutate(
    y = case_when(
      moderator_class == "Africa" ~ 10 + (0.5 *1),
      moderator_class == "Asia" ~ 10 + (0.5 * 3),
      moderator_class == "Europe" ~ 10 + (0.5 * 5),
      moderator_class == "Latin America and the Caribbean" ~ 10 + (0.5 * 7),
      moderator_class == "Northern America" ~ 10 + (0.5 * 9),
      TRUE ~ NA_real_))%>%
  left_join(m_region_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                     "moderator_class"="moderator_class"))%>%
  mutate(icon_n_articles= if_else(n_ES>=10,
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/more10.png",
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/less10.png" ))%>%
  rbind(c(factor_sub_class = "Biophysical context",
             pcc_factor_unit = "Soil fertility (1= moderate)",
          moderator= "m_region",
             moderator_class = "Africa", 
             rep(NA, ncol(m_region) - 4)))

sort(unique(m_region$moderator_class))
  

#https://stackoverflow.com/questions/15420621/reproduce-table-and-plot-from-journal -----
## SYNTHESIZE SOME PLOT DATA 
## OVERALL RESULTS
overall<-m_farm_size%>%
  select(ID, pcc_factor_unit)

overall<-rbind(overall,data.frame(ID=c(max(m_farm_size$ID)+1,
                                       max(m_farm_size$ID)+2,
                                       max(m_farm_size$ID)+3),
                                       #max(overal_results$ID)+4),
                                  estimate=NA,
                                  pcc_factor_unit=NA))
#overall<-overall%>%
 # left_join(overall_results_more10, by= c("ID","beta", "ci.lb","ci.ub" ))


## identify the rows to be highlighted, and 
## build a function to add the layers
# Create a data frame with alternating colors
hl_rows <- data.frame(ID = (1:floor(length(unique(overall$ID[which(overall$ID > 0)])) / 2)) * 2,
                      col = "lightgrey", width = 1,height=1)

# Set the color and width for ID = 40
hl_rows$col[hl_rows$ID %in% 33] <- "white"
hl_rows$col[hl_rows$ID %in% 34] <- "white"
hl_rows$height[hl_rows$ID %in% 33] <- 4



hl_rect <- function(col = "lightgrey", alpha = 0.8, 
                    border_col = "lightgrey", width = 1, height = 1) {
  rectGrob(
    x = 0, y = 0, width = width, height = height, just = c("left", "bottom"),
    gp = gpar(alpha = alpha, fill = col, col = border_col)
  )
}


## Systems icons
library("ggimage")
systems_path1 <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons/"

systems_icons <- list.files(systems_path1, pattern = "\\.png$", full.names = TRUE)
systems_icons

RtLabels <- data.frame(systems_path = systems_icons)%>%
  mutate(moderator_class = sub(paste0("^", systems_path1), "", systems_path))%>%
  mutate(moderator_class = sub("\\.png$", "", moderator_class))%>%
  mutate(
    moderator_class = case_when(
      moderator_class == "integrated_aquaculture_agriculture"~"Agro-aquaculture",
      moderator_class == "agrosilvopasture"~"Agro-silvopasture",
      moderator_class == "agroforestry"~"Agroforestry" ,
      moderator_class == "combined_systems" ~"Combined systems",
      moderator_class =="cover_crops"  ~"Cover crops" ,
      moderator_class == "crop_rotation"~"Crop rotation",
      moderator_class == "embedded_seminatural_habitats"~"Embedded seminatural habitats",
      moderator_class == "fallow"~"Fallow" ,
      moderator_class == "intercropping"~"Intercropping" ,
      moderator_class == "rotational_grazing"~"Rotational grazing" ))%>%
  mutate(
    y = case_when(
      moderator_class == "Agro-aquaculture" ~ 0 + (0.5 *1),
      moderator_class == "Agro-silvopasture" ~ 0 + (0.5 *3),
      moderator_class == "Agroforestry" ~ 0 + (0.5 *5),
      moderator_class == "Combined systems" ~ 0 + (0.5 * 7),
      moderator_class == "Cover crops" ~ 0 + (0.5 * 9),
      moderator_class == "Crop rotation" ~ 0 + (0.5 * 11),
      moderator_class == "Embedded seminatural habitats" ~ 0 + (0.5 * 13),
      moderator_class == "Fallow" ~ 0 + (0.5 * 15),
      moderator_class == "Intercropping" ~ 0 + (0.5 * 17),
      moderator_class == "Rotational grazing" ~ 0 + (0.5 * 19),
      TRUE ~ NA_real_))


## BASIC PLOT
#Themes
significance <- c("#F7ADA4","#FF4933","#D3D3D3","#BAF2C4","#256C32")
        

#####  
## LEFT PANEL
plot1<-ggplot(overall,aes(x=factor(ID),y=estimate))+ labs(x=NULL, y=NULL)

  
plot1
plot1 + 
  geom_hline(aes(yintercept=-3.75),linetype=1, size=0.5)+
  geom_hline(aes(yintercept=0),linetype=1, size=1)+
  geom_hline(aes(yintercept=1),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=2),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=3),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=4),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=5),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=6),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=7),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=8),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=9),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=10),linetype=1, size=1)+
  geom_hline(aes(yintercept=11),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=12),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=13),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=14),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=15),linetype=1, size=1)+
  
  #Determinant factors
  geom_text(data=overall,aes(x=factor(ID),y=-0.05,label=pcc_factor_unit),
            vjust=0.4, hjust=1, size=5)+
  coord_flip()+
  #Sub-titles
  geom_image(data=RtLabels, aes(x=34.5, y=y,image=systems_path), size=.05)+
  #Moderator: Diversified farming systems
  geom_point(data=m_dfs,aes(x=factor(ID),y=y,fill=factor(significance2),
                            colour= factor(significance2)),
             size=11.8,shape=21,show.legend = F)+
  geom_image(data=m_dfs, aes(x=factor(ID),y=y,image=icon_n_articles), 
             colour="black",size=.02)+
  scale_colour_manual(values = significance)+
  scale_fill_manual(values = significance)+
  #Moderator: region
  geom_point(data=m_region,aes(x=factor(ID),y=y,fill=factor(significance2),
                            colour= factor(significance2)),
             size=11.8,shape=21,show.legend = F)+
  geom_image(data=m_region, aes(x=factor(ID),y=y,image=icon_n_articles), 
             colour="black",size=.02)+
  #scale_size_binned_area(breaks = c(2,5,10,25, 50,75,100),max_size = 10)+
  scale_y_continuous(position = "right",limit = c(-3.75,15),expand = c(0, 0),
                     breaks=c(-2,3.6,7.25),
                     labels=c("Determinant factors",
                             "Diversified farming system",
                             "Regions")) +
  geom_vline(xintercept = 37.5, linetype = 1, size = 0.5)+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position="none",
    axis.line.y = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks=element_blank(),
    axis.text.x = element_text(face="bold",size=12,family = "sans",colour = "black",
                               vjust = 8),
    axis.line = element_line(size=0.7,colour = "black"),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.ticks.length = unit(0.0001, "mm"),
    panel.background = element_rect(fill = "transparent"), 
    panel.border = element_blank()) +
  geom_vline(xintercept = 31.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 30.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 29.5, linetype = 1, size = 1)+
  geom_vline(xintercept = 28.5, linetype = 1, size = 1)+
  geom_vline(xintercept = 27.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 26.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 25.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 24.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 23.5, linetype = 1, size = 1)+
  geom_vline(xintercept = 22.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 21.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 20.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 19.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 18.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 17.5, linetype = 1, size = 1)+
  geom_vline(xintercept = 16.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 15.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 14.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 13.5, linetype = 1, size = 1)+
  geom_vline(xintercept = 12.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 11.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 10.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 9.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 8.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 7.5, linetype = 1, size = 1)+
  geom_vline(xintercept = 6.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 5.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 4.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 3.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 2.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 1.5, linetype = 1, size = 0.5, colour="grey50")+
  geom_vline(xintercept = 0.5, linetype = 1, size = 1)
  
  
  
#19x23
  
#___________________________
## Overall results for the most studied factors
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")

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

dfs<- ggplot(m_dfs, aes(x=moderator_class ,y=reorder(pcc_factor_unit,ID,decreasing=T))) +
  geom_tile(aes(fill=factor(significance2)),color= "grey45",lwd = 0.5,fill = "white") +
  geom_point(aes(size = factor(icon_n_articles), fill=factor(significance2),
                 colour= factor(significance2)), shape = 21, 
             show.legend=F) +
  scale_fill_manual(values = c("#F7ADA4","#FF4933","#D3D3D3","#BAF2C4","#256C32"))+
  scale_colour_manual(values = c("#F7ADA4","#FF4933","#D3D3D3","#BAF2C4","#256C32"))+
  scale_size_manual(values=c(5,11))+
  
  scale_fill_manual(values = significance)+
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
  geom_point(aes(size = factor(icon_n_articles), fill=factor(significance2),
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