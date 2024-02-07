library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(forcats)


####################################################################################################################################
################ META-REGRESSION FIGURES ############################################################################################
##############################################################################################################################
factors_metric_assessed <- read_excel("C:/Users/AndreaSanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/Meta_data_2024.01.25.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

#### PCC data 
pcc_data<- read.csv("pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("pcc_data_2levels.csv",header = TRUE, sep = ","))

#### Overall results
#Two-levels
pcc_2level<-read.csv("pcc_data_2levels.csv",header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_sub_class.x,pcc_factor_unit) %>%
  dplyr::summarise(n_articles = n_distinct(article_id))

overall_2level_results<-read.csv("overall_results_2levels.csv",header = TRUE, sep = ",")%>%
  left_join(pcc_2level,by="pcc_factor_unit")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_articles")
  
#Three-level
overall_3level_results<-read.csv("overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_articles")

#### OVERALL RESULTS
overall_results<- overall_3level_results%>%
  rbind(overall_2level_results)%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  arrange(factor_sub_class,desc(beta))%>%
  mutate_at(vars("n_ES","n_articles"),as.numeric)%>%
  mutate(significance2 = if_else(beta >0 & pval <=0.05, "significant_positive",
                                if_else(beta <0 & pval <=0.05, "significant_negative",
                                        if_else(beta>0&pval>0.05,"no_significant_positive",
                                                "no_significant_negative"))))%>%
  mutate(icon_n_articles= if_else(n_articles>10,
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/more10.png",
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/less10.png" ))


overall_results_more10<- overall_results%>%
  filter(n_articles>9)

overall_results_more10$ID <- as.numeric(seq(39, 1, by = -1)) #add a new column with the effect size ID number

#### Meta-regression results
#Two-level
meta_regression_2levels<-read.csv("meta_regression_2levels.csv",header = TRUE, sep = ",")%>%
  select(pcc_factor_unit, factor_sub_class,moderator, moderator_class,beta, significance2,pval)

sort(unique(meta_regression_2levels$pcc_factor_unit))
#Three-level
meta_regression_3levels<-read.csv("meta_regression_3levels.csv",header = TRUE, sep = ",")%>%
  select(pcc_factor_unit, factor_sub_class,moderator, moderator_class,beta, significance2,pval)
sort(unique(meta_regression_3levels$pcc_factor_unit))

meta_regression<- meta_regression_3levels%>%
  rbind(meta_regression_2levels)%>%
  arrange(factor_sub_class, moderator, 
          moderator_class, desc(beta))

sort(unique(meta_regression$pcc_factor_unit))

#Moderator: farm size------
m_farm_size_distribution<-pcc_data%>%
  mutate(m_mean_farm_size_ha= as.numeric(m_mean_farm_size_ha))%>%
  filter(!is.na(m_mean_farm_size_ha))%>%
  group_by( pcc_factor_unit)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))
  mutate(moderator_class="m_mean_farm_size_ha")

names(m_farm_size_distribution)

m_farm_size<- meta_regression%>%
  filter(moderator== "m_mean_farm_size_ha")%>%
  filter(moderator_class != "intrcpt")%>%
  #dplyr::left_join(select(overall_results_more10, c(ID,pcc_factor_unit)), 
   #                by="pcc_factor_unit")%>%
  dplyr::left_join(m_farm_size_distribution, 
                   by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  mutate(icon_n_articles= if_else(n_articles>=10,
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/more10.png",
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/less10.png" ))

m_farm_size$ID <- as.numeric(seq(32, 1, by = -1))

#Moderator: diversified farming systems------
m_dfs_distribution<-pcc_data%>%
  group_by( pcc_factor_unit, m_intervention_recla2)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_intervention_recla2")
names(pcc_data)

m_dfs<- meta_regression%>%
  filter(moderator== "m_intervention_recla2")%>%
  left_join(select(m_farm_size, c(ID,pcc_factor_unit)), 
            by=c("pcc_factor_unit"="pcc_factor_unit"))%>%
  mutate(
    y = case_when(
      moderator_class == "Agro-aquaculture" ~ 1 + (0.3 *1),
      moderator_class == "Agro-silvopasture" ~ 1 + (0.3 *3),
      moderator_class == "Agroforestry" ~ 1 + (0.3 *5),
      moderator_class == "Combined systems" ~ 1 + (0.3 * 7),
      moderator_class == "Cover crops" ~ 1 + (0.3 * 9),
      moderator_class == "Crop rotation" ~ 1 + (0.3 * 11),
      moderator_class == "Embedded seminatural habitats" ~ 1 + (0.3 * 13),
      moderator_class == "Fallow" ~ 1 + (0.3 * 15),
      moderator_class == "Intercropping" ~ 1 + (0.3 * 17),
      moderator_class == "Rotational grazing" ~ 1 + (0.3 * 19),
      TRUE ~ NA_real_))%>%
  left_join(m_dfs_distribution, by=c("pcc_factor_unit"="pcc_factor_unit",
                                     "moderator_class"="moderator_class"))%>%
  filter(!is.na(ID))%>%
  mutate(icon_n_articles= if_else(n_articles>=10,
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/more10.png",
                                  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/icons_significance/less10.png" ))

m_dfs <- complete(m_dfs, pcc_factor_unit, moderator_class, fill = list(beta = NA))


sort(unique(m_dfs$moderator_class))

#Moderator: Region------
m_region_distribution<-pcc_data%>%
  group_by( pcc_factor_unit, m_region)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  dplyr::rename("moderator_class"="m_region")

m_region<- meta_regression%>%
  filter(moderator== "m_region")%>%
  left_join(select(overall_results_more10, c(ID,pcc_factor_unit)), by="pcc_factor_unit")%>%
  mutate(
    y = case_when(
      moderator_class == "Africa" ~ 6 + (0.25 *1),
      moderator_class == "Asia" ~ 6 + (0.25 * 3),
      moderator_class == "Europe" ~ 6 + (0.25 * 5),
      moderator_class == "Latin America and the Caribbean" ~ 6 + (0.25 * 7),
      moderator_class == "Northern America" ~ 6 + (0.25 * 9),
      TRUE ~ NA_real_))%>%
  left_join(m_region_distribution, by=c("moderator_class","pcc_factor_unit"))%>%
  filter(!is.na(ID))

sort(unique(m_region$moderator_class))
  

#https://stackoverflow.com/questions/15420621/reproduce-table-and-plot-from-journal -----
## SYNTHESIZE SOME PLOT DATA 
## OVERALL RESULTS
overall<-m_farm_size%>%
  select(ID,beta, pcc_factor_unit)

overall<-rbind(overall,data.frame(ID=c(max(m_farm_size$ID)+1,
                                       max(m_farm_size$ID)+2),
                                       #max(overal_results$ID)+3),
                                       #max(overal_results$ID)+4),
                                  beta=NA,
                                  pcc_factor_unit=NA))
#overall<-overall%>%
 # left_join(overall_results_more10, by= c("ID","beta", "ci.lb","ci.ub" ))


## identify the rows to be highlighted, and 
## build a function to add the layers

# Create a data frame with alternating colors
#hl_rows<-data.frame(ID=(1:floor(length(unique(overall$ID[which(overall$ID>0)]))/2))*2,
#                    col="lightgrey")
#hl_rows$ID<-hl_rows$ID+blankRows+1
#hl_rows<-rbind(hl_rows,c("71","black"))
#hl_rows$col[hl_rows$ID %in% "69"]<- "white"

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
      moderator_class == "Agro-aquaculture" ~ 1 + (0.3 *1),
      moderator_class == "Agro-silvopasture" ~ 1 + (0.3 *3),
      moderator_class == "Agroforestry" ~ 1 + (0.3 *5),
      moderator_class == "Combined systems" ~ 1 + (0.3 * 7),
      moderator_class == "Cover crops" ~ 1 + (0.3 * 9),
      moderator_class == "Crop rotation" ~ 1 + (0.3 * 11),
      moderator_class == "Embedded seminatural habitats" ~ 1 + (0.3 * 13),
      moderator_class == "Fallow" ~ 1 + (0.3 * 15),
      moderator_class == "Intercropping" ~ 1 + (0.3 * 17),
      moderator_class == "Rotational grazing" ~ 1 + (0.3 * 19),
      TRUE ~ NA_real_))


## BASIC PLOT
#Themes
significance <- c("#F7ADA4","#BAF2C4","#FF4933","#256C32")
        

#####  
## LEFT PANEL
plot1<-ggplot(overall,aes(x=factor(ID),y=beta))+ labs(x=NULL, y=NULL)

  
plot1
plot1 + 
  apply(hl_rows, 1,
        function(x) annotation_custom(
          hl_rect(x["col"], alpha = 0.3, height = x["height"], width = x["width"]),
          as.numeric(x["ID"]) - 0.5,
          as.numeric(x["ID"]) + 0.5, -20, 20
        )) +
  geom_hline(aes(yintercept=0),linetype=1, size=0.5)+
  geom_hline(aes(yintercept=1),linetype=1, size=0.5)+
  geom_hline(aes(yintercept=1.6),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=2.2),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=2.8),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=3.4),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=4),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=4.6),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=5.2),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=5.8),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=6.4),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=7),linetype=1, size=0.5)+
  geom_hline(aes(yintercept=8),linetype=1)+
  geom_hline(aes(yintercept=8.8),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=9.4),linetype=2, size=0.5, colour="grey50")+
  geom_hline(aes(yintercept=10),linetype=1)+
  geom_hline(aes(yintercept=10.6),linetype=1, size=0.5)+
  #Determinant factors
  geom_text(data=overall,aes(x=factor(ID),y=-0.05,label=pcc_factor_unit),
            vjust=0.4, hjust=1, size=5)+
  coord_flip()+
  
  #Sub-titles
  geom_image(data=RtLabels, aes(x=33.5, y=y,image=systems_path), size=.05)+
  #Moderator: Farm size
  geom_point(data=m_farm_size,aes(x=factor(ID),y=0.5,fill=factor(significance2),
                                  colour= factor(significance2)),
             size=9,shape=21,show.legend = F)+
  geom_image(data=m_farm_size, aes(x=factor(ID),y=0.5,image=icon_n_articles), 
             colour="black",size=.02)+
  #Moderator: Diversified farming systems
  geom_point(data=m_dfs,aes(x=factor(ID),y=y,fill=factor(significance2),
                            colour= factor(significance2)),
             size=9,shape=21,show.legend = F)+
  geom_image(data=m_dfs, aes(x=factor(ID),y=y,image=icon_n_articles), 
             colour="black",size=.02)+
  scale_colour_manual(values = significance)+
  scale_fill_manual(values = significance)+
  
  #scale_size_binned_area(breaks = c(2,5,10,25, 50,75,100),max_size = 10)+
  scale_y_continuous(position = "right",limit = c(-3.75,7),expand = c(0, 0),
                     breaks=c(-2,0.5,3.5,7.25,9,10,10.5),
                     labels=c("Determinant factors",
                             "Farm size\n(ha)",
                             "Diversified farming system",
                             "Regions",
                             "Farm size\n(ha)",
                             "Education\n(years)",
                             "")) +
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
  geom_vline(xintercept = 37.5, linetype = 1, size = 0.5)+
geom_vline(xintercept = 34.5, linetype = 1, size = 0.5)+
  geom_vline(xintercept = 28.5, linetype = 1, size = 0.5)+
  geom_vline(xintercept = 6.5, linetype = 1, size = 0.5)

  
  
  

  

  #Moderator: region
  geom_point(data=m_region,aes(x=factor(ID),y=y,fill=factor(significance2),size=n_articles),
             shape=21,show.legend = F)+
  #Moderator: Farm size
  geom_point(data=m_farm_size,aes(x=factor(ID),y=9,fill=factor(significance2),size=n_articles),
             shape=21,show.legend = F) +
  scale_fill_manual(values = significance)+
  scale_size_binned_area(breaks = c(2,5,10,25, 50,75,100),max_size = 10)+
  scale_y_continuous(position = "right",limit = c(-3.75,6),expand = c(0, 0),
                     breaks=c(-2,0.5,3.5,7.25,9,10,10.5),
                     labels=c("Determinant factors",
                             "Overall\neffect",
                             "Diversified farming system",
                             "Regions",
                             "Farm size\n(ha)",
                             "Education\n(years)",
                             "")) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
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
  geom_vline(xintercept = 37.5, linetype = 1, size = 0.5)+
geom_vline(xintercept = 34.5, linetype = 1, size = 0.5)+
  geom_vline(xintercept = 28.5, linetype = 1, size = 0.5)


    #panel.grid.major = element_line(colour="grey"), 
    #panel.grid.minor = element_line(colour="grey"), 
    panel.margin = unit(c(0,-0.1,-0.1,-0.1), "mm")) 
    plot.margin = unit(c(t=5,r=0.5,b=0.5,l=0.5), "mm"))
  





  #Moderator: Education
  geom_point(data=m_farm_size,aes(factor(ID),8.1),
             size=7,shape=1,show.legend = F) +
  geom_text(data=m_farm_size,
            aes(x=factor(ID),y=8.1, colour=significance,label=sign, fontface="bold"), 
            hjust=0.5, vjust=0.4, size=5,show.legend = F) +
  #scale_x_discrete(limits=rev)+
  
  


