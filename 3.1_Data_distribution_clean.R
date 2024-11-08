####################### Data distribution ---------------------------------
######### PCC data ---------------------------------
library(pals)
library(RColorBrewer)
library(egg)
library(hrbrthemes)
library(plotly)

library(ggh4x)
library(gtable)
library(dplyr)
library(forcats)
library(ggplot2)
library(readxl)
library(countrycode)
library(geosphere)
library(cowplot)
library(grid)
library(gridExtra) 
library(stringr)

factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

#### PCC data 
pcc_data<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))

pcc_data$factor_sub_class.x <- toupper(pcc_data$factor_sub_class.x)
pcc_data$m_un_region <- toupper(pcc_data$m_un_region)
names(pcc_data)
pcc_data<-pcc_data%>%
  mutate(m_un_region= if_else(m_un_region=="LATIN AMERICA AND THE CARIBBEAN","LATIN\nAMERICA",
                           if_else(m_un_region=="NORTHERN AMERICA","NORTH\nAMERICA",m_un_region)))%>%
  mutate(m_un_subregion= if_else(is.na(m_un_subregion),"Multi sub-regions",m_un_subregion))%>%
  mutate(m_un_subregion = str_replace_all(m_un_subregion, " ", "\n"))%>%
  mutate(m_dp_recla = str_replace_all(m_dp_recla, " ", "\n"))%>%
  
  filter(factor_sub_class.x!="NO SE")%>%
  mutate(factor_sub_class.x= if_else(factor_sub_class.x=="FINANCIAL RISK-MECHANISMS"|
                                       factor_sub_class.x=="KNOWLEDGE ACCESS"|
                                       factor_sub_class.x=="LAND TENURE","POLITICAL AND\nINSTITUTIONAL\nCONTEXT",
                                     factor_sub_class.x))%>%
  mutate(factor_sub_class.x=if_else(factor_sub_class.x=="SOCIAL CAPITAL","SOCIAL\nCAPITAL",
                                    if_else(factor_sub_class.x=="NATURAL CAPITAL","NATURAL\nCAPITAL",
                                            if_else(factor_sub_class.x=="FINANCIAL CAPITAL","FINANCIAL\nCAPITAL",
                                                    if_else(factor_sub_class.x=="PHYSICAL CAPITAL","PHYSICAL\nCAPITAL",
                                                            factor_sub_class.x)))))
  
  
  
length(unique(pcc_data$study_id)) #154
sort(unique(pcc_data$x_metric_recla2))
sort(unique(pcc_data$m_dp_recla))

sort(unique(pcc_data$factor_sub_class.x))

sort(unique(pcc_data$m_un_subregion))
sort(unique(pcc_data$country))

### Figure: Represented countries ---------
pcc_data$country[pcc_data$country %in%"Viet Nam" ] <- "Vietnam"

sort(unique(pcc_data$pcc_factor_unit)) #70
sort(unique(pcc_data$country)) #44
table(pcc_data$m_un_region , pcc_data$pcc_factor_unit)
sort(unique(pcc_data$country))

country<- pcc_data%>%
  group_by(country,m_un_region)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))%>%
  mutate(n_ES_articles= paste(n_ES, "|", n_studies, sep=""))

sort(unique(country$m_un_region))

world <- ggplot2::map_data("world")%>%filter(region != "Antarctica")

world_map <- ggplot2::map_data("world")%>%filter(region != "Antarctica")%>%
  left_join(country, by =  c("region" ="country"))%>%
  mutate_all(~replace(., is.na(.), 0))

centroids <- world_map%>%
  filter(subregion!="Hawaii")%>%
  filter(subregion!="Alaska")%>%
  filter(subregion!="Marion Island")%>%
  group_by(region) %>% 
  group_modify(~ data.frame(centroid(cbind(.x$long, .x$lat))))%>%
  left_join(country, by =  c("region" ="country"))
  
centroids$lon[centroids$region %in% "Malaysia"] <- 102.171799
centroids$lat[centroids$region %in% "Malaysia"] <- 4.166895
centroids$lon[centroids$region %in% "Indonesia"] <- 120.842625
centroids$lat[centroids$region %in% "Indonesia"] <- -2.164827

world<-
  ggplot(data = world_map, aes(x = long, y = lat, group = group, fill = m_un_region)) +
  geom_polygon(aes(group = group, fill = m_un_region),colour="grey25",
               size = 0.05, show.legend = TRUE) +
  coord_fixed() +
  scale_fill_manual(
    #labels = c("No data", "1-2", "3-4", "5-6","7-8"),
    #breaks = c("0", "(1,2]", "(2,4]", "(4,6]","(6,8]"),
    values = c("grey95", "#843272","#b5562f","#743341", "#f1ba41",
                 "#5b6454"),
    guide = guide_legend(label.position = "top"))+
  
  geom_point(data = centroids, 
             aes(x= lon, y=lat, group=region,size =n_ES), 
             shape=21,fill="black",color="grey80",stroke=1.5, alpha = 0.5,show.legend = T)+
  
  scale_size_continuous(limits=c(1,361),breaks = c(5,10,25,50,100,200),
                        name = "Number of effect sizes",range = c(3, 13))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))+
  labs(x = NULL, y = NULL)
world

legend_ES<- ggplot()+
  geom_point(data = centroids, 
             aes(x= lon, y=lat, group=region,size =n_ES), 
             shape=21,fill="black",color="white", alpha = 0.5)+
  scale_size_continuous(limits=c(1,361),breaks = c(5,10,25,50,100,200),
                        labels=c("≤5","10","25","50","100","≥200"),
                        name = "Effect sizes",range = c(3, 13))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.title=element_text(size=9,face="bold"))+
  guides(size = guide_legend(title.position = "left",
                             title.hjust = 0.5, label.position = "left", nrow = 1,
                             label.hjust = 1))

legend_ES
legend_ES <- get_legend(legend_ES)
grid.newpage()
grid.draw(legend_ES)

## Data distribution by m_region 
region<- pcc_data%>%
  group_by(m_un_region)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))%>%
  mutate(percentage_ES= (n_ES/sum(n_ES))*100,
         percentage_articles= (n_studies/sum(n_studies))*100)

sum(region$percentage_ES)
sum(region$n_ES)

## Data distribution by pcc_factor_sub_class 
factor_sub_class<- pcc_data%>%
  group_by(factor_sub_class.x)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))%>%
  mutate(percentage_ES= (n_ES/sum(n_ES))*100,
         percentage_studies= (n_studies/sum(n_studies))*100)

sum(factor_sub_class$n_ES)

## Data distribution by m_dp_recla 
systems<- pcc_data%>%
  group_by(m_dp_recla)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_ES = n_distinct(ES_ID))%>%
  mutate(percentage_ES= (n_ES/sum(n_ES))*100,
         percentage_studies= (n_studies/sum(n_studies))*100)

sum(systems$n_ES)

## Data distribution by region, factor class, system
library(ggsankey)

region_factor_systems<- pcc_data%>%
  select(ES_ID,m_un_region, factor_sub_class.x,m_dp_recla)

skey_region_factor_systems <- region_factor_systems %>%
  make_long(m_un_region, factor_sub_class.x,m_dp_recla)              

sort(unique(skey_region_factor_systems$node))
sort(unique(skey_region_factor_systems$next_node))


fills <- c("AFRICA"="#843272",
           "ASIA"="#b5562f",
           "NORTH\nAMERICA"="#5b6454",
           "LATIN\nAMERICA"= "#f1ba41",
           "EUROPE"="#743341",
           "BIOPHYSICAL CONTEXT"= "#f0c602",
           "FARM MANAGEMENT"="#F09319",
           "FARMERS ATTITUDES"= "#ea6044",
           "FINANCIAL\nCAPITAL"="#d896ff",
           "NATURAL\nCAPITAL"=  "#87CEEB",
           "HUMAN CAPITAL"="#6a57b8",
           "PHYSICAL\nCAPITAL"="#496491",
           "POLITICAL AND\nINSTITUTIONAL\nCONTEXT"="#92c46d",

           "SOCIAL\nCAPITAL"= "#297d7d",
           "Agroforestry"=  "#545454", "Crop\nrotation"="#545454", 
           "Cover\ncrops"="#545454", "Fallow"="#545454",
           "Intercropping"="#545454",
           "Rotational\ngrazing"="#545454",
           "Combined\npractices"="#545454",
           "Agro-aquaculture"="#545454","Embedded\nseminatural\nhabitats"="#545454",
           "Agro-silvopasture"="#545454")


ggplot(skey_region_factor_systems, 
       aes(x = x,         next_x = next_x, node = node,
           next_node = next_node,
           fill = node,
           colour=node),
       label = node) +
  geom_sankey(flow.alpha = 0.4,
              #space = 15,
              #node.color = "black",
              show.legend = FALSE)+
  scale_fill_manual(values= fills)+
  scale_colour_manual(values= fills)+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_discrete(expand = c(0, 0))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    
    axis.title = element_blank(), 
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "transparent"))
#16*12