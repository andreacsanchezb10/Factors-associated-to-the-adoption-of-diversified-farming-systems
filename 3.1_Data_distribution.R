#install.packages("plyr")
#library(Rtools)
#library(readr)
#library(plyr)
#library(readxl)

#library(tidyverse)

#library(readxl)
#library(tidyr)
#library(stringr)

####################### Data distribution ---------------------------------

######### PCC data ---------------------------------
library(pals)
library(RColorBrewer)
library(egg)
library(hrbrthemes)
library(plotly)
library(ggh4x)
library(grid)
library(gtable)

library(dplyr)
library(forcats)
library(ggplot2)
library(readxl)
library(countrycode)


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


sort(unique(pcc_data$x_metric_recla2))
sort(unique(pcc_data$x_metric_recla))

sort(unique(pcc_data$pcc_factor_unit))

### Figure: Represented countries ---------
pcc_data$country[pcc_data$country %in% "Vietnam, Thailand"] <- "Thailand"
pcc_data$country[pcc_data$country %in% "Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia"] <- "Ethiopia"
pcc_data$country[pcc_data$country %in% "Vietnam"] <- "Viet Nam"

sort(unique(pcc_data$pcc_factor_unit))
sort(unique(pcc_data$country)) #42

table(pcc_data$m_region , pcc_data$pcc_factor_unit)
sort(unique(pcc_data$country))

length(unique(pcc_data$article_id)) #153

country<- pcc_data%>%
  group_by(country,m_region)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))

sort(unique(country$m_region))

world <- ggplot2::map_data("world")%>%filter(region != "Antarctica")

world_map <- ggplot2::map_data("world")%>%filter(region != "Antarctica")%>%
  left_join(country, by =  c("region" ="country"))%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate(n_articles_intervals= cut(n_articles,seq(0,8,2)))%>%
  mutate(n_articles_intervals= if_else(n_articles_intervals=="(0,2]", "(1,2]",
                                       n_articles_intervals))%>%
  mutate_all(~replace(., is.na(.), 0))

world<-
  ggplot(data = world_map, aes(x = long, y = lat, group = group, fill = m_region)) +
  geom_polygon(aes(group = group, fill = m_region),colour="grey25",
               size = 0.05, show.legend = TRUE) +
  coord_fixed() +
  scale_fill_manual(
    #labels = c("No data", "1-2", "3-4", "5-6","7-8"),
    #breaks = c("0", "(1,2]", "(2,4]", "(4,6]","(6,8]"),
    values = c("grey95", "#843272","#b5562f","#743341", "#f1ba41",
                 "#5b6454"),
    guide = guide_legend(label.position = "top"))+
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

## Data distribution by m_region 
region<- pcc_data%>%
  group_by(m_region)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  mutate(percentage_ES= (n_ES/sum(n_ES))*100,
         percentage_articles= (n_articles/sum(n_articles))*100)
filter(m_region=="Africa")

## Data distribution by pcc_factor_sub_class 
factor_sub_class<- pcc_data%>%
  group_by(factor_sub_class.x)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  mutate(percentage_ES= (n_ES/sum(n_ES))*100,
         percentage_articles= (n_articles/sum(n_articles))*100)


## Data distribution by m_intervention_recla2 
systems<- pcc_data%>%
  group_by(m_intervention_recla2)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  mutate(percentage_ES= (n_ES/sum(n_ES))*100,
         percentage_articles= (n_articles/sum(n_articles))*100)

sum(systems$n_ES)

## Data distribution by region, factor class, system
region_factor_systems<- pcc_data%>%
  select(ES_ID,m_region, factor_sub_class.y,m_intervention_recla2)

skey_region_factor_systems <- region_factor_systems %>%
  make_long(m_region, factor_sub_class.y,m_intervention_recla2)              


fills <- c("Africa"="#843272","Asia"="#b5562f",
           "Northern America"="#5b6454",
           "Latin America and the Caribbean"= "#f1ba41",
           "Europe"="#743341",
           "Accessibility"= "#f0c602","Biophysical"= "#ea6044","Financial capital"="#d896ff",
           "Physical capital"=  "#87CEEB","Personal behaviour"="#6a57b8",
           "Social capital"="#496491","Socio-demographic"="#92c46d",
           "Technical information"= "#297d7d",
           "Agroforestry"=  "#545454", "Crop rotation"="#545454", 
           "Cover crops"="#545454", "Fallow"="#545454",
           "Intercropping"="#545454",
           "Rotational grazing"="#545454",
           "Combined systems"="#545454",
           "Agro-aquaculture"="#545454","Embedded seminatural habitats"="#545454",
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
plot.margin = unit(c(t=1,r=1,b=1,l=1), "cm"))


######################################################

article_continent<- pcc_data%>%
  #select("id", "country")%>%
  left_join(UN_region, by=c("country" ="Country_Name"))%>%
  group_by(UN_Regions)%>%
  mutate(articles_continent = n_distinct(id))%>%
  mutate(models_continent = n_distinct(id_model_id))%>%
  group_by(articles_continent,models_continent, UN_Regions)%>%
  tally()

article_continent_system<- pcc_data%>%
  #select("id", "country")%>%
  left_join(UN_region, by=c("country" ="Country_Name"))%>%
  group_by(UN_Regions, intervention_recla2)%>%
  mutate(articles_continent = n_distinct(id))%>%
  mutate(models_continent = n_distinct(id_model_id))%>%
  group_by(articles_continent,models_continent, UN_Regions,intervention_recla2)%>%
  tally()


article_factor_continent<- pcc_data%>%
  left_join(UN_region, by=c("country" ="Country_Name"))%>%
  group_by(factor_sub_class,UN_Regions)%>%
  mutate(articles = n_distinct(id))%>%
  mutate(factor_sub_class= if_else(factor_sub_class=="0","prueba",factor_sub_class))%>%
  group_by(factor_sub_class,articles, UN_Regions)%>%
  tally()

article_factor_continent<-as.data.frame(article_factor_continent)%>%
  complete(UN_Regions, factor_sub_class, fill = list(articles = 0, n = 0))




#######































# Data distribution by x_metric_recla2
dist_pcc_factor <-pcc_data%>%
  group_by( x_metric_recla2)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
            n_ES = n_distinct(ES_ID))




  mutate(n_articles_es = paste(n_articles," (",n_ES,")", sep = "" ),
         Total= "Number of articles (number of effect sizes)")%>%
  select(factor_sub_class,x_metric_recla, pcc_factor_unit,n_articles_es,n_articles,Total)%>%
  mutate(factor_sub_class = fct_reorder(factor_sub_class, pcc_factor_unit))

sort(unique(dist_pcc_factor_unit$factor_sub_class))
names(dist_pcc_factor_unit)
plot_pcc_factor_unit
ggplot(dist_pcc_factor_unit, 
       aes(y=pcc_factor_unit,x=Total,
           #colour = n_articles,
           fill= n_articles))+
  geom_tile()+
  scale_fill_gradient(low = "#fdffb6", high = "#5CB270", na.value = "white", guide = "legend")+   # Set up a gradient color scale
  #geom_point(aes(size = n_articles))+
  #scale_size( range = c(5, 20))+
  facet_grid(vars(factor_sub_class),
             scales= "free", space='free_y', switch = "y")+
  geom_text(aes(label = n_articles_es),  size = 4, colour="black")+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), # Adjust margin to create a frame
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y =  element_blank(),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.text.y = element_text(size=11, color="black",family = "sans"),
        strip.text= element_text(size=12, face="bold", color="black",family = "sans"),
        legend.position = "none",
        strip.placement.y = "outside")
geom_hline(yintercept = seq(0.5, nrow(subset_data) - 0.5), color = "black", linetype = "dotted", size = 0.5)

print(plot_pcc_factor_unit)  
#https://github.com/tidyverse/ggplot2/issues/2096
plot_pcc_factor_unit <- ggplot_gtable(ggplot_build(plot_pcc_factor_unit))
print(plot_pcc_factor_unit)
stripr <- which(grepl( 'strip-l',plot_pcc_factor_unit$layout$name))
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8","#87CEEB",  "#85a5cc", "#496491", "#92c46d", "#297d7d")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', plot_pcc_factor_unit$grobs[[i]]$grobs[[1]]$childrenOrder))
  plot_pcc_factor_unit$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(plot_pcc_factor_unit)




# Data distribution by pcc_factor_metric

dist_pcc_factor_unit <-pcc_data%>%
  group_by( factor_sub_class,x_metric_recla, pcc_factor_unit)%>%
  summarise(n_articles = n_distinct(article_id),
            n_ES = n_distinct(ES_ID))%>%
  mutate(n_articles_es = paste(n_articles," (",n_ES,")", sep = "" ),
         Total= "Number of articles (number of effect sizes)")%>%
  select(factor_sub_class,x_metric_recla, pcc_factor_unit,n_articles_es,n_articles,Total)%>%
  mutate(factor_sub_class = fct_reorder(factor_sub_class, pcc_factor_unit))

sort(unique(dist_pcc_factor_unit$factor_sub_class))
names(dist_pcc_factor_unit)
plot_pcc_factor_unit
ggplot(dist_pcc_factor_unit, 
                aes(y=pcc_factor_unit,x=Total,
                    #colour = n_articles,
                    fill= n_articles))+
  geom_tile()+
  scale_fill_gradient(low = "#fdffb6", high = "#5CB270", na.value = "white", guide = "legend")+   # Set up a gradient color scale
  #geom_point(aes(size = n_articles))+
  #scale_size( range = c(5, 20))+
  facet_grid(vars(factor_sub_class),
             scales= "free", space='free_y', switch = "y")+
  geom_text(aes(label = n_articles_es),  size = 4, colour="black")+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), # Adjust margin to create a frame
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y =  element_blank(),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.text.y = element_text(size=11, color="black",family = "sans"),
        strip.text= element_text(size=12, face="bold", color="black",family = "sans"),
        legend.position = "none",
        strip.placement.y = "outside")
  geom_hline(yintercept = seq(0.5, nrow(subset_data) - 0.5), color = "black", linetype = "dotted", size = 0.5)

print(plot_pcc_factor_unit)  
#https://github.com/tidyverse/ggplot2/issues/2096
plot_pcc_factor_unit <- ggplot_gtable(ggplot_build(plot_pcc_factor_unit))
print(plot_pcc_factor_unit)
stripr <- which(grepl( 'strip-l',plot_pcc_factor_unit$layout$name))
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8","#87CEEB",  "#85a5cc", "#496491", "#92c46d", "#297d7d")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', plot_pcc_factor_unit$grobs[[i]]$grobs[[1]]$childrenOrder))
  plot_pcc_factor_unit$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(plot_pcc_factor_unit)

# Distribution by systems by factor_unit

dist_pcc_factor_unit_system <-pcc_data%>%
  group_by(intervention_recla2, factor_sub_class,x_metric_recla,pcc_factor_unit)%>%
  summarise(n_articles = n_distinct(article_id),
            n_ES = n_distinct(ES_ID))%>%
  mutate(n_articles_es = paste(n_articles, " (",n_ES,")", sep = "" ))%>%
  select(intervention_recla2,factor_sub_class,x_metric_recla, pcc_factor_unit,n_articles_es,n_articles )

plot_pcc_factor_unit_system<-ggplot(dist_pcc_factor_unit_system, 
                                    aes(y=pcc_factor_unit,x=intervention_recla2,
                                        fill= n_articles))+ 
  geom_tile()+
  scale_fill_gradient(low = "#fdffb6", high = "#5CB270", na.value = "white", guide = "legend") +  # Set up a gradient color scale
  #geom_point(aes(size = n_articles))+
  #scale_size( range = c(5, 20))+
  facet_grid(vars(factor_sub_class), vars(intervention_recla2),
             scales= "free", space='free_y', switch = "y")+
  geom_text(aes(label = n_articles_es),  size = 3, colour="black")+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), # Adjust margin to create a frame
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y =  element_blank(),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.text.y = element_text(size=10, color="black",family = "sans"),
        strip.text= element_text(size=10, face="bold", color="black",family = "sans"),
        legend.position = "none",
        strip.placement.y = "outside")+
  geom_hline(yintercept = seq(0.5, nrow(subset_data) - 0.5), color = "grey", linetype = "dotted", size = 0.5)

print(plot_pcc_factor_unit_system)  
#https://github.com/tidyverse/ggplot2/issues/2096
plot_pcc_factor_unit_system <- ggplot_gtable(ggplot_build(plot_pcc_factor_unit_system))
print(plot_pcc_factor_unit_system)
stripr <- which(grepl( 'strip-l',plot_pcc_factor_unit_system$layout$name))
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8","#87CEEB",  "#85a5cc", "#496491", "#92c46d", "#297d7d")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', plot_pcc_factor_unit_system$grobs[[i]]$grobs[[1]]$childrenOrder))
  plot_pcc_factor_unit_system$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(plot_pcc_factor_unit_system)





### Figure: Number of articles by country ---------
#Study locations
#install.packages("countrycode")
library(countrycode)
names(pcc_data)
sort(unique(pcc_data$factor_metric))
sort(unique(pcc_data$factor_sub_class))
table(pcc_data$factor_metric , pcc_data$factor_sub_class)
sort(unique(pcc_data$country))

country<- pcc_data%>%
  #select("article_id", "country")%>%
  #left_join(UN_region, by=c("country" ="Country_Name"))%>%
  group_by(country)%>%
  mutate(n_articles = n_distinct(article_id))%>%
  group_by(country,n_articles, UN_Regions,UN_sub_region)%>%
  tally()

sort(unique(country$UN_sub_region))
sort(unique(country$UN_Regions))


length(sort(unique(country$country))) #total number of countries #44


world <- ggplot2::map_data("world")%>%filter(region != "Antarctica")

world_map <- ggplot2::map_data("world")%>%filter(region != "Antarctica")%>%
  left_join(country, by =  c("region" ="country"))%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate(n_articles_intervals= cut(n_articles,seq(0,8,2)))%>%
  mutate(n_articles_intervals= if_else(n_articles_intervals=="(0,2]", "(1,2]",
                                       n_articles_intervals))%>%
  mutate_all(~replace(., is.na(.), 0))

sort(unique(pcc_data$factor_sub_class))
sort(unique(UN_region$UN_Regions))

article_continent<- pcc_data%>%
  #select("id", "country")%>%
  group_by(UN_Regions)%>%
  mutate(articles_continent = n_distinct(article_id))%>%
  mutate(models_continent = n_distinct(ES_ID))%>%
  group_by(articles_continent,models_continent, UN_Regions)%>%
  tally()

article_continent_system<- pcc_data%>%
  #select("id", "country")%>%
  group_by(UN_Regions, intervention_recla2)%>%
  mutate(articles_continent = n_distinct(article_id))%>%
  mutate(models_continent = n_distinct(ES_ID))%>%
  group_by(articles_continent,models_continent, UN_Regions,intervention_recla2)%>%
  tally()

article_factor_continent<- pcc_data%>%
  group_by(factor_sub_class,UN_sub_region)%>%
  mutate(articles = n_distinct(article_id))%>%
  mutate(factor_sub_class= if_else(factor_sub_class=="0","prueba",factor_sub_class))%>%
  group_by(factor_sub_class,articles, UN_sub_region)%>%
  tally()

article_factor_continent<-as.data.frame(article_factor_continent)%>%
  complete(UN_Regions, factor_sub_class, fill = list(articles = 0, n = 0))



world<- ggplot(data = world_map, aes(x = long, y = lat, group = group, fill = UN_sub_region)) +
  geom_polygon(aes(group = group, fill = UN_sub_region,color = "black"), size = 0.05, show.legend = TRUE) +
  coord_fixed() +
  scale_color_manual(values = "grey50")+
  scale_fill_manual(#labels = c("No data", "1-2", "3-4", "5-6","7-8"),
    #breaks = c("0", "(1,2]", "(2,4]", "(4,6]","(6,8]"),
    values = c("white","#479c6c",
               "#ffec51","#fbd1a2",
               "#203993","#ff4365",
               "#ba0019","#0e402a",
               "#945631","#beeba9",
               "#4a2d25","#dba364",
               "#05668d","#fab9c6",
               "#3d405b" ),
    guide = guide_legend(label.position = "top"))+
  labs(fill = "Number of articles")+
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

sort(unique(world_map$UN_sub_region))

names(article_factor_continent)  

x<-ggplot(data=article_factor_continent, aes(x=n, y=factor_sub_class, fill=UN_sub_region)) +
  geom_bar(stat="identity", width=2)+
  scale_fill_manual(#labels = c("No data", "1-2", "3-4", "5-6","7-8"),
    #breaks = c("0", "(1,2]", "(2,4]", "(4,6]","(6,8]"),
    values = c("#479c6c",
               "#ffec51","#fbd1a2",
               "#203993","#ff4365",
               "#ba0019","#0e402a",
               "#945631","#beeba9",
               "#4a2d25","#dba364",
               "#05668d","#fab9c6",
               "#3d405b" ))+
  scale_x_continuous(limits = c(0,700), expand =c(0,0))+
  #geom_text(aes(label = n),
   #         vjust = 0,   position = position_nudge(x = 1)) + 
  
facet_grid(vars(factor_sub_class),
             scales= "free",  switch = "y")+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), # Adjust margin to create a frame
        axis.title.x = element_text(size=11,face="bold", color="black",family = "sans"),
        axis.title.y = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y =  element_blank(),
        axis.text.y = element_text(size=12,face="bold", color="black",family = "sans"),
        strip.text= element_text(size=12, face="bold", color="grey",family = "sans"),
        legend.position = "none",
        strip.placement.y = "outside",
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black"))

print(x)

x <- ggplot_gtable(ggplot_build(x))
print(x)
stripr <- which(grepl( 'strip-l',x$layout$name))
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8","#87CEEB",  "#85a5cc", "#496491", "#92c46d", "#297d7d","black")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', x$grobs[[i]]$grobs[[1]]$childrenOrder))
  x$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(x)  


#https://r-graph-gallery.com/128-ring-or-donut-plot.html
# Create test data.
dist_pcc_factor_subclass <-pcc_data%>%
  group_by( factor_sub_class)%>%
  summarise(n_articles = n_distinct(article_id),
            n_ES = n_distinct(ES_ID))%>%
  mutate(label = paste(n_articles,"\n (",n_ES,")", sep = "" ))%>%
  select(factor_sub_class,label,n_articles,n_ES)%>%
  mutate(percent_ES= (n_ES/sum(n_ES))*100)


# Compute percentages
dist_pcc_factor_subclass$fraction <- dist_pcc_factor_subclass$percent_ES / sum(dist_pcc_factor_subclass$percent_ES)

# Compute the cumulative percentages (top of each rectangle)
dist_pcc_factor_subclass$ymax <- cumsum(dist_pcc_factor_subclass$fraction)

# Compute the bottom of each rectangle
dist_pcc_factor_subclass$ymin <- c(0, head(dist_pcc_factor_subclass$ymax, n=-1))

# Compute label position
dist_pcc_factor_subclass$labelPosition <- (dist_pcc_factor_subclass$ymax + dist_pcc_factor_subclass$ymin) / 2

# Make the plot
ggplot(dist_pcc_factor_subclass, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=factor_sub_class)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=5,col = "white") +
  #geom_text( x=2, aes(y=labelPosition, label=label, color=factor_sub_class), size=6) + # x here controls label position (inner / outer)
  scale_fill_manual= (values=fills) +
  #scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")



ggplot(dist_pcc_factor_subclass, aes(x = 2, y = percent_ES, fill = factor_sub_class)) +
  geom_col(color = "black") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size=4.5,
            fontface="bold") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#f0c602", "#ea6044",
                               "#d896ff","#6a57b8",
                               "#87CEEB",  "#85a5cc",
                               "#496491", "#92c46d",
                               "#297d7d","black")) +
  #scale_fill_brewer(palette = "GnBu") +
  xlim(c(0.2, 2 + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")








####################### CHECK FOR NORMALITY OF THE DATA ---------------------------------
check_models<-meta_data%>%
  group_by(article_id, intervention_recla2) %>%
  summarise(n_models = n_distinct(model_id))%>%
  filter(n_models>1)



#________________________________________________________
#library(dplyr)
#library(parallel)
#library(tidyverse)

####################### EFFECT SIZE calculation---------------------------------
names(meta_data)
table(pcc_data$pcc_factors)


#_______Evaluation of the Normality Assumption in Meta-Analyses
#https://rpubs.com/dylanjcraven/metaforr
perform_analysis_combined <- function(factor_units, pcc_data) {
  par(mfrow=c(1, length(factor_units)))  # Set up a single row of plots
  
  for (factor_unit in factor_units) {
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    rma_model <- rma.mv(yi, vi,
                        random = list(~ 1 | ES_ID, ~ 1 | article_id),
                        tdist = TRUE,
                        data = factor_unit_subset,
                        method = "REML")
    
    summary(rma_model, digits = 3)
    
    qqnorm(residuals(rma_model, type = "rstandard"), 
           main = paste("QQ plot for", factor_unit, "residuals"))
    qqline(residuals(rma_model, type = "rstandard"), col = "red")
  }
  
  par(mfrow=c(1, 1))  # Reset to default plotting layout
}

# Example usage for multiple factor units
factor_metric_units <- unique(pcc_data$pcc_factors)
perform_analysis_combined(factor_units, pcc_data)

#####
perform_analysis_combined_grid <- function(factor_units, pcc_data) {
  num_cols <- 6
  num_rows <- ceiling(length(factor_units) / num_cols)
  par(mfrow = c(num_rows, num_cols), mar = c(3, 3, 1, 1))  # Adjust margins
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    overall_model <- rma.mv(yi, vi,
                        random = list(~ 1 | ES_ID, ~ 1 | article_id),
                        tdist = TRUE,
                        data = factor_unit_subset,
                        method = "REML")
    
    summary(overall_model, digits = 3)
    
    plot_index <- i %% num_cols
    if (plot_index == 0) plot_index <- num_cols
    
    qqnorm(residuals(overall_model, type = "rstandard"), 
           main = paste("QQ plot for", factor_unit, "residuals"))
    qqline(residuals(overall_model, type = "rstandard"), col = "red")
  }
  
  par(mfrow = c(1, 1))  # Reset to default plotting layout
}

# Example usage for multiple factor units
factor_metric_units <- unique(pcc_data$pcc_factor_unit)

perform_analysis_combined_grid(factor_metric_units, pcc_data)




################################  PUBLICATION BIAS ###################################################################3----------------------------------------------------#
##--------- Egger regression test
pcc_data<- pcc_data%>%
  mutate(se = sqrt(vi),
         precision= (1/se))

sort(unique(pcc_data$pcc_factor_unit))
access_credit_egger <- rma.mv(yi, vi,
                         mods = ~ precision, 
                         random = list(~ 1 | ES_ID, ~ 1 | article_id), 
                         tdist=TRUE, 
                         data=pcc_data, method="REML",
                         subset=(pcc_factor_unit=="Access to credit (1= yes)"))
summary(access_credit_egger, digits=3)

access_credit_overall <- rma.mv(yi, vi,
                              random = list(~ 1 | ES_ID, ~ 1 | article_id), 
                              tdist=TRUE, 
                              data=pcc_data,
                              method="REML",
                              subset=(pcc_factor_unit=="Access to credit (1= yes)"))
summary(access_credit_overall, digits=3)
access_credit_resid<-as.data.frame(rstandard.rma.mv(access_credit_overall, type="rstandard"))

pcc_data$precision

plot(pcc_data$precision[pcc_data$pcc_factor_unit%in%"Access to credit (1= yes)"],
     access_credit_resid$resid,
     ylab="residuals", xlab= "precision")




#_______________plots------------

ggplot(overall_results, 
           aes(y=reorder(factor_metric_unit, beta),x=beta,xmin=ci.lb, xmax=ci.ub,
               colour = factor(factor_metric_unit) ))+
  geom_vline(xintercept=0, colour = "grey20",linetype = 1, linewidth=0.7)+
  #geom_errorbar(width=0,size=1, show.legend = TRUE)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_point(size = 4, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=ci.ub+0.01, group=factor_metric_unit), vjust=0.7, hjust=-0.005,
            color="black", size=7, family="sans",position = (position_dodge(width = -0.5)))



articles_count <- PPC_ES %>%
  group_by(factor_sub_class,factor_metric_unit2, factor_context,factor_metric_unit) %>%
  summarise(n_articles = n_distinct(id))

results<- meta_regression_results%>%
  mutate(beta = as.numeric(beta))%>%
  left_join(articles_count, by = c("factor_metric_unit"))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval<=0.1,"","")))))%>%
  select(factor_sub_class,factor_context, factor_metric_unit2, beta, ci.lb, ci.ub,significance,n_articles,k)
  

str(results)
#install.packages("tidyverse")
#results<- results%>%
#  mutate(factor_metric_unit2 = paste(factor_metric_unit2, " [", n_articles, "]", sep = ""))
  
  #mutate(factor_metric_unit2 = paste(factor_metric_unit2, " [", k,", ",n_articles, "]", sep = ""))

#results$factor_metric_unit = with(results, reorder(factor_context,median, factor_metric_unit2))
#results$model <- factor(results$factor_metric_unit2, levels = unique(results$factor_metric_unit2))


#install.packages("pals")
library(ggplot2)
library(pals)
library(RColorBrewer)
library(egg)

dark2_palette <- brewer.pal(n = 8, name = "Dark2")
dark2_palette


### Figure: Number of articles by system, by factor ----
sort(unique(PCC_ES$intervention_recla2))
table(PCC_ES$intervention_recla2)
table(PCC_ES$intervention_recla2)

article_system <- PPC_ES %>%
  group_by(intervention_recla2) %>%
  mutate(articles_system = n_distinct(id))%>%
  mutate(id_model_id = n_distinct(id_model_id))%>%
  group_by(intervention_recla2,id_model_id,articles_system)%>%
  tally()
View(article_system)

article_system_factor <- PPC_ES %>%
  group_by(factor_sub_class,intervention_recla2) %>%
  mutate(articles_system_factor = n_distinct(id))%>%
  group_by(factor_sub_class, intervention_recla2,articles_system_factor)%>%
  tally()%>%
  select(intervention_recla2,factor_sub_class,  articles_system_factor)
  
article_system_factor <- as.data.frame(article_system_factor)%>%
  complete(factor_sub_class,intervention_recla2 , 
           fill = list(articles_system_factor = 0, articles_system = 0))%>%
  left_join(article_system, by= "intervention_recla2")%>%
  select(intervention_recla2,factor_sub_class,  articles_system_factor,articles_system)%>%
  mutate(factor_sub_class= as.factor(factor_sub_class))%>%
  mutate(intervention_recla2= as.factor(intervention_recla2))

str(article_system_factor)

article_system_factor %>%
  mutate(factor_sub_class = fct_reorder(factor_sub_class, articles_system_factor)) %>%
  mutate(intervention_recla2 = fct_reorder(intervention_recla2, -articles_system)) %>%
  ggplot(aes(x=articles_system_factor,
           fill=factor_sub_class)) + 
  geom_bar(aes(y=factor_sub_class),position="dodge", 
           stat="identity", colour="black",width=0.9)+
  scale_y_discrete(expand = c(0,0),position = "left")+
  scale_x_continuous(limit = c(0,48), expand = c(0,0))+
  geom_text(aes(x= articles_system_factor+3,y=factor_sub_class,label = articles_system_factor,group=factor_sub_class),
              position = position_dodge(width = 0.99),color = "black", size = 5,family = "sans") +
  facet_wrap(vars(intervention_recla2), ncol=3,
             strip.position="left")+
  scale_fill_manual(values = c(
    "Accesibility" =  "#f0c602",
    "Agricultural information" ="#ea6044",
    "Biophysical" = "#d896ff",
    "Financial capital" = "#85a5cc",
    "Physical capital" = "#496491",
    "Social capital" = "#92c46d",
    "Socio-demographic" = "#297d7d"))+
  xlab("Number of articles")+
  theme(legend.position = "none",
        strip.placement='outside',
        strip.background =element_blank(),
        strip.text.y.left = element_text(angle = 0,vjust = 1,
                                         size=16, face="bold", color="black",family = "sans"),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(color = "black", size = 13, family = "sans", face = "bold",
                                    vjust = -2), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(color = "black", size = 12, family = "sans", face = "bold"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.line= element_line(size = 1, colour = "black"),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.ticks.x = element_line(size = 1, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))
 

system_count<-PCC_ES %>%
  group_by(intervention_recla,intervention_recla_detail_1, intervention_recla_detail_2, intervention_recla_detail_3) %>%
  summarise(n_articles = n_distinct(id))

write.csv(system_count, "C:/Users/andreasanchez/OneDrive - CGIAR/Documents/1_Chapter_PhD/1_chapter_Data_cleaning/PCC/PCC_system_count.csv", row.names=FALSE)

 


