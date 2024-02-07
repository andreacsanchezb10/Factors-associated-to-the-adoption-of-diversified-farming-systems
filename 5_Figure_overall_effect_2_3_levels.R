library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(forcats)

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
#Two-level
pcc_2level<-read.csv("pcc_data_2levels.csv",
                     header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_sub_class.x,pcc_factor_unit) %>%
  dplyr::summarise(n_articles = n_distinct(article_id))

overall_2level_results<-read.csv("overall_results_2levels.csv",
                                          header = TRUE, sep = ",")%>%
  left_join(pcc_2level,by="pcc_factor_unit")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_articles")
  
#Three-level
overall_3level_results<-read.csv("overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","n_ES","n_articles")

sort(unique(overall_3level_results$pcc_factor_unit))

overal_results<- overall_3level_results%>%
  rbind(overall_2level_results)%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  arrange(factor_sub_class,desc(beta))%>%
  mutate_at(vars("n_ES","n_articles"),as.numeric)%>%
  mutate(significance2 = if_else(beta >0 & pval <=0.05, "significant_positive",
                                if_else(beta <0 & pval <=0.05, "significant_negative",
                                        if_else(beta>0&pval>0.05,"no_significant_positive",
                                                "no_significant_negative"))))

#overal_results$factor_sub_class <- toupper(overal_results$factor_sub_class)

overal_results$ID <- as.numeric(seq(70, 1, by = -1)) #add a new column with the effect size ID number

########################################################################################################
############# OVERALL RESULTS ONLY  ########################################################################################################
########################################################################################################
## Overall results for the most studied factors
fills_more10 <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#297d7d")

  
c("#f0c602", "#ea6044","#d896ff","#87CEEB",   "#496491", "#92c46d", "#297d7d")

overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = fills_more10),
  text_y = elem_list_text(size= 1,colour= fills_more10,angle = 90),
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
  #panel.background = element_blank(),
  panel.grid.major  = element_line(color = "grey95",size = 0.6),
  axis.line.x = element_line(colour = "black"))

overall_effect_more10<-
  ggplot(overal_results, 
  #ggplot(subset(overal_results,n_articles>9), 
         aes(y=reorder(pcc_factor_unit, beta),x=beta,xmin=ci.lb, xmax=ci.ub,
             colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=ci.ub+0.01, group=pcc_factor_unit), vjust=0.7, hjust=-0.005,
            color="black", size=7, family="sans",position = (position_dodge(width = -0.5)))+
  scale_colour_manual(values = fills_more10)+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.25,0.5),expand = c(0.05, 0.05),
                     breaks = c(-0.5,-0.25,0,0.25,0.5),
                     labels = c("-0.5","-0.25","0","0.25","0.5"))+
  xlab("Partial Correlation Coefficient (PCC)")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=3.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))
overall_effect_more10

overall_distribution_more10<-
  ggplot(overal_results, 
  
  #ggplot(subset(overal_results,n_articles>9), 
         aes(x=n_articles, y=reorder(pcc_factor_unit, beta),
                                  fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F) +
    geom_point(aes(x=n_ES, y=reorder(pcc_factor_unit, beta),
                   fill = factor(factor_sub_class)),
               shape=18,size=2, position = (position_dodge(width = -0.2)),
               show.legend = F)+
  scale_fill_manual(values = fills_more10)+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("Number")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y=element_line(colour = "grey"),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))+
  scale_x_continuous(
    limit = c(0,140),expand = c(0,0),
    breaks = c(0,25,50,75,100,125),
    labels= c("0","25","50","75","100","125"))
overall_distribution_more10

overall.plot_more10<-ggarrange(overall_effect_more10,overall_distribution_more10,ncol = 2,widths = c(1, 0.3))

overall.plot_more10

library(cowplot)
grob <- ggplotGrob(overall.plot_more10)

# Draw the main plot
grid.newpage()
grid.draw(grob)

# Add "ACCESSIBILITY" outside the plot
grid.text("ACCESSIBILITY", 
          x = unit(0.16, "npc"), 
          y = unit(0.96, "npc"),
          just = "right", 
          gp = gpar(col = "#f0c602", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("BIOPHYSICAL", 
          x = unit(0.16, "npc"), 
          y = unit(0.86, "npc"),
          just = "right", 
          gp = gpar(col = "#ea6044", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("FINANCIAL\nCAPITAL", 
          x = unit(0.16, "npc"), 
          y = unit(0.74, "npc"),
          just = "right", 
          gp = gpar(col = "#d896ff", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("PHYSICAL\nCAPITAL", 
          x = unit(0.16, "npc"), 
          y = unit(0.46, "npc"),
          just = "right", 
          gp = gpar(col = "#87CEEB", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("SOCIAL\nCAPITAL", 
          x = unit(0.16, "npc"), 
          y = unit(0.35, "npc"),
          just = "right", 
          gp = gpar(col = "#496491", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("SOCIO-\nDEMOGRAPHIC", 
          x = unit(0.16, "npc"), 
          y = unit(0.27, "npc"),
          just = "right", 
          gp = gpar(col = "#92c46d", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("TECHNICAL\nINFORMATION", 
          x = unit(0.16, "npc"), 
          y = unit(0.14, "npc"),
          just = "right", 
          gp = gpar(col = "#297d7d", fontsize = 11, family = "sans",fontface = "bold"))



## Overall results for the less studied factors --------
fills_less10 <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",   "#496491", "#92c46d", "#297d7d")

overall_strips_less10 <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = fills_less10),
  text_y = elem_list_text(size= 1,colour= fills_less10,angle = 90),
  by_layer_y = FALSE
)

overall_distribution_strips_less10 <- strip_themed(
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
  #panel.background = element_blank(),
  panel.grid.major  = element_line(color = "grey95",size = 0.6),
  axis.line.x = element_line(colour = "black"))

overall_effect_less10<-
  ggplot(subset(overal_results,n_articles<10), 
         aes(y=reorder(pcc_factor_unit, beta),x=beta,xmin=ci.lb, xmax=ci.ub,
             colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=ci.ub+0.01, group=pcc_factor_unit), vjust=0.7, hjust=-0.005,
            color="black", size=7, family="sans",position = (position_dodge(width = -0.5)))+
  scale_colour_manual(values = fills_less10)+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips_less10)+
  scale_x_continuous(limit = c(-1,1),expand = c(0.05, 0.05),
                     breaks = c(-1,-0.5,0,0.5,1),
                     labels = c("-1","-0.5","0","0.5","1"))+
  xlab("Partial Correlation Coefficient (PCC)")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=3.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))
overall_effect_less10

overall_distribution_less10<-
  ggplot(subset(overal_results,n_articles<10), 
         aes(x=n_articles, y=reorder(pcc_factor_unit, beta),
             fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0.5, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F) +
  scale_fill_manual(values = fills_less10)+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips_less10)+
  xlab("Number")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y=element_line(colour = "grey"),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))+
  scale_x_continuous(
    limit = c(0,140),expand = c(0,0),
    breaks = c(0,25,50,75,100,125),
    labels= c("0","25","50","75","100","125"))
overall_distribution_less10

overall.plot_less10<-ggarrange(overall_effect_less10,overall_distribution_less10,ncol = 2,widths = c(1, 0.3))

overall.plot_less10

grob <- ggplotGrob(overall.plot_less10)

# Draw the main plot
grid.newpage()
grid.draw(grob)

# Add "ACCESSIBILITY" outside the plot
grid.text("ACCESSIBILITY", 
          x = unit(0.16, "npc"), 
          y = unit(0.96, "npc"),
          just = "right", 
          gp = gpar(col = "#f0c602", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("BIOPHYSICAL", 
          x = unit(0.16, "npc"), 
          y = unit(0.86, "npc"),
          just = "right", 
          gp = gpar(col = "#ea6044", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("FINANCIAL\nCAPITAL", 
          x = unit(0.16, "npc"), 
          y = unit(0.74, "npc"),
          just = "right", 
          gp = gpar(col = "#d896ff", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("PERSONAL\nBEHAVIOUR", 
          x = unit(0.16, "npc"), 
          y = unit(0.63, "npc"),
          just = "right", 
          gp = gpar(col = "#6a57b8", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("SOCIAL\nCAPITAL", 
          x = unit(0.16, "npc"), 
          y = unit(0.35, "npc"),
          just = "right", 
          gp = gpar(col = "#496491", fontsize = 11, family = "sans",fontface = "bold"))

grid.text("SOCIO-\nDEMOGRAPHIC", 
          x = unit(0.16, "npc"), 
          y = unit(0.27, "npc"),
          just = "right", 
          gp = gpar(col = "#92c46d", fontsize = 11, family = "sans",fontface = "bold"))


