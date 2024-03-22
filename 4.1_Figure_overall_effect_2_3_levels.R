library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(forcats)

factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

#### PCC data 
pcc_data<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))

  names(pcc_data)
#### Overall results
#Two-level
pcc_2level<-read.csv("data/pcc_data_2levels.csv",
                     header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_sub_class.x,pcc_factor_unit) %>%
  dplyr::summarise(n_articles = n_distinct(article_id))

overall_2level_results<-read.csv("results/overall_results_2levels.csv",
                                          header = TRUE, sep = ",")%>%
  left_join(pcc_2level,by="pcc_factor_unit")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_articles",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")
  
#Three-level
overall_3level_results<-read.csv("results/overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_articles",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")


sort(unique(overall_3level_results$pcc_factor_unit))

overal_results<- overall_3level_results%>%
  rbind(overall_2level_results)%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  arrange(factor_sub_class,desc(pcc.beta))%>%
  mutate_at(vars("n_ES","n_articles"),as.numeric)%>%
  mutate(significance2 = if_else(pcc.beta >0 & pval <=0.05, "significant_positive",
                                if_else(pcc.beta <0 & pval <=0.05, "significant_negative",
                                        if_else(pcc.beta>0&pval>0.05,"no_significant_positive",
                                                "no_significant_negative"))))%>%
  mutate(pcc.ci.lb_l = ifelse(pcc.ci.lb < -0.27, -0.27, NA),
         pcc.ci.ub_l = ifelse(pcc.ci.ub > 0.75, 0.75, NA))%>%
  mutate(pcc.ci.ub_l1= ifelse(pcc_factor_unit=="Soil slope (1= steep)", pcc.ci.ub,
                              ifelse(pcc_factor_unit=="Attitude toward practice (positive continuous)",pcc.ci.ub,
                                     ifelse(pcc_factor_unit=="Perceived benefit (1= erosion reduction)",pcc.ci.ub,
                                            ifelse(pcc_factor_unit=="Plot size (continuous)",pcc.ci.ub,
                                                   ifelse(pcc_factor_unit=="Access to irrigation (1= yes)",pcc.ci.ub,
                                     NA))))))%>%
  mutate(pcc.ci.lb_l1= ifelse(pcc_factor_unit=="Non-farm income (continuous)", pcc.ci.lb,NA))

#overal_results$factor_sub_class <- toupper(overal_results$factor_sub_class)

overal_results$ID <- as.numeric(seq(70, 1, by = -1)) #add a new column with the effect size ID number

########################################################################################################
############# OVERALL RESULTS ONLY  ########################################################################################################
########################################################################################################
## Overall results for the most studied factors
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#297d7d")

  
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
  axis.title.x = element_text(color="black",size=13, family = "sans", face = "bold",vjust = -1),
  axis.text.x =element_text(color="black",size=12, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.background = element_blank(),
  panel.grid.major  = element_line(color = "grey85",size = 0.6),
  axis.line = element_line(colour = "black"))

overall_effect<-
  ggplot(overal_results, 
         aes(y=reorder(pcc_factor_unit, beta),x=pcc.beta,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,
             colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_text(aes(label=significance1, x=pcc.ci.ub+0.01, group=pcc_factor_unit), 
            vjust=0.35, hjust=-0.005,size=10,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
    geom_segment(aes(y = reorder(pcc_factor_unit, pcc.beta),
                     yend = reorder(pcc_factor_unit, pcc.beta),
                     x=pcc.beta, xend = pcc.ci.lb_l),show.legend = F,size=1,
                 arrow = arrow(length = unit(0.2, "cm")))+
    geom_segment(aes(y = reorder(pcc_factor_unit, pcc.beta),
                     yend = reorder(pcc_factor_unit, beta),
                     x=pcc.beta, xend = pcc.ci.ub_l),show.legend = F,size=1,
                 arrow = arrow(length = unit(0.2, "cm")))+
    geom_segment(aes(y = reorder(pcc_factor_unit, pcc.beta),
                     yend = reorder(pcc_factor_unit, pcc.beta),
                     x=pcc.beta, xend = pcc.ci.ub_l1),show.legend = F,size=1)+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc.beta),
                   yend = reorder(pcc_factor_unit, pcc.beta),
                   x=pcc.beta, xend = pcc.ci.lb_l1),show.legend = F,size=1)+
  scale_colour_manual(values = fills)+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.5,-0.25,0,0.25,0.5,0.75),
                     labels = c("-0.5","-0.25","0","0.25","0.5","0.75"))+
  xlab("Partial Correlation Coefficient (PCC)")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=3.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))
overall_effect

overall_distribution<-
  ggplot(overal_results, 
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
  scale_fill_manual(values = fills)+
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
overall_distribution

overall.plot<-ggarrange(overall_effect,overall_distribution,ncol = 2,widths = c(1, 0.25))

overall.plot

#1200 1700



overall_distribution_legend<- ggplot(
  subset(overal_results, factor_sub_class %in% c("Accessibility")),
                                       aes(x=n_articles, y=reorder(pcc_factor_unit, beta),
                                         fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = T) +
  geom_point(aes(x=n_ES, y=reorder(pcc_factor_unit, beta),
                 fill = factor(factor_sub_class)),
             shape=18,size=2, position = (position_dodge(width = -0.2)),
             show.legend = T)+
  scale_fill_manual(values = fills)+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("Number")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y=element_line(colour = "grey"),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"),
        legend.text = element_text(size = 8))+
  scale_x_continuous(
    limit = c(0,140),expand = c(0,0),
    breaks = c(0,25,50,75,100,125),
    labels= c("0","25","50","75","100","125"))

overall_distribution_legend
overall_distribution_legend <- get_legend(overall_distribution_legend)
grid.newpage()
grid.draw(overall_distribution_legend)
