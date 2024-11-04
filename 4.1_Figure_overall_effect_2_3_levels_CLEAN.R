library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(forcats)

factors_metric_assessed <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/evidence_paper/Meta_data_2024.02.15.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$pcc_unit,")", sep="")
factors_metric_assessed$logor_factor_unit <- paste(factors_metric_assessed$x_metric_recla2," (",factors_metric_assessed$logor_unit,")", sep="")
names(factors_metric_assessed)
#### PCC data ----
pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit,x_metric_recla2,pcc_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

pcc_data<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))

#### Overall results
#Two-level
pcc_2level<-read.csv("data/pcc_data_2levels.csv",
                     header = TRUE, sep = ",")  %>%
  dplyr::group_by(factor_sub_class.x,pcc_factor_unit) %>%
  dplyr::summarise(n_studies = n_distinct(study_id))

overall_2level_results<-read.csv("results/overall_results_2levels.csv",
                                          header = TRUE, sep = ",")%>%
  left_join(pcc_2level,by="pcc_factor_unit")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_studies",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")%>%
  filter(pcc_factor_unit!="Attitude toward practice (positive continuous)" )

sort(unique(overall_2level_results$pcc_factor_unit))

  
#Three-level
overall_3level_results<-read.csv("results/overall_results_3levels.csv",header = TRUE, sep = ",")%>%
  select("pcc_factor_unit", "beta","ci.lb","ci.ub","zval", "pval","significance","significance1","n_ES","n_studies",
         "pcc.beta","pcc.ci.lb","pcc.ci.ub")


sort(unique(overall_3level_results$pcc_factor_unit))

overal_results<- overall_3level_results%>%
  rbind(overall_2level_results)%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  mutate_at(vars("n_ES","n_studies"),as.numeric)%>%
  mutate(significance2 = if_else(pcc.beta >0 & pval <=0.05, "significant_positive",
                                if_else(pcc.beta <0 & pval <=0.05, "significant_negative",
                                        if_else(pcc.beta>0&pval>0.05,"no_significant_positive",
                                                "no_significant_negative"))))%>%
  mutate(pcc.ci.lb_l = ifelse(pcc.ci.lb < -0.27, -0.27, NA),
         pcc.ci.ub_l = ifelse(pcc.ci.ub > 0.75, 0.75, NA))%>%
  mutate(pcc.ci.ub_l1= ifelse(pcc_factor_unit=="Soil slope (Steep)", pcc.ci.ub,
                                     ifelse(pcc_factor_unit=="Perceived benefit from practice (Erosion reduction)",pcc.ci.ub,
                                            ifelse(pcc_factor_unit=="Plot size (Plot size)",pcc.ci.ub,
                                                   ifelse(pcc_factor_unit=="Access to irrigation (Access to irrigation)",pcc.ci.ub,
                                                          NA)))))%>%
  mutate(pcc.ci.lb_l1= ifelse(pcc_factor_unit=="Plot size (Plot size)", pcc.ci.lb,
                              if_else(pcc_factor_unit=="Land tenure security (Secure tenure)", pcc.ci.lb,
                                      NA)))%>%
  mutate(factor_sub_class= if_else(factor_sub_class=="Financial risk-mechanisms","Political_1",
                                   if_else(factor_sub_class=="Knowledge access","Political_2",
                                           if_else(factor_sub_class=="Land tenure","Political_3",
                                                   factor_sub_class))))%>%
  #mutate(significance1= if_else(pval>0.05&pval<=0.1,"†",""))%>%
  arrange(desc(pcc.beta))%>%
  mutate(pcc_factor_unit2= seq(70, 1 ))%>%
  mutate(label= paste("(",n_studies,"|",n_ES,")",sep=""))


#Figure 2 ----
overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Political_2"] <- "1Political_2"

overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Extension services"] <- "1Extension services"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Access to information"] <- "2Access to information"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Access to training"] <- "3Access to training"

overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Social capital"] <- "2Social capital"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Association membership"] <- "4Association\nmembership"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Communicate with other farmers"] <- "5Communicate with\n other farmers"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Relatives and friends"] <- "6Relatives and friends"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Trust in extension services"] <- "7Trust in extension\nservices"

overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Farmers behaviour"] <- "3Farmers behaviour"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Attitude toward practice"] <- "8Attitude toward practice"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Attitude to risk"] <- "90Attitude to risk"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Perceived production constraint"] <- "91Perceived production constraint"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Awareness"] <- "92Awareness"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Perceived benefit from practice"] <- "93Perceived benefit from practice"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Perceived constraint from practice"] <- "94Perceived constraint from practice"

overal_results$pcc_unit[overal_results$pcc_factor_unit %in%"Perceived benefit from practice (Environmental)"]<-"95Evironmental"
overal_results$pcc_unit[overal_results$pcc_factor_unit %in%"Perceived benefit from practice (Financial)"]<-"96Financial"
overal_results$pcc_unit[overal_results$pcc_factor_unit %in%"Perceived benefit from practice (Soil fertility)"]<-"97Soil fertility"
overal_results$pcc_unit[overal_results$pcc_factor_unit %in%"Perceived benefit from practice (Erosion reduction)"]<-"98Erosion reduction"

#Figure 3 -----
overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Political_3"] <- "1Political_3"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Land tenure security"]<-"1Land tenure security"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Land tenure status"]<-"2Land tenure status"

overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Biophysical context"] <- "2Biophysical context"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Soil depth"] <- "3Soil depth"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Soil fertility"] <- "4Soil fertility"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Soil slope"] <- "5Soil slope"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Climate"] <- "6Climate"
overal_results$pcc_unit[overal_results$x_metric_recla2%in% "5Soil slope"& overal_results$pcc_unit%in% "Moderate"] <- "SSModerate"
overal_results$pcc_unit[overal_results$x_metric_recla2%in% "3Soil depth"& overal_results$pcc_unit%in% "Moderate"] <- "SDModerate"

names(overal_results)
overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Natural capital"] <- "3Natural capital"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Plot size"] <- "7Plot size"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Farm size"] <- "8Farm size"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Number of plots"] <- "9Number of plots"

#Figure 4----

overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Human capital"] <- "1Human capital"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Household head"] <- "1Household head"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Household"] <- "2Household"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Hired labour"] <- "3Hired labour"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Farm labour"] <- "4Farm labour"

"Farm labour"
overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Financial capital"] <- "2Financial capital"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Non-farm income"] <- "5Non-farm income"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "On-farm income"] <- "6On-farm income"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Total income"] <- "7Total income"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Livestock"] <- "8Livestock"


overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Political_1"] <- "3Political_1"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Receive incentive for conservation"] <- "90Receive incentive for conservation"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Access to credit is a constraint"] <- "91Access to credit is a constraint"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Access to credit"] <- "92Access to credit"

overal_results$factor_sub_class[overal_results$factor_sub_class %in% "Physical capital"] <- "4Physical capital"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Distance to market"] <- "93Distance to market"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Distance to road"] <- "94Distance to road"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Distance to farm-house"] <- "95Distance to farm-house"
overal_results$x_metric_recla2[overal_results$x_metric_recla2 %in% "Access to irrigation"] <- "96Access to irrigation"



sort(unique(overal_results$factor_sub_class))
sort(unique(overal_results$significance1))
sort(unique(overal_results$label))


########################################################################################################
############# OVERALL RESULTS   ########################################################################################################
########################################################################################################
# Define themes and strips
overall_strips <- strip_themed(
  background_y = elem_list_rect(fill = c("black")),
  text_y = elem_list_text(size= 0.0005, colour= c("black"), angle = 90),
  by_layer_y = FALSE
)

overall_distribution_strips <- strip_themed(
  background_y = elem_list_rect(fill = "white"),
  text_y = elem_list_text(size= 0.1, colour= "white", angle = 90),
  by_layer_y = FALSE
)

theme_overall <- theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(color="black", size=13, family = "sans", face = "bold", vjust = -1),
  axis.text.x = element_text(color="black", size=12, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.background = element_blank(),
  panel.grid.major.x = element_line(color = "grey85", size = 0.6),
  axis.line = element_line(colour = "black")
)


#Figure 2 -----
figure2<-
  ggplot(
    subset(overal_results,factor_sub_class%in%
             c("1Political_2",
               "3Farmers behaviour","2Social capital"
             )),
    aes(y=reorder(pcc_unit, pcc_factor_unit2),x=pcc.beta,
        xmin=pcc.ci.lb, xmax=pcc.ci.ub,
        colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l1),show.legend = F,size=1)+
  scale_colour_manual(values = c( "#92c46d","#297d7d","#ea6044"))+
  facet_grid2(vars(x_metric_recla2),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.50,-0.25,0,0.25,0.50,0.75),
                     labels = c("-0.50","-0.25","0","0.25","0.50","0.75"))+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=14, family = "sans"))
figure2

figure2_distribution<-
ggplot(
  subset(overal_results,factor_sub_class%in%
           c("1Political_2","3Farmers behaviour","2Social capital" )),
  aes(x=n_studies, y=reorder(pcc_unit, pcc_factor_unit2),
      fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F) +
  geom_point(aes(x=n_ES, y=reorder(pcc_unit, pcc_factor_unit2),
                 fill = factor(factor_sub_class)),
             shape=15,size=2, position = (position_dodge(width = -0.2)),
             show.legend = F)+
  scale_fill_manual(values = c( "#92c46d","#297d7d","#ea6044"))+
  facet_grid2(vars(x_metric_recla2),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y=element_line(colour = "grey"),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))+
  scale_x_continuous(
    limit = c(0,175),expand = c(0,0),
    breaks = c(0,25,50,75,100,125,150,175),
    labels= c("0","25","50","75","100","125","150","175"))+
  theme(        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0), "cm"))
figure2_distribution

figure2_distribution.plot<-ggarrange(figure2,figure2_distribution,ncol = 2,widths = c(1, 0.25))
figure2_distribution.plot
16x12

#Figure 3 ----
figure3<-
  ggplot(
    subset(overal_results,factor_sub_class%in%
             c("1Political_3", "2Biophysical context", "3Natural capital")),
    aes(y=reorder(pcc_unit, pcc_factor_unit2),x=pcc.beta,
        xmin=pcc.ci.lb, xmax=pcc.ci.ub,
        colour = factor(factor_sub_class)))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l1),show.legend = F,size=1)+
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l1),show.legend = F,size=1)+
  scale_colour_manual(values = c( "#92c46d","#f0c602", "#87CEEB"))+
  
  facet_grid2(vars(x_metric_recla2),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.50,-0.25,0,0.25,0.50,0.75),
                     labels = c("-0.50","-0.25","0","0.25","0.50","0.75"))+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))
figure3
 
figure3_distribution<-
  ggplot(
    subset(overal_results,factor_sub_class%in%
             c("1Political_3", "2Biophysical context", "3Natural capital")),
    aes(x=n_studies, y=reorder(pcc_unit, pcc_factor_unit2),
        fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 4,
                show.legend = F, colour="black", alpha=0.6) +
  scale_fill_manual(values =c("#92c46d","#f0c602", "#87CEEB"))+
  facet_grid2(vars(x_metric_recla2),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y=element_line(colour = "grey"),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))+
  scale_x_continuous(
    limit = c(0,175),expand = c(0,0),
    breaks = c(0,25,50,75,100,125,150,175),
    labels= c("0","25","50","75","100","125","150","175"))
figure3_distribution

figure3_distribution.plot<-ggarrange(figure3,figure3_distribution,ncol = 2,widths = c(1, 0.25))
figure3_distribution.plot
16x12





#######################################################################################

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")

  
overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = 
      c("black")),
  text_y = elem_list_text(size= 0.0005,colour= 
                            c("black"),
                          angle = 90),
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
  panel.grid.major.x  = element_line(color = "grey85",size = 0.6),
  axis.line = element_line(colour = "black"))

overall_effect<-
ggplot(
  subset(overal_results,factor_sub_class%in%
           c("Farmers behaviour" )),#, "1Political_2", "Social capital")),
         aes(y=reorder(pcc_unit, pcc_factor_unit2),x=pcc.beta,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,
             colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
    geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                     yend = reorder(pcc_unit, pcc_factor_unit2),
                     x=pcc.beta, xend = pcc.ci.lb_l),show.legend = F,size=1,
                 arrow = arrow(length = unit(0.2, "cm")))+
    geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                     yend = reorder(pcc_unit, pcc_factor_unit2),
                     x=pcc.beta, xend = pcc.ci.ub_l),show.legend = F,size=1,
                 arrow = arrow(length = unit(0.2, "cm")))+
    geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                     yend = reorder(pcc_unit, pcc_factor_unit2),
                     x=pcc.beta, xend = pcc.ci.ub_l1),show.legend = F,size=1)+
  geom_segment(aes(y = reorder(pcc_unit, pcc_factor_unit2),
                   yend = reorder(pcc_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l1),show.legend = F,size=1)+
  scale_colour_manual(values = c("#ea6044"))+
  facet_grid2(vars(x_metric_recla2),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.50,-0.25,0,0.25,0.50,0.75),
                     labels = c("-0.50","-0.25","0","0.25","0.50","0.75"))+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))
overall_effect

overall_distribution<-ggplot(
  subset(overal_results,factor_sub_class%in%
           c("Farmers behaviour" )),#, "1Political_2", "Social capital")),
         aes(x=n_studies, y=reorder(pcc_unit, pcc_factor_unit2),
                                  fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F) +
    geom_point(aes(x=n_ES, y=reorder(pcc_unit, pcc_factor_unit2),
                   fill = factor(factor_sub_class)),
               shape=18,size=2, position = (position_dodge(width = -0.2)),
               show.legend = F)+
  scale_fill_manual(values = "#ea6044")+
  facet_grid2(vars(x_metric_recla2),
              scales= "free", space='free_y', switch = "x", strip=overall_distribution_strips)+
  xlab("")+
  #xlab("Number")+
  theme_overall+
  theme(strip.placement.y = "outside",
        axis.text.y =element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.y=element_line(colour = "grey"),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))+
  scale_x_continuous(
    limit = c(0,175),expand = c(0,0),
    breaks = c(0,25,50,75,100,125,150,175),
    labels= c("0","25","50","75","100","125","150","175"))
overall_distribution

overall.plot<-ggarrange(overall_effect,overall_distribution,ncol = 2,widths = c(1, 0.25))

overall.plot

#1200 1700

overall_distribution_legend<- ggplot(
  subset(overal_results, factor_sub_class %in% c("Accessibility")),
                                       aes(x=n_studies, y=reorder(pcc_factor_unit, beta),
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
pcc_factor_unit2

# Compare PCC results vs Log-OR results
overall_effect<-
  ggplot(overal_results, 
         #aes(y=pcc_factor_unit,x=pcc.beta,
         aes(y=reorder(pcc_factor_unit, pcc_factor_unit2),x=pcc.beta,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,
             colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey30",linetype = 1, linewidth=0.5)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 3, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=7,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
   geom_text(aes(label=label, x=pcc.ci.ub+0.08, group=pcc_factor_unit,fontface = "bold"), 
            vjust=0.5, hjust=-0.005,size=3, 
           color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l1),show.legend = F,size=1)+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l1),show.legend = F,size=1)+
  scale_colour_manual(values = fills)+
  facet_grid2(vars(factor_sub_class),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.50,-0.25,0,0.25,0.50,0.75),
                     labels = c("-0.50","-0.25","0","0.25","0.50","0.75"))+
  xlab("")+
  #xlab(bquote(bold("Partial correlation coefficient (" *italic(r)[p]*")")))+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=3.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))
overall_effect


## PRESENTATION
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d")


overall_strips <- strip_themed(
  # Vertical strips
  background_y = elem_list_rect(
    fill = "white"),
  text_y = elem_list_text(size= 1,colour= "#ea6044",angle = 90),
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
  axis.text.x =element_text(color="black",size=16, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.background = element_blank(),
  panel.grid.major  = element_blank(),
  axis.line = element_line(colour = "black"))

presentation_overall_effect<-
  ggplot(subset(overal_results,#factor_sub_class=="2Financial capital"
                  factor_sub_class=="3Political_1"
                  #pcc_factor_unit=="Soil depth (Shallow)" |
                #pcc_factor_unit=="Soil slope (Moderate)" #|
                #pcc_factor_unit=="Land tenure security (continuous)"
                ),
   #ggplot(overal_results, 
         #aes(y=pcc_factor_unit,x=pcc.beta,
         aes(y=reorder(pcc_factor_unit, pcc_factor_unit2),x=pcc.beta,
             xmin=pcc.ci.lb, xmax=pcc.ci.ub,
             colour = factor(significance2) ))+
  geom_vline(xintercept=0, colour = "black",linetype = 1, linewidth=1)+
  geom_errorbar(width=0,size=3, position = (position_dodge(width = -0.2)),
                show.legend = F)+
  geom_point(size = 6, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=pcc.ci.ub+0.01, group=pcc_factor_unit), 
            vjust=0.7, hjust=-0.005,size=10,
            color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  # geom_text(aes(label=significance1, x=pcc.ci.ub+0.01, group=pcc_factor_unit,fontface = "bold"), 
  #          vjust=0.35, hjust=-0.005,size=3, 
  #         color="black",  family="sans",position = (position_dodge(width = -0.5)))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l),show.legend = F,size=1,
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.ub_l1),show.legend = F,size=1)+
  geom_segment(aes(y = reorder(pcc_factor_unit, pcc_factor_unit2),
                   yend = reorder(pcc_factor_unit, pcc_factor_unit2),
                   x=pcc.beta, xend = pcc.ci.lb_l1),show.legend = F,size=1)+
  scale_colour_manual(values = c("grey60","grey60","#92c46d"))+
 # facet_grid2(vars(factor_sub_class),
    #          scales= "free", space='free_y', switch = "y",
     #         strip = overall_strips)+
  scale_x_continuous(limit = c(-0.27,0.75),expand = c(0.05, 0.05),
                     breaks = c(-0.50,-0.25,0,0.25,0.50,0.75),
                     labels = c("-0.50","-0.25","0","0.25","0.50","0.75"))+
  xlab("")+
  theme_overall+
  theme(strip.placement.y = "outside",
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=3.5), "cm"),
        axis.text.y =element_text(color="black",size=12, family = "sans"))
presentation_overall_effect


