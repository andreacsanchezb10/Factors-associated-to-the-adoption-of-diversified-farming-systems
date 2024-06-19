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
               "2Farmers behaviour","3Social capital"
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
  scale_colour_manual(values = c( "#92c46d","#ea6044","#297d7d"))+
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

figure2_distribution<-ggplot(
  subset(overal_results,factor_sub_class%in%
           c("1Political_2","2Farmers behaviour","3Social capital" )),
  aes(x=n_articles, y=reorder(pcc_unit, pcc_factor_unit2),
      fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F) +
  geom_point(aes(x=n_ES, y=reorder(pcc_unit, pcc_factor_unit2),
                 fill = factor(factor_sub_class)),
             shape=15,size=2, position = (position_dodge(width = -0.2)),
             show.legend = F)+
  scale_fill_manual(values = c( "#92c46d","#ea6044","#297d7d"))+
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
        colour = factor(factor_sub_class) ))+
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
  aes(x=n_articles, y=reorder(pcc_unit, pcc_factor_unit2),
      fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F) +
  geom_point(aes(x=n_ES, y=reorder(pcc_unit, pcc_factor_unit2),
                 fill = factor(factor_sub_class)),
             shape=15,size=2, position = (position_dodge(width = -0.2)),
             show.legend = F)+
  
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

#Figure 4 ----
figure4<-
ggplot(
  subset(overal_results,factor_sub_class%in%
           c("1Human capital", "2Financial capital",
             "3Political_1", "4Physical capital")),
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
  scale_colour_manual(values = c( "#6a57b8","#d896ff", "#92c46d", "#496491"))+
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

figure4
  
  
figure4_distribution<-ggplot(
  subset(overal_results,factor_sub_class%in%
           c("1Human capital", "2Financial capital",
             "3Political_1", "4Physical capital")),
aes(x=n_articles, y=reorder(pcc_unit, pcc_factor_unit2),
    fill = factor(factor_sub_class))) +
  geom_bar(stat="identity",show.legend = F)+
  geom_errorbar(aes(xmin=0, xmax=n_ES), 
                width=0, position = position_dodge(width = 0.9),size = 0.7,
                show.legend = F) +
  geom_point(aes(x=n_ES, y=reorder(pcc_unit, pcc_factor_unit2),
                 fill = factor(factor_sub_class)),
             shape=15,size=2, position = (position_dodge(width = -0.2)),
             show.legend = F)+
  scale_fill_manual(values = c( "#6a57b8","#d896ff", "#92c46d", "#496491"))+
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
figure4_distribution

figure4_distribution.plot<-ggarrange(figure4,figure4_distribution,ncol = 2,widths = c(1, 0.25))
figure4_distribution.plot
16x12