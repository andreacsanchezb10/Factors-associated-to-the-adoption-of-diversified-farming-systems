#install.packages("plyr")
#library(Rtools)
#library(readr)
#library(plyr)

#library(tidyverse)

library(readxl)
library(tidyr)
library(stringr)
library(dplyr)

library(metafor)
library(tibble)
factors_metric_assessed <- read_excel("C:/Users/AndreaSanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/Meta_data_2024.01.25.xlsx",
                                      sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

pcc_data<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/pcc_data_2024.01.31.csv",
                   header = TRUE, sep = ",")
names(pcc_data)

comparison<-read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/comparison_2024.01.31.csv",
                   header = TRUE, sep = ",")

sort(unique(comparison$best_model))
######## THREE-LEVEL META-ANALYSIS -------------- 
pcc_data_3level<- pcc_data%>%
  left_join(comparison, by= "pcc_factor_unit")%>%
  filter(best_model == "Three-level" )

sort(unique(pcc_data_3level$pcc_factor_unit))

write.csv(pcc_data_3level,
          "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/pcc_data_3_levels_2024.01.31.csv", row.names=FALSE)

names(pcc_data_3level)
#### Estimate the overall effect by fitting an intercept-only model ----
overall_3level <- function(data, metric_unit) {
  overal_model <- rma.mv(yi, vi, 
                         random = list(~ 1 | ES_ID, ~ 1 | article_id),
                         data = data,
                         method = "REML", 
                         test = "t",
                         dfs="contain",
                         subset = (pcc_factor_unit == metric_unit))
  
  return(summary(overal_model, digits = 3))
  
}

# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data_3level$pcc_factor_unit)

# List to store the results of all models
overall_3level_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- overall_3level(data = pcc_data_3level, metric_unit = unit)
  overall_3level_list[[unit]] <- result
}

# Combine overall results into one table
overall_3level_results_list<- do.call(rbind, overall_3level_list)

overall_3level_results <- as.data.frame(overall_3level_results_list)%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(ci.lb = sapply(ci.lb, as.numeric),
         ci.ub = sapply(ci.ub, as.numeric))%>%
  mutate(beta = as.numeric(beta))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval<=0.1,"","")))))%>%
  select(pcc_factor_unit, beta,se, ci.lb, ci.ub,zval,pval,significance,sigma2,QEdf,
         QE,QEp,s.nlevels)%>%
  mutate(sigma2=substr(sigma2, 3, nchar(sigma2) - 1))%>%
  mutate(sigma2.1= as.numeric(str_extract(sigma2, ".*(?=\\,)")))%>%
  mutate(sigma2.2= as.numeric(str_extract(sigma2, "(?<=, ).*")))%>%
  mutate(s.nlevels=substr(s.nlevels, 3, nchar(s.nlevels) - 1))%>%
  mutate(n_ES= as.numeric(str_extract(s.nlevels, ".*(?=\\,)")))%>%
  mutate(n_articles= as.numeric(str_extract(s.nlevels, "(?<=, ).*")))%>%
  mutate_at(2:7, as.numeric)%>%
  mutate_at(8,as.character)%>%
  mutate_at(9:12, as.numeric)%>%
  mutate_at(14:17, as.numeric)%>%
  mutate(across(where(is.numeric), ~ round(., 3)))%>%
  mutate(QEp= as.character(QEp))%>%
  mutate(QEp= if_else(QEp==0, "<0.001", QEp))%>%
  select(pcc_factor_unit, beta,se, ci.lb, ci.ub,zval,pval,significance,n_ES, n_articles,sigma2.1,sigma2.2,QEdf,QE,QEp)
  

write.csv(overall_3level_results,
          "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/overall_3level_results_2024.01.31.csv", row.names=FALSE)

#### D istribution of the variance over the three levels of the meta-analytic model ----
#Equation: Cheung (2014) Formula to calculate the estimate sampling variance (formula 14)
#v= vi
estimated.sampling.variance.func <- function (v) {  
  result<- ((length(v)-1) * sum(1/v))/ (((sum(1/v))^2)-(sum(1/(v^2))))
  return(result)
}

overall_3level_sampling_variance<- pcc_data_3level%>%
  group_by(pcc_factor_unit)%>%
  mutate(sampling.variance= estimated.sampling.variance.func(vi))%>%
  group_by(pcc_factor_unit,sampling.variance)%>%
  tally()%>%
  left_join(overall_3level_results, by = ("pcc_factor_unit"))%>%
  mutate(heterogeneity_test= paste("Q(df = ", QEdf,") = ",QE,", pval ", QEp, sep = ""))


## Each of the three variance components (I2_1, I2_2, I2_3) is divided by the total amount of variance
# Sampling variance (Amount of variance at level 1)
overall_3level_sampling_variance$I2_1<-((overall_3level_sampling_variance$sampling.variance)/(overall_3level_sampling_variance$sigma2.1+overall_3level_sampling_variance$sigma2.2+overall_3level_sampling_variance$sampling.variance))*100

# Within-study variance (Amount of variance at level 2)
overall_3level_sampling_variance$I2_2<-((overall_3level_sampling_variance$sigma2.1) / (overall_3level_sampling_variance$sigma2.1 + overall_3level_sampling_variance$sigma2.2 + overall_3level_sampling_variance$sampling.variance))*100

# Between-study variance (Amount of variance at level 3)
overall_3level_sampling_variance$I2_3<-((overall_3level_sampling_variance$sigma2.2) / (overall_3level_sampling_variance$sigma2.1 + overall_3level_sampling_variance$sigma2.2 + overall_3level_sampling_variance$sampling.variance))*100


overall_3level_sampling_variance<-overall_3level_sampling_variance%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  select("factor_sub_class","pcc_factor_unit", "sigma2.1", "sigma2.2",
         "heterogeneity_test", "I2_1",
         "I2_2", "I2_3")%>%
  mutate(across(where(is.numeric), ~ round(., 3)))


write.csv(overall_3level_sampling_variance,
          "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/heterogeneity_3_levels_2024.01.31.csv", row.names=FALSE)


######## TWO-LEVEL META-ANALYSIS -------------- 
pcc_data_2level<- pcc_data%>%
  left_join(comparison, by= "pcc_factor_unit")%>%
  filter(best_model == "Two-level" )


sort(unique(pcc_data_2level$pcc_factor_unit))

write.csv(pcc_data_2level,
          "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/pcc_data_2_levels_2024.01.31.csv", row.names=FALSE)

#### Estimate the overall effect by fitting an intercept-only model ----
overall_2level <- function(data, metric_unit) {
  overal_model <- rma(yi, vi, 
                         data = data,
                         method = "REML", 
                         test = "knha",
                         subset = (pcc_factor_unit == metric_unit))
  
  return(summary(overal_model, digits = 3))
  
}

# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data_2level$pcc_factor_unit)

# List to store the results of all models
overall_2level_list <- list()

# Loop over all factor_metric_unit levels and run the models
for (unit in factor_metric_units) {
  result <- overall_2level(data = pcc_data_2level, metric_unit = unit)
  overall_2level_list[[unit]] <- result
}

# Combine overall results into one table
overall_2level_results_list<- do.call(rbind, overall_2level_list)

overall_2level_results <- as.data.frame(overall_2level_results_list)%>%
  rownames_to_column(., var = "pcc_factor_unit")%>%
  mutate(ci.lb = sapply(ci.lb, as.numeric),
         ci.ub = sapply(ci.ub, as.numeric))%>%
  mutate(beta = as.numeric(beta))%>%
  mutate(significance = if_else(pval <=0.001,"***",
                                if_else(pval>0.001&pval<0.01,"**",
                                        if_else(pval>0.01&pval<=0.05,"*",
                                                if_else(pval>0.05&pval<=0.1,"","")))))%>%
  rename("n_ES"="k")%>%
  select(pcc_factor_unit, beta,ci.lb,ci.ub,
         zval, pval,
         significance,n_ES,
         tau2,se.tau2, I2, QE,dfs, QEp)%>%
  #separate(fit.stats, into = c("ll.ML", "dev.ML", "AIC.ML","BIC.ML","AICc.ML",
   #                            "ll.REML", "dev.REML", "AIC.REML","BIC.REML","AICc.REML"), sep = ", ")%>%
  mutate_at(2:6, as.numeric)%>%
  mutate_at(7, as.character)%>%
  mutate_at(8:14, as.numeric)
  
write.csv(overall_2level_results,
          "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/overall_2level_results_2024.01.31.csv",
          row.names=FALSE)
str(overall_2level_results)


overall_2level_sampling_variance<-  overall_2level_results%>%
  select(pcc_factor_unit,tau2,se.tau2, I2, QE, dfs, QEp)%>%
  mutate_at(2:7, as.numeric)%>%
  mutate(across(where(is.numeric), ~ round(., 3)))%>%
  mutate(QEp= as.character(QEp))%>%
  mutate(QEp= if_else(QEp==0, "<0.001", QEp))%>%
  mutate(heterogeneity_test= paste("Q(df = ", dfs,") = ",QE,", pval ", QEp, sep = ""))%>%
  mutate(tau2_se= paste(tau2," (",se.tau2,")",sep=""))%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")%>%
  select(factor_sub_class,pcc_factor_unit,tau2_se, heterogeneity_test, I2)
  
write.csv(overall_2level_sampling_variance,
          "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/meta-analysis/adoption_meta_analysis/heterogeneity_2_levels_2024.01.31.csv", row.names=FALSE)






#-----------------------------------------------------------





#install.packages("pals")
library(ggplot2)
library(pals)
library(RColorBrewer)
library(egg)

#"#7FC97F" "#BEAED4" "#FDC086" "#FFFF99" "#386CB0" "#F0027F" "#BF5B17" "#666666"

#"#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072" "#80B1D3" "#FDB462" "#B3DE69" "#FCCDE5"

#"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"


fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8","#87CEEB",  "#85a5cc", "#496491", "#92c46d", "#297d7d","black")

# Forest plot -----
#https://stackoverflow.com/questions/75697325/how-can-i-combine-two-plots-that-share-the-same-x-axis-seamlessly-one-is-stacke

sort(unique(results$factor_sub_class))
install.packages("forestplot")
names(overall_2level_results)

overall_results_figure<-overall_2level_results%>%
  left_join(pcc_factor_class_unit, by="pcc_factor_unit")

ggplot(overall_results_figure, 
           aes(y=reorder(pcc_factor_unit, beta),x=beta,xmin=ci.lb, xmax=ci.ub,
               colour = factor(factor_sub_class) ))+
  geom_vline(xintercept=0, colour = "grey20",linetype = 1, linewidth=0.7)+
  #geom_errorbar(width=0,size=1, show.legend = TRUE)+
  geom_errorbar(width=0,size=1, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_point(size = 4, position = (position_dodge(width = -0.2)),show.legend = F)+
  geom_text(aes(label=significance, x=ci.ub+0.01, group=pcc_factor_unit), vjust=0.7, hjust=-0.005,
            color="black", size=7, family="sans",position = (position_dodge(width = -0.5)))+
  scale_colour_manual(values = fills)+
  facet_grid(vars(factor_sub_class),
             scales= "free", space='free_y', switch = "y")+
  scale_x_continuous(limit = c(-0.4, 0.5))+
  xlab("Partial Correlation Coefficient (PCC)")+
  theme(legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="black",size=13, family = "sans", face = "bold",vjust = -1),
        axis.text.x =element_text(color="black",size=12, family = "sans"),
        plot.title = element_text(hjust = -0.6, vjust = -3, color="white",size=11, family = "sans", face = "bold"),
        axis.text.y = element_text(color="black",size=12, family = "sans"),
        plot.background = element_rect(fill = "White", color = "White"),
        panel.background = element_blank(),
        panel.grid.major  = element_line(color = "grey95",size = 0.6),
        axis.line = element_line(colour = "black"),
        axis.ticks.x= element_blank(),
        strip.placement.y = "outside")
        plot.title = element_text(hjust = -0.4, vjust = -3,color="white",size=11, family = "sans", face = "bold"))










#_______Evaluation of the Normality Assumption in Meta-Analyses
##### Q-Q PLOTS
#https://rpubs.com/dylanjcraven/metaforr
qqplot_overall_2level <- function(factor_units, pcc_data) {
  num_cols <- 6
  num_rows <- ceiling(length(factor_units) / num_cols)
  par(mfrow = c(num_rows, num_cols), mar = c(3, 3, 1, 1))  # Adjust margins
  
  for (i in 1:length(factor_units)) {
    factor_unit <- factor_units[i]
    factor_unit_subset <- subset(pcc_data, pcc_factor_unit == factor_unit)
    
    overall_model <- rma(yi, vi,
                         tdist = TRUE,
                         test = "knha",
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
factor_metric_units <- unique(pcc_data_2level$pcc_factor_unit)

qqplot_overall_2level(factor_metric_units, pcc_data_2level)

