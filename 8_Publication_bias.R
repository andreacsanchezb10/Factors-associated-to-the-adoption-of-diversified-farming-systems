library(dplyr)
library(metafor)
library(tibble)
library(purrr)
library(readxl)
library(stringr)

################# META-REGRESSION ----------------
factors_metric_assessed <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/Meta_data_2024.02.15.xlsx",
  sheet = "FACTORS_metric_assessed")

factors_metric_assessed$pcc_factor_unit <- paste(factors_metric_assessed$x_metric_recla2,
                                                 " (",factors_metric_assessed$pcc_unit,")", sep="")

pcc_factor_class_unit<-factors_metric_assessed%>%
  select(factor_sub_class,pcc_factor_unit)
pcc_factor_class_unit<-unique(pcc_factor_class_unit)

################################  PUBLICATION BIAS ###################################################################3----------------------------------------------------#
#### THREE-LEVEL DATA
pcc_data_3level<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(vi),
         pcc_precision = (1/pcc_se))
names(pcc_data_3level)

#### TWO-LEVEL DATA
pcc_data_2level<- read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ",")%>%
  mutate(pcc_se = sqrt(vi),
         pcc_precision = (1/pcc_se))

##--------- EGGER REGRESSION TEST
# Define a function to extract and format model results
extract_model_results <- function(result, unit, source) {
  data.frame(
    pcc_factor_unit = unit,
    moderator = source,
    Estimate = result[1],
    SE = result[2],
    tval = result[3],
    CI_Upper = result[4],
    CI_Lower = result[5]
  )
}

#### THREE-LEVEL DATA
# Vector of factor_metric_unit levels
factor_metric_units <- unique(pcc_data_3level$pcc_factor_unit)

# List to store the results of all models
egger_3level_list <- lapply(factor_metric_units, function(unit) {
  # Fit the model
  egger_model <- rma.mv(yi, vi, 
                        random = list(~ 1 | ES_ID, ~ 1 | article_id),
                        data = pcc_data_3level,
                        mod = ~pcc_precision,
                        method = "REML", 
                        test = "t",
                        dfs = "contain",
                        subset = (pcc_factor_unit == unit))
  
  # Extract model results
  result <- coef(summary(egger_model))
  intercept_result <- result[1, ]
  precision_result <- result[2, ]
  
  # Combine results into a list
  rbind(extract_model_results(intercept_result, unit, "Intercept"),
        extract_model_results(precision_result, unit, "Precision"))
})

# Combine all results into one data frame
egger_3level_results <- do.call(rbind, egger_3level_list)


#### TWO-LEVEL DATA
unit_counts <- pcc_data_2level %>%
  group_by(pcc_factor_unit) %>%
  summarize(n = n())

# Filter out levels with insufficient data
selected_units <- unit_counts %>%
  filter(n >= 3) %>%
  pull(pcc_factor_unit)

sort(unique(pcc_data_2level$pcc_factor_unit))    

all_units <- unique(pcc_data_2level$pcc_factor_unit)

# Get the levels that are missed
missed_units <- setdiff(all_units, selected_units)

# Print the missed levels
print(missed_units)

# Fit the model only for selected levels of pcc_factor_unit
egger_2level_list <- lapply(selected_units, function(unit) {
  egger_model2 <- rma.uni(yi, vi, 
                          data = pcc_data_2level %>% filter(pcc_factor_unit == unit),
                          mod = ~pcc_precision,
                          method = "REML", 
                          test = "knha")
  
  # Extract model results
  result <- coef(summary(egger_model2))
  intercept_result <- result[1, ]
  precision_result <- result[2, ]
  
  # Combine results into a list
  rbind(extract_model_results(intercept_result, unit, "Intercept"),
        extract_model_results(precision_result, unit, "Precision"))
})
# Combine all results into one data frame
egger_2level_results <- do.call(rbind, egger_2level_list)

### COMBINE THREE AND TWO LEVEL RESULTS

egger_test<- rbind(egger_3level_results,egger_2level_results)%>%
  mutate(pval= round(pval,4),
         tval=round(tval,4))%>%
  mutate(significance = if_else(pval <=0.001,paste(pval,"***",sep=""),
                                if_else(pval>0.001&pval<0.01,paste(pval,"**",sep=""),
                                        if_else(pval>0.01&pval<=0.05,paste(pval,"*",sep=""),
                                                        paste(pval)))))%>%
  mutate(significance= if_else(significance=="0***","<0.0001***",significance))%>%
  left_join(pcc_factor_class_unit, by= "pcc_factor_unit")%>%
  dplyr::select("factor_sub_class","pcc_factor_unit","moderator","estimate" ,"se" ,"tval", "significance")
  
write.csv(egger_test,"results/egger_test.csv", row.names=FALSE)

##########################################################################################
##--------- Funnel plot
### THREE-LEVEL DATA
pcc_data_3level%>%
  select(ES_ID)
sort(unique(pcc_data_3level$pcc_factor_unit))

rstandard_3level <- function(factor_unit, data) {
  funnel <- rma.mv(yi, vi, 
                   random = list(~ 1 | ES_ID, ~ 1 | article_id),
                   data = data,
                   method = "REML", 
                   test = "t",
                   dfs = "contain",
                   subset = (pcc_factor_unit == factor_unit))
  
  # Extracting standardized residuals
  rstandard_stats <- rstandard.rma.mv(funnel,type= "rstandard")
  
  # Extracting variance-covariance matrix
  vcov_matrix <- funnel$V
  
  # Extracting standard errors and z-values
  se <- sqrt(diag(vcov_matrix))
  z <- funnel$coef / se
  
  # Create data frame with results
  results <- data.frame(
    pcc_factor_unit = factor_unit,
    resid = rstandard_stats,
    se = se,
    z = z
  )
  return(results)
}

# List of all factor units
factor_units3 <- sort(unique(pcc_data_3level$pcc_factor_unit))

# Apply the analysis function for each factor unit
rstandard_3level_list <- lapply(factor_units3, rstandard_3level, data = pcc_data_3level)

# Combine results into one data frame
rstandard_3level_results <- do.call(rbind, rstandard_3level_list)

sort(unique(rstandard_3level_results$pcc_factor_unit))

ggplot(pcc_data_3level, aes(x=pcc_precision, y=rstandard_3level_results$resid.resid,  colour = factor(pcc_factor_unit)))+
  geom_hline(yintercept = 0, colour = "grey20")+
  geom_point(shape=1, size=3, color="black")+
  facet_wrap(~ pcc_factor_unit, ncol = 3)


### TWO-LEVEL DATA
pcc_data_2level
sort(unique(pcc_data_2level$pcc_factor_unit))

funnel <- rma.uni(yi, vi, 
                  data = pcc_data_2level,
                  method = "REML", 
                  test = "knha",
                  subset = (pcc_factor_unit == "Total income (continuous)"))

results<-  as.data.frame(rstandard(funnel, type = "conditional"))



seq_along(as.data.frame(results))

  rownames_to_column(., var = "column_id")
  
  

funnel_2level_results <- data.frame()

# Iterate over each unique pcc_factor_unit
for (factor_unit in unique(pcc_data_2level$pcc_factor_unit)) {
  # Fit a random effects meta-analysis model
  model <- rma.uni(yi, vi, 
                   data = subset(pcc_data_2level, pcc_factor_unit == factor_unit),
                   method = "REML", 
                   test = "knha")
  
  # Calculate rstandard
  rstandard_values <- rstandard(model, type = "conditional")
  
  # Store results in a data frame
  results <- data.frame(factor_unit = factor_unit,
                        rstandard = rstandard_values)
  
  # Append results to all_results data frame
  funnel_2level_results <- rbind(funnel_2level_results, results)
}

library(gridExtra)

library(ggplot2)
library(ggh4x)
library(readxl)
library(dplyr)
library(ggpubr)
library(grid)
library(plyr)
library(forcats)

ggplot(pcc_data_2level, aes(x=pcc_precision, y=funnel_2level_results$rstandard.resid,  colour = factor(factor_sub_class)))+
  geom_hline(yintercept = 0, colour = "grey20")+
  geom_point(shape=1, size=3, color="black")+
  facet_wrap(~ pcc_factor_unit, ncol = 3)

  facet_grid(~pcc_factor_unit,ncol=4)
              scales= "free", space='free_y', switch = "y")


funnel_plot






funnel_plot <- ggplot(effectsize_2, aes(x=Financial_precision, y=resid.overall$resid))+
  geom_hline(yintercept = 0, colour = "grey20")+
  geom_point(shape=1, size=3, color="black")+
  facet_wrap_custom(~Financial_outcome, scales = "free", ncol = 3,nrow=2,
                    strip.position = "left",
                    labeller = as_labeller(c("B/C ratio" = "Residuals", 
                                             "Net income" = "Residuals",
                                             "Gross income" = "Residuals",
                                             "Gross margin" = "Residuals",
                                             "Total cost" = "Residuals")),
                    scale_overrides = list(
                      scale_override(1, scale_x_continuous(breaks = c(0,5,10,15,20,25,30),limits = c(0,30))),
                      scale_override(2, scale_x_continuous(breaks = c(0,1,2,3,4),limits = c(0,4))),
                      scale_override(3, scale_x_continuous(breaks = c(0,2,4,6,8),limits = c(0,8))),
                      scale_override(4, scale_x_continuous(breaks = c(0,50,100,150,200),limits = c(0,200))),
                      scale_override(5, scale_x_continuous(breaks = c(0,100,200,300),limits = c(0,300))),
                      scale_override(1, scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5),limits = c(-1.5,1.5))),
                      scale_override(2, scale_y_continuous(breaks = c(-20,-15,-10,-5,0,5,10,15,20),limits = c(-15,15))),
                      scale_override(3, scale_y_continuous(breaks = c(-20,-10,0,10,20,30),limits = c(-20,35))),
                      scale_override(4, scale_y_continuous(breaks = c(-2,-1,0,1,2,3),limits = c(-2,3))),
                      scale_override(5, scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3,4),limits = c(-3,4)))))+
  
  theme_F3+
  theme(plot.margin = unit(c(t=0.5,b=1,l=0.5,r=0.5), "lines"))
funnel_plot<- ggplotGrob(funnel_plot)
grid.show.layout(gtable:::gtable_layout(funnel_plot))
funnel_plot<- gtable_add_rows(x = funnel_plot, heights = unit(1, 'cm'), pos = 2)
funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = "a)  B/C ratio", gp = gpar(col = "black",fontface="bold",fontsize=18,fontfamily="sans"))),
                               t = 3, l = 10, b = 2, r = 6, name = c("strip-top-1-rectg", "strip-top-1-text"))
funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = "b)  Gross margin", gp = gpar(col = "black",fontface="bold",fontsize=18,fontfamily="sans"))),
                               t = 3, l = 18, b = 2, r = 9, name = c("strip-top-2-rectg", "strip-top-2-text"))

funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = "c)  Net income", gp = gpar(col = "black",fontface="bold",fontsize=18,fontfamily="sans"))),
                               t = 3, l = 23, b = 2, r = 17, name = c("strip-top-3-rectg", "strip-top-3-text"))
funnel_plot<- ggplotGrob(funnel_plot)
funnel_plot<- gtable_add_rows(x = funnel_plot, heights = unit(1.5, 'cm'), pos = 11)
funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = "d)  Gross income", gp = gpar(col = "black",fontface="bold",fontsize=18,fontfamily="sans"))),
                               t = 15, l = 10, b = 6, r = 6, name = c("strip-top-4-rectg", "strip-top-3-text"))

funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = "e)  Total cost", gp = gpar(col = "black",fontface="bold",fontsize=18,fontfamily="sans"))),
                               t = 15, l = 18, b = 6, r = 9, name = c("strip-top-5-rectg", "strip-top-3-text"))

funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = "  Gross income", gp = gpar(col = "black",fontface="bold",fontsize=15,fontfamily="sans"))),
                               t = 15, l = 10, b = 6, r = 6, name = c("strip-top-4-rectg", "strip-top-3-text"))
funnel_plot<- ggplotGrob(funnel_plot)
funnel_plot<- gtable_add_rows(x = funnel_plot, heights = unit(0.5, 'cm'), pos = -3)
funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = expression(paste("Precision ", (SE^-1))), gp = gpar(col = "black",fontface="plain",fontsize=14,fontfamily="sans"))),
                               t = -3, l = 7, b = 18, r = 9, name = c("strip-top-1-rectg", "strip-top-1-text"))
funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = expression(paste("Precision ", (SE^-1))), gp = gpar(col = "black",fontface="plain",fontsize=14,fontfamily="sans"))),
                               t = -3, l = 17.5, b = 18, r = 12, name = c("strip-top-2-rectg", "strip-top-2-text"))


funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = expression(paste("Precision ", (SE^-1))), gp = gpar(col = "black",fontface="plain",fontsize=14,fontfamily="sans"))),
                               t = 10, l = 7, b = 10, r = 9, name = c("strip-top-1-rectg", "strip-top-1-text"))
funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = expression(paste("Precision ", (SE^-1))), gp = gpar(col = "black",fontface="plain",fontsize=14,fontfamily="sans"))),
                               t = 10, l = 17.5, b = 10, r = 12, name = c("strip-top-2-rectg", "strip-top-2-text"))

funnel_plot<-  gtable_add_grob(x = funnel_plot,grobs = list(rectGrob(gp = gpar(col = NA,fill = NA)),
                                                            textGrob(label = expression(paste("Precision ", (SE^-1))), gp = gpar(col = "black",fontface="plain",fontsize=14,fontfamily="sans"))),
                               t = 10, l = 23, b = 10, r = 17, name = c("strip-top-2-rectg", "strip-top-2-text"))
grid.newpage()
grid.draw(funnel_plot) 

tiff('Results_2022.06.21/Figure_A18.tiff', units="cm", width=35, height=20, res=300)
grid.draw(funnel_plot) 
dev.off()

ggsave("Results_2022.06.21/Figure_A18.pdf", plot = funnel_plot, dpi = 320,
       width = 55,height = 30, units = "cm")
