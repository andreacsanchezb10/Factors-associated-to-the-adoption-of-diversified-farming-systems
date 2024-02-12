library(ggplot2)
library(webr)
library(dplyr)

data <- as.data.frame(Titanic)
head(data)

names(pcc_data)
distrubution<- pcc_data%>%
  group_by(m_region, factor_sub_class.y)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))%>%
  filter(m_region=="Africa")%>%
  mutate(Bottom = Tot - cumsum(DomSize))%>%
  mutate(Pos = Bottom + CUM - Share/2,)



metadata2 <- metadata2 %>%
  mutate(Tot = sum(Share)) %>% 
  group_by(Domain) %>% 
  mutate(CUM = cumsum(Share), DomSize = max(CUM))

#Calculate the bottom edge of the Domains when stacked
DomBot <- unique(select(metadata2, Domain, Tot, DomSize)) %>% ungroup() %>% 
  mutate(Bottom = Tot - cumsum(DomSize))

metadata2 <- inner_join(metadata2, select(DomBot, Domain, Bottom))
#> Joining, by = "Domain"
metadata2 <- mutate(metadata2, Pos = Bottom + CUM - Share/2)



distrubution2<- pcc_data%>%
  group_by(m_region, m_intervention_recla2)%>%
  dplyr::summarise(n_articles = n_distinct(article_id),
                   n_ES = n_distinct(ES_ID))
  filter(m_region=="Africa")



# Hole size
hsize <- 5
fills <- c("#f0c602", "#ea6044","#d896ff","#6a57b8","#87CEEB",  "#85a5cc", "#496491", "#92c46d", "#297d7d",)

ggplot(distrubution, aes(x = hsize, y = n_ES, fill = factor_sub_class.y)) +
    geom_col(color = "black") +
    geom_text(aes(label = n_ES),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = fills) +
    xlim(c(1, hsize + 0.5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())



  
  
ggplot() + 
  geom_col(aes(x = 2, y = n_ES, fill = factor(factor_sub_class.y)),
           distrubution, color = "black") +

  geom_col(aes(x = 3, y = n_ES, fill = factor(m_intervention_recla2)),
           distrubution2, color = "black") +
  xlim(0, 3.5) + labs(x = NULL, y = NULL) + 
  theme(axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())+
  coord_polar(theta = "y") 

pltplt + 
