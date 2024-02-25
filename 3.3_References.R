install.packages("pander")
library(dplyr)
library(readxl)



references <- read.delim("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/included_articles.txt")%>%
  rename("references"="X1.")
references <- as.data.frame(references[!grepl("^\\d", references$references), ])


studies_list <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/data_extraction/checked_data/articles_list_2024.02.15.xlsx",
  sheet = "article_list")


#### PCC data 
studies_source<- read.csv("data/pcc_data_3levels.csv",header = TRUE, sep = ",")  %>%
  rbind(read.csv("data/pcc_data_2levels.csv",header = TRUE, sep = ","))%>%
  select(article_id)%>%
  distinct(., .keep_all = TRUE)%>%
  left_join(studies_list)

table(studies_source$source)


