library(ggplot2)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))

dflong2<- readRDS("candyc.rds")
centennial_ages<- 1:21
millennial_ages<- 22:40
generationX_ages<- 41:52
baby_Boomers_ages<- 53:71
traditionalists_ages<- 72:100

centennials<-dflong2 %>% filter(age %in% centennial_ages) 
centennials %>% head()
cent_emotions<- centennials %>% group_by(brand, emotion) %>%  count(emotion) %>% arrange(brand, desc(n))
plot<-cent_emotions %>% ggplot(aes( x = brand, y = n, fill= emotion)) + 
  geom_bar(stat= "identity", position = "stack") +
  theme(axis.text.x = element_text(angle=90))+ ggtitle("Emotions: Centennials")
ggsave("plot.png", plot)
