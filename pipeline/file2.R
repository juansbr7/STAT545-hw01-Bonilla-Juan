suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
library(stringr)

data<- readRDS("candydata.rds")

formated_names<-str_replace_all(preselectedcandy,c("\\[" = " ", "\\]" = " ")) %>% str_trim()
names(data)<- formated_names
new_names<-names(data) %>% str_replace_all(c("How old are you\\?"="Age", "Gummy Bears straight up"= "Gummy Bears", "Regular M&Ms"= "M&Ms")) %>% tolower()
names(data)<- new_names
data$age<-as.integer(data$age) 
data2<- data %>% filter(age<100 & age!= 0 & age != "NA")%>% arrange(age)
data_clean <- as.data.frame(data2)
data_clean[is.na(data_clean)] <- "NEUTRAL"
dflong<- data_clean  %>% gather(brand , emotion,-age) %>% mutate(emotion= tolower(emotion))

saveRDS(dflong, "candyc.rds")