library(readxl)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))


suppressMessages(candy <- read_excel("~/Downloads/CANDY-HIERARCHY-2015-SURVEY-Responses.xlsx"))
preselectedcandy<- c( "How old are you?","[Butterfinger]", "[100 Grand Bar]", "[Bonkers]","[Bubble Gum]", "[Caramellos]", "[Snickers]", "[Gummy Bears straight up]", "[Mars]", "[Maynards]", "[Skittles]", "[Kit Kat]","[Twix]", "[Starburst]", "[Nerds]", "[Nestle Crunch]", "[Milky Way]", "[Reese’s Peanut Butter Cups]", "[Lollipops]", "[Hershey’s Milk Chocolate]", "[Regular M&Ms]") 
data<-candy %>% select(preselectedcandy)
saveRDS(data, "candydata.rds")



