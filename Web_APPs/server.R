library(ggplot2)
library(dplyr)

candydata <- read.csv("cdata.csv")


server <- function(input, output) {
           output$agebrand <- renderPlot({
                                  candydata %>%
                                  filter(age== input$age, brand %in% c(input$brand)) %>% 
                                  ggplot(aes(x = emotion,
                                            y = n))+
                                  geom_bar(aes(fill= brand),stat= "identity", position = "dodge") +
                                  theme(axis.text.x = element_text(angle=90))+ 
                                  ggtitle("Emotions")
                                  
                                  
                                  })
  
}
