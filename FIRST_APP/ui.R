library(shiny)
library(dplyr)
library(ggplot2)

candydata <- read.csv("cdata.csv")

ui <- fluidPage(
       titlePanel("Most and Least popular Candy per age"),
       sidebarLayout(
         sidebarPanel(
           tags$p(style = "color: darkred", "See the code in ",
            a(href = "https://github.com/juansbr7/STAT547Bonilla_Juan", "Github REPO")),
           tags$hr(), 
          sliderInput(inputId= "age", 
                       label = "Select the age ",
                       value = 25,
                       min = 5,
                       max = 70
                       ),
           checkboxGroupInput(inputId= "brand",
                      label = "Filter by brand",
                      choices= unique(candydata$brand),
                      selected = "bonkers")
         ),
         mainPanel(
           fluidRow( column(h3("candy"), width = 4), column(width = 4),
             column(tags$img(height = 150, width = 200, src = "https://s3.eu-central-1.amazonaws.com/euobs-media/6fbb5999d09de6e36c27d38309e59b7f.jpg"),
                    width = 4) 
           ),
           plotOutput("agebrand")
             )
           
         )   
        )
        
   
