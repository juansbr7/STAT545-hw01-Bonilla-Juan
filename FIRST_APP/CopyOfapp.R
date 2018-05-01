
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(leaflet)
library(htmltools)
library(tidyr)
library(purrr)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("darkly"),
   
   # Application title
   tags$h1(style= "color: green","PR Applications Per Class in BC 2006 to 2013"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel( 
         sliderInput("year",
                     tags$p(style= " color: green", "Year"),
                     min = 2006,
                     max = 2013,
                     value = 2006),
         checkboxGroupInput(inputId= "Country",
                            tags$p(style= " color: green", "Filter by Country"),
                            choices= c("China", "India", "Philippines", "Germany", "England", "Iran", "Japan", "Mexico", "Other"),
                            selected = c("China", "Mexico")),
         downloadButton('downloadData', 'Dataset Download', class = "button"),
         tags$head(tags$style(".button{background-color:green;} .button{color: white;} .button{font-style: italic;}")) 
         

      ),

    
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("distPlot")),
          tabPanel("Table", tableOutput("numtable")),
          tabPanel("Map", tags$p(style= "size: 18", "Total Applications per Country and Year"), leafletOutput("map"), tableOutput("Totals")),   
          tabPanel("Summary", selectInput("Class", tags$p(style= " color: green", "Class across time"), choices = c("Family", "Investor", "Skilled.Worker", "Live.In.Caregiver", "Self.Employed", "Refugee", "Entrepreneur", "Other.Classes"),
                                          selected= "Family"), plotOutput("allcountry"))
                    )            
                )
                  ))

PRapp <- read.csv("PRapps.csv")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$note<- renderPrint({"Note: Canadian Experience Class was introduced in 2010"})
   
   output$distPlot <- renderPlot({
          PRapp %>%
           filter(YEAR== input$year, COUNTRY %in% c(input$Country)) %>% 
           ggplot(aes(x = ICLASS,
                      y = NUMBER))+
           geom_bar(aes(fill= COUNTRY),stat= "identity", position = "dodge") +
           theme(axis.text.x = element_text(angle=90))+ 
           ggtitle("Applications per class")
                                })
     output$numtable<- renderTable({
          PRapp %>%
          filter(YEAR== input$year, COUNTRY %in% c(input$Country)) %>% 
          select(COUNTRY, YEAR, ICLASS, NUMBER) 
                                 })
         
     output$downloadData <- downloadHandler(
       filename = function() { 
         paste("PRappfile", ".csv", sep="")},
       content = function(file) {
         write.csv(PRapp, file)}
                                          ) 

      
       
       output$map<- renderLeaflet(
         PRapp %>%
           filter(COUNTRY %in% input$Country, YEAR== input$year) %>% 
           droplevels() %>% group_by(COUNTRY) %>% mutate(APPLICATIONS= sum(NUMBER))%>% 
           leaflet() %>% 
           addTiles() %>% 
           addCircleMarkers(~LONGITUD, ~LATITUD, 
                      label = ~paste(YEAR, COUNTRY,"TOTAL:", APPLICATIONS),
                      stroke = FALSE, 
                      color = "blue",
                      radius= ~log(NUMBER)
                      )
                      )
       output$Totals<- renderTable({
         PRapp %>%
           filter(COUNTRY %in% input$Country, YEAR== input$year) %>% 
           droplevels() %>% group_by(COUNTRY, ICLASS) %>% summarize(APPLICATIONS= sum(NUMBER)) %>% 
           spread(ICLASS, APPLICATIONS) %>%  
           set_names(c("COUNTRY", "CE", "Entr", "Family", "Investor", "Caregiver", "Other","PrvNom", "Refugee", "S_Empl", "Sk_Worker"))
       })
       
       output$allcountry<- renderPlot({
         PRapp %>%
           filter(COUNTRY %in% c(input$Country), ICLASS %in% c(input$Class)) %>% 
           droplevels() %>% group_by(YEAR) %>% arrange(YEAR)%>% 
           ggplot(aes(x= YEAR, y= NUMBER)) + geom_point(aes(color= COUNTRY))+ geom_line(aes(color= COUNTRY))
              })
           
       
     }
     
      

# Run the application 
shinyApp(ui = ui, server = server)

