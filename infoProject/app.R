#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#C:/Users/AquaP/OneDrive/Desktop/Info201/ps06-shiny-app/ps06-shiny-app/app.R



# Seattle Police Crime Data Info Project

# shiny app

library(shiny)

library(tidyverse)

crime <- read_delim("crimeDataEnding3-6-2023.csv")

crime1 <- crime %>% 
  mutate(datech = substr(`Offense Start DateTime`,1,10)) %>% 
  mutate(date = as.Date(datech, format = "%m/%d/%Y")) %>% 
  filter(as.numeric(format(date,'%Y'))>=2008) 

crime2 <- crime %>% 
  mutate(dateDirty = gsub("(.)\\.?[Mm]\\.?","\\1m",crime$`Offense Start DateTime`)) %>% 
  mutate(date = as.POSIXct(dateDirty, format="%d/%m/%Y %I:%M:%S %p", tz="PST8PDT")) 

maxRows <- crime1 %>% 
  na.omit() %>% 
  nrow()

ui <- fluidPage(
  mainPanel(
    titlePanel("Seattle Police Crime Data"),
    tabsetPanel(
      tabPanel("About the data",
               img(alt="image of crime", 
                   src="crimeSceneImage.jpg", align = "right"),
               
               h1("About the project"),
               p("Crime poses a serious threat to our safety. 
                 Since crime is always lurking around us, it is very important to be aware of what is going on around us. 
                 Our aim with this project is to provide information to the public about a crime that threatens us. 
                 We believe that having access to this information will help people make better decisions and protect themselves from potential harm. 
                 We want to empower people with the knowledge of how to stay safe and secure in their own communities. 
                 We hope that this project will help make our world a safer place."),
               h1("About the data"),
               p("The data we are using for this project is from", a("Seattle open data portal.", href = 'https://data.seattle.gov/Public-Safety/SPD-Crime-Data-2008-Present/tazs-3rd5'), 
                 "It’s directly from the Seattle Police Department and it’s updated daily."),
               h1("Our group members"),
               p("Marcus Christopher Liu, Zack Tisdale, Ray Hwang")
      ),
      
      
      tabPanel("Reports by Year",sidebarPanel(
        sliderInput("n", "How many reports of crime:",
                    min = 1000,
                    max = maxRows,
                    value = 10000),
        radioButtons("color", "Choose color",
                     choices = c("skyblue", "lawngreen", "orangered",
                                          "purple", "gold")),
                                          uiOutput("againstCategory")
      ),
      mainPanel(plotOutput("plot"),
                textOutput("textSummary1")),
      ),
      tabPanel("Table",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("show_vars", "Columns in the Crime Data to show:",
                                      names(crime), selected = names(crime))
                 ),mainPanel(DT::dataTableOutput("table"),textOutput("textSummary2")))),
      
      tabPanel("Reports by Month", sidebarLayout(sidebarPanel(
        radioButtons("color2", "Choose color",
                     choices = c("skyblue", "lawngreen", "orangered",
                                          "purple", "gold")),
                                          uiOutput("againstCategory2")
      ),
                                     mainPanel(plotOutput("plot2")))),
      
      tabPanel("Reports by time of day", sidebarLayout(sidebarPanel(
        radioButtons("color3", "Choose color",
                     choices = c("skyblue", "lawngreen", "orangered",
                                          "purple", "gold")),
                                          uiOutput("againstCategory3"),
        sliderInput("range", "Year Range:",
                    min = 2008, max = 2023,
                    value = c(2020,2021)),
      ),
      mainPanel(plotOutput("plot3"))))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$againstCategory <- renderUI({
    checkboxGroupInput("against", "Who the crime was againt",
                       choices = unique(crime$`Crime Against Category`)
    )
  })
  
  output$againstCategory2 <- renderUI({
    checkboxGroupInput("against2", "Who the crime was againt",
                       choices = unique(crime$`Crime Against Category`)
    )
  })
  
  output$againstCategory3 <- renderUI({
    checkboxGroupInput("against3", "Who the crime was againt",
                       choices = unique(crime$`Crime Against Category`)
    )
  })
  
  
  output$plot <- renderPlot({
    crime2 <- crime1 %>% 
      sample_n(input$n) %>% 
      filter(`Crime Against Category` %in% input$against) %>% 
      select(date,`Offense ID`) %>%
      group_by(as.numeric(format(date,'%Y'))) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime2) +
      geom_line(aes(x =`as.numeric(format(date, "%Y"))`,y = n),col=input$color)+
      ggtitle("Amount of Crime Reports per year from the sample")+
      xlab("Year") +
      ylab("Number of Reports that year")
  })
  
  output$plot2 <- renderPlot({
    crime2 <- crime1 %>% 
      select(date,`Crime Against Category`,`Offense ID`) %>% 
      filter(`Crime Against Category` %in% input$against2) %>% 
      select(date,`Offense ID`) %>%
      group_by(as.numeric(format(date,'%m'))) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime2) +
      geom_line(aes(x =`as.numeric(format(date, "%m"))`,y = n),col=input$color2)+
      ggtitle("Amount of Crime Reports per Month")+
      xlab("Month") +
      ylab("Number of Reports that month")
  })
  
  output$plot3 <- renderPlot({
    crime3 <- crime2 %>% 
      select(date,`Crime Against Category`,`Offense ID`) %>% 
      filter(`Crime Against Category` %in% input$against3) %>% 
      select(date,`Offense ID`) %>%
      filter(as.numeric(format(date, "%Y")) >= input$range[1] & as.numeric(format(date, "%Y")) <= input$range[2]) %>% 
      group_by(as.numeric(format(date,'%H'))) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime3) +
      geom_line(aes(x =`as.numeric(format(date, "%H"))`,y = n),col=input$color3)+
      ggtitle("Amount of Crime Reports per Hour")+
      xlab("Hour of the Day") +
      ylab("Number of Reports per hour")
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(crime[, input$show_vars, drop = FALSE])
  })
  
  output$textSummary1 <- renderText({
    paste("Currently you have selected ", as.character(input$n), " reports. In the reports that 
         and that is ", as.character(signif(input$n/nrow(crime) * 100),digits = 3),"% of the reports 
            in the dataset, also the data above shows that there is almost no crime in 2023 as 
            the year is not finished, thus there hasn't been enough data. The data above represents
            the yearly crime rate if you were unaware, and it it displays the amount that was found
            in that year in the selected sample. I made it so that changing the colors resets the 
            graph, so you don't have to click the slider again if you want to changet the sample 
            but not the sample amount." 
    )
  })
  
  output$textSummary2 <- renderText({
    paste("Of the data types that you have selected, the number of rows that have
      no values in them or in other words have NA in them is ",
          as.character(nrow(crime) - crime %>% 
                         select(input$show_vars) %>% 
                         na.omit() %>% 
                         nrow()),
          "The data in the table above was reduced from a previous amount because
            it was too large. The difference between a report number and an offense
            ID is that a report number contains the year, whereas an offense ID is
            wholy unique to each report, so it is useful when searching ignoring year." 
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#this took like 10 hours perhaps

