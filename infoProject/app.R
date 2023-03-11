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
    titlePanel("Seattle Polihowce Crime Data"),
    tabsetPanel(
      tabPanel("About the data",
               img(alt="image of crime", 
                   src='crimeSceneImage.jpg', height="50%", width="50%",align = "right"),
               
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
                                          uiOutput("againstCategory2")
      ),
                                     mainPanel(plotOutput("plot2"),textOutput("textSummary3")))),
      
      tabPanel("Reports by time of day", sidebarLayout(sidebarPanel(
                                          uiOutput("againstCategory3"),
        sliderInput("range", "Year Range:",
                    min = 2008, max = 2023,
                    value = c(2020,2021)),
      ),
      mainPanel(plotOutput("plot3"),textOutput("textSummary4")))),
      
      
      tabPanel("Conclusion", 
      mainPanel(p("From our project, we noted three significant findings relating
      to the number of crime reports during the day, by the month, and throughout 
      the years. We found that over the years, the number of crime reports has slightly 
      increased from the years 2008-2022. Over the set of these years, there was an 
      increase in 10,000 crime reports. When looking at the crime reports per month, 
      we found that January and May were the months with the highest amounts of crime 
      reports, with around 90,000 and 92,500 crime reports respectively. We also 
      found that February had the lowest amount of crime reports out of all the 
      months coming in at around 76,000. We also found that the time of the day 
      with the most amount of crime is at around midnight, and the time of the 
      day with the least amount of crime reports is around 5am. 
      From these findings, we are able to be more informed of the times of the year, 
      month, and day where crime reports increase, which allows us and our loved ones to be 
      in the most safe situation at all times, for example you are safest at around 
      5 am it seems, as that is the time where crime reports happen the least.
      The more we worked with this dataset, 
      the more we also realized how high quality this data set is, and also the sheer 
      amount of data they have collected. The data is constantly being updated, and it
      was relatively rare for us to find crime reports that had missing information outside
      of the time informtion, where there was more missing rows than usual.
      This data is however collected by the Seattle Police Department, so it is 
      extremely possible that there is bias in this dataset towards the SPD which could 
      cause harm to groups that the SPD negatively affects. 
      Going 
      forward, I think three thing we can do to advance this project is also to look at 
      the areas in Seattle where the most crime reports are made, to add the year feature, 
      or year range feature to the other datasets, and to add info on which types of crime
      or crime subgroups are the most popular. Adding the location data would give us 
      even more insight on the areas and the times to avoid when looking to avoid crime.
      The other information would help us see the differences in how the pandemic and time
      has effected the information, and to see trends in certain types of crime over time. 
                  ")))
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
      select(date,`Offense ID`,`Crime Against Category`) %>%
      group_by(as.numeric(format(date,'%Y')),`Crime Against Category`) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime2) +
      geom_line(aes(x =`as.numeric(format(date, "%Y"))`,y = n,col=`Crime Against Category`))+
      ggtitle("Amount of Crime Reports per year from the sample")+
      xlab("Year") +
      ylab("Number of Reports that year")
  })
  
  output$plot2 <- renderPlot({
    crime2 <- crime1 %>% 
      select(date,`Crime Against Category`,`Offense ID`) %>% 
      filter(`Crime Against Category` %in% input$against2) %>% 
      group_by(as.numeric(format(date,'%m')),`Crime Against Category`) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime2) +
      geom_line(aes(x =`as.numeric(format(date, "%m"))`,y = n, col= `Crime Against Category`))+
      ggtitle("Amount of Crime Reports per Month")+
      xlab("Month") +
      ylab("Number of Reports that month")
  })
  
  output$plot3 <- renderPlot({
    crime3 <- crime2 %>% 
      select(date,`Crime Against Category`,`Offense ID`) %>% 
      filter(`Crime Against Category` %in% input$against3) %>% 
      filter(as.numeric(format(date, "%Y")) >= input$range[1] & as.numeric(format(date, "%Y")) <= input$range[2]) %>% 
      group_by(as.numeric(format(date,'%H')),`Crime Against Category`) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime3) +
      geom_line(aes(x =`as.numeric(format(date, "%H"))`,y = n, col= `Crime Against Category`))+
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
            in the dataset, This dataset displays the amount of reports over the years. This is useful
          in showing the user if crime truly has been on the rise for each type of crime. It also has
          a function that lets the user decide how many reports will be in the sample for the crime,
          so if their computer can't handle all of the reports, than they can just look at a sample
           of the crime."
          )
  })
  
  output$textSummary2 <- renderText({
    paste("Of the data types that you have selected, the number of rows that have
      no values in them or in other words have NA in them is ",
          as.character(nrow(crime) - crime %>% 
                         select(input$show_vars) %>% 
                         na.omit() %>% 
                         nrow()),
          " This table is helpful because it allows the user to look at individual 
          crime reports to get a sense of what data in the set is made of.
          he difference between a report number and an offense." 
    )
  })
  
  output$textSummary3 <- renderText({
    paste("This graph shows the categories that crime went against, over the months in 
          a year. The data in the data is all rows in the data that didn't have na 
          in them. It allow users 
          to select 4 categories which are society, property, person, and not a crime.
          This graph shows which categories have more report against them by month.
          This allows it to show which months crime has occured."
    )
  })
  
  output$textSummary4 <- renderText({
    paste("This graph shows the number of crime reports by hours of day, also letting
          the user select which category of what the crime was against.
          This category also has a year range so a user can see how the categories of crime
          were different over time. This really helps people because it helps 
          citizens know when they need to be carefull and the category of which type of crime 
          they need to aware of."
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#this took like 10 hours perhaps

