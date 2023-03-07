#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#C:/Users/AquaP/OneDrive/Desktop/Info201/ps06-shiny-app/ps06-shiny-app/app.R

# shiny app

library(shiny)

library(tidyverse)

crime <- read_delim("crimeDataEnding3-6-2023.csv")

ui <- fluidPage(
  mainPanel(
    titlePanel("Seattle Police Crime Data"),
    tabsetPanel(
      tabPanel("About the data",
               h1("What is this data?"),
               p("This data is a collection of crime data from the ", strong("Seattle 
                 Police Department"), ", the data spans from 2008 and is still updated
                 to this day, although I retrieved the data on ", 
                 strong("February 15th"),", so it doesn't have any data after 
                 that point"),
               em("I am not sure about how recent the data needs to be."),
               h1("What does this application do?"),
               p("The plot plots a sample from 1000 to ", nrow(crime), " reports, or all
               of the reports in the dataset. The plot charts how
               many of the reports there are from each year, letting you
               get a good of the distribution. without having to look at all the
               data if you prefer."),
               p(("There are "), nrow(crime), " rows of data and ", ncol(crime),
                 "collumns of data. IN the dataset, the ranking of who the the crime
               was against is as follows,
               PROPERTY,
               PERSON,
               SOCIETY,
               NOT_A_CRIME",
                 "I wasn't able to calculate this data on this document but a the 
               project information document that will be in the github file")),
      
      tabPanel("Plot",sidebarPanel(
        sliderInput("n", "How many reports of crime:",
                    min = 1000,
                    max = nrow(crime),
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
                 ),mainPanel(DT::dataTableOutput("table"),textOutput("textSummary2"))))
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
  
  
  output$plot <- renderPlot({
    crime1 <- crime %>% 
      sample_n(input$n) %>% 
      mutate(datech = substr(`Offense Start DateTime`,1,10)) %>% 
      mutate(date = as.Date(datech, format = "%m/%d/%Y")) %>% 
      filter(as.numeric(format(date,'%Y'))>2008) %>% 
      filter(`Crime Against Category` %in% input$against) %>% 
      select(date,`Offense ID`) %>%
      group_by(as.numeric(format(date,'%Y'))) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime1) +
      geom_line(aes(x =`as.numeric(format(date, "%Y"))`,y = n),col=input$color)+
      ggtitle("Amount of Crime reports per year from the sample")+
      xlab("Year") +
      ylab("Number of Reports that year")
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

