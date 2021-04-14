#### Create a Shiny App ####
#### By: Natalie Beckman-Smith
#### Created: April 7, 2021

#### Load Libraries ####
library(tidyverse)
library(here)
library(lubridate)
library(shiny)

#### Load Data ####
HatchBabyExport <- read_csv(here("Week_10","Data","HatchBabyExport.csv"))


#### UI ####
ui <- fluidPage(
    
    titlePanel("Baby Weight"), #adds title
    sidebarLayout ( 
        sidebarPanel(
            radioButtons( #makes radio buttons
                inputId = "which.baby", #ID connected to server object
                label = "Baby:", #panel label above buttons
                choices = c("Blakely", "Micah"), #button choices
                selected = "Blakely" #default 
            )
        ),
        mainPanel(
            plotOutput("weightPlot") #shows plot 
        )
    )
)

#### Server ####
server <- function(input, output) {
    
    babyweight <- HatchBabyExport %>% #creates a new dataframe
        rename(babyname = "Baby Name", starttime = "Start Time", activity = "Activity", weight = "Amount") %>% #renames columns 
        select(babyname, starttime, activity, weight) %>% #selects columns
        filter(activity == "Weight") %>% #filters only weightings
        mutate(date = mdy_hm(starttime), #makes dates into characters
               weightround = as.numeric(weight)) #rounds numbers 

    weight.filter <- reactive({ #conditional filtering, uses data for plot based on which baby is selected 
        babyweight %>% 
            filter(babyname == input$which.baby & !is.na(weightround))
    })
    
    output$weightPlot <- renderPlot({ #creates scatterplot
        weight.filter() %>% 
            ggplot(aes( x = date, 
                        y = weightround,
                        group = 1)) + #makes it possible to havea line 
            geom_point() +
            geom_line() +
            xlab("Date") +
            ylab("Weight (lbs)") +
            ggtitle(paste("Change in Baby's Weight over Time")) +
            theme_minimal()
    })
}

#### Run the App ####
shinyApp(ui = ui, server = server)
