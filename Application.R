library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)

options(shiny.maxRequestSize=30*1024^2)

download.file('https://covid.ourworldindata.org/data/owid-covid-data.csv', 'data.csv')

df<- read.csv('data.csv', header=TRUE, sep =',')

# Define UI for app that draws a histogram ----
ui <- fluidPage(


    # Main panel for displaying outputs ----
    mainPanel(

      DT::dataTableOutput('Full_Data'), #Probelm here


    )
  )




# Define server logic required to draw a histogram ----
server <- function(input, output) {



  output$Full_Data <- DT::renderDataTable({DT::datatable(df)})


}

shinyApp(ui, server)
