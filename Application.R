  rm(list = ls())

  if (interactive()){

  library(dplyr)
  library(tidyr)
  library(shiny)
  library(shinythemes)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(shinyWidgets)
  library(lubridate)
  library(stringr)

  options(shiny.maxRequestSize=30*1024^2)

  data_presence <- F

  if (data_presence){

  download.file('https://covid.ourworldindata.org/data/owid-covid-data.csv', 'data.csv')
  download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', 'data_mobility.csv')
  }

  df<- read.csv('data.csv', header=TRUE, sep =',')
  colnames(df)[4] <- "dates"
  df$dates <- ymd(as.character(df$dates))

  df <- df %>% mutate(location = recode(str_trim(location), "United States" = "USA",
                              "United Kingdom" = "UK",
                              "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                              "Congo" = "Republic of Congo",
                              "Samoa" = "American Samoa",
                              "Antigua" = "Antigua and Barbuda",
                              "Barbuda" = "Antigua and Barbuda",
                              "Cote d'Ivoire" = "Ivory Coast",
                              "Czechia" = "Czech Republic",
                              "Micronesia (country)" = "Micronesia",
                              "North Macedonia" = "Macedonia",
                              "Eswatini" = "Swaziland",
                              "Timor" = "Timor-Leste")) 

  df_mobility <- read.csv('data_mobility.csv', header = TRUE, sep  =',')
  df_mobility$date <- ymd(as.character(df_mobility$date))

  df_market <- df_mobility%>%complete(country_region,date)%>%group_by(country_region,date, .drop=F)%>%summarise(transit_stations_percent_change_from_baseline_smoothed = mean(transit_stations_percent_change_from_baseline, na.rm =T),
  workplaces_percent_change_from_baseline_smoothed = mean(workplaces_percent_change_from_baseline, na.rm = T),
  residential_percent_change_from_baseline_smoothed = mean(residential_percent_change_from_baseline, na.rm = T),
  parks_percent_change_from_baseline_smoothed = mean(parks_percent_change_from_baseline, na.rm = T),
  grocery_and_pharmacy_percent_change_from_baseline_smoothed = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = T),
  retail_and_recreation_percent_change_from_baseline_smoothed = mean(retail_and_recreation_percent_change_from_baseline, na.rm = T))%>%as.data.frame() # .drp= F to avoid empty groups

  df_market$date <- ymd(as.character(df_market$date))
  colnames(df_market)[1] <- "location"
  colnames(df_market)[2] <- "dates"

  world_map <- map_data("world")
  colnames(world_map)[5] <- "location"


  df_histo <- df%>%group_by(location) %>% mutate(new_cases_variation = new_cases - lead(new_cases))

  #

  ##############################################################################################
  ##############################################################################################

  # Define UI for random distribution app ----
  ui <- fluidPage(theme = shinytheme("slate"),

    # App title ----
    titlePanel("Covid TrackR"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(
        switchInput(inputId = "Log_selector", value = TRUE, label='Axis as log', onLabel = 'Log', offLabel = 'Linear', size = 'mini'),
        # Input: Select the random distribution type ----
        selectInput("variable_selector", "Select variable to plot:", 
                    choices=c("new_cases","reproduction_rate")),
              selectInput("variable_selector_2", "Select variable to plot:", 
        choices=c("workplaces_percent_change_from_baseline_smoothed","residential_percent_change_from_baseline_smoothed")),
        sliderInput('Date_selection',label = 'Date Selection', min = min(df$dates), max = max(df$dates), value = c(min(df$dates), max(df$dates))),
        # br() element to introduce extra vertical spacing ----
        br(),

        # Input: Slider for the number of observations to generate ----
        sliderInput("Map_Date_Selection",
                    "Map Date Selection :",
                    value = max(df$dates),
                    min = min(df$dates), max = max(df$dates)),
        
        pickerInput(
          inputId = "country_picker", # TODO :Change Name
          label = "Country Selection",
          choices = df%>%select(location)%>%unique()%>%c(),#%>%sapply(levels),
          selected = "France",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 4"
          ),
          multiple = TRUE
  ),

        pickerInput(
          inputId = "country_picker_2", # TODO :Change Name
          label = "Country Selection for dynamics data",
          choices = df_market%>%select(location)%>%unique()%>%c()%>%sapply(levels),
          selected = "France",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 4"
          ),
          multiple = TRUE
  )),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel('New_cases', plotlyOutput(("Total_cases")), plotlyOutput("Mortality_graph"),plotlyOutput('Total_deaths'), plotlyOutput('New_cases_pm'), plotlyOutput('New_deaths_pm'), plotlyOutput("daily_cases"), plotlyOutput('Reproduction_rate')), # TO MODIFY
                    tabPanel("Map", plotlyOutput("New_cases_map")),
                    tabPanel("Population Dynamics", plotlyOutput("dynamics"),plotlyOutput("Dynamics_map")),
                    tabPanel("Not Used Yet", plotlyOutput('test'), plotlyOutput('test_2'),verbatimTextOutput("value")),
                    tabPanel("Table",  DT::dataTableOutput('Full_Data'))
        )

      )
    )
  )


  server <- function(input, output) {

    # Generate an HTML table view of the data ----

  output$Full_Data <- DT::renderDataTable({DT::datatable(df)})



  output$New_cases <- renderPlotly(ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = dates, y = input$variable_selector , group = location, color = location)) 
  + geom_line() + ggtitle('Number of new  cases - Rolling 7 days average')))





  output$Total_cases <- renderPlotly((ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), aes(x = dates, y = total_cases, group = location, color = location)) 
  + geom_line() + ggtitle('Total cases') 
  + geom_line(aes(y=total_vaccinations),linetype ='dotdash'))))

  output$Mortality_graph<- renderPlotly((ggplotly(ggplot(data = df%>%filter(location == input$country_picker)%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes( x = dates, y = new_deaths_smoothed,group = location, color = location)) 
  + geom_line() + ggtitle('Number of new  death - Rolling 7 days average'))))

  output$Total_deaths <- renderPlotly((ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = total_deaths, group = location, color = location)) 
  + geom_line() + ggtitle('Total deaths'))))

  output$New_cases_pm <- renderPlotly((ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_cases_smoothed_per_million, group = location, color = location)) 
  + geom_line() + ggtitle('Number of new cases per million - Rolling 7 days average'))))


  output$New_deaths_pm <- renderPlotly((ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_deaths_smoothed_per_million, group = location, color = location)) 
  + geom_line() + ggtitle('Number of new deaths per million - Rolling 7 days average'))))

  output$Reproduction_rate <- renderPlotly((ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = reproduction_rate, group = location, color = location)) 
  + geom_line() + ggtitle('Reproduction rate over time'))))


  output$New_cases_map <- renderPlotly((ggplotly(ggplot(inner_join(df%>%filter(dates == input$Map_Date_Selection), world_map, by = "location"), 
  aes(long, lat, group = group))+
    geom_polygon(aes(fill = new_cases_per_million ), color = "gray") 
    + scale_fill_gradientn(colours = c("#FBFCFC","#ffba08","#faa307","#f48c06","#e85d04","#dc2f02","#d00000","#9d0208","#6a040f","#370617","#03071e"),
                        breaks=c(0,0.5,5.0,10,50,100,250,500,1000,Inf),
                        na.value = "gray"))))

  output$daily_cases <- renderPlotly(ggplotly((ggplot(df_histo%>%filter(location == 'World')%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_cases_smoothed, fill = new_cases_variation)) 
  + geom_bar(stat='identity') + scale_fill_gradient2(low="green", mid = "yellow", high="red") + geom_point(aes(x = dates, y = new_cases)) 
  + scale_shape(solid = FALSE, name = "Raw Data") + ggtitle('Daily Cases due to Covid19'))))

  output$dynamics <- renderPlotly(ggplotly((ggplot(df_market %>% filter(location == input$country_picker_2)%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = transit_stations_percent_change_from_baseline_smoothed,group = location, color = location)) 
  + geom_line() + ggtitle('Transit stations: How did the number of visitors change since the beginning of the pandemic?')))) # TO correct, group_by country

  output$Dynamics_map <- renderPlotly((ggplotly(ggplot(full_join(df_market%>%filter(dates == input$Map_Date_Selection), world_map, by = "location"), 
  aes(long, lat, group = group))+
    geom_polygon(aes(fill = transit_stations_percent_change_from_baseline_smoothed ), color = "gray") 
    + scale_fill_gradientn(colours = c("#51087E","#6C0BA9","#880ED4","#A020F0","#B24BF3","#03071e","#B7D6F7","#84BCF3","#509BDE","#2480CD","#0645A4"),
                        breaks=c(-50,-25,-10,-5,0,5,10,25,50,Inf),
                        na.value = "blue"))))


  # output$test <- renderPlotly(ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  # aes_string(x = "dates", y = input$variable_selector , group = "location", color = "location")) 
  # + geom_line() + ggtitle('Number of new  cases - Rolling 7 days average')))


  output$test_2 <- renderPlotly((ggplotly(ggplot(inner_join(df%>%filter(dates == input$Map_Date_Selection), world_map, by = "location"), 
  aes_string('long', 'lat', group = 'group'))+
    geom_polygon(aes_string(fill = input$variable_selector_2), color = "gray") 
    + scale_fill_gradientn(colours = c("#FBFCFC","#ffba08","#faa307","#f48c06","#e85d04","#dc2f02","#d00000","#9d0208","#6a040f","#370617","#03071e"),
                        breaks=c(0,0.5,5.0,10,50,100,250,500,1000,Inf),
                        na.value = "gray"))))

  output$value <- renderPrint({ input$Log_selector })



    output$test <- renderPlotly(
      
      if(input$Log_selector){
      ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = "dates", y = input$variable_selector , group = "location", color = "location")) 
  + geom_line() + scale_y_log10() + ggtitle('Number of new  cases - Rolling 7 days average'))}

  else{

    ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = "dates", y = input$variable_selector , group = "location", color = "location")) 
  + geom_line()  + ggtitle('Number of new  cases - Rolling 7 days average'))

  }
  
  
  )

 



  

  }


  shinyApp(ui, server)
  }


