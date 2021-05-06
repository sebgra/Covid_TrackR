  rm(list = ls())

  if (interactive()){

  library(dplyr)
  library(tidyr)
  library(shiny)
  library(shinythemes)
  library(ggplot2); theme_set(theme_classic())
  library(plotly)
  library(DT)
  library(shinyWidgets)
  library(lubridate)
  library(stringr)
  library(rmarkdown)


  options(shiny.maxRequestSize=30*1024^2)
  options(repr.plot.width=800, repr.plot.height=500)
  theme_update(plot.title = element_text(hjust = 0.5))

  data_presence <- F

  if (data_presence){

  download.file('https://covid.ourworldindata.org/data/owid-covid-data.csv', 'data.csv')
  download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', 'data_mobility.csv')
  }



  df<- read.csv('/home/sebastien/Bureau/Covid_TrackR/data.csv', header=TRUE, sep =',')
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

  df_mobility <- read.csv('/home/sebastien/Bureau/Covid_TrackR/data_mobility.csv', header = TRUE, sep  =',')
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
  ui <- fluidPage(
    includeCSS("www/special_mode.css"),
    theme = shinytheme("sandstone"),

    # App title ----
    titlePanel("Covid TrackR"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      dropdownButton(
        switchInput(inputId = "Log_selector", value = TRUE, label='Axis as', onLabel = 'Log', offLabel = 'Linear', size = 'mini'),
        # Input: Select the random distribution type ----
        selectInput("variable_selector", "Select variable to plot:", 
                    choices=c("total_cases","reproduction_rate","total_deaths","Fatality_rate","total_tests","positive_rate","total_vaccinations","people_vaccinated","people_fully_vaccinated")),

        awesomeCheckbox(
                    inputId = "Add_vaccination",
                    label = "Add vaccination curve on graph", 
                      value = TRUE
                  ),

        selectInput("interval_selector", "Select interval to plot:", # TO MODIFY
                    choices=c("total_cases","reproduction_rate","total_deaths","total_tests","positive_rate","total_vaccinations","people_vaccinated","people_fully_vaccinated"), width = '450px'),      
              
        selectInput("variable_selector_2", "Select variable to plot:", 
                    choices=c("transit_stations_percent_change_from_baseline_smoothed","workplaces_percent_change_from_baseline_smoothed","residential_percent_change_from_baseline_smoothed","parks_percent_change_from_baseline_smoothed","grocery_and_pharmacy_percent_change_from_baseline_smoothed","retail_and_recreation_percent_change_from_baseline_smoothed"), width = '450px'),

        sliderInput('Date_selection',label = 'Date Selection', min = min(df$dates), max = max(df$dates), value = c(min(df$dates), max(df$dates)), width = '450px'),
        # br() element to introduce extra vertical spacing ----
        br(),

        # Input: Slider for the number of observations to generate ----
        sliderInput("Map_Date_Selection",
                    "Map Date Selection :",
                    value = max(df$dates),
                    min = min(df$dates), max = max(df$dates), width = '450px'),
        
        pickerInput(
          inputId = "country_picker", # TODO :Change Name
          label = "Country Selection for charts",
          choices = df%>%select(location)%>%unique()%>%c(),#%>%sapply(levels),
          selected = "France",
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 4"
          ),
          multiple = TRUE, width = '450px'
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
          multiple = TRUE, width = '450px'
  ),
  
  actionButton(inputId = "reload", label = "Refresh data"), 
  
  right = T,icon = icon("gear"),  tooltip = tooltipOptions(title = "Click to see inputs !"), width = 500),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",

              tabPanel('Charts',br(), h3('Use first selector to plot the desired data'), plotlyOutput('Main_variable'),
                br(), h3('Total Cases Chart'), plotlyOutput(("Total_cases")), br(),
                br(), h3('Mortality Chart'), plotlyOutput("Mortality_graph"), br(),
                br(), h3('Total deaths Chart'), plotlyOutput('Total_deaths'), br(),
                br(), h3('New cases per million Chart'), plotlyOutput('New_cases_pm'), br(),
                br(), h3('New deaths per million Chart'), plotlyOutput('New_deaths_pm'), br(),
                br(), h3('Daily Cases Chart'), plotlyOutput("daily_cases"), br(),
                br(), h3('Reproduction rate Chart'), plotlyOutput('Reproduction_rate')), # TO MODIFY
              
              tabPanel("Map", br(), h3('New cases per million per country map'), plotlyOutput("New_cases_map")),
              
              tabPanel("People Dynamics", br(), h3('Use first selector to plot the desired data'), plotlyOutput("dynamics"),
                br(), h3('People dynamics map'),plotlyOutput("Dynamics_map")),
              
              tabPanel("Sources"),
              
              tabPanel("Download report", h4('Download Covid_19 data'),br(), downloadButton("downloadData", "Download"), br(),
                h4('Download people dynamics data'),br(), downloadButton("downloadData_dynamics", "Download"),br(),
                h4('Download complete report'),br(), radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                inline = TRUE), downloadButton('downloadReport')),
              
              tabPanel("Table", br(), h3('Data used for charts'), br(), DT::dataTableOutput('Data_Charts'),
                br(), h3('Data used for people dynamics'), br(), DT::dataTableOutput('Data_dynamics'))
        )

      )
    )
  )


  server <- function(input, output) {

    # Generate an HTML table view of the data ----

    observeEvent(input$reload, {
    df <- reactiveValues()
    df_mobility <- reactiveValues()

    url_data <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
    url_dynamics <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

    download.file(url_data, 'data.csv')
    download.file(url_dynamics, 'data_mobility.csv')

      df<- read.csv('/home/sebastien/Bureau/Covid_TrackR/data.csv', header=TRUE, sep =',')
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

  df_mobility <- read.csv('/home/sebastien/Bureau/Covid_TrackR/data_mobility.csv', header = TRUE, sep  =',')
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

  })

  output$Data_Charts <- DT::renderDataTable({DT::datatable(df)})

  output$Data_dynamics <- DT::renderDataTable({DT::datatable(df_market)})



  output$New_cases <- renderPlotly(
    
    if(input$Log_selector){
    ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = dates, y = input$variable_selector , group = location, color = location)) 
  + geom_line() + scale_y_log10() + ggtitle('Number of new  cases - Rolling 7 days average'))}

  else{
    ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = dates, y = input$variable_selector , group = location, color = location)) 
  + geom_line() + ggtitle('Number of new  cases - Rolling 7 days average'))

  })



  output$Total_cases <- renderPlotly(

    if(input$Add_vaccination){

          if(input$Log_selector){
    
    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), aes(x = dates, y = total_cases, group = location, color = location)) 
  + geom_line() + scale_y_log10() + ggtitle('Total cases') 
  + geom_line(aes(y=total_vaccinations),linetype ='dotdash')))
    }

    else{

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), aes(x = dates, y = total_cases, group = location, color = location)) 
  + geom_line() + ggtitle('Total cases') 
  + geom_line(aes(y=total_vaccinations),linetype ='dotdash')))

}}
    

else{

    if(input$Log_selector){
    
    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), aes(x = dates, y = total_cases, group = location, color = location)) 
  + geom_line() + scale_y_log10() + ggtitle('Total cases') 
  ))
    }

    else{

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), aes(x = dates, y = total_cases, group = location, color = location)) 
  + geom_line() + ggtitle('Total cases') 
  ))

}}
)


  output$Mortality_graph <- renderPlotly(
    
    if(input$Log_selector){
    
    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker)%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes( x = dates, y = new_deaths_smoothed,group = location, color = location)) 
  + geom_line() + scale_y_log10() + ggtitle('Number of new  death - Rolling 7 days average')))}

    else{

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker)%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
    aes( x = dates, y = new_deaths_smoothed,group = location, color = location)) 
    + geom_line() + ggtitle('Number of new  death - Rolling 7 days average')))

    })

  output$Total_deaths <- renderPlotly(
    
    if(input$Log_selector){
    
    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = total_deaths, group = location, color = location)) 
  + geom_line()+ scale_y_log10() + ggtitle('Total deaths')))}

    else{

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = total_deaths, group = location, color = location)) 
  + geom_line() + ggtitle('Total deaths')))

    })

  output$New_cases_pm <- renderPlotly(
    
    if(input$Log_selector){

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_cases_smoothed_per_million, group = location, color = location)) 
  + geom_line() + scale_y_log10() + ggtitle('Number of new cases per million - Rolling 7 days average')))}

  else{

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_cases_smoothed_per_million, group = location, color = location)) 
  + geom_line() + ggtitle('Number of new cases per million - Rolling 7 days average')))

  })


  output$New_deaths_pm <- renderPlotly(
    
    if(input$Log_selector){
    
    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_deaths_smoothed_per_million, group = location, color = location)) 
  + geom_line() + scale_y_log10() + ggtitle('Number of new deaths per million - Rolling 7 days average')))}

  else{

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_deaths_smoothed_per_million, group = location, color = location)) 
  + geom_line() + ggtitle('Number of new deaths per million - Rolling 7 days average')))

  })

  output$Reproduction_rate <- renderPlotly(
    
    if(input$Log_selector){
    
    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = reproduction_rate, group = location, color = location)) 
  + geom_line() + scale_y_log10() + ggtitle('Reproduction rate over time')))}

  else{

    (ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = reproduction_rate, group = location, color = location)) 
  + geom_line() + ggtitle('Reproduction rate over time')))

  })


  output$New_cases_map <- renderPlotly((ggplotly(ggplot(inner_join(df%>%filter(dates == input$Map_Date_Selection), world_map, by = "location"), 
  aes(long, lat, group = group))+
    geom_polygon(aes(fill = new_cases_per_million ), color = "gray") 
    + scale_fill_gradientn(colours = c('#FFFFFF','#ffff61','#ffec44','#ffd326','#ffb409','#eb8b00','#ce6500','#b14500','#932a00','#761600','#000000'),
                            breaks=c(0,100,250,500,750,1000,1500,Inf),
                        na.value = "gray") + ggtitle('New cases per million map') + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10, title = 'New cases per million')))))




  output$daily_cases <- renderPlotly(
    
    if(input$Log_selector){
    
    ggplotly((ggplot(df_histo%>%filter(location == 'World')%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_cases_smoothed, fill = new_cases_variation)) 
  + geom_bar(stat='identity') + scale_fill_gradient2(low="green", mid = "yellow", high="red") + geom_point(aes(x = dates, y = new_cases)) 
  + scale_shape(solid = FALSE, name = "Raw Data") + scale_y_log10() + ggtitle('Daily Cases due to Covid19')))}

  else{

    ggplotly((ggplot(df_histo%>%filter(location == 'World')%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes(x = dates, y = new_cases_smoothed, fill = new_cases_variation)) 
  + geom_bar(stat='identity') + scale_fill_gradient2(low="green", mid = "yellow", high="red") + geom_point(aes(x = dates, y = new_cases)) 
  + scale_shape(solid = FALSE, name = "Raw Data") + ggtitle('Daily Cases due to Covid19')))

  })

  output$dynamics <- renderPlotly(ggplotly((ggplot(df_market %>% filter(location == input$country_picker_2)%>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = 'dates', y = input$variable_selector_2 ,group = 'location', color = 'location')) 
  + geom_line() + ggtitle('How did the pandemic affect people dynamics ?') + labs(y = str_replace_all(paste(input$variable_selector_2),"_"," "))))) # TO correct, group_by country

  output$Dynamics_map <- renderPlotly((ggplotly(ggplot(full_join(df_market%>%filter(dates == input$Map_Date_Selection), world_map, by = "location"), 
  aes_string('long', 'lat', group = 'group'))+
    geom_polygon(aes_string(fill = input$variable_selector_2), color = "gray") + ggtitle(str_replace_all(paste(input$variable_selector_2, "map"),"_"," "))
    + scale_fill_gradientn(colours = c('#00884c','#00b565','#00e27e','#0fff95','#3cffa9','#69ffbd','#ffffff','#b288d9','#9b5dd8','#822fdc','#6a16c9','#5209ab','#3b0088'),
                        breaks=c(-200,-100,-75,-50,-25,0,25,50,75,100,200),
                        na.value = "gray") +  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10, title = str_replace_all(paste(input$variable_selector_2),"_"," "))))))


    output$Main_variable <- renderPlotly(
      
      if(input$Log_selector){
      ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = "dates", y = input$variable_selector , group = "location", color = "location")) + labs(y= str_replace_all(paste(input$variable_selector),"_"," "))
  + geom_line() + scale_y_log10() + ggtitle(str_replace(paste(input$variable_selector),"_"," ")))}

  else{

    ggplotly(ggplot(data = df%>%filter(location == input$country_picker) %>% filter(dates > input$Date_selection[1] & dates < input$Date_selection[2]), 
  aes_string(x = "dates", y = input$variable_selector , group = "location", color = "location")) + labs(y= str_replace_all(paste(input$variable_selector),"_"," ")) 
  + geom_line()  + ggtitle(str_replace(paste(input$variable_selector),"_"," ")))

  })

   # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Covid_19_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$downloadData_dynamics <- downloadHandler(
    filename = function() {
      paste("People_dynamics_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_market, file, row.names = FALSE)
    }
  )


  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

  


  


    content = function(file){
      src <- normalizePath('report_file.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report_file.Rmd', overwrite = TRUE)

      out <- render('report_file.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    })

  }


  shinyApp(ui, server)
  }

