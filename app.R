library(ggplot2)
library(tidyverse)
library(utils)
library(shiny)
library(ggthemes)
library(lubridate)
library(scales)
library(shinythemes)
library(ggrepel)
library(scales)
library(maps)
library(gganimate)

## loading data ---------------------------------------------------------------
response <- read.csv("response_clean.csv")

## Setting Date Format
response$Date <- ymd(response$Date)

codebook <- read.csv("codebook.csv") # codebook with policy details

map2 <- read.csv("map-data.csv") # world map data 
map2$date <- ymd(map2$date)


## UI code --------------------------------------------------------------------
ui <- fluidPage(
  headerPanel("Countries Covid-19 Policy Response App"),
  tabsetPanel(
    
    ## Panel 1 ----------------------------------------------------------------
    tabPanel(title = "Daily Covid-19 cases and deaths by day",
             fluidRow(
               column(
                 4,
                 selectInput(
                   "country",
                   "Select Country",
                   multiple = TRUE,
                   choices = unique(response$CountryName),
                   selected = "United Kingdom"
                 ),
                 
                 selectInput(
                   "cd",
                   "Select Cases/Deaths",
                   choices = c("DailyCases", "DailyDeaths"),
                   selected = "DailyCases"
                 ),
                 checkboxInput("logscale", "Display Y-axis in log10 scale", FALSE)
               ),
               
               column(8,
                      
                      plotOutput("plot"))
             ),
             
             
             fluidRow(
               column(
                 4, 
                  selectInput(
                   "country2",
                   "Select Country",
                   multiple = TRUE,
                   choices = unique(map2$location),
                   selected = "United Kingdom"
                 ),
                 
                 selectInput(
                   "per_capita_cd",
                   "Select Cases/Deaths Per Capita",
                   choices = c("new_cases_per_100k", "new_deaths_per_100k"),
                   selected = "new_cases_per_100k"
                 ),
                 
                 checkboxInput("logscale2", "Display Y-axis in log10 scale", FALSE),
                 
               ),
               
               column(
                 8,
                 plotOutput("per_capita_plot")
               )
               
             )
             
             
             
             
             
             
             ),
    ## Panel 2 ----------------------------------------------------------------
    tabPanel(
      title = "Government Policy Responses to Covid-19",
      
      fluidRow(
        column(
          2,
          selectInput(
            "response_country1",
            "Select First Country",
            multiple = FALSE,
            choices = unique(response$CountryName),
            selected = "United Kingdom"
          )
        ),
        
        column(
          2,
          selectInput(
            "response_country2",
            "Select Second Country",
            multiple = FALSE,
            choices = unique(response$CountryName),
            selected = "Germany"
          )
        ),
        column(
          2,
          selectInput(
            "label1",
            "Select Policies",
            multiple = TRUE,
            choices = unique(response$policy),
            selected = "Sch"
          )
        )
      ),
      
      
      column(
        6,
        plotOutput("country1_cases"),
        plotOutput("country2_cases")
      ),
      
      
      
      column(
        6,
        plotOutput("country1_deaths"),
        plotOutput("country2_deaths")
      ),
      
      DT::DTOutput("codebook")
    ),
    
    
    
    ## Panel 3 ----------------------------------------------------------------
    tabPanel(title = "United Kingdom Sub-Regional Data",
             
             fluidRow(
              column(2,
              selectInput(
               "uk_regions",
               "Select Country",
               multiple = TRUE,
               choices = c("England", "Scotland", "Wales", "Northern Ireland"),
               selected = c("England", "Scotland", "Wales", "Northern Ireland")
             ),
             
             selectInput(
               "cd2",
               "Select Cases/Deaths",
               choices = c("DailyCases", "DailyDeaths"),
               selected = "DailyCases"
             ),
             checkboxInput("logscale2", "Display Y-axis in log10 scale", FALSE)),
             
             column(10,
             plotOutput("uk_plot")))
             
             
            ),
    
    
    ## Panel 4 ----------------------------------------------------------------
    tabPanel(
      title = "Analysis Plot Renders",
      
      fluidRow(column(
        2,
        selectInput(
          "country_plot",
          "Select Country",
          multiple = FALSE,
          choices = unique(response$CountryName),
          selected = "United Kingdom"
        )
      ),
      
      column(
        2,
        selectInput(
          "label2",
          "Select Policies",
          multiple = TRUE,
          choices = unique(response$policy),
          selected = "Sch"
        )
      ),
      
      column(
        2,
        sliderInput(
          "text_size",
          "Adjust Label Text Size",
          min = 3,
          max = 6,
          step = .25,
          value = 4.5
        )
             
             ),
      column(
        2,
        sliderInput(
          "force_value",
          "Adjust Label Y-axis Repulsion",
          min = 3,
          max = 15,
          value = 9
        )
      )),
      column(
        8,
        plotOutput("country_plot_render"),
        
        fluidRow(
          downloadButton("download",
                         "Download Plot"),
          downloadButton("download2",
                         "Download Policy Data")
        )
      ),
      
      column(
        8,
        plotOutput("country_plot_render2"),
        fluidRow(downloadButton("download3",
                                "Download Plot"))
      )
    ),
    
    ## Panel 5 ----------------------------------------------------------------
    tabPanel(
      title = "World Map",
      
      sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "date_slider", 
                      label = "Dates:",
                      min = as.Date(min(map2$date)),
                      max = as.Date(max(map2$date)),
                      value = as.Date(min(map2$date)), 
                      step = 1,
                      animate = animationOptions(interval = 1800))),
        mainPanel(plotOutput(outputId = "animated_map", height = "70vh")))
      
    )
    
    
  )
)







  ## Server Code --------------------------------------------------------------

server <- function(input, output) {
  ## Tab 1 --------------------------------------------------------------------
  
  
  
  output$plot <- renderPlot({
    
    # filtering out regional data and negative case values 
    plot.data <- response %>%
      filter(CountryName %in% input$country) %>%
      filter(RegionName == "",
             DailyCases >= 0)## there are negative values in the datasets for some reason, i will just filter out for now
    
    
    ggline <-
      ggplot(plot.data, aes_string(y = input$cd)) + ## need to use aes_string to read input$datatype
      geom_line(mapping = aes(x = Date, colour = CountryName)) +
      labs(
        title = "Covid-19 Cases and Deaths Over Time",
        subtitle = "Comparing multiple countries",
        y = "Cases/Deaths",
        x = "Date"
      ) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0) # stops the plot going further than the available dates 
      ) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      scale_color_brewer(palette = "Set1")
    
    # if statement which adds on a log scale if the box is selected 
    if (input$logscale)
      ggline <- ggline + 
        scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                      labels = comma)
    
    return(ggline)
    
    
  })
  
  output$per_capita_plot <- renderPlot({
    
    plot.data2 <- map2 %>%
      filter(location %in% input$country2)
    
    
    
    ggline2 <-
      ggplot(plot.data2, aes_string(y = input$per_capita_cd)) + ## need to use aes_string to read input$datatype
      geom_line(mapping = aes(x = date, colour = location)) +
      labs(
        title = "Covid-19 Cases and Deaths Over Time per capita (100k)",
        subtitle = "Comparing multiple countries",
        y = "Cases/Deaths",
        x = "Date"
      ) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0) # stops the plot going further than the available dates 
      ) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      scale_color_brewer(palette = "Set1")
    
    
    if (input$logscale2)
      ggline2 <- ggline2 + 
      scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                    labels = comma)
    
    
    return(ggline2)
    
    
    
    
  })
  
  ## Tab 2 --------------------------------------------------------------------
  
  ## creating a reactive variable as these labels are used multiple times 
  label <- reactive({
    
    ## using the country input to filter for a specific countries data 
    labels1 <- response %>%
      filter(CountryName == input$response_country1,
             RegionName == "") %>%
      arrange(policy) %>%
      select(CountryName,
             RegionName,
             Date,
             policy,
             value,
             DailyCases,
             DailyDeaths)
    
    
    
    ## Finding the start and end date of policies for the purpose of plotting and analysis 
    ## Currently, only start dates are plotted due to overcrowding. 
    policy_filter <- c()
    
    for (i in 1:length(labels1$value)) {
      policy_filter[i] <-
        case_when(
          labels1$value[i] > 0 &
            labels1$value[i] != labels1$value[i - 1] ~ "start",
          ## larger than zero and does not equal the previous value
          labels1$value[i] > 0 &
            labels1$value[i] != labels1$value[i + 1] ~ "end"
        ) ## larger than zero and does not equal the next value
    }
    
    
    df <- cbind(labels1, policy_filter)
    
    
    # filtering for start dates only 
    policy_start = df %>% drop_na() %>% filter(policy_filter == "start")
    
    # filtering for policies selected by the user 
    lab1 <- policy_start %>%
      filter(policy %in% input$label1)
    
    
  })
  
  output$country1_cases <- renderPlot({

    # assigning the reactive variable function to an object to be used in plotting 
    lab1 <- label()
    
    
    
    response %>%
      filter(CountryName == input$response_country1,
             RegionName == "") %>%
      ggplot() +
      geom_col(
        aes(x = Date, y = DailyCases),
        position = "dodge",
        width = .3,
        alpha = 0.01 # making the bars more transparent for better readability of the labels 
      ) +
      geom_text_repel(data = lab1, # geom_text_repel helps with the overcrowding issue 
                      aes(
                        x = Date,
                        y = DailyCases,
                        label = paste(policy, "", label_number_si()(value)) # label_number_si() formats the large fiscal numbers to save space
                      ),                                                     
                      direction = "y",
                      force = 6,
                      min.segment.length = 100,
                      size = 4.5) + # making the label text larger for readability
      labs(
        title = paste(input$response_country1, "Covid-19 Cases over time"),
        y = "Cases",
        x = "Date"
      ) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0)
      ) + 
      theme_classic() +
      theme(text = element_text(size = 20))
    
    
  })
  
  
  output$country1_deaths <- renderPlot({
    
    # same process as before but for deaths 
    
    lab1 <- label()
    
    
    response %>%
      filter(CountryName == input$response_country1,
             RegionName == "") %>%
      ggplot() +
      geom_col(
        aes(x = Date, y = DailyDeaths),
        position = "dodge",
        width = .3,
        alpha = 0.01
      ) +
      geom_text_repel(
        data = lab1,
        aes(
          x = Date,
          y = DailyDeaths,
          label = paste(policy, "", label_number_si()(value))
        ),
        direction = "y",
        force = 6,
        min.segment.length = 100,
        size = 4.5
      ) +
      labs(
        title = paste(input$response_country1, "Covid-19 Deaths over time"),
        y = "Deaths",
        x = "Date"
      ) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0)
      ) + 
      theme_classic() +
      theme(text = element_text(size = 20))
    
    
  })
  
  
  label2 <- reactive({
    
    # same process but for a new country 
    
    labels1 <- response %>%
      filter(CountryName == input$response_country2,
             RegionName == "") %>%
      arrange(policy) %>%
      select(CountryName,
             RegionName,
             Date,
             policy,
             value,
             DailyCases,
             DailyDeaths)
    
    
    
    
    policy_filter <- c()
    
    for (i in 1:length(labels1$value)) {
      policy_filter[i] <-
        case_when(
          labels1$value[i] > 0 &
            labels1$value[i] != labels1$value[i - 1] ~ "start",
          ## larger than zero and does not equal the previous value
          labels1$value[i] > 0 &
            labels1$value[i] != labels1$value[i + 1] ~ "end"
        ) ## larger than zero and does not equal the next value
    }
    
    df <- cbind(labels1, policy_filter)
    
    
    
    policy_start = df %>% drop_na() %>% filter(policy_filter == "start")
    
    lab1 <- policy_start %>%
      filter(policy %in% input$label1)
    
    
  })
  
  
  
  output$country2_cases <- renderPlot({

    
    # exact same code as before but for country two 
    lab1 <- label2()
    
    response %>%
      filter(CountryName == input$response_country2,
             RegionName == "") %>%
      ggplot() +
      geom_col(
        aes(x = Date, y = DailyCases),
        position = "dodge",
        width = .3,
        alpha = 0.01
      ) +
      geom_text_repel(data = lab1,
                      aes(
                        x = Date,
                        y = DailyCases,
                        label = paste(policy, "", label_number_si()(value))
                      ),
                      direction = "y",
                      force = 6,
                      min.segment.length = 100,
                      size = 4.5) +
      labs(
        title = paste(input$response_country2, "Covid-19 Cases over time"),
        y = "Cases",
        x = "Date"
      ) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0)
      ) + 
      theme_classic() +
      theme(text = element_text(size = 20))
    
    
    ## if i set the labels to the level of the policy and rename the policies to their shorthand descriptions then i have solved the final
    ## major issue with the plot.
    
    
  })
  
  output$country2_deaths <- renderPlot({

    # exact same code as before but for country two 
    lab1 <- label2()
    
    
    response %>%
      filter(CountryName == input$response_country2,
             RegionName == "") %>%
      ggplot() +
      geom_col(
        aes(x = Date, y = DailyDeaths),
        position = "dodge",
        width = .3,
        alpha = 0.01
      ) +
      geom_text_repel(data = lab1,
                      aes(
                        x = Date,
                        y = DailyDeaths,
                        label = paste(policy, "", label_number_si()(value))
                      ),
                      direction = "y",
                      force = 6,
                      min.segment.length = 100,
                      size = 4.5) +
      labs(
        title = paste(input$response_country2, "Covid-19 Deaths over time"),
        y = "Deaths",
        x = "Date"
      ) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0)
      ) + 
      theme_classic() +
      theme(text = element_text(size = 20))
    
    
    
  })
  
  #rendering the codebook so the reader can see what the shorthand policies refer to 
  output$codebook <- DT::renderDT({
    codebook
  })
  
  ## Tab 3 --------------------------------------------------------------------
  
  
  output$uk_plot <- renderPlot({
    
    # filtering out regional data and negative case values 
    plot.data2 <- response %>%
      filter(CountryName == "United Kingdom") %>% 
      filter(RegionName %in% input$uk_regions) %>%
      filter(RegionName != "",
             DailyCases >= 0)## there are negative values in the datasets for some reason, i will just filter out for now
    
    
    ggline2 <-
      ggplot(plot.data2, aes_string(y = input$cd2)) + ## need to use aes_string to read input$datatype
      geom_line(mapping = aes(x = Date, colour = RegionName)) +
      labs(
        title = "Covid-19 Cases and Deaths Over Time",
        subtitle = "Comparing the regions of the United kingdom",
        y = "Cases/Deaths",
        x = "Date"
      ) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0)
      ) + 
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      scale_color_brewer(palette = "Set1")
    
    # if statement which adds on a log scale if the box is selected 
    if (input$logscale2)
      ggline2 <- ggline2 + 
      scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                    labels = comma)
    
    return(ggline2)
    
    
  })
  
  
  ## Tab 4 --------------------------------------------------------------------
  
  
  # This tab is being used for the purpose of downloading data and pdfs of the plots 
  vals <- reactiveValues()
  
  labels <- reactive({
    
    # Same process used in the policy tab 
    label2 <- response %>%
      filter(CountryName == input$country_plot,
             RegionName == "") %>%
      arrange(policy) %>%
      select(
        CountryName,
        RegionName,
        Date,
        policy,
        value,
        DailyCases,
        DailyDeaths,
        ConfirmedCases,
        ConfirmedDeaths
      )
    
    
    
    
    policy_filter2 <- c()
    
    for (i in 1:length(label2$value)) {
      policy_filter2[i] <-
        case_when(
          label2$value[i] > 0 &
            label2$value[i] != label2$value[i - 1] ~ "start",
          ## larger than zero and does not equal the previous value
          label2$value[i] > 0 &
            label2$value[i] != label2$value[i + 1] ~ "end"
        ) ## larger than zero and does not equal the next value
    }
    
    df2 <- cbind(label2, policy_filter2)
    
    
    
    policy_start2 = df2 %>% drop_na()
    
    labs2 <- policy_start2 %>%
      filter(policy %in% input$label2)
    
    
    
  })
  
  output$country_plot_render <- renderPlot({
    labs2 <- labels()
    labs2 <- labs2 %>% filter(policy_filter2 == "start")
    
    gg <- response %>%
      filter(CountryName == input$country_plot,
             RegionName == "") %>%
      ggplot() +
      geom_col(
        aes(x = Date, y = DailyCases),
        position = "dodge",
        width = .3,
        alpha = 0.01
      ) +
      geom_text_repel(
        data = labs2,
        aes(
          x = Date,
          y = DailyCases,
          label = paste(policy, "", label_number_si()(value))
        ),
        direction = "y",
        force = input$force_value,
        min.segment.length = 100,
        size = input$text_size
      ) +
      labs(
        title = paste(input$country_plot, "Covid-19 Cases over time"),
        y = "Cases",
        x = "Date"
      ) +
      scale_y_continuous(breaks = pretty_breaks()) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0)
      ) +
      theme_classic() +
      theme(text = element_text(size = 20))
    
    
    
    print(gg)
    
    vals$gg <- gg # the reactive values are used by the download button 
    
  })
  
  vals2 <- reactiveValues()
  
  output$country_plot_render2 <- renderPlot({
    labs2 <- labels()
    labs2 <- labs2 %>% filter(policy_filter2 == "start")
    
    gg2 <- response %>%
      filter(CountryName == input$country_plot,
             RegionName == "") %>%
      ggplot() +
      geom_col(
        aes(x = Date, y = DailyDeaths),
        position = "dodge",
        width = .3,
        alpha = 0.01
      ) +
      geom_text_repel(
        data = labs2,
        aes(
          x = Date,
          y = DailyDeaths,
          label = paste(policy, "",  label_number_si()(value))
        ),
        direction = "y",
        force = input$force_value,
        min.segment.length = 100,
        size = input$text_size
      ) +
      labs(
        title = paste(input$country_plot, "Covid-19 Deaths over time"),
        y = "Deaths",
        x = "Date"
      ) +
      scale_y_continuous(breaks = pretty_breaks()) +
      scale_x_date(
        date_breaks = "months" ,
        date_labels = "%d-%b",
        expand = c(0, 0)
      ) +
      theme_classic() +
      theme(text = element_text(size = 20))
    
    
    
    print(gg2)
    
    vals2$gg2 <- gg2
    
  })
  
  
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste(input$country_plot, '_cases.pdf', sep = '') # creates the name of the file
    },
    
    content = function(file) {            # downloading the cases plot
      pdf(file, width = 14 , height = 8)
      print(vals$gg)
      dev.off()
    }
  )
  
  
  output$download2 <- downloadHandler(
    filename = function() {
      paste(input$country_plot, '.csv', sep = '')  
    },
    
    content = function(file) { 
      write.csv(labels(), file) # downloading the unfiltered data with both start and end dates for selected policies 
    }
  )
  
  
  output$download3 <- downloadHandler(
    filename = function() {
      paste(input$country_plot, '_deaths.pdf', sep = '')
    },
    
    content = function(file) {
      pdf(file, width = 14 , height = 8) # downloading plot of deaths for selected country 
      print(vals2$gg2)
      dev.off()
    }
  )
  

  ## Tab 5 ----------------------------------------------------------------------  
  output$animated_map <- renderPlot({  ## World map output 
    
    options(scipen = 999) # to get rid of the scientific notation in the generated labels 
    
    map2 %>% 
      filter(date == input$date_slider) %>%
      ggplot() +
      borders("world", colour = "gray90", fill = "gray85") +
      theme_map() + 
      geom_point(aes(x = Longitude..average., y = Latitude..average., size = total_cases_per_100k), 
                 colour = "red", alpha = 0.55) +
      labs(size = "Cases per 100k") + 
      ggtitle("Total Covid-19 Cases over time per 100k population") 
  })

  
  
}



shinyApp(ui = ui, server = server)








