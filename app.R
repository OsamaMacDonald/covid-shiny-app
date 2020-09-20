library(ggplot2)
library(tidyverse)
library(utils)
library(shiny)
library(ggthemes)
library(lubridate)
library(scales)
library(shinythemes)
library(ggrepel)
library(plotly)
response <- read.csv("response_clean.csv")

## Setting Date Format
response$Date <- ymd(response$Date)

codebook <- read.csv("codebook.csv")


## Shiny App code -------------------------------------------------------------
ui <- fluidPage(
  headerPanel("Countries Covid-19 Policy Response App"),
  tabsetPanel(
    tabPanel(
      title = "Daily Covid-19 cases and deaths by day",
      sidebarLayout(
        sidebarPanel(
          
          selectInput("country",
                      "Select Country",
                      multiple = TRUE,
                      choices = unique(response$CountryName),
                      selected = "United Kingdom"),
          
          selectInput("cd", 
                      "Select Cases/Deaths", 
                      choices= c("DailyCases", "DailyDeaths"),
                      selected = "DailyCases"),
          checkboxInput("logscale", "Display Y-axis in log10 scale", FALSE)),
        
          

        
       mainPanel(
         plotOutput("plot"))),
    ),
    
    tabPanel(
      title = "Government Policy Responses to Covid-19",
      
      fluidRow(
      column(2,
        selectInput("response_country1",
                    "Select First Country", 
                    multiple = FALSE, 
                    choices = unique(response$CountryName),
                    selected = "United Kingdom")),

      column(2,
             selectInput("response_country2",
                            "Select Second Country", 
                            multiple = FALSE, 
                            choices = unique(response$CountryName),
                         selected = "Germany")),
      column(2,  
             selectInput("label1",
                         "Select Policies",
                         multiple = TRUE, 
                         choices = unique(response$policy),
                         selected = "Sch"))),
      
      
      column(6, 
        plotOutput("country1_cases"),
        plotOutput("country2_cases")), 
      
    
      
      column(6,
        plotOutput("country1_deaths"),
        plotOutput("country2_deaths")),
      
      DT::DTOutput("codebook")),
    
    tabPanel(
      title = "Analysis Plot Renders",
      
      fluidRow(
        column(2,
               selectInput("country_plot",
                           "Select Country", 
                           multiple = FALSE, 
                           choices = unique(response$CountryName),
                           selected = "United Kingdom")),
        
        column(2,  
               selectInput("label2",
                           "Select Policies",
                           multiple = TRUE, 
                           choices = unique(response$policy),
                           selected = "Sch"))),
        
        
               plotOutput("country_plot_render"),
      downloadButton("download", 
                     "Download Plot"))
    
 
      
  )
)
    
    
  



## i need to spend some time making the plot look good, but the basic idea is there. 

server <- function(input, output) {
  
## Tab 1 ----------------------------------------------------------------------  
  
  output$plot <- renderPlot({
    
    plot.data <- response %>%
      filter(CountryName %in% input$country) %>%
      filter(RegionName == "",
             DailyCases >= 0)## there are negative values in the datasets for some reason, i will just filter out for now 
    
    ggline <- ggplot(plot.data, aes_string(y = input$cd)) + ## need to use aes_string to read input$datatype  
      geom_line(mapping = aes(x = Date, colour = CountryName)) +
      labs(title = "Covid-19 Cases and Deaths Over Time",
           subtitle = "Comparing multiple countries",
           y = "Cases/Deaths",
           x = "Date") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      scale_color_brewer(palette = "Set1")
    
    if(input$logscale)
      ggline <- ggline + scale_y_log10(breaks = c(1,10,100, 1000, 10000, 100000, 1000000), labels = comma)
    
    return(ggline)
    
    
  })
  
## Tab 2 -------------------------------------------------------------------- 
  
  output$country1_cases <- renderPlot({
    
    ## make labels reactive variable 
    
    
    labels1 <- response %>%
      filter(CountryName == input$response_country1,
             RegionName == "") %>%
      arrange(policy) %>%
      select(CountryName, RegionName, Date, policy, value, DailyCases, DailyDeaths)
    
    
    
    
    policy_filter <- c()
    
    for(i in 1:length(labels1$value)){
      policy_filter[i] <- case_when(labels1$value[i]>0 & labels1$value[i]!=labels1$value[i-1] ~ "start", ## larger than zero and does not equal the previous value 
                                    labels1$value[i]>0 & labels1$value[i]!=labels1$value[i+1] ~ "end") ## larger than zero and does not equal the next value 
    }
    
    df <- cbind(labels1, policy_filter)
    
    
    
    policy_start = df %>% drop_na() %>% filter(policy_filter == "start")
    
    lab1 <- policy_start %>%
      filter(policy %in% input$label1)
    

    response %>%
      filter(CountryName == input$response_country1,
             RegionName == "") %>%
      ggplot() +
      geom_col(aes(x = Date, y = DailyCases), position = "dodge", width = .3, alpha = 0.01) +
      geom_text_repel(data = lab1, aes(x = Date, y = DailyCases, label = paste(policy,"",value)), size = 4.5) +
      labs(title = paste(input$response_country1, "Covid-19 Cases over time"),
           y = "Cases",
           x = "Date") +
      theme_classic() +
      theme(text = element_text(size=20))
    
    
    ## if i set the labels to the level of the policy and rename the policies to their shorthand descriptions then i have solved the final
    ## major issue with the plot. 
    
    
  })
  
  
  output$country1_deaths <- renderPlot({
    
    
    labels1 <- response %>%
      filter(CountryName == input$response_country1,
             RegionName == "") %>%
      arrange(policy) %>%
      select(CountryName, RegionName, Date, policy, value, DailyCases, DailyDeaths)
    
    
    
    
    policy_filter <- c()
    
    for(i in 1:length(labels1$value)){
      policy_filter[i] <- case_when(labels1$value[i]>0 & labels1$value[i]!=labels1$value[i-1] ~ "start", ## larger than zero and does not equal the previous value 
                                    labels1$value[i]>0 & labels1$value[i]!=labels1$value[i+1] ~ "end") ## larger than zero and does not equal the next value 
    }
    
    df <- cbind(labels1, policy_filter)
    
    
    
    policy_start = df %>% drop_na() %>% filter(policy_filter == "start")
    
    lab1 <- policy_start %>%
      filter(policy %in% input$label1)

    

      response %>%
      filter(CountryName == input$response_country1,
             RegionName == "") %>%
      ggplot() +
      geom_col(aes(x = Date, y = DailyDeaths), position = "dodge", width = .3, alpha = 0.01) +
      geom_text(data = lab1, aes(x = Date, y = DailyDeaths, label = paste(policy,"",value)), size = 4.5, check_overlap = TRUE) +
      labs(title = paste(input$response_country1, "Covid-19 Deaths over time"),
           y = "Deaths",
           x = "Date") +
      theme_classic() +
        theme(text = element_text(size=20))
      
    
  })
  
  
  output$country2_cases <- renderPlot({
    
    ## make labels reactive variable 
    
    
    labels1 <- response %>%
      filter(CountryName == input$response_country2,
             RegionName == "") %>%
      arrange(policy) %>%
      select(CountryName, RegionName, Date, policy, value, DailyCases, DailyDeaths)
    
    
    
    
    policy_filter <- c()
    
    for(i in 1:length(labels1$value)){
      policy_filter[i] <- case_when(labels1$value[i]>0 & labels1$value[i]!=labels1$value[i-1] ~ "start", ## larger than zero and does not equal the previous value 
                                    labels1$value[i]>0 & labels1$value[i]!=labels1$value[i+1] ~ "end") ## larger than zero and does not equal the next value 
    }
    
    df <- cbind(labels1, policy_filter)
    
    
    
    policy_start = df %>% drop_na() %>% filter(policy_filter == "start")
    
    lab1 <- policy_start %>%
      filter(policy %in% input$label1)
    
    
    response %>%
      filter(CountryName == input$response_country2,
             RegionName == "") %>%
      ggplot() +
      geom_col(aes(x = Date, y = DailyCases), position = "dodge", width = .3, alpha = 0.01) +
      geom_text_repel(data = lab1, aes(x = Date, y = DailyCases, label = paste(policy,"",value)), size = 4.5) +
      labs(title = paste(input$response_country2, "Covid-19 Cases over time"),
           y = "Cases",
           x = "Date") +
      theme_classic() +
      theme(text = element_text(size=20))
    
    
    ## if i set the labels to the level of the policy and rename the policies to their shorthand descriptions then i have solved the final
    ## major issue with the plot. 
    
    
  })
  
  output$country2_deaths <- renderPlot({
    
    ## make labels reactive variable 
    
    
    labels1 <- response %>%
      filter(CountryName == input$response_country2,
             RegionName == "") %>%
      arrange(policy) %>%
      select(CountryName, RegionName, Date, policy, value, DailyCases, DailyDeaths)
    
    
    
    
    policy_filter <- c()
    
    for(i in 1:length(labels1$value)){
      policy_filter[i] <- case_when(labels1$value[i]>0 & labels1$value[i]!=labels1$value[i-1] ~ "start", ## larger than zero and does not equal the previous value 
                                    labels1$value[i]>0 & labels1$value[i]!=labels1$value[i+1] ~ "end") ## larger than zero and does not equal the next value 
    }
    
    df <- cbind(labels1, policy_filter)
    
    
    
    policy_start = df %>% drop_na() %>% filter(policy_filter == "start")
    
    lab1 <- policy_start %>%
      filter(policy %in% input$label1)
    
    
    response %>%
      filter(CountryName == input$response_country2,
             RegionName == "") %>%
      ggplot() +
      geom_col(aes(x = Date, y = DailyDeaths), position = "dodge", width = .3, alpha = 0.01) +
      geom_text_repel(data = lab1, aes(x = Date, y = DailyDeaths, label = paste(policy,"",value)), size = 4.5) +
      labs(title = paste(input$response_country2, "Covid-19 Deaths over time"),
           y = "Deaths",
           x = "Date") +
      theme_classic() +
      theme(text = element_text(size=20))
    
 
    ## if i set the labels to the level of the policy and rename the policies to their shorthand descriptions then i have solved the final
    ## major issue with the plot. 
    
    
  })
  
  
  output$codebook <- DT::renderDT({
    codebook
  })

  
## Tab 3 ----------------------------------------------------------------------
 
   vals <- reactiveValues()
  
  output$country_plot_render <- renderPlot({
    
    
    labels1 <- response %>%
      filter(CountryName == input$country_plot,
             RegionName == "") %>%
      arrange(policy) %>%
      select(CountryName, RegionName, Date, policy, value, DailyCases, DailyDeaths)
    
    
    
    
    policy_filter <- c()
    
    for(i in 1:length(labels1$value)){
      policy_filter[i] <- case_when(labels1$value[i]>0 & labels1$value[i]!=labels1$value[i-1] ~ "start", ## larger than zero and does not equal the previous value 
                                    labels1$value[i]>0 & labels1$value[i]!=labels1$value[i+1] ~ "end") ## larger than zero and does not equal the next value 
    }
    
    df <- cbind(labels1, policy_filter)
    
    
    
    policy_start = df %>% drop_na() %>% filter(policy_filter == "start")
    
    lab1 <- policy_start %>%
      filter(policy %in% input$label1)
    
    
    gg <-response %>%
      filter(CountryName == input$country_plot,
             RegionName == "") %>%
      ggplot() +
      geom_col(aes(x = Date, y = DailyCases), position = "dodge", width = .3, alpha = 0.01) +
      geom_text_repel(data = lab1, aes(x = Date, y = DailyCases, label = paste(policy,"",value)), size = 4.5) +
      labs(title = paste(input$response_country1, "Covid-19 Cases over time"),
           y = "Cases",
           x = "Date") +
      theme_classic() +
      theme(text = element_text(size=20))

 
    
    print(gg)
    
    vals$gg <- gg

  })
  




output$download <- downloadHandler(
  filename = function(){paste(input$country_plot, '.pdf', sep = '')},
  
  content = function(file){
    pdf(file, width = 18 , height = 8)
    print(vals$gg)
    dev.off()
  })
}

  



## it wasn't plotting the multiple lines correctly before but it is now, no idea why. 
shinyApp(ui = ui, server = server)

#ross smells




