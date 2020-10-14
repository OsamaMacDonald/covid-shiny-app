library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)


## The government tracker data is now tracking subnational data 
## this means i need to filter by region name to filter for either a whole country or its regional areas
## the region names that are blank denote the overall country values 
response <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv", fileEncoding = "UTF-8-BOM")


## setting correct date format with lubridate 
response$Date <- ymd(response$Date)




## Now to implement into the response dataset ---------------------------------

##I am filling the values downwards, by group. Doing it by group is essential as it doesn't create cascading values all the 
##way down and allows me to retain the leading NA values 
 
response <- response %>%
              group_by(CountryName, RegionName) %>%
                fill(ConfirmedCases, .direction = "down")

response <- response %>%
                mutate(lag1 = ConfirmedCases - lag(ConfirmedCases))

response <- response %>% mutate(DailyCases = replace_na(coalesce(lag1, ConfirmedCases), 0))

## test if it worked

test <- response %>% 
  filter(CountryName == "United Kingdom") %>%
  select(-!c("CountryName", "RegionName", "Date", "ConfirmedCases", "lag1", "DailyCases"))

## it has successfully performed the fill by country and region 



## daily deaths ---------------------------------------------------------------

response <- response %>%
            group_by(CountryName, RegionName) %>%
            fill(ConfirmedDeaths, .direction = "down")

response <- response %>%
            mutate(lag2 = ConfirmedDeaths - lag(ConfirmedDeaths))

response <- response %>% mutate(DailyDeaths = replace_na(coalesce(lag2, ConfirmedDeaths), 0))


## testing if it worked 
test2 <- response %>% 
  filter(CountryName == "United Kingdom") %>%
  select(-!c("CountryName", "RegionName", "Date", "ConfirmedDeaths", "lag2", "DailyDeaths"))






##test plot -------------------------------------------------------------------           

## just confirming the right number of rows are being used. 
test3 <- response %>%
         filter(CountryName == "United Kingdom",
         RegionName == "")


response %>%
  filter(CountryName == "United Kingdom",
         RegionName == "") %>%
  filter(DailyCases >= 0) %>%
  ggplot(aes(x = Date, y = DailyCases)) +
  geom_col(position = "dodge", width= .4)





## attempting to pivot and plot -----------------------------------------------

  
policy_plot <- response %>%
  select(-contains("Flag")) %>%
  pivot_longer(
    cols = C1_School.closing:M1_Wildcard,
    names_to = "policy",
    values_to = "value") %>%
  mutate(policy = case_when(
    policy == "C1_School.closing" ~ "Sch",
    policy == "C2_Workplace.closing" ~ "Wrk",
    policy == "C3_Cancel.public.events" ~ "CPE",
    policy == "C4_Restrictions.on.gatherings" ~ "Gathr",
    policy == "C5_Close.public.transport" ~ "PubTran",
    policy == "C6_Stay.at.home.requirements" ~ "Home_req",
    policy == "C7_Restrictions.on.internal.movement" ~ "Move_req",
    policy == "C8_International.travel.controls" ~ "Int_Trav",
    policy == "E1_Income.support" ~ "Inc_sup",
    policy == "E2_Debt.contract.relief" ~ "Contr",
    policy == "E3_Fiscal.measures" ~ "Fisc",
    policy == "E4_International.support" ~ "Int_sup",
    policy == "H1_Public.information.campaigns" ~ "Pub_inf",
    policy == "H2_Testing.policy" ~ "Test",
    policy == "H3_Contact.tracing" ~ "Con_trac",
    policy == "H4_Emergency.investment.in.healthcare" ~ "Inv_hel",
    policy == "H5_Investment.in.vaccines" ~ "Inv_vacc",
    policy == "M1_Wildcard" ~ "Wild"))


## Creating labels for geom_text layer ----------------------------------------

## i forgot that since i have calculated everything within one dataset, i can actually make another dataframe which just has when each policy 
## was introduced, at what date, and set the position to be above the bar plots. 


labels2 <- policy_plot %>%
  filter(CountryName == "United Kingdom",
         RegionName == "") %>%
  arrange(policy) %>%
  select(CountryName, RegionName, Date, policy, value, DailyCases, DailyDeaths)




policy_filter <- c()

for(i in 1:length(labels2$value)){
  policy_filter[i] <- case_when(labels2$value[i]>0 & labels2$value[i]!=labels2$value[i-1] ~ "start", ## larger than zero and does not equal the previous value 
                         labels2$value[i]>0 & labels2$value[i]!=labels2$value[i+1] ~ "end") ## larger than zero and does not equal the next value 
}

df <- cbind(labels2, policy_filter)



policy_start = df %>% drop_na() %>% filter(...8 == "start")
policy_start


response %>%
    filter(CountryName == "United Kingdom") %>%
    filter(DailyCases >= 0) %>%
      ggplot(aes(x = Date, y = DailyCases)) +
        geom_col(position = "dodge", width = .4) +
        geom_text_repel(data = policy_start, aes(x = Date, y = DailyCases, label = paste(policy, "", label_number_si()(value))))




## saving cleaned dataset -----------------------------------------------------

write.csv(policy_plot, "response_clean.csv")





