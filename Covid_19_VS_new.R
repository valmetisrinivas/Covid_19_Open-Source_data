# Load the required packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(scales)

# Read datasets/confirmed_cases_worldwide.csv into confirmed_cases_worldwide
covid_ds <- data.table::fread('https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv', drop='province') %>% 
  mutate (date=as.Date(date)) %>%
  select(-lat, -long)

# View info about the dataset
glimpse(covid_ds)
str(covid_ds)
tail(covid_ds,20)
dim(covid_ds)

covid_ds$type[covid_ds$type == 'death'] <-'dead'

# Convert negative cases to positive assuming it is typo error
#covid_ds$cases = abs(covid_ds$cases)
#covid_ds %>% filter(cases < 0) %>% select(country,cases, type)

# compute daily cases for all cuntries
daily_cases <- covid_ds %>%
  group_by(date, country, type) %>%
  summarize (daily_cases = sum(cases)) %>% 
  ungroup()

# Totals so far worldwide
totals <- covid_ds %>%
  group_by(type) %>%
  summarize(total=sum(cases))

totals %>% 
  ggplot(aes(type, total, fill=type)) +
  geom_col() +
  scale_y_continuous(labels=comma)+
  geom_text(aes(label=total), vjust=-.5) +
  labs(subtitle="Total cases world wide")

# Total daily cases across the globe
tot_daily_all_countries <- covid_ds %>% 
  group_by(date, type) %>% 
  mutate (total_daily_cases = sum(cases)) %>%
  arrange(desc(total_daily_cases)) %>%
  select(date, type, total_daily_cases) 

tot_daily_all_countries %>%
  filter(total_daily_cases == max(total_daily_cases)) %>%
  head(1)

tot_daily_all_countries %>% ggplot(aes(date,total_daily_cases, col=type)) +
  geom_point()+
  geom_line() +
  facet_wrap(~type)+
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" Total daily cases across all nations")

# Confirmed cumulative cases country_wise
confirmed <- daily_cases %>%
  filter(type=='confirmed') %>%
  select(-type) %>%
  group_by(country) %>% 
  mutate(cum_c_cases= cumsum(daily_cases)) %>% 
  select(-daily_cases) %>%
  ungroup()

# Recovered cumulative cases
recovered <- daily_cases %>%
  filter(type=='recovered') %>% 
  select(-type) %>%
  group_by(country) %>% 
  mutate(cum_r_cases= cumsum(daily_cases)) %>% 
  select(-daily_cases) %>%
  ungroup()

# dead cumulative cases
dead <- daily_cases %>%
  filter(type=='dead') %>% 
  select(-type) %>%
  group_by(country) %>% 
  mutate(cum_d_cases= cumsum(daily_cases)) %>% 
  select(-daily_cases) %>%
  ungroup() 

# Combine cumulative cases of all 3 categories
all_cases <- confirmed %>% 
  inner_join(recovered, by=c('date', 'country')) %>%
  inner_join(dead, by=c('date', 'country')) %>%
  mutate(country=as.factor(country))

# All cumultaive cases, all countries on all dates with all 3 categories - Top 50
all_cases %>% arrange(desc(date,cum_c_cases)) %>% head(50)

# All cumulative cases of all countries on all dates
countries_cases <- all_cases %>% 
  rename(confirmed=cum_c_cases, recovered=cum_r_cases, dead=cum_d_cases) %>%
  gather(type, cum_cases, confirmed:dead) %>% 
  mutate(type=as.factor(type)) %>% arrange(desc(date))

# Select, major hit nations (top 20)
top_20_countries <- covid_ds %>% group_by(country) %>% summarize(total_cases=sum(cases)) %>% arrange(desc(total_cases)) %>% head(20)
select_countries <- top_20_countries$country

countries_cases %>%
  filter(country %in% select_countries) %>%
  ggplot(aes(country, cum_cases, fill=type)) +
  geom_col( position = position_dodge(1)) +
  scale_y_continuous(labels = comma)+
  coord_flip() +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle="Total numbers across select nations")

# Cumulative cases by date and type
countries_cases_all <- countries_cases %>% 
  group_by(date, type) %>%
  mutate(tot_cum_cases = sum(cum_cases)) %>%
  arrange(desc(tot_cum_cases))

# Cumulative cases by date and type - Top 50
cum_cases_all <- countries_cases_all %>% 
  group_by(date, type) %>%
  top_n(1, wt = cum_cases) %>%
  ungroup()

cum_cases_all %>% 
  select(date, type, tot_cum_cases) %>%
  arrange(desc(date, type)) %>% head(50)

cum_cases_all %>%
  ggplot(aes(date, tot_cum_cases, col=type)) +
  geom_line(size=1) +
  scale_y_continuous(label=comma) +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" Cumulative number progression across all nations")

# Cases in select, major hot nations
select_cases <- countries_cases %>%
  filter(country %in% select_countries) 

select_cases %>%
  ggplot(aes(date, cum_cases, col=country)) +
  geom_line(size=1) +
  scale_y_continuous(label=comma) +
  facet_grid(~type) +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" Cumulative number progression across major hit nations")

# Compare China and USA
china_vs_usa <- countries_cases %>%
  filter(country %in% c('China',  'US')) 

china_vs_usa %>%
  ggplot(aes(date, cum_cases, col=country)) +
  geom_line(size=1) +
  scale_y_continuous(label=comma) +
  facet_grid(~type)  +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" China vs USA cumulative progression") 

# non-China and non_USA cases (non-extreme case countries)
non_china_usa_cases <- countries_cases %>%
  filter(country %in% select_countries & country !='China' & country != 'US')

non_china_usa_cases %>%
  ggplot(aes(date, cum_cases, col=country)) +
  geom_line(size=1) +
  facet_grid(~type)  +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  scale_y_continuous(label=comma) +
  labs(subtitle="Non_china, Non_USA cumulative number progression") 

# Cases in india
india_cases <- countries_cases %>%
  filter(country %in% c('India')) %>% 
  select(-country)

india_cases %>% arrange(desc(date)) %>% head(50)

india_cases %>% group_by(type) %>% top_n(1) %>% rename(total_cases = cum_cases) %>%
  ggplot(aes(type, total_cases, fill=type)) +
  geom_col() +
  geom_text(aes(label = total_cases), vjust = -0.5) +
  labs(subtitle=" Total cases in India")

india_cases %>%
  ggplot(aes(date, cum_cases, col=type)) +
  geom_line(size=1) +
  facet_grid(~ type) +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" Cumulative case progression in India")

# Total daily cases across the globe
tot_d_india_cases <- covid_ds %>% 
  filter(country=='India') %>%
  group_by(date, type) %>% mutate (total_daily_cases = sum(cases)) %>%
  arrange(desc(total_daily_cases))%>% 
  select(date, type, total_daily_cases) 

tot_d_india_cases %>% ggplot(aes(date,total_daily_cases, col=type)) +
  geom_point()+
  geom_line() +
  facet_wrap(~type)+
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" Total daily cases in India")

# Cases in Oman & UAE
oman_vs_uae <- countries_cases %>%
  filter(country %in% c('Oman', 'United Arab Emirates')) 

oman_vs_uae %>% group_by(country,type) %>% top_n(1) %>% rename(total_cases = cum_cases) %>%
  ggplot(aes(country, total_cases, fill=type)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(label = total_cases),position = position_dodge(1), vjust = -0.5) +
  labs(subtitle=" Total cases in Oman & UAE")

oman_vs_uae %>% ggplot(aes(date, cum_cases, col=country)) +
  geom_line(size=1) +
  facet_grid(~type)  +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" Oman vs UAE cumulative progression") 

# Total daily cases across the globe
oman_vs_uae_daily <- covid_ds %>% 
  filter(country=='Oman' | country == "United Arab Emirates") %>%
  group_by(date,country, type) %>% mutate (total_daily_cases = sum(cases)) %>%
  arrange(desc(total_daily_cases))%>% 
  select(date, country, type, total_daily_cases) 

oman_vs_uae_daily %>% ggplot(aes(date,total_daily_cases, col=country)) +
  geom_point()+
  geom_line() +
  facet_wrap(~type)+
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "top") +
  labs(subtitle=" Total daily cases in Oman & UAE")

percents <- covid_ds %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarize(tot_cases= sum(cases)) %>%
  spread(type, tot_cases) %>%
  summarize(recovery_percent= 100*sum(recovered)/sum(confirmed),
            dead_percent= 100*sum(dead)/sum(confirmed),
            dead_to_recovery= 100*sum(dead)/sum(recovered)) %>%
  gather(type, percentage, recovery_percent:dead_to_recovery)

percents %>%
  filter(country %in% select_countries) %>%
  ggplot(aes(country, percentage, fill=country)) +
  geom_col()+
  facet_grid(type ~.)+
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "none") +
  labs(subtitle="% of dead, recoveries and dead to recoveries")

percents %>%
  filter(country %in% select_countries & country != 'United Kingdom' & country != 'Netherlands') %>%
  ggplot(aes(country, percentage, fill=country)) +
  geom_col()+
  facet_grid(type ~.)+
  theme(axis.text.x = element_text(
    angle = 90,
    size = 8,
    hjust = 1
  ),
  legend.position = "none") +
  labs(subtitle="% of dead, recoveries and dead to recoveries")
