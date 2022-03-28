#setting working directory
setwd('D:Tetris/Capstone/Data')

#import the used library
library(readxl)  
library(readr)
library(tidyverse)

#import data
basic_price <- read_excel('./Netflix_Price_Data.xlsx', sheet='Basic')%>% 
  select(1,5) %>% 
  rename(price_basic = Price_USD)

standard_price <- read_excel('./Netflix_Price_Data.xlsx', sheet='Standard')%>% 
  select(1,5) %>% 
  rename(price_standard = Price_USD)

premium_price <- read_excel('./Netflix_Price_Data.xlsx', sheet='Premium') %>% 
  select(1,5) %>% 
  rename(price_premium = Price_USD)

#checking data str
str(basic_price)
str(standard_price)
str(premium_price)

join_price <- basic_price %>% 
  full_join(standard_price, by= 'Country') %>% 
  full_join(premium_price, by= 'Country') %>% 
  filter(!is.na(Country) & Country != 'Average' & Country != 'Minimum' & Country != 'Maximum') %>% 
  mutate(total_price = price_basic+price_standard+price_premium) %>% 
  filter(total_price < quantile(total_price,0.05) | 
           total_price > quantile(total_price, 0.95) | 
           Country == 'Indonesia' )

income_data <- read_csv('./Income_All_over.csv') %>% 
  select(2,4) %>% 
  rename('income' = `Avg. income per month`) %>% 
  filter(Country %in% join_price$Country)

user_data <- read_csv('./data_user.csv') %>% 
  select(1,2,5,8,10) %>% 
  rename('Q1 2021'=`# of Subscribers Q1 2021`,
         'Q2 2021'=`# of Subscribers Q2 2021`,
         'Q3 2021'=`# of Subscribers Q3 2021 (Estimate)`,
         'Q4 2021'=`# of Subscribers Q4 2021 (Estimate)`) %>% 
  filter(Country %in% join_price$Country)

price_income_relation <- join_price %>% 
  select(1,2,3,4) %>% 
  right_join(income_data, by= 'Country') %>% 
  mutate(basic_income= price_basic / income*100) %>% 
  mutate(standard_income= price_standard/income*100) %>% 
  mutate(premium_income= price_premium/income*100) 

user_data_pivot <- user_data %>% 
  pivot_longer(cols=c(`Q1 2021`,`Q2 2021`,`Q3 2021`,`Q4 2021`),
               names_to='Kuarter', values_to='Jumlah') 

join_price_pivot <- join_price %>%
  select(1,2,3,4) %>% 
  filter(Country != 'Liechtenstein') %>% 
  rename('Basic'= price_basic,
         'Standard' = price_standard,
         'Premium' = price_premium) %>% 
  pivot_longer(cols=c(Basic,Standard,Premium),
               names_to='Tipe', values_to='Biaya')

price_income_relation_pivot <- price_income_relation %>% 
  select(1,5,6,7,8) %>% 
  rename('Basic'= basic_income,
         'Standard' = standard_income,
         'Premium' = premium_income) %>% 
  pivot_longer(cols=c(Basic,Standard,Premium), names_to='Tipe', values_to='Relasi')


#plotting
ggplot(data = join_price_pivot,aes(x=Tipe, y=Biaya, color=Country,fill=Country))+
  geom_bar(stat = 'identity')+
  ggtitle('Biaya langganan per Negara')+
  labs(x='Tipe Langganan',y='Biaya($)')+
  facet_wrap(~Country)+
  theme_minimal()

ggplot(data=user_data_pivot,aes(x=Kuarter, y=Jumlah,colour=Country,group=Country))+
  geom_point(size = 3, na.rm = TRUE) +
  geom_line()+
  ggtitle('Pertambahan Pelanggan perNegara')+
  labs(x='Quarter',y='Jumlah Pelanggan')+
  theme_minimal()

ggplot(data=income_data,aes(x=Country, y=income,colour=Country))+
  geom_point(size=3, alpha=0.6)+
  ggtitle('Pendapatan per Bulan setiap Negara')+
  labs(x='Negara', y='Pendapatan($)')+
  geom_text(aes(label=income),hjust = 0, nudge_x = 0.1)+
  theme_minimal()

price_income_relation_pivot %>% 
  filter(income < 4000) %>% 
  ggplot(aes(x=Tipe,y=income,colour=Country,group=Country))+
  geom_point(aes(size=Relasi), alpha=0.5)+
  ggtitle('Persentase Biaya Langganan Relatif terhadap Pendapatan')+
  labs(x='Tipe', y='Pendapatan')+
  theme_minimal()