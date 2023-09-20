

library(tidyverse)


library(readxl)



getwd()
demand<-read.csv("./ITM812/demand.csv") %>% 
    rename(date = 날짜) %>% 
    pivot_longer(-date, names_to="hour", values_to ="MWh") %>% 
    mutate(hour= gsub('X', '', hour),
           hour= gsub('시', '', hour),
           hour = as.numeric(hour)) %>% 
    rename(전력수요 = MWh)


str(demand)



trade<-read.csv("./ITM812/trade.csv") %>% 
    rename(date = 거래일, fuel = 연료원, hour = 거래시간, trade = 전력거래량) %>% 
    select(date, hour, fuel, trade) %>% 
    as.tibble() %>% 
    pivot_wider(names_from="fuel", values_from="trade") 



trade %>% 
    mutate(year = year(date)) %>% 
    relocate(year) %>% 
    distinct(year)

str(trade)


demand_trade<- inner_join(demand, trade, by = c('date', 'hour'))


demand_trade


## 인코딩 cp949
demand_trade %>% 
    select(-date, -hour) %>% 
    write.csv('demand_trade.csv', fileEncoding ='cp949', row.names=FALSE)


colnames(demand_trade)
