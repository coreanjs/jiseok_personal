
library(readxl)

getwd()


### dac _ emission by sector
data_ssp2_sector<-read_excel(paste0(getwd(), "/KAIST_IAM_GROUP/dac_emission_by_sector.xlsx"))


data_ssp2_sector


library(tidyverse)

data_ssp2_sector_longer<-data_ssp2_sector %>% 
    pivot_longer(-c('scenario', 'region','Units', 'sector'), names_to= "year", values_to="MTC") %>% 
    mutate(year = as.numeric(year))

str(data_ssp2_sector_longer)


library(scales)
options(scipen = 999)
data_ssp2_sector_longer%>% 
    ggplot(aes(x = year, y = MTC, group = scenario, color = scenario))+
    geom_line()+
    scale_y_continuous(labels = comma)+
    scale_x_continuous(breaks = c( 2020, 2050, 2100))+
    facet_wrap(~sector, scale="free_y", ncol = 4)+
    theme(legend.position ="bottom")+
    labs(title ="CO2 emission by tech (dac_ssp2 vs dap_ssp2_x3)")

ggsave(filename="dac_co2_emission_by_sector.png", path = paste0(getwd(), "/KAIST_IAM_GROUP/"), 
       dpi=100, width = 1200, height = 1400, units ="px")






### dac _ seq by sector
data_ssp2_seq_sector<-read_excel(paste0(getwd(), "/KAIST_IAM_GROUP/dac_sequestration_by_sector.xlsx"))


data_ssp2_seq_sector
data_ssp2_seq_sector

library(tidyverse)

data_ssp2_sector_seq_longer<-data_ssp2_seq_sector %>% 
    pivot_longer(-c('scenario', 'region','Units', 'sector'), names_to= "year", values_to="MTC") %>% 
    mutate(year = as.numeric(year))

str(data_ssp2_sector_seq_longer)


library(scales)
options(scipen = 999)
data_ssp2_sector_seq_longer%>% 
    ggplot(aes(x = year, y = MTC, group = scenario, color = scenario))+
    geom_line()+
    scale_y_continuous(labels = comma)+
    scale_x_continuous(breaks = c( 2020, 2050, 2100))+
    facet_wrap(~sector, scale="free_y", ncol = 4)+
    theme(legend.position ="bottom")+
    labs(title ="CO2 emission by tech (dac_ssp2 vs dap_ssp2_x3)")

ggsave(filename="dac_co2_sequestration_by_sector.png", path = paste0(getwd(), "/KAIST_IAM_GROUP/"), 
       dpi=100, width = 1200, height = 1400, units ="px")








######## 여기 미완성!!!!!!!!!!!!!!!!!!



### dac _ emission by tech
data_ssp2_tech<-read_excel(paste0(getwd(), "/KAIST_IAM_GROUP/dac_emission_by_tech.xlsx"))

data_ssp2_tech




data_ssp2_tech_longer<-data_ssp2_tech %>% 
    pivot_longer(-c('scenario', 'region','Units', 'sector', 'subsector', 'technology'), names_to= "year", values_to="MTC") %>% 
    mutate(year = as.numeric(year))


data_ssp2_tech_longer


library(scales)
options(scipen = 999)
data_ssp2_tech_longer %>% 
    ggplot(aes(x = year, y = MTC, group = scenario, color = scenario))+
    geom_line()+
    scale_y_continuous(labels = comma)+
    scale_x_continuous(breaks = c( 2020, 2050, 2100))+
    facet_wrap(~sector, scale="free_y", ncol = 4)+
    theme(legend.position ="bottom")+
    labs(title ="CO2 emission by tech (dac_ssp2 vs dap_ssp2_x3)")

ggsave(filename="dac_co2_emission_by_tech.png", path = paste0(getwd(), "/KAIST_IAM_GROUP/"), 
       dpi=100, width = 1200, height = 1400, units ="px")
 