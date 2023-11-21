library(tidyverse)

options(scipen =999)


getwd()


setwd("C:/R/Rproject/jiseok_personal")

library(readxl)
AR6_meta<- read_excel('./AR6/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx', sheet="meta")



unique(AR6_meta$Category_name)

unique(AR6_meta$Regional_scope)




AR6<- read.csv('./AR6/AR6_Scenarios_Database_World_v1.1.csv') %>% 
    select(-Region) %>%  # Region은 모두 World라 지워도 됌.
    pivot_longer(-c('Model', 'Scenario', 'Variable', 'Unit'), names_to ="year", values_to="value") %>% 
    mutate(year = as.numeric(str_sub(year, 2, 5))) %>% 
    drop_na(value)


#AR6_R10<- read.csv('./AR6/AR6_Scenarios_Database_R10_regions_v1.1.csv') %>% 
#    pivot_longer(-c('Model', 'Scenario', 'Variable', 'Unit', 'Region'), names_to ="year", values_to="value") %>% 
#    mutate(year = as.numeric(str_sub(year, 2, 5))) %>% 
#    drop_na(value) %>% as_tibble()

head(AR6_R10)


str(AR6_R10)
unique(AR6_R10$Region)


str(AR6)

unique(AR6$Model)

unique(AR6$Scenario)

unique(AR6$Variable)




seq(2020, 2100, 5)

AR6 %>% 
    filter(Variable =="Emissions|CO2|Energy and Industrial Processes") %>% 
    distinct(Model)

AR6 %>% 
    filter(Variable =="Emissions|CO2|Energy and Industrial Processes") %>% 
    ggplot(aes(x = year, y = value, group = Scenario))+
    geom_line(alpha = .3)+
    facet_wrap(~Model)
    

#Emissions|NOx


AR6 %>% 
    filter(Variable =="Emissions|NOx") %>% 
    ggplot(aes(x = year, y = MTCO2, group = Scenario))+
    geom_line(alpha = .3)+
    facet_wrap(~Model)





AR6 %>% 
    filter(Variable =="Emissions|NOx") %>% 
    #distinct(Unit)
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model)))+  ## interaction으로 two variable을 group
    geom_line(alpha = .1)+
    labs(y = "Mt NO2/yr")



unique(AR6$Variable)



##Carbon Sequestration|CCS|Fossil|Industrial Processes

AR6 %>% 
    filter(Variable =="Carbon Sequestration|CCS|Fossil|Industrial Processes") %>% 
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model)))+  ## interaction으로 two variable을 group
    geom_line(alpha = .1)





###
AR6 %>% 
    filter(Variable =="Carbon Sequestration|CCS") %>% 
    distinct(Unit) %>%  pull(Unit) -> Unit
AR6 %>% 
    filter(Variable =="Carbon Sequestration|CCS") %>% 
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model)))+  ## interaction으로 two variable을 group
    geom_line(alpha = .1)+
    labs(y = Unit)



AR6 %>% 
    filter(Variable =="Emissions|F-Gases") %>% 
   
     ggplot(aes(x = year, y = value, group = interaction(Scenario, Model)))+  ## interaction으로 two variable을 group
    geom_line(alpha = .1)




library(gghighlight)

AR6 %>% 
    filter(Variable =="Emissions|NOx" & Model =="GCAM 5.3") %>% 
    ggplot(aes(x = year, y = value, group = Scenario, color = Scenario))+
    geom_line()+
    gghighlight(Scenario %in% c('SSP_SSP1', 'SSP_SSP2', 'SSP_SSP3', 'SSP_SSP4', 'SSP_SSP5'))


#Carbon Sequestration|Land Use

AR6 %>% 
    filter(Variable =="Emissions|CO2|Energy and Industrial Processes" & Model =="GCAM 5.3") %>% 
    ggplot(aes(x = year, y = value, group = Scenario))+
    geom_line(alpha = .3)



unique(AR6$Variable)

AR6 %>% 
    filter(Variable =="Price|Carbon") %>% 
    ggplot(aes(x = year, y = value,  group = interaction(Scenario, Model)))+
    geom_line(alpha = .3)

    

unique(AR6$Model)


unique(AR6$Variable)


AR6 %>% 
    filter(Variable =="AR6 climate diagnostics|Effective Radiative Forcing|FaIRv1.6.2|5.0th Percentile") %>% 
    ggplot(aes(x = year, y = value, group = Scenario))+
    geom_line(linewidth = .3)+
    facet_wrap(~Model)





AR6 %>% 
    filter(Variable =="Final Energy|Industry|Heat") %>% 
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model)))+
    geom_line(alpha =.2)




#Carbon Sequestration|Direct Air Capture

AR6 %>% 
    filter(Variable =="Carbon Sequestration|Direct Air Capture") %>% 
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model)))+
    geom_line(alpha =.3)



AR6 %>% 
    filter(Variable %in% c("Carbon Sequestration|Direct Air Capture",
                           "Carbon Sequestration|CCS|Biomass",
                           "Carbon Sequestration|Land Use|Afforestation"
                           )) %>% 
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model, Variable), color = Variable))+
    geom_line(alpha =.2)



str(AR6)
unique(AR6$Scenario)
unique(AR6$Model)

AR6 %>% 
    filter(Variable %in% c("Carbon Sequestration|Direct Air Capture",
                           "Carbon Sequestration|CCS|Biomass",
                           "Carbon Sequestration|Land Use|Afforestation"
    )) %>% 
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model, Variable), color = Variable))+
    geom_line(alpha =.2)+
    facet_wrap(~Variable)



AR6 %>% 
    filter(Variable %in% c("Carbon Sequestration|Direct Air Capture",
                           "Carbon Sequestration|CCS|Biomass",
                           "Carbon Sequestration|Land Use|Afforestation"
    )) %>% 
    ggplot(aes(x = year, y = value, group = year, color = Variable))+
    geom_boxplot(outlier.size=.3, width = 3)+
    facet_wrap(~Variable)+
    theme(legend.position="none")


AR6 %>% 
    filter(Scenario %in% c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) %>% distinct(Model)


## SSP 시나리오는 별로 볼 게 없네..

AR6 %>% 
    filter(Scenario %in% c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) %>% distinct(Variable) %>% View() 
    


    
    unique(AR6$Variable)

AR6 %>% 
    filter(str_detect(Variable, "Carbon Sequestration")) %>% 
    distinct(Variable) %>% View()



AR6 %>% 
    filter(Variable =="GDP|MER") %>% 
    #distinct(Unit)
    ggplot(aes(x = year, y = value, group = interaction(Scenario, Model)))+
    geom_line(alpha =.3)+
    labs(y = "billion US$2010/yr")









####
 