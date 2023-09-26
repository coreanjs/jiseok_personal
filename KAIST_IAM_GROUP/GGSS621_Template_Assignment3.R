library(gcamextractor)
library(rchart)
library(rmap)
library(dplyr)
library(ggplot2)
library(gghighlight)
library(scales)
library(ggthemes)
options(scipen =999)



#### GCAM-KAIST2 


getwd()


setwd("C:/R/Rproject/jiseok_personal/GGS621")



### Import NZ2050
GGS621_cap_tax <- gcamextractor::readgcam(gcamdatabase = "E:/gcam-v7.0-Windows-Release-Package_GGS621/output/Assignment0927",
                                    paramsSelect = c(#"energy" ## energy 오류 남
                                                     "electricity", 
                                                     "transport", 
                                                     "building", 
                                                     "emissions",
                                                     "hydrogen",
                                                     "energyPrimaryByFuelEJ",
                                                     "energyPrimaryRefLiqProdEJ",
                                                     "energyFinalConsumBySecEJ",
                                                     "energyFinalByFuelEJ",
                                                     "energyFinalSubsecByFuelTranspEJ", 
                                                     "energyFinalSubsecByFuelBuildEJ",
                                                     "energyFinalSubsecByFuelIndusEJ",
                                                     "energyFinalSubsecBySectorBuildEJ",
                                                     "energyPrimaryByFuelMTOE",
                                                     "energyPrimaryRefLiqProdMTOE",
                                                     "energyFinalConsumBySecMTOE",
                                                     "energyFinalbyFuelMTOE",
                                                     "energyFinalSubsecByFuelTranspMTOE", 
                                                     "energyFinalSubsecByFuelBuildMTOE", 
                                                     "energyFinalSubsecByFuelIndusMTOE",
                                                     "energyFinalSubsecBySectorBuildMTOE", 
                                                     "energyPrimaryByFuelTWh",
                                                     "energyPrimaryRefLiqProdTWh", 
                                                     "energyFinalConsumBySecTWh",
                                                     "energyFinalbyFuelTWh",
                                                     "energyFinalSubsecByFuelTranspTWh",
                                                     "energyFinalSubsecByFuelBuildTWh",
                                                     "energyFinalSubsecByFuelIndusTWh",
                                                     "energyFinalSubsecBySectorBuildTWh"
                                                     ), ### this is a key
                                    regionsSelect = c("South Korea"),
                                    folder = "gcamextractor/GGS621_cap_tax")




 

GGS621_cap_tax

??gcamextractor

??readgcam

gcamextractor::params # view available parameters
gcamextractor::queries # Get all queries used
gcamextractor::map_param_query # Get a table of params and the relevants queries used to extract and calculate them.

gcamextractor::map_param_query %>% View()

GGS621_cap_tax
### list 파일 만지고 놀기 R리스트 완전정복(슬기로운 통계생활)
class(GGS621_cap_tax)

# View your data

library(RColorBrewer)
### Elec gen



NZ2050#

## Merging two scenarios into a dataframe \
## param : NA -> 'CO2 sequestration by sector' 수정 

GGS621_cap_tax

### CO2 sequestration by sector --> NA
GGS_621_data<- GGS621_cap_tax$dataAll %>% 
    mutate(param = ifelse(is.na(param),origQuery, param),
           scenario = factor(scenario, levels = c('Reference', 'Decarbonization')))



str(GGS_621_data)

##What's in parameter
unique(GGS_621_data$param)


unique(GGS_621_data$scenario)



## 전 부문의 기술별 전력생산



unique(GGS_621_data$param)

unique(GGS_621_data$param)[2] -> param_selected

param_selected

GGS_621_data %>%
    filter(param == param_selected) %>% 
    distinct(units)->unit_param_selected

unit_param_selected

GGS_621_data %>%
    filter(param == param_selected) %>% 
    distinct(origQuery) ->origQuery

origQuery



GGS_621_data %>%
    filter(param == param_selected & x >= 2015) %>%
    group_by(x, class1, scenario, origQuery) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = class1, fill = class1))+
    geom_bar(stat='identity')+
    scale_fill_brewer(palette="Set1")+
   scale_x_continuous(limits = c(2010, 2055), breaks = c(2015, 2050))+
    scale_y_continuous(labels = comma, limits= c(0, 1200), breaks = seq(0, 1200, 200))+
  #  geom_text(aes(label = round(after_stat(y), 0), group = x), 
  #            stat = 'summary', fun = sum, vjust = -1)+
    facet_wrap(~scenario)+
    theme_bw()+
    theme_minimal()+
    theme(plot.title = element_text(size = 16),
          strip.text.x  = element_text(size=12),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title.position =  "plot")+
    labs(title = paste("Parameter:", unit_param_selected, "\nOriginQuery:", origQuery),
         fill ="구분",
         x = "연도",
         y = unit_param_selected)


ggsave(file =paste0("param_", param_selected, ".png"),  width =1200, height = 700, units ="px", dpi = 150)


    
 
 
 




### CO2 sequestration은 데이터 구성이 특이해서 group을 sector로 해야 함
 
 
 unique(GGS_621_data$param)
 
 unique(GGS_621_data$param)[3] -> param_selected
 
 param_selected
 
 GGS_621_data %>%
     filter(param == param_selected) %>% 
     distinct(units)->unit_param_selected
 
 unit_param_selected
 
 GGS_621_data %>%
     filter(param == param_selected) %>% 
     distinct(origQuery) ->origQuery
 
 origQuery
 
 
 GGS_621_data %>%
     filter(param == param_selected & x >= 2015) %>%
     #group_by(x, class1, scenario, origQuery) %>% 
     #summarise(value = sum(value)) %>% 
     ggplot(aes(x = x, y = value, 
                group = sector, fill = sector
                ))+
     geom_bar(stat='identity')+
    # scale_fill_brewer(palette="Set1")+
     scale_x_continuous(limits = c(2010, 2055), breaks = seq(2015, 2050, 5))+
   #  scale_y_continuous(labels = comma, limits= c(0, 1200), breaks = seq(0, 1200, 200))+
     geom_text(aes(label = round(after_stat(y), 0), group = x), 
               stat = 'summary', fun = sum, vjust = -1)+
     facet_wrap(~scenario)+
     theme_bw()+
     theme_minimal()+
     theme(plot.title = element_text(size = 16),
           strip.text.x  = element_text(size=12),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x = element_blank(),
           # panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           plot.background = element_rect(fill = "white"),
           plot.title.position =  "plot")+
     labs(title = paste("Parameter:", unit_param_selected, "\nOriginQuery:", origQuery),
          fill ="구분",
          x = "연도",
          y = unit_param_selected)
 
 
 
 

 
 
 
 
 ####### Image create using function
dir.create("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img")

 is.na(unique(GGS_621_data$param))
 
 
 
 unique(GGS_621_data$param)[-15] ->param_all

 param_all
 
 for (i in param_all) {
     
     GGS_621_data %>%
         filter(param == i) %>% 
         distinct(units)->unit_param_selected
     
     unit_param_selected
     
     GGS_621_data %>%
         filter(param == i) %>% 
         distinct(origQuery) ->origQuery
     
     
     graph = GGS_621_data %>%
         filter(param == i & x >= 2015) %>%
         group_by(x, class1, scenario, origQuery) %>% 
         summarise(value = sum(value)) %>% 
         ggplot(aes(x = x, y = value, group = class1, fill = class1))+
         geom_bar(stat='identity')+
        # scale_fill_brewer(palette="Set1")+
         scale_x_continuous(limits = c(2010, 2055), breaks = c(2015, 2050))+
        # scale_y_continuous(labels = comma, limits= c(0, 1200), breaks = seq(0, 1200, 200))+
        # geom_text(aes(label = round(after_stat(y), 0), group = x), 
        #           stat = 'summary', fun = sum, vjust = -1)+
         facet_wrap(~scenario)+
         theme_bw()+
         theme_minimal()+
         theme(plot.title = element_text(size = 16),
               strip.text.x  = element_text(size=12),
               panel.grid.minor.x = element_blank(),
               panel.grid.major.x = element_blank(),
               # panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(),
               plot.background = element_rect(fill = "white"),
               plot.title.position =  "plot")+
         labs(title = paste("Parameter:", i, "\nOriginQuery:", origQuery),
              fill ="구분",
              x = "연도",
              y = unit_param_selected)
     
     setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/Assignment3")
     ggsave(plot =graph, file =paste0(i, ".png"),  width =1200, height = 700, units ="px", dpi = 150)  
     
 }

 
 
 
 
 
 
 
 
 
 ### parameter that has more than two original query -> emissGHGBySectorGWPAR5
 
 unique(GGS_621_data$param)
 
 
 unique(GGS_621_data$param)[17] -> param_selected
 
 param_selected
 
 GGS_621_data %>%
     filter(param == param_selected) %>% 
     distinct(units)->unit_param_selected
 
 unit_param_selected
 
 
 ## origQuery 4개로 구분 
 
 GGS_621_data %>%
     filter(param == param_selected) %>% 
     distinct(origQuery) ->origQuery
 
 origQuery
 
 ## unit은 GHG emissions
 GGS_621_data %>%
     filter(param == param_selected) %>% 
     distinct(units)
 
 
 ### value(CO2eq) = origValue * coefficient
 ## coefficient 확인 가능
 GGS_621_data %>%
     filter(param == param_selected) %>%
     mutate(coefficient = value/origValue) %>% 
     relocate(coefficient)

 
 ## origQuery
 
 GGS_621_data %>%
     filter(param == param_selected & x >= 2015) %>%
     group_by(x, class1, scenario, 
              #origQuery
              ) %>% 
     summarise(value = sum(value)) %>% 
     ungroup() %>% 
     group_by(x,scenario) %>% 
     mutate(net = sum(value)) %>% 
     ggplot(aes(x = x, y = value, group = class1, fill = class1))+
     geom_bar(stat='identity')+
   #  scale_fill_brewer(palette="Set1")+
     scale_x_continuous(limits = c(2010, 2055), breaks = seq(2015, 2050, 5))+
  #   scale_y_continuous(labels = comma, limits= c(0, 1200), breaks = seq(0, 1200, 200))+
     geom_text(aes(label = round(after_stat(y), 0), group = x), 
               stat = 'summary', fun = sum, vjust = -1)+
     facet_wrap(~scenario)+
     theme_bw()+
     theme_minimal()+
     theme(plot.title = element_text(size = 16),
           strip.text.x  = element_text(size=12),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x = element_blank(),
           # panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           plot.background = element_rect(fill = "white"),
           plot.title.position =  "plot")+
     labs(title = paste("Parameter:", unit_param_selected, "\nOriginQuery:", origQuery),
          fill ="구분",
          x = "연도",
          y = unit_param_selected)
 
 
 ggsave(file =paste0("param_", param_selected, ".png"),  width =1200, height = 700, units ="px", dpi = 150)
 
 
 
 
 
 
 
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 #############################
 
 
 getwd()
 
 setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/GGS621_cap_tax")
 
 
 
 
 #### aggParam.csv
 aggParam<-read.csv('gcamDataTable_aggParam.csv') 
 
 
 unique(aggParam$param)
 
 aggParam %>% 
     filter(region =="Global" & param == "emissCO2BySector") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     gghighlight()
 
 
 
 aggParam %>% 
     filter(region =="Global" & param == "emissCO2BySector") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     gghighlight()
 
 
 
 
 
 
 
 #### Extended.csv
 Extended<-read.csv('gcamDataTable_Extended.csv') 
 
 
 unique(Extended$param)
 
 Extended %>% 
     filter(region =="Global" & param == "emissCO2BySector") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     gghighlight()
 
 
 
 Extended %>% 
     filter(region =="Global" & param == "emissCO2BySector") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     gghighlight()
 
 
 
 
 
 
 #### aggClass1.csv
 
 setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/GGS621_cap_tax")
 
 aggParam<-read.csv('gcamDataTable_aggClass1.csv') %>% 
     filter(region == "South Korea")
 
 aggParam %>% 
     filter(is.na(param))
 
 
 unique(aggParam$region)
 
 unique(aggParam$param)
 

 
  
 ###  elec_coal에서 CO2 발생량이 큰 차이가 나네

  aggParam %>%
     filter(param == "emissCO2BySector") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(~class)
 
 
 unique(aggParam$param) ->aggParam_all
 
 aggParam_all
 
 
 i <-aggParam_all[1]

 i 
 
 aggParam %>%
     filter(param == i) %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(~class)
 
 

 
 
 ####### Image create using function
 dir.create("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_aggParam_Assignment3")
 
 is.na(unique(aggParam$param))
 
 
 
 
 
 str(aggParam)
 
 
 
 unique(aggParam$param) ->aggParam_all
 
 aggParam_all
 
 i <-aggParam_all
 
 i
 for (i in aggParam_all) {
     
     aggParam %>%
         filter(param == i) %>% 
         distinct(units)->unit_param_selected
     
     unit_param_selected
     
     
     graph =  aggParam %>%
         filter(param == i) %>% 
         ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
         geom_line(linewidth = 1.5)+
         facet_wrap(~class)+
         scale_x_continuous(limits = c(2010,2100), breaks = c(2015, 2050,2100))+
         scale_color_manual(values = c("#1f5c99", "#d83d36"))+
         theme_bw()+
         geom_vline(xintercept = 2050, linetype = "dotted")+
        #theme_minimal()+
         theme(plot.title = element_text(size = 20, face ="bold"),
               strip.text.x  = element_text(size=18),
               axis.text.x = element_text(size =14),
               axis.text.y = element_text(size =14),
               axis.title = element_text(size =16),
               panel.grid.minor.x = element_blank(),
               panel.grid.major.x = element_blank(),
               # panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(),
               plot.background = element_rect(fill = "white"),
               plot.title.position =  "plot",
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16),
               plot.subtitle = element_text(size = 16))+
         labs(title = paste("Parameter:", i),
              x = "year",
              y = unit_param_selected)
     
     setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_aggParam_Assignment3")
     ggsave(plot =graph, file =paste0(i, ".png"),  width =1800, height = 800, units ="px", dpi = 150)  
     
 }
 
 
 
 
 
 
 
 
 aggParam %>%
     filter(param == "emissGHGBySectorGWPAR5") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line(linewidth = 1.2)+
     facet_wrap(~class)+
     scale_x_continuous(limits = c(2010,2100), breaks = c(2015, 2050,2100))+
     scale_color_manual(values = c("#1f5c99", "#d83d36"))+
     theme_bw()+
     geom_vline(xintercept = 2050, linetype = "dotted")+
     #theme_minimal()+
     theme(plot.title = element_text(size = 20, face ="bold"),
           strip.text.x  = element_text(size=14),
           axis.text.x = element_text(size =12),
           axis.text.y = element_text(size =16),
           axis.title = element_text(size =16),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x = element_blank(),
           # panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           plot.background = element_rect(fill = "white"),
           plot.title.position =  "plot",
           legend.text = element_text(size = 16),
           legend.title = element_text(size = 16),
           plot.subtitle = element_text(size = 16))+
     labs(title = paste("Parameter:", "emissGHGBySectorGWPAR5"),
          x = "year",
          y = "GHG emissions(MTCO2eq)")
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 
 
 
 ######### energy 일부부
 #### aggClass1.csv
 
 setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/GGS621_cap_tax_test")
 
 aggParam<-read.csv('gcamDataTable_aggClass1.csv') %>% 
     filter(region == "South Korea")
 
 aggParam %>% 
     filter(is.na(param))
 
 
 unique(aggParam$region)
 
 unique(aggParam$param)
 
 
 
 
 
 
 ####### Image create using function
 dir.create("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_aggParam")
 
 is.na(unique(aggParam$param))
 
 
 
 
 
 str(aggParam)
 
 unique(aggParam$param) ->aggParam_all
 
 aggParam_all
 
 i <-aggParam_all[1]
 
 
 i
 for (i in aggParam_all) {
     
     aggParam %>%
         filter(param == i) %>% 
         distinct(units)->unit_param_selected
     
     unit_param_selected
     
     
     graph =  aggParam %>%
         filter(param == i) %>% 
         ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
         geom_line()+
         facet_wrap(~class)+
         scale_x_continuous(limits = c(2010, 2055), breaks = c(2015, 2050))+
         theme_bw()+
         theme_minimal()+
         theme(plot.title = element_text(size = 20, face ="bold"),
               strip.text.x  = element_text(size=12),
               panel.grid.minor.x = element_blank(),
               panel.grid.major.x = element_blank(),
               # panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(),
               plot.background = element_rect(fill = "white"),
               plot.title.position =  "plot")+
         labs(title = unit_param_selected,
              subtitle = paste("Parameter:", i),
              x = "year",
              y = unit_param_selected)
     
     setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_aggParam")
     ggsave(plot =graph, file =paste0(i, ".png"),  width =1200, height = 700, units ="px", dpi = 150)  
     
 }
 
 
 
 
 
 
total_final_energy <-data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
          scenario = c("Decarbonization", "Reference"),
            `1990` = c(3.279279782, 3.279279782),
            `2005` = c(6.929202893, 6.929202893),
            `2010` = c(7.80470756, 7.80470756),
            `2015` = c(8.655149426, 8.655149426),
            `2020` = c(8.907405333, 8.907405333),
            `2025` = c(9.246042203, 9.053733383),
            `2030` = c(9.461572644, 9.330508359),
            `2035` = c(9.522384587, 9.402758768),
            `2040` = c(9.453087547, 9.357385019),
            `2045` = c(9.322865369, 9.22499561),
            `2050` = c(9.074813488, 9.025688666),
            `2055` = c(8.757253928, 8.769109901),
            `2060` = c(8.280716309, 8.460735619),
            `2065` = c(7.887725258, 8.127873817),
            `2070` = c(7.492689793, 7.790472261),
            `2075` = c(7.13839849, 7.462691223),
            `2080` = c(6.808850659, 7.175058187),
            `2085` = c(6.497077989, 6.899124992),
            `2090` = c(6.197903885, 6.639073591),
            `2095` = c(5.909714511, 6.389948054),
            `2100` = c(5.632034763, 6.145750802)
)%>% pivot_longer(-scenario, values_to = "EJ", names_to = "year") %>% 
    mutate(year = as.numeric(year))
 
library(tidyverse)
 
total_final_energy %>% 
    ggplot(aes( x = year, y = EJ, group = scenario, color = scenario))+
    geom_line()+
    theme_bw()+
    geom_vline(xintercept = 2050, linetype = "dotted")+
    #theme_minimal()+
    theme(plot.title = element_text(size = 20, face ="bold"),
          strip.text.x  = element_text(size=14),
          axis.text.x = element_text(size =12),
          axis.text.y = element_text(size =16),
          axis.title = element_text(size =16),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title.position =  "plot",
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          plot.subtitle = element_text(size = 16))+
    labs(title = paste("Parameter:", "Total final energy by region"),
         x = "year",
         y = "EJ")



 
 
 
 
 
 
 setwd("C:/R/Rproject/jiseok_personal/GGS621")
 
 
decarb_two_regions <- gcamextractor::readgcam(gcamdatabase = "E:/gcam-v7.0-Windows-Release-Package_GGS621/output/database_basexdb",
                                           paramsSelect = c(
                                               "emissions" 
                                           ), ### this is a key
                                           regionsSelect = c("South Korea", "Japan"),
                                           folder = "gcamextractor/GGS621_cap_tax_two_regions")
 
 
 
unique(decarb_two_regions$dataAggClass1$region)

unique(decarb_two_regions$dataAll$param) 




decarb_two_regions$dataAll %>% 
    filter(region %in% c('South Korea', 'Japan') & param == "emissCO2BySectorNoBio") %>% 
    distinct(units)

decarb_two_regions$dataAll %>% 
    filter(region %in% c('South Korea', 'Japan') & param == "emissCO2BySectorNoBio") %>%
    ggplot(aes(x = x, y = value, group = region, color = region))+
    geom_line()+
    facet_grid(scenario~class1)+
    theme_bw()+
    
    #theme_minimal()+
    theme(plot.title = element_text(size = 20, face ="bold"),
          strip.text.x  = element_text(size=12),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title.position =  "plot")+
    labs(title = "CO2 emissions - (MTCO2)",
         subtitle = paste("Parameter:", "emissCO2BySectorNoBio"),
         x = "year",
         y = " CO2 emissions - (MTCO2)")
 

aggParam<- decarb_two_regions$dataAll
 
 
unique(aggParam$param)[-10] ->aggParam_all


aggParam_all


for (i in aggParam_all) {
    
    aggParam %>%
        filter(param == i) %>% 
        distinct(units)->unit_param_selected
    
    unit_param_selected
    
    
    graph =  aggParam %>%
        filter(region %in% c('South Korea', 'Japan') & param == "emissCO2BySectorNoBio") %>%
        ggplot(aes(x = x, y = value, group = region, color = region))+
        geom_line()+
        facet_wrap(scenario~class1)+
        scale_x_continuous(limits = c(2010, 2055), breaks = c(2015, 2050))+
        theme_bw()+
        theme_minimal()+
        theme(plot.title = element_text(size = 20, face ="bold"),
              strip.text.x  = element_text(size=12),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              # panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              plot.background = element_rect(fill = "white"),
              plot.title.position =  "plot")+
        labs(title = unit_param_selected,
             subtitle = paste("Parameter:", i),
             x = "year",
             y = unit_param_selected)
    
    setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_two_regions")
    ggsave(plot =graph, file =paste0(i, ".png"),  width =1200, height = 700, units ="px", dpi = 150)  
    
}

