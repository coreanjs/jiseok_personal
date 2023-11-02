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
GGS621_cap_tax <- gcamextractor::readgcam(gcamdatabase = "E:/gcam-v7.0-Windows-Release-Package_GGS621/output/assignment4_land_carbon_pricing",
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
                                    regionsSelect = c("Global"),
                                    folder = "gcamextractor/assignment4_land_carbon_pricing")

GGS621_cap_tax <- gcamextractor::readgcam(gcamdatabase = "E:/gcam-v7.0-Windows-Release-Package_GGS621/output/assignment4_land_carbon_pricing",
                                          paramsSelect = c("energy",
                                              "electricity", 
                                              "transport", 
                                              "building", 
                                              "emissions",
                                              "hydrogen"
                                          ), ### this is a key
                                          scenOrigNames = c("with tax", "without tax"),
                                          regionsSelect = c("Global"),
                                          folder = "gcamextractor/assignment4_land_carbon_pricing")


??readgcam
 

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
    mutate(param = ifelse(is.na(param),origQuery, param))



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
  # scale_x_continuous(limits = c(2010, 2055), breaks = c(2015, 2050))+
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
     
     setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/Assignment4")
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
 dir.create("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_aggParam_Assignment4_1004")
 
 is.na(unique(aggParam$param))
 
 
 
 
 
 str(aggParam)
 
 
 
 unique(aggParam$param) ->aggParam_all
 
 aggParam_all
 
 i <-aggParam_all
 
 i
 
 ####  과제용 결과
 for (i in aggParam_all) {
     
     aggParam %>%
         filter(param == i) %>% 
         distinct(units)->unit_param_selected
     
     unit_param_selected
     
     
     graph =  aggParam %>%
         filter(param == i) %>% 
         ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
         geom_line(linewidth = .8)+
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
         labs(title = paste("Parameter:", i, "\nRegion: South Korea"),
              x = "year",
              y = unit_param_selected)
     
     setwd("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_aggParam_Assignment4")
     ggsave(plot =graph, file =paste0(i, ".png"),  width =1800, height = 800, units ="px", dpi = 150)  
     
 }
 
 
 
 
 
 unique( aggParam$param)
 
 
 aggParam %>%
     filter(param == "emissGHGByGasNoBioGWPAR5") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line(linewidth = .7)+
     facet_wrap(~class, nrow = 1)+
     scale_y_continuous(limits = c(0,800), breaks = seq(0, 800, 200))+
     scale_x_continuous(limits = c(2010,2100), breaks = c(2015, 2050,2100))+
     scale_color_manual(values = c("#1f5c99", "#d83d36"))+
     theme_bw()+
     geom_vline(xintercept = 2050, linetype = "dotted")+
     #theme_minimal()+
     theme(plot.title = element_text(size = 20, face ="bold"),
           strip.text.x  = element_text(size=14),
           axis.text.x = element_text(size =8),
           axis.text.y = element_text(size =16),
           axis.title = element_text(size =16),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x = element_blank(),
           # panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           plot.background = element_rect(fill = "white"),
           plot.title.position =  "plot",
           legend.position ="top",
           legend.text = element_text(size = 16),
           legend.title = element_text(size = 16),
           plot.subtitle = element_text(size = 16))+
     labs(title = paste("Parameter:", "emissGHGBySectorGWPAR5", "\nRegion: South Korea"),
          x = "year",
          y = "GHG emissions(MTCO2eq)")
 
 
 CO2<-aggParam %>%
     filter(param == "emissGHGByGasNoBioGWPAR5" & class =="CO2") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line(linewidth = 1.2)+
     facet_wrap(~class)+
     scale_y_continuous(limits = c(0,800), breaks = seq(0, 800, 200))+
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
           legend.position ="none",
           legend.text = element_text(size = 16),
           legend.title = element_text(size = 16),
           plot.subtitle = element_text(size = 16))+
     labs(title = paste("Parameter:", "emissGHGBySectorGWPAR5"),
          x = "year",
          y = "GHG emissions(MTCO2eq)")
 
 
 
 non_cO2<-aggParam %>%
     filter(param == "emissGHGByGasNoBioGWPAR5" & class !="CO2") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line(linewidth = 1.2)+
     facet_wrap(~class)+
     scale_x_continuous(limits = c(2010,2100), breaks = c(2015, 2050,2100))+
     scale_color_manual(values = c("#1f5c99", "#d83d36"))+
     theme_bw()+
     scale_y_continuous(limits = c(0,200), breaks = seq(0, 200, 50))+
     
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
 
 
 

library(patchwork)

 CO2+non_cO2+plot_layout(widths= c(1, 2))
  
 

 
 
 
 
 
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





Assign4_CO2<-data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
          scenario = c("without tax,date=2023-3-10T11:17:05+09:00","without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00","without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00","without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00","without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00","without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00","without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00","without tax,date=2023-3-10T11:17:05+09:00",
                       "without tax,date=2023-3-10T11:17:05+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00","with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00",
                       "with tax,date=2023-3-10T10:26:00+09:00"),
            region = c("Africa_Eastern",
                       "Africa_Northern","Africa_Southern","Africa_Western","Argentina",
                       "Australia_NZ","Brazil","Canada",
                       "Central America and Caribbean","Central Asia","China","Colombia","EU-12",
                       "EU-15","Europe_Eastern","Europe_Non_EU",
                       "European Free Trade Association","India","Indonesia","Japan",
                       "Mexico","Middle East","Pakistan","Russia","South Africa",
                       "South America_Northern","South America_Southern",
                       "South Asia","South Korea","Southeast Asia","Taiwan",
                       "USA","Africa_Eastern","Africa_Northern",
                       "Africa_Southern","Africa_Western","Argentina","Australia_NZ",
                       "Brazil","Canada","Central America and Caribbean",
                       "Central Asia","China","Colombia","EU-12","EU-15",
                       "Europe_Eastern","Europe_Non_EU","European Free Trade Association",
                       "India","Indonesia","Japan","Mexico","Middle East",
                       "Pakistan","Russia","South Africa",
                       "South America_Northern","South America_Southern","South Asia",
                       "South Korea","Southeast Asia","Taiwan","USA"),
            `1990` = c(7.29546958,68.6011219,
                       12.12024443,31.33727561,30.30898252,84.05244416,60.66401818,
                       122.8644938,28.98602272,157.6478554,704.3086216,
                       15.57370341,280.6282376,929.8597022,247.6758835,
                       76.30907415,21.75811493,163.1816518,45.45734086,313.6469309,
                       82.27524677,201.2712341,18.0003098,687.3974359,
                       110.8058714,29.44348321,24.21949015,7.266842758,76.41944241,
                       120.7887822,35.9482324,1399.615833,7.29546958,
                       68.6011219,12.12024443,31.33727561,30.30898252,84.05244416,
                       60.66401818,122.8644938,28.98602272,157.6478554,
                       704.3086216,15.57370341,280.6282376,929.8597022,
                       247.6758835,76.30907415,21.75811493,163.1816518,45.45734086,
                       313.6469309,82.27524677,201.2712341,18.0003098,
                       687.3974359,110.8058714,29.44348321,24.21949015,7.266842758,
                       76.41944241,120.7887822,35.9482324,1399.615833),
            `2005` = c(13.26049531,112.0510738,
                       13.74954972,33.95777911,42.68001188,117.3077335,
                       99.05170058,160.8218219,47.15161122,111.4762844,1834.718894,
                       17.90755567,210.0585213,979.7425884,117.885666,
                       99.31097999,24.61551877,340.1312365,99.46612143,347.5839551,
                       127.4409894,388.1067566,35.53391885,466.9120144,
                       135.9720698,45.315721,40.53509607,18.83741971,144.7092859,
                       233.8970074,79.14951935,1637.918437,13.26049531,
                       112.0510738,13.74954972,33.95777911,42.68001188,
                       117.3077335,99.05170058,160.8218219,47.15161122,111.4762844,
                       1834.718894,17.90755567,210.0585213,979.7425884,
                       117.885666,99.31097999,24.61551877,340.1312365,99.46612143,
                       347.5839551,127.4409894,388.1067566,35.53391885,
                       466.9120144,135.9720698,45.315721,40.53509607,18.83741971,
                       144.7092859,233.8970074,79.14951935,1637.918437),
            `2010` = c(18.60324822,132.1833064,
                       16.36180632,37.93840925,52.00964039,123.9798852,
                       117.1849099,156.04456,53.58061116,131.1313088,2646.823844,
                       21.70528338,205.6855991,890.8484555,106.4456099,
                       114.9417471,24.88155055,494.8799899,111.345204,325.3416707,
                       134.9299007,504.5306359,42.34560222,482.7929978,
                       147.4966581,57.9726238,54.05683784,24.71153329,169.8134322,
                       282.334684,79.34489853,1528.415113,18.60324822,
                       132.1833064,16.36180632,37.93840925,52.00964039,123.9798852,
                       117.1849099,156.04456,53.58061116,131.1313088,
                       2646.823844,21.70528338,205.6855991,890.8484555,
                       106.4456099,114.9417471,24.88155055,494.8799899,111.345204,
                       325.3416707,134.9299007,504.5306359,42.34560222,
                       482.7929978,147.4966581,57.9726238,54.05683784,24.71153329,
                       169.8134322,282.334684,79.34489853,1528.415113),
            `2015` = c(22.56909506,148.1200526,
                       23.77440473,51.06993825,56.06245078,122.160479,138.9904851,
                       164.5752499,56.81639572,147.7041843,3044.986753,
                       26.05313134,188.3831329,798.2274754,77.79744725,
                       130.4221869,23.0077097,627.8980905,138.6639407,328.7782966,
                       136.2411711,587.0841762,48.53482949,489.0646884,
                       144.3761191,48.93943718,62.11418195,36.47257858,181.484604,
                       326.6939383,78.2146595,1437.174546,22.56909506,
                       148.1200526,23.77440473,51.06993825,56.06245078,122.160479,
                       138.9904851,164.5752499,56.81639572,147.7041843,
                       3044.986753,26.05313134,188.3831329,798.2274754,
                       77.79744725,130.4221869,23.0077097,627.8980905,138.6639407,
                       328.7782966,136.2411711,587.0841762,48.53482949,
                       489.0646884,144.3761191,48.93943718,62.11418195,36.47257858,
                       181.484604,326.6939383,78.2146595,1437.174546),
            `2020` = c(29.22679889,168.4551604,
                       23.97391522,53.90801308,54.14909056,128.7102331,137.742786,
                       176.1223478,60.4733743,159.9544444,3609.729939,
                       28.35076738,218.1269862,818.9077331,84.22685382,
                       137.3838326,23.7405821,812.8494418,166.4916273,324.1226335,
                       143.9971953,591.519254,55.37940134,531.955734,
                       143.9704283,33.09648993,67.13804396,46.53565579,183.1868556,
                       377.7638917,82.9608282,1454.137454,29.22679889,
                       168.4551604,23.97391522,53.90801308,54.14909056,128.7102331,
                       137.742786,176.1223478,60.4733743,159.9544444,
                       3609.729939,28.35076738,218.1269862,818.9077331,84.22685382,
                       137.3838326,23.7405821,812.8494418,166.4916273,
                       324.1226335,143.9971953,591.519254,55.37940134,531.955734,
                       143.9704283,33.09648993,67.13804396,46.53565579,
                       183.1868556,377.7638917,82.9608282,1454.137454),
            `2025` = c(34.01529934,188.2958988,
                       27.57516256,62.1866885,59.04663619,134.8848511,147.1886783,
                       179.1102175,63.73746653,176.5599916,3974.638212,
                       32.25012553,226.343162,799.5233554,89.70493777,
                       148.7495878,22.88422508,1081.632,196.9065129,319.3228308,
                       153.2826995,616.4500768,60.46088775,543.9287137,
                       151.0948874,34.72957532,73.27153598,56.8502032,189.4524688,
                       428.3805768,86.37192403,1453.701497,32.42741702,
                       181.0834717,25.82202622,60.1195561,56.84976308,131.9193607,
                       141.6341826,174.9263245,61.53938398,169.3488372,
                       3788.077275,31.170539,216.662518,783.7230911,85.04042606,
                       143.0061659,22.4688885,1011.39537,188.6799015,
                       313.4555169,149.8798625,599.5502891,57.78475918,523.7610585,
                       146.0580046,33.44933477,70.29810914,53.04707644,
                       185.6746417,414.4774668,84.28562865,1430.838129),
            `2030` = c(40.65621773,213.2398004,
                       34.15811631,81.99542121,62.65154612,138.7540679,
                       156.9660859,180.7679315,67.25850187,190.5881105,4173.89243,
                       35.50720324,230.9906345,781.8643401,95.80273187,
                       158.1084669,22.02204365,1306.565016,228.306611,312.1227847,
                       162.5645428,655.7555994,71.07401258,563.6889094,
                       160.9983399,38.42238243,80.25105817,68.66254768,191.4815814,
                       472.103949,87.57515753,1449.701053,37.28400889,
                       200.5801886,29.57686372,75.54834322,58.64879903,
                       133.221536,145.6172315,173.0528255,63.38966703,177.3988545,
                       3872.430472,33.69920967,216.5170759,752.866276,
                       87.29792977,148.8485962,21.31710607,1178.348812,213.1057375,
                       300.302082,156.1530785,618.9711188,65.63748962,
                       526.5715953,152.4096414,36.55929001,74.46572997,61.32620259,
                       184.444437,448.0629002,83.86123907,1404.39946),
            `2035` = c(49.43411935,236.6642377,
                       42.64468658,107.4025495,65.47799684,140.8787487,
                       164.8798669,179.7536267,70.53396265,198.7990274,4234.5883,
                       38.95071772,232.2018804,771.3722918,100.6843222,
                       165.0640174,21.11672413,1524.033909,256.8506552,301.0086184,
                       170.1437128,690.1975182,83.14272764,572.8699822,
                       167.4231796,39.22963453,86.98452459,80.80740573,189.3828348,
                       510.3668694,87.70732237,1439.246758,42.83443198,
                       217.3977004,34.2971576,94.34683587,59.49022597,
                       131.8139735,146.1540206,168.132913,64.26763508,179.3009498,
                       3790.472017,35.88050156,212.020723,724.762866,
                       87.6954951,151.2391166,19.91905828,1321.816742,232.543679,
                       281.9126135,159.8283357,633.0965075,73.82649951,
                       515.7782725,154.0875322,36.42505496,77.47993314,69.51464588,
                       178.1628012,471.1394746,81.89163845,1363.127833),
            `2040` = c(61.98955101,259.8522437,
                       53.97331579,141.0955531,67.54456822,141.7988499,
                       169.4489409,179.0514387,73.47775113,203.0956821,4212.156104,
                       43.40252704,230.2394419,762.2588002,105.1129314,
                       170.6847582,20.15657382,1731.657245,283.2776005,288.4595888,
                       176.6802375,721.5675328,97.85783404,575.3672799,
                       171.6739997,41.75508688,93.87482217,94.2249808,
                       184.2367052,548.2378449,87.11271429,1436.397368,49.57506036,
                       232.8415617,40.11915994,117.9588991,59.40137153,
                       127.9141327,141.0935882,162.0145861,63.81988939,176.8662685,
                       3612.034818,38.36779582,202.7029743,691.0483105,
                       87.14376125,150.8210841,18.12690161,1443.759782,
                       246.1992182,261.6219467,161.0668392,640.60331,82.78479693,
                       496.320106,152.200779,37.4114632,79.49849543,77.71093481,
                       167.885194,486.5153567,78.88102444,1319.440182),
            `2045` = c(77.83355648,283.8527617,
                       69.55562793,185.5795052,68.67162922,145.1039105,
                       173.9367274,179.4040513,76.57999005,205.6159829,4132.335744,
                       47.94664612,228.7829749,775.0755334,108.3874359,
                       176.8515029,21.31454465,1915.550124,307.1883449,277.8808749,
                       183.7029462,753.0010374,117.8405663,577.5586228,
                       174.5983244,46.50202987,101.0421299,108.3874044,
                       176.6848704,583.9588051,85.31319692,1445.686371,56.44284035,
                       248.2661913,47.47900818,147.3237591,58.05232768,
                       124.4254584,132.9398702,154.43479,62.34510306,171.7532036,
                       3371.180269,40.40803431,192.2112216,657.8953656,
                       85.27856112,149.4948172,16.82490425,1528.445531,
                       254.4593979,237.8678086,160.5004009,643.6312351,93.83451134,
                       474.5189787,147.8188763,39.784422,80.34542103,
                       84.66457745,154.1325264,492.3953244,74.3697803,1256.598174),
            `2050` = c(98.5996862,307.7088544,
                       91.02555495,244.6752292,69.69648244,146.566531,176.1439106,
                       178.5203592,80.47557657,207.2492338,3985.598949,
                       52.28839352,225.5109578,778.8874756,110.3100326,
                       181.2088248,23.33019824,2083.605434,327.8668734,267.0440491,
                       192.377525,786.0516088,136.9077275,578.7164168,
                       176.6751571,47.50381941,107.9031634,126.1686736,167.9607246,
                       621.9113651,82.95809364,1454.802896,62.79804399,
                       261.8464809,57.36925717,184.1989187,56.13396071,
                       118.5025155,121.7748012,144.1798833,60.18072818,165.0584491,
                       3068.629304,41.48366801,179.2344531,615.2525928,
                       82.06302811,145.5800305,16.44943982,1588.807757,256.9931934,
                       213.3711201,157.7048206,642.972221,104.1873307,
                       450.1610444,141.6506417,38.65653256,79.6136287,
                       92.37316836,138.8100492,491.7256719,69.08778217,1180.2861),
            `2055` = c(127.7146783,331.9035023,
                       119.8041742,320.3928796,71.34566231,149.9466427,
                       183.0379805,177.6358258,85.65370334,209.3423595,3826.575938,
                       57.42934572,223.1808892,788.5783842,112.2512526,
                       186.2654572,25.62027583,2250.18918,349.1180712,257.5678471,
                       204.2584932,835.6583581,156.5159338,582.7902039,
                       179.2143315,48.26513381,115.638901,147.5086368,160.3240919,
                       666.6615148,80.9861112,1471.262945,70.945076,
                       274.2148599,69.78982631,229.4842251,54.50847202,113.5936911,
                       113.6902158,135.1892635,58.54361779,159.5072532,
                       2761.852527,42.86296881,167.7216908,580.2522258,
                       78.98647512,142.0953571,16.63539759,1640.376987,257.4215349,
                       191.5374931,157.1635651,645.2219833,113.6818622,
                       427.3879197,135.3453088,37.00770261,79.28347999,
                       101.2393737,125.2741011,492.6836762,64.14129778,1116.8092),
            `2060` = c(163.879351,353.2701263,
                       155.7928079,411.9678506,73.28144527,155.392751,193.1244069,
                       179.1687292,91.31418638,212.2789635,3670.315532,
                       62.98773189,221.51639,803.2245442,113.4838834,191.1717104,
                       28.08642124,2408.219755,369.507219,248.2010255,
                       216.627972,891.2805272,172.7700974,588.8639652,
                       183.2253843,49.8277631,123.8732486,170.2289476,153.3963509,
                       714.8227205,79.72773905,1500.84604,79.24487879,
                       281.1653979,82.97334912,276.7413367,52.68297403,109.3797975,
                       107.7289661,128.0895135,56.61031802,154.2574016,
                       2453.404732,43.97467711,156.5932386,551.0738102,75.5167223,
                       138.148159,16.96590094,1674.593186,254.4049458,
                       169.6291722,156.2272927,653.0916633,120.3022232,
                       405.8980727,129.5137563,36.68095083,78.54709778,108.4731548,
                       112.8202208,489.2317615,59.57611106,1061.766505),
            `2065` = c(204.8029334,369.2979231,
                       197.7068107,518.1111398,75.19405104,161.5716908,
                       203.1226219,182.7009781,96.95476809,212.5582083,3520.38774,
                       68.29306866,219.2877581,816.8899714,113.3268583,
                       195.2986645,30.27936953,2548.003536,387.0093971,239.4088026,
                       229.2135054,955.3564795,192.3170562,589.5360619,
                       187.8484361,52.41531566,131.42358,192.7510128,147.2693996,
                       762.1146103,79.71521098,1530.082013,84.79310886,
                       278.96672,95.03675058,321.4498474,49.61034931,103.4692436,
                       96.6437659,119.035428,52.98534943,143.5851517,
                       2115.107604,43.39209017,141.5607974,512.0485579,
                       70.14731872,131.6779419,16.27926119,1668.556318,243.1048914,
                       145.8521904,151.8563726,663.5393995,124.864894,
                       377.1034244,122.8234126,36.38091212,74.90828705,111.8067528,
                       100.3257182,468.8645941,54.62511577,981.7546877),
            `2070` = c(249.0820614,384.461,
                       245.2009445,634.4204772,76.72436146,168.5108078,213.3813325,
                       190.7735498,103.2996196,214.2695827,3389.273604,
                       73.24643577,217.9859438,838.0597555,112.8923344,200.0238143,
                       32.85276299,2672.67829,402.7707496,233.2174614,
                       242.491755,1028.054091,212.9773199,599.9471542,
                       192.3320493,56.4789122,138.7975704,214.8701564,142.4321037,
                       808.8538212,80.30168875,1575.334793,85.13503773,
                       274.2918176,102.3441908,356.4593166,46.02450959,94.48699759,
                       77.72603784,107.4241291,47.99032044,132.4811295,
                       1740.671642,40.73713023,123.6444757,466.6286237,
                       63.93969697,122.5253833,15.26678586,1599.029825,221.9998462,
                       123.8518013,142.8442104,676.9200708,127.5462291,
                       353.3689864,113.380332,37.11432186,67.55308734,110.4927413,
                       87.28146224,428.8494574,48.75809134,899.9126231),
            `2075` = c(296.2041547,395.9934972,
                       295.8600314,755.7675781,77.89456411,174.0600172,
                       223.2127498,198.3123714,109.3862917,213.9988178,3228.463829,
                       77.49049805,214.970876,861.1427231,109.7057443,
                       203.6100693,35.48962106,2766.573509,414.5182906,225.6318971,
                       255.3965865,1092.10555,233.3154766,606.479761,
                       193.9298501,61.33751624,145.1680755,237.6745339,136.334038,
                       849.1154327,80.16727653,1613.7459,78.66058358,
                       259.9937156,101.7272567,371.1028404,39.85226686,79.79210908,
                       39.11166265,91.0497681,38.26038589,114.5593603,
                       1299.684541,34.56696091,98.3411135,390.4563954,53.62774375,
                       107.150429,12.49590028,1429.116451,177.3135878,
                       90.77653416,123.1377493,651.0667603,124.8499794,
                       310.5320585,97.76496661,36.64612392,53.21274897,104.018217,
                       70.61855932,347.1880207,39.16931083,742.6597539),
            `2080` = c(345.8546988,403.5932163,
                       348.1220444,879.0347686,78.64732081,175.9323116,
                       231.0556437,203.1876269,114.8414064,210.0592346,2946.76731,
                       80.70795003,204.2090687,870.160771,106.687298,
                       204.5715459,37.67964388,2772.148521,419.3857919,211.6702339,
                       266.5259945,1143.845409,252.3027189,605.1009376,
                       194.8038036,65.49752208,149.469966,259.5888409,129.4367049,
                       872.8869353,76.88243762,1664.702099,66.70924733,
                       236.7210307,91.89302795,361.8346355,31.07190078,
                       61.72559455,-17.05021837,64.17445385,22.867561,88.80237187,
                       823.7062288,24.77728756,66.5245034,291.8062196,
                       41.88178885,85.45049146,7.191285385,1139.959955,111.6164352,
                       51.62383977,92.13678839,579.5447766,116.1833404,
                       245.1366139,78.26210184,33.63952373,33.02107368,92.10243643,
                       53.31347798,227.7063478,26.14767441,519.2633677),
            `2085` = c(398.0955105,413.5790429,
                       400.192508,998.8886111,79.3318818,178.5325664,237.197008,
                       207.5436874,120.232929,207.0288924,2698.793303,
                       83.33161512,197.8453891,885.0580491,103.9062507,204.7149491,
                       39.47335941,2757.727628,423.5752223,204.5222328,
                       276.3062896,1184.3128,269.7765742,602.5970948,
                       194.1212707,68.98455632,153.0883621,281.9934453,123.5061408,
                       896.2174192,75.13603086,1686.630391,40.97686588,
                       202.5048736,65.48381854,309.6520667,16.10185483,35.91524097,
                       -72.0902946,14.36374288,-3.289982281,49.29630255,
                       341.4125319,7.436551794,32.89181767,157.8023339,
                       28.22502068,55.77709303,-3.800916494,779.6949607,48.78622232,
                       4.991945559,37.69425553,445.0972622,98.67583512,
                       134.5703008,54.78665991,25.58740672,-2.281595515,
                       72.8642898,36.5535851,57.98341103,12.73821582,166.3621832),
            `2090` = c(450.6002525,424.1257854,
                       449.6314015,1115.659567,79.9947083,180.2852332,241.9318876,
                       210.8466171,125.2393715,204.2185137,2492.477806,
                       85.46843749,191.483216,894.6871049,101.0968001,
                       203.7144796,41.05878869,2731.837478,424.5431113,197.475954,
                       284.2412644,1217.201342,283.808965,599.7703733,
                       190.4906455,71.24349806,155.4970962,305.4798504,117.5174456,
                       915.588193,73.59453343,1689.223435,3.122860256,
                       159.6341337,26.20690393,209.9708481,-11.16660316,
                       -3.257280927,-136.4669812,-69.5063151,-42.14589987,-13.52517694,
                       -180.2349154,-22.01996097,-3.709831133,-24.1528696,
                       12.68272044,17.58627594,-18.18626544,462.1944486,
                       1.140927642,-51.26092084,-60.40570405,250.7279036,
                       71.53600526,-56.8363958,33.78394426,10.47143343,-72.61081438,
                       51.68406128,22.4267725,-142.5137399,0.013111688,
                       -353.1614598),
            `2095` = c(503.3356163,434.4414051,
                       494.8427853,1226.711928,80.51729087,181.7665866,
                       245.8993706,214.3410897,129.8060026,202.9031881,2323.816331,
                       87.15263046,185.9696703,903.7202026,98.68335606,
                       202.5675466,42.6206581,2691.781979,423.3088907,191.2337808,
                       291.8207429,1238.500616,298.3963176,597.4523179,
                       186.0709547,72.80822848,157.3798911,332.8208244,
                       111.6897509,930.775057,71.79919875,1686.06859,-49.6948271,
                       113.6283908,-18.79557757,60.50672156,-56.21242264,
                       -57.75199215,-224.2195309,-154.5561319,-90.70433561,
                       -88.844982,-815.3908682,-57.0206217,-34.8276284,-195.3035263,
                       -2.485717205,-13.70304922,-30.53400754,207.3023978,
                       -45.56285893,-108.2082878,-193.4691718,29.82173702,
                       35.58134277,-271.0307537,16.25841099,-11.21866612,
                       -191.6389448,27.68972726,10.03963825,-322.7075817,
                       -12.3832771,-1053.728754),
            `2100` = c(556.4704487,444.8941417,
                       537.1213802,1333.856851,81.09393367,183.3259425,
                       249.0912379,216.6993214,134.138919,202.5303815,2180.8376,
                       88.57413529,181.2552761,912.016173,96.46460733,201.7352406,
                       44.27563179,2648.81038,420.4833962,184.190504,
                       299.3727735,1261.182713,312.9350108,596.6040067,
                       180.9930284,74.19196698,159.0815572,371.9019149,106.0299819,
                       942.6569498,69.87676339,1675.786441,-105.6052556,
                       72.84402778,-63.64400006,-115.9118139,-87.50247013,
                       -89.87197637,-321.1708997,-194.3865427,-129.1129222,
                       -130.743314,-1162.001733,-80.74727141,-54.93582246,
                       -308.5208841,-14.18256839,-33.407619,-38.32210443,2.334450319,
                       -100.9540237,-133.3976218,-265.0162148,-110.7958143,
                       -2.292130448,-380.5379948,-0.301711953,-25.6075267,
                       -248.9989846,6.331699725,-1.982737994,-434.1782278,
                       -22.03655021,-1461.918311),
             Units = c("MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC")
) %>% 
    select(-Units) %>% 
    pivot_longer(-c('scenario', 'region'), names_to = 'year', values_to="MTC") %>% 
    mutate(year = as.numeric(year))

library(tidyverse)




Assign4_CO2 %>% 
    mutate(scenario = ifelse(str_detect(scenario, "without tax"), "without tax", "with tax")) %>% 
    group_by(scenario, year) %>% 
    summarise(MTC = sum(MTC)) %>% 
    ggplot(aes(x = year, y =MTC, group = scenario, color = scenario))+
    geom_line()+
    scale_y_continuous(labels = comma)





netCO2<-data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
          scenario = c("without tax", "with tax"),
            `1990` = c(7093.481074, 7093.481074),
            `2005` = c(8615.980176, 8615.980176),
            `2010` = c(10094.56154, 10094.56154),
            `2015` = c(11588.23347, 11588.23347),
            `2020` = c(11580.47692, 11580.47692),
            `2025` = c(13406.77216, 12993.06957),
            `2030` = c(13817.99291, 13083.86094),
            `2035` = c(14309.55435, 13182.63907),
            `2040` = c(14665.49731, 13025.90227),
            `2045` = c(15133.4513, 12929.59604),
            `2050` = c(15288.50585, 12448.98694),
            `2055` = c(15624.73118, 11719.50642),
            `2060` = c(15839.43063, 11160.39865),
            `2065` = c(16147.0178, 10517.50844),
            `2070` = c(16485.26431, 9668.31595),
            `2075` = c(16784.19559, 8430.658211),
            `2080` = c(16813.83979, 6760.315867),
            `2085` = c(16843.18215, 5390.26811),
            `2090` = c(16855.09117, 3624.592101),
            `2095` = c(16872.62885, 1101.861467),
            `2100` = c(16919.35988, -4144.086314)
) %>% 
    pivot_longer(-scenario, names_to ="year", values_to="MTC") %>% 
    mutate(year = as.numeric(year))


netCO2 %>% 
    ggplot(aes(x = year, y =MTC, group = scenario, color = scenario))+
    geom_line()+
    scale_y_continuous(labels = comma)+
    theme_bw()+
    theme_minimal()+
    labs(title = "Global net CO2 emission from different scenarios")



food_price<-data.table::data.table(
  check.names = FALSE,
     scenario = c("with tax,date=2023-3-10T10:26:00+09:00",
                  "with tax,date=2023-3-10T10:26:00+09:00",
                  "without tax,date=2023-3-10T11:17:05+09:00",
                  "without tax,date=2023-3-10T11:17:05+09:00"),
       region = c("Global", "Global", "Global", "Global"),
        input = c("FoodDemand_NonStaples",
                  "FoodDemand_Staples","FoodDemand_NonStaples","FoodDemand_Staples"),
       `1990` = c(10.96287, 1.3047748, 10.96287, 1.3047748),
       `2005` = c(10.507308, 1.2741625, 10.507308, 1.2741625),
       `2010` = c(9.50633, 1.2635177, 9.50633, 1.2635177),
       `2015` = c(8.997136, 1.2399638, 8.997136, 1.2399638),
       `2020` = c(9.044503, 1.2445786, 9.044503, 1.2445786),
       `2025` = c(9.132006, 1.2581823, 9.126368, 1.2560148),
       `2030` = c(9.204063, 1.263529, 9.195602, 1.2604015),
       `2035` = c(9.28314, 1.2740607, 9.270983, 1.2696227),
       `2040` = c(9.360132, 1.2834574, 9.345019, 1.2780425),
       `2045` = c(9.440007, 1.2940342, 9.419221, 1.2867668),
       `2050` = c(9.512478, 1.3026854, 9.482584, 1.2926839),
       `2055` = c(9.597168, 1.3177376, 9.565208, 1.307122),
       `2060` = c(9.671017, 1.3303829, 9.635543, 1.3187121),
       `2065` = c(9.738667, 1.3416277, 9.694347, 1.3270833),
       `2070` = c(9.799458, 1.3508437, 9.740192, 1.3320448),
       `2075` = c(9.861513, 1.3604229, 9.774384, 1.3343301),
       `2080` = c(9.929654, 1.3719317, 9.79896, 1.3347136),
       `2085` = c(10.037166, 1.3948823, 9.813991, 1.3329829),
       `2090` = c(10.210461, 1.436337, 9.820498, 1.3295583),
       `2095` = c(10.479325, 1.5035217, 9.821104, 1.3251138),
       `2100` = c(10.629202, 1.5378529, 9.814321, 1.3191097)
) %>% 
    select(-region) %>% 
    pivot_longer(-c('scenario', 'input'), names_to = "year", values_to="dollar") %>% 
    mutate(year = as.numeric(year))


food_price %>% 
    mutate(scenario = ifelse(str_detect(scenario, "without tax"), "without tax", "with tax")) %>% 
    ggplot(aes(x = year, y = dollar, group = scenario, color =scenario))+
    geom_line()+
    facet_wrap(~input)+
    theme_bw()+
    theme_minimal()+
    labs(title = "Global food demand prices changes from different scenarios",
         y = "2005 US$/Mcal/day")

    
    
    