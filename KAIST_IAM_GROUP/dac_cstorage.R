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


setwd("C:/R/Rproject/jiseok_personal/KAIST_IAM_GROUP")




### Import NZ2050
dac_cstorage<- gcamextractor::readgcam(gcamdatabase = "E:/gcam-v7.0-Windows-Release-Package/output/cstorage_dac",
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
                                    regionsSelect = c("South Korea", "Japan"),
                                    folder = "gcamextractor/dac_cstorage")



gcamextractor::params # view available parameters
gcamextractor::queries # Get all queries used
gcamextractor::map_param_query # Get a table of params and the relevants queries used to extract and calculate them.

gcamextractor::map_param_query %>% View()

GGS621_cap_tax
### list 파일 만지고 놀기 R리스트 완전정복(슬기로운 통계생활)
class(dac_cstorage)

# View your data

library(RColorBrewer)
### Elec gen



NZ2050#

## Merging two scenarios into a dataframe 
## param : NA -> 'CO2 sequestration by sector' 수정 

dac_cstorage

### CO2 sequestration by sector --> NA
dac_cstorage<- dac_cstorage$dataAll %>% 
    mutate(param = ifelse(is.na(param),origQuery, param))


getwd()
library(stringr)
dac_cstorage_Extended<- read.csv("./gcamextractor/dac_cstorage/gcamDataTable_Extended.csv") %>% 
    mutate(scenario = ifelse(str_detect(scenario, "cstorage"), "dac+cstorage", "dac"),
           param = ifelse(is.na(param),origQuery, param)) 


dac_cstorage_Extended %>% View()



head(dac_cstorage_Extended)


###Count unique values for every column 
apply(dac_cstorage_Extended, 2, function(x) length(unique(x)))


unique(dac_cstorage_Extended$origScen)




str(dac_cstorage)

##What's in parameter
unique(dac_cstorage_Extended$param)




## 전 부문의 기술별 전력생산


dac_cstorage_Extended %>% 
    filter(is.na(param))



### CO2 sequestration은 데이터 구성이 특이

### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이
### CO2 sequestration은 데이터 구성이 특이

## CO2 SEQ를 제외하면 다른 결과는 다 동일함

unique(dac_cstorage_Extended$param)

unique(dac_cstorage_Extended$param)[15] -> param_selected

param_selected

dac_cstorage_Extended %>%
    filter(param == param_selected) %>% 
    distinct(units)->unit_param_selected

unit_param_selected

dac_cstorage_Extended %>%
    filter(param == param_selected) %>% 
    distinct(origQuery) ->origQuery

origQuery



dac_cstorage_Extended %>%
    filter(param == param_selected & x >= 2015 & x <= 2050) %>%
    group_by(x, scenario, sector, region) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = sector, fill = sector))+
    geom_bar(stat='identity')+
   # scale_fill_brewer(palette="Set1")+
    scale_x_continuous(limits = c(2010, 2100), breaks = c(2015, 2050, 2100))+
    facet_grid(region~scenario)+
    geom_text(aes(label = round(after_stat(y), 0), group = x), 
              stat = 'summary', fun = sum, vjust = 1.3)+
    theme_bw()+
    theme(plot.title = element_text(size = 16),
          strip.text.x  = element_text(size=12),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title.position =  "plot")+
    labs(title = paste("OriginQuery:", origQuery),
         fill ="구분",
         x = "연도",
         y = unit_param_selected)


ggsave(file =paste0("param_", param_selected, ".png"),  width =1200, height = 700, units ="px", dpi = 150)



dac_cstorage_Extended %>%
    filter(param == param_selected & x >= 2015 ) %>%
    group_by(x, scenario, sector, region) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = sector, color = sector))+
    geom_line()+
    scale_x_continuous(limits = c(2010, 2100), breaks = c(2015, 2050, 2100))+
    facet_wrap(region~scenario)+
    #gghighlight(sector =="process heat dac")+
    
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
 
 


dac_cstorage_Extended %>%
    filter(param == param_selected & x >= 2015 & region =="South Korea") %>%
    group_by(x, scenario, sector) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
    geom_line(linewidth = 1)+
    scale_x_continuous(limits = c(2010, 2100), breaks = c(2015, 2050, 2100))+
    facet_wrap(~sector, ncol = 3)+
    #gghighlight(sector =="process heat dac")+
    
    theme_bw()+
    theme(plot.title = element_text(size = 24),
          strip.text.x  = element_text(size=14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title.position =  "plot",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14))+
    labs(title = paste( "OriginQuery:", origQuery, "\nRegion: South Korea"),
         fill ="구분",
         x = "연도",
         y = unit_param_selected)




 
 
 

 
 
 
 
 ####### Image create using function
dir.create("C:/R/Rproject/jiseok_personal/KAIST_IAM_GROUP/gcamextractor/dac_cstorage/img")

 is.na(unique(dac_cstorage_Extended$param))
 
 
 
 unique(dac_cstorage_Extended$param)[-15] ->param_all

 param_all
 
 for (i in param_all) {
     
     dac_cstorage_Extended %>%
         filter(param == i) %>% 
         distinct(units)->unit_param_selected
     
     unit_param_selected
     
     dac_cstorage_Extended %>%
         filter(param == i) %>% 
         distinct(origQuery) ->origQuery
     
     
     graph = dac_cstorage_Extended %>%
         filter(param == i & x >= 2015) %>%
         group_by(x, class1, scenario, region) %>% 
         summarise(value = sum(value)) %>% 
         ggplot(aes(x = x, y = value, group = class1, fill = class1))+
         geom_bar(stat='identity')+
        # scale_fill_brewer(palette="Set1")+
         scale_x_continuous(limits = c(2010, 2055), breaks = c(2015, 2050))+
        # scale_y_continuous(labels = comma, limits= c(0, 1200), breaks = seq(0, 1200, 200))+
        # geom_text(aes(label = round(after_stat(y), 0), group = x), 
        #           stat = 'summary', fun = sum, vjust = -1)+
         facet_grid(region~scenario)+
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
     
     setwd("C:/R/Rproject/jiseok_personal/KAIST_IAM_GROUP/gcamextractor/dac_cstorage/img")
     ggsave(plot =graph, file =paste0(i, ".png"),  width =1200, height = 700, units ="px", dpi = 150)  
     
 }

 
 
 
 
 
 
 
 
 
 ### parameter that has more than two original query -> emissGHGBySectorGWPAR5
 
 unique(dac_cstorage_Extended$param)
 
 
 unique(dac_cstorage_Extended$param)[19] -> param_selected
 
 param_selected
 
 dac_cstorage_Extended %>%
     filter(param == param_selected) %>% 
     distinct(units)->unit_param_selected
 
 unit_param_selected
 
 
 ## origQuery 4개로 구분 
 
 dac_cstorage_Extended %>%
     filter(param == param_selected) %>% 
     distinct(origQuery) ->origQuery
 
 origQuery
 
 ## unit은 GHG emissions
 dac_cstorage_Extended %>%
     filter(param == param_selected) %>% 
     distinct(units)
 
 
 ### value(CO2eq) = origValue * coefficient
 ## coefficient 확인 가능
 dac_cstorage_Extended %>%
     filter(param == param_selected) %>%
     mutate(coefficient = value/origValue) %>% 
     relocate(coefficient)

 
 ## origQuery
 
 dac_cstorage_Extended %>%
     filter(param == param_selected & x >= 2015) %>%
     group_by(x, class1, scenario, region,
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
     facet_wrap(region~scenario)+
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