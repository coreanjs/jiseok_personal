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
dac_cstorage<- gcamextractor::readgcam(gcamdatabase = "E:/gcam-v7.0-Windows-Release-Package/output/231011_dac_modified",
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
                                    folder = "gcamextractor/dac_cstorage")

dac_cstorage


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




## Merging two scenarios into a dataframe 
## param : NA -> 'CO2 sequestration by sector' 수정 

dac_cstorage



unique(dac_cstorage$param)

### CO2 sequestration by sector --> NA
dac_cstorage<- dac_cstorage$dataAll %>% 
    mutate(param = ifelse(is.na(param),origQuery, param))


unique(dac_cstorage$scenario)

getwd()




######### 시작


library(stringr)
dac_cstorage_Extended<- read.csv("./gcamextractor/dac_cstorage/gcamDataTable_Extended.csv") %>% 
    mutate(param = ifelse(is.na(param),origQuery, param))




           unique(dac_cstorage_Extended$scenario)
           
           


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

unique(dac_cstorage_Extended$param)[7] -> param_selected

param_selected

dac_cstorage_Extended %>%
    filter(param == param_selected) %>% 
    distinct(units)->unit_param_selected

unit_param_selected

dac_cstorage_Extended %>%
    filter(param == param_selected) %>% 
    distinct(origQuery) ->origQuery

origQuery


unique(dac_cstorage_Extended$scenario)

library(patchwork)
unique(dac_cstorage_Extended$param)[7] -> param_selected
param_selected
a<- dac_cstorage_Extended %>%
    filter(param == param_selected & origQuery== "CO2 emissions by sector (no bio)" & scenario == "dac_ssp2_with_cstorage_same_as_Japan")  %>% 
    group_by(x, class1) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = class1, fill = class1))+
    geom_bar(stat="identity")+
    scale_x_continuous(limits = c (2010, 2100), breaks = seq(2010, 2100, 10))+
    scale_y_continuous(limits = c(-250, 1000), breaks = seq(-250, 1000, 250))+
    theme_minimal()+
    theme_bw()+
    theme(legend.position = c(0.8, 0.7))+
    labs(title ="emissCO2BySectorNoBio",
         y ="MTCO2")

a    
b<- dac_cstorage_Extended %>%
    filter(param == param_selected & origQuery== "CO2 emissions by sector (no bio)"  & scenario == "dac_ssp2_with_cstorage_same_as_Japan") %>% 
    group_by(x, class1) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = class1, fill = class1))+
    geom_bar(stat="identity")+
    scale_x_continuous(limits = c (2010, 2100), breaks = seq(2010, 2100, 20))+
    scale_y_continuous(limits = c(-250, 1000), breaks = seq(-250, 1000, 250))+
    facet_wrap(~class1, nrow =2)+
    theme_minimal()+
    theme_bw()+
    theme(legend.position = "none",
          strip.text = element_text(size = 16))+
    labs( y ="MTCO2")
    
a    
library(patchwork)
a+b+ plot_layout(widths = c(1, 1.7))


ggsave(file ="combo_japan.png",  width =1800, height = 700, units ="px", dpi = 150)






unique(dac_cstorage_Extended$param)[7] -> param_selected
param_selected
a2<- dac_cstorage_Extended %>%
    filter(param == param_selected & origQuery== "CO2 emissions by sector (no bio)" & scenario == "dac_ssp2_without_cstorage")  %>% 
    group_by(x, class1) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = class1, fill = class1))+
    geom_bar(stat="identity")+
    scale_x_continuous(limits = c (2010, 2100), breaks = seq(2010, 2100, 10))+
    scale_y_continuous(limits = c(-250, 1000), breaks = seq(-250, 1000, 250))+
    
    theme_minimal()+
    theme_bw()+
    theme(legend.position = c(0.8, 0.7))+
    labs(title ="emissCO2BySectorNoBio",
         y ="MTCO2")

b2<- dac_cstorage_Extended %>%
    filter(param == param_selected & origQuery== "CO2 emissions by sector (no bio)"  & scenario == "dac_ssp2_without_cstorage") %>% 
    group_by(x, class1) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = class1, fill = class1))+
    geom_bar(stat="identity")+
    scale_x_continuous(limits = c (2010, 2100), breaks = seq(2010, 2100, 20))+
    scale_y_continuous(limits = c(-250, 1000), breaks = seq(-250, 1000, 250))+
    
    facet_wrap(~class1, nrow =2)+
    theme_minimal()+
    theme_bw()+
    theme(legend.position = "none",
          strip.text = element_text(size = 16))+
    labs( y ="MTCO2")

a    
library(patchwork)
a2+b2+ plot_layout(widths = c(1, 1.7))


getwd()
ggsave(file ="combo_without.png",  width =1800, height = 700, units ="px", dpi = 150)















unique(dac_cstorage_Extended$param)[15] -> param_selected
param_selected
dac_cstorage_Extended %>%
    filter(param == param_selected & x >= 2015) %>%
    group_by(x, scenario, sector, region) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = sector, fill = sector))+
    geom_bar(stat='identity')+
   # scale_fill_brewer(palette="Set1")+
    scale_x_continuous(limits = c(2010, 2105), breaks = c(2015, 2050, 2100))+
    facet_grid(~scenario)+
    geom_text(aes(label = round(after_stat(y), 0), group = x), size = 3, 
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
    labs(title = paste("OriginQuery:", origQuery, "\nRegion:South Korea"),
         fill ="구분",
         x = "연도",
         y = unit_param_selected)


ggsave(file =paste0("param_", param_selected, ".png"),  width =1200, height = 700, units ="px", dpi = 150)


origQuery


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
 




unique(dac_cstorage_Extended$param)[15] -> param_selected
param_selected

dac_cstorage_Extended %>%
    filter(param == param_selected & x >= 2015 & region =="South Korea" ) %>%
    group_by(x, scenario, sector) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
    geom_line(linewidth = .5)+
    scale_color_manual(values = c("darkorange", "darkgreen", "red"))+
    scale_x_continuous(limits = c(2010, 2100), breaks = c(2015, 2050, 2100))+
    facet_wrap(~sector, nrow = 2)+
    #gghighlight(sector =="process heat dac")+
    
    theme_bw()+
    theme(plot.title = element_text(size = 24),
          strip.text.x  = element_text(size=10),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 8),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title.position =  "plot",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.position ="top")+
    labs(title = paste( "OriginQuery:", origQuery, "\nRegion: South Korea"),
         fill ="구분",
         x = "연도",
         y = unit_param_selected)





dac_cstorage_Extended %>%
    filter(param == param_selected & x >= 2015 & region =="South Korea" & !sector %in% c('chemical feedstocks', 'refining') ) %>%
    group_by(x, scenario, sector) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
    geom_line(linewidth = .5)+
    scale_x_continuous(limits = c(2010, 2100), breaks = c(2015, 2050, 2100))+
    facet_wrap(~sector, nrow = 2)+
    #gghighlight(sector =="process heat dac")+
    
    theme_bw()+
    theme(plot.title = element_text(size = 24),
          strip.text.x  = element_text(size=10),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 8),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "white"),
          plot.title.position =  "plot",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.position ="top")+
    labs(title = paste( "OriginQuery:", origQuery, "\nRegion: South Korea"),
         fill ="구분",
         x = "연도",
         y = unit_param_selected)



 





 
 seq_by_tech <- data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
                          scenario = c("dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_with_cstorage,date=2023-4-10T21:56:44+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00",
                                       "dac_ssp2_without_cstorage,date=2023-4-10T21:08:56+09:00"),
                            sector = c("H2 central production",
                                       "H2 central production","H2 central production",
                                       "N fertilizer","cement","chemical energy use",
                                       "chemical energy use",
                                       "chemical energy use","chemical energy use",
                                       "chemical feedstocks","construction feedstocks",
                                       "elec_biomass (IGCC CCS)",
                                       "elec_biomass (IGCC CCS)","elec_biomass (IGCC CCS)",
                                       "elec_biomass (conv CCS)",
                                       "elec_biomass (conv CCS)","elec_biomass (conv CCS)",
                                       "elec_biomass (conv CCS)",
                                       "elec_coal (IGCC CCS)","elec_coal (IGCC CCS)",
                                       "elec_coal (IGCC CCS)","elec_coal (conv pul CCS)",
                                       "elec_coal (conv pul CCS)",
                                       "elec_coal (conv pul CCS)","elec_coal (conv pul CCS)",
                                       "elec_gas (CC CCS)",
                                       "elec_gas (CC CCS)","elec_gas (CC CCS)",
                                       "elec_refined liquids (CC CCS)",
                                       "elec_refined liquids (CC CCS)","elec_refined liquids (CC CCS)",
                                       "iron and steel","iron and steel",
                                       "other industrial feedstocks","refining",
                                       "refining","refining","refining",
                                       "refining","refining","H2 central production",
                                       "H2 central production",
                                       "H2 central production","N fertilizer","cement",
                                       "chemical energy use","chemical energy use",
                                       "chemical energy use",
                                       "chemical energy use","chemical feedstocks",
                                       "construction feedstocks","elec_biomass (IGCC CCS)",
                                       "elec_biomass (IGCC CCS)",
                                       "elec_biomass (IGCC CCS)","elec_biomass (conv CCS)",
                                       "elec_biomass (conv CCS)",
                                       "elec_biomass (conv CCS)","elec_biomass (conv CCS)",
                                       "elec_coal (IGCC CCS)",
                                       "elec_coal (IGCC CCS)","elec_coal (IGCC CCS)",
                                       "elec_coal (conv pul CCS)","elec_coal (conv pul CCS)",
                                       "elec_coal (conv pul CCS)",
                                       "elec_coal (conv pul CCS)","elec_gas (CC CCS)",
                                       "elec_gas (CC CCS)","elec_gas (CC CCS)",
                                       "elec_refined liquids (CC CCS)",
                                       "elec_refined liquids (CC CCS)",
                                       "elec_refined liquids (CC CCS)","iron and steel",
                                       "iron and steel","other industrial feedstocks",
                                       "refining","refining","refining",
                                       "refining","refining","refining"),
                        technology = c("biomass to H2 CCS","coal chemical CCS",
                                       "natural gas steam reforming CCS",
                                       "gas CCS","cement CCS","biomass CCS",
                                       "coal CCS","gas CCS","refined liquids CCS",
                                       "refined liquids","refined liquids",
                                       "biomass (IGCC CCS) (dry cooling)",
                                       "biomass (IGCC CCS) (recirculating)",
                                       "biomass (IGCC CCS) (seawater)",
                                       "biomass (conv CCS) (dry cooling)",
                                       "biomass (conv CCS) (once through)",
                                       "biomass (conv CCS) (recirculating)","biomass (conv CCS) (seawater)",
                                       "coal (IGCC CCS) (dry cooling)",
                                       "coal (IGCC CCS) (recirculating)",
                                       "coal (IGCC CCS) (seawater)",
                                       "coal (conv pul CCS) (dry cooling)",
                                       "coal (conv pul CCS) (once through)",
                                       "coal (conv pul CCS) (recirculating)","coal (conv pul CCS) (seawater)",
                                       "gas (CC CCS) (dry cooling)",
                                       "gas (CC CCS) (recirculating)",
                                       "gas (CC CCS) (seawater)",
                                       "refined liquids (CC CCS) (dry cooling)",
                                       "refined liquids (CC CCS) (recirculating)",
                                       "refined liquids (CC CCS) (seawater)","BLASTFUR CCS",
                                       "EAF with DRI CCS","refined liquids",
                                       "FT biofuels CCS level 1","FT biofuels CCS level 2",
                                       "cellulosic ethanol CCS level 1",
                                       "cellulosic ethanol CCS level 2",
                                       "coal to liquids CCS level 1",
                                       "coal to liquids CCS level 2","biomass to H2 CCS",
                                       "coal chemical CCS","natural gas steam reforming CCS",
                                       "gas CCS","cement CCS","biomass CCS",
                                       "coal CCS","gas CCS",
                                       "refined liquids CCS","refined liquids","refined liquids",
                                       "biomass (IGCC CCS) (dry cooling)",
                                       "biomass (IGCC CCS) (recirculating)",
                                       "biomass (IGCC CCS) (seawater)",
                                       "biomass (conv CCS) (dry cooling)",
                                       "biomass (conv CCS) (once through)",
                                       "biomass (conv CCS) (recirculating)",
                                       "biomass (conv CCS) (seawater)","coal (IGCC CCS) (dry cooling)",
                                       "coal (IGCC CCS) (recirculating)",
                                       "coal (IGCC CCS) (seawater)",
                                       "coal (conv pul CCS) (dry cooling)",
                                       "coal (conv pul CCS) (once through)",
                                       "coal (conv pul CCS) (recirculating)",
                                       "coal (conv pul CCS) (seawater)","gas (CC CCS) (dry cooling)",
                                       "gas (CC CCS) (recirculating)",
                                       "gas (CC CCS) (seawater)",
                                       "refined liquids (CC CCS) (dry cooling)",
                                       "refined liquids (CC CCS) (recirculating)",
                                       "refined liquids (CC CCS) (seawater)","BLASTFUR CCS",
                                       "EAF with DRI CCS","refined liquids",
                                       "FT biofuels CCS level 1","FT biofuels CCS level 2",
                                       "cellulosic ethanol CCS level 1",
                                       "cellulosic ethanol CCS level 2",
                                       "coal to liquids CCS level 1",
                                       "coal to liquids CCS level 2"),
                         subsector = c("biomass","coal","gas","gas",
                                       "cement","biomass","coal","gas",
                                       "refined liquids","refined liquids",
                                       "refined liquids","biomass (IGCC CCS)",
                                       "biomass (IGCC CCS)","biomass (IGCC CCS)",
                                       "biomass (conv CCS)","biomass (conv CCS)",
                                       "biomass (conv CCS)","biomass (conv CCS)",
                                       "coal (IGCC CCS)","coal (IGCC CCS)",
                                       "coal (IGCC CCS)","coal (conv pul CCS)",
                                       "coal (conv pul CCS)","coal (conv pul CCS)",
                                       "coal (conv pul CCS)","gas (CC CCS)",
                                       "gas (CC CCS)","gas (CC CCS)",
                                       "refined liquids (CC CCS)","refined liquids (CC CCS)",
                                       "refined liquids (CC CCS)","BLASTFUR",
                                       "EAF with DRI","refined liquids",
                                       "biomass liquids","biomass liquids",
                                       "biomass liquids","biomass liquids",
                                       "coal to liquids","coal to liquids","biomass",
                                       "coal","gas","gas","cement","biomass",
                                       "coal","gas","refined liquids",
                                       "refined liquids","refined liquids",
                                       "biomass (IGCC CCS)","biomass (IGCC CCS)",
                                       "biomass (IGCC CCS)","biomass (conv CCS)",
                                       "biomass (conv CCS)","biomass (conv CCS)",
                                       "biomass (conv CCS)","coal (IGCC CCS)",
                                       "coal (IGCC CCS)","coal (IGCC CCS)",
                                       "coal (conv pul CCS)","coal (conv pul CCS)",
                                       "coal (conv pul CCS)",
                                       "coal (conv pul CCS)","gas (CC CCS)","gas (CC CCS)",
                                       "gas (CC CCS)","refined liquids (CC CCS)",
                                       "refined liquids (CC CCS)",
                                       "refined liquids (CC CCS)","BLASTFUR","EAF with DRI",
                                       "refined liquids","biomass liquids",
                                       "biomass liquids","biomass liquids",
                                       "biomass liquids","coal to liquids",
                                       "coal to liquids"),
                            `1990` = c(0,0,0,0,0,0,0,0,0,4.63486,
                                       0.639134,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,
                                       4.63486,0.639134,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0),
                            `2005` = c(0,0,0,0,0,0,0,0,0,23.301,
                                       0.996854,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0.615342,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,23.301,0.996854,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0.615342,0,0,0,0,0,0),
                            `2010` = c(0,0,0,0,0,0,0,0,0,29.4642,
                                       1.05593,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0.617805,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,29.4642,1.05593,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0.617805,0,0,0,0,0,0),
                            `2015` = c(0,0,0,0,0,0,0,0,0,36.836,
                                       0.968959,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0.572679,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,36.836,0.968959,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0.572679,0,0,0,0,0,0),
                            `2020` = c(0,0,0,0,0,0,0,0,0,37.8157,
                                       1.01028,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0.625721,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,37.8157,1.01028,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0.625722,0,0,0,0,0,0),
                            `2025` = c(0.0148418,0.0241006,0.00513446,
                                       0.00217456,0,0.00913676,0.00106362,
                                       0.00460604,0.00311633,37.9567,1.03252,
                                       0.000351,0.0143736,0.00406959,0.0134311,
                                       0.00257431,0.021534,0.023678,0.00418843,
                                       0.170909,0.0483624,0.0750675,0.0116663,
                                       0.12839,0.143693,0.00267682,0.0913121,
                                       0.0248785,0.00338986,0.100228,
                                       0.0266691,0.529436,0.0300865,0.635486,0,0,
                                       0,0,0,0,0.00000654,0.00268596,
                                       0.000118,0.0000658,0,0.00022,0.0000234,
                                       0.000782,0.000732,37.9384,1.03243,
                                       0.00000544,0.000161,0.0000428,0.0000258,
                                       0.0000193,0.0000337,0.0000344,0.0000314,
                                       0.000933,0.000248,0.000187,0.000127,
                                       0.000249,0.000256,0.0000993,
                                       0.00306515,0.000821,0.000698,0.020045,
                                       0.00530572,0.0650958,0.00330361,0.635592,0,0,
                                       0,0,0,0),
                            `2030` = c(0.1262871,0.1130124,0.04511562,
                                       0.00714406,0,0.04977567,0.00357985,
                                       0.02627196,0.0189863,37.6201,1.05834,
                                       0.001518532,0.058177,0.01637509,0.0440645,
                                       0.0076253,0.0677517,0.0742551,0.01102996,
                                       0.398866,0.1106936,0.1320809,
                                       0.02495606,0.2117517,0.23292,0.00756087,
                                       0.2528748,0.0688284,0.00718529,0.21267875,
                                       0.05666251,1.742974,0.1083115,0.698244,
                                       0,0,0,0,0,0,0.000613,0.024281272,
                                       0.004552876,0.000622,0,0.003712144,
                                       0.00031,0.00818343,0.007282577,37.593,
                                       1.0577,0.0000701,0.002132603,0.000572,
                                       0.000422,0.000236,0.000559,0.000577,
                                       0.000359,0.010690977,0.002855243,
                                       0.001988842,0.001127892,0.002645074,
                                       0.002730174,0.000713,0.02226305,0.005982824,
                                       0.002762382,0.07962791,0.021123717,
                                       0.4688119,0.02716079,0.697544,0,0,0,0,
                                       0,0),
                            `2035` = c(0.10145688,0.1017265,0.05424277,
                                       0.01094496,0.0599734,0.07632792,0.00512017,
                                       0.05921234,0.04791978,36.4454,1.0778,
                                       0.0013504,0.05275923,0.01480358,
                                       0.0137821,0.001986253,0.02417062,0.02688226,
                                       0.003188915,0.1235377,0.03414525,
                                       0.01908229,0.004675209,0.03344332,
                                       0.0367788,0.00641423,0.226633,0.0618339,
                                       0.004286784,0.12622886,0.033592578,
                                       2.010856,0.1096274,0.806458,0.00335526,
                                       0.00258103,0.00718647,0.00787998,0.0000181,
                                       0.00000945,0.006311391,0.04907798,
                                       0.017106412,0.004087746,0.0506918,
                                       0.027541591,0.001707235,0.039061135,
                                       0.034775201,36.4473,1.07755,0.000359,
                                       0.011278338,0.003048828,0.002452637,0.001010153,
                                       0.003309535,0.003456061,0.001183131,
                                       0.035609702,0.009534322,0.006084333,
                                       0.00301316,0.008126144,0.008425696,
                                       0.001908359,0.06060661,0.016340036,
                                       0.003840579,0.111529577,0.029650451,1.0493582,
                                       0.06426675,0.805655,0.0039463,
                                       0.00296438,0.00908681,0.00895656,0.0000176,
                                       0.00000874),
                            `2040` = c(0.22439469,0.1252961,0.08989959,
                                       0.02577858,0.7090779,0.16369936,0.007661181,
                                       0.11105259,0.0943294,34.7732,1.08357,
                                       0.004329549,0.1502295,0.04161639,
                                       0.05319736,0.009129208,0.08120761,
                                       0.08865061,0.005922601,0.19421488,0.0526845,
                                       0.029714414,0.011046017,0.043961298,
                                       0.046836908,0.009428488,0.322492,
                                       0.08778583,0.005531331,0.16287073,0.043406213,
                                       2.0454862,0.12338151,0.916351,
                                       0.25175641,0.2275506,0.2354243,0.70560962,
                                       0.000369,0.000225,0.142071117,0.0844891,
                                       0.059067057,0.020372626,0.742694,
                                       0.126612047,0.004581208,0.093117215,
                                       0.083031117,34.7815,1.08357,0.003312418,
                                       0.10841584,0.029770928,0.035700585,
                                       0.008511503,0.049355535,0.052703662,
                                       0.005249955,0.157422933,0.042217473,
                                       0.025182478,0.011179879,0.033482389,0.034828579,
                                       0.005942265,0.18812287,0.05083515,
                                       0.005551733,0.162147626,0.043197479,
                                       1.508911,0.098036644,0.916053,0.27768012,
                                       0.25479174,0.24787061,0.79484961,
                                       0.000389,0.000242),
                            `2045` = c(1.04969599,0.13586239,0.17715026,
                                       0.05108839,1.1991031,0.31158908,
                                       0.008966978,0.14485591,0.13195528,33.5779,
                                       1.09418,0.009029532,0.3239173,0.09136718,
                                       0.17155506,0.017905357,0.25312263,
                                       0.28160015,0.006598017,0.21350436,
                                       0.05785029,0.029931378,0.010237935,0.043249273,
                                       0.045976309,0.01222888,0.4168986,
                                       0.11361713,0.004948435,0.15025297,
                                       0.040230183,2.412603,0.177579,0.98569,
                                       2.26220297,2.50000065,0.90971163,8.04362849,
                                       0.000731,0.000561,0.994333832,
                                       0.117767195,0.153905984,0.046857565,1.2335604,
                                       0.282750408,0.006530219,0.130126594,
                                       0.12271058,33.5791,1.09423,0.007765727,
                                       0.274794713,0.077527608,0.13379438,
                                       0.013995284,0.194138367,0.216738136,
                                       0.005928634,0.181867865,0.048932115,
                                       0.025720181,0.009877438,0.034589508,
                                       0.036177753,0.008928985,0.28948162,0.078533369,
                                       0.004761359,0.142950771,0.038253761,
                                       2.2271837,0.16754415,0.985819,
                                       2.27412055,2.51345705,0.91558725,8.09096742,
                                       0.000732,0.000563),
                            `2050` = c(1.77736766,0.13208254,0.22501209,
                                       0.06621206,1.3457104,0.41565298,
                                       0.009098111,0.161164992,0.15568966,32.2102,
                                       1.10114,0.011134941,0.4054527,0.1152998,
                                       0.22174545,0.016622501,0.32800652,
                                       0.36941974,0.004252063,0.14909076,
                                       0.04076998,0.017561522,0.004074806,0.027339584,
                                       0.02960805,0.012170032,0.4244164,
                                       0.11608124,0.002754316,0.08961551,
                                       0.02415174,2.894818,0.23667825,1.0361,
                                       3.7842774,4.27560667,1.34129533,13.6989614,
                                       0.00055,0.00044,1.743164289,0.119187061,
                                       0.206298003,0.064127214,1.3781177,
                                       0.401960259,0.007727409,0.15245314,
                                       0.150370497,32.2126,1.10122,0.009861981,
                                       0.356124753,0.10141146,0.183615956,
                                       0.013101551,0.26867682,0.304257343,
                                       0.003629259,0.119571572,0.032427207,
                                       0.013634639,0.003791779,0.019283436,0.020449598,
                                       0.009010539,0.30184331,0.082264059,
                                       0.002559188,0.081883054,0.022052387,
                                       2.891665,0.23691699,1.03615,3.80301195,
                                       4.29837781,1.34804275,13.77760216,
                                       0.000552,0.000442),
                            `2055` = c(1.86788716,0.25823837,0.23694013,
                                       0.061492037,1.7178517,0.526005909,
                                       0.011136236,0.181561136,0.183784259,30.4381,
                                       1.11251,0.009462944,0.3443827,
                                       0.09791817,0.19048669,0.018640759,0.28112977,
                                       0.31616215,0.015763841,0.5406458,
                                       0.14855167,0.09615948,0.006232375,0.15688134,
                                       0.17410763,0.01526194,0.5001971,
                                       0.136151,0.002755423,0.10289146,0.02801851,
                                       2.291933,0.2092742,1.16327,
                                       4.74281314,5.26055062,2.31484864,16.86247426,
                                       0.01097863,0.008375413,1.847332917,
                                       0.218890875,0.218382586,0.061157644,
                                       1.7503457,0.527957294,0.010677719,
                                       0.178529267,0.182421906,30.4448,1.11274,
                                       0.008360617,0.301781125,0.085926221,
                                       0.156578267,0.013810329,0.228788179,0.258853325,
                                       0.009624999,0.298619246,0.08063522,
                                       0.038792705,0.003780543,0.054688535,
                                       0.058214351,0.010396013,0.32963467,
                                       0.089432096,0.002255304,0.08342306,
                                       0.022712691,2.299964,0.21019995,1.16319,
                                       4.78046015,5.30493579,2.33413386,17.01303479,
                                       0.010982384,0.008392632),
                            `2060` = c(1.92234888,0.32974788,0.25397365,
                                       0.060920477,1.6611821,0.573985395,
                                       0.013668382,0.203014124,0.206668072,28.4126,
                                       1.09705,0.009651211,0.3513753,
                                       0.09992087,0.19270354,0.011688425,0.28482873,
                                       0.32076715,0.015751924,0.542033,
                                       0.1491334,0.09461431,0.004935165,0.15463555,
                                       0.17235442,0.01541236,0.5061546,
                                       0.1378779,0.001918473,0.07262529,0.01982019,
                                       2.232849,0.2589352,1.23158,4.77998548,
                                       5.29407558,2.37846767,16.96324547,
                                       0.010765952,0.007419723,1.922752516,
                                       0.294435179,0.239488489,0.06126583,
                                       1.68804208,0.582865627,0.01360117,0.202518783,
                                       0.207223254,28.4191,1.09728,
                                       0.008534623,0.308156249,0.087751441,
                                       0.159010958,0.00918847,0.232575996,0.263392512,
                                       0.009640289,0.300264179,0.08118038,
                                       0.038340926,0.00296925,0.054239131,
                                       0.057964427,0.010510958,0.33396488,
                                       0.090670602,0.001573856,0.05910198,0.01612894,
                                       2.237375,0.2588657,1.23137,4.8151069,
                                       5.3361765,2.39433354,17.10681266,
                                       0.010610207,0.007304468),
                            `2065` = c(1.93291503,0.4012742,0.26163328,
                                       0.053869193,1.46933671,0.555258102,
                                       0.015461007,0.213055688,0.215817606,26.5099,
                                       1.08167,0.010124148,0.3688513,
                                       0.10491643,0.19937547,0.007564114,0.29545806,
                                       0.33348303,0.01571523,0.5449042,
                                       0.15025909,0.09094138,0.003630605,0.1494778,
                                       0.16770651,0.01560246,0.5160935,
                                       0.1408151,0.001584788,0.06033286,0.01650733,
                                       2.077893,0.274533,1.29034,4.78351173,
                                       5.28359471,2.47192823,16.91045299,
                                       0.014535238,0.010149304,1.959464441,
                                       0.37867095,0.25308034,0.054334449,
                                       1.49089565,0.565625572,0.015495037,0.213136841,
                                       0.216900916,26.5153,1.08187,
                                       0.008952155,0.323407777,0.092112018,0.165282839,
                                       0.006136204,0.24218798,0.274715647,
                                       0.009713606,0.305020382,0.082643999,
                                       0.037624312,0.002179717,0.053641738,
                                       0.057697107,0.010688658,0.3418757,
                                       0.09295681,0.001300623,0.049149946,0.013448332,
                                       2.083385,0.2746955,1.29014,4.81808835,
                                       5.32535269,2.4872492,17.05256828,
                                       0.014322541,0.010004305),
                            `2070` = c(1.91827947,0.445467,0.25286393,
                                       0.044695731,1.256420442,0.494319285,
                                       0.016091564,0.209787278,0.210974469,24.8288,
                                       1.06589,0.01087042,0.3960566,0.11266005,
                                       0.21383966,0.006714646,0.31688519,
                                       0.35783029,0.015264766,0.5366351,
                                       0.14838961,0.08292326,0.002764659,0.13790957,
                                       0.15579639,0.01377024,0.4558986,
                                       0.1245534,0.001258048,0.04800375,0.01315805,
                                       1.9485107,0.27649285,1.3354,
                                       4.542000086,4.999056545,2.48783929,15.96637751,
                                       0.018817597,0.013089634,1.98362488,
                                       0.4430684,0.25624987,0.04516382,
                                       1.275601632,0.504235376,0.016138944,
                                       0.209898585,0.212063645,24.8322,1.06602,
                                       0.00960769,0.347118674,0.098870852,
                                       0.177113552,0.00537099,0.259564042,0.294560302,
                                       0.009632523,0.30701919,0.083433467,
                                       0.035670855,0.001664781,0.051631376,
                                       0.055982557,0.010660074,0.3454968,
                                       0.09416543,0.001088497,0.04133693,0.01133052,
                                       1.9550033,0.27705937,1.33539,
                                       4.572456106,5.036424278,2.50165027,16.09315291,
                                       0.018599266,0.012954506),
                            `2075` = c(2.0224023,0.5093024,0.254473,
                                       0.038851595,1.09563902,0.431810684,0.01588023,
                                       0.200190361,0.200588233,23.2687,
                                       1.04922,0.011854931,0.4355816,0.12467109,
                                       0.23797732,0.007637489,0.35262967,
                                       0.40148207,0.015587464,0.5494036,0.15224028,
                                       0.0826179,0.002784875,0.13667792,
                                       0.15484292,0.01066703,0.347797,0.09505085,
                                       0.001396612,0.04956945,0.01357423,
                                       1.814357,0.2696751,1.37116,4.056186526,
                                       4.395888907,2.627410924,13.92724672,
                                       0.030778119,0.02185349,2.074226,
                                       0.5064818,0.2572152,0.039252714,1.11210314,
                                       0.439837665,0.015919008,0.200241885,
                                       0.201442539,23.2696,1.04923,0.010549312,
                                       0.38491842,0.110367305,0.19903632,
                                       0.005986403,0.292185742,0.33479638,
                                       0.010247124,0.329530634,0.089921372,
                                       0.039023951,0.001814601,0.056533738,0.061801575,
                                       0.010651908,0.3472422,0.09489032,
                                       0.00136732,0.0486073,0.01331157,1.820181,
                                       0.2702512,1.37133,4.079619857,
                                       4.425346988,2.63703055,14.02747877,0.03057556,
                                       0.021739103),
                            `2080` = c(2.352344,0.5976014,0.26183644,
                                       0.036954137,1.01788915,0.401768287,
                                       0.015339603,0.189354321,0.192808116,21.7342,
                                       1.03325,0.013252755,0.4978993,0.14451951,
                                       0.27699096,0.010131948,0.41333189,
                                       0.48098294,0.016947544,0.5944935,
                                       0.16490261,0.09019149,0.003175062,0.14750861,
                                       0.16707849,0.010401842,0.3388184,
                                       0.09281993,0.001665276,0.05740586,
                                       0.015731052,1.678399,0.2657522,1.40684,
                                       3.4343821,3.6158191,2.7916638,11.2705826,
                                       0.043256942,0.030661566,2.401723,
                                       0.5955975,0.26339793,0.037275599,1.03036657,
                                       0.407734096,0.015367912,0.189360889,
                                       0.19342126,21.7356,1.0333,0.011938663,
                                       0.447074254,0.130192573,0.237310983,
                                       0.007935145,0.35201802,0.413657426,
                                       0.011606291,0.37449544,0.102566347,
                                       0.046591647,0.002183725,0.067272507,0.074006324,
                                       0.010541092,0.3435222,0.09410001,
                                       0.001669478,0.0575846,0.015779949,1.681258,
                                       0.2659205,1.40693,3.4562707,
                                       3.6427361,2.8008732,11.3605429,0.042995513,
                                       0.030510121),
                            `2085` = c(3.05441,0.7151948,0.27854732,
                                       0.037377624,1.02110651,0.415962392,0.01485211,
                                       0.180232136,0.190962799,20.2453,
                                       1.0183,0.013945189,0.5357474,0.15784229,
                                       0.29715075,0.012409201,0.44746021,
                                       0.53513485,0.015894597,0.5347211,0.1474061,
                                       0.07341913,0.002854335,0.11170863,
                                       0.12436688,0.007758829,0.2525477,
                                       0.06945567,0.001342782,0.04673383,0.012855777,
                                       1.559653,0.2629299,1.4407,2.9846309,
                                       3.0634065,2.8327651,9.348277,
                                       0.046524798,0.032266027,3.07957,0.71231,
                                       0.27872834,0.037571785,1.02849969,0.419003734,
                                       0.014861707,0.180141531,0.191194916,
                                       20.2453,1.01828,0.012936623,0.4971141,
                                       0.146867097,0.269090667,0.010268225,
                                       0.40482537,0.48778863,0.012347041,
                                       0.40030747,0.10987264,0.050224654,
                                       0.002150509,0.07281963,0.08045809,0.007686305,
                                       0.2502541,0.06882059,0.00131333,
                                       0.04577527,0.012592887,1.562058,0.2632318,
                                       1.44088,2.9980958,3.0799049,2.8378904,
                                       9.403208,0.04645719,0.032252217),
                            `2090` = c(3.8891984,0.8204578,0.30582583,
                                       0.038438154,1.04560709,0.448369146,
                                       0.014590321,0.175504736,0.193832206,18.8336,
                                       1.00264,0.016442635,0.6864432,0.21156898,
                                       0.38545799,0.012370626,0.60664072,
                                       0.7820624,0.01609679,0.5193838,
                                       0.14304229,0.07291701,0.003772699,0.10412879,
                                       0.11531056,0.006472075,0.2109804,
                                       0.05858236,0.002353318,0.07560971,0.02073466,
                                       1.408339,0.2569605,1.46449,2.7278913,
                                       2.7680913,2.7490947,8.30187,
                                       0.044874724,0.030155841,3.8604449,0.8177619,
                                       0.30501669,0.038493823,1.04755753,
                                       0.448175542,0.014590709,0.175435959,
                                       0.193746023,18.8308,1.00247,0.01666481,
                                       0.69447462,0.21372767,0.39027943,0.01255511,
                                       0.61308192,0.78777684,0.016215816,
                                       0.5229399,0.14400497,0.07345465,
                                       0.003833969,0.10483303,0.11606112,0.006496658,
                                       0.2117542,0.05879627,0.002386709,
                                       0.07652692,0.020980889,1.408825,0.2570325,
                                       1.4644,2.7312883,2.7723913,2.7492762,
                                       8.3172021,0.045295884,0.0304747),
                            `2095` = c(4.4935803,0.8987983,0.33357725,
                                       0.038978416,1.04762326,0.472629567,
                                       0.01451157,0.174324816,0.198573346,17.5349,
                                       0.986403,0.020973605,0.9231335,0.29061481,
                                       0.5259082,0.013121426,0.8457244,
                                       1.1174919,0.019609475,0.6314687,0.17445074,
                                       0.09520521,0.004928154,0.13488048,
                                       0.14977496,0.007635166,0.2513986,
                                       0.07019255,0.003023599,0.09773693,0.02685558,
                                       1.250766,0.2480342,1.48217,2.585778,
                                       2.6184551,2.6321793,7.7919153,
                                       0.043923784,0.028683788,4.4664415,0.8974763,
                                       0.33319165,0.038967067,1.04680304,
                                       0.471166372,0.014510747,0.174305165,
                                       0.198397612,17.534,0.986346,0.021254549,
                                       0.9321648,0.29287816,0.5305589,0.013565718,
                                       0.8512421,1.1209781,0.019866608,
                                       0.6396977,0.17668665,0.09633357,
                                       0.004996085,0.13651235,0.15153568,0.00764619,
                                       0.2517792,0.0702995,0.00301865,
                                       0.09759854,0.02681789,1.250397,0.2479805,
                                       1.48214,2.5868445,2.6196254,2.6323916,
                                       7.7963197,0.044133963,0.028849698),
                            `2100` = c(5.1285568,0.9868807,0.3643889,
                                       0.038905171,1.02141325,0.485911989,
                                       0.014520031,0.175288432,0.203852489,16.3117,
                                       0.969643,0.024492886,1.1445704,0.36742713,
                                       0.6673596,0.00966775,1.0941197,
                                       1.4693743,0.020617325,0.6689543,0.18595274,
                                       0.10859045,0.005335748,0.15340843,
                                       0.17152477,0.011157629,0.3678052,
                                       0.10262694,0.004236573,0.1391696,0.03827737,
                                       1.102844,0.2378017,1.49673,2.50102223,
                                       2.54045157,2.51680008,7.53012066,
                                       0.043521039,0.028374758,5.117214,0.9868934,
                                       0.3643944,0.038880753,1.02000416,
                                       0.484525476,0.014521226,0.175306812,
                                       0.203715742,16.3122,0.969682,0.024529067,
                                       1.1457624,0.36764671,0.6682079,
                                       0.00965779,1.094862,1.4687695,0.020636983,
                                       0.6696532,0.18615265,0.10875878,
                                       0.005345775,0.15364991,0.17179791,0.01118057,
                                       0.3685719,0.10284138,0.004250381,
                                       0.13960048,0.03839524,1.102235,0.2377006,
                                       1.49673,2.50117146,2.5403993,2.51717251,
                                       7.53007621,0.043481244,0.028353244),
                             Units = c("MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC","MTC","MTC",
                                       "MTC","MTC","MTC","MTC")
                ) %>% 
     mutate(scenario = if_else(str_detect(scenario, "without"), "without csorage", "with cstorage")) %>% 
     select(-Units) %>%   ## Units : MTC
     pivot_longer(-c('scenario', 'sector','subsector', 'technology'), names_to = "year", values_to ="MTC")

 head(seq_by_tech)



 
 unique(seq_by_tech$scenario)

 unique(seq_by_tech$sector)
 
 
 
 ###Count unique values for every column 
 apply(seq_by_tech, 2, function(x) length(unique(x)))
 


 ## https://approximation.tistory.com/56
 ## 
 seq_by_tech %>% 
     group_by(sector) %>% 
     summarise(n_distinct(subsector))

 
 seq_by_tech %>% 
     group_by(sector, subsector) %>%
     summarize(id = n()) %>% distinct()
     
  
 
seq_by_tech %>% 
    ggplot(aes(x = year, y = MTC, group = technology, color = technology))+
    geom_line()+
    facet_wrap(~sector)
  
 


 seq_by_tech %>% 
     filter(sector =="refining") %>% 
     ggplot(aes(x = year, y = MTC, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(subsector~technology)
     

 
 seq_by_tech %>% 
     filter(sector =="chemical feedstocks") %>%  
     ggplot(aes(x = year, y = MTC, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(sector~subsector)
 
 
 seq_by_tech %>% 
     filter(sector =="H2 central production") %>%  
     ggplot(aes(x = year, y = MTC, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(sector~subsector)
 
 
 



 
 
 
 
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
 ## 20231011
 
 
 
 
 
modified<- data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
           scenario = c("Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified",
                        "Same_as_Japan_base_service_modified","Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan","Same_as_Japan","Same_as_Japan","Same_as_Japan",
                        "Same_as_Japan"),
             sector = c("CO2 removal","CO2 removal",
                        "CO2 removal","H2 central production",
                        "H2 central production","H2 central production","N fertilizer",
                        "cement","chemical energy use","chemical energy use",
                        "chemical energy use","chemical energy use",
                        "chemical feedstocks","construction feedstocks",
                        "elec_biomass (IGCC CCS)","elec_biomass (IGCC CCS)",
                        "elec_biomass (IGCC CCS)","elec_biomass (conv CCS)","elec_biomass (conv CCS)",
                        "elec_biomass (conv CCS)","elec_biomass (conv CCS)",
                        "elec_coal (IGCC CCS)","elec_coal (IGCC CCS)",
                        "elec_coal (IGCC CCS)","elec_coal (conv pul CCS)",
                        "elec_coal (conv pul CCS)","elec_coal (conv pul CCS)",
                        "elec_coal (conv pul CCS)","elec_gas (CC CCS)","elec_gas (CC CCS)",
                        "elec_gas (CC CCS)","elec_refined liquids (CC CCS)",
                        "elec_refined liquids (CC CCS)",
                        "elec_refined liquids (CC CCS)","iron and steel","iron and steel",
                        "other industrial feedstocks","process heat dac","refining",
                        "refining","refining","refining","refining","refining",
                        "H2 central production","H2 central production",
                        "H2 central production","N fertilizer","cement",
                        "chemical energy use","chemical energy use",
                        "chemical energy use","chemical energy use","chemical feedstocks",
                        "construction feedstocks","elec_biomass (IGCC CCS)",
                        "elec_biomass (IGCC CCS)","elec_biomass (IGCC CCS)",
                        "elec_biomass (conv CCS)","elec_biomass (conv CCS)",
                        "elec_biomass (conv CCS)","elec_biomass (conv CCS)",
                        "elec_coal (IGCC CCS)","elec_coal (IGCC CCS)","elec_coal (IGCC CCS)",
                        "elec_coal (conv pul CCS)","elec_coal (conv pul CCS)",
                        "elec_coal (conv pul CCS)","elec_coal (conv pul CCS)",
                        "elec_gas (CC CCS)","elec_gas (CC CCS)",
                        "elec_gas (CC CCS)","elec_refined liquids (CC CCS)",
                        "elec_refined liquids (CC CCS)","elec_refined liquids (CC CCS)",
                        "iron and steel","iron and steel",
                        "other industrial feedstocks","refining","refining","refining","refining",
                        "refining","refining"),
         technology = c("hightemp DAC NG",
                        "hightemp DAC elec","lowtemp DAC heatpump","biomass to H2 CCS",
                        "coal chemical CCS","natural gas steam reforming CCS",
                        "gas CCS","cement CCS","biomass CCS","coal CCS",
                        "gas CCS","refined liquids CCS","refined liquids",
                        "refined liquids","biomass (IGCC CCS) (dry cooling)",
                        "biomass (IGCC CCS) (recirculating)",
                        "biomass (IGCC CCS) (seawater)","biomass (conv CCS) (dry cooling)",
                        "biomass (conv CCS) (once through)",
                        "biomass (conv CCS) (recirculating)","biomass (conv CCS) (seawater)",
                        "coal (IGCC CCS) (dry cooling)","coal (IGCC CCS) (recirculating)",
                        "coal (IGCC CCS) (seawater)",
                        "coal (conv pul CCS) (dry cooling)","coal (conv pul CCS) (once through)",
                        "coal (conv pul CCS) (recirculating)",
                        "coal (conv pul CCS) (seawater)","gas (CC CCS) (dry cooling)",
                        "gas (CC CCS) (recirculating)","gas (CC CCS) (seawater)",
                        "refined liquids (CC CCS) (dry cooling)",
                        "refined liquids (CC CCS) (recirculating)","refined liquids (CC CCS) (seawater)",
                        "BLASTFUR CCS","EAF with DRI CCS","refined liquids",
                        "gas CCS","FT biofuels CCS level 1",
                        "FT biofuels CCS level 2","cellulosic ethanol CCS level 1",
                        "cellulosic ethanol CCS level 2","coal to liquids CCS level 1",
                        "coal to liquids CCS level 2","biomass to H2 CCS",
                        "coal chemical CCS","natural gas steam reforming CCS","gas CCS",
                        "cement CCS","biomass CCS","coal CCS","gas CCS",
                        "refined liquids CCS","refined liquids","refined liquids",
                        "biomass (IGCC CCS) (dry cooling)",
                        "biomass (IGCC CCS) (recirculating)","biomass (IGCC CCS) (seawater)",
                        "biomass (conv CCS) (dry cooling)",
                        "biomass (conv CCS) (once through)","biomass (conv CCS) (recirculating)",
                        "biomass (conv CCS) (seawater)","coal (IGCC CCS) (dry cooling)",
                        "coal (IGCC CCS) (recirculating)",
                        "coal (IGCC CCS) (seawater)","coal (conv pul CCS) (dry cooling)",
                        "coal (conv pul CCS) (once through)",
                        "coal (conv pul CCS) (recirculating)","coal (conv pul CCS) (seawater)",
                        "gas (CC CCS) (dry cooling)","gas (CC CCS) (recirculating)",
                        "gas (CC CCS) (seawater)",
                        "refined liquids (CC CCS) (dry cooling)","refined liquids (CC CCS) (recirculating)",
                        "refined liquids (CC CCS) (seawater)","BLASTFUR CCS",
                        "EAF with DRI CCS","refined liquids",
                        "FT biofuels CCS level 1","FT biofuels CCS level 2",
                        "cellulosic ethanol CCS level 1","cellulosic ethanol CCS level 2",
                        "coal to liquids CCS level 1","coal to liquids CCS level 2"),
          subsector = c("dac","dac","dac",
                        "biomass","coal","gas","gas","cement","biomass","coal",
                        "gas","refined liquids","refined liquids",
                        "refined liquids","biomass (IGCC CCS)","biomass (IGCC CCS)",
                        "biomass (IGCC CCS)","biomass (conv CCS)",
                        "biomass (conv CCS)","biomass (conv CCS)","biomass (conv CCS)",
                        "coal (IGCC CCS)","coal (IGCC CCS)","coal (IGCC CCS)",
                        "coal (conv pul CCS)","coal (conv pul CCS)",
                        "coal (conv pul CCS)","coal (conv pul CCS)","gas (CC CCS)",
                        "gas (CC CCS)","gas (CC CCS)","refined liquids (CC CCS)",
                        "refined liquids (CC CCS)","refined liquids (CC CCS)",
                        "BLASTFUR","EAF with DRI","refined liquids","gas CCS",
                        "biomass liquids","biomass liquids","biomass liquids",
                        "biomass liquids","coal to liquids","coal to liquids",
                        "biomass","coal","gas","gas","cement","biomass",
                        "coal","gas","refined liquids","refined liquids",
                        "refined liquids","biomass (IGCC CCS)","biomass (IGCC CCS)",
                        "biomass (IGCC CCS)","biomass (conv CCS)",
                        "biomass (conv CCS)","biomass (conv CCS)","biomass (conv CCS)",
                        "coal (IGCC CCS)","coal (IGCC CCS)","coal (IGCC CCS)",
                        "coal (conv pul CCS)","coal (conv pul CCS)",
                        "coal (conv pul CCS)","coal (conv pul CCS)","gas (CC CCS)",
                        "gas (CC CCS)","gas (CC CCS)","refined liquids (CC CCS)",
                        "refined liquids (CC CCS)","refined liquids (CC CCS)",
                        "BLASTFUR","EAF with DRI","refined liquids",
                        "biomass liquids","biomass liquids","biomass liquids",
                        "biomass liquids","coal to liquids","coal to liquids"),
             `1990` = c(0,0,0,0,0,0,0,0,0,0,
                        0,0,4.63486,0.639134,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,4.63486,0.639134,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0),
             `2005` = c(0,0,0,0,0,0,0,0,0,0,
                        0,0,23.301,0.996854,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0.615342,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,23.301,0.996854,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0.615342,0,0,0,0,0,0),
             `2010` = c(0,0,0,0,0,0,0,0,0,0,
                        0,0,29.4642,1.05593,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0.617805,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,29.4642,1.05593,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0.617805,0,0,0,0,0,0),
             `2015` = c(0,0,0,0,0,0,0,0,0,0,
                        0,0,36.836,0.968959,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0.572679,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,36.836,0.968959,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0.572679,0,0,0,0,0,0),
             `2020` = c(0,0,0,0,0,0,0,0,0,0,
                        0,0,37.8157,1.01028,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0.625721,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,37.8157,1.01028,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0.625721,0,0,0,0,0,0),
             `2025` = c(8.74e-06,2.13e-08,1.97e-08,
                        0.025686,0.0303485,0.00653496,0.00298544,0,
                        0.0144475,0.00171051,0.0055009,0.00359276,37.9721,1.03258,
                        0.000406,0.0196255,0.00574623,0.0232692,0.00253013,
                        0.0415204,0.0475216,0.00558229,0.265254,0.0774076,
                        0.124469,0.0110594,0.240254,0.280906,0.00363592,
                        0.125964,0.0344142,0.0038697,0.114831,0.0305753,
                        0.697846,0.0400866,0.635451,3.3e-06,0,0,0,0,0,0,
                        0.0256861,0.0303486,0.00653497,0.00298545,0,0.0144475,
                        0.00171051,0.0055009,0.00359277,37.9721,1.03258,
                        0.000406,0.0196255,0.00574625,0.0232693,0.00253014,
                        0.0415206,0.0475218,0.00558231,0.265255,0.0774079,
                        0.124469,0.0110595,0.240254,0.280907,0.00363593,
                        0.125964,0.0344143,0.00386971,0.114831,0.0305753,0.69785,
                        0.0400868,0.635451,0,0,0,0,0,0),
             `2030` = c(0.000148,6.57e-07,1.12e-06,
                        0.8084525,0.2931004,0.10828979,0.0158158,0,
                        0.1929158,0.01133436,0.04191783,0.02815827,37.7583,1.06049,
                        0.000988,0.1139786,0.04516273,0.1112726,
                        0.002752121,0.3831174,0.6622456,0.02655209,1.154318,0.3342841,
                        0.466634,0.0371366,0.836766,0.973006,0.01670231,
                        0.577033,0.1583294,0.010991861,0.3317167,0.0886771,
                        3.438874,0.2218987,0.700082,5.25e-05,0.00293391,
                        0.00331246,0.00205369,0.0123904,0.000155,0.000168,
                        0.8084356,0.2930955,0.10828879,0.01581571,0,0.1929158,
                        0.01133436,0.04191793,0.02815827,37.7583,1.06049,
                        0.000988,0.1139719,0.04516835,0.1112778,0.0027514,
                        0.3830375,0.6623128,0.02655201,1.154294,0.3342904,
                        0.466658,0.0370852,0.836765,0.973067,0.01670232,0.577026,
                        0.1583305,0.01099186,0.3317135,0.08867721,3.438888,
                        0.2218998,0.700077,0.00293292,0.00331134,
                        0.00205301,0.0123862,0.000155,0.000168),
             `2035` = c(0.001601127,1.06e-05,
                        3.28e-05,2.8213522,0.7551216,0.29706023,0.05167261,
                        0.859481,0.7672623,0.03693428,0.15570027,0.11018324,
                        36.6806,1.08276,0.001071654,0.199693,0.13566453,
                        0.1275016,0.002752894,0.5986294,1.9691456,0.0493688,
                        2.100279,0.6087459,0.780488,0.0534212,1.370374,1.598601,
                        0.03156539,1.092917,0.3008477,0.012961475,0.4076718,
                        0.10954709,6.621088,0.4730691,0.814054,0.000534,
                        0.18402029,0.21825263,0.09724204,0.7996395,0.010061543,
                        0.011539975,2.8215762,0.7551326,0.29706667,
                        0.05167321,0.859487,0.7672863,0.03693429,0.15570037,
                        0.11018394,36.6806,1.08276,0.001071593,0.199651,0.13570775,
                        0.1274843,0.002752177,0.5983566,1.9695328,
                        0.04936932,2.100263,0.6087572,0.780521,0.0533622,1.370378,
                        1.598688,0.03156579,1.09292,0.3008517,0.012961705,
                        0.4076773,0.10954929,6.6211,0.4730682,0.814053,
                        0.1840473,0.21828651,0.09725396,0.7997643,0.010062451,
                        0.01154112),
             `2040` = c(0.023851903,0.000247,
                        0.001453792,4.1602088,1.2221222,0.50363362,0.0970797,
                        2.236453,1.2916979,0.06663989,0.31371156,0.2224652,
                        34.9464,1.09142,0.00287103,0.3616935,0.19606531,
                        0.2955251,0.003209827,1.0185802,2.6372246,0.07376741,
                        3.089351,0.8970441,1.069808,0.06164075,1.849388,2.16638,
                        0.04968455,1.721797,0.4755843,0.014489381,0.4693397,
                        0.12669485,9.157863,0.71885594,0.931955,0.0073935,
                        0.49618266,0.60354627,0.2014413,2.1523007,
                        0.033824827,0.04151295,4.1654239,1.2226197,0.50385315,
                        0.097089,2.236558,1.2923469,0.0666348,0.31370356,
                        0.2224828,34.9467,1.09143,0.002864169,0.3616207,0.19617363,
                        0.2953138,0.003203482,1.0187345,2.6394039,
                        0.07377021,3.089373,0.8970652,1.069851,0.06158356,1.849379,
                        2.166447,0.04968963,1.721955,0.4756321,0.014493348,
                        0.4694671,0.12672991,9.158064,0.71882714,0.931954,
                        0.49685467,0.60444615,0.20160452,2.1555546,
                        0.033828426,0.041521795),
             `2045` = c(0.55621698,0.009637283,
                        0.100185438,8.001081,1.7677781,0.74055392,0.12693933,
                        2.711542,1.61849074,0.078360925,0.40212012,0.29571992,
                        33.6391,1.10004,0.003342236,0.4794429,0.26841683,
                        0.3609121,0.003217701,1.3104163,3.5530055,0.09018245,
                        3.712715,1.077454,1.225997,0.05525581,2.094188,
                        2.455789,0.06340269,2.196793,0.6083822,0.015724897,
                        0.530447,0.14411258,11.259338,0.974857,1.01197,
                        0.159224,1.66245768,2.18445307,0.42078518,7.6352142,
                        0.079341705,0.106017446,8.275587,1.7780821,0.74215584,
                        0.12696693,2.711924,1.62596676,0.078306127,0.40195512,
                        0.29607391,33.6475,1.10022,0.003227536,0.47313,
                        0.27264035,0.3481307,0.003211471,1.2862035,3.6112838,
                        0.09000097,3.705414,1.0753273,1.223947,0.05504607,
                        2.090831,2.451869,0.06326754,2.192119,0.607079,
                        0.015759889,0.531858,0.14450298,11.259279,0.9737055,
                        1.01277,1.6915087,2.22830096,0.42423948,7.7932381,
                        0.079030742,0.105731187),
             `2050` = c(2.31941894,0.060872303,
                        1.070062261,10.9502515,2.1271648,0.86417799,0.139042596,
                        2.750767,1.73862229,0.080631851,0.44359269,
                        0.338510919,32.2501,1.105,0.003481873,0.5321655,0.31533253,
                        0.3800582,0.003139538,1.4117952,4.0890865,
                        0.09728199,3.982354,1.1564859,1.2872538,0.04444902,2.189085,
                        2.571682,0.06909906,2.395715,0.664579,0.014436077,
                        0.5082663,0.13898167,12.619241,1.200311,1.06637,
                        0.621003,2.87455977,3.85246236,0.62189309,13.2881297,
                        0.127011122,0.175377042,12.5233586,2.1812932,
                        0.85601637,0.138763581,2.752701,1.766902,0.080430561,
                        0.44283459,0.339945715,32.2835,1.10589,0.003232912,
                        0.4864085,0.34718435,0.34872571,0.003163252,1.29591291,
                        4.2376018,0.09612998,3.938063,1.1435189,1.2754295,
                        0.04356288,2.1706501,2.54977,0.06815155,2.363412,
                        0.6555218,0.01444114,0.5097274,0.1394025,12.61611,
                        1.1939366,1.07187,2.99707883,4.0446483,0.63375579,
                        13.9735828,0.124274688,0.172274854),
             `2055` = c(0.010405613,0.000388,
                        0.90941605,11.159056,2.1146849,0.8094692,0.117852424,
                        2.995346,1.691382385,0.074707683,0.432779191,0.34454604,
                        30.5003,1.11277,0.002890852,0.4418339,0.26180705,
                        0.3155465,0.002645419,1.1721541,3.3949901,0.08103967,
                        3.315117,0.9625671,1.0771344,0.03807266,1.82943,
                        2.147599,0.05710738,1.981943,0.5499307,0.009834204,
                        0.3616243,0.09931683,6.48309,0.6197843,1.17995,
                        0.0027279,4.07619569,5.21879118,1.51227705,17.8362501,
                        0.205545397,0.255266726,11.6558875,2.05085141,
                        0.81290122,0.117217565,2.99123,1.708038286,0.074518583,
                        0.432038311,0.34519744,30.4837,1.11228,0.002682321,
                        0.4035682,0.28805541,0.289333774,0.002622612,1.07520617,
                        3.5158984,0.08006611,3.277316,0.9514787,1.0677001,
                        0.0380698,1.8144755,2.1295881,0.05635024,1.955602,
                        0.5425207,0.009486849,0.3496552,0.0960367,6.503034,
                        0.6181028,1.17868,4.11919988,5.31479927,1.47692158,
                        18.1945286,0.201463116,0.250469888),
             `2060` = c(0.005652524,0.000246,
                        0.003822715,10.40459201,2.0192206,0.8584324,0.101411346,
                        2.694799,1.474711294,0.065524383,0.406365087,
                        0.33511459,28.444,1.09752,0.002980837,0.4555892,0.26995823,
                        0.3253526,0.00221507,1.2085823,3.5005976,0.08345297,
                        3.414756,0.9916152,1.107526,0.03044754,1.8816238,
                        2.209986,0.05857719,2.035124,0.5648915,0.007074171,
                        0.2666621,0.07353115,5.389758,0.6200298,1.24196,
                        0.00148184,4.16422796,5.30542723,1.61133884,18.09150919,
                        0.218699611,0.267328806,10.74461787,1.90778799,
                        0.84570249,0.100408654,2.672948,1.470414264,0.065382654,
                        0.406202797,0.33489979,28.4216,1.09693,0.002774379,
                        0.4174234,0.29794707,0.299225605,0.002076477,
                        1.11197081,3.6363573,0.08271625,3.386631,0.9833234,
                        1.1015145,0.030555808,1.8724483,2.1986718,0.05798165,
                        2.014353,0.5590239,0.006706128,0.2535289,0.06991808,
                        5.403855,0.6214361,1.24238,4.20289846,5.39494767,
                        1.5767405,18.42700509,0.216064943,0.263798364),
             `2065` = c(0.004341093,0.000232,
                        0.003256573,9.708504,1.9708814,0.8144638,0.08257468,
                        2.3515995,1.218600093,0.055959162,0.366012672,0.311530338,
                        26.5349,1.08219,0.003169353,0.4844108,0.28704013,
                        0.3458227,0.001791247,1.2846515,3.7213772,0.08848922,
                        3.62294,1.0522974,1.1705662,0.024155171,1.9902095,
                        2.339714,0.06149073,2.142261,0.595027,0.006041674,
                        0.2303205,0.06367562,4.129355,0.5834976,1.30048,
                        0.00113804,4.17374632,5.283861277,1.705425344,
                        17.94372632,0.233639206,0.278883368,9.9210339,1.85152211,
                        0.79270678,0.081325449,2.3139742,1.199155183,
                        0.055852552,0.366221573,0.310653646,26.5222,1.08187,
                        0.002960456,0.4454349,0.31794872,0.319134482,0.001664465,
                        1.18601471,3.8793018,0.08802653,3.606159,1.047294,
                        1.1684192,0.024234916,1.9876716,2.3361752,0.06107127,
                        2.127695,0.5908793,0.005821966,0.2223434,0.06147308,
                        4.122472,0.5838129,1.3011,4.20905014,5.367524073,
                        1.673100919,18.25816756,0.232819399,0.27693642),
             `2070` = c(0.004713845,0.000321,
                        0.004023647,8.865147,1.8877996,0.7271576,0.067821893,
                        2.08056986,0.996736814,0.048272762,0.323128024,
                        0.281375654,24.8508,1.0662,0.003393501,0.51867,0.30734024,
                        0.3702868,0.001741937,1.3755178,3.9846077,0.09421352,
                        3.861888,1.1221495,1.2382204,0.01894469,2.108651,
                        2.483165,0.06111812,2.138749,0.595099,0.005136697,
                        0.1972554,0.0546598,3.471303,0.541955,1.34409,
                        0.00123576,3.976444243,4.989037996,1.748709898,16.84051362,
                        0.237629799,0.275858527,8.9194442,1.75503521,
                        0.69886512,0.0666617,2.03763468,0.966363293,0.048233711,
                        0.323676154,0.279987417,24.8433,1.06599,0.003182236,
                        0.4787986,0.34176198,0.343094021,0.001672075,
                        1.27502378,4.1702311,0.09407495,3.858609,1.1210652,
                        1.240482,0.018986318,2.1137478,2.488654,0.06086799,
                        2.130231,0.5926369,0.005003594,0.1923832,0.0533103,
                        3.456662,0.541622,1.34462,4.003255296,5.059477243,
                        1.716498665,17.10792443,0.237759437,0.274763995),
             `2075` = c(0.004787935,0.000371,
                        0.004399895,7.774029,1.7914214,0.6309218,0.059249325,
                        1.8651053,0.822244444,0.042581087,0.289431475,
                        0.255545471,23.2695,1.04843,0.004310632,0.5838325,0.33457709,
                        0.4174557,0.001714659,1.4800452,4.2016058,
                        0.09812756,4.020902,1.16945,1.2705151,0.016298321,2.1643468,
                        2.5546064,0.05117775,1.792961,0.5004579,0.004638136,
                        0.172766,0.04789682,3.15861,0.5096076,1.37953,
                        0.00125518,3.65908497,4.4688181,1.90799924,14.8803573,
                        0.257775316,0.281885808,7.7729579,1.67687921,
                        0.60449717,0.058336271,1.8251595,0.790297164,0.042603688,
                        0.290200765,0.254159563,23.2668,1.04838,0.004242979,
                        0.5489302,0.37039268,0.393979094,0.001713544,
                        1.37827456,4.3726875,0.09747867,3.9952767,1.1619989,
                        1.2641021,0.016402189,2.1539538,2.542089,0.05062185,
                        1.7730176,0.4948346,0.004743257,0.1755974,0.04866102,
                        3.140425,0.5079574,1.3795,3.6713819,4.5132267,
                        1.87989844,15.0516164,0.25944883,0.282322893),
             `2080` = c(0.004187022,0.000326,
                        0.003940202,6.927169,1.7302177,0.5495178,0.054942714,
                        1.68359772,0.700323371,0.038275558,0.266040016,
                        0.239074136,21.6995,1.03047,0.00607661,0.68631,0.37132259,
                        0.4880347,0.001771965,1.6055165,4.3980445,0.10137976,
                        4.1448567,1.2065758,1.2898348,0.016759704,2.1942253,
                        2.5938169,0.03940156,1.3757138,0.3852306,
                        0.00438402,0.1580802,0.04381189,2.81756,0.4847921,1.41497,
                        0.00109765,3.1982226,3.7254458,2.08636451,12.1195797,
                        0.2909945,0.2959734,6.9785286,1.66134287,0.53281226,
                        0.054341405,1.65650892,0.678673205,0.038319856,
                        0.266695666,0.238229601,21.7018,1.03057,0.00595733,
                        0.6502616,0.40702971,0.464062541,0.00180968,1.50409784,
                        4.5720477,0.1006795,4.1173514,1.1985685,1.2828893,
                        0.016854851,2.1830549,2.5803778,0.03879638,1.3542281,
                        0.3791776,0.00439517,0.15853838,0.04394301,
                        2.806361,0.4835149,1.41456,3.1915012,3.7327259,2.06579888,
                        12.1513813,0.29352426,0.29759331),
             `2085` = c(0.00561885,0.000428,
                        0.005364639,7.220249,1.950263,0.5404804,0.052909417,
                        1.5803636,0.661181373,0.034715805,0.247901706,0.231177907,
                        20.225,1.01531,0.006767367,0.7558711,0.4028285,
                        0.523227,0.000699,1.6882168,4.591537,0.09814796,
                        3.970398,1.1562045,1.1819577,0.014968766,1.9818978,
                        2.3469719,0.02397018,0.831227,0.2337453,0.003582162,
                        0.12806888,0.03558206,2.492521,0.4554951,1.44889,
                        0.00147301,2.8315446,3.1486837,2.1756577,9.924296,
                        0.30989665,0.30093486,7.3357366,1.91951267,0.53321913,
                        0.052620296,1.56874392,0.652887799,0.034762512,
                        0.248308539,0.230984442,20.2284,1.01545,0.006598818,0.7189167,
                        0.4388139,0.498122189,0.000697,1.58656743,4.771148,
                        0.09743255,3.9422911,1.1480183,1.1748943,
                        0.015020002,1.9705114,2.3332864,0.02333146,0.8087007,
                        0.2274083,0.003578796,0.12787215,0.03553101,2.489405,
                        0.4551173,1.44874,2.8182566,3.1369507,2.1647378,9.882996,
                        0.31233808,0.30290583),
             `2090` = c(0.005533537,0.000357,
                        0.004976206,8.0349525,2.1631404,0.5650812,0.051552485,
                        1.50982095,0.658057657,0.031943816,0.235539831,
                        0.228407318,18.8264,0.999662,0.008996363,0.9257732,
                        0.4807645,0.5926908,0.000654,1.7097483,4.744738,0.0849852,
                        3.3673446,0.9827479,0.9182348,0.012658843,1.5026893,
                        1.789172,0.01526969,0.5234851,0.1480387,0.00460126,
                        0.15243298,0.04215067,2.173677,0.4273516,1.47553,
                        0.00145065,2.5765532,2.7906472,2.13452497,8.557673,
                        0.31222339,0.29652218,8.0542966,2.17870436,
                        0.567947905,0.051490388,1.50890081,0.658835064,0.03199726,
                        0.235811972,0.228680453,18.8281,0.999736,0.00875447,
                        0.8883148,0.5179667,0.565903098,0.000641,1.60890899,
                        4.939174,0.08427226,3.3394251,0.9746162,0.9110485,
                        0.012670676,1.4911509,1.7753257,0.01474145,0.5050109,
                        0.1428491,0.004578528,0.1514661,0.04188549,2.173579,
                        0.4273954,1.47561,2.5704234,2.7840787,2.1309492,
                        8.533422,0.31376747,0.29793776),
             `2095` = c(0.004705162,0.00027,
                        0.004140668,9.0270753,2.3363923,0.60638996,0.050309858,
                        1.44414388,0.660537322,0.029884968,0.227356284,
                        0.228355668,17.5492,0.984242,0.011919698,1.0983018,
                        0.4985661,0.7289517,0.000753,1.8334291,4.099527,0.06873946,
                        2.660329,0.7777387,0.6653484,0.009664937,1.0594933,
                        1.2679855,0.01319648,0.4501914,0.12757,0.00476888,
                        0.1577625,0.04366895,1.893157,0.402409,1.49678,
                        0.00123348,2.427059,2.6049382,2.0569715,7.8760992,
                        0.30376817,0.28590056,9.08441,2.34417326,0.60664634,
                        0.050339008,1.44666431,0.66365018,0.029950671,
                        0.227676669,0.228809529,17.5466,0.984133,0.011721456,
                        1.0588515,0.5337356,0.70189599,0.000741,1.72822757,4.275029,
                        0.06797299,2.6305312,0.7690671,0.657704,
                        0.009634312,1.0472862,1.2533685,0.01337499,0.4562977,
                        0.1293088,0.0048037,0.1591006,0.04404541,1.894295,0.4028574,
                        1.49655,2.426086,2.6038562,2.0562704,7.8712584,
                        0.30458071,0.28675916),
             `2100` = c(0.003960951,0.000213,
                        0.003456565,9.866681,2.4713535,0.6477739,0.049035293,
                        1.37349357,0.657635844,0.028214873,0.221641239,
                        0.229347121,16.333,0.967641,0.013362582,1.1877154,0.5371623,
                        0.7072224,0.000459,1.7257875,3.988076,0.04985542,
                        1.8714352,0.5474802,0.4233864,0.006542471,0.6505976,
                        0.7818996,0.01665739,0.568692,0.1610056,0.005816107,
                        0.1950272,0.0540212,1.648027,0.3784041,1.51275,
                        0.00103838,2.32245367,2.49093105,1.96913428,7.4791973,
                        0.290231063,0.271997038,9.869927,2.4795677,0.6497579,
                        0.049069382,1.37527694,0.659866157,0.028285594,
                        0.222034949,0.22980662,16.3307,0.967537,0.013259523,
                        1.1499192,0.5719961,0.681847272,0.000464,1.61992039,
                        4.154227,0.04911197,1.8426423,0.5391151,0.4158031,
                        0.006467297,0.6385433,0.7675317,0.01686126,0.5756702,
                        0.1629912,0.005871681,0.1969521,0.05455562,1.648466,
                        0.3787848,1.5125,2.32759502,2.49759836,1.97082632,
                        7.5004207,0.290485699,0.272394329)
 )%>% 
     pivot_longer(-c('scenario','sector','technology','subsector'), names_to = "year", values_to ="MTC") %>% 
     mutate(year = as.numeric(year))
 

 
 
 modified %>% 
     filter(subsector =="dac") %>% 
     ggplot(aes(x = year, y =MTC, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(~technology)
 