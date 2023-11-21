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
GGS621_cap_tax <- gcamextractor::readgcam(gcamdatabase = "E:/gcam-v7.0-Windows-Release-Package_GGS621/output/Assignment1004",
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
 
 
 unique(Extended$param)[15]
 
 Extended %>% 
     filter(region =="Global" & param == "emissCO2BySector") %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(~class1)
 
 str(Extended)
 
 ############## 과제용 231107
 Extended %>% 
     filter(origQuery == "CO2 sequestration by sector") %>% 
     select(scenario, x, value, sector) %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(~sector, scales="free_y")+
     scale_color_manual(values = c('blue', 'red'))+
     theme_bw()+
     labs(title ="Plot the negative emission sectors(scales=free_y)",
          x = "year",
          y = "CO2 emissions - (MTCO2)")+
     theme(plot.title = element_text(size = 20),
           strip.text.x  = element_text(size=12),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x = element_blank(),
           # panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           plot.background = element_rect(fill = "white"),
           plot.title.position =  "plot",
           legend.position ="top")
 
 unique(Extended$param)
 
 Extended %>% 
     filter(origQuery == "CO2 sequestration by sector" & sector!='chemical feedstocks') %>% 
     select(scenario, x, value, sector) %>% 
     ggplot(aes(x = x, y = value, group = scenario, color = scenario))+
     geom_line()+
     facet_wrap(~sector)+
     scale_color_manual(values = c('blue', 'red'))+
     theme_bw()+
     labs(title ="Plot the negative emission sectors(y-scales are the same)",
          x = "year",
          y = "CO2 emissions - (MTCO2)")+
     theme(plot.title = element_text(size = 20),
           strip.text.x  = element_text(size=12),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x = element_blank(),
           # panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           plot.background = element_rect(fill = "white"),
           plot.title.position =  "plot",
           legend.position ="top")
 
 Extended %>% 
     filter(origQuery == "CO2 sequestration by sector" & x >=2020) %>% 
     select(scenario, x, value, sector) %>% 
     ggplot(aes(x = x, y = value, group = sector, fill = sector))+
     geom_col()+
   #  scale_fill_brewer(palette ="Set1")+
     facet_wrap(~scenario)
 
 Extended %>% 
     filter(region =="Global" & param == "emissCO2BySector" ) %>% 
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
 
 ### 과제 231107
  aggParam
  
 
  
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
 dir.create("C:/R/Rproject/jiseok_personal/GGS621/gcamextractor/img_aggParam_Assignment4")
 
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










Assign6<-data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
          scenario = c("capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00","capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00","capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00","capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_NZ2060,date=2023-27-9T15:58:34+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00","capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00",
                       "capuct_Reference,date=2023-27-9T17:00:44+09:00","capuct_Reference,date=2023-27-9T17:00:44+09:00"),
            region = c("South Korea","South Korea",
                       "South Korea","South Korea","South Korea","South Korea",
                       "South Korea","South Korea","South Korea",
                       "South Korea","South Korea","South Korea","South Korea",
                       "South Korea","South Korea","South Korea","South Korea",
                       "South Korea","South Korea","South Korea","South Korea",
                       "South Korea","South Korea","South Korea","South Korea",
                       "South Korea","South Korea","South Korea",
                       "South Korea","South Korea"),
            sector = c("H2 central production",
                       "N fertilizer","cement","chemical energy use",
                       "chemical feedstocks","construction feedstocks",
                       "elec_biomass (IGCC CCS)","elec_biomass (conv CCS)","elec_coal (IGCC CCS)",
                       "elec_coal (conv pul CCS)","elec_gas (CC CCS)",
                       "elec_refined liquids (CC CCS)","iron and steel",
                       "other industrial feedstocks","refining","H2 central production",
                       "N fertilizer","cement","chemical energy use",
                       "chemical feedstocks","construction feedstocks",
                       "elec_biomass (IGCC CCS)","elec_biomass (conv CCS)",
                       "elec_coal (IGCC CCS)","elec_coal (conv pul CCS)","elec_gas (CC CCS)",
                       "elec_refined liquids (CC CCS)","iron and steel",
                       "other industrial feedstocks","refining"),
            `1990` = c(0,0,0,0,4.63486,0.639134,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,4.63486,0.639134,
                       0,0,0,0,0,0,0,0,0),
            `2005` = c(0,0,0,0,23.301,0.996854,0,
                       0,0,0,0,0,0,0.615342,0,0,0,0,0,23.301,
                       0.996854,0,0,0,0,0,0,0,0.615342,0),
            `2010` = c(0,0,0,0,29.4642,1.05593,0,
                       0,0,0,0,0,0,0.617805,0,0,0,0,0,29.4642,
                       1.05593,0,0,0,0,0,0,0,0.617805,0),
            `2015` = c(0,0,0,0,36.836,0.968959,0,
                       0,0,0,0,0,0,0.572679,0,0,0,0,0,36.836,
                       0.968959,0,0,0,0,0,0,0,0.572679,0),
            `2020` = c(0,0,0,0,37.8204,1.01029,0,
                       0,0,0,0,0,0,0.625741,0,0,0,0,0,37.8204,
                       1.01029,0,0,0,0,0,0,0,0.625741,0),
            `2025` = c(0.006668477,0.0000593,0,
                       0.002217596,37.8501,1.04248,0.000192,0.000124,0.00063,
                       0.000589,0.003244648,0.025693554,0.2073924,0.679697,
                       0.003636057,0.006668477,0.0000593,0,0.002217596,
                       37.8501,1.04248,0.000192,0.000124,0.00063,0.000589,
                       0.003244648,0.025693554,0.2073924,0.679697,0.003636057),
            `2030` = c(0.052575308,0.000792,0,
                       0.024341524,37.5245,1.06425,0.001379824,0.001072708,
                       0.006670633,0.005437702,0.015692353,0.054792331,0.75486628,
                       0.723695,0.003526363,0.042007819,0.000266,0.000199,
                       0.016324631,37.4356,1.06992,0.000385,0.000235,
                       0.00136254,0.00106338,0.006841122,0.031851403,0.59060022,
                       0.754805,0.003204197),
            `2035` = c(0.118924895,0.002599774,
                       0.0214891,0.102650754,36.3426,1.08631,0.003227708,
                       0.002120343,0.0146716,0.010488083,0.031556318,0.067244418,
                       1.47189041,0.844867,0.053067695,0.083901347,0.000957,
                       0.00247136,0.066262821,36.2707,1.09197,0.000653,
                       0.00036,0.002297522,0.001610579,0.010850638,0.032227698,
                       1.03357742,0.874395,0.046722462),
            `2040` = c(0.223564198,0.009239686,
                       0.1829132,0.257994068,34.6239,1.09425,0.022039124,
                       0.020177659,0.08348828,0.058401032,0.101879866,0.133115236,
                       2.05596306,0.955479,0.72831151,0.126185828,
                       0.001933265,0.006051271,0.135535247,34.5708,1.10476,
                       0.001080525,0.000546,0.003565378,0.002330548,0.015595359,
                       0.029906438,1.39808637,1.00679,0.28239188),
            `2045` = c(0.339136053,0.017522011,
                       0.3325736,0.369545995,33.1676,1.09854,0.069287604,
                       0.060134575,0.234912251,0.152821678,0.224129386,0.158131399,
                       3.14138643,1.0236,2.101434375,0.155952921,
                       0.002670667,0.008191624,0.178072688,33.1234,1.1111,0.001569817,
                       0.00072,0.005042438,0.003133891,0.020435968,
                       0.023822622,1.84740643,1.08809,0.777244206),
            `2050` = c(0.557310135,0.029337658,
                       0.4034207,0.463103472,31.8059,1.1001,0.15131688,
                       0.122730949,0.471007969,0.289334826,0.393149297,0.174076889,
                       4.2315315,1.06219,3.508790721,0.190584849,0.003276451,
                       0.009422135,0.206507664,31.7455,1.11365,0.001981097,
                       0.000787,0.006416314,0.003845906,0.024545266,
                       0.01812398,2.23972345,1.14156,1.317099699),
            `2055` = c(1.616574337,0.066166075,
                       0.6926676,0.648006065,30.2427,1.08768,0.469758832,
                       0.489633467,1.08561176,0.614512727,0.956101487,0.353398572,
                       5.5082233,1.09147,5.024072627,0.260492608,0.004022329,
                       0.012692391,0.233395054,30.0882,1.10751,0.002606299,
                       0.000947,0.007936322,0.004655018,0.028607554,
                       0.01569618,2.45408273,1.19931,1.644148568),
            `2060` = c(4.294617623,0.109876988,
                       1.24362336,0.938002464,28.4386,1.06804,0.791629977,
                       0.985894367,1.256957578,0.601689958,1.41165271,0.354162863,
                       6.7740796,1.14772,5.596362786,0.347465818,
                       0.004862987,0.019273115,0.264611958,28.2159,1.09591,
                       0.003801482,0.001372489,0.010049213,0.00579552,0.033687199,
                       0.016159965,2.527432,1.26219,1.753714563),
            `2065` = c(4.286490072,0.137428123,
                       1.70884922,1.098589884,26.4231,1.05029,0.860610703,
                       1.004920752,1.314895019,0.600442222,1.499824316,0.176141046,
                       7.3362802,1.21247,6.186795646,0.430865801,
                       0.005730147,0.027143998,0.287540572,26.3498,1.08018,
                       0.00524422,0.001790961,0.012473572,0.007065071,0.038539928,
                       0.015797048,2.5362207,1.31495,1.829594606),
            `2070` = c(4.375272299,0.156534621,
                       1.882924884,1.131414017,24.5241,1.02245,0.938164553,
                       0.961120744,1.446567608,0.660097306,1.640404233,
                       0.117480078,7.3503039,1.26148,6.327513824,0.501247966,
                       0.006562849,0.033879265,0.297791165,24.6578,1.0622,
                       0.007208101,0.002372138,0.015296907,0.008498451,0.041163326,
                       0.014499945,2.5209275,1.35214,1.839373744),
            `2075` = c(4.31979522,0.166236297,
                       1.88260285,1.096004011,22.9334,1.00341,1.024585629,
                       0.966820363,1.540411932,0.692983761,1.750730527,0.102869273,
                       7.0456986,1.29974,6.964933823,0.561686445,
                       0.007350564,0.041003756,0.302072407,23.0783,1.04307,
                       0.010986212,0.003851558,0.019966986,0.010987378,0.045765709,
                       0.015312518,2.502559,1.38257,1.896391114),
            `2080` = c(4.27871213,0.169313964,
                       1.80237439,1.010761134,21.3809,0.984437,1.145351819,
                       1.039191481,1.65749849,0.740151732,1.88838208,0.103602034,
                       6.651177,1.33548,7.951705902,0.623013759,0.008011109,
                       0.049420235,0.306849046,21.531,1.02546,0.017062109,
                       0.006312586,0.02721884,0.01493982,0.053968822,
                       0.018238154,2.4559789,1.41764,1.951161158),
            `2085` = c(4.2020007,0.168797603,
                       1.706996263,0.911719881,19.8774,0.965648,1.287643082,
                       1.117506236,1.783662445,0.792365745,2.01541746,0.10173608,
                       6.2681936,1.37414,8.691071417,0.683619339,0.00849176,
                       0.056269165,0.311197965,20.032,1.00881,0.021201935,
                       0.006865451,0.032349317,0.017564069,0.058864548,
                       0.0163904,2.3501654,1.45507,1.97715192),
            `2090` = c(4.1802268,0.166873623,
                       1.62021312,0.838050054,18.4975,0.948102,1.439393672,
                       1.248448669,1.923418053,0.858295858,2.08492162,0.093073298,
                       5.8359366,1.40662,8.861216513,0.74461107,0.00869137,
                       0.058634053,0.312207237,18.6507,0.993241,0.026727571,
                       0.008400502,0.037520384,0.020324672,0.064551077,
                       0.017120868,2.2033087,1.48825,1.95954817),
            `2095` = c(4.230485,0.165031433,
                       1.54106237,0.794992739,17.2507,0.931185,1.566034559,
                       1.406022045,2.03107794,0.912593393,2.05468545,0.077006543,
                       5.3620216,1.42811,8.68508621,0.803921467,0.008641063,
                       0.056827419,0.309582357,17.4014,0.978477,0.031878414,
                       0.009524944,0.041775367,0.022621079,0.069525552,
                       0.017217557,2.0345178,1.51388,1.91484259),
            `2100` = c(4.2172783,0.163043926,
                       1.46814589,0.774147072,16.0606,0.914071,1.84133647,
                       1.741048104,2.28065496,1.062669121,1.84010337,0.110045246,
                       4.8573656,1.43951,8.460527031,0.859888295,0.008435683,
                       0.052468967,0.304709544,16.2098,0.962558,0.037268104,
                       0.010842912,0.045197703,0.024514945,0.07390303,
                       0.017757478,1.8575908,1.53124,1.86302128),
             Units = c("MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC","MTC","MTC","MTC","MTC","MTC","MTC","MTC",
                       "MTC")
) %>% 
    select(-region, -Units) %>% 
    pivot_longer(-c('scenario', 'sector'), names_to = "year", values_to="MTC" ) %>% 
    mutate(year = as.numeric(year))
    



Assign6 %>% 
    ggplot(aes(x =year, y =MTC, group = scenario, color= scenario))+
    geom_line()+
    facet_wrap(~sector)
