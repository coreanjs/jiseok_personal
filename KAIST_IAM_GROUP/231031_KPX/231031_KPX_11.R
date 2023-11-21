
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(showtext)
library(extrafont)
library(ggtext)
library(gghighlight)
library(scales)

font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')
font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")

showtext_auto()

options(scipen=999)

getwd()


setwd("C:/R/Rproject/jiseok_personal/KAIST_IAM_GROUP/231031_KPX")


dir()

library(readxl)


### KPX 전망

KPX_11_projection<- read_excel('2023전망작업_전력거래소분류_KDI전망치반영_안지석 수정.xlsx', sheet ="KDI_projection_raw") %>% 
    pivot_longer(-c(name, name_GCAM), names_to = "year", values_to = "value") %>% 
    mutate(year = as.numeric(year)) %>% 
    filter(year %in% seq(2005,2040, 5)) %>% 
    group_by(name_GCAM, year) %>%
    summarise(value = sum(value))

### GCAM

KPX_11_output_GCAM<- read_excel('2023전망작업_전력거래소분류_KDI전망치반영_안지석 수정.xlsx', sheet ="industry primary output by sec", skip = 1) %>% 
    select(-scenario, -region, -output, -sector) %>% 
    pivot_longer(-c(name_GCAM, Units), names_to = "year", values_to = "value") %>% 
    mutate(year = as.numeric(year)) %>% 
    group_by(name_GCAM, year) %>%
    summarise(value = sum(value))


KPX_11_projection 


## socioeconomics
socioecomonics<- read_excel('socioeconomics.xlsx', sheet="gwangnam_ver")
 
#socioecomonics<- read_excel('socioeconomics.xlsx', "for_GCAM7")
socioecomonics

## 합치기

KPX_GCAM_merged<- left_join(KPX_11_projection, KPX_11_output_GCAM, by = c('name_GCAM', 'year')) %>% 
    rename(KDI_projection = value.x, GCAM7_reference =value.y) %>% 
    pivot_longer(-c(name_GCAM, year), names_to = "type", values_to = "value") %>% 
    drop_na()

KPX_GCAM_merged







### scales="free_y"
### except 'other'
coeff<- 17000000
KPX_GCAM_merged %>%
    filter(!name_GCAM %in% c('INTERMEDIATE', 'COMMERCIAL', 'other')) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    ggplot(aes( x = year))+
    geom_line(aes(y = KDI_projection), color ="red")+
    geom_line(aes(y = GCAM7_reference*coeff), color ="blue")+
    scale_y_continuous(labels= comma,
                       sec.axis = sec_axis(~ . * 1/coeff, name = "GCAM7 output")
    )+
    theme_bw()+
    theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo',
                              size = 14),
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
          axis.ticks.x = element_line(linewidth = .2,
                                      color = 'black'),
          axis.ticks.length = unit(.08, "cm"),
          axis.line.x = element_line(colour = "gray80", 
                                     size = .5, linetype = "solid"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = "plot",
          strip.text.x = element_text(size = 14),
          legend.position ="none"
    )+
    facet_wrap(~name_GCAM, 
               scales="free_y"
    )+
    labs(title = "KDI projection and GCAM7 industry primary output by sector",
         subtitle = " <span style = 'color:red;'><b>KDI projection</b></span>과<span style = 'color:blue;'>
       <b>GCAM7 output</b></span>",
         x ="year",
         caption = "Source : KEPCO, Graphic : Jiseok")



### except 'other'


coeff2<- 1000000
KPX_GCAM_merged %>%
    filter(!name_GCAM %in% c('INTERMEDIATE', 'COMMERCIAL', 'other')) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    ggplot(aes( x = year))+
    geom_line(aes(y = KDI_projection), color ="red")+
    geom_line(aes(y = GCAM7_reference*coeff2), color ="blue")+
    scale_y_continuous(labels= comma,
        sec.axis = sec_axis(~ . * 1/coeff2, name = "GCAM7 output")
    )+
    theme_bw()+
    theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo',
                              size = 14),
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
          axis.title.y = element_markdown(size = 20),
          axis.ticks.x = element_line(linewidth = .2,
                                      color = 'black'),
          axis.ticks.length = unit(.08, "cm"),
          axis.line.x = element_line(colour = "gray80", 
                                     size = .5, linetype = "solid"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = "plot",
          strip.text.x = element_text(size = 14),
          legend.position ="none"
    )+
    facet_wrap(~name_GCAM, 
               #scales="free_y"
               )+
    labs(title = "KDI projection and GCAM7 industry primary output by sector",
         subtitle = " <span style = 'color:red;'><b>KDI projection</b></span>과<span style = 'color:blue;'>
       <b>GCAM7 output</b></span>",
         x ="year",
         y = "<span style = 'color:red;'><b>KDI projection</b></span>",
         caption = "Source : KEPCO, Graphic : Jiseok")




### argricultural


coeff2<- 300000000
unique(KPX_GCAM_merged$name_GCAM)[3] -> target_name
KPX_GCAM_merged %>%
    filter(name_GCAM ==target_name) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    ggplot(aes( x = year))+
    geom_line(aes(y = KDI_projection), color ="red")+
    geom_line(aes(y = GCAM7_reference*coeff2), color ="black")+
    scale_y_continuous(
        sec.axis = sec_axis(~ . * 1/coeff2, name = "GCAM7 output")
    )+
    theme_bw()+
    theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo',
                              size = 14),
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
          axis.title.y = element_markdown(size = 20),
          axis.ticks.x = element_line(linewidth = .2,
                                      color = 'black'),
          axis.ticks.length = unit(.08, "cm"),
          axis.line.x = element_line(colour = "gray80", 
                                     size = .5, linetype = "solid"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = "plot",
          strip.text.x = element_text(size = 14),
          legend.position ="none"
    )+
    labs(title = paste("Sector:", target_name),
         subtitle = " <span style = 'color:red;'><b>KDI projection</b></span> & <span style = 'color:black;'>
       <b>GCAM7 output</b></span>",
         x ="year",
         y = "<span style = 'color:red;'><b>KDI projection</b></span>",
         caption = "Source : KEPCO, Graphic : Jiseok")




### cement


coeff2<- 300000
unique(KPX_GCAM_merged$name_GCAM)[4] -> target_name
KPX_GCAM_merged %>%
    filter(name_GCAM ==target_name) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    ggplot(aes( x = year))+
    geom_line(aes(y = KDI_projection), color ="red")+
    geom_line(aes(y = GCAM7_reference*coeff2), color ="black")+
    scale_y_continuous(
        sec.axis = sec_axis(~ . * 1/coeff2, name = "GCAM7 output")
    )+
    theme_bw()+
    theme_minimal()+
    theme(text = element_text(family = 'Nanum Myeongjo',
                              size = 14),
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
          axis.title.y = element_markdown(size = 20),
          axis.ticks.x = element_line(linewidth = .2,
                                      color = 'black'),
          axis.ticks.length = unit(.08, "cm"),
          axis.line.x = element_line(colour = "gray80", 
                                     size = .5, linetype = "solid"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = "plot",
          strip.text.x = element_text(size = 14),
          legend.position ="none"
    )+
    labs(title = paste("Sector:", target_name),
         subtitle = " <span style = 'color:red;'><b>KDI projection</b></span> & <span style = 'color:black;'>
       <b>GCAM7 output</b></span>",
         x ="year",
         y = "<span style = 'color:red;'><b>KDI projection</b></span>",
         caption = "Source : KEPCO, Graphic : Jiseok")





3345258/28688404

##### Show trends in data series measured in different units


KPX_index_100<-KPX_GCAM_merged %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    drop_na()%>% 
    group_by(name_GCAM) %>% 
    mutate(KDI_index = KDI_projection/KDI_projection[which.min(year)],
           GCAM_index = GCAM7_reference/GCAM7_reference[which.min(year)]) %>% 
    select(-KDI_projection, -GCAM7_reference) %>% 
    pivot_longer(-c(name_GCAM, year), values_to = "value", names_to = c("type", "index"), names_sep="_") %>% 
    select(-index)



KPX_index_100 %>% 
    filter(name_GCAM =="chemical")

############### 이미지 추출
KPX_index_100 %>%
    filter(name_GCAM !="other") %>% 
    ggplot(aes(x = year, y = value, group = type,color = type))+
    geom_line()+
    scale_y_continuous(labels = comma, limits = c(-2, 2.5))+
    theme_bw()+
   # theme_minimal()+
    scale_color_manual(values = c("blue", "red"))+
    theme(text = element_text(family = 'Nanum Myeongjo',
                              size = 14),
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
          axis.ticks.x = element_line(linewidth = .2,
                                      color = 'black'),
          axis.ticks.length = unit(.08, "cm"),
          axis.line.x = element_line(colour = "gray80", 
                                     size = .5, linetype = "solid"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = "plot",
          strip.text.x = element_text(size = 14),
         # legend.position ="none"
    )+
    facet_wrap(~name_GCAM, nrow = 1)+
    labs(title = "KDI projection and GCAM7 industry primary output by sector",
         subtitle = " <span style = 'color:red;'><b>KDI projection</b></span>과<span style = 'color:blue;'>
       <b>GCAM7 output</b></span><br>2005 values is set to index = 100 for each industry group",
         x ="year",
         caption = "Source : KEPCO, Graphic : Jiseok")

ggsave( file ="img_1.png",  width =2400, height = 1000, units ="px", dpi = 150,bg="white")  



KPX_11_projection %>% 
    ggplot(aes(x = year, y = value, group = name_GCAM, fill = name_GCAM))+
    geom_area()

#aggregate(value ~ name_GCAM + year, KPX_11_projection, sum)



socio_step1<- KPX_GCAM_merged %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    drop_na() 

### lag를 활용!!!!!!!!!

KPX_with_socioeconomics<-left_join(socio_step1, socioecomonics) %>% 
    relocate(name_GCAM, year, population, GDP) %>% 
    mutate(lagged_population = lag(population),
           pop_growth = population/lagged_population,
           prod_per_pop = KDI_projection/population,
           lagged_prod_per_pop = lag(prod_per_pop),
           production_growth_rate = prod_per_pop/lagged_prod_per_pop,
           income_elasticity = log(production_growth_rate)/log((1+labor_productivity)^5)) %>% 
    relocate(name_GCAM, year, population, pop_growth, GDP)



head(KPX_with_socioeconomics)


library(xlsx)
library("writexl")
write_xlsx(KPX_with_socioeconomics, 'test.xlsx')


??write

KPX_with_socioeconomics %>%
    pivot_longer(-c('name_GCAM', 'year'), names_to="type", values_to = "value") %>%
    filter(name_GCAM !="other") %>% 
    ggplot(aes(x = year, y = value, group = type, color = type))+
    geom_line(data= . %>% filter(type =="income_elasticity"))+
    facet_wrap(~name_GCAM)+
    theme_bw()+
    theme_minimal()+
    scale_color_manual(values = c("black"))+
    theme(text = element_text(family = 'Nanum Myeongjo',
                              size = 14),
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
          axis.ticks.x = element_line(linewidth = .2,
                                      color = 'black'),
          axis.ticks.length = unit(.08, "cm"),
          axis.line.x = element_line(colour = "gray80", 
                                     size = .5, linetype = "solid"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = "plot",
          strip.text.x = element_text(size = 14),
          # legend.position ="none"
    )


KPX_with_socioeconomics %>%
    pivot_longer(-c('name_GCAM', 'year'), names_to="type", values_to = "value") %>%
    filter(name_GCAM !="other") %>% 
    ggplot(aes(x = year, y = value, group = type, color = type))+
    geom_line(data= . %>% filter(type =="KDI_projection"))+
    facet_wrap(~name_GCAM)



## lag를 활용하여 production_GCAM 계산 후 index도 추가
KPX_full<- KPX_with_socioeconomics %>%
    mutate(lagged_KDI_projection = lag(KDI_projection),
           production_GCAM = lagged_KDI_projection*((1+labor_productivity)^5)^income_elasticity*pop_growth,
           KDI_index = KDI_projection/KDI_projection[which.min(year)],
           GCAM_index = GCAM7_reference/GCAM7_reference[which.min(year)])


KPX_full %>% 
    pivot_longer(-c("name_GCAM", "year"), names_to = "type", values_to = 'value') %>% 
    filter(name_GCAM!='other') %>% 
    ggplot(aes(x = year, y = value, color = type, group = type))+
    geom_line(data= . %>% filter(type %in% c('KDI_index', 'GCAM_index')))+
    facet_wrap(~name_GCAM)+
    labs(title = "index line graphs for differnet units of measures")



KPX_full %>% 
    pivot_longer(-c("name_GCAM", "year"), names_to = "type", values_to = 'value') %>% 
    filter(name_GCAM!='other') %>% 
    ggplot(aes(x = year, y = value, color = type, group = type))+
    geom_line(data= . %>% filter(type %in% c('KDI_index', 'GCAM_index', 'income_elasticity')))+
    facet_wrap(~name_GCAM)+
    theme_bw()+
    scale_y_continuous(labels = comma, limits = c(-2, 2.5))+
    scale_color_manual(values = c("blue","black","red"))+
    theme(text = element_text(family = 'Nanum Myeongjo',
                              size = 14),
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_markdown(size = 14, lineheight = 1.2),
          axis.ticks.x = element_line(linewidth = .2,
                                      color = 'black'),
          axis.ticks.length = unit(.08, "cm"),
          axis.line.x = element_line(colour = "gray80", 
                                     size = .5, linetype = "solid"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = "plot",
          strip.text.x = element_text(size = 14),
          #legend.position ="none"
    )+
    facet_wrap(~name_GCAM, 
               nrow=1
    )+
    labs(title = "KDI projection and GCAM7 industry primary output by sector",
         subtitle = " <span style = 'color:red;'><b>KDI projection</b></span>과<span style = 'color:blue;'>
       <b>GCAM7 output</b></span><br>2005 values is set to index = 100 for each industry group",
         x ="year",
         caption = "Source : KEPCO, Graphic : Jiseok")
ggsave( file ="img.png",  width =2400, height = 1000, units ="px", dpi = 150,bg="white")  















################
##SSP_database 활용


SSP_db_population<- data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
          SCENARIO = c("SSP3_v9_130115",
                       "SSP1_v9_130219","SSP2_v9_130219","SSP3_v9_130219","SSP4_v9_130219",
                       "SSP5_v9_130219","SSP1_v9_130325","SSP2_v9_130325",
                       "SSP3_v9_130325","SSP4_v9_130325","SSP5_v9_130325",
                       "SSP5_v9_130115","SSP1_v9_130115","SSP2_v9_130115",
                       "SSP3_v9_130115","SSP4_v9_130115","SSP5_v9_130115",
                       "SSP1_v9_130115","SSP2_v9_130115","SSP4d_v9_130115",
                       "KOREA_Census_2022"),
            `1950` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1955` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1960` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1965` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1970` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1975` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1980` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1985` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1990` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `1995` = c(NA,NA,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
            `2000` = c(NA,NA,NA,NA,NA,NA,45.988,
                       45.988,45.988,45.988,45.988,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,46.136101),
            `2005` = c(NA,NA,NA,NA,NA,NA,47.044,
                       47.044,47.044,47.044,47.044,NA,NA,NA,NA,NA,NA,
                       NA,NA,NA,47.278951),
            `2010` = c(48.183584,48.183584,48.183584,
                       48.183584,48.183584,48.183584,48.184,48.184,48.184,
                       48.184,48.184,48.183584,48.183584,48.183584,
                       48.183584,48.183584,48.183584,48.183584,48.183584,
                       48.183584,48.580293),
            `2015` = c(48.74400904,48.96909406,
                       48.90610765,48.74400904,48.81323093,49.07374738,48.969,
                       48.906,48.744,48.813,49.074,49.07374738,NA,NA,NA,NA,
                       NA,48.96909406,48.90610765,48.81323093,51.069375),
            `2020` = c(48.90773906,49.59029387,
                       49.37755506,48.90773906,49.1337652,49.90543775,49.59,
                       49.378,48.908,49.134,49.905,49.90543775,49.59029387,
                       49.37755506,48.90773906,49.1337652,49.90543775,
                       49.59029387,49.37755506,49.1337652,51.836),
            `2025` = c(48.66885644,50.04193186,
                       49.64065365,48.66885644,49.15417457,50.67285373,50.042,
                       49.641,48.669,49.154,50.673,50.67285373,NA,NA,NA,NA,
                       NA,50.04193186,49.64065365,49.15417457,51.448),
            `2030` = c(48.00708895,50.28266889,
                       49.66064722,48.00708895,48.84403855,51.32509133,50.283,
                       49.661,48.007,48.844,51.325,51.32509133,50.28266889,
                       49.66064722,48.00708895,48.84403855,51.32509133,
                       50.28266889,49.66064722,48.84403855,51.199),
            `2035` = c(46.93701295,50.24795501,
                       49.34232195,46.93701295,48.21306934,51.71521485,50.248,
                       49.342,46.937,48.213,51.715,51.71521485,NA,NA,NA,NA,
                       NA,50.24795501,49.34232195,48.21306934,50.869),
            `2040` = c(45.4196932,49.90433081,
                       48.63018578,45.4196932,47.20897979,51.81731566,49.904,
                       48.63,45.42,47.209,51.817,51.81731566,49.90433081,
                       48.63018578,45.4196932,47.20897979,51.81731566,49.90433081,
                       48.63018578,47.20897979,50.193),
            `2045` = c(43.47021541,49.26399226,
                       47.55950786,43.47021541,45.83033337,51.67342562,49.264,
                       47.56,43.47,45.83,51.673,51.67342562,NA,NA,NA,NA,
                       NA,49.26399226,47.55950786,45.83033337,49.03),
            `2050` = c(41.15776759,48.3506638,
                       46.18325095,41.15776759,44.10734162,51.33614282,48.351,
                       46.183,41.158,44.107,51.336,51.33614282,48.3506638,
                       46.18325095,41.15776759,44.10734162,51.33614282,
                       48.3506638,46.18325095,44.10734162,47.359),
            `2055` = c(38.63200125,47.21493295,
                       44.5978673,38.63200125,42.14035231,50.84629627,47.215,
                       44.598,38.632,42.14,50.846,50.84629627,NA,NA,NA,NA,
                       NA,47.21493295,44.5978673,42.14035231,NA),
            `2060` = c(36.02944105,45.91693027,
                       42.90645891,36.02944105,40.03312143,50.25249206,45.917,
                       42.906,36.029,40.033,50.252,50.25249206,45.91693027,
                       42.90645891,36.02944105,40.03312143,50.25249206,
                       45.91693027,42.90645891,40.03312143,NA),
            `2065` = c(33.44025858,44.50387087,
                       41.15544527,33.44025858,37.85106074,49.56692609,44.504,
                       41.155,33.44,37.851,49.567,49.56692609,NA,NA,NA,NA,
                       NA,44.50387087,41.15544527,37.85106074,NA),
            `2070` = c(30.92005632,42.91743962,
                       39.3918148,30.92005632,35.65787606,48.71405101,42.917,
                       39.392,30.92,35.658,48.714,48.71405101,42.91743962,
                       39.3918148,30.92005632,35.65787606,48.71405101,
                       42.91743962,39.3918148,35.65787606,NA),
            `2075` = c(28.50103483,41.14659751,
                       37.65467429,28.50103483,33.49693089,47.68409652,41.147,
                       37.655,28.501,33.497,47.684,47.68409652,NA,NA,NA,NA,
                       NA,41.14659751,37.65467429,33.49693089,NA),
            `2080` = c(26.20066083,39.30860921,
                       35.97464517,26.20066083,31.39595475,46.60629486,39.309,
                       35.975,26.201,31.396,46.606,46.60629486,39.30860921,
                       35.97464517,26.20066083,31.39595475,46.60629486,
                       39.30860921,35.97464517,31.39595475,NA),
            `2085` = c(24.02129817,37.48402883,
                       34.37058693,24.02129817,29.37281931,45.57013916,37.484,
                       34.371,24.021,29.373,45.57,45.57013916,NA,NA,NA,NA,
                       NA,37.48402883,34.37058693,29.37281931,NA),
            `2090` = c(21.98626301,35.66370615,
                       32.85468706,21.98626301,27.44421889,44.56367988,35.664,
                       32.855,21.986,27.444,44.564,44.56367988,35.66370615,
                       32.85468706,21.98626301,27.44421889,44.56367988,
                       35.66370615,32.85468706,27.44421889,NA),
            `2095` = c(20.12701092,33.78486373,
                       31.43552563,20.12701092,25.6266586,43.51078316,33.785,
                       31.436,20.127,25.627,43.511,43.51078316,NA,NA,NA,NA,
                       NA,33.78486373,31.43552563,25.6266586,NA),
            `2100` = c(18.45279802,31.93662737,
                       30.04447259,18.45279802,23.86058987,42.48365715,31.937,
                       30.044,18.453,23.861,42.484,42.48365715,31.93662737,
                       30.04447259,18.45279802,23.86058987,42.48365715,
                       31.93662737,30.04447259,23.86058987,NA)
) %>% 
    pivot_longer(-SCENARIO, values_to ="value", names_to ="year")

head(SSP_db_population)

SSP_db_population %>%
    filter(year>= 2000) %>% 
    ggplot(aes(x = year, y =value, group =SCENARIO, color =SCENARIO))+
    scale_y_continuous(limits = c(0, 55))+
    geom_line()+
    geom_line(data =.%>% filter(SCENARIO =="KOREA_Census_2022"), color ="red", linewidth = 1.5)



dir()
SSP_db<-read.csv('SSP_database_v9.csv', skip= 6) %>% 
    pivot_longer(-c('MODEL', 'SCENARIO', 'REGION', 'VARIABLE', 'UNIT'),
                 names_to="year", values_to ="value") %>% 
    mutate(year=as.numeric(str_sub(year, 2,5)))


head(SSP_db)

unique(SSP_db$VARIABLE)

str(SSP_db)



SSP_db_pop_GDP <- SSP_db %>% filter(VARIABLE %in% c('GDP|PPP', 'Population'))


SSP_db_pop_GDP %>% 
    filter(VARIABLE =="Population") %>% 
    ggplot(aes(x = year, y = value, group = interaction(MODEL, SCENARIO, REGION)))+
    geom_line(alpha =.2)+
    geom_line(data =. %>% filter(REGION =="KOR"), color ="red")
