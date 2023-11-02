
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


KPX_11_projection<- read_excel('2023전망작업_전력거래소분류_KDI전망치반영_안지석 수정.xlsx', sheet ="KDI_projection_raw") %>% 
    pivot_longer(-c(name, name_GCAM), names_to = "year", values_to = "value") %>% 
    mutate(year = as.numeric(year)) %>% 
    filter(year %in% seq(2005,2040, 5)) %>% 
    group_by(name_GCAM, year) %>%
    summarise(value = sum(value))



KPX_11_output_GCAM<- read_excel('2023전망작업_전력거래소분류_KDI전망치반영_안지석 수정.xlsx', sheet ="industry primary output by sec", skip = 1) %>% 
    select(-scenario, -region, -output, -sector) %>% 
    pivot_longer(-c(name_GCAM, Units), names_to = "year", values_to = "value") %>% 
    mutate(year = as.numeric(year)) %>% 
    group_by(name_GCAM, year) %>%
    summarise(value = sum(value))

KPX_11_output_GCAM %>% View()

KPX_11_projection 



KPX_GCAM_merged<- left_join(KPX_11_projection, KPX_11_output_GCAM, by = c('name_GCAM', 'year')) %>% 
    rename(KDI_projection = value.x, GCAM7_reference =value.y) %>% 
    pivot_longer(-c(name_GCAM, year), names_to = "type", values_to = "value") %>% 
    drop_na()

KPX_GCAM_merged


KPX_GCAM_merged %>%
    filter(!name_GCAM %in% c('INTERMEDIATE', 'COMMERCIAL')) %>% 
    ggplot(aes( x = year, y = value, group = type, color = type))+
    geom_line()+
    scale_y_continuous(
        "mpg (US)", 
        sec.axis = sec_axis(~ . * 1/10000000, name = "mpg (UK)")
    )+
    facet_wrap(~name_GCAM, scales="free_y")



coeff<- 200000





### scales="free_y"

KPX_GCAM_merged %>%
    filter(!name_GCAM %in% c('INTERMEDIATE', 'COMMERCIAL')) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    ggplot(aes( x = year))+
    geom_line(aes(y = KDI_projection), color ="red")+
    geom_line(aes(y = GCAM7_reference*coeff), color ="blue")+
    scale_y_continuous(
        "mpg (US)", 
        sec.axis = sec_axis(~ . * 1/coeff, name = "mpg (UK)")
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




