library(readxl)
library(tidyverse)


garam<-read_excel('./garam/RAW.xlsx')


garam %>% 
    mutate(전용면적 = as.factor(전용면적),
           category = round(거래금액/10000)) %>% 
    group_by(category) %>% 
    count() %>% 
    ggplot(aes(x = category, y = n))+
    geom_bar(stat='identity')+
    scale_x_continuous(breaks = seq(0, 22, 1))+
    geom_text(aes(label = n), vjust = -.5)+
    theme_bw()+
    theme_minimal()+
    labs(title = "거래량을 10,000으로 나눈 값을 반올림 한 것이 category")


library(ggdist)
library(RColorBrewer)
garam %>% 
    mutate(전용면적 = as.factor(전용면적)) %>% 
    ggplot(aes(x = 전용면적, y = 거래금액, group = 전용면적, color =전용면적))+
    geom_dots()+
    geom_text(data = .%>% group_by(전용면적) %>% summarise(거래금액 =mean(거래금액)),
              aes(label = round(거래금액, 1)))+
    coord_flip()+
    theme(legend.position ="none")



garam %>% 
    mutate(전용면적 = as.factor(전용면적),
           category = round(거래금액/10000)) %>% 
    group_by(전용면적, category) %>% 
    count() %>% 
    ggplot(aes(x = 거래금액 , y = category))+
    geom_line()+
    facet_wrap(~전용면적)



    



garam %>% 
    mutate(전용면적 = as.factor(전용면적),
           category = round(거래금액/10000)) %>% 
    ggplot(aes(x = 전용면적, y = 거래금액))+
    geom_boxplot(fill ="grey92", outlier.shape = NA)+
    geom_point(size = .6, alpha =.3, position = position_jitter(seed =1, width = .2))+
    coord_flip()+
    theme_bw()+
    theme_minimal()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())+
    labs(title = '전용면적에 따른 거래금액 분포포')





garam %>% 
    mutate(전용면적 = as.factor(전용면적),
           category = round(거래금액/10000)) %>% 
    group_by(전용면적, category) %>% 
    count() %>% 
    ggplot(aes(x = category , y = n))+
    geom_bar(stat="identity")+
    facet_wrap(~전용면적, ncol = 3)+
    scale_y_continuous(limits = c(0, 60))+
    geom_text(aes(label = n), size = 3, vjust = -.5)

