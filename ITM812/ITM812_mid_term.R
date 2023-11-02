

library(tidyverse)


library(readxl)



getwd()
demand<-read.csv("./ITM812/demand.csv") %>% 
    rename(date = 날짜) %>% 
    pivot_longer(-date, names_to="hour", values_to ="MWh") %>% 
    mutate(hour= gsub('X', '', hour),
           hour= gsub('시', '', hour),
           hour = as.numeric(hour)) %>% 
    rename(전력수요 = MWh)


str(demand)




trade<-read.csv("./ITM812/trade.csv") %>% 
    rename(date = 거래일, fuel = 연료원, hour = 거래시간, trade = 전력거래량) %>% 
    select(date, hour, fuel, trade) %>% 
    as.tibble() %>% 
    pivot_wider(names_from="fuel", values_from="trade") 


trade %>% View()

## Count the number of Missing Values with sapply

sapply(trade, function(x) sum(is.na(x)))



read.csv("./ITM812/trade.csv") %>% 
    rename(date = 거래일, fuel = 연료원, hour = 거래시간, trade = 전력거래량) %>% 
    select(date, hour, fuel, trade) %>% 
    distinct(fuel)

unique(trade$fuel)


trade %>% 
    mutate(year = year(date)) %>% 
    relocate(year) %>% 
    distinct(year)

str(trade)

## 필요 없는 애들 제거. 가스압, RPS는 모든 값이 0이라 제외
demand_trade<- inner_join(demand, trade, by = c('date', 'hour')) %>% 
        select(-date, -hour, -가스압, -RPS) 
    



## 인코딩 cp949
demand_trade %>% 
    write.csv('demand_trade.csv', fileEncoding ='cp949', row.names=FALSE)


colnames(demand_trade)






############### 션형회귀분석 돌리기
##https://m.blog.naver.com/PostView.naver?blogId=paperfactor_ceo&logNo=222212441873&categoryNo=12&proxyReferer=



head(demand_trade)

## 전력수요와 원자력의 상관관계
cor.test(demand_trade$전력수요, demand_trade$원자력)

plot(demand_trade$전력수요, demand_trade$원자력)

lm_fit <- lm(전력수요 ~ 원자력, data=demand_trade) 


lm_fit
plot(demand_trade$전력수요, demand_trade$원자력)
abline(lm_fit,col="red")




## 전력수요와 유연탄의 상관관계
cor.test(demand_trade$전력수요, demand_trade$유연탄)



## 상관행렬
round(cor(demand_trade),2)



## 상관계수 높은 순으로 보기 순서 보기 위해 절대값 씌움
#https://ek-koh.github.io/r/modeling1/

sort(abs(cor(demand_trade)[1,2:length(demand_trade)]), decreasing=T)


library(corrplot)
corrplot(round(cor(demand_trade),2), method ="number")



##단순 선형회귀분석(Simple/Univariable Linear Regression Analysis)


## lm(dependent ~ independent)
lm_fit <- lm(전력수요 ~ 원자력, data=demand_trade) 


summary(lm_fit)





colnames(demand_trade)

##다중 선형회귀분석(Multiple/Multivariable Linear Regression Analysis)
multi_fit <- lm(전력수요~ 원자력 + 유연탄 + 무연탄 + 바이오가스 +
                    부생가스 + 소수력 + 수력 + 양수 + 매립가스 + 중유
                + 태양광 + 폐기물 + 풍력 + LNG + LPG + 연료전지 + 경유 +
                    해양에너지 + 바이오매스 + IGCC + 바이오중유 + 기타, data=demand_trade)

summary(multi_fit)



## 다중공산성 VIF
library(car)
vif(multi_fit)






## 머신러닝
#https://kerpect.tistory.com/154

## 변수간의 상관관계
## 결과 : 변수간으 ㅣ상관관계 큰 거 없다 -> 뺼 거 없다.

colnames(demand_trade)

cor(demand_trade[,-1])

corrplot(cor(demand_trade[,-1]))

corrplot(round(cor(demand_trade[,-1]), 2), method="number")

library(caTools)

### train 0.7, test 0.3
x<- sample(1:nrow(demand_trade), 0.7*nrow(demand_trade))

x

train<- demand_trade[x, ]
test<- demand_trade[-x, ]


train

test


model <- lm(전력수요~ 원자력 + 유연탄 + 무연탄 + 바이오가스 +
                    부생가스 + 소수력 + 수력 + 양수 + 매립가스 + 중유
                + 태양광 + 폐기물 + 풍력 + LNG + LPG + 연료전지 + 경유 +
                    해양에너지 + 바이오매스 + IGCC + 바이오중유 + 기타, data=train)

model


summary(model)

head(train, 1)


## 예측치 생성 predict()함수

pred<- predict(object = model,  newdata = test)


## test 데이터 셋의 y 예측치
pred

##test 데이터셋의 y 관측치
test$전력수요

## 회귀모델 평가

cor(pred, test$전력수요)



cor(pred, test$전력수요)^2

mean(model$residuals^2)
