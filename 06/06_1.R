setwd("C:/RworkProject")
getwd()

#데이터 로드
df <- read.csv(file = "./06/06_지상관측.csv", header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
head(df$일시)
names(df)

#열명 변경
names(df) <- c("지점", "지점명", "일시", "기온", "풍속", "습도")

#체감온도 계산(단위환산 중요 m/s -> km/h)
df$체감온도 <- round(13.12 + 0.6215*df$기온 - 11.37*(df$풍속*3.6)*0.16 + 0.3965*(df$풍속*3.6)*0.16*df$기온, 2)
head(df)

#부산지역 겨울철 체감온도
dfBusan <- subset(df, df$기온<=10 & df$풍속>=1.3 & df$지점명=="부산")
dfBusan
library("ggplot2")
ggplot(mapping =aes(x=일시, y=체감온도, fill=체감온도), data=dfBusan) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("부산지역 겨울철 체감온도도")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1))

#열지수 계산
df$열지수 = round(0.5 * {df$기온 + 61.0 + ((df$기온-68.0)*1.2) + (df$습도*0.094)},2)
head(df)

# 지점명이 ’서울‘,’부산‘,’제주‘인 자료를 추출
#df %>% filter(지점명 %in% c("서울", "부산","제주")) 
#같은 열의 내용을 비교하는 내용은 여러번의 or연산보다 in 연산자로 한번에 쓰는게 편하다
dfHI <- subset(df, df$지점명=="서울" | df$지점명=="부산" | df$지점명=="제주")
dfHI

#일자별 기온그래프 + 평균기온 수평선
ggplot(mapping =aes(x=일시, y=기온, fill=지점명), data=dfHI) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("기온현황")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1))+
  geom_hline(aes(yintercept=mean(df$기온)), linetype=2, color="red")

#일자별 열지수그래프 + 열지수 5 수평선
ggplot(data=dfHI, aes(x=일시, y=열지수, group=지점명)) +
  geom_line(aes(color=지점명)) +  geom_point(aes(color=지점명)) + ggtitle("열지수현황") +
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1)) + 
  geom_hline(aes(yintercept=5), linetype=2, color="red")

#일자별로 그룹핑하여 열지수가 5이하인 자료를 추출.
library(dplyr)
#group_by with summarize 같이 써야함(그룹화된 정보를 어떻게 표현할 지 결정해야함)
dfHI5 <- df %>% group_by(일시) %>% summarize(평균열지수=mean(열지수)) %>% filter(평균열지수 <=5)
dfHI5
