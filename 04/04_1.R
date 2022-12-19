#인구 동향
library(dplyr)

#데이터 불러오기(인구동향)
setwd("C:/RworkProject")
getwd()
df <- read.csv(file = "./04/04_인구동향.csv", header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
df

# 열명 확인
names(df)

# "행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수"
names(df) <- c("행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수") 
head(df)

#EDA
#데이터 구조 확인
str(df)

#번주형 변경 : 시점: int -> category
df$시점 <- as.factor(df$시점)
mode(df$시점) #numeric
class(df$시점) #factor

#결측치 확인
summary(df)
df2 <- df %>% filter(is.na(df$출생아수)) #1
unique(df2$행정구역별)
#2. df[is.na(df$출생아수), ]  

#결측치 값을 0으로 변경
df2[is.na(df2)] <- 0
df2 %>% filter(df2$출생아수 == 0)
 
#결측치 행 제거
df3 <- na.omit(df)
summary(df3)

#결측치 값 대체
df4 <- df
df4$출생아수 <- ifelse(is.na(df4$출생아수), 0, df4$출생아수)
summary(df4)

#반복을 통한 na값 처리
col <- names(df)[3:6]
col

for(c in col){
  #print(df$c) ==> 안됨
  temp <- df[,c]
  temp <- ifelse(is.na(temp), 0, temp)
  df[,c] <- temp
}

summary(df)

#filter사용해서 na처리
df4 <- df4 %>% replace(is.na(df4), 0)

# 자연증가수 
#출생아수-사망자수
df$자연증가수 <- df$출생아수-df$사망자수
head(df)
df %>% filter((df$행정구역별 == "전국") & (df$자연증가수<0))
## df %>% filter((df$행정구역별 == "전국") & (df$자연증가수<0)) %>% select('시점')
## df[df$행정구역별 == "전국" & df$자연증가수<0, ]['시점']
## df[which(df$행정구역별 == "전국" & df$자연증가수<0, )]['시점']
str(df)

#기술통계분석 - 범주형 자료- 빈도분석
table(df$행정구역별)
table(df$시점)

#기술통계분석 - 연속형 자료
summary(df$출생아수)
plot(df$출생아수, df$혼인건수)
 
# 전국 자료와 지역자료 분리
dft <- df %>% filter(df$행정구역별 == "전국")
dfa <- df %>% filter(df$행정구역별 != "전국")

dftt <- dft %>% group_by(시점) %>% summarise(계 = sum(자연증가수))
dftt
dfaa <- dfa %>% group_by(행정구역별) %>% summarise(계 = sum(자연증가수))
dfaa 
#데이터 분석
ggplot(mapping =aes(x=행정구역별, y=출생아수), data=dfa) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("행정구역별 출생아수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1))

ggplot(aes(x=시점, y=자연증가수), data=dfa) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("연도별 자연증가수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1))

# 전국데이터 자연 증가수 그래프
ggplot(mapping =aes(x=시점, y=계), data=dftt) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("연도별 전체 인구증가수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1))

library(ggplot2)
  


