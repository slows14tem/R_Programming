#암종류별 성별 분석
install.packages("dplyr")
library(dplyr)
#라이브러리에 올려놓지 않으면 %>% 사용 못함

#데이터 불러오기(암발생자수)
setwd("C:/RworkProject")
getwd()
df1 <- read.csv(file = "./03/03_암발생자수_.csv", header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
df1

# 열명 변경
# "암종별", "성별", "연령별", "발생자수", "조발생률"
names(df1) <- c("암종별", "성별", "연령별", "발생자수", "조발생률")
names(df1) 

# 데이터셋 조회
# 1) 특정 변수 조회
t1 <- df1$암종별
class(t1) #character
mode(t1)  #character
is.vector(t1)

# 2) 특정 열명을 사용하여 조회(1번과는 결과의 데이터 타입이 다름)
t2 <- df1['암종별']
class(t2) #data.frame
mode(t2)  #list

# 3) 특정 행 조회 :1행 조회
df1[1,]
# 3-1) 특정 행 조회 :2, 4행 조회
df1[c(2,4),]

# 4)특정행 제거 : 1행제거 (파이썬에서 -1은 '뒤에서부터'의 의미)
df <- df1[-1,]
head(df)

# 암종류 확인
unique(df$암종별)

# 5) 특정행 열 조회
df[1:3, c('암종별','발생자수')]

# 열 데이터 타입 확인
str(df)

# 값 변경 : - => 0 (na를 - 로 표시함)
df$발생자수 <- ifelse(df$발생자수 == "-", 0, df$발생자수)
df$조발생률 <- ifelse(df$조발생률 == "-", 0, df$조발생률)
df
# 열 데이터타입 변경
df$발생자수 <- as.numeric(df$발생자수)
df$조발생률 <- as.numeric(df$조발생률)
str(df)

# 모든암 제거하고 연령별 개인 데이터 
# 모든 암 제거
df2 <- df %>% filter(암종별 != "모든 암(C00-C96)") %>% filter(연령별 == "계")
df2
df21 <- df2 %>% filter(성별 == "계")
df22 <- df2 %>% filter(성별 != "계")
df21
df22
unique(df2$암종별)
df21 <- df21[, c('암종별','발생자수')]
df22 <- df22[, c('암종별','성별', '발생자수')]
str(df22)
 
# 그래프 
library(ggplot2)
ggplot(mapping =aes(x=암종별, y=발생자수), data=df21) +
  geom_bar(stat="identity", position=position_dodge(),fill="red") +
  ggtitle("암종별 발생자수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1))

ggplot(mapping =aes(x=암종별, y=발생자수, fill=성별), data=df22) +
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()  +
  ggtitle("암종별 성별 분석")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),
        axis.text.x = element_text(angle = 90, hjust=1))

#axis.text.x = element_text(angle = 90, hjust=1) 텍스트 90도 회전 및 한쪽 정렬
# hjust=1 텍스트 한쪽 정렬(0=아래쪽, 1=위쪽)

ggplot(mapping =aes(x=성별, y=발생자수), data=df22) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("성별 암 발생자수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

