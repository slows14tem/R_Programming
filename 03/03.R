#현재 경로를 확인
getwd()
#새 경로 지정
setwd("C:/Rwork/03")
df <- read.csv(file = "03_치매환자현황.csv", fileEncoding = "euc-kr")
df$거주지역=as.factor(df$거주지역)
barplot(table(df$거주지역))
mode(df$데이터기준일자)

#날짜가 character형식이라서 계산할 수 없음(non-numeric argument to binary operator)
#df$진단일수 <- df$데이터기준일자-df$진단일자

#계산할 수 있는 날짜 데이터로 형식 변경
df$진단일수 <- as.Date(df$데이터기준일자)-as.Date(df$진단일자)
df
mean(as.integer(df$진단일수))

#진단일자에서 년도를 추출해서 나이 계산산
df$나이 <- as.integer(substring(df$진단일자,1,4)) - df$출생년도
df

#ifelse를 이용한 나이대 열 생성 방법
#df$나이대 <- ifelse(df$나이<50, "40대",
#                ifelse(df$나이<60, "50대",
#                      ifelse(df$나이<70, "60대",
#                            ifelse(df$나이<80,"70대",
#                                  ifelse(df$나이<90,"80대","90대")))))

df$나이대 <- paste(ifelse((df$나이%/%10)*10>=100,90,(df$나이%/%10)*10), "대", sep="")
df

barplot(table(df$나이대))
table(df$나이대)

##################################################################

#데이터 처리 페키지
#filter()	지정한 조건식에 맞는 데이터 추출	subset()
#select()	열의 추출	data[, c(“Year”, “Month”)]
#mutate()	열 추가	transform()
#arrange()	정렬	order(), sort()
#summarise()	집계	aggregate()
#group_by()  그룹화

install.packages("dplyr")
library("dplyr")
df <- read.csv(file = "03_암발생자수_.csv", header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
df
names(df)
names(df) <- c("암종별", "성별","연령별","2019","2019.1")
df2<- subset(df,df$암종별 == "모든 암(C00-C96)")

#or조건을 쓸때는 in연산자를 쓰는게 좋다.
#df %>% filter(!(연령별 %in% c("계", "연령미상")))

df22 <- df2 %>% filter(df2$암종별 == "모든 암(C00-C96)" & !(연령별 %in% c("계", "연령미상")))
df22
unique(df22$연령별)
df22$연령대 <- ifelse(df22$연령별 %in% c("0-4세","5-9세", "10-14세", "15-19세",
                                   "20-24세", "25-29세","30-34세", "35-39세"),"30대이하",
                   ifelse(df22$연령별 %in% c("40-44세","45-49세","50-54세","55-59세"), "40~50대",
                          ifelse(df22$연령별 %in% c("60-64세","65-69세","70-74세","75-79세"),"60~70대","80대이상")))

names(df22)  <- c("암종별", "성별","연령별", "y2019","y2019.1", "연령대")
class(df22$y2019)

#summaries 함수 사용을 위해 y2019열을 numeric화
df22$y2019 <- as.numeric(df22$y2019)
df22g <- df22 %>% group_by(연령대, 성별) %>% summarise(계 = sum(y2019))
df22g

install.packages("ggplot2")
library("ggplot2")
#qplot(연령대, data=df22, fill="성별")+
#  ggtitle("연령별 암환자수")+
#  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

ggplot(mapping =aes(x=연령대, y=계, fill=성별), data=df22g) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("연령대별 성별 분석")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

