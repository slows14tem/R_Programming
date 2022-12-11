#경로를 지정
setwd("C:/Rwork/02")
install.packages("readxl")
library(readxl)
df = read_excel(path = "02_역주행사고.xlsx")

# subset(ubset은 설정하는 조건에 맞는 벡터, 매트릭스 혹은 데이터 프레임을 반환)
df2 <- subset(df, df$구분 == "전체")
#df2 <- df[df$구분 == "전체",]  쉼표 필수
df3 <- subset(df, df$구분 == "역주행")
df4 <- df2

#열 전체의 데이터 변경경
df4$구분 <- "일반"
df4

#열이름을 기준으로 각 행의 데이터 계산
df4[c("사고", "사망")] <- df2[c("사고", "사망")] - df3[c("사고", "사망")]

df2$치명률 <- round((df2$사망/df2$사고)*100 ,2)
df2

df3$치명률 <- round((df3$사망/df3$사고)*100, 2)
df3

df4$치명률 <- round((df4$사망/df4$사고)*100 ,2)
df4


mean(df2$치명률)
mean(df3$치명률)
mean(df4$치명률)

#기초통계값
summary(df2)
summary(df3)
summary(df4)

#특정 문자열 출력
cat("최근 3년간 역주행 교통사고의 치명률이 ",
    round(mean(df3$치명률),1),
    "%로 일반 교통사고(",
    round(mean(df4$치명률),1),
    "%)보다 ",
    (round(mean(df3$치명률),1)/round(mean(df4$치명률),1)),
    "배 높은 것으로 나타났다.")

#시각화
install.packages("ggplot2")
library(ggplot2)
ggplot(mapping =aes(x=년도, y=사고, fill=구분), data=df) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('년도별 사고건수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

################################################################

dftax <- read.csv(file = "02_부산광역시_지방세 체납현황.csv", fileEncoding = "euc-kr")
dftax <- dftax[c("과세년도", "세목명", "체납액구간", "누적체납건수", "누적체납금액")]
names(dftax)

#해당 열에 중복없는 유일한 관측치 추출
list <-unique(dftax$세목명)
list

#과세년도 범주형
dftax$과세년도 <- as.factor(dftax$과세년도)

#함수 생성
makedf <- function (item) {
  #세목명 열의 값으로 새로운 subset생성해서 그래프 출력력
  dftemp <- subset(dftax, dftax$세목명==item)
  ggplot(mapping =aes(x=과세년도, y=누적체납건수, fill=과세년도), data=dftemp) +
    geom_bar(stat="identity", position=position_dodge()) +
    ggtitle(item)+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
}

makedf(list[1])
makedf(list[2])
makedf(list[3])
makedf(list[4])
makedf(list[5])
makedf(list[6])

################################################################

dfwt <- read.csv(file = "02_기상개황.csv", fileEncoding = "euc-kr")
names(dfwt)
dfwt <- dfwt[c("월별.1.", "평균기온....", "평균상대습도....")]
dfwt

#names로 데이터의 열이름 변경
names(dfwt) <- c("월별", "평균기온", "평균상대습도")
dfwt
dfwt$불쾌지수 <- round((0.81 * dfwt$평균기온 + 0.01 * dfwt$평균상대습도*(0.99 * dfwt$평균기온 - 14.3) + 46.3),2)
dfwt
dfwt$단계 <- ifelse(dfwt$불쾌지수>=80, "매우높음",
                  ifelse(dfwt$불쾌지수>=75, "높음",
                         ifelse(dfwt$불쾌지수>=68, "보통","낮음")))

dfwt

#일반 문자 데이터를 범주형 데이터로 변경해서 분석에 활용
dfwt$단계 <- as.factor(dfwt$단계)
table(dfwt$단계)
barplot(table(dfwt$단계), col = c("red", "green", "blue"))
#'연간' 열을 제거하기 위해 2:13까지의 데이터 사용
dfwt12 = dfwt[2:13, ]
barplot(table(dfwt12$단계), col = c("red", "green", "blue"))

dfwt2 <- as.data.frame(dfwt)
class(dfwt2)
dfwt2

