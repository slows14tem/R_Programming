# 기상개황 자료를 이용하여 월별 불쾌지수를 계산하고 불쾌지수가 높음이상인 월을 구하시오.
# 
# https://kosis.kr/statHtml/statHtml.do?orgId=735&tblId=DT_A1040&vw_cd=MT_ZTITLE&list_id=215_215A_735_73503_A&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=MT_ZTITLE
# 
# 불쾌지수 공식
# 
# DI = 0.81 * Ta + 0.01 * RH * (0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)
# 불쾌지수 단계
# 
# 매우높음: 80이상
# 높음: 75이상 80미만
# 보통: 68이상 75미만
# 낮음: 68미만

setwd("C:/RworkProject")
df <- read.csv(file = "./04/04_기상개황.csv", header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
names(df)
df <- df[c("월별.1.", "평균기온....", "평균상대습도....")]
df
names(df) <- c("월별", "평균기온", "평균상대습도")
df$불쾌지수 <- round(0.81 * df$평균기온 + 0.01 * df$평균상대습도 * (0.99 * df$평균기온 - 14.3) + 46.3,2)
df$단계 <- ifelse(df$불쾌지수>=80, "매우 높음",
                ifelse(df$불쾌지수>=75, "높음",
                       ifelse(df$불쾌지수>=68, "보통", "낮음")))
str(df)
as.factor(df$단계)
dfy <- df %>% filter(df$월별 != "연간")
dfy
class(dfy)
as.factor(dfy$단계)
dfy %>% filter(dfy$단계 == "높음")

ggplot(mapping =aes(x=월별, y=불쾌지수), data=dfy) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("월별 불쾌지수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

barplot(table(dfy$단계))
