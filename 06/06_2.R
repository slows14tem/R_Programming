setwd("C:/RworkProject")
getwd()

#데이터 로드
df <- read.csv(file = "./06/06_국민건강보험공단500.csv", header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
head(df)
names(df)

#필요한 열 추출
df <- df[c("시도코드", "성별코드", "수축기.혈압", "이완기.혈압", "식전혈당.공복혈당.",  "트리글리세라이드", "HDL.콜레스테롤", "허리둘레" )]

#열이름 변경
names(df) <- c("시도코드", "성별코드", "수축기혈압", "이완기혈압", "공복혈당", "트리글리세라이드", "HDL콜레스테롤", "허리둘레" )

#결측치 제거
df2 <- na.omit(df)
summary(df2)

#대사증후군 판별
df2$높은혈압 <- ifelse(df2$수축기혈압 >= 130 & df2$이완기혈압 >= 85, 1, 0)
df2$높은혈당 <- ifelse(df2$공복혈당 >= 100, 1, 0)
df2$높은중성지방 <- ifelse(df2$트리글리세라이드 >= 150, 1, 0)
df2$낮은HDL수치 <- ifelse((df2$성별코드 == 1 & df2$HDL콜레스테롤 < 40)
                      | (df2$성별코드 == 2 & df2$HDL콜레스테롤 < 50), 1, 0) 
df2$복부비만 <- ifelse((df2$성별코드 == 1 & df2$허리둘레 >= 90)
                   | (df2$성별코드 == 2 & df2$허리둘레 >= 85), 1, 0)
df2$대사증후군 <- ifelse(df2$높은혈압 + df2$높은혈당 + df2$높은중성지방 + df2$낮은HDL수치 + df2$복부비만 < 1, 1,
                    ifelse(df2$높은혈압 + df2$높은혈당 + df2$높은중성지방 + df2$낮은HDL수치 + df2$복부비만 < 3, 2, 3
                    ))

head(df2)
#대사증후군 factor로 형변환
df2$대사증후군 <- as.factor(df2$대사증후군)

#학습데이터와 테스트데이터 나누기
x <- sample(1:nrow(df2), 0.7 * nrow(df2))
train <- df2[x, ]
test <- df2[-x, ]

library(party)
#분류모델 생성
model1 <- ctree(대사증후군~., data = train)
model2 <- ctree(대사증후군 ~ 수축기혈압+이완기혈압+공복혈당+트리글리세라이드+HDL콜레스테롤+허리둘레 , data = train)

#예측치
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)

#혼돈행렬
t1 <- table(pred1, test$대사증후군) 
t2 <- table(pred2, test$대사증후군) 
t1
t2

#정확도 평가가
acc1 <- (t1[1,1]+t1[2,2]+t1[3,3])/sum(t1)
acc2 <- (t2[1,1]+t2[2,2]+t2[3,3])/sum(t2)
acc1
acc2
