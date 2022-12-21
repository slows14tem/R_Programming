setwd("C:/RworkProject")
getwd()

#데이터 로드
df <- read.csv(file = "./06/06_국민건강보험공단500.csv", header=T, stringsAsFactors = F, fileEncoding = "euc-kr")
names(df)

#필요한 열 추출출
df2 <- df[c("신장.5Cm단위.","체중.5Kg.단위.")]

#열이름 변경
names(df2) <- c("신장", "체중")

#BMI 계산
df2$BMI <- round(df2$체중/(df2$신장/100)^2,2)

#비만도 판별
df2$비만도 <- ifelse(df2$BMI < 20, 1,
                    ifelse(df2$BMI < 25, 2,
                           ifelse(df2$BMI < 30, 3,4
                           )))

summary(df2)

#학습데이터와 테스트데이터 나누기
x <- sample(1:nrow(df2), 0.7 * nrow(df2))
train <- df2[x, ]
test <- df2[-x, ]

#회귀모델 : 신장, 체중으로 비만도 예측
#학습(lm)
model <- lm(formula = BMI ~ 신장 + 체중, data=train)
#(Intercept)         신장         체중  
#6.50870     -0.05337      0.06958  
#y = -0.05337 * 신장 + 0.06958 * 체중 + 6.50870(Intercept)
model

#예측치 생성
pred <- predict(model, test)

#평가1
cor(pred, test$BMI)
#평가2
sqrt(mean((test$BMI - pred)^2))
