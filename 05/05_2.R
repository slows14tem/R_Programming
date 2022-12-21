#타이타닉 데이터
#https://www.kaggle.com/c/titanic/overview
setwd("C:/RworkProject/05")
#데이터불러오기
df <- read.csv("05_titanic.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr') 
head(df)

#데이터구조
# PassengerID	승객을 구별하는 고유 ID number	Int
# Survived	승객의 생존 여부를 나타내며 생존은 1, 사망은 0 입니다.	Factor
# Pclass	선실의 등급으로서 1등급(1)부터 3등급(3)까지 3개 범주입니다.	Ord.Factor
# Name	승객의 이름	Factor
# Sex	승객의 성별	Factor
# Age	승객의 나이	Numeric
# SibSp	각 승객과 동반하는 형제 또는 배우자의 수를 설명하는 변수이며 0부터 8까지 존재합니다.	Integer
# Parch	각 승객과 동반하는 부모님 또는 자녀의 수를 설명하는 변수이며 0부터 9까지 존재합니다.	Integer
# Ticket	승객이 탑승한 티켓에 대한 문자열 변수	Factor
# Fare	승객이 지금까지 여행하면서 지불한 금액에 대한 변수	Numeric
# Cabin	각 승객의 선실을 구분하는 변수이며 범주와 결측치가 너무 많습니다.	Factor
# Embarked	승선항, 출항지를 나타내며 C, Q, S 3개 범주이다.	Factor

df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)

# 결측치 확인
summary(df)
# 결측치 처리(삭제)
df2 <- na.omit(df)
summary(df2)
# 평균값으로 대체
round(mean(df$Age, na.rm = TRUE),2)
df$Age <- ifelse(!is.na(df$Age), df$Age, round(mean(df$Age, na.rm = TRUE),2))
 

# 성별에 따른 생존여부
library(ggplot2)

class(df$Survived)
mode(df$Survived)
table(df$Survived, df$Sex)
ggplot(df, aes(x=Survived, fill=Sex)) + geom_bar(position = "dodge")


#Pclass	선실의 등급에 따른 생존여부
ggplot(df, aes(x=Survived, fill=Pclass)) + geom_bar(position = "dodge")


#분류모델
names(df)
##각 필드의 정보를 하나하나 살펴보고 목적에 맞는 데이터인지 판단한 뒤 선별
ta <- df[, c("Pclass", "Sex", "Age", "Survived")]
head(ta)
 
#수치데이터로 변환
##data class “character” is not supported
ta$Sex <- ifelse(ta$Sex == "male", 1, 2)
ta
x <- sample(1:nrow(ta), 0.7 * nrow(ta))
x

train <- ta[x, ]
test <- ta[-x, ]

#학습
names(ta)
library(party)
model <- ctree(Survived ~ Pclass + Sex + Age, data=train)
model
summary(model)
plot(model)

#예측
pred = predict(model, test)
pred
table(pred)

#혼돈행렬

t <- table(test$Survived, pred) 
t
#accuracy
acc <- (t[1,1]+t[2,2])/sum(t) 
acc

