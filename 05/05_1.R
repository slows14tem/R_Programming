
#R 내장 데이터 가져오기
data(iris)
#꽃잎, 꽃밭침 -> 품종 == 분류
#꽃잎 -> 꽃받침 길이 예측 == 회귀
iris

#iris 데이터 확인
str(iris)

#iris : 꽃받침, 꽃잎 데이터 추출
names(iris)
iris1 <- iris[c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")]
#iris1 <- iris[, -5] 또는 iris[1, 4]
head(iris1)

#기술통계량
summary(iris1)
 

#상관계수
cor(iris1, method="pearson")

#색의 농도로 상관계수 
install.packages("corrgram")
library(corrgram)

corrgram(iris1, upper.panel = panel.conf)

#상관계수 챠트
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(iris1)

#학습데이터와 테스트데이터 분리
#데이터 셈플링 (렌덤 추출하기위해서)
# sample = 1행부터 끝까지 70%만 뽑아옴(nrow(iris1) = iris1의 모든 행을 의미)
x <- sample(1:nrow(iris1), 0.7 * nrow(iris1))
x

train <- iris[x, ]  #학습데이터 선정
test <- iris[-x, ]  #검정데이터 선정
nrow(train)
nrow(test)
 
#회귀모델 : 꽃받침 길이 예측 
names(iris1)

#학습(다중 회귀분석)
#lm은 분류모델 사용 불가능(모델 자체에서 타겟 피쳐를 가른다.)
model1 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train)
model2 <- lm(formula = Sepal.Length ~ Petal.Length + Petal.Width, data=train)
model3 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data=train)
model4 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Width, data=train)
model1
# y=0.7542*x1 + 0.7592*x2 - 0.6273*x3 +1.4187(intercept:절편)
summary(model1)
 
#예측(검정데이터 test를 이용해 회귀모델의 예측치 생성)
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)
pred4 <- predict(model4, test)

#평가
cor(pred1, test$Sepal.Length)
#RMSE : sqrt((실제 - 예측)^2의 평균)
RMSE1 <- sqrt(mean((test$Sepal.Length - pred1)^2))
RMSE2 <- sqrt(mean((test$Sepal.Length - pred2)^2))
RMSE3 <- sqrt(mean((test$Sepal.Length - pred3)^2))
RMSE4 <- sqrt(mean((test$Sepal.Length - pred4)^2))
RMSE1
RMSE2
RMSE3
RMSE4
#회귀모델은 오차(RMSE)가 더 작을수록 좋다.


#분류모델 (종을 맞추기)
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width))  + 
  geom_point(aes(colour = Species))

ggplot(iris, aes(Petal.Length , Petal.Width))  + 
  geom_point(aes(colour = Species))

#학습데이터와 테스트데이터 나누기
x <- sample(1:nrow(iris), 0.7 * nrow(iris))
x

train <- iris[x, ]
test <- iris[-x, ]

#모델 학습
#트리 모델 
install.packages("party")
library(party)
names(iris)

#회귀모델에도 ctree사용 가능
model1 <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train)
model2 <- ctree(Species ~ Sepal.Length + Petal.Length + Petal.Width, data = train)
model3 <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Width, data = train)
model4 <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length, data = train)
model1
plot(model1)
plot(model2)
plot(model3)
plot(model4)

#예측치
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)
pred4 <- predict(model4, test)

#혼돈행렬
## 실제 테스트 데이터와 예측데이터를 비교하여 정확한 예측을 했는지 확인할 수 있다.
t1 <- table(test$Species, pred1) 
t2 <- table(test$Species, pred2) 
t3 <- table(test$Species, pred3) 
t4 <- table(test$Species, pred4) 
t1
t2
t3
t4

#정확도(test 데이터 중 몇개를 맞췄는지)
acc1 <- (t1[1,1]+t1[2,2]+t1[3,3])/sum(t1)
acc2 <- (t2[1,1]+t2[2,2]+t2[3,3])/sum(t2)
acc3 <- (t3[1,1]+t3[2,2]+t3[3,3])/sum(t3)
acc4 <- (t4[1,1]+t4[2,2]+t4[3,3])/sum(t4)
acc1
acc2
acc3
acc4

#iris data 저장
 
