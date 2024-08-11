# 01. 데이터 불러오기
mt <- read.csv("measure_total.csv", header=TRUE)

# 각 feedType 별로 총 일수를 구하기 (몇일동안 진행되었는지)
type1 <- mt[mt$feedType == 1,]
type2 <- mt[mt$feedType == 2,]
type3 <- mt[mt$feedType == 3,]
type4 <- mt[mt$feedType == 4,]
type5 <- mt[mt$feedType == 5,]
d1 <- nrow(type1)
d2 <- nrow(type2)
d3 <- nrow(type3)
d4 <- nrow(type4)
d5 <- nrow(type5)

# 가장 낮은 일수를 common_days로 지정 (양식이 진행된 기간을 통일하기 위해서)
# 기간(총 일수)을 통일하지 않을 경우 -> day가 늘어날수록 당연히 새우의 증량이 증가하기 때문에
#   이 추가값들은 통계량에 영향을 미침 (즉 치하+사료별 차이를 알아보는데에 방해가 됨)
common_days <- min(d1,d2,d3,d4,d5)
common_days


# 4개 그룹에서 common_days를 초과하는 일자의 관측치가 있다면 지워버리기
type1 <- type1[1:37,]
type2 <- type2[1:37,]
type3 <- type3[1:37,]
type4 <- type4[1:37,]
type5 <- type5[1:37,]

mt2 <- rbind(type1, type2, type3, type4, type5)

View(mt2)

mt2$feedType <- factor(mt2$feedType,
                       levels = c(1:5),
                       labels = c("1차", "2차", "3차:1번수조", "3차:3번수조", "4차"))
str(mt2)
View(mt2)

# 02. 기본통계치 확인
library(psych)
attach(mt2)
describeBy(weight, feedType, mat=T) 


# 03. 그래프 그리기 (박스그래프, 히스토그램)
library(ggplot2)

ggplot(mt2, aes(x=feedType, y=weight)) +
  geom_boxplot(outlier.colour = "red") +
  ggtitle("치하+사료별 중량") +
  theme_classic() +
  theme(title = element_text(color = "darkblue", size=20))

# 04. 통계분석

# bartlett.test 를 사용한 등분산 검증
library(car)
bartlett.test(weight ~ feedType, data=mt2) 
# p-value <0.05이므로 분산이 동질하다는 귀무가설을 기각 -> 분산이 동일하지 않다
# 그러므로 "치하+사료" 타입마다 분산이 동일하지 않다

# leveneTest 를 사용한 등분산 검증
leveneTest(weight ~ feedType, data = mt2, center=mean)
# p-value <0.05이므로 분산이 동질하다는 귀무가설을 기각 -> 분산이 동일하지 않다
# 그러므로 "치하+사료" 타입마다 분산이 동일하지 않다


# 구한 p값이 0.05보다 작으므로 그룹간에 차이가 난다. 어떻게 차이가 나는지를,
#  그룹간의 차이를 밝히기 위해서 사후검정을 한다
mtResult <- aov(weight ~ feedType, data=mt2)  # 등분산일때 , 하지만 이 데이터는 이분산임
summary(mtResult)

# 이분산일때는 Welch's ANOVA test
oneway.test(weight ~ feedType, data=mt2, var.equal = FALSE)
# p-value 가 0.05보다 작음
# 그러므로 그룹 간에 중량이 같다는 귀무가설을 기각할 수 있음
# 그 다음에 사후검정 (Post-hoc test) 를 진행해서 그룹간 평균이 다른지 아닌지를 확인할수있음

# 앞서 설명했던것처럼, 우리는 표본수를 동일하게 만들고 분석을 진행했음
# 그룹별 표본수가 같기 때문에 TukeyHSD 사용
TukeyHSD(mtResult)

# 그러나 Tukey 방법은 그룹으로 묶어서 차이가 나는 방법이 없으므로 Duncan 사용
library(agricolae)
duncan.test(mtResult, "feedType", group=TRUE, console=TRUE)


# 05. 통계결과 그래프
tukeyPlot <- TukeyHSD(mtResult)
tukeyPlot
plot(tukeyPlot)

duncanPlot <- duncan.test(mtResult, "feedType")
plot(duncanPlot)
detach(mt2)


######### Random Forest

library(ggplot2)
# 필요한 라이브러리를 불러옵니다.

# 데이터 파일을 읽어옵니다.
measure2 <- read.csv("measure2.csv", header=TRUE, na.strings = ".")
measure3 <- read.csv("measure3.csv", header=TRUE, na.strings = ".")
measure4_1 <- read.csv("measure4_1.csv", header=TRUE, na.strings = ".")
measure4_3 <- read.csv("measure4_3.csv", header=TRUE, na.strings = ".")
measure5 <- read.csv("measure5.csv", header=TRUE, na.strings = ".")

# 각 데이터셋의 인덱스를 랜덤하게 분할합니다.
measure2_idx = sample(1:nrow(measure2), size=nrow(measure2)*0.8,replace = F)
measure3_idx = sample(1:nrow(measure3), size=nrow(measure3)*0.8,replace = F)
measure4_1idx = sample(1:nrow(measure4_1), size=nrow(measure4_1)*0.8,replace = F)
measure4_3idx = sample(1:nrow(measure4_3), size=nrow(measure4_3)*0.8,replace = F)
measure5_idx = sample(1:nrow(measure5), size=nrow(measure5)*0.8,replace = F)

# 분할된 인덱스를 기반으로 트레이닝 데이터와 테스트 데이터를 분리합니다.
measure2_train = measure2[measure2_idx,]
measure2_test = measure2[-measure2_idx,]

measure3_train = measure3[measure3_idx,]
measure3_test = measure3[-measure3_idx,]

measure4_1train = measure4_1[measure4_1idx,]
measure4_1test = measure4_1[-measure4_1idx,]

measure4_3train = measure4_3[measure4_3idx,]
measure4_3test = measure4_3[-measure4_3idx,]

measure5_train = measure5[measure5_idx,]
measure5_test = measure5[-measure5_idx,]

# 랜덤포레스트 모델을 생성하고 훈련합니다.
attach(measure2)
measure2_rf <- randomForest(weight ~ day + watertemp + pH + salinity + DO + Ammonia + NitrousAcid + Nitrate + Mg.Ca + waterHardness + feeds, data = measure2_train, ntree = 500)
detach(measure2)

attach(measure3)
measure3_rf <- randomForest(weight ~ day + watertemp + pH + salinity + DO + Ammonia + NitrousAcid + Nitrate + Mg.Ca + waterHardness + feeds, data = measure2_train, ntree = 500)
detach(measure3)

attach(measure4_1)
measure4_1rf <- randomForest(weight ~ day + watertemp + pH + salinity + DO + Ammonia + NitrousAcid + Nitrate + Mg.Ca + waterHardness + feeds, data = measure2_train, ntree = 500)
detach(measure4_1)

attach(measure4_3)
measure4_3rf <- randomForest(weight ~ day + watertemp + pH + salinity + DO + Ammonia + NitrousAcid + Nitrate + Mg.Ca + waterHardness + feeds, data = measure2_train, ntree = 500)
detach(measure4_3)

attach(measure5)
measure5_rf <- randomForest(weight ~ day + watertemp + pH + salinity + DO + Ammonia + NitrousAcid + Nitrate + Mg.Ca + waterHardness + feeds, data = measure5_train, ntree = 500)
detach(measure5)

# 훈련된 모델의 결과를 출력합니다.
print(measure2_rf)
print(measure3_rf)
print(measure4_1rf)
print(measure4_3rf)
print(measure5_rf)

# 테스트 데이터에 대한 예측을 수행합니다.
measure2_predictions <- predict(measure2_rf, newdata = measure2_test)
measure3_predictions <- predict(measure3_rf, newdata = measure3_test)
measure4_1predictions <- predict(measure4_1rf, newdata = measure4_1test)
measure4_3predictions <- predict(measure4_3rf, newdata = measure4_3test)
measure5_predictions <- predict(measure5_rf, newdata = measure5_test)

# 각 데이터셋에 대한 예측 결과를 출력합니다.
print(measure2_predictions)
print(measure3_predictions)
print(measure4_1predictions)
print(measure4_3predictions)
print(measure5_predictions)

# 예측 오차를 계산합니다.
measure2_errors <- measure2_test$weight - measure2_predictions
measure3_errors <- measure3_test$weight - measure3_predictions
measure4_1errors <- measure4_1test$weight - measure4_1predictions
measure4_3errors <- measure4_3test$weight - measure4_3predictions
measure5_errors <- measure5_test$weight - measure5_predictions

# 각 데이터셋에 대한 평균 절대 오차(MAE)를 계산하여 출력합니다.
measure2_mae <- mean(abs(measure2_errors))
cat("1차 평균 절대 오차 (MAE):", measure2_mae, "\n")

measure3_mae <- mean(abs(measure3_errors))
cat("2차 평균 절대 오차 (MAE):", measure3_mae, "\n")

measure4_1mae <- mean(abs(measure4_1errors))
cat("3-1번수조 평균 절대 오차 (MAE):", measure4_1mae, "\n")

measure4_3mae <- mean(abs(measure4_3errors))
cat("3차-3번수조 평균 절대 오차 (MAE):", measure4_3mae, "\n")

measure5_mae <- mean(abs(measure5_errors))
cat("4차 평균 절대 오차 (MAE):", measure5_mae, "\n")

# 각 데이터셋에 대한 제곱 평균 제곱근 오차(RMSE)를 계산하여 출력합니다.
measure2_rmse <- sqrt(mean(measure2_errors^2))
cat("1차 제곱 평균 제곱근 오차 (RMSE):", measure2_rmse, "\n")

measure3_rmse <- sqrt(mean(measure3_errors^2))
cat("2차 제곱 평균 제곱근 오차 (RMSE):", measure3_rmse, "\n")

measure4_1rmse <- sqrt(mean(measure4_1errors^2))
cat("3-1번수조 제곱 평균 제곱근 오차 (RMSE):", measure4_1rmse, "\n")

measure4_3rmse <- sqrt(mean(measure4_3errors^2))
cat("3차-3번수조 제곱 평균 제곱근 오차 (RMSE):", measure4_3rmse, "\n")

measure5_rmse <- sqrt(mean(measure5_errors^2))
cat("4차 제곱 평균 제곱근 오차 (RMSE):", measure5_rmse, "\n")

# 각 데이터셋에 대한 R-squared(결정 계수)를 계산하여 출력합니다.
measure2_actual_mean <- mean(measure2_test$weight)
measure2_total <- sum((measure2_test$weight - measure2_actual_mean)^2)
measure2_residual <- sum(measure2_errors^2)
measure2_rsquared <- 1 - (measure2_residual / measure2_total)
cat("1차 R-squared (결정 계수):", measure2_rsquared, "\n")

measure3_actual_mean <- mean(measure3_test$weight)
measure3_total <- sum((measure3_test$weight - measure3_actual_mean)^2)
measure3_residual <- sum(measure3_errors^2)
measure3_rsquared <- 1 - (measure3_residual / measure3_total)
cat("2차 R-squared (결정 계수):", measure3_rsquared, "\n")

measure4_1actual_mean <- mean(measure4_1test$weight)
measure4_1total <- sum((measure4_1test$weight - measure4_1actual_mean)^2)
measure4_1residual <- sum(measure4_1errors^2)
measure4_1rsquared <- 1 - (measure4_1residual / measure4_1total)
cat("3-1번수조 R-squared (결정 계수):", measure4_1rsquared, "\n")

measure4_3actual_mean <- mean(measure4_3test$weight)
measure4_3total <- sum((measure4_3test$weight - measure4_3actual_mean)^2)
measure4_3residual <- sum(measure4_3errors^2)
measure4_3rsquared <- 1 - (measure4_3residual / measure4_3total)
cat("3차-3번수조 R-squared (결정 계수):", measure4_3rsquared, "\n")

measure5actual_mean <- mean(measure5_test$weight)
measure5_total <- sum((measure5_test$weight - measure5actual_mean)^2)
measure5_residual <- sum(measure5_errors^2)
measure5_rsquared <- 1 - (measure5_residual / measure5_total)
cat("4차 R-squared (결정 계수):", measure5_rsquared, "\n")

# 각 데이터셋에 대한 그래프를 생성합니다.
library(ggplot2)

combined_data2 <- data.frame(
  Day = measure2$day,
  Actual = measure2$weight,
  Predicted = predict(measure2_rf, newdata = measure2)
)

combined_data3 <- data.frame(
  Day = measure3$day,
  Actual = measure3$weight,
  Predicted = predict(measure3_rf, newdata = measure3)
)

combined_data4_1 <- data.frame(
  Day = measure4_1$day,
  Actual = measure4_1$weight,
  Predicted = predict(measure4_1rf, newdata = measure4_1)
)

combined_data4_3 <- data.frame(
  Day = measure4_3$day,
  Actual = measure4_3$weight,
  Predicted = predict(measure4_3rf, newdata = measure4_3)
)

combined_data5 <- data.frame(
  Day = measure5$day,
  Actual = measure5$weight,
  Predicted = predict(measure5_rf, newdata = measure5)
)

# 1차 데이터셋 그래프
ggplot(combined_data2, aes(x = Day)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_point(aes(y = Actual, color = "Actual"), shape = 16, size = 2) +
  geom_point(aes(y = Predicted, color = "Predicted"), shape = 17, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
  labs(title = "1차 Random Forest: 실제 vs. 예측 새우 성장",
       x = "일자 (Day)",
       y = "무게 (Weight)",
       color = "Data Type") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(text = element_text(family = "NanumBarunGothic"), legend.position = "top") +
  coord_cartesian(xlim = c(1, 55), ylim = c(0, 0.6)) +
  scale_x_continuous(breaks = seq(1, 55, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1))

# 2차 데이터셋 그래프
ggplot(combined_data3, aes(x = Day)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_point(aes(y = Actual, color = "Actual"), shape = 16, size = 2) +
  geom_point(aes(y = Predicted, color = "Predicted"), shape = 17, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
  labs(title = "2차 Random Forest: 실제 vs. 예측 새우 성장",
       x = "일자 (Day)",
       y = "무게 (Weight)",
       color = "Data Type") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(text = element_text(family = "NanumBarunGothic"), legend.position = "top") +
  coord_cartesian(xlim = c(1, 55), ylim = c(0, 0.6)) +
  scale_x_continuous(breaks = seq(1, 55, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1))

# 3-1번수조 데이터셋 그래프
ggplot(combined_data4_1, aes(x = Day)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_point(aes(y = Actual, color = "Actual"), shape = 16, size = 2) +
  geom_point(aes(y = Predicted, color = "Predicted"), shape = 17, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
  labs(title = "3-1번수조 Random Forest: 실제 vs. 예측 새우 성장",
       x = "일자 (Day)",
       y = "무게 (Weight)",
       color = "Data Type") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(text = element_text(family = "NanumBarunGothic"), legend.position = "top") +
  coord_cartesian(xlim = c(1, 55), ylim = c(0, 0.6)) +
  scale_x_continuous(breaks = seq(1, 55, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1))

# 3차-3번수조 데이터셋 그래프
ggplot(combined_data4_3, aes(x = Day)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_point(aes(y = Actual, color = "Actual"), shape = 16, size = 2) +
  geom_point(aes(y = Predicted, color = "Predicted"), shape = 17, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
  labs(title = "3차-3번수조 Random Forest: 실제 vs. 예측 새우 성장",
       x = "일자 (Day)",
       y = "무게 (Weight)",
       color = "Data Type") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(text = element_text(family = "NanumBarunGothic"), legend.position = "top") +
  coord_cartesian(xlim = c(1, 55), ylim = c(0, 0.6)) +
  scale_x_continuous(breaks = seq(1, 55, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1))

# 4차 데이터셋 그래프
ggplot(combined_data5, aes(x = Day)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_point(aes(y = Actual, color = "Actual"), shape = 16, size = 2) +
  geom_point(aes(y = Predicted, color = "Predicted"), shape = 17, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
  labs(title = "4차 Random Forest: 실제 vs. 예측 새우 성장",
       x = "일자 (Day)",
       y = "무게 (Weight)",
       color = "Data Type") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(text = element_text(family = "NanumBarunGothic"), legend.position = "top") +
  coord_cartesian(xlim = c(1, 55), ylim = c(0, 0.6)) +
  scale_x_continuous(breaks = seq(1, 55, by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1))

