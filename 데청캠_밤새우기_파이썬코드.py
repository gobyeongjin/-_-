# 필요한 라이브러리 임포트
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import mean_squared_error


# 데이터 로드
data = pd.read_csv('measure3.csv')

# 특성과 타겟 변수 분리
X = data.drop(columns=['weight'])
y = data['weight']

# Gradient Boosting 모델 훈련
gb_model = GradientBoostingRegressor(n_estimators=100, random_state=42)
gb_model.fit(X, y)

def predict_date(model, X_new, threshold=0.3):

    # 첫 번째 행을 시작점으로 설정
    current_day_data = X_new.iloc[0:1].copy()

    for day in range(1, len(X_new) + 1):
        # 해당 날의 중량 예측
        predicted_weight = model.predict(current_day_data)[0]

        # 중량이 threshold와 같거나 초과하면 해당 날짜 반환
        if predicted_weight >= threshold:
            return int(current_day_data['day'].values[0])

        # 다음 날의 데이터 설정 (제공된 데이터에서 가져오기)
        if day < len(X_new):
            current_day_data = X_new.iloc[day:day + 1].copy()

    # threshold를 초과하는 날짜를 찾지 못한 경우
    return None


# 새로운 독립변수 데이터 로드
new_data = pd.read_csv('test_4.csv')

# 예측 수행
predicted_day = predict_date(gb_model, new_data)
print(predicted_day)


#############################################################
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import mean_squared_error, r2_score


data_m1 = pd.read_csv('m1.csv')

# 특성과 타겟 변수 분리
X_m1 = data_m1[['day']]
y_m1 = data_m1['weight']

# Gradient Boosting 모델 훈련
gb_model_m1 = GradientBoostingRegressor(n_estimators=100, random_state=42)
gb_model_m1.fit(X_m1, y_m1)

# 전체 데이터셋에 대한 예측 수행
y_all_pred_gb_m1 = gb_model_m1.predict(X_m1)

# RMSE, R^2, Percentage of Explained Variance 계산
rmse_gb_m1 = np.sqrt(mean_squared_error(y_m1, y_all_pred_gb_m1))
r2_gb_m1_value = r2_score(y_m1, y_all_pred_gb_m1)
percentage_var_explained_gb_m1 = r2_gb_m1_value * 100  # R^2 value is equivalent to percentage of variance explained

# 그래프 그리기
plt.figure(figsize=(14, 7))
plt.plot(X_m1, y_m1, color='blue', label='Actual values', marker='o', alpha=0.6)
plt.plot(X_m1, y_all_pred_gb_m1, color='red', label='Predicted values', marker='x', alpha=0.6)
plt.title(f'Daily Comparison of Actual vs Predicted Weights (m5.csv, Gradient Boosting) - RMSE: {rmse_gb_m1:.4f}, R^2: {r2_gb_m1_value:.4f}, % Var Explained: {percentage_var_explained_gb_m1:.2f}%')
plt.xlabel('Day')
plt.ylabel('Weight')
plt.legend()
plt.grid(True)
plt.show()

print("% var explained : " +str(percentage_var_explained_gb_m1))
print("RMSE : " + str(rmse_gb_m1))
print("r^2 : " +str(r2_gb_m1_value))

## 2

data_m2 = pd.read_csv('m2.csv')

# 특성과 타겟 변수 분리
X_m2 = data_m2[['day']]
y_m2 = data_m2['weight']

# Gradient Boosting 모델 훈련
gb_model_m2 = GradientBoostingRegressor(n_estimators=100, random_state=42)
gb_model_m2.fit(X_m2, y_m2)

# 전체 데이터셋에 대한 예측 수행
y_all_pred_gb_m2 = gb_model_m2.predict(X_m2)

# RMSE, R^2, Percentage of Explained Variance 계산
rmse_gb_m2 = np.sqrt(mean_squared_error(y_m2, y_all_pred_gb_m2))
r2_gb_m2_value = r2_score(y_m2, y_all_pred_gb_m2)
percentage_var_explained_gb_m2 = r2_gb_m2_value * 100  # R^2 value is equivalent to percentage of variance explained

# 그래프 그리기
plt.figure(figsize=(14, 7))
plt.plot(X_m2, y_m2, color='blue', label='Actual values', marker='o', alpha=0.6)
plt.plot(X_m2, y_all_pred_gb_m2, color='red', label='Predicted values', marker='x', alpha=0.6)
plt.title(f'Daily Comparison of Actual vs Predicted Weights (m2.csv, Gradient Boosting) - RMSE: {rmse_gb_m2:.4f}, R^2: {r2_gb_m2_value:.4f}, % Var Explained: {percentage_var_explained_gb_m2:.2f}%')
plt.xlabel('Day')
plt.ylabel('Weight')
plt.legend()
plt.grid(True)
plt.show()

print("% var explained : " +str(percentage_var_explained_gb_m2))
print("RMSE : " + str(rmse_gb_m2))
print("r^2 : " +str(r2_gb_m2_value))

## 3


data_m3 = pd.read_csv('m3.csv')

# 특성과 타겟 변수 분리
X_m3 = data_m3[['day']]
y_m3 = data_m3['weight']

# Gradient Boosting 모델 훈련
gb_model_m3 = GradientBoostingRegressor(n_estimators=100, random_state=42)
gb_model_m3.fit(X_m3, y_m3)

# 전체 데이터셋에 대한 예측 수행
y_all_pred_gb_m3 = gb_model_m3.predict(X_m3)

# RMSE, R^2, Percentage of Explained Variance 계산
rmse_gb_m3 = np.sqrt(mean_squared_error(y_m3, y_all_pred_gb_m3))
r2_gb_m3_value = r2_score(y_m3, y_all_pred_gb_m3)
percentage_var_explained_gb_m3 = r2_gb_m3_value * 100  # R^2 value is equivalent to percentage of variance explained

# 그래프 그리기
plt.figure(figsize=(14, 7))
plt.plot(X_m3, y_m3, color='blue', label='Actual values', marker='o', alpha=0.6)
plt.plot(X_m3, y_all_pred_gb_m3, color='red', label='Predicted values', marker='x', alpha=0.6)
plt.title(f'Daily Comparison of Actual vs Predicted Weights (m3.csv, Gradient Boosting) - RMSE: {rmse_gb_m3:.4f}, R^2: {r2_gb_m3_value:.4f}, % Var Explained: {percentage_var_explained_gb_m3:.2f}%')
plt.xlabel('Day')
plt.ylabel('Weight')
plt.legend()
plt.grid(True)
plt.show()

print("% var explained : " +str(percentage_var_explained_gb_m3))
print("RMSE : " + str(rmse_gb_m3))
print("r^2 : " +str(r2_gb_m3_value))

## 4


data_m4 = pd.read_csv('m4.csv')

# 특성과 타겟 변수 분리
X_m4 = data_m4[['day']]
y_m4 = data_m4['weight']

# Gradient Boosting 모델 훈련
gb_model_m4 = GradientBoostingRegressor(n_estimators=100, random_state=42)
gb_model_m4.fit(X_m4, y_m4)

# 전체 데이터셋에 대한 예측 수행
y_all_pred_gb_m4 = gb_model_m4.predict(X_m4)

# RMSE, R^2, Percentage of Explained Variance 계산
rmse_gb_m4 = np.sqrt(mean_squared_error(y_m4, y_all_pred_gb_m4))
r2_gb_m4_value = r2_score(y_m4, y_all_pred_gb_m4)
percentage_var_explained_gb_m4 = r2_gb_m4_value * 100  # R^2 value is equivalent to percentage of variance explained

# 그래프 그리기
plt.figure(figsize=(14, 7))
plt.plot(X_m4, y_m4, color='blue', label='Actual values', marker='o', alpha=0.6)
plt.plot(X_m4, y_all_pred_gb_m4, color='red', label='Predicted values', marker='x', alpha=0.6)
plt.title(f'Daily Comparison of Actual vs Predicted Weights (m4.csv, Gradient Boosting) - RMSE: {rmse_gb_m4:.4f}, R^2: {r2_gb_m4_value:.4f}, % Var Explained: {percentage_var_explained_gb_m4:.2f}%')
plt.xlabel('Day')
plt.ylabel('Weight')
plt.legend()
plt.grid(True)
plt.show()

print("% var explained : " +str(percentage_var_explained_gb_m4))
print("RMSE : " + str(rmse_gb_m4))
print("r^2 : " +str(r2_gb_m4_value))

## 5


data_m5 = pd.read_csv('m5.csv')

# 특성과 타겟 변수 분리
X_m5 = data_m5[['day']]
y_m5 = data_m5['weight']

# Gradient Boosting 모델 훈련
gb_model_m5 = GradientBoostingRegressor(n_estimators=100, random_state=42)
gb_model_m5.fit(X_m5, y_m5)

# 전체 데이터셋에 대한 예측 수행
y_all_pred_gb_m5 = gb_model_m5.predict(X_m5)

# RMSE, R^2, Percentage of Explained Variance 계산
rmse_gb_m5 = np.sqrt(mean_squared_error(y_m5, y_all_pred_gb_m5))
r2_gb_m5_value = r2_score(y_m5, y_all_pred_gb_m5)
percentage_var_explained_gb_m5 = r2_gb_m5_value * 100  # R^2 value is equivalent to percentage of variance explained

# 그래프 그리기
plt.figure(figsize=(14, 7))
plt.plot(X_m5, y_m5, color='blue', label='Actual values', marker='o', alpha=0.6)
plt.plot(X_m5, y_all_pred_gb_m5, color='red', label='Predicted values', marker='x', alpha=0.6)
plt.title(f'Daily Comparison of Actual vs Predicted Weights (m5.csv, Gradient Boosting) - RMSE: {rmse_gb_m5:.4f}, R^2: {r2_gb_m5_value:.4f}, % Var Explained: {percentage_var_explained_gb_m5:.2f}%')
plt.xlabel('Day')
plt.ylabel('Weight')
plt.legend()
plt.grid(True)
plt.show()

print("% var explained : " +str(percentage_var_explained_gb_m5))
print("RMSE : " + str(rmse_gb_m5))
print("r^2 : " +str(r2_gb_m5_value))