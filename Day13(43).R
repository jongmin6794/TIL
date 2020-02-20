#통계학 ? 모집단, 전수조사x, 표본을 조사, 모집단 추정
#        개체집합            모집단 일부
#모집단의 요약값(모수:평균, 분산) <--- 표본 계산값(통계량)
#                               모수추정
#자료 유형 -수치형 -이산형:분리되는 숫자자료
#                  -연속형:연속적인 값
#
#          -범주형 -명목형:순서가 없는 범주형 자료 ex)혈액형
#                  -순위형:순서가 있음 ex)학점

insurance<-read.csv("Data/insurance.csv")
#미국 환자 의료비 저장 데이터 bmi 
#보험사를 위한 데이터
insurance
str(insurance)#1338r 7c
#회귀모델을 구축하기 전에 정규성 확인
#종속변수가 정규분포를 따르는 경우 모델이 잘 만들어짐
#시각화 필요!
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)
#변수 상관 관계(상관행렬, cor함수)
cor(insurance[c("age","bmi","children","expenses")])
pairs(insurance[c("age","bmi","children","expenses")])
#install.packages("psych")#산포도? 시각화 라이브러리 설치
#panels 쓰기 위해
library(psych)
pairs.panels(insurance[
  c("age","bmi","children","expenses")])

#모델 생성
#expenses종속변수,
ins_model<-lm(expenses~age+children+bmi+sex+smoker+region,data=insurance)
ins_model<-lm(expenses~.,data=insurance)#위아래 같은 의미 .전체
ins_model
summary(ins_model)
#p값이 작다->계수가 0이 아닐 가능성이 높다
#해당 독립 변수가 종속변수와 영향력 높다
#독립 변수 종속변수의 상관계수가 높다.
#별 없는 것들 빼거나 파생변수?로 활용?
#R-squared(결정계수, r제곱값)
#모델이 종속변수 값을 얼마나 잘 설명하는가
#0.7509 => 75% 설명하고 있다. ?? <- 높이는게 중요...

#독립/종속 변수:선형 가정
#비선형 관계:높은 차수 항을 모델 추가
insurance$age2<-insurance$age^2
#lm(expensesage+age2)를 통해 비선형 과정

#BMI 이진화로
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)
#expenses~bmi30*smoker
#비만이면서 담배도 피면 의료비 더 나온다
ins_model2<-lm(expenses~age+age2+ #비선형 회귀모델
                 children+bmi+sex+
                 bmi30*smoker+region,data=insurance)
summary(ins_model2)#0.8664높아짐...


###문제 초미세먼지 데이터 초미세먼지 예측하기
#2008년 1월 1일 01시~2019년 11월03일 04시 1시간 간격
#오늘 배운 회귀분석 모델로
library(readxl)#엑셀파일 읽기
air02<-read_excel("Data/air_0.2_.xlsx")
air02
str(air02)#99978r 16c
colnames(air02)
air0.2d<-air02[c("Date","pm2.5","o3","no2","co","so2")]
air0.2<-air02[c("pm2.5","o3","no2","co","so2")]#date 제외
library(psych)
colSums(is.na(air0.2))#결측치 확인
air0.2n<-na.omit(air0.2)#결측치 존재 행 제거
cor(air0.2n[c("pm2.5","o3","no2","co","so2")])
pairs(air0.2n[c("pm2.5","o3","no2","co","so2")])
pairs.panels(air0.2n[c("pm2.5","o3","no2","co","so2")])
air_model<-lm(pm2.5~.,data=air0.2n)
summary(air_model)
