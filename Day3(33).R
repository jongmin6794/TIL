#텍스트마이닝 : text mining
#문장->형태소분석->명사, 동사...->빈도표->시각화
#java가 설치되어있어야함www.oracle.com
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_241")
library(KoNLP)
install.packages("ggiraphExtra")#시각화 하는 도구세트
library(ggiraphExtra)
str(USArrests)
head(USArrests)

library(tibble)#??<-질문하자 가벼운 데이터프레임느낌
#테이블형식을 쓰는 라이브러리 
crime<-rownames_to_column(USArrests,
                   var = 'state')#행이름을 열로 변환
head(crime)
crime$state<-tolower(crime$state)#소문자로 변환
crime$state
str(crime)
#미국 지도 데이터 준비
library(ggplot2)
install.packages("maps")
library(maps)
map_data('state')
states_map<-map_data('state')
str(states_map)
install.packages("mapproj")#<-?? 버전오류? 아마 다음시간 다시 사용
library(mapproj)
ggChoropleth(data = crime,#지도에 표시할 데이터
             aes(fill=Murder,#색깔로 표시할 변수
                 map_id=state),#지역 기준 변경
             map=states_map,#지도 데이터
             interactive = T)#지도 상호 작용
install.packages("stringi")#<-???문자열 관련 패키지... 아마 다음시간 다시 사용
install.packages("devtools")#깃헙의 패키지들을 사용할 수 있게 해줌?
devtools::install_github("cardiomoon/kormaps2014")#<-함수를 
#이용해 깃헙 함수 사용가능

library(devtools)
str(korpop1)
library(kormaps2014)#행정지도 관련 맵
str(kormap1)#글씨 깨져 나오기 때문에 변환과정 필요
str(changeCode(kormap1))#changeCode로 해결
str(changeCode(korpop1))#시도별 인구
head(changeCode(korpop1))
head(changeCode(korpop2))#군
head(changeCode(korpop3))#동
library(dplyr)
korpop1<-rename(korpop1,pop="총인구_명",name="행정구역별_읍면동")
str(changeCode(korpop1))
library(ggplot2)
library(maps)
library(mapproj)
library(ggiraphExtra)
ggChoropleth(data=changeCode(korpop1),#지도에 표시할 데이터
             aes(fill=pop,#색깔로 나타낼 변수
                 map_id=code,#지역 기준 변수
                 tooltip=name),#지도 위에 표시할 지역명
             map=kormap1,
             )
ggplot(korpop1,
       aes(map_id=code,fill="총인구_명"))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long,y=kormap1$lat)+
  #scale_fill_gradient(colours=c("white","orage","red"))+
  ggtitle("2015인구 분포도")+
  coord_map()
#devtools::install_github("cardiomoon/moonBook2")
#3ggChoropleth(korpop2,kormap2,fillvar="남자_명")

#############머신러닝#############
#통계적 기법, 연산 능력, 빅데이터
#데이터마이닝?
#
#벡터는 순서가 매우 중요
subject_name<-c("John","Jane","Steve")
temp<-c(37,35,33)
flu_status<-c(TRUE,FALSE,FALSE)
temp[2:3]
temp[-2]
temp[c(TRUE,FALSE,TRUE)]
#팩터:명목형 데이터를 표현
gender<-factor(c("M","F","M"))
gender
blood<-factor(c("O","AB","A"),
              levels = c("O","AB","A","B"))
blood
factor(c("A","F","C"),
       levels=c("A","B","C","D","F"),
       ordered = TRUE)

subject_name
#리스트:순서x, 타입이 다양
sb1<-list(fn=subject_name[1],
     temp=temp[1],
     flu=flu_status[1])
sb1
sb1$fn
sb1[[1]]
class(sb1[[1]])
subject_name[2]
sb1[c("temp","flu")]
df<-data.frame(sb1,stringsAsFactors =FALSE )
#stringsAsFactors:팩터형으로 문자열을 읽을것
sb1
str(df)

#apply계열함수:함수연산을 특정단위로 쉽게
#할 수 있도록 지원
#for,while(소규모 반복 연산)
#apply(대규모 반복 연산)
iris_num<-NULL
class(iris)
ncol(iris)#ncol컬럼 개수 알려줌
for(x in 1:ncol(iris)){
  if(is.numeric(iris[,x]))
    iris_num<-cbind(iris_num,iris[,x])
  #print(iris[,x])
}
class(iris_num)#타입 데이터프레임 아닌 matrix
iris_num<-iris[,sapply(iris,is.numeric)]
class(iris_num)#타입이 다시 데이터프레임...
iris_num<-data.frame(iris_num)#이 코드로 타입변환가능

iris_num<-iris[1:10,1:4]
set.seed(123)
sample(1:10,2)
idx_r<-sample(1:10,2)
idx_c<-sample(1:4,2)
idx_r
idx_c
for(i in 1:2){
  iris_num[idx_r[i], idx_c[i]]<-NA
}
iris_num
#apply:행(1) 또는 열(2) 단위 연산(MARGIN)
#입력:배열,메트릭스(같은 변수형)
#출력:메트릭스 또는 벡터
apply(iris_num,1,mean)
apply(iris_num,2,mean,na.rm=T)

apply(iris_num,2,function(x){x*2+1})

apply(iris_num,2,function(x){median(x*2+1,na.rm = T)})

#lapply:list+apply => 실행 결과가 list로 출력
#데이터프레임:모든 변수가 벡터를 가져야함
#리스트:벡터,메트릭스,데이터프레임 다 저장가능
apply(iris_num,2,mean,na.rm=T)#벡터 타입
lapply(iris_num,mean,na.rm=T)#list타입
#sapply:lapply와 비슷, 간단하게 기술
#연산결과가 벡터, 리스트(길이가 다른 경우)
class(sapply(iris_num,mean,na.rm=T))#타입 벡터
class(sapply(iris_num,mean,na.rm=T,simplify = F))#list타입
#vapply:sapply+템플릿 지정
sapply(iris_num,fivenum)#최소값,1사분위,2사분위,3사분위,최대값
vapply(iris_num,fivenum,c("Min."=0,"1st."=0,"med."=0,"3rd."=0,"Max."=0))


##usedcar.csv(미국 유명 중고차 사이트 데이터_케글) KNN알고리즘?
usedcars<-read.csv("Data/usedcars.csv",stringsAsFactors = FALSE)
#요소별로 그러나 factor, str로 나누지 못함... 다 바뀜.
#따라서 많은 형식으로 처리한 후 나머지 소수를 따로 작업
str(usedcars)
summary(usedcars$year)
summary(usedcars[c("price","mileage")])
range(usedcars$price)#최소 최대 범위
diff(range(usedcars$price))#최소 최대 차이
IQR(usedcars$price)
quantile(usedcars$price)#4분위수
quantile(usedcars$price,seq(from=0,to=1,by=0.1))
#4분위수대신 사용자 정의
boxplot(usedcars$price,
        main="Car prices",ylab="price($)")
boxplot(usedcars$mileage,
        main="Car mileage",ylab="odometer")

hist(usedcars$price,
        main="Car prices",xlab="price($)")
hist(usedcars$mileage,
        main="Car mileage",xlab="odometer")
var(usedcars$price)#분산(데이터-평균)의 제곱합 평균
sd(usedcars$price)#표준편차:분산의 제곱근

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

c_table<-table(usedcars$color)
round(prop.table(c_table)*100,1)#비율 퍼센트지 나옴
#round를 통해 소수 ,1 째 자리 까지

#일변량 통계
#이변량 통계(두 변수의 관계)
#다변량 통계(두 개 이상의 변수 관계)
#산포도(이변량)
plot(x=usedcars$mileage,
     y=usedcars$price)#반비례로 나옴 -> 강한 음의 상관관계

usedcars$conservative<-usedcars$color %in% c("Black","Gray","silver","White")
table(usedcars$conservative)
install.packages("gmodels")
library(gmodels)
CrossTable(x=usedcars$model,
           y=usedcars$conservative)#crossTable쓰기 위해 gmodels다운

#위스콘신 대학 암 데이터 활용 KNN
wbcd<-read.csv("Data/wisc_bc_data.csv",stringsAsFactors = FALSE)
str(wbcd)
wbcd<-wbcd[-1]#첫번째 열 제거
str(wbcd)

#knn이용해 얼굴인식 가능(이미지인식 많이 쓰임), 숫자인식, 추천 시스템, 
#유전자 패턴인식 #x,y축 표준화 먼저 수행해야함.
#머신러닝 factor로 쓰임...factor 변환 필요
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c('B','M'),
       labels = c('Benign','Malignant'))
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
normalize<-function(x){#정규화 함수 작성
  return ( (x-min(x))/(max(x)-min(x)) )
}
wbcd$diagnosis
normalize(c(1,2,3,4,5))
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))#정규화 
class(wbcd_n)
summary(wbcd_n$area_mean)

wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]

wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

library(class)#Knn이 class 라이브러리 안에 있으므로.
wbcd_test_pred<-knn(train = wbcd_train,
    test = wbcd_test,
    cl = wbcd_train_labels,
    k = 21)#k의 의미가 머지?
wbcd_test_pred
library(gmodels)
CrossTable(x=wbcd_test_labels,
           y=wbcd_test_pred,
           prop.chisq = FALSE)

#정규화:표준화는 최대/최소값이 없음.
#그 값이 중심 방향으로 축소되지 않음
wbcd[1]
wbcd_z<-as.data.frame(scale(wbcd[-1]))#표준화 작업 한방에??
#-1는 1번째열 빼고 다...
summary(wbcd_z$area_mean)

#모델 -> 테스트 -> 정확도

#iris(1:35, 51:85, 101:135)=train
#나머지 데이터=test
#iris
#data:1000건
#700건
#490     210             300건
#train valid(검증)       test
wbcd_n
#문제1
wbcd<-read.csv("Data/wisc_bc_data.csv",stringsAsFactors = FALSE)
wbcd
wbcd<-wbcd[-1]
wbcd
wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c('B','M'),
                       labels = c('Benign','Malignant'))
wbcd_z<-as.data.frame(scale(wbcd[-1]))#표준화 작업 한방에??
wbcd_z
wbcd_z_train<-wbcd_z[1:469,]
wbcd_z_test<-wbcd_z[470:569,]

wbcd_z_train_labels<-wbcd[1:469,1]
wbcd_z_test_labels<-wbcd[470:569,1]
library(class)
wbcd_z_test_pred<-knn(train = wbcd_z_train,
                      test = wbcd_z_test,
                      cl = wbcd_z_train_labels,
                      k = 21)
wbcd_z_test_pred
library(gmodels)
CrossTable(x=wbcd_z_test_labels,
           y=wbcd_z_test_pred,
           prop.chisq = TRUE)#

#문제2
iris
