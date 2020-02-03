a<-1#a에 1할당
a
b<-2
(a+b)/2

v1<-c(1,2,5,8,9)#r에선 벡터생성 c
v1
v2<-c(1:5)
v2
v3<-seq(1,5)
v3
v4<-seq(1,10,by=3)
v4+1
s1<-"a"
s2<-"text"
s3<-"hi"
s4<-c(s1,s2,s3)
s4+1
mean(v1)
max(v1)
min(v1)
s4
paste(s4,collapse = ",")

install.packages("ggplot2")#페키지 설치
library(ggplot2)#메모리에 적재해야 사용가능
x<-c("a","a","b","c")
qplot(x)#빈도 그래프

mpg
qplot(data = mpg, x=hwy)#x고속도로hwy
qplot(data=mpg,x=drv,y=hwy,geom='line')#drv 구동방식(2륜 4륜 등)
qplot(data=mpg,x=drv,y=hwy,geom='boxplot')
qplot(data=mpg,x=drv,y=hwy,geom='boxplot',color=drv)
?qplot

eng<-c(90,80,60,70)
math<-c(50,10,20,90)
df_mid<-data.frame(eng,math)
df_mid
str(df_mid)
class<-c(1,1,2,2)
df_mid<-data.frame(eng,math,class)
df_mid
mean(df_mid$eng)

data.frame(eng=c(90,80,60,70),
math=c(50,10,20,90),
class=c(1,1,2,2))
df
#페키지 설치해야 파일 읽을 수 있음
#설치 후 메모리 적재 해야함
install.packages("readxl")
library(readxl)
df<-read_excel("C:/JMOh/Data/excel_exam.xlsx")
df
df$english
novar_df<-read_excel("C:/JMOh/Data/excel_exam_novar.xlsx"
                     ,col_names=F)
novar_df

df<-read.csv("csv_exam.csv")
df
str(df)

write.csv(df,file="mydf.csv")
exam<-read.csv("Data/csv_exam.csv")
head(exam,10)
tail(exam,10)
View(exam)
dim(exam) #파이썬 shape과 유사
str(exam)
summary(exam)

str(mpg) #num이 int보다 범위 넓다. 수치(실수)개념
head(mpg)
View(mpg)
summary(mpg)

df<-data.frame(v1=c(1,2,1),
           v2=c(2,3,2))
df
install.packages("dplyr")
library(dplyr)
#?rename
df<-rename(df,var1=v1)
df
df$v_sum<-df$var1+df$v2
df
str(mpg)
#total컬럼추가=hwy+cty의 평균
mpg$total<-(mpg$hwy+mpg$cty)/2
mpg
summary(mpg$total)
mpg
mpg<-as.data.frame(mpg)
mpg
mpg$test<-ifelse(mpg$total>=20,"pass","fail")
head(mpg,20)
table(mpg$test)
qplot(mpg$test)
#A,B,C등급 나누기
mpg$grade<-ifelse(mpg$total>=30,"A",
       ifelse(mpg$total>=20,"B","C"))
mpg
table(mpg$grade)
qplot(mpg$grade)

exam<-read.csv("Data/csv_exam.csv")
exam
#exam에서 class가 1인 것만 추출
exam %>% filter(class==1) %>% filter(math>=50)
#2반이면서 영어점수가 80점 이상인 데이터
exam %>% filter(class==2) %>% filter(english>=80)
exam %>% filter(class==2&english>=80)
exam %>% filter(class==2|english>=80)
#1, 3, 5반 추출
exam %>% filter(class==1|class==3|class=='5')
exam %>% filter(class %in% c(1,3,5))
exam$math
exam %>% select(math)
str(exam %>% select(math,class))
exam %>% select(-math,-class)
#class가 1 인 행에 대해 english를 추출
exam %>% filter(class==1) %>% select(english)
#exam에서 id, math추출 앞부분 6행까지 추출
head(exam %>%select(id,math) )
exam %>% select(id,math) %>% head(6)
exam %>% arrange(math)#math기준으로 오름차순 정렬
exam %>% arrange(desc(math))
exam %>% arrange(class,desc(math))
#파생변수
exam %>% mutate(total=math+english+science) %>% head
#science가 60점 이상pass, 미만이면 fail
#test열을 추가(ifelse)
exam %>% 
  mutate(test=ifelse(science>=60,"pass","fail"))
#total열추가 total=m+e+s
#total 내림차순 상위 10개 출력
exam %>% 
  mutate(total=math+english+science) %>% 
  arrange(desc(total)) %>% head(10)
