exam<-read.csv("Data/csv_exam.csv")
exam
library(dplyr)
exam %>% summarise(mean_math=sum(math))
exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math))
exam %>% 
  group_by(class) %>%
  summarise(mm=mean(math),
            sum=sum(math),
            md=median(math),
            cnt=n())
library(ggplot2)
mpg %>%
  group_by(manufacturer,drv) %>% 
  summarise(mc=mean(cty))
#  head(10)
#mpg 데이터를 회사별로 그룹화
#class가 suv추출
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=='suv') %>%
#tot=cty와 hwy의 평균값
  mutate(tot=(cty+hwy)/2) %>% 
  summarise(mt=mean(tot)) %>% 
  arrange(desc(mt)) %>% 
  head(5)

test1<-data.frame(id=c(1,2,3,4,5),
                   midterm=c(60,80,70,90,55))
test2<-data.frame(id=c(1,2,3,4,5),
                  final=c(70,80,40,80,75))
total<-left_join(test1,test2,by="id")
total
exam
name<-data.frame(class=c(1,2,3,4,5),
                 teacher=c("kim","lee","park","choi","cho"))
exam_new<-left_join(exam,name,by="class")
exam_new

test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))
test2<-data.frame(id=c(1,2,3,4,5),
                  fianl=c(70,80,40,80,75))
ta<-bind_rows(test1,test2)
ta
exam %>% filter(english>=80)
exam %>% filter(class==1&math>=50)
exam %>% filter(class%in% c(1,3,5))

exam %>% 
  select(id,math)
#test 컬럼 추가
#english>=60=>pass,fail
exam %>% 
  mutate(test=ifelse(english>=60,'pass','fail')) %>% 
  arrange(test)
test1
test2
left_join(test1,test2,by='id')

df<-data.frame(sex=c("M","F",NA,"M","F"),
           score=c(5,4,3,5,NA))
df
is.na(df)#결측치 확인
table(is.na(df))#value_count랑 비슷
table(is.na(df$sex))
table(is.na(df$score))
mean(df$score)#na와 계산시 na값으로
sum(df$score)
df %>% filter(is.na(score))
#score가 na인 데이터만 출력력
df_nomiss<-df %>% filter(!is.na(score))
df_nomiss#결측치 제거
mean(df_nomiss$score)
sum(df_nomiss$score)
df_nomiss<-df %>% filter(!is.na(sex)&!is.na(score))
  #score,sex컬럼에서 na가 아닌 데이터만 추출
df_nomiss
df
df_nomiss2<-na.omit(df)
#결측치 없는 데이터만 추출
df_nomiss2
mean(df$score, na.rm=T)
sum(df$score,na.rm = T)
exam<-read.csv("Data/csv_exam.csv")
exam[c(3,8,15),"math"]
exam[c(3,8,15),"math"]<-NA
exam
exam %>% summarise(mm=mean(math))
exam %>% summarise(mm=mean(math,na.rm = T),
                   sm=sum(math,na.rm = T),
                   med=median(math,na.rm = T))
#대체
#exam$math<-math열 값이 na면 55를 대입, 아니면 값 그대로
exam$math<-ifelse(is.na(exam$math),55,exam$math)
exam$math
table(is.na(exam$math))
mean(exam$math)
df<-data.frame(sex=c(1,2,1,3,2,1),
               score=c(5,4,3,4,2,6))
table(df$sex)
table(df$score)
df$sex<-ifelse(df$sex==3,NA,df$sex)
df$score<-ifelse(df$score>5,NA,df$score)
df %>% 
  filter(!is.na(sex)&!is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(ms=mean(score))
boxplot(mpg$hwy)
boxplot(mpg$why)stats
mpg$hwy<-ifelse(mpg$hwy<12|mpg$hwy>37,NA,mpg$hwy)
table(is.na(mpg$hwy))
#drv를 기준으로 그룹화
#mean_hwy<-hwy의 평균, 결측값은 제외
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm = T))

table(is.na(df$score))
ggplot(data=mpg,aes(x=displ,y=hwy))
ggplot(data=mpg,aes(x=displ,y=hwy))+
  geom_point()+
  xlim(3,6)+
  ylim(10,30)

df_mpg<-mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hway=mean(hwy))
df_mpg
ggplot(data=df_mpg,aes(x=drv,y=mean_hway))+geom_col()
economics
ggplot(data=economics,aes(x=date,y=unemploy))+
  geom_line()
install.packages("foreign")
library(foreign)#SPSS파일 로드드
library(dplyr)#전처리
library(ggplot2)#시각화
library(readxl)#엑셀파일
raw_welfare<-read.spss(file="Data/Koweps_hpc10_2015_beta1.sav",
                       to.data.frame = T)
welfare<-raw_welfare
str(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare<-rename(welfare,
       sex=h10_g3,#성별
       birth=h10_g4,
       marriage=h10_g10,
       religion=h10_g11,
       code_job=h10_eco9,
       income=p1002_8aq1,
       code_region=h10_reg7,)#지역코드
welfare
class(welfare$sex)
table(welfare$sex)
View(welfare)
#이상치 결측값 처리
ifelse(welfare$sex==9,NA,welfare$sex)
table(is.na(welfare$sex))
str(welfare)
welfare$sex
#1=>"male",2=>"female
welfare$sex<-ifelse(welfare$sex==1,"male","female")
welfare
table(welfare$sex)
qplot(welfare$sex)
class(welfare$income)
qplot(welfare$income)+xlim(0,1000)
summary(welfare$income)
#이상치 결측값 처리
welfare$income<-ifelse(welfare$income %in% c(0,9999),
                NA,welfare$income)
table(is.na(welfare$income))
sex_income<-welfare %>% 
  filter(is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mi=mean(income))
ggplot(data=sex_income,aes(x=sex,y=mi))+
  geom_col()
summary(welfare$birth)
table(is.na(welfare$birth))
##9999=>NA
welfare$birth<-ifelse(welfare$birth==9999,NA,welfare$birth)
table(is.na(welfare$birth))
welfare$age<-2015-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)
age_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mi=mean(income))
head(age_income)  
ggplot(data=age_income,
       aes(x=age,y=mi))+
  geom_line()
welfare<-welfare %>% 
  mutate(ageg=ifelse(age<30,"young",
         ifelse(age<59,"middle","old")))
table(welfare$ageg)
qplot(welfare$ageg)

#연령대별(초년, 중년, 장년) 월 수입 평균
#시각화까지...
ageg_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mi=mean(income))
ggplot(data=ageg_income,
       aes(x=ageg,y=mi))+
  geom_col()

ggplot(data=ageg_income,
       aes(x=ageg,y=mi))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))

#성별 월급 차이 연령대별로 다를까?
sex_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mi=mean(income))
sex_income

ggplot(data=sex_income,aes(x=ageg,y=mi,fill=sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits=c("young","middle","old"))
#성별, 연령별, 월급 평균표
sex_age_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mi=mean(income))
sex_age_income
ggplot(data=sex_age_income,aes(x=age,y=mi,col=sex))+
  geom_line()
welfare$code_job
#직업 코드별 인원수 확인
table(welfare$code_job)
library(readxl)
list_job<-read_excel("Data/Koweps_Codebook.xlsx",
                     sheet = 2,col_names = T)
list_job
str(welfare)
welfare$code_job
welfare<-left_join(welfare,list_job,id="code_job")
welfare$job
welfare$code_job
#welfare에서 code_job이 na가 아닌 데이터에 대해 
#데이터 eogo code_job, job 열 
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(20)
job_income<-welfare %>% 
  filter(!is.na(job)&!is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mi=mean(income))
head(job_income)
#상위 10개 직업 추출
top10<-job_income %>% arrange(desc(mi)) %>% head(10)
job_income %>% arrange(mi) %>% head(10)
ggplot(data=top10,aes(x=job,y=mi))+
  geom_col()+
  coord_flip()
ggplot(data=top10,aes(x=reorder(job,-mi),y=mi))+
  geom_col()+
  coord_flip()
test<-welfare %>%
  filter(!is.na(job)) %>% 
  group_by(sex,job) %>% 
  summarise(count=n())
test
