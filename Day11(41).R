# 말뭉치 구성
library(tm)
my.text.location <- "c:/JMOh/refer_data/ymbaek_papers"
mypaper <- VCorpus(DirSource(my.text.location))
mypaper
#meta():메타데이터 구성
summary(mypaper)
mypaper[[2]] #두번째 문서
mypaper[[2]]$content
mypaper[[2]]$meta
#meta:mypaper[[2]]를 설명하는 데이터
meta(mypaper[[2]], tag ='author')<-'g.d.hong'
mypaper[[2]]
mypaper[[2]]$meta

#tm_map(코퍼스, 사전처리함수)
library(stringr)
myfunc<-function(x){
  #특수기호(-, /,...) 전후의 단어를 확인
  mypuncts<-
    str_extract_all(x,"[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")
}
mypuncts<-lapply(mypaper,myfunc)
table(unlist(mypuncts))

myfunc<-function(x){
  #수치 자료 추출
  mydigits<-str_extract_all(x,"[[:digit:]]{1,}")
}
mydigits<-lapply(mypaper,myfunc)
table(unlist(mydigits))

#고유명사 추출(대문자로 시작)
myfunc<-function(x){
  #수치 자료 추출
  myuppers<-str_extract_all(x,"[[:upper:]]{1}[[:alpha:]]{1,}")
}
myuppers<-lapply(mypaper,myfunc)
table(unlist(myuppers))

mypaper[[2]]$content
#추가
mycorpus<-tm_map(mypaper,removeNumbers)
mytempfunc<-function(myobject,oldexp,newexp){
  newobject<-tm_map(myobject, 
                    content_transformer(
                      function(x,pattern) gsub(pattern, newexp, 
                                               x)),oldexp)
  #x:myobject, pattern:-collar, newexp:collar
  newobject
}
mycorpus<-mytempfunc(mypaper,"-collar","collar")
mycorpus<-mytempfunc(mypaper,"e\\.g\\.","for example")
mycorpus<-mytempfunc(mypaper,"and/or","and or")

mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,stripWhitespace)
mycorpus<-tm_map(mycorpus,
                 content_transformer(tolower))
mycorpus<-tm_map(mycorpus,removeWords, 
                 words=stopwords("SMART"))
mycorpus<-tm_map(mycorpus,
                 stemDocument, language='en')
#어근 동일화

#문자 개수 계산 함수
mycharfunc<-function(x){
  str_extract_all(x, ".")
}
#단어수 계산 함수
mywordfunc<-function(x){
  str_extract_all(x, boundary("word"))
}
mychar<-lapply(mypaper, mycharfunc)
myuniquechar0<-length(table(unlist(mychar))) #79문자 사용
mytotalchar0<-sum(table(unlist(mychar)))#24765글자
myword<-lapply(mypaper,mywordfunc)
myuniqueword0<-length(table(unlist(myword))) #1151 개 종류 단어
mytotalword0<-sum(table(unlist(myword))) #총 3504 개 단어 사용
#전처리 이후
mychar<-lapply(mycorpus, mycharfunc)
myuniquechar1<-length(table(unlist(mychar))) #79문자 사용
mytotalchar1<-sum(table(unlist(mychar)))#24765글자
myword<-lapply(mycorpus,mywordfunc)
myuniqueword1<-length(table(unlist(myword))) #1151 개 종류 단어
mytotalword1<-sum(table(unlist(myword))) #총 3504 개 단어 사용

results.comparing<-rbind(
  c(myuniquechar0, myuniquechar1),
  #전처리 전 글자 종류:79, 전처리 후 : 41
  c(mytotalchar0, mytotalchar1),
  c(myuniqueword0, myuniqueword1),
  #1151, 710
  c(mytotalword0, mytotalword1))

#3504, 2060
results.comparing
colnames(results.comparing)<-c("before","after")
rownames(results.comparing)<-c("고유문자수","총문자수",
                               "고유단어수","총단어수")
results.comparing

#문서*단어 행렬 구성
dtm.e<-DocumentTermMatrix(mycorpus)
dtm.e

#가로줄 이름(문서 이름)
rownames(dtm.e[,])
#단어
colnames(dtm.e[,])
#행렬의 내용 참조
inspect(dtm.e[1:3,50:55])

# TF-IDf(토픽모델링) 만들어지기 전에 dtm만들어져야함
# 행은 문서 이름, 열 단어, 내용은 단어 수가 채워져있어야함.
# Bag of Words(BoW)
# -단어 출현 빈도 
# -DTM 구성 TF기반으로 구성
# TF-IDF(단어빈도-역 문서 빈도)
# DTM내의 각 단어들마다 중요한 정도를 가중치로 주는 방법
# DTM생성->TF-IDF생성
# TF-IDF활용? 문서 유사도, 검색결과 중요도를 정할때
# TFID: TF*IDF
# 문서:d, 단어:t, 문서의 총 개수:n
# TF(d,t):특정 문서 d에서 특정 단어 t의 등장 횟수=>DTM
# 문서1, 문서2, 문서3 => 각 문서에서 중요한 단어 추출
# DF(t):특정 단어인 t가 등장한 문서의 개수
# @단어 등장 횟수 관심 X 
# IDF(d,t)=log(n/(1+df(t)))
# TFIDF는 모든 문서에서 자주 등장하는 단어는 중요도가 낮다.
#         특정 문서에서 자주 등장하는 단어는 중요도가 높다.
# TFIDF가 크면 단어의 중요도가 크다
dtm.e.tfidf<-DocumentTermMatrix(mycorpus,control = 
                     list(weighting= 
                            function(x) weightTfIdf(x,normalize=FALSE)))
dtm.e.tfidf
inspect(dtm.e.tfidf[1:3,50:55])
# TF는 크지만, TFIDF는 작은 단어들 검출
# 영향력 작은 흔한 단어들
value.tf.dtm<-as.vector(as.matrix(dtm.e[,]))
value.tf.dtm
value.tfidf.dtm<-as.vector(as.matrix(dtm.e.tfidf[,]))
value.tfidf.dtm
# tfidf 매트릭스
dim(dtm.e[,])[1]#24 24개 문서, 703개 단어
dim(dtm.e[,])[2]#703
colnames(dtm.e[,])#703개 단어 확인
#rep("test",each=3)
rep(1:3,each=10)#11111...222222...3333...
rep(1:3,times=10)#123123123...#times가 디폴트
word.label.dtm<-rep(colnames(dtm.e[,]),each=dim(dtm.e[,])[1])
doc.label.dtm<-rep(rownames(dtm.e[,]),dim(dtm.e[,])[2])
word.label.dtm
doc.label.dtm
mydata<-data.frame(word.label.dtm, doc.label.dtm,
           value.tf.dtm, value.tfidf.dtm)
mydata
colnames(mydata)<-c("word","doc","tf","tfidf")
mydata[120:130,]
#산점도
#상관관계(켄달)
cor.test(mydata$tf,mydata$tfidf,method="kendall")
#0.98 굉장이 높은 양의 상관관계 나옴
mydata$tf[mydata$tf>0]
mydata$tfidf[mydata$tfidf>0]
cor.test(mydata$tf[mydata$tf>0], #0.46높지않다. 
         mydata$tfidf[mydata$tfidf>0],method="kendall")
#즉, TF값의 순위가 높아도 TFIDF값의 순위가 
#높지 않은 경우가 적지 않다.

#어떤 단어가 TF가 높고 TFIDF가 낮은지?
#1)TF, TFIDF가 모두 0보다 큰 데이터 추출
#2)추출된 것들 중 TF>중위수, TFIDF<중위수 추출 =>단어확인
#class(mydata)->sub dataframe  subset사용
#subset(mydata,조건)#1
mydata2<-subset(mydata,tf>0 & tfidf>0)
#
table(mydata2$word)
mydata3<-subset(mydata2,tf>median(mydata2$tf) & 
                  tfidf<median(mydata2$tfidf))
str(mydata3)
table(mydata3$word)[table(mydata3$word)>0]#??


############################
install.packages("Sejong")
install.packages("hash")
install.packages("tau")
install.packages("RSQLite")
install.packages("rgdal")
install.packages("geojsonio")
install.packages("rgeos")
library(httpuv)
library(rgdal)
library(geojsonio)
library(rgeos)
library(KoNLP)

sentence <- '아버지가 방에 들어가신다'
extractNoun(sentence)

#버전 오류
############################
# 인공신경망(ANN)
# 필기체 인식, 음성 인식
# 자율주행차, 드론, 스마트 장치 자동화
# 
concrete <- read_csv("Data/concrete.csv")
str(concrete)#1030, 9
#수치 확인 후 정규화 혹은 표준화 필요
normalize<-function(x){
  return( (x-min(x))/(max(x)-min(x)))
}
lapply(concrete,normalize)
concrete_norm<-as.data.frame(lapply(concrete,normalize))
summary(concrete_norm)
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]
install.packages("neuralnet")#신경망설치
library(neuralnet)
#알고싶은것은 콘크리트의 강도
concrete_model<-neuralnet(formula = strength~cement+slag+
            ash+water+superplastic+coarseagg+fineagg+age,
          data=concrete_train)
plot(concrete_model)
concrete_test[1:8]
model_results<-compute(concrete_model,concrete_test[1:8])
str(model_results)

pre_str<-model_results$net.result
pre_str
cor(pre_str,concrete_test$strength)

#히든 레이어 여러개 주기
concrete_model2<-neuralnet(formula = strength~cement+slag+
                            ash+water+superplastic+
                             coarseagg+fineagg+age,
                          data=concrete_train,hidden = 5)
plot(concrete_model2)
model_results2<-compute(concrete_model2,concrete_test[1:8])
str(model_results)

pre_str2<-model_results2$net.result
pre_str2
cor(pre_str2,concrete_test$strength)
#보통 딥러닝 층 갯수 3~5개

#문제1
data(iris)
colnames(iris)
#Sepal.Length" "Sepal.Width"  "Petal.Length 3개의 컬럼으로 
#"Petal.Width" 예측
#data를 random shuffle
#70%:30% 분할(train/test)
#cor(hidden 레이어 수를 변경해 가면서)

#정규화
iris_data<-iris[-5]
normalize<-function(x){
  return( (x-min(x))/(max(x)-min(x)))
}
lapply(iris_data,normalize)
iris_norm<-as.data.frame(lapply(iris_data,normalize))
#train, test 구분
set.seed(1004)
train_sample<-sample(150:105)
iris_train<-iris_norm[train_sample,]
iris_test<-iris_norm[-train_sample,]
library(neuralnet)
#신경망
iris_model<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+
                        Petal.Width,
                      data=iris_train)
plot(iris_model)
model_results<-compute(iris_model,iris_test[1:3])
str(model_results)
pre_str<-model_results$net.result
pre_str
cor(pre_str,iris_test$Petal.Width)
#딥러닝
iris_model2<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+
                        Petal.Width,
                      data=iris_train,hidden=5)
plot(iris_model2)
model_results2<-compute(iris_model2,iris_test[1:3])
str(model_results)
pre_str2<-model_results2$net.result
pre_str2
cor(pre_str2,iris_test$Petal.Width)