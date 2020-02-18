
# 조건부 확률
# 녹색 티 -> 잭팟 => P(AnB)=P(A)*P(B)
#    A        B 
# 1. 서로 영향을 끼치지 않는 경우
# p(B|A)=P(B) A와B가 서로 영향을 주지 않음
# 2. 서로 영향을 주는 경우
# 비가 오는 날(A), 우산 판매(B)
# P(AnB)=P(A)*P(B|A), P(B|A)=P(AnB)/P(A)

# 독립변수, 종속 변수
# 머신러닝 알고리즘 
# -> 데이터가 주어졌을 때, 어떠한 사건이 발생할 확률
# 겉보기 날씨, 습도 => 테니스 칠 확률

# 날씨, 습도 -> 테니스를 친다   
# (맑)  (좋)          or
#            -> 테니스를 안친다
# 테니스를 치는 경우가 많아 지려면?
# 1. 테니스를 많이 친다
# 2. 테니스를 쳤을 때 날씨가 맑고 습도가 좋은 조건 자주 발생
# 설명 변수(날씨, 습도)간에 독립을 가정
# -> 테니스를 쳤는데, 날씨와 습도는 서로 상관관계가 없다고 가정
# 독립이라고 가정 => 계산이 간단해짐,
#                   data적은 경우에도 모델 생성 가능

# 베이즈 정리
# 확률 큰 값으로 분류
# P(Ai|B)=P(Ai n B)/P(B) 
# 조건부 확률, 특정 상황이 만족되면 계산을 이렇게 할 수 있다.
# 특정상황 1. 배반 사건(교집합이 없음)
#          2. A1~A4 전체 집합
# P(B|Ai)P(Ai)/(P(B|A1)P(A1)+P(B|A2)P(A2)+P(B|A3)P(A3)+P(B|A4)P(A4))
# P(Ai n B)/P(B)=P(Ai n B)/(P(A1 n B)+...+P(A4 n B))
# P(A|B)=P(AnB)/P(B)=P(B|A)P(A)/P(B)
# 라플라스 추정량 모두 1로 더함? 0건의 데이터 처리

sms_raw<-read.csv("Data/sms_spam_ansi.txt",
                  stringsAsFactors = FALSE)#str형
sms_raw<-read.csv("Data/sms_spam_ansi.txt") #factor형
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)#factor로 바꿈
names(sms_raw)
str(sms_raw$type)
table(sms_raw$type)#ham 4812, spam 747

# 텍스트 데이터 정리, 표준화
# tm 패키지 : 텍스트 마이닝 패키지
# 설치 
install.packages("tm")
library(tm)
# 코퍼스 : 단어 집합 생성 -> VCorpus()코퍼스생성 함수
# 메모리에 만들어짐... 따라서 디스크에 저장하는 방식도 익히면 좋다.
# 데이터 소스 객체 생성 -> VectorSource()
sms_corpus<-VCorpus(VectorSource(sms_raw$text))
sms_corpus
inspect(sms_corpus[1:2])
sms_corpus[1]
as.character(sms_corpus[[1]])#데이터 내용 확인
# 1번부터 5번까지 문서 내용 출력(lapply함수 이용)
lapply(sms_corpus[1:5],as.character)
sms_corpus_clean<-tm_map(sms_corpus,
                         content_transformer(tolower))
# 예전 R버전에서는 아래와 같이 작업할 것
# tm_map(sms_corpus,content_trasformer(tolower))
class(sms_corpus_clean)
as.character(sms_corpus_clean[[1]])
# class(as.character(sms_corpus_clean[[1]]))
# read.csv 함수에 추가할 것(글씨 깨짐)
# fileEncoding="CP949",encoding = "UTF-8"

# 숫자 제거
sms_corpus_clean<-tm_map(sms_corpus_clean, removeNumbers)
inspect(sms_corpus_clean[1:5])
# 구두점 제거
removePunctuation("hi....hello...bye")
sms_corpus_clean<-tm_map(sms_corpus_clean, removePunctuation)
# 불용어(stop words) 제거
# 불용어 : to, and, but, or...
# 불용어 무조껀 제거 x 중요 의미 사용하는 경우 존재
stopwords()
?stopwords
sms_corpus_clean<-tm_map(sms_corpus_clean, removeWords,
                         stopwords())
inspect(sms_corpus_clean[1:5])
# 불용어 제거 - 정규식 사용
replacePunctuation<-function(x){# patter = 정규식, "바꿀것"
  gsub("[[:punct:]]+"," ",x)  
}#x에 전달된 문자열에 대해 punctuation제거
replacePunctuation("hi+.{hello<;")
x="대한민국 대한 민국 대한민국"
gsub("대한민국","코리아",x)
gsub("한국","코리아",x)
gsub("우리나라","코리아",x)
gsub("조선","코리아",x)

# 형태소 분석
install.packages("SnowballC")
library(SnowballC)
# 단어의 어근을 추출 wordStem
wordStem(c("learn","learned","learning","learns"))
# 텍스트 문서의 전체 코퍼스에 wordStem적용 stemDocument함수
sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
inspect(sms_corpus_clean[1:5])
# 불필요 공백 제거 stripWhitespace
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)
lapply(sms_corpus[1:3],as.character)
inspect(sms_corpus_clean[1:3])

#######################################################
# 토큰화(단어)
# DocumentTermMatrix() : sms 메세지코퍼스 -> 토큰화
# 행 : sms 메시지, 열 : 단어
# DTM행렬, TDM행렬(행:단어,열:메시지)
# 위에서부터의 과정 한줄로 해결.
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm2<-DocumentTermMatrix(sms_corpus,
                             control = list(tolower=TRUE,
                                            removeNumbers=TRUE,
                                            stopwords=TRUE,
                                            removePunctuation=TRUE,
                                            stemming=TRUE #형태소 분석
                                            ))# sparse 희소행렬

sms_dtm_train<-sms_dtm2[1:4169,]
sms_dtm_test<-sms_dtm2[4170:5559,]
sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type
install.packages("wordcloud")
library(wordcloud)
#최소 50개 이상 나오는 단어들만
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE)
wordcloud(sms_corpus_clean,min.freq = 50,
          scale = c(5,0.2),#가장 빈도 큰 단어 크기5, 작은 0.2
          rot.per = 0.1,#무작위 10프로의 데이터 갯수 회전
          max.words = 100,random.color = T,
          colors=brewer.pal(10,"Paired"),random.order = FALSE)
spam<-subset(sms_raw,type=="spam")#부분 집합 구분
ham<-subset(sms_raw,type=="ham")#부분 집합 구분
wordcloud(spam$text,max.words = 40,scale = c(3,0.5))
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

sms_dtm_train
#최소 5번 이상 등장한 단어
sms_freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)
convert_counts<-function(x){
  x<-ifelse(x>0,"Yes","No")
}
#행렬의 열/행 단위로 전달(apply,MARGIN=1(행),2(열))
sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]
sms_train<-apply(sms_dtm_freq_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN = 2,convert_counts)
sms_train

#나이브 베이지안 필터기 생성(모델)
#베이지안 뿐만 아니라 여러 머신러닝 알고리즘 들어있음
install.packages("e1071")
library(e1071)
#우도표 생성
sms_classifier<-naiveBayes(sms_train,sms_train_labels)
#test단어들 확률 계산
#predict(만든 모델,테스트data)
sms_test_pred<-predict(sms_classifier,sms_test)
sms_test_pred
library(gmodels)
CrossTable(sms_test_pred,sms_test_labels,
           prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))
#라플라스 적용
sms_classifier2<-naiveBayes(sms_train,
                           sms_train_labels,laplace = 1)
sms_test_pred2<-predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_test_labels,
           prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))

#문제1 mushrooms데이터 베이지 이론 분류
mushrooms<-read.csv("Data/mushrooms.csv")
str(mushrooms)#8124,23
names(mushrooms)
table(mushrooms$type)#e:4208,p:3916
#mushroom 데이터 형태소 분석등 전처리 필요x
#나이브 베이지안 필터기 생성 패키지
#널값 확인
colSums(is.na(mushrooms))#결측치 존재 하지 않음 확인
mush<-mushrooms[-1]
set.seed(1004)
train_sample<-sample(8124,5700)#5700
mush_train<-mush[train_sample,]
str(mush_train)
mush_test<-mush[-train_sample,]
mush_train_labels<-mushrooms[train_sample,1]
mush_train_labels
mush_test_labels<-mushrooms[-train_sample,1]
#우도표 생성
library(e1071)
mush_classifier<-naiveBayes(mush_train,mush_train_labels)
mush_test_pred<-predict(mush_classifier,mush_test)
library(gmodels)
CrossTable(mush_test_pred,mush_test_labels,
           prop.t = FALSE,prop.r = FALSE,
           dnn=c("predicted","actual"))
#라플라스 적용
mush_classifier2<-naiveBayes(mush_train,
                             mush_train_labels,laplace = 1)
mush_test_pred2<-predict(mush_classifier2,mush_test)
CrossTable(mush_test_pred2,mush_test_labels,
           prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))
# 라플라스 적용전 보다 128,6->108,5 만큼 줄어들음
# 0.945 -> 0.953