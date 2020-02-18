library(readr)
IMDB<-read_csv("Data/imdb-dataset-of-50k-movie-reviews/IMDB dataset.csv")
str(IMDB)#215519행, 2열 ?? 엑셀파일 행은 5만행인데 왜 알에선 21만??;;
#library readr 사용해서 해결... 이유는 몰라... 그저 더 상위 버전인가봄;;
names(IMDB)#review, sentiment
table(IMDB$sentiment)## neg 25000, posi 25000
IMDB$sentiment<-factor(IMDB$sentiment)
IMDB_1<-IMDB[1:5000,]
IMDB_review<-IMDB_1$review
library(stringr)
IMDB_review[1:10]
IMDB_review<-str_replace_all(IMDB_review,"<br />"," ")#br태그 제거
IMDB_review[1:10]

library(tm)
IMDB_corpus<-VCorpus(VectorSource(IMDB_review))
colSums(is.na(IMDB))#결측값 존재 x
as.character(IMDB_corpus[[1]])#데이터 내용 확인

IMDB_dtm<-DocumentTermMatrix(IMDB_corpus,
                             control = list(tolower=TRUE,#소문자로 적용
                                            removeNumbers=TRUE,#숫자 제거
                                            stopwords=TRUE,#불용어 제거
                                            removePunctuation=TRUE,#구두점 제거
                                            stemming=TRUE#
                             ))
str(IMDB_dtm)#row 5000 33047개의 단어
set.seed(1004)#균일한 데이터 추출을 위해 랜덤 row 추출
train_sample<-sample(5000,3500)
#train과 test 분류(70:30) labels정의 
IMDB_train<-IMDB_dtm[train_sample,]
IMDB_test<-IMDB_dtm[-train_sample,]
IMDB_train_labels<-IMDB_1[train_sample,]$sentiment
IMDB_test_labels<-IMDB_1[-train_sample,]$sentiment

table(IMDB_train_labels)#3500 neg 1771, posi 1729기준 
class(IMDB_test_labels)
#35000개기준 neg 17567, posi 17533
IMDB_train_freq<-findFreqTerms(IMDB_train,10)#최소 단어 갯수기준 분류
str(IMDB_train_freq)#5000기준 10->4524개
#50000기준 최소 20->11870개, 5->25661개, 10->17595개
#최소단어 10개의 단어로 컬럼 재구성
convert_counts<-function(x){#단어 카운트 대신 단어 존재 자체로 판단
  x<-ifelse(x>0,"Yes","No")
}
#행렬의 열/행 단위로 전달(apply,MARGIN=1(행),2(열))
IMDB_dtm_freq_train<-IMDB_train[,IMDB_train_freq]
IMDB_dtm_freq_test<-IMDB_test[,IMDB_train_freq]
IMDB_convert_train<-apply(IMDB_dtm_freq_train,MARGIN = 2,convert_counts)
IMDB_convert_test<-apply(IMDB_dtm_freq_test,MARGIN = 2,convert_counts)

IMDB_dtm_freq_train<-as.matrix(IMDB_dtm_freq_train)
IMDB_dtm_freq_test<-as.matrix(IMDB_dtm_freq_test)

library(e1071)#베이지안 필터사용을 위한 라이브러리
#우도표 생성
IMDB_classifier<-naiveBayes(IMDB_convert_train,IMDB_train_labels)
IMDB_classifier2<-naiveBayes(IMDB_dtm_freq_train,IMDB_train_labels)

IMDB_test_pred<-predict(IMDB_classifier,IMDB_convert_test)
IMDB_test_pred2<-predict(IMDB_classifier2,IMDB_dtm_freq_test)

library(gmodels)
CrossTable(IMDB_test_pred,IMDB_test_labels,# 단어 10개 이상 1262/1500 약 0.84
           prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))

CrossTable(IMDB_test_pred2,IMDB_test_labels,# 단어 10개 이상 1035/1500 약 0.69
           prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))

#라플라스 적용 pred 기준
IMDB_classifier2<-naiveBayes(IMDB_convert_train, #1269/1500 약 0.85
                             IMDB_train_labels,laplace = 1)
IMDB_test_laplace_pred2<-predict(IMDB_classifier2,IMDB_convert_test)
CrossTable(IMDB_test_laplace_pred2,IMDB_test_labels,
           prop.t = FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))
