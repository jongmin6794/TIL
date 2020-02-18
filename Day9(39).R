install.packages("stringr")
library(stringr)
rwiki<-"R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
str_extract(rwiki,"software environment")
#regexpr(), regmatches()거의 유사
str_extract_all(rwiki,"software environment")#리스트형
str_extract_all(rwiki,"software environment",simplify = TRUE)#매드릭스형
#첫 문자가 대문자로 시작하는 단어 추출
myextract<-str_extract_all(rwiki,"[[:upper:]]{1}[[:alpha:]]{0,}")
#{1}대문자 1번 나와야함
#[[:alpha:]]{0,1}:알파벳 문자 0개 이상
table(myextract)
str_locate(rwiki,"software environment")
#base : regexpr, gregexpr
str_extract_all(rwiki,"software environment")
#첫번째 글자가 대문자로 시작되는 단어들의 위치 모두 출력
mylocate<-str_locate_all(rwiki,"[[:upper:]]{1}[[:alpha:]]{0,}")
dim(mylocate[[1]])
mydata<-data.frame(mylocate[[1]])#df타입
mydata
mydata$myword<-myextract[[1]]
mydata
# mydata에 myword.length열 추가
# myword.length에는 myword 길이가 저장
mydata$myword.length<-str_length(myextract[[1]])
mydata
mydata$myword.length2<-mydata$end-mydata$start+1
mydata
str_replace(rwiki,"software environment","software_environment")
temp<-str_replace_all(rwiki,"software environment","software_environment")
str_extract_all(temp,"software_environment|software|environment")
str_extract_all(rwiki,"software_environment|software|environment")
table(str_extract_all(rwiki,"software_environment|software|environment"))

#R
temp<-str_replace_all(rwiki,"R","R_computer.language_")
temp<-str_replace_all(temp,"C","C_computer.language_")
#temp에서 _computer.language_ 표현이 붙은 부분에는 
#어떤 단어들이 있고, 빈도가 어떤지 출력
# C_computer.language_  R_computer.language_
#          2                     9
table(str_extract_all(temp,"[[:alpha:]]{1}_computer.language_"))

#텍스트 데이터의 문단을 구분(줄바꿈)
rwiki
rwikipara<-str_split(rwiki,"\n")
rwikipara

#문단별로 문장을 구분(.)
rwikisent<-str_split(rwikipara[[1]],"\\. ")

#str_split_fixed함수
class(rwikisent)
rwikisent[[2]]
# stringr패키지 대다수 리스트로 출력
my2sentences<-unlist(rwikisent)[c(4,7)]
#unlist:list->vector

my2sentences
#각 문장의 단어수를 출력
mylength1<-sum(table(str_split(my2sentences[[1]]," ")))
mylength2<-sum(table(str_split(my2sentences[[2]]," ")))
mylength1;mylength2
mylength1<-length(unlist(str_split(my2sentences[1]," ")))
mylength2<-length(unlist(str_split(my2sentences[2]," ")))
mylength1;mylength2
#str_split_fixed함수
myfixed.short<-str_split_fixed(my2sentences," ",5)
#str_split(my2sentences[2]," ",n=5)
#4까지 단어 구분, 5부터 문장
myfixed.long<-str_split_fixed(my2sentences," ",13)
#12까지 단어 구분, 13부터 문장으로 출력
myfixed.short
myfixed.long

#rwikisent 문장*단어
rep(3,5)# 3 3 3 3 3 앞에 수 뒤에 갯수 만큼 출력
length.sentences<-rep(NA,length(unlist(rwikisent)))
length.sentences#
rwikisent
for(i in 1:length(length.sentences)){
  length.sentences[i]<-
    length(unlist(str_split(unlist(rwikisent)[i]," ")))
}
max.length.sentences<-max(length.sentences)
str_split(rwikisent[[1]]," ")
str_split(unlist(rwikisent[1])," ")

sent.word.matrix<-str_split_fixed(unlist(rwikisent)," ",
                                  max.length.sentences)
sent.word.matrix
mydata<-data.frame(sent.word.matrix)
mydata

#행과 열 이름 바꿔주기
rownames(mydata)#sent.1 sent.2 ... sent.7
names(mydata)
colnames(mydata)#word.1 ... word.21
paste("abc",1:5,sep=".")
rownames(mydata)<-paste("sent",1:length(rownames(mydata)),sep=".")
colnames(mydata)<-paste("word",1:length(colnames(mydata)),sep=".")
mydata

mydata[,1]
mydata[3,1:10]

#R
rwiki
str_count(rwiki,"R")#R 등장 횟수
str_count(rwikipara,"R")
rwikipara[[1]]
str_count(rwikipara[[1]],"R")
rwikisent
str_count(rwikisent[[1]],"R")#1번째 문단의 문장들만 체크
#따라서 전체 문단 확인 필요 unlist사용
str_count(unlist(rwikisent),"R")

#R이라는 단어가 등장한 후에
#stat으로 시작하는 단어가 등장하는 빈도
str_count(unlist(rwikisent),"R(.)+stat")
str_count(unlist(rwikisent),"R.{1,}stat")
# s,S 대소 구분이 필요 없는 경우
str_count(unlist(rwikisent),"R.{1,}(s|S)tat[[:lower:]]{1,}")
str_extract_all(unlist(rwikisent),"R.{1,}(s|S)tat[[:alpha:]]{1,}")
#R과 stat사이에 R이라는 표현이 있으면 안됨
str_count(unlist(rwikisent),
          "R[[:lower:][A-Q][s-z][:digit:][:space:]]{1,}(s|S)tat[[:alpha:]]{1,}")
str_count(unlist(rwikisent),"R{1}[^R]{1,}(s|S)tat[[:alpha:]]{1,}")
str_sub(unlist(rwikisent[1],1,30))

str_dup("software",3)
rep("software",3)
paste(rep("software",3),collapse = "")
str_dup("software",3)==paste(rep("software",3),collapse = "")

str_length(unlist(rwikisent))#글자 수 카운트
nchar(unlist(rwikisent))#글자 수 카운트
name<-c("Joe","Jack","Jackie","Jefferson")
donation<-c("$1","$111","$11111","$1111111")
mydata<-data.frame(name,donation)
mydata

name2<-str_pad(mydata$name,width = 15,side="right")
name2
#side=right 공백문자를 오른쪽으로
str_pad(mydata$name, width=15, side="left")
str_pad(mydata$name, width=15, side ="both")
donation2<-str_pad(mydata$name, width =15, side = "both",pad="~")
name2
donation2
mydata2<-data.frame(name2,donation2)
mydata2
str_length(mydata2$name2)
str_length(mydata$name)

#패딩된 공백 문자를 제거
name3<-str_trim(mydata2$name2, side = 'right')
name3
#양쪽에 패딩(~)기호를 모두 제거
mydata2$donation2
dd<-str_replace_all(mydata2$name2,'~',' ')#물결을 공백으로 변환 뒤 제거
name4<-str_trim(dd,side = 'right')
name4
#혹은 remove
name4<-str_remove_all(mydata2$name2,'~')
name4
#양쪽에 패딩(~)기호를 모두 제거
donation3<-str_trim(str_replace_all(mydata2$donation2,"~"," ")
                    ,side="both")
mydata3<-data.frame(name3,donation3)
mydata
all(mydata3==mydata)

mytext<-c("software environment",
          "software  environment",
          "software\tenvironment")
mytext
#white space(공란) 제거
str_split(mytext," ")

sapply(str_split(mytext," "),length)
lapply(str_split(mytext," "),length)

mytext.nowhitespace<-
  str_replace_all(mytext,"[[:space:]]{1,}"," ")
mytext.nowhitespace

sapply(str_split(mytext.nowhitespace," "),length)
lapply(str_split(mytext.nowhitespace," "),length)

mytext<-"The 45th President of the United States,
Donald Trump, states that he knows how to play trump with the former president"
#스플릿 공백으로 구분할 수있지만, boundary로도 가능
#(차이점:단어뒤에,제거)
str_extract_all(mytext,boundary("word"))
#대통령Trump와 악기 trump다름, 
#앞에 45th president와 뒤에 former president 다름
#따라서 45th_president식으로 묶어 구별
myword<-unlist(str_extract_all(mytext,boundary("word")))
table(myword)
table(tolower(myword))
myword<-str_replace(myword,"Trump","Trump_unique_")
myword<-str_replace(myword,"States","States_unique_")
table(myword)

mytext<-c("He is one of statisticians agreeing that R is the No. 1 statistical software.","He is one of statisticians agreeing that R is the No. one statistical software.")
mytext
#숫자 제거
mytext2<-str_split(str_replace_all
          (mytext,"[[:digit:]]{1,}[[:space:]]{1,}","")," ")
str_c(mytext2[[1]],collapse = " ")#str_c? collapse?
str_c(mytext2[[2]],collapse = " ")
#숫자 자료임을 표시
mytext3<-str_split(str_replace_all
                   (mytext,"[[:digit:]]{1,}[[:space:]]{1,}","_number_")," ")
mytext3
mytext<-"Kim et al. (2020) argued that the state of"
str_split(mytext,"\\. ")
#성 et al. (년도) => 하나의 단어로 교체 
#                 => _reference_

mytext<-c("She is an actor", 
          "She is the actor")
#a an the 불용어
mystopword<-"(an )|(the )"
#mystopword<-"(\\ban )|(\\bthe )"
str_replace_all(mytext,mystopword,"")

library('tm')#텍스트 마이닝 약자
length(stopwords("en"))
length(stopwords("SMART"))

#어근동일화 프로그램
mytext<-c("I am a boy. You are a boy. He might be a boy")
mystemmer.func<-function(mytextobj){
  #am, are, is, was, were, be => be
  mystopword<-"(am)|(are)|(is)|(was)|(were)|(be)"
  str_replace_all(mytext,mystopword,"be")
}
mytext.stem<-mystemmer.func(mytext)
mytext.stem

#p.514 word2vec(word to vector) 자연어 처리 대표적 5년전 이야기...
#최근 추가 BERT, Glove
#감성어휘 사전=>감성분석
install.packages("tidytext")
library(tidytext)
install.packages("tidyr")
library(tidyr)

install.packages("textdata")
library(textdata)
get_sentiments("bing")#tidytext
mynrc<-data.frame(get_sentiments("nrc"))#install textdata
mynrc
table(mynrc$sentiment)#10개 감정
#각각 감정에 해당하는 단어들 카운트 하여 판별

#문제1
#글자 n-gram기반 유사도
#n=2
#오늘,늘 , 강, ..., 다.
#강남, 남에, ..., 다.
#n=2,n=3 유사도 출력?
data<-c("오늘 강남에서 맛있는 스파게티를 먹었다.",
        "강남에서 먹었던 오늘의 스파게티는 맛있었다.")
data1<-data[1]
data2<-data[2]
n-gram<-function(data,num){
  for i in(2:length(data)){
    
  }
}


#문제2
#감성사전 기반 감성분석, 
#bing(P or N), nrc(10가지) => 영문장 입력 => 감성분석 결과
#(dp)레벤슈타인 거리 알고리즘


