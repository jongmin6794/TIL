#텍스트 분석, r 함수
myvector<-c(1:6,'a')
myvector
mylist<-list(1:6,'a')
mylist

obj1<-1:4
obj2<-6:10
obj3<-list(obj1,obj2)
obj3

mylist<-list(obj1,obj2,obj3)
mylist
#벡터:[], 리스트:[[]]
mylist[[3]][1]
mylist[[3]][[1]]
#리스트에스 자료 추출시,[1]를 사용해서 리스트를 추출,
#[[1]] 사용하면 벡터를 추출할 수 있다.
mylist[[3]]
#unlist:리스트를 벡터형식으로 리턴
mylist<-list(1:6,'a')
mylist
unlist(mylist)
mylist
myvector==unlist(mylist)
myvector
mean(mylist[[1]][1:6])
mean(unlist(mylist)[1:6])#문자열?이라 에러
name1<-"Donald"
myspace<-" "
name2<-"Trump"
list(name1,myspace,name2)
unlist(list(name1,myspace,name2))
#unlist:하나의 문자 형태의 객체로 합치고자 할때
name<-c("갑","을","병","정")
gender<-c(2,1,1,2)
mydata<-data.frame(name,gender)
mydata
#attr():속성값을 저장하거나 추출할때 사용
#메타데이터:데이터의 데이터
#ex)gender의 메타데이터:성별을 의미함
attr(mydata$name,"what the variable means")<-"응답자 이름"
mydata$name
attr(mydata$gender,"what the variable means")<-"응답자 성별"
mydata$gender
myvalues<-gender
for(i in 1:length(gender)){
  myvalues[i]<-ifelse(gender[i]==1,"남성","여성")
}
myvalues

mydata$gender
attr(mydata$gender,"what the value means")<-myvalues
mydata$gender

mydata$gender.character<-
  attr(mydata$gender,"what the value means")
mydata
#리스트->lapply
mylist<-list(1:4,6:10,list(1:4,6:10))
mylist
lapply(mylist[[3]],mean)
#tapply는 텍스트 데이터에 대해 사용
wordlist<-c("the","is","a","the")
df1<-c(3,4,2,4)
#문서1에서 worldlist에 속한 단어가 등장한 횟수
#the:3, is:4, a:2, the:4
df2<-rep(1,4)#문서2
#문서2에서 worldlist에 속한 단어가 등장한 횟수
tapply(df1,wordlist,length)
tapply(df1,wordlist,sum)#가중치 계산
#the:1, is:1, a:1, the:1
tapply(df2,wordlist,length)
tapply(df2,wordlist,sum)
#알파벳 출력 함수
letters[3] #"c"
LETTERS[3] #"C"
letters[1:26]#"a","b","c",..,"z"
#nchar함수:문자수를 세는 함수
nchar("Korea")#5
nchar("한국")#2
nchar("Korea",type="bytes")#5
nchar("한국",type="bytes")#4,한글은 한글자당 2바이트
nchar("Korea ")#6공백도 포함
nchar("Korea\t")#6 \t 1글자 취급
nchar("Korea\t",type="bytes")#6 바이트도 1바이트취급
nchar("Korea, Republic of")#18
nchar("Korea, 
      Republic of")#25
nchar("Korea, \nRepublic of")#19

#문장을 단어로 분리
mysentence<-"Learning R is so interesting"
strsplit(mysentence,split=" ")
#단어를 문자로 분해
mywords<-strsplit(mysentence,split=" ")
mywords
strsplit(mywords[[1]][5],split="")
#"i" "n" "t" ... "i" "n" "g"
#초기화
myletters<-list(rep(NA,5))
myletters

for (i in 1:5){
  myletters[i]<-strsplit(mywords[[1]][i],split="")
}
myletters

#문자를 합쳐서 단어로 구성
paste(1,2,3)
paste(myletters[[1]],collapse = '#')#"L#e#a...#g"
paste(myletters[[1]],collapse = '')#"Learning"

mywords2<-list(rep(NA,5))
for (i in 1:5){
  mywords2[i]<-paste(myletters[[i]],collapse="")
}
mywords2
paste(mywords2,collapse = " ")

rwiki<-"R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
#문단단위로 구분
rwikipara<-strsplit(rwiki,split='\n')
#점 공백이 문장을 나누는 기호
rwikisent<-strsplit(rwikipara[[1]],split = '\\. ')
rwikiword<-list(NA,NA)
rwikiword
for(i in 1:2){
  rwikiword[[i]]<-strsplit(rwikisent[[i]],split=" ")
}
rwikiword
#language 찾기
rwikiword[[1]][[2]][3]

#regexpr함수:정규표현식, 
#처음 등장하는 텍스트 위치 출력
mysentence<-"Learning R is so interesting"
regexpr("ing",mysentence)
class(regexpr("ing",mysentence))#integer
loc.begin<-as.vector(regexpr("ing",mysentence))#6
loc.begin
loc.length<-attr(regexpr('ing',mysentence),'match.length')
loc.end<-loc.begin+loc.length-1
#grepexpr은 패턴이 등장하는 모든 텍스트 위치 출력
gregexpr('ing',mysentence)
#발견된 패턴 수
length(gregexpr('ing',mysentence)[[1]])
loc.begin<-as.vector(gregexpr("ing",mysentence)[[1]])#6 26
loc.begin
loc.length<-attr(gregexpr('ing',mysentence)[[1]],'match.length')#3 3
loc.length
loc.end<-loc.begin+loc.length-1
loc.end#8 28
regexpr("interesting",mysentence)
regexec("interestin(g)",mysentence)#괄호 같이 검색
regexec("so (interestin(g))",mysentence)
mysentence
mysentences<-unlist(rwikisent)

regexpr("software",mysentences)#-1은 단어가 없음을 의미
gregexpr("software",mysentences)
sub('ing','ING',mysentence)#맨처음 찾는거 대체
gsub('ing','ING',mysentence)#찾는거 모두 대체
mytemp<-regexpr("software",mysentences)
my.begin<-as.vector(mytemp)
my.begin
my.begin[my.begin==-1]<-NA
my.begin
my.end<-my.begin+attr(mytemp,"match.length")-1
my.end
length(my.begin)
mylocs<-matrix(NA,nrow=length(my.begin),ncol=2)
mylocs
colnames(mylocs)<-c("begin",'end')
rownames(mylocs)<-paste("sentence",1:length(my.begin))
mylocs
#paste("hi",1:3,sep=".")
#paste("hi","hello")
for(i in 1:length(my.begin)){
  mylocs[i,]<-cbind(my.begin[i],my.end[i])
}
mylocs
#grep(위치 알려줌), grepl(true, false):특정 표현이 텍스트에 있는지 확인
mysentences
grep('software',mysentences)
#1, 2, 5번 문장에서 표현이 발견
grepl('software',mysentences)
#고유명사 처리
#"Donald Trump"=>"Donald_Trump"
rwikisent
sent1<-rwikisent[[1]][1]
sent1
new.sent1<-gsub("R Foundation for Statistical Computing",
                "R_Foundation_for_Statistical_Computing",sent1)
sent1 #단어 개수 출력
new.sent1 #단어 개수 출력
sum(table(strsplit(sent1,split =' ')))
sum(table(strsplit(new.sent1,split =' ')))

#단어 제거
drop.sent1<-gsub("and |by |for |the","",new.sent1)
sum(table(strsplit(drop.sent1,split=" ")))

mysentence
mypattern<-regexpr("ing",mysentence)
regmatches(mysentence,mypattern)

mypattern<-gregexpr("ing",mysentence)
regmatches(mysentence,mypattern)

#invert옵션 : 반대 표현
mypattern<-regexpr("ing",mysentence)
regmatches(mysentence,mypattern,invert = TRUE)

mypattern<-gregexpr("ing",mysentence)
regmatches(mysentence,mypattern,invert = TRUE)
strsplit(mysentence, split = "ing")
gsub('ing','',mysentence)

substr(mysentence,1,20)#1~20출력
substr(mysentences,1,20)

my2sentence<-c("Learning R is so interesting",
               "He is a fascinating singer")
#ing로 끝나는 모든 단어를 검출
mypattern0<-gregexpr('ing',my2sentence)
regmatches(my2sentence,mypattern0 )

#ing앞에 알파벳 표현 확인 => [[:alpha:]]
mypattern1<-gregexpr("[[:alpha:]]+(ing)",my2sentence)
regmatches(my2sentence,mypattern1 )

#[[:alpha:]]+ing => \\b
mypattern2<-gregexpr("[[:alpha:]]+(ing)\\b",my2sentence)
regmatches(my2sentence,mypattern2 )

#7개 문장 모두에 대해 ing로 끝나는 영어 단어 출력
mypattern3<-gregexpr("[[:alpha:]]+(ing)\\b",mysentences)
myings<-regmatches(mysentences,mypattern3 )

table(unlist(myings))
#문서 전체에서 ing로 끝나는 영어 단어를 모두 
#추출하고, 빈도수를 조사

mypattern<-gregexpr("[[:alpha:]]+(ing)\\b",tolower(mysentences))
mypattern
myings<-regmatches(tolower(mysentences),mypattern3 )
table(unlist(myings))

#대소문자 구분없이 stat~ 시작되는 단어 추출
mypattern<-gregexpr("(stat)[[:alpha:]]+",tolower(mysentences))
regmatches(tolower(mysentences),mypattern )

mypattern=gregexpr("[[:upper:]]",mysentences)
my.uppers<-regmatches(mysentences,mypattern)
table(unlist(my.uppers))

mypattern=gregexpr("[[:lower:]]",mysentences)
my.lowers<-regmatches(mysentences,mypattern)
table(unlist(my.lowers))

mypattern=gregexpr("[[:upper:]]",toupper(mysentences))
my.alphas<-regmatches(toupper(mysentences),mypattern)
mytable<-table(unlist(my.alphas))
mytable
mytable[mytable==max(mytable)]
sum(mytable)
library(ggplot2)
class(mytable)
#ggplot할때 데이터프레임으로 변환한 다음 시각화
mydata<-data.frame(mytable)
ggplot(data=mydata, 
       aes(x=Var1, y=Freq, fill=Var1))+
  geom_bar(stat='identity')+
  #guides(fill=FALSE)+ #범례나타낼지 말지
  geom_hline(aes(yintercept=median(mytable)))+
  xlab("알파벳")+
  ylab("빈도수")

#문제1 베이지안필터기 제작
#2. POS/NEG 단위로 알파벳 문자 많이 사용 적게사용 빈도 확인
movie_pang<-read.csv("Data/movie-pang02.csv")
str(movie_pang) #2000 raw, 2 col
colSums(is.na(movie_pang))#결측값 확인
movie<-movie_pang[-1]#라벨 제거
set.seed(1004)
movie