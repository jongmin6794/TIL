install.packages("rvest")#r크롤링위해
library(rvest)
library(dplyr) #require(패키지명)

#문제
#네이버or신문사or포털 등
#기사 추출->tfidf 구성->상관계수
#기사 키워드 추출
#경제신문 ex)오늘, 어제, 그제, ...
#웹 스크래핑

#1. 포털, 신문사, 언론사,...
#TEXT -> DTM -> TFIDF 구성 -> 상관계수

#2.한글 논문 문서
#DTM => TFIDF 구성
#TF는 높고, IDF가 낮은 단어 추출

#doc1~doc10 
#corpus->dtm->tfdif->cor

#read_html(), 
#html_node()/html_nodes(),
#html_text()

#tv.naver.com/jtbc.youth
url_tvcast<-"http://tv.naver.com/jtbc.youth"
html_tvcast<-read_html(url_tvcast,encoding = "UTF-8")
#class가 title인 부분 안에 있는 a태그의 내용 추출
html_tvcast %>% html_nodes(".title a")
#html_node():매칭된 요소 하나 추출
#html_nodes():모든 요소 추출(class, tag)
html_tvcast %>% 
  html_nodes(".title a") %>% 
  html_text()
tvcast_res<-html_tvcast %>%  # 여기 코드 다시
  html_nodes(".title a") %>% 
  html_text()
tvcast_res
tvcast_df<-html_tvcast %>%  # 여기 코드 다시확인
  html_nodes(".title a") %>% 
  html_text() %>% 
  data.frame()
str(tvcast_df)

#https://en.wikipedia.org/wiki/Student%27s_t-distribution
url_t<-"https://en.wikipedia.org/wiki/Student%27s_t-distribution"
html_t<-read_html(url_t,encoding = "UTF-8")
#class가 title인 부분 안에 있는 a태그의 내용 추출
html_t %>% html_nodes(".wikitable") %>% 
  html_table()

#html_text():텍스트 추출
#html_name():attribute 명을 추출
#html_childer():하위 요소 추출
#html_tag():태그명 추출
#html_attrs:속성을 추출

#자바스크립트로 된 문서는 스크래핑 불가

library(httpuv)
library(rgdal)
library(geojsonio)
library(rgeos)
library(KoNLP)
library(tm)

sentence <- '아버지가 방에 들어가신다'
str_replace_all(sentence,"방에","가방에")
extractNoun(sentence)

my.text.location<-"C:/JMOh/refer_data/ymbaek_refer"
mypaper<-VCorpus(DirSource(my.text.location))
mypaper
mykorean<-mypaper[[19]]$content
#전처리
library(stringr)
mykorean
mytext<-str_replace_all(mykorean,"[[:lower:]]","")
mytext
mytext<-str_replace_all(mytext,"\\(","")
mytext<-str_replace_all(mytext,"\\)","")
mytext<-str_replace_all(mytext,"’","")
mytext<-str_replace_all(mytext,"‘","")
mytext<-str_replace_all(mytext," · ","")
removePunctuation(mytext)
mytext

#명사 추출
noun.mytext<-extractNoun(mytext)
noun.mytext
table(noun.mytext)
