library(arules)
data("Epub")
summary(Epub)
inspect(Epub[1:10])
itemFrequency(Epub[,1:10])
itemFrequencyPlot(Epub,support=0.01,topN=20,
                  main="item frequency")
image(sample(Epub,500))
epub_rule<-apriori(data=Epub,
                   parameter = list(support=0.001,
                                    confidence=0.2,
                                    minlen=2))
summary(epub_rule)
inspect(epub_rule)
inspect(sort(epub_rule,by="lift")[1:20])

rule_ins<-subset(epub_rule,items %in% #l,r어디든 하나이상
                   c("doc_72f","doc_4ac"))
inspect(rule_ins)
rule_ins<-subset(epub_rule,lhs %in% #l 하나이상
                   c("doc_72f","doc_4ac"))
inspect(rule_ins)
rule_ins<-subset(epub_rule,rhs %in% #r 하나이상
                   c("doc_72f","doc_4ac"))
inspect(rule_ins)
#%in%은 적어도 하나의 제품이 
#존재하면 해당 규칙을 가져옴
rule_ins<-subset(epub_rule,items %pin% #60e부분 포함
                   c("60e"))
inspect(rule_ins)
rule_ins<-subset(epub_rule,lhs %ain% #60e부분 포함
                   c("doc_6e8", "doc_6e9"))
#ain은 온전히 일치
inspect(rule_ins)
rule_ins<-subset(epub_rule,items %pin% #60e부분 포함
                   c("60e")&confidence>0.25)
inspect(rule_ins)
install.packages("arulesViz")#???
library(arulesViz)
plot(epub_rule)
plot(sort(epub_rule,by="support")[1:20],method="grouped")
plot(epub_rule,method="graph",
     control=list(type="times"),
     vertex.label.cex=0.7,#점의 크기 디폴트는1
     edge.arrow.size=0.3,#화살표의 크기
     edge.arrow.width=2
     )
#원의 크기:지지도에 비례
#원의 색깔:향상도(lift)에 비례
#화살표:lhs->rhs
#보라색책 p.225 3.5군집 k-means
#클러스터링, k-means
#snsdata.csv 미국 10대 sns 단어 데이터 클러스터링
#기업 타깃 마케팅 활용-유사 구매패턴 그룹 나눔
#무단 네트워크 침입탐지 활용-패턴 다름. 
#포트스캐닝(port scanning) 주로 침입패턴.
#실제 안랩 등 보안솔루션 포트스캐닝(피쳐중 1개) 
#기반으로 많이 만들어짐

teens<-read.csv("Data/snsdata.csv")
#View(teens)
str(teens)
table(teens$gender,useNA = "ifany")#useNa na도 포함함
summary(teens$age)
teens$age<-ifelse(teens$age>=13&teens$age<20,teens$age,NA)
#나이 3살, 100살 이상 존재 하므로 이상치 전처리
summary(teens$age)

teens$female<-ifelse(teens$gender=='F'& 
         !is.na(teens$gender),1,0)
table(teens$female)
teens$no_gender<-ifelse(is.na(teens$gender),1,0)
teens$male<-ifelse(teens$gender=='M'& 
                       !is.na(teens$gender),1,0)
table(teens$female)
table(teens$no_gender)
table(teens$male)
#각 해당하는 젠더의 값에 1값을 주어(각각 새로운 컬럼)
#한눈에 보기 쉽도록 처리

mean(teens$age,na.rm=TRUE)
myagg<-aggregate(data=teens,age~gradyear,mean,na.rm=TRUE)
myagg
#그룹(졸업연도)에 대한 통계(평균)계산
#졸업연도~우측 기준으로 ~왼쪽을 그룹화
#데이터프레임으로 출력

avg_age<-ave(teens$age, teens$gradyear,
             FUN=function(x) mean(x,na.rm=TRUE))
#두번째 인자 gradyear기준으로 첫인자 age 의 mean구함
avg_age#벡터 데이터
teens$age<-ifelse(is.na(teens$age),avg_age,teens$age)
summary(teens$age)

interests<-teens[5:40]

interests_z<-as.data.frame(lapply(interests,scale))
head(interests_z)
set.seed(2345)
teen_clusters<-kmeans(interests_z,5)
teen_clusters$size
teen_clusters$centers
teen_clusters$cluster
teens$cluster<-teen_clusters$cluster
teens[1:5,c("cluster","gender","age","friends")]

#클러스터 단위로 나이 평균
aggregate(data=teens,age~cluster,mean)
aggregate(data=teens,friends~cluster,mean)
#teens$female
aggregate(data=teens,female~cluster,mean)

#아이리스 data(3개 그룹)
#=>3개 그룹(정확도?)
iris
iris_data<-iris[-5]
iris_data_name<-iris[5]
iris_data_z<-as.data.frame(lapply(iris_data,scale))
head(iris_data_z)
set.seed(1004)
iris_data_z_cluster<-kmeans(iris_data_z,3)
iris_data_z_cluster$cluster
iris_data_z_cluster$centers
iris_data_z$cluster<-iris_data_z_cluster$cluster
iris_data_z_cluster_cluster<-iris_data_z_cluster$cluster
#iris_data_z_cluster_cluster<-as.data.frame(iris_data_z_cluster_cluster)
iris$cluster<-iris_data_z_cluster_cluster
table(iris$Species,iris$cluster)
install.packages("fpc")
library(fpc)
library(cluster)
library(ggplot2)
iris_data_z
iris_new<-iris[-5]
iris_new
plotcluster(iris_data, iris_data_z_cluster_cluster, color=TRUE, shade=TRUE)
ggplot(data=iris_new,aes(x=v1,y=v2,color=clustering,shape=clustering))+geom_point()
plot(iris_new)
