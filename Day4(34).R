#지도학습(supervised)
#예측값의 모습을 표현하는 
#용어: 편향(bias), 분산(variance)
#편향? 예측값들과 정답이 어느정도 떨어져 있는지를
#나타내는 용어
#bias높다=>정답과 예측값이 차이가 크다
#분산? 예측값들끼리 서로 얼마나 멀리 떨어져 있느냐를
#나타낸 것
#variance높다=>예측값들끼리 서로 멀리 떨어져 있다.

#x:데이터, f(x):정답(f위에 ^표시 있으면 예측값), E[ ]:기대값(평균)
#편향수식f^는 f위에^:( E[f^(x)]-f(x) )**2
#분산수식:E[ f^(x)-E[f^(x)] ]**2
#Error(x)=현향수식+분산수식+e(indd) redueible error 근본적인 오류
#overfiting
#unerfiging
#편향이 높아지면 분산이 낮아지는 경향이 존재
#모델의 복잡도? 데이터를 학습하는 횟수

#1.트레이닝 데이타로 모델 적용
#2.validation data 에 대해 과적합.과소적합 적용 검증->최적적합
#3.테스트 데이터 최적모델로 test

##연관규칙!! 장바구니!!
#-연관성 찾기, 거래data 특성, 찾기, 패턴 식별
#아이템, 아이템 집합{빵, 버터, 우유, 껌}
#{버터,빵}->{우유} 연관규칙 대표적 알고리즘(Apriori)
#  LHS        RHS
#암데이터 분석에 많이 쓰임(DNA패턴, 단백질 서열검색)
#사기성 신용카드 사용, 부당 의료비(보험)청구패턴 찾기
#휴대폰 변심? 선행되는 동작 패턴 식별 
#시간의 흐름에 따른 구매 패턴
##추천시스템
#1.연관규칙 함께 구매가 발생하는 규칙
#2.C.F(Collaboration Filtering):상관계수
#3.순차분석:시간의 흐름에 따른 구매 패턴
#데이터가 충분히 많아야 한다. 통찰필요 해석 스스로
#domain knowledge(배경지식?)

#지지도(support)=item 집합이 나타나는 트랜잭션(거래) 비율
#x,y 지지도 = x,y모두 포함하고 있는 거래의 수/전체 거래 수
#1.계란, 우유                    
#2.계란, 기저귀, 맥주, 사과                   
#3.우유, 기저귀, 맥주, 콜라      
#4.계란, 우유, 맥주, 기저귀
#5.계란, 우유, 맥주, 콜라
#연관규칙{계란,맥주}->{기저귀}
#            x           y
#지지도? 2건(2,4)/5건(1,2,3,4,5)=0.4

#신뢰도(confidence)?
#항목집합x를 포함하는 거래중에서, 
#항목집합y도 포함하는 거래 비율 (조건부 확률)
#x와 y를 모두 포함하는 거래수/x가 포함된 거래수
#신뢰도? 2건(2,4)/3건(2,4,5)=2/3=0.667

#향상도(lift)
#항목집합 x가 주어져 있지 않은 상황에서,
#항목집합 y의 확률 대비 항목집합 x가 주어졌을때,
#항목집합 y의 확률 증가 비율
#=신뢰도/지지도 => c(x->Y)/S(Y)  c:신뢰도, s:지지도

#{a,b,c,d}=2^4
#연관규칙 수=3^K-2^(K+1)+1 =>3^4-2^5+1=50
#너무나도 많은 연관규칙 수가 나오기 때문에
#사람이 구하기 힘듬... 컴퓨터 계산으로 얻어낸다.
#1000개만 되어도 3의 천승...
#pruning(가지치기) 더이상 해가 될 가능성이 없다면
#애초에 제거(연관규칙 안될거 같은거 제거?)

#1.최소지지도<-이상을 갖는 집합:빈발항목집합
#아프리알고리즘 핵심:모든 항목집합 지지도를 조사 못함...
#최소지지도이상 갖는 빈발항목집합만을 찾아서 연관규칙 적용
#x가 빈발항목집합이면, 부분집합도 빈발항목집합으로 볼 수 있다.
#2.한 항목집합(item set)이 비 빈발 하다면,
#이 항목 집합을 포함하는 모든 집합은 비 빈발 집합
#{사과}=0.1 => {사과,딸기}=0.1같거나 이하
install.packages("arules")#apriori쓰기 위해
library(arules)
#groceries<-read.csv("Data/groceries.csv",sep=",")
#groceries 아이템 갯수 들쭉날쭉 데이터 위처럼 읽으면 안됨...
#str(groceries)
groceries<-read.transactions("Data/groceries.csv",sep=",")
summary(groceries)
#sparse 희소행렬(0이 많은 행렬) 반대는 밀집행렬
#169:거래내역에서 정체상품의 종류
#1~32:거래 당 구매 상품의 개수
inspect(groceries[1:5])
itemFrequency(groceries[,1:3])
itemFrequency(groceries[,150:169])
itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=20)
image(groceries[1:5])
image(sample(groceries,100))#sample 임의의 데이터 100개 뽑아냄
groceryRules<-apriori(groceries,
                      parameter = list(support=0.006,
                                       confidence=0.25,
                                       minlen=2))
#minlen=2개 미만의 아이템을 갖는 규칙은 제거
summary(groceryRules)

inspect(groceryRules[1:3])
inspect(sort(groceryRules,by="lift")[1:5])
#3.956477의 의미는 대략4
#허브를 산 사람들이 채소를 살 가능성이
#채소를 산 일반고객보다 4배가 더 높다.

berryRules<-subset(groceryRules,items %in% "berries")
inspect(berryRules)
berryRules<-subset(groceryRules,items %in% c("berries","yogurt"))
inspect(berryRules)
write(groceryRules,file='groceryRules.csv',sep=",")
grdf<-as(groceryRules,"data.frame")
#grdf<-as.data.frame(groceryRules)
grdf
str(grdf)

#20003~2008 전자책 거래 데이터 
help(Epub)
data(Epub)#데이터 로드
summary(Epub)
#문제1 Epub 데이터 연관규칙 apriori
library(arules)
data("Epub")
summary(Epub)
Epub
epub<-Epub
write(epub,file='epub.csv',sep=",")
str(epub)
inspect(epub[1:5])
summary(epub)
#most = doc_11d:356, doc_813:329, doc_4c6:288
#rows:157929, columns:936
#size 1:11615, 2:2189 ~ 58:1
itemFrequencyPlot(epub[,1:3])
image(epub[1:5])
itemFrequencyPlot(epub,topN=20)
itemFrequencyPlot(epub,support=0.01)
image(sample(epub,100))
epubRules<-apriori(epub,parameter = list(support=0.002,
                                         confidence=0.2,
                                         minlen=2))
summary(epubRules)
inspect(sort(epubRules,by="lift")[1:5])
epubRules_df<-as(epubRules,"data.frame")
plot(epubRules_df$support)