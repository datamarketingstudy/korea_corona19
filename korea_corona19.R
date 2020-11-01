library(XML)
library(RCurl)
library(tidyverse)


# 서비스 URL

serviceURL <- "http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19InfStateJson"

# 오퍼레이션명

## operation <- "Covid19InfStateJson" 

# 요청변수
## 오픈 API 호출 시 얻게 되는 데이터의 개수

rows <- 100

## 페이지 위치

pg <- 1

## 인증키

ServiceKey <- "yourservicekey" ## https://www.data.go.kr/data/15043376/openapi.do 회원가입 후 활용신청 

## 조회기간

dt <- paste0("&startCreateDt=20200101&endCreateDt=",gsub("-","",as.character(Sys.Date())))


# 오픈 API 호출 URL 생성

url <- paste0(serviceURL,
              paste0("?serviceKey=",ServiceKey),
              paste0("&numOfRows=",rows),
              paste0("&pageNo=",pg, dt))

# Open API를 호출
xmlDocument <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "UTF-8")

# XML Root Node 획득

rootNode <- xmlRoot(xmlDocument)

# 오픈API 호출 결과 데이터의 개수 획득

numOfRows <- as.numeric(xpathSApply(rootNode,"//numOfRows", xmlValue))

# 전체 데이터의 개수 획득

totalCount <- as.numeric(xpathSApply(rootNode,"//totalCount",xmlValue))

# 총 오픈API 호출 횟수 계산
loopCount <- round(totalCount / numOfRows, 0)

## API 호출 횟수 보정
if(loopCount * numOfRows < totalCount){
  loopCount <- loopCount+1
}

# 전체 데이터를 저장할 변수 선언

totalData <- data.frame()

# 오픈 API 호출을 총 오픈API 호출 횟수만큼 반복 실행

for(i in 1:loopCount){
  # 호출 URL 생성
  url <- paste0(serviceURL,
                paste0("?ServiceKey=",ServiceKey),
                paste0("&numOfRows=",rows),
                paste0("&pageNo=",i, dt))
  
  
  # 오픈 API를 호출하여 XML 데이터 획득
  
  doc <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "UTF-8")
  
  # XML 데이터의 Root Node에 접근
  
  rootNode <- xmlRoot(doc)
  
  # item Node의 데이터 추출
  
  xmlData <- xmlToDataFrame(nodes = getNodeSet(rootNode, '//item'))
  
  # 추출한 데이터를 전체 데이터를 저장한 변수에 누적 저장
  
  totalData <- rbind(totalData, xmlData)
}

# 데이터 확인하기
View(totalData)


# 데이터 정렬
totalData <- totalData %>%
  arrange(stateDt) %>%
  distinct()

str(totalData)

head(totalData, 50) # 데이터 앞 부분 확인
tail(totalData, 50) # 데이터 뒷 부분 확인

# 필요한 컬럼 추출
totalData2 <- totalData %>%
  select(accDefRate, accExamCnt, accExamCompCnt, careCnt, clearCnt, deathCnt, decideCnt, examCnt, resutlNegCnt,
         stateDt, stateTime)

# 데이터 타입 변경
str(totalData2)
totalData2$accDefRate <- as.numeric(totalData2$accDefRate)
totalData2$accExamCnt <- as.numeric(totalData2$accExamCnt)
totalData2$accExamCompCnt <- as.numeric(totalData2$accExamCompCnt)
totalData2$careCnt <- as.numeric(totalData2$careCnt)
totalData2$clearCnt <- as.numeric(totalData2$clearCnt)
totalData2$deathCnt <- as.numeric(totalData2$deathCnt)
totalData2$decideCnt <- as.numeric(totalData2$decideCnt)
totalData2$examCnt <- as.numeric(totalData2$examCnt)
totalData2$resutlNegCnt <- as.numeric(totalData2$resutlNegCnt)
totalData2$stateDt <- as.Date(totalData2$stateDt, format = '%Y%m%d')

str(totalData2) # 데이터 구조 확인


## 일단위  누적 검사수 / 누적 검사 완료 수 / 검사 진행 수 / 결과 음성수 / 치료중 환자수 / 사망자 /  격리해제 / 확진자 /누적 확진률    
## 데이터 집계

daily_corona <- totalData2 %>%
  group_by(stateDt) %>%
  summarise(accExamCnt = max(accExamCnt),
            accExamCompCnt = max(accExamCompCnt),
            examCnt = max(examCnt),
            resutlNegCnt = max(resutlNegCnt),
            careCnt = max(careCnt),
            deathCnt = max(deathCnt),
            clearCnt = max(clearCnt),
            decideCnt = max(decideCnt),
            accDefRate = max(accDefRate)) %>%
  arrange(stateDt)

head(daily_corona)
tail(daily_corona)

## 일별 확진자수 / 사망자수 / 격리해제수 

daily_corona <- daily_corona %>%
  mutate(day_ExamCnt = accExamCnt - lag(accExamCnt, default = first(accExamCnt)),
         day_ExamCompCnt = accExamCompCnt - lag(accExamCompCnt, default = first(accExamCompCnt)),
         day_NegCnt = resutlNegCnt - lag(resutlNegCnt, default = first(resutlNegCnt)),
         day_deathCnt = deathCnt - lag(deathCnt, default = first(deathCnt)),
         day_clearCnt = clearCnt - lag(clearCnt, default = first(clearCnt)),
         day_decideCnt = decideCnt - lag(decideCnt, default = first(decideCnt)))

View(tail(daily_corona))

## == 코로나 환자 추이 == ##

## 순 확진자수 추이 

ggplot(daily_corona, aes(x = stateDt, y = careCnt, group = 1)) +
  geom_line(stat = "identity") +
  geom_point() +
  scale_x_date(date_breaks = "2 week", date_labels = "%b/%d", limits = c(as.Date('2020-03-01'), NA))


## 8월 이후 신규 확진자 수 추이

daily_corona %>%
  filter(stateDt >= '2020-08-01') %>%
  ggplot(aes(x = stateDt, y = day_decideCnt, group = 1)) +
  geom_line(stat = "identity") +
  geom_point() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b/%d") +
  geom_hline(yintercept = 100, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2020-10-12'), color = 'blue', alpha = 0.4) +
  annotate("rect", xmin = as.Date('2020-10-05'), xmax = as.Date('2020-10-19'), ymin = 180, ymax = 210, alpha = 0.8, fill = 'white') +
  annotate("text", x = as.Date('2020-10-12'), y = 200, label = "사회적 거리두기 \n1단계 전환", fontface = 2)


