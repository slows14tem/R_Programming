install.packages("jsonlite")
library(jsonlite)

#일일박스오피스 데이터 로드
#url = https://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=f5eef3421c602c6cb7ea224104795888&targetDt=20221220

apikey <- "f5eef3421c602c6cb7ea224104795888"
dt <- "20221220"
url <- paste("https://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?",
              "key=", apikey,
              "&targetDt=", dt, sep = "")
url

mv <- fromJSON(url)
mv

class(mv)
mode(mv)

#박스오피스 목록 추출
boxOfficeList <- mv$boxOfficeResult$dailyBoxOfficeList
boxOfficeList

#데이터 형변환
names(boxOfficeList)
str(boxOfficeList)

col <- c("rnum", "rank","rankInten", "salesAmt","salesShare",    "salesInten",   
  "salesChange",   "salesAcc",      "audiCnt",       "audiInten",     "audiChange",   
  "audiAcc",       "scrnCnt",       "showCnt")

for(c in col){
  boxOfficeList[c] <- as.numeric(unlist(boxOfficeList[c]))
}

#df$~~ => vector, df[~~] => list
#list는 unlist로 깨야 vector가 된다
#함수 안에서는 df$~~는 사용 못해서 df[~~]를 사용해야함함

#매출평균보다 매출이 높은 영화
library(dplyr)
#salesAmt = 매출
df <- boxOfficeList %>% filter(salesAmt>mean(salesAmt)) %>% select(rank, movieNm, salesAmt)
df

#날짜 입력받아서 함수형식으로 리스트 불러오기기
List <- function(date){
  
  url <- paste("https://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?",
               "key=", apikey,
               "&targetDt=", date, sep = "")
  mv <- fromJSON(url)
  mvList <- mv$boxOfficeResult$dailyBoxOfficeList
  return(mvList)
}
list1 <- List("20221212")
