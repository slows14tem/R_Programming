library(jsonlite)

#종합배출내역(요일별) 목록 조회
ServiceKey <- "J4go4JlNEjpl7R%2BYsdi5BCCq0demOgNcS6EXbqh6JRAwTRGcE9BxlUTpfA1Hcs7ZdBJrOMZUeeCy2uUBlE8gRQ%3D%3D"
year <- "2020"
month <- "08"

url = paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?",
            "ServiceKey=", ServiceKey,
            "&type=json&page=1&rowNum=1&",
            "disYear=", year, "&disMonth=", month, sep = "")
url
list <- fromJSON(url)

#연도별 목록 추출 함수
dffunc <- function(Year, Month){
  ServiceKey <- "J4go4JlNEjpl7R%2BYsdi5BCCq0demOgNcS6EXbqh6JRAwTRGcE9BxlUTpfA1Hcs7ZdBJrOMZUeeCy2uUBlE8gRQ%3D%3D"
  url = paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?",
              "ServiceKey=", ServiceKey,
              "&type=json&page=1&rowNum=1&",
              "disYear=", Year, "&disMonth=", Month, sep = "")
  templist <- fromJSON(url)
  list <- templist$data$list
  return(list)
}

df2020 <- dffunc("2020", "08")
df2021 <- dffunc("2021", "08")
df2022 <- dffunc("2022", "08")

df <- c(df2020, df2021, df2022)
class(df)
