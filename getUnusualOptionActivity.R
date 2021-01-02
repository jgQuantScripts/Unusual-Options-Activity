require("httr");require("dplyr");require("purrr");require("RQuantLib");require("rvest")
setwd("/Volumes/3TB/R")
# Test to see if it is a business day, if it is not it will not get any data
if(isBusinessDay(calendar = "UnitedStates/NYSE",Sys.Date())){
# ************************
# get Optionable tickers
# ************************
# page url
pg <- html_session("https://www.barchart.com/options/unusual-activity/etfs")
# save page cookies
cookies <- pg$response$cookies
# Use a named character vector for unquote splicing with !!!
token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                           cookies$name)))
# store today's date and next business day
dates = c(Sys.Date(),adjust(calendar="UnitedStates/NYSE", dates=Sys.Date()+1, bdc = 0))
#****************************************************************
#                             stocks
#****************************************************************
cat("\nUnusual Option Activity: Stocks")
# get data by passing in url and cookies
pg <- 
  pg %>% rvest:::request_GET(
    paste0("https://www.barchart.com/proxies/core-api/v1/options/get?fields=symbol",
           "%2CbaseSymbol%2CbaseLastPrice%2CbaseSymbolType%2CsymbolType%2CstrikePrice",
           "%2CexpirationDate%2CdaysToExpiration%2CbidPrice%2Cmidpoint%2CaskPrice%2ClastPrice",
           "%2Cvolume%2CopenInterest%2CvolumeOpenInterestRatio%2Cvolatility%2CtradeTime%2C",
           "symbolCode&meta=field.shortName%2Cfield.type%2Cfield.description&orderBy=",
           "volumeOpenInterestRatio&orderDir=desc&baseSymbolTypes=stock&between",
           "(volumeOpenInterestRatio%2C1.24%2C)=&between(lastPrice%2C.10%2C)=",
           "&between(tradeTime%2C",dates[1],"%2C",dates[2],")=&between(volume%2C500%2C)=",
           "&between(openInterest%2C100%2C)=&in(exchange%2C(AMEX%2CNASDAQ%2CNYSE))=",
           "&page=1&limit=10000&raw=1"),
    config = httr::add_headers(`x-xsrf-token` = token)
  )

# raw data
data_raw <- httr::content(pg$response)
# convert into a data table
data <- 
  purrr::map_dfr(
    data_raw$data,
    function(x){
      as.data.frame(x$raw)
    }
  )
# fix time 
data$tradeTime = as.POSIXct(data$tradeTime, origin="1970-01-01")
# save as .rds file
saveRDS(data,paste0("/Volumes/3TB/OPTIONS/Unusual Activity/Stocks/",
                    format(Sys.Date(),"%Y%m%d"),".rds"))
#****************************************************************
#                             index
#****************************************************************
cat("\nUnusual Option Activity: Index")
pg <- 
  pg %>% rvest:::request_GET(
paste0("https://www.barchart.com/proxies/core-api/v1/options/get?fields=symbol",
"%2CbaseSymbol%2CbaseLastPrice%2CbaseSymbolType%2CsymbolType%2CstrikePrice%2C",
"expirationDate%2CdaysToExpiration%2CbidPrice%2Cmidpoint%2CaskPrice%2ClastPrice",
"%2Cvolume%2CopenInterest%2CvolumeOpenInterestRatio%2Cvolatility%2CtradeTime%2C",
"symbolCode&meta=field.shortName%2Cfield.type%2Cfield.description&orderBy=",
"volumeOpenInterestRatio&orderDir=desc&baseSymbolTypes=index&between",
"(volumeOpenInterestRatio%2C1.24%2C)=&between(lastPrice%2C.10%2C)=",
"&between(tradeTime%2C",dates[1],"%2C",dates[2],")=&between(volume%2C500%2C)=",
"&between(openInterest%2C100%2C)=&page=1&limit=10000&raw=1"),
config = httr::add_headers(`x-xsrf-token` = token)
  )
# raw data
data_raw <- httr::content(pg$response)
# convert into a data table
data <- 
  purrr::map_dfr(
    data_raw$data,
    function(x){
      as.data.frame(x$raw)
    }
  )
# fix time 
data$tradeTime = as.POSIXct(data$tradeTime, origin="1970-01-01")
# save as .rds file
saveRDS(data,paste0("/Volumes/3TB/OPTIONS/Unusual Activity/Index/",
                    format(Sys.Date(),"%Y%m%d"),".rds"))
#****************************************************************
#                             ETFs
#****************************************************************
cat("\nUnusual Option Activity: ETFs")
pg <- 
  pg %>% rvest:::request_GET(
paste0("https://www.barchart.com/proxies/core-api/v1/options/get?fields=symbol",
       "%2CbaseSymbol%2CbaseLastPrice%2CbaseSymbolType%2CsymbolType%2CstrikePrice",
       "%2CexpirationDate%2CdaysToExpiration%2CbidPrice%2Cmidpoint%2CaskPrice%2ClastPrice",
       "%2Cvolume%2CopenInterest%2CvolumeOpenInterestRatio%2Cvolatility%2CtradeTime%2C",
       "symbolCode&meta=field.shortName%2Cfield.type%2Cfield.description&orderBy=",
       "volumeOpenInterestRatio&orderDir=desc&baseSymbolTypes=etf&between",
       "(volumeOpenInterestRatio%2C1.24%2C)=&between(lastPrice%2C.10%2C)=",
       "&between(tradeTime%2C",dates[1],"%2C",dates[2],")=&between(volume%2C500%2C)=",
       "&between(openInterest%2C100%2C)=&in(exchange%2C(AMEX%2CNASDAQ%2CNYSE))=",
       "&page=1&limit=10000&raw=1"),
config = httr::add_headers(`x-xsrf-token` = token)
)
# raw data
data_raw <- httr::content(pg$response)
# convert into a data table
data <- 
  purrr::map_dfr(
    data_raw$data,
    function(x){
      as.data.frame(x$raw)
    }
  )
# fix time 
data$tradeTime = as.POSIXct(data$tradeTime, origin="1970-01-01")
# save as .rds file
saveRDS(data,paste0("/Volumes/3TB/OPTIONS/Unusual Activity/ETF/",
                    format(Sys.Date(),"%Y%m%d"),".rds"))
cat("\nUnusual Option Activity: Done!")
}else{
  cat("\nNOT A BUSINESS DAY!\n")
}