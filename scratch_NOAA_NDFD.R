# Testing out NWS NDFD API for RH/Burn Period
# MAC 05/24/22

# NDFD links
# https://graphical.weather.gov/xml/rest.php
# https://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXML.htm
# https://graphical.weather.gov/xml/docs/elementInputNames.php



library(XML)
library(RCurl)


url<-paste0("https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=",PSAtemp$STNID[j],"&sig=&user=&type=N&start=01-Jan-2022&end=31-Dec-2022&time=&priority=&fmodel=16Y&sort=asc&ndays=")

url<-"https://forecast.weather.gov/MapClick.php?lat=32.196&lon=-110.9682&FcstType=digitalDWML"

#https://graphical.weather.gov/xml/sample_products/browser_interface/ndfdBrowserClientByDay.php?whichClient=NDFDgenByDay&lat=32&lon=-110&format=24+hourly&startDate=2022-05-24&numDays=7

xData <- getURL(url)
xmldoc <- xmlParse(xData)
currYear <- xmlToDataFrame(xData)

# use this instead
# https://www.weather.gov/documentation/services-web-api#/default/gridpoint_forecast_hourly
# example from 

# https://stackoverflow.com/questions/32019566/r-xml-parse-for-a-web-address
require(httr)
UA <- "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.0 Safari/537.36"
url <- "http://forecast.weather.gov/MapClick.php?lat=32&lon=-112&FcstType=digitalDWML"
doc <- GET(my_url, user_agent(UA))

data <- XML::xmlParse(content(doc, "text"))
data2 <- xml2::read_xml(content(doc, "text"))


#####
# WORKING EXAMPLE

library(dplyr)

url <- "http://forecast.weather.gov/MapClick.php?lat=32&lon=-112&FcstType=digitalDWML"
download.file(url=url,"url.txt" )
data <- XML::xmlParse("url.txt")

xml_data <- XML::xmlToList(data)
# get location info
location <- as.list(xml_data[["data"]][["location"]][["point"]])
# get times
start_time <- as.data.frame(as.character(unlist(xml_data[["data"]][["time-layout"]][
  names(xml_data[["data"]][["time-layout"]]) == "start-valid-time"])))
colnames(start_time)<-"date_time"
  start_time$date_time<-as.character(start_time$date_time)
  start_time$date_time<-lubridate::ymd_hms(substring(start_time$date_time, 1,19))
  start_time$doy<-as.numeric(format(start_time$date_time,"%j"))
  
# get RH
rhum<-as.data.frame(as.numeric(unlist(xml_data[["data"]][["parameters"]][["humidity"]])))
  colnames(rhum)<-"rh_perc"
rhum<-as.data.frame(rhum[1:nrow(start_time),1])  
  colnames(rhum)<-"rh_perc"
# add in times
rhum<-cbind.data.frame(start_time,rhum)  
rhum$bhour<-ifelse(rhum$rh_perc<=20, 1, 0)

fcst_bhrs<- rhum %>% group_by(doy) %>% 
            summarise(bhours=sum(bhour),
                      nhrs =n())
fcst_bhrs$date<-as.Date(paste0(fcst_bhrs$doy,"-",format(Sys.Date(),"%Y")),format="%j-%Y")
fcst_bhrs<-subset(fcst_bhrs, nhrs==24)
  
# ndfd data
#url<-"https://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php?lat=32&lon=-112&product=time-series&rh=rh"
#doc <- GET(my_url, user_agent(UA))
#data2 <- xml2::read_xml(content(doc, "text"))

