# develop fire danger rating plots for SW GACC
# pull data from WIMS
# MAC 5/11/22

library(XML)
library(RCurl)
library(dplyr)
library(ggplot2)
library(tidyr)

# get recent RAWS data
url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-",format(Sys.time(), "%Y"),"&end=31-Dec-",format(Sys.time(), "%Y"),"&time=&sort=&ndays=")

# get data
url<-paste0("https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=290401&type=F&priority=&fmodel=16Y&sort=asc&ndays=7")
url<-paste0("https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=290401&type=O&priority=&fmodel=16Y&sort=asc&start=01-Jan-2022&end=31-Dec-2022")

url<-paste0("https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=20207&sig=&user=&type=N&start=01-Jan-1980&end=31-Dec-2022&time=&priority=&fmodel=16Y&sort=asc&ndays=")

# past year
#url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
xData <- getURL(url)
xmldoc <- xmlParse(xData)
currYear <- xmlToDataFrame(xData)

# examples from Chuck...

# Links to 7-Day NFDRS and Point Weather Forecasts for station 290401: 
#"https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=290401&type=F&priority=&fmodel=16Y&sort=asc&ndays=7" 
#"https://famprod.nwcg.gov/wims/xsql/pfcst.xsql?stn=290401&type=F&start=24-Apr-22&ndays=7" 

# Links to "Yesterdays" Observed NFDRS and Point Weather values for station 290401: 
#"https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=290401&type=N&priority=&fmodel=16Y&ndays=1" 
#"https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=290401&type=O&ndays=1" 

# get all data from Jemez-- choose type
url<-paste0("https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=20212&sig=&user=&type=&start=01-Jan-1980&end=31-Dec-2021&time=&priority=&fmodel=16Y&sort=asc&ndays=")

# past year
#url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
xData <- getURL(url)
xmldoc <- xmlParse(xData)
allYears <- xmlToDataFrame(xData)

# change col types -- All years
#sapply(allYears, class)
col.names <- c("sta_id","latitude","longitude","nfdr_tm","one_hr","ten_hr","hu_hr","th_hr","xh_hr","ic","kbdi","sc","ec","bi","lr","lo","hr","ho",
              "fl","hrb","wdy","herb_gsi","woody_gsi")
allYears[col.names] <- sapply(allYears[col.names],as.character)
allYears[col.names] <- sapply(allYears[col.names],as.numeric)
allYears$nfdr_dt<-as.Date(as.character(allYears$nfdr_dt),"%m/%d/%Y")
allYears$doy<-as.numeric(format(allYears$nfdr_dt, "%j"))

# get curr Year data from Jemez
url<-paste0("https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=290702&sig=&user=&type=N&start=01-Jan-2022&end=31-Dec-2022&time=&priority=&fmodel=16Y&sort=asc&ndays=")

# past year
#url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
xData <- getURL(url)
xmldoc <- xmlParse(xData)
currYear <- xmlToDataFrame(xData)

# change col types -- curr year
#sapply(allYears, class)
col.names <- c("sta_id","latitude","longitude","nfdr_tm","one_hr","ten_hr","hu_hr","th_hr","xh_hr","ic","kbdi","sc","ec","bi","lr","lo","hr","ho",
               "fl","hrb","wdy","herb_gsi","woody_gsi")
currYear[col.names] <- sapply(currYear[col.names],as.character)
currYear[col.names] <- sapply(currYear[col.names],as.numeric)
currYear$nfdr_dt<-as.Date(as.character(currYear$nfdr_dt),"%m/%d/%Y")
currYear$doy<-as.numeric(format(currYear$nfdr_dt, "%j"))


# get forecast data
url<-paste0("https://famprod.nwcg.gov/wims/xsql/nfdrs.xsql?stn=290702&type=F&priority=&fmodel=16Y&sort=asc&ndays=7")
xData <- getURL(url)
xmldoc <- xmlParse(xData)
currYearFcst <- xmlToDataFrame(xData)

# change col types -- curr year
#sapply(allYears, class)
col.names <- c("sta_id","latitude","longitude","nfdr_tm","one_hr","ten_hr","hu_hr","th_hr","xh_hr","ic","kbdi","sc","ec","bi","lr","lo","hr","ho",
               "fl","hrb","wdy","herb_gsi","woody_gsi")
currYearFcst[col.names] <- sapply(currYearFcst[col.names],as.character)
currYearFcst[col.names] <- sapply(currYearFcst[col.names],as.numeric)
currYearFcst$nfdr_dt<-as.Date(as.character(currYearFcst$nfdr_dt),"%m/%d/%Y")
currYearFcst$doy<-as.numeric(format(currYearFcst$nfdr_dt, "%j"))



# get daily stats
dayQuantiles<- allYears %>% group_by(doy) %>% summarise(
  q97 = quantile(ec,0.97,na.rm='TRUE'),
  q90 = quantile(ec,0.90,na.rm='TRUE'),
  min = min(ec,na.rm='TRUE'),
  max = max(ec,na.rm='TRUE'),
  avg = mean(ec,na.rm='TRUE'),
  n = n())

# add in dummy dates to quantiles
dumYr<-as.numeric(format(Sys.Date(),"%Y"))
dayQuantiles$date<-as.Date(paste0(dumYr,dayQuantiles$doy),format="%Y %j")

# build dataframe for plot
temp<-merge(dayQuantiles,currYear, by.x = "date",by.y="nfdr_dt", all.x=TRUE)
temp<-merge(temp, currYearFcst[,c("nfdr_dt","ec")], by.x = "date",by.y="nfdr_dt", all.x=TRUE)
# pull out key vars
temp<-temp[,c("date","min","max","avg","ec.x","ec.y")]
# gather into long df
temp<-gather(temp,key="type",value = "var", 2:6)
colnames(temp)<-c("date","ERC_stat","ERC-Y")

# rename vars
temp$ERC_stat[temp$ERC_stat == "ec.x"] <- format(Sys.Date(),"%Y")
temp$ERC_stat[temp$ERC_stat == "ec.y"] <- "forecasted"
# factor order
temp$ERC_stat<-factor(temp$ERC_stat, levels = c(format(Sys.Date(),"%Y"),"forecasted","avg","min","max"))

# split into stats/curr
tempStats<-subset(temp, ERC_stat %in% c("min","max","avg"))
tempCurr<-subset(temp, ERC_stat %in% c( format(Sys.Date(),"%Y"),"forecasted"))

# build plot
ggplot()+
  geom_line(data=tempStats, aes(date,`ERC-Y`, color=ERC_stat), size=0.1)+
  geom_line(data=tempCurr, aes(date,`ERC-Y`, color=ERC_stat), size=1.5)+
  #geom_line(data=temp, aes(date,`ERC-Y`, color=ERC_stat), size=1.5)+
  
  scale_color_manual(values = c("goldenrod2","royalblue","green","red","blue"))+
  ggtitle(currYear$sta_nm[1])+
  geom_hline(yintercept = quantile(allYears$ec,probs = 0.9), size=0.25, color="grey50")+
    geom_text(aes(x=as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01"))+10, label="90%", y=quantile(allYears$ec,probs = 0.9)+1.5),
              colour="grey50", size=3)+
  geom_hline(yintercept = quantile(allYears$ec,probs = 0.97),size=0.25, color="grey50")+
    geom_text(aes(x=as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01"))+10, label="97%", y=quantile(allYears$ec,probs = 0.97)+1.5),
            colour="grey50", size=3)+
  scale_x_date(date_labe="%m/%d", expand=c(0,0), date_breaks = "1 month")+
  theme_bw()+
  theme(legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())






