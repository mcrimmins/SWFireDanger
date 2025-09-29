# extract stats from FF+ files
# MAC 05/16/2022
# all of pre-processed FF+ reports are in subdir stats

library(stringr)
library(readr)

# list freq dist files
freqType<-c("ERC_freq","BI_freq")
freqStat<-list()

for(j in 1:length(freqType)){
  
  files<-list.files("./stats",pattern=freqType[j])
  statsTemp<-list()
  for(i in 1:length(files)){
    # get percentiles from freq_dist_report
    f <- readLines(paste0("./stats/",files[i]))
    cline <- grep("90%",f,
                  value=TRUE)
    val90<-as.numeric(sub('.*=', '', cline))
    cline <- grep("97%",f,
                  value=TRUE)
    val97<-as.numeric(sub('.*=', '', cline))
    
    statsTemp[[i]]<-cbind.data.frame(freqType[j],sub("_.*", "", files[i]) ,val90,val97)
  }
  statsTemp = do.call(rbind, statsTemp)
  colnames(statsTemp)<-c("type","PSA","val90","val97")
  freqStat[[j]]<-statsTemp
}

# get daily stats
# list freq dist files
statType<-c("ERC_stat","BI_stat")
dayStat<-list()

for(j in 1:length(statType)){
  
files<-list.files("./stats",pattern=statType[j])
statsDFs<-list()

  for(i in 1:length(files)){
    # get percentiles from freq_dist_report
    f <- readLines(paste0("./stats/",files[i]))
    cline <- grep("Begins",f)+1
    tempStats<-read_table(paste0("./stats/",files[i]), skip = cline, n_max = 365, col_names = FALSE)
    colnames(tempStats)<-c("day","n_yrs","mean","stdev","crit_pctile","highest_avg","highest_avg_year","lowest_avg","lowest_avg_yr",
                        "high","high_yr","avg_high","avg_high_stdev","median_high","low","low_year", "avg_low","avg_low_stdev","median_low","period_begins")
    #tempStats$date<-as.Date(paste0(dayStats$day,"/",format(Sys.Date(),"%Y")),"%m/%d/%Y")
    tempStats$PSA<-sub("_.*", "", files[i])
    tempStats$type<-statType[j]
    statsDFs[[i]]<-tempStats
  }

dayStat[[j]]<-statsDFs
}

# get order of PSAs in list
# PSAs<-NULL
# for(i in 1:length(dayStat[[1]])){
#   PSAs<-append(PSAs,dayStat[[1]][[i]][["PSA"]][1])
# }

# read in supporting PSA list
SW_PSAs <- read.csv(file = 'SW_PSAs.txt')

save(freqStat, dayStat,SW_PSAs, file="ff_report_stats_new.RData")

