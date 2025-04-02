# ARCHIVE VERSION
# develop fire danger rating plots for SW GACC
# pull data from WIMS and use FF+ reports for climatology
# MAC 5/28/24

#library(rgdal)
library(XML)
library(RCurl)
#library(dplyr)
library(ggplot2)
#library(tidyr)
library(tidyverse)
library(magick)
library(sf)

# DEAL WITH PANDOC ERROR
#Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
Sys.setenv(RSTUDIO_PANDOC="/usr/bin/pandoc")

#####
# load supporting data from getFFStats.R
load("/home/crimmins/RProjects/FireDangerPlots/ff_report_stats.RData")

# ggplot inset data
states <- map_data("state")

#####
# load spatial data ---- OLD
# psa zones
#psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/FireClimate/monsoonClimo/shapes", layer="National_Predictive_Service_Areas_(PSA)_Boundaries")
#psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/FireDangerPlots/shapefiles", layer="National_PSA_Current_20220112")
#sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")
# get psa centroids for factor order
#sw_psaDF<- cbind.data.frame(sw_psa, rgeos::gCentroid(sw_psa,byid=TRUE))
#####

###### New code with SF
# Read the shapefile using sf
psa <- st_read(dsn = "/home/crimmins/RProjects/FireDangerPlots/shapefiles", layer = "National_PSA_Current_20220112")
# Subset for the Southwest Coordination Center
sw_psa <- psa[psa$GACCName == "Southwest Coordination Center", ]
# Compute centroids for factor order
sw_psa_centroids <- st_centroid(sw_psa)
# Convert to data frame and combine with PSA data
sw_psaDF <- cbind.data.frame(sw_psa, st_coordinates(sw_psa_centroids))
sw_psaDF <- st_drop_geometry(sw_psaDF)
######

PSAlist<-unique(SW_PSAs$PSA)

# WIMS available back to 2014, only back to 2018 for NFDRS?
yrs<-c(2024:2024)

for(l in 1:length(yrs)){
  arcYR<-yrs[l]
  # create year dirs in archive
  dir.create(paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/"),showWarnings = FALSE)
  dir.create(paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/plotly/"),showWarnings = FALSE)

  # copy in necessary files
  file.copy(from = "/home/crimmins/RProjects/FireDangerPlots/FireDangerTemplate_historic.Rmd", 
            to=paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR), overwrite = TRUE)
  file.copy(from = "/home/crimmins/RProjects/FireDangerPlots/archive/psaMap.png", 
            to=paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR), overwrite = TRUE)
  file.copy(from = "/home/crimmins/RProjects/FireDangerPlots/archive/GitHub-Mark-32px.png", 
            to=paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR), overwrite = TRUE)
  file.copy(from = "/home/crimmins/RProjects/logos/CLIMAS_UACOOP_SWCC_horiz.png", 
            to=paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR), overwrite = TRUE)
  
  
# create plots for each year
    for(i in 1:length(PSAlist)){
      # get stations in single PSA
      PSAtemp<-subset(SW_PSAs, PSA==PSAlist[i])
      
      print(paste0("Processing ", PSAlist[i]))
      
      # loop through station downloads in PSA
      
      # curr Obs list
      currERC<-list()
      currBI<-list()
      # forecast list
      fcstERC<-list()
      fcstBI<-list()
      # station list
      stations<-list()
      
      # curr year
      #format(Sys.Date(),"%Y")
      
      for(j in 1:nrow(PSAtemp)){
        # download XML for curr yr observations
        url<-paste0("https://famprod.nwcg.gov/prod-wims/xsql/nfdrs.xsql?stn=",PSAtemp$STNID[j],"&sig=&user=&type=N&start=01-Jan-",arcYR,"&end=31-Dec-",arcYR,"&time=&priority=&fmodel=16Y&sort=asc&ndays=")
        xData <- getURL(url)
        xmldoc <- xmlParse(xData)
        currYear <- xmlToDataFrame(xData)
        
        if(nrow(currYear)!=0){
          # convert from factors to numeric
          col.names <- c("sta_id","latitude","longitude","nfdr_tm","one_hr","ten_hr","hu_hr","th_hr","xh_hr","ic","kbdi","sc","ec","bi","lr","lo","hr","ho",
                         "fl","hrb","wdy","herb_gsi","woody_gsi")
          currYear[col.names] <- sapply(currYear[col.names],as.character)
          currYear[col.names] <- sapply(currYear[col.names],as.numeric)
          # add in date field
          currYear$date<-as.Date(as.character(currYear$nfdr_dt),"%m/%d/%Y")
          # get ERC
          currERC[[j]] <- currYear[,c("date","ec")]                 
          # get BI
          currBI[[j]] <- currYear[,c("date","bi")]
          # get station info
          stations[[j]]<-currYear[1,c("sta_nm","latitude","longitude")]
        }
        
      }
    
      # remove empty list entries from missing stations
      rmNull <- function(x) {
        x <- Filter(Negate(is.null), x)
      }
      currERC <- rmNull(currERC)
      currBI <- rmNull(currBI)
      stations <- rmNull(stations)
      
      
      # merge lists into dataframes based on date
      currBI<- currBI %>% reduce(full_join, by='date')
        currBI$curr_BI<-rowMeans(currBI[ , c(2:ncol(currBI)), drop=FALSE], na.rm=TRUE)
      currERC<- currERC %>% reduce(full_join, by='date')
        currERC$curr_ERC<-rowMeans(currERC[ , c(2:ncol(currERC)), drop=FALSE], na.rm=TRUE)
      # fcstBI<- fcstBI %>% reduce(full_join, by='date')
      #   fcstBI$fcst_BI<-rowMeans(fcstBI[ , c(2:ncol(fcstBI)), drop=FALSE], na.rm=TRUE)
      # fcstERC<- fcstERC %>% reduce(full_join, by='date')
      #   fcstERC$fcst_ERC<-rowMeans(fcstERC[ , c(2:ncol(fcstERC)), drop=FALSE], na.rm=TRUE)
      
      # get daily climo data
      # ERC
      climoERC<-dayStat[[1]][[i]]
      climoERC$date<-as.Date(paste0(climoERC$day,"/",arcYR),"%m/%d/%Y")
      climoERC<-climoERC[,c("date","mean","high","low")]
      # BI
      climoBI<-dayStat[[2]][[i]]
      climoBI$date<-as.Date(paste0(climoBI$day,"/",arcYR),"%m/%d/%Y")
      climoBI<-climoBI[,c("date","mean","high","low")]
      
      # combine into common df
      # ERC
      ercDF<-list(currERC[,c("date","curr_ERC")],climoERC)
      ercDF<- ercDF %>% reduce(full_join, by='date')
      # BI
      biDF<-list(currBI[,c("date","curr_BI")],climoBI)
      biDF<- biDF %>% reduce(full_join, by='date')
      
      # station data into DF
      stations<-do.call(rbind, stations)
      
      #####
      # make plots
      
      ##### ERC plot
      temp<-gather(ercDF,key="type",value = "var", 2:5)
      colnames(temp)<-c("date","stat","ERC-Y")
      # rename vars
      temp$stat[temp$stat == "curr_ERC"] <- arcYR
      #temp$stat[temp$stat == "fcst_ERC"] <- "Forecasted"
      temp$stat[temp$stat == "mean"]     <- "Avg"
      temp$stat[temp$stat == "high"]     <- "Max"
      temp$stat[temp$stat == "low"]      <- "Min"
    
      # factor order
      temp$stat<-factor(temp$stat, levels = c(arcYR,"Avg","Min","Max"))
      
      # split into stats/curr
      tempStats<-subset(temp, stat %in% c("Min","Max","Avg"))
      tempCurr<-subset(temp, stat %in% c( arcYR))
      
      # freq stats
      p90erc<-freqStat[[1]][["val90"]][i]
      p97erc<-freqStat[[1]][["val97"]][i]
      
      pERC<-ggplot()+
        geom_line(data=tempStats, aes(date,`ERC-Y`, color=stat), size=0.1)+
        geom_line(data=tempCurr, aes(date,`ERC-Y`, color=stat), size=1.1)+
        #geom_line(data=temp, aes(date,`ERC-Y`, color=ERC_stat), size=1.5)+
        
        scale_color_manual(values = c("blue","royalblue","red","goldenrod1"))+
        ggtitle(paste0(arcYR," Energy Release Component: PSA ", PSAlist[i]))+
        geom_hline(yintercept = p90erc, size=0.25, color="grey50")+
        geom_text(aes(x=as.Date(paste0(arcYR,"-01-01"))+10, label="90%", y=p90erc+1.75),
                  colour="grey50", size=3)+
        geom_hline(yintercept = p97erc,size=0.25, color="grey50")+
        geom_text(aes(x=as.Date(paste0(arcYR,"-01-01"))+10, label="97%", y=p97erc+1.75),
                  colour="grey50", size=3)+
        scale_x_date(date_labe="%m/%d", expand=c(0,0), date_breaks = "1 month")+
        theme_bw()+
        theme(legend.position="bottom",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.title= element_blank())
      
      # interactive plot
      pERCly<-plotly::ggplotly(pERC)
      htmlwidgets::saveWidget(pERCly, paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/plotly/",PSAlist[i],"_ERC.html"))
      
      ##### BI Plot
      temp<-gather(biDF,key="type",value = "var", 2:5)
      colnames(temp)<-c("date","stat","BI-Y")
      # rename vars
      temp$stat[temp$stat == "curr_BI"] <- arcYR
      #temp$stat[temp$stat == "fcst_BI"] <- "Forecasted"
      temp$stat[temp$stat == "mean"]     <- "Avg"
      temp$stat[temp$stat == "high"]     <- "Max"
      temp$stat[temp$stat == "low"]      <- "Min"
      
      # factor order
      temp$stat<-factor(temp$stat, levels = c(arcYR,"Avg","Min","Max"))
      
      # split into stats/curr
      tempStats<-subset(temp, stat %in% c("Min","Max","Avg"))
      tempCurr<-subset(temp, stat %in% c( arcYR))
      
      # freq stats
      p90bi<-freqStat[[2]][["val90"]][i]
      p97bi<-freqStat[[2]][["val97"]][i]
      
      pBI<-ggplot()+
        geom_line(data=tempStats, aes(date,`BI-Y`, color=stat), size=0.1)+
        geom_line(data=tempCurr, aes(date,`BI-Y`, color=stat), size=1.1)+
        #geom_line(data=temp, aes(date,`ERC-Y`, color=ERC_stat), size=1.5)+
        
        scale_color_manual(values = c("blue","royalblue","red","goldenrod1"))+
        ggtitle(paste0(arcYR," Burning Index: PSA ", PSAlist[i]))+
        geom_hline(yintercept = p90bi, size=0.25, color="grey50")+
        geom_text(aes(x=as.Date(paste0(arcYR,"-01-01"))+10, label="90%", y=p90bi+1.75),
                  colour="grey50", size=3)+
        geom_hline(yintercept = p97bi,size=0.25, color="grey50")+
        geom_text(aes(x=as.Date(paste0(arcYR,"-01-01"))+10, label="97%", y=p97bi+1.75),
                  colour="grey50", size=3)+
        scale_x_date(date_labe="%m/%d", expand=c(0,0), date_breaks = "1 month")+
        theme_bw()+
        theme(legend.position="bottom",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.title= element_blank())
      
      # interactive plot
      pBIly<-plotly::ggplotly(pBI)
      htmlwidgets::saveWidget(pBIly, paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/plotly/",PSAlist[i],"_BI.html"))
      
      # Make common plot parts
      # inset map:
      # zoomLev<-5
      #sw_psa_df<-fortify(sw_psa)
      #keyPSA_df<-fortify(subset(sw_psa, PSANationa==gsub("-","",PSAlist[i])))
      keyPSA_df<-(subset(sw_psa, PSANationa==gsub("-","",PSAlist[i])))
      #stationLatLon<-stations
      insetmap<-ggplot() +
        geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
        #geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey", alpha=0.8)  + # get the state border back on top
        #geom_polygon(data = keyPSA_df, aes(x = long, y = lat, group = group), fill="powderblue", color=NA, alpha=0.8) +
        geom_sf(data = sw_psa, fill="lightgrey", color="grey", alpha=0.8)  + # get the state border back on top
        geom_sf(data = keyPSA_df, fill="powderblue", color=NA, alpha=0.8) +
         #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
        #coord_fixed(xlim=c(-115, -102.5), ylim=c(31, 37.5), ratio = 1) +
        coord_sf(xlim=c(-115, -102.5), ylim=c(31, 37.5)) +
        geom_point(data = stations, aes(x = longitude, y = latitude), size=0.5, color='red')+
        theme_bw(base_size=5)+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      
      #g <- ggplotGrob(insetmap)
      
      pERC<-pERC + 
        #annotation_custom(grob = g, xmin = as.Date(paste0(format(Sys.Date(),"%Y"),"-11-01")), xmax = Inf, ymin = p97erc, ymax = Inf)+
        labs(caption=paste0("Updated: ",format(Sys.time(), "%Y-%m-%d"),
                            "\nClimatology from FireFamily Plus Statistical Summary Report\nNFDRS Data Source: famprod.nwcg.gov/wims"))
      
      pBI<-pBI + 
        #annotation_custom(grob = g, xmin = as.Date(paste0(format(Sys.Date(),"%Y"),"-11-01")), xmax = Inf, ymin = p97bi, ymax = Inf)+
        labs(caption=paste0("Updated: ",format(Sys.time(), "%Y-%m-%d"),
                            "\nClimatology from FireFamily Plus Statistical Summary Report\nNFDRS Data Source: famprod.nwcg.gov/wims"))
      
      # write out ERC file
      png(paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/",PSAlist[i],"_ERC.png"), width = 9, height = 6, units = "in", res = 300L)
      #grid.newpage()
        subvp <- grid::viewport(width = 0.16, height = 0.16, x = 0.91, y = 0.875)
        print(pERC, newpage = FALSE)
        print(insetmap, vp = subvp)
      dev.off()
        # add logos
        # Call back the plot
        plot <- image_read(paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/",PSAlist[i],"_ERC.png"))
        # And bring in a logo
        logo_raw <- image_read("/home/crimmins/RProjects/logos/CLIMAS_UACOOP_SWCC_horiz.png") 
        logo <- image_resize(logo_raw, geometry_size_percent(width=65,height = 65))
        # Stack them on top of each other
        final_plot <- image_composite(plot, logo, offset = "+130+1600")
        # And overwrite the plot without a logo
        image_write(final_plot, paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/",PSAlist[i],"_ERC.png"))
        # ----
      
      # write out BI file
      png(paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/",PSAlist[i],"_BI.png"), width = 9, height = 6, units = "in", res = 300L)
      #grid.newpage()
        subvp <- grid::viewport(width = 0.16, height = 0.16, x = 0.91, y = 0.875)
        print(pBI, newpage = FALSE)
        print(insetmap, vp = subvp)
      dev.off()
        # add logos
        # Call back the plot
        plot <- image_read(paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/",PSAlist[i],"_BI.png"))
        # And bring in a logo
        logo_raw <- image_read("/home/crimmins/RProjects/logos/CLIMAS_UACOOP_SWCC_horiz.png") 
        logo <- image_resize(logo_raw, geometry_size_percent(width=65,height = 65))
        # Stack them on top of each other
        final_plot <- image_composite(plot, logo, offset = "+130+1600")
        # And overwrite the plot without a logo
        image_write(final_plot, paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/",PSAlist[i],"_BI.png"))
        # ----
    }
    
    # create Website with markdown ----
    library(knitr)
    library(rmarkdown)
    
    pageTitle<-paste0("Southwest U.S. NFDRS Charts - ",arcYR)
    render(paste0('/home/crimmins/RProjects/FireDangerPlots/archive/',arcYR,'/FireDangerTemplate_historic.Rmd'), output_file='index.html',
           output_dir=paste0("/home/crimmins/RProjects/FireDangerPlots/archive/",arcYR,"/"), clean=TRUE)
    
    # #####
    
    #source('/home/crimmins/RProjects/FireDangerPlots/pushNotify.R')

}


