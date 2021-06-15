#library(EIAdata)
library(nleqslv)
library(curl)
library(janitor)
library(viridis)
library(scales)
library(openxlsx)
library(reshape2)
library(zoo)
library(RColorBrewer)
library(scales) 
library(pdfetch)
library(tidyverse)
library(XML)
library(lubridate)
library(httr)
library(jsonlite)
library(readxl)
library(rvest)
library(forcats)
library(ggrepel)
library(ggpubr)
library(gridExtra)
library(timeDate)
library(cansim)

library(ggthemes)
working_directory<-getwd()

#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/")
#print(getwd())

source("tableau.R")

set_png<-function(file_sent,width=1400,height=750,res=130){
  #MAC
  if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
    png(file=file_sent, width = width, height = height,res=res)
  #PC
  if(R.version$platform ==  "x86_64-w64-mingw32")
    png(file=file_sent, width = width, height = height,res=res,type='cairo')
}

ajl_hourly<-function(){
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 18, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )
}



ajl_line<-function(caption_align=1){
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=caption_align),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black",margin = margin(t = 10, b = 5)),
  )
}

make_breaks <- function(strt, hour, interval="day", length.out=31) {
  strt <- as.POSIXlt(strt - 60*60*24)  # start back one day
  strt <- ISOdatetime(strt$year+1900L, strt$mon+1L, strt$mday, hour=hour, min=0, sec=0, tz="UTC")
  seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
}


slide_theme<-function(){
  return( theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
                axis.line.x = element_line(color = "gray"),
                axis.line.y = element_line(color = "gray"),
                axis.text = element_text(size = 16),
                axis.text.x = element_text(margin = margin(t = 10)),
                axis.title = element_text(size = 16),
                #axis.label.x = element_text(size=20,vjust=+5),
                plot.subtitle = element_text(size = 12,hjust=0.5),
                plot.caption = element_text(face="italic",size = 12,hjust=0),
                legend.key.width=unit(2,"line"),
                legend.position = "bottom",
                #legend.direction = "horizontal",
                #legend.box = "horizontal",
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 16),
                plot.title = element_text(hjust=0.5,size = 20),
                plot.margin=unit(c(1,1,1.5,1.2),"cm")
  )
  )
}

small_theme<-function(){
  return( theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
                axis.line.x = element_line(color = "gray"),
                axis.line.y = element_line(color = "gray"),
                axis.text = element_text(size = 12),
                axis.text.x = element_text(margin = margin(t = 10)),
                axis.title = element_text(size = 12),
                #axis.label.x = element_text(size=20,vjust=+5),
                plot.subtitle = element_text(size = 12,hjust=0.5),
                plot.caption = element_text(face="italic",size = 8,hjust=0),
                legend.key.width=unit(2,"line"),
                legend.position = "bottom",
                #legend.direction = "horizontal",
                #legend.box = "horizontal",
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 12),
                plot.title = element_text(hjust=0.5,size = 14),
                plot.margin=unit(c(1,1,1.5,1.2),"cm")
  )
  )
}



#Set EIA API Key

KEY <- "91b4dca0b858df64a2279d82f71af240"




weekly_graphs<-function(caption_align=1){
  theme_minimal()+theme(
    plot.margin = margin(.25, .75, .25, .75, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=caption_align),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.title.x = element_text(size = 14,face = "bold", colour="black",margin = margin(t = 15, b = 0)),
    axis.text = element_text(size = 14,face = "bold", colour="black",margin = margin(t = 10, b = 10)),
  )
}

weekly_small<-function(caption_align=1){
  theme_minimal()+theme(
    plot.margin = margin(.25, .75, .25, .75, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 9),
    plot.caption = element_text(size = 11, face = "italic",hjust=caption_align),
    plot.title = element_text(size = 16,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 11,face = "bold"),
    axis.title.x = element_text(size = 11,face = "bold", colour="black",margin = margin(t = 15, b = 0)),
    axis.text = element_text(size = 11,face = "bold", colour="black",margin = margin(t = 10, b = 10)),
  )
}

tombe_theme<-function(caption_align=1){
  theme_minimal()+theme(
  axis.title.y = element_text(size=12),
  axis.title.x = element_text(size=12),
  axis.text = element_text(size=12),
  legend.position = "bottom",
  legend.text=element_text(size=12),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.caption = element_text(size = 9, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 10, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)
}

#EIA stuff


data_fetch<-function(key, cat=2227122){
  key <- unlist(strsplit(key, ";"))
  ifelse(cat==999999999,
         url <- paste("http://api.eia.gov/category?api_key=",
                      key, "&out=xml", sep="" ),
         url <- paste("http://api.eia.gov/category?api_key=",
                      key, "&category_id=", cat, "&out=xml", sep="" )
  )
  doc <- xmlParse(file=url, isURL=TRUE)
  Parent_Category <- tryCatch(xmlToDataFrame(,stringsAsFactors = F,nodes =
                                               XML::getNodeSet(doc, "//category/parent_category_id")),
                              warning=function(w) FALSE, error=function(w) FALSE)
  Sub_Categories <- xmlToDataFrame(,stringsAsFactors = F,nodes =
                                     XML::getNodeSet(doc, "//childcategories/row"))
  Series_IDs <- xmlToDataFrame(nodes =
                                 XML::getNodeSet(doc, "///childseries/row"),stringsAsFactors = F)
  Categories <- list(Parent_Category, Sub_Categories, Series_IDs)
  names(Categories) <- c("Parent_Category", "Sub_Categories", "Series_IDs")
  return(Categories)
}


# store_packages.R
#
# stores a list of your currently installed packages

#tmp = installed.packages()

#installedpackages = as.vector(tmp[is.na(tmp[,"Priority"]), 1])
#save(installedpackages, file="installed_packages.rda")

# restore_packages.R
#
# installs each package from the stored list of packages

#load("installed_packages.rda")
#
#install.packages("Rtools")
#for (count in 1:length(installedpackages)) install.packages(installedpackages[count])



#havingIP <- function() {
#  if (.Platform$OS.type == "windows") {
#    ipmessage <- system("ipconfig", intern = TRUE)
#  } else {
#    ipmessage <- system("ifconfig", intern = TRUE)
#  }
#  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
#  any(grep(validIP, ipmessage))
#}

#havingIP()


#revert to previous working directory
setwd(working_directory)
print(getwd())


wordwrap<-function(x,len) paste(strwrap(x,width=len),collapse="\n") 

#convert to two decimal places for currency labels
scale_dec2 <- function(x) sprintf("%.2f", x)







assign_peaks<-function(data_orig,time_var=time){
  #default is that we're receiving a data_frame with time as the time variable
  #create temp_time with whatever the time variable might be, then use that to create stats and peaks
  #modify data_sent so you have a data-frame with only the time varible
  #data_mod<-data_orig %>% mutate_(temp_time=`time_var`) %>% select(temp_time)
  temp_time<- enquo(time_var)
  data_mod<-data_orig%>% select(!!temp_time)
    #first, figure out the holidays
  #Holidays:
  #xNew Year's Day  January 1
  #xAlberta Family Day   Third Monday in February
  #xGood Friday   Friday before Easter 
  #Victoria Day  Monday before May 25 	
  #xCanada Day July 1, except when it falls on a Sunday, then it is July 2
  #xLabour Day  First Monday in September
  #xThanksgiving Day  Second Monday in October 
  #xRemembrance Day   November 11 
  #xChristmas Day   December 25
  holiday_list<-c("Christmas","NYD","CDA_Day","Rem_Day","Labour_Day","Good_Friday","Family_Day",  
                  "Thanksgiving", "Victoria_Day")
  data_mod<-data_mod%>%mutate(
    Christmas=ifelse(month(!!temp_time)==12 & day(!!temp_time)==25,T,F),
    NYD=ifelse(month(!!temp_time)==1 & day(!!temp_time)==1,T,F),
    CDA_Day=ifelse(month(!!temp_time)==7 & day(!!temp_time)==1 & wday(!!temp_time,label = T)!="Sun" ,T,F), #Canada Day Holiday if it's not a Sunday
    CDA_Day=ifelse(month(!!temp_time)==7 & day(!!temp_time)==2 & wday(!!temp_time,label = T)=="Mon" ,T,F), #Canada Day Stat if the 2nd is a monday
    Rem_Day=ifelse(month(!!temp_time)==11 & day(!!temp_time)==11,T,F),
    Labour_Day=ifelse(month(!!temp_time)==9 & day(!!temp_time)<=7 & wday(!!temp_time,label = T)=="Mon",T,F), #first Monday in September
    Good_Friday=ifelse(date(!!temp_time)==as.Date(Easter(year(!!temp_time)))-days(2),T,F),
    #Family day - third monday in february so earliest it can be is day 15, latest is day 21
    Family_Day=ifelse(month(!!temp_time)==2 & day(!!temp_time)<=21 & day(!!temp_time)>=15 & wday(!!temp_time,label = T)=="Mon",T,F), #third Monday in Feb
    #Thanksgiving day - second monday in Oct so earliest it can be is day 8, latest is day 14
    Thanksgiving=ifelse(month(!!temp_time)==10 & day(!!temp_time)<=14 & day(!!temp_time)>=8 & wday(!!temp_time,label = T)=="Mon",T,F), #second Monday in Oct
    #Victoria day - monday before May 25, so earliest it can be is day 18, latest is day 24
    Victoria_Day=ifelse(month(!!temp_time)==5 & day(!!temp_time)<=24 & day(!!temp_time)>=18 & wday(!!temp_time,label = T)=="Mon",T,F) #Monday before May 25
  ) %>% mutate(
    stat = select(., holiday_list) %>% rowSums()>0
  )
  #On-Peak: hour ending HE8 to HE23 Monday through Saturday, excluding Sundays and NERC holidays
  #Off-Peak: HE1 to HE7 and HE24 Monday through Saturday, and all hours on Sundays and NERC holidays
  #Extended Peak: HE8 to HE23 every day in the contract period
  #Extended Off-Peak: HE1 to HE7 and HE24 every day in the contract period
  #Super Peak: HE17 to HE22 each day in the contract period
  #for AS, AESO does AM super peak HE 6, 7, 8 and a winter PM Super Peak (HE 17-34, in Nov, Dec, Jan)
  data_mod<-data_mod%>%mutate(
    on_peak=ifelse(wday(!!temp_time,label = T)!="Sun" & stat==F & hour(!!temp_time)>=8 & hour(!!temp_time)<=23,T,F), #Peak hours, not stat or Sunday
    off_peak=ifelse(wday(!!temp_time,label = T)=="Sun" | stat==T | hour(!!temp_time)>=24 | hour(!!temp_time)<=7,T,F), #Off-Peak hours, stat or Sunday
    ext_peak=ifelse(hour(!!temp_time)>=8 & hour(!!temp_time)<=23,T,F), #Ext Peak hours
    ext_off_peak=ifelse(hour(!!temp_time)<8 & hour(!!temp_time)>23,T,F), #Ext Off Peak hours
    super_peak=ifelse(hour(!!temp_time)>=17 & hour(!!temp_time)<=22,T,F), #Super Peak hours
  )
  #return indicators for stats and peaks - same # of rows as data sent
  data_mod<-data_mod %>% select(stat,on_peak,off_peak,ext_peak,ext_off_peak,super_peak)
  bind_cols(data_orig,data_mod)
}


assign_date_time_days<-function(data_sent,time_var=time){
  quo_time<- enquo(time_var)
  data_sent %>% 
    mutate(year=year(!!quo_time),
           month=month(!!quo_time), #month dummies
           month_fac=factor(month.abb[month],levels = month.abb),
           day=day(!!quo_time),
           wday=wday(!!quo_time,label=T),
           hour=hour(!!quo_time),
           temp_time=NULL
    )
}

#create time from date and he
#assign_time<-function(data_sent,date_var="date",he_var="he"){
#  data_sent %>% mutate_(temp_date=`date_var`,temp_he=`he_var`) %>%
#    mutate(time=ymd_h(paste(year(temp_date),"-",month(temp_date),"-",month(temp_date)," ",temp_he,sep="")),
#           temp_date=NULL,
#           temp_he=NULL
#    )
#}




assign_time <- function(data, date_var=date, he_var=he){
  quo_date <- enquo(date_var)
  quo_he <- enquo(he_var)
  data %>% 
    mutate(time = ymd_h(paste(year(!!quo_date), "-", 
                                  month(!!quo_date), "-", 
                                  day(!!quo_date), "-", 
                                  !!quo_he, sep = "")))
}




#u of a palette

colors_ua10 <- function()
{
  #return(c("#007C41", "#FFDB05", "#7D9AAA", "#CA7700", "#165788", "#A8B400",
  #         "#E0D760", "#404545", "#8D3C1E", "#004250"))
  c("#007C41", "#FFDB05", "#7D9AAA","#165788","#404545","#8D3C1E","#3CB6CE")
}


#You really can only have one of c("#FFDB05", "#A8B400","#E0D760") 


