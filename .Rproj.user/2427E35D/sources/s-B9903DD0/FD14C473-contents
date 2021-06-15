#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/EIA_data_pulls")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/EIA_data_pulls")
print(getwd())
#Set EIA API Key


source("andrew_base.R")
library(ggthemes)
library(gghighlight)
library(roll)
library(cowplot)
library(patchwork)



blakes_blue<-"#4477AA"

work_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.5)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5)),
          legend.title = element_text(color="grey10",size=rel(1)),
          legend.text = element_text(color="grey10",size=rel(1)),
          strip.text = element_text(size=rel(1)),
          axis.title = element_text(size=rel(1)),
          axis.text = element_text(size=rel(1)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "bottom",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}

work_theme_small<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.4)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.25)),
          legend.title = element_text(color="grey10",size=rel(.3)),
          legend.text = element_text(color="grey10",size=rel(.35)),
          strip.text = element_text(size=rel(.35)),
          axis.title = element_text(size=rel(.35)),
          axis.text = element_text(size=rel(.35)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "bottom",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}

 KEY <- "91b4dca0b858df64a2279d82f71af240"


eia_fix_dates<-function(data_sent)
{
  data_sent$date=ymd(rownames(data_sent))
  rownames(data_sent)<-NULL
  data_sent[]
}
 
steo_data_fetch<-function(date_sent){
#for any month and year, get the STEO Price outlook
#testing
  #date_sent<-ymd("2019-9-01")
  #month_sent should be lower case month.abb

#convert date to month/year notation used by eia
month_sent<-tolower(month.abb[month((date_sent))])
year_sent<-sprintf('%02d', year(date_sent)%% 100)

#before jun2013 they are xls files
file_date<-ymd(paste(year_sent,month_sent,1,sep="-"))
excel_file<-ifelse(file_date>ymd("2013-06-01"),
                   paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xlsx"),
                   paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xls"))
temp_file<-ifelse(file_date>ymd("2013-06-01"),
                  paste0("steo_",month_sent,"_",year_sent,".xlsx"),
                  paste0("steo_",month_sent,"_",year_sent,".xls"))
download.file(excel_file,mode = "wb",destfile = temp_file)
  dates<-read_excel(path=temp_file,sheet = "2tab",range = "C3:C4",col_names = F)
  names(dates)[1]<-"X__1"
year_start<-dates$X__1[1]
month_start<-grep(dates$X__1[2],month.abb)

#process price outlook
price_outlook<-read_excel(path=temp_file,sheet = "2tab",range = "A5:BV40",na="n/a")
names(price_outlook)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
#drop electricity and refined product headers
price_outlook<-price_outlook[-c(4,26),]
#rows which could be headers
headers<-grep("TRUE",is.na(price_outlook[,1]))
#for each header, the next x rows get a concatenated header
price_outlook$Header<-NA
price_outlook$Header[1]<-"Crude Oil"
for(j in headers){
  #print(price_outlook$Region[j])
  price_outlook$Header[[j]]<-price_outlook$Region[[j]]
  }
price_outlook<-price_outlook %>% fill(Header)
price_outlook<-price_outlook[!is.na(price_outlook$code),]
price_outlook<-melt(price_outlook,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
price_outlook$table<-"2tab"
price_outlook<-price_outlook %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)
#file ends up with columns code, Region, Header, Date, value, forecast, version

#process non_opec_supply
crude_supply_data<-read_excel(path=temp_file,sheet = "3atab",range = "A5:BV47",na="n/a")
names(crude_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
crude_supply_data<-crude_supply_data[rowSums(is.na(crude_supply_data)) != ncol(crude_supply_data),]
headers<-grep("TRUE",is.na(crude_supply_data[,1]))
#for each header, the next x rows get a concatenated header
crude_supply_data$Header<-NA
crude_supply_data$Header[1]<-"Supply (million barrels per day) (a)"
for(j in headers){
  #print(price_outlook$Region[j])
  crude_supply_data$Header[[j]]<-crude_supply_data$Region[[j]]
}
crude_supply_data<-crude_supply_data %>% fill(Header)
crude_supply_data<-crude_supply_data[!is.na(crude_supply_data$code),]
crude_supply_data<-melt(crude_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
crude_supply_data$table<-"3atab"
crude_supply_data<-crude_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)
#file ends up with columns code, Region, Header, Date, value, forecast, version

#process non_opec_supply
non_opec_supply_data<-read_excel(path=temp_file,sheet = "3btab",range = "A5:BV50",na="n/a")
names(non_opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
non_opec_supply_data<-non_opec_supply_data[rowSums(is.na(non_opec_supply_data)) != ncol(non_opec_supply_data),]
non_opec_supply_data$Header<-"Petroleum Supply  (million barrels per day)"
non_opec_supply_data<-melt(non_opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
non_opec_supply_data$table<-"3btab"
non_opec_supply_data<-non_opec_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)

#process opec_data
opec_supply_data<-read_excel(path=temp_file,sheet = "3ctab",range = "A4:BV55",na=c("n/a","-"))
names(opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
opec_supply_data<-opec_supply_data[rowSums(is.na(opec_supply_data)) != ncol(opec_supply_data),]
opec_supply_data$Header<-NA
headers<-grep("TRUE",is.na(opec_supply_data[,1]))
#for each header, the next x rows get a concatenated header
for(j in headers){
  #print(price_outlook$Region[j])
  opec_supply_data$Header[[j]]<-opec_supply_data$Region[[j]]
}
opec_supply_data<-opec_supply_data %>% fill(Header)
opec_supply_data<-opec_supply_data[!is.na(opec_supply_data$code),]
opec_supply_data<-melt(opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
opec_supply_data$table<-"3ctab"
opec_supply_data<-opec_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)


#stack everthing

steo_data<-rbind(price_outlook,crude_supply_data,non_opec_supply_data,opec_supply_data)

steo_data
}


#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#get historic data
#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#find current issue of STEO - latest it could be out is the 12th, but let's use the 15th
steo_date<-as.Date(ifelse(day(Sys.Date())>=11,Sys.Date(),Sys.Date()-months(1)))
steo_date<-Sys.Date()

steo_data0<-filter(steo_data_fetch(steo_date),Date>=ymd("2019-1-01"),forecast==0)
get_history<-0
if(get_history==1)
  {
  steo_data1<-filter(steo_data_fetch(ymd("2019-1-1")),Date>=ymd("2015-01-01"),forecast==0)
  #2011-2014 histories
  steo_data2<-filter(steo_data_fetch(ymd("2015-1-1")),forecast==0)
  #2007-2010 histories
  steo_data3<-filter(steo_data_fetch(ymd("2011-1-1")),forecast==0)
  #2004-2007 histories
  steo_data4<-filter(steo_data_fetch(ymd("2008-1-1")),Date<ymd("2007-01-01"),forecast==0)
  steo_history<-rbind(steo_data4,steo_data3,steo_data2,steo_data1)
  save(steo_history,file="steo_history.RData")
  }

load("steo_history.RData")
steo_history<-rbind(steo_history,steo_data0)

#add forecasts

steo_forecast<-filter(steo_data_fetch(steo_date),forecast==1)
steo_data<-rbind(steo_history,steo_forecast)




#global supply and demand
#the brackets mess up filter, so this is a fix
supply_demand<-steo_data %>%filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")))
#find forecast dates
min_forecast<-min(supply_demand$Date[supply_demand$forecast==1])
max_forecast<-max(supply_demand$Date[supply_demand$forecast==1])

#historical demand forecasts
steo_old_sd_forecasts<-filter(steo_data_fetch(ymd("2019-12-1")),Date>=ymd("2015-01-01"),forecast==1) %>%
  rbind(filter(steo_data_fetch(ymd("2020-2-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  #rbind(filter(steo_data_fetch(ymd("2020-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-4-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-6-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2021-1-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))

graph_df<-supply_demand%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(format(max(supply_demand$version), "%b %Y"), "forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))%>%
  bind_rows(steo_old_sd_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")),
         levels=sort(ymd(version)),
         version=factor(paste(month.abb[month(version)],year(version),"EIA STEO forecast"),
                        levels=format(sort(unique(levels)),"%b %Y EIA STEO forecast")))


forecast_label<-paste(format(max(supply_demand$version), "%b %Y"), "EIA STEO forecast")
other_versions<-graph_df %>% filter(forecast==1,version!=forecast_label) %>% select(version) %>% unique()




ggplot(filter(graph_df,Region=="Total World Consumption",forecast==0,Date>=ymd("2004-01-01")))+
  geom_line(data=filter(graph_df,Region=="Total World Consumption"),
            aes(Date,value,group=version,colour=version),linetype="solid",size=1.5)+
  geom_point(data=filter(graph_df,Region=="Total World Consumption"),
             aes(Date,ifelse(month(Date)%in% c(3,6,9,12)&forecast==1,value,NA),group=version,shape=version,colour=version,fill=version),size=2.75)+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.5)+
  scale_x_date(breaks = "12 months",date_labels = "%Y",expand=c(0,0))+
  scale_shape_manual("",values=c(15,16,1,2,17,18,1))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(.20,.20))+
  # expand_limits(y=0)+
  expand_limits(x=ymd("2022-01-01"))+
  #scale_linetype_manual("",values=c(1,1))+
  #scale_color_viridis("",discrete = T,option="C",direction = 1,end = .9)+
  scale_color_manual("",values = colors_ua10())+
  scale_fill_viridis("",discrete = T,option="C",direction = 1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("EIA Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  weekly_graphs()+
  work_theme()+
  theme(plot.caption = element_blank())+#no caption
  guides(shape = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3),
         linetype = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3),
         colour = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3),
         fill = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3))+
  labs(y="Global Liquids Demand (million barrels per day)",x="",
       #title=paste("Will the world oil market recover?"),
       #subtitle=paste("In early 2020, the COVID-19 pandemic dropped global oil demand to levels not seen since 2004. The initially-forecast rapid recovery has not materialized. What will 2021 bring?"),
       caption="Source: Historical data and forecasts via Energy Information Administration (EIA) Short-term Energy Outlook (STEO), graph by Andrew Leach.")

ggsave("demand_new.png",width=16,height = 9,dpi=600)

ggplot(filter(graph_df,Region=="Total World Supply",forecast==0,Date>=ymd("2004-01-01")))+
  geom_line(data=filter(graph_df,Region=="Total World Supply"),
            aes(Date,value,group=version,colour=version),linetype="solid",size=1.5)+
  geom_point(data=filter(graph_df,Region=="Total World Supply"),
             aes(Date,ifelse(month(Date)%in% c(3,6,9,12)&forecast==1,value,NA),group=version,shape=version,colour=version,fill=version),size=2.75)+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.5)+
  scale_x_date(breaks = "12 months",date_labels = "%Y",expand=c(0,0))+
  scale_shape_manual("",values=c(15,16,1,2,17,18,1))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(.20,.20))+
  # expand_limits(y=0)+
  expand_limits(x=ymd("2022-01-01"))+
  #scale_linetype_manual("",values=c(1,1))+
  #scale_color_viridis("",discrete = T,option="C",direction = 1,end = .9)+
  scale_color_manual("",values = colors_ua10())+
  scale_fill_viridis("",discrete = T,option="C",direction = 1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("EIA Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  weekly_graphs()+
  work_theme()+
  theme(plot.caption = element_blank())+#no caption
  guides(shape = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3),
         linetype = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3),
         colour = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3),
         fill = guide_legend(keywidth = unit(2.6,"cm"),nrow = 3))+
  labs(y="Global Liquids Supply (million barrels per day)",x="",
       #title=paste("Will the world oil market recover?"),
       #subtitle=paste("In early 2020, the COVID-19 pandemic dropped global oil demand to levels not seen since 2004. The initially-forecast rapid recovery has not materialized. What will 2021 bring?"),
       caption="Source: Historical data and forecasts via Energy Information Administration (EIA) Short-term Energy Outlook (STEO), graph by Andrew Leach.")
ggsave("supply_new.png",width=16,height = 9,dpi=600)

#WTI PRICE FORECASTS

#historical demand forecasts
steo_old_WTI_forecasts<-filter(steo_data_fetch(ymd("2008-1-1")),forecast==1) %>%
  rbind(filter(steo_data_fetch(ymd("2009-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2010-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2011-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2012-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2013-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2014-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2015-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2016-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2017-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2018-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2019-1-1")),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-1-1")),forecast==1))%>%
  filter(code %in% c("WTIPUUS"))%>%
  mutate(value=as.numeric(value),
         Region=as_factor(Region),
         version=factor(paste(month.abb[month(version)],year(version),"STEO"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))
#Oct 2013 STEO Apr 2017 STEO Jan 2020 STEO

wti_fc<-steo_data %>%filter(code %in% c("WTIPUUS"))%>%
  mutate(Region=as_factor(Region),
           version=factor(paste(format(max(supply_demand$version), "%b %Y"), "STEO"),
                          levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))%>%
  as.data.frame()%>%
  bind_rows(steo_old_WTI_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")))%>%
  mutate(version=factor(paste(month.abb[month(version)],year(version),"EIA STEO forecast",sep = " "),
  levels=paste(month.abb[month(unique(sort(version)))],year(unique(sort(version))),"EIA STEO forecast"))
  )

#wti_futs<-read_csv("nymex_wti_rec.csv")
#nymex_test<-wti_futs %>% filter(biz_dt==ymd("2020-04-29")) %>%
#  mutate(Date=ymd(paste(substr(mmy,1,4),substr(mmy,5,6),1,sep = "-")),
#         version=paste("NYMEX WTI Futures",format(biz_dt,"%b %d, %Y")),
#         forecast=3)%>%
#  rename("value"="settle_price")%>%
#  select(Date,value,version,forecast)%>% as.data.frame()

#forecasts<-levels(wti_fc$version)
#forwards<-unique(nymex_test$version)

#code to run off current nyumex
nymex_wti<-read_csv("ftp://ftp.cmegroup.com/pub/settle/nymex_future.csv")%>%clean_names()%>%
#code to run off specific date
#nymex_wti<-read_csv("nymex_futures_dec28.csv")%>%clean_names()%>%
  
#nymex_wti<-read_csv("nymex_futures_dec28.csv")%>%clean_names()%>%
  filter(product_description=="Crude Oil Last Day Financial Futures")%>%
  mutate(Date=ymd(paste(contract_year,contract_month,15)),
         version=paste("NYMEX WTI Futures",format(max(mdy(tradedate)),"%b %d, %Y")),
         forecast=3)%>%
  rename("value"="settle")%>%
  select(Date,value,version,forecast)%>%
  mutate(version=factor(version))

nymex_version<-as.character(unique(nymex_wti$version))


#wti_fcg<-wti_fc %>% bind_rows(nymex_wti) 
 
#WTI Historic prices
#PET.RWTC.M
wti_hist<-pdfetch_EIA(c("PET.RWTC.D"),KEY)
wti_hist<- setNames(wti_hist, "value")
wti_hist<-data.frame(date=index(wti_hist), coredata(wti_hist))
wti_hist <- wti_hist %>% mutate(date=ymd(date),
                                case = "NYMEX Historic WTI Price Data",
                                year=year(date),month=month(date))%>%
  group_by(month,year)%>% summarize(value=mean(value,na.rm = T))%>%
  mutate(date=ymd(paste(year,month,1,sep="-")))%>%
  filter(date>ymd("2005-01-01"))

 


shape_set<-c(0,1,2,5,6,15,16,17,18,19)

linetypes = c(apply(expand.grid(c(2,4), c(1,2,4,8,"A")), 1, paste, collapse=""), 
              apply(expand.grid(c(2,4,8), c(2,4), c(5,"F"), 2), 1, paste, collapse=""),
              "4284B4F4", "228F61A4")
  
 ggplot(filter(wti_fc,forecast==1))+
    #geom_line(data=filter(wti_fcg,Date>ymd("2010-01-01"),Date<ymd("2030-1-1")),aes(Date,value,group=version,colour=version),linetype="solid",size=1.15)+
    geom_line(data=wti_hist,aes(date,value,linetype="Monthly Average WTI Prices"),size=1.15)+
    geom_line(data=nymex_wti,aes(Date,value,group=version,linetype=version),size=1.15)+
    geom_line(aes(Date,value,group=version,colour=version),linetype="solid",size=1.15)+
    geom_point(aes(Date,ifelse(month(Date) %in% c(6,12),value,NA),group=version,colour=version,shape=version),size=2.15)+
    scale_x_date(breaks = "24 months",date_labels = "%Y",expand=c(0,0))+
    scale_shape_manual("",values=c(shape_set,shape_set))+ #skipped 19
    scale_y_continuous(breaks=pretty_breaks(),expand=c(.20,.20))+
    # expand_limits(y=0)+
    expand_limits(x=ymd("2022-01-01"))+
    #scale_linetype_manual("",values=c(1,1))+
    #scale_color_viridis("",discrete = T,option="C",direction = 1,end = .9)+
    scale_color_manual("",values = c(colors_tableau10()[-8],colors_tableau10_light()[-8],"grey80"))+
   #scale_color_manual("",values = c("grey80"))+
    
    scale_linetype_manual("",values=c("solid",linetypes[1]))+
    #scale_fill_manual("",values=colors_tableau10()[2])+
    #ajl_line()+
    weekly_graphs()+
    work_theme()+
    theme(plot.caption = element_blank())+#no caption
    guides(shape = guide_legend(keywidth = unit(1.6,"cm"),ncol = 2),
           linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 4),
           colour = guide_legend(keywidth = unit(1.6,"cm"),ncol = 2),
           NA
           )+
  labs(y="WTI Spot Monthly Average ($/bbl)",x="",
       #title=paste("WTI Monthly Average Spot Price History and Forecasts"),
       #subtitle=paste("Historic Values, EIA STEO Forecasts through ",format(max(supply_demand$version), "%B %Y"),", and ",nymex_version," settlements.",sep=""),
       caption="Source: Data via CME Group and EIA, graph by Andrew Leach.")

 ggsave("wti_fcast_nymex.png",width=16,height = 9,dpi=300)
 





#END WTI PRICE


#aeo henry hub gas
#annual energy outlooks
#get 2014 (when they bought the project), 2017 (comparability to hearings analysis), and now
series<-paste("AEO.",c(2014,2017,2020),".REF",c(2014,2017,2020),".PRCE_NOMP_TEN_NA_HHP_NA_USA_NDLRPMBTU.A",sep="")
series<-c(series,paste("AEO.",c(2014,2017,2020),".LOWPRICE.PRCE_NOMP_TEN_NA_HHP_NA_USA_NDLRPMBTU.A",sep=""))
hh_data<-pdfetch_EIA(series,KEY) 
names<-c(paste("AEO ",c(2014,2017,2020)," Reference Case",sep=""),paste("AEO ",c(2014,2017,2020)," Low Price Case",sep=""))
hh_data <- setNames(hh_data, names)
hh_data<-data.frame(date=index(hh_data), coredata(hh_data))
hh_data$date<-ymd(hh_data$date)
write_csv(hh_data,"aeo_hh_runs.csv")
hh_melt <- melt(hh_data,id="date",variable.name = "version")
hh_melt$version<-as.character(hh_melt$version)
hh_melt$version<-gsub("\\."," ",hh_melt$version)
hh_melt <- hh_melt%>%mutate(case = substr(version,10,100),
                              case=factor(case),
                              set=substr(version,1,8),
                              year=substr(version,5,8)
)



#hh Historic prices
#PET.RWTC.M
hh_hist<-pdfetch_EIA(c("NG.RNGWHHD.M"),KEY)
hh_hist<- setNames(hh_hist, "value")
hh_hist<-data.frame(date=index(hh_hist), coredata(hh_hist))
hh_hist <- hh_hist %>% mutate(date=ymd(date),
                                case = "Historic Data",
                                set=case,
                                case=factor(case),
                                version=case,
                                year=as.character(2020))%>%
  filter(date>ymd("2005-01-01"))
hh_melt<-hh_melt %>% bind_rows(hh_hist)%>%
  mutate(case=factor(case),case=fct_relevel(case,"Reference Case",after = 1))



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("hh_fcast_EIA.png")
ggplot(hh_melt)+
  geom_line(data=filter(hh_melt,set!="Historic Data"),aes(date,value,colour=set,group=version,linetype=case),size=1.5)+
  geom_line(data=filter(hh_melt,set=="Historic Data"),aes(date,value,group=version,linetype=case),size=1.5,colour="black")+
  scale_y_continuous(breaks=pretty_breaks(),limits=c(0,max(hh_melt$value)))+
  #scale_color_manual("",values = c(brewer.pal(3,"Blues")))+
  scale_color_manual("",values = (brewer.pal(3,"Greys")))+
  #scale_color_manual("",values = colors_tableau10())+
  scale_linetype_manual("",values=c(1,3,2))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "5 years",date_labels = "%b\n%Y",expand=c(0,0))+
  weekly_small()+
  theme(plot.subtitle = element_text(size = 10))+
  guides(col = guide_legend(keywidth = unit(1.6,"cm"),nrow = 3),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 3))+
  labs(y="Henry Hub Spot Price ($/MMBtu)",x="",
       title=paste("EIA Natural Gas Outlook"),
       subtitle=paste("Historic values and EIA Annual Energy Outlook (AEO) forecasts"),
       caption="Sources: EIA. Graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





#annual energy outlooks
#get 2014 (when they bought the project), 2017 (comparability to hearings analysis), and now
series<-paste("AEO.",c(2014,2017,2020),".REF",c(2014,2017,2020),".PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep="")
series<-c(series,paste("AEO.",c(2014,2017,2020),".LOWPRICE.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep=""))
series<-c(series,paste("AEO.",c(2014,2017,2020),".HIGHPRICE.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep=""))
wti_data<-pdfetch_EIA(series,KEY) 
names<-c(paste("AEO ",c(2014,2017,2020)," Reference Case",sep=""),paste("AEO ",c(2014,2017,2020)," Low Price Case",sep=""),paste("AEO ",c(2014,2017,2020)," High Price Case",sep=""))
wti_data <- setNames(wti_data, names)
wti_data<-data.frame(date=index(wti_data), coredata(wti_data))
wti_data$date<-ymd(wti_data$date)
write_csv(wti_data,"aeo_runs.csv")
wti_melt <- melt(wti_data,id="date",variable.name = "version")
wti_melt$version<-as.character(wti_melt$version)
wti_melt$version<-gsub("\\."," ",wti_melt$version)
wti_melt <- wti_melt%>%mutate(case = substr(version,10,100),
                              case=gsub(" Case","",case),
                              case=factor(case),
                              set=substr(version,1,8),
                              year=substr(version,5,8)
                              )

#WTI Historic prices
#PET.RWTC.M
wti_hist<-pdfetch_EIA(c("PET.RWTC.D"),KEY)
wti_hist<- setNames(wti_hist, "value")
wti_hist<-data.frame(date=index(wti_hist), coredata(wti_hist))
wti_hist <- wti_hist %>% mutate(date=ymd(date),
                              case = "NYMEX Historic WTI Price Data",
                              set=case,
                              case=factor(case),
                              version=case,
                              year=as.character(2020))%>%
  filter(date>ymd("2005-01-01"))

wti_melt<-wti_melt %>% bind_rows(wti_hist)%>%
  mutate(case=factor(case),case=fct_relevel(case,"Reference",after = 1))
#wti_melt<-wti_melt %>% rename("Date"="date") %>% filter(year(Date)<2024)


wti_wide<-wti_melt %>%filter(case!="NYMEX Historic WTI Price Data")%>%pivot_wider(-c(case,version),names_from=case,values_from=value)%>%
  rename(low=`Low Price`,high=`High Price`)%>% mutate(labels=paste("EIA Reference case and high and low price scenario range,","Annual Energy Oulook",year))
#%>%
#  left_join(wti_hist %>% select(date,wti_history=value))


#get CER 2020 prices

cer_2020<-read_csv("benchmark-prices-2020.csv")%>%clean_names()%>%filter(variable=="West Texas Intermediate (WTI) - US$/bbl",year>=2020)%>%
  mutate(date=ymd(paste(year,"-12-31")),
         value=value*1.02^(year-2020))%>% select(date,value,scenario)%>%
  pivot_wider(-scenario,values_from=value,names_from=scenario)%>%
  select(date,cer_reference=Reference,cer_evolving=Evolving)%>%
  mutate(labels="Reference and Evolving scenario range, CER Canada's Energy Future 2020")
         


cer_fx<-read_csv("macro-indicators-2020.csv")%>%clean_names()%>%filter(region=="Canada",variable %in% c("Consumer Price Index (2002=100)","Canada-US Exchange Rate (C$/US$)"))%>%
  mutate(date=ymd(paste(year,"-12-31")))


v_int<-c("West Texas Intermediate (WTI) - US$/bbl", "Western Canadian Select (WCS) - US$/bbl","Nova Inventory Transfer (NIT) - US$/MMBtu", "Canada-US Exchange Rate (C$/US$)","Consumer Price Index (2002=100)")


cer_data<-read_csv("benchmark-prices-2020.csv")%>%clean_names()%>% #filter(variable=="West Texas Intermediate (WTI) - US$/bbl",year>=2020)%>%
  mutate(date=ymd(paste(year,"-12-31")))%>% bind_rows(cer_fx%>%select(-region))%>%select(-year)%>% filter(variable %in% v_int)%>%
  pivot_wider(id=c(-scenario,-variable),values_from=value,names_from=c(variable,scenario))
  
#convert NIT to cad
cer_data$`Nova Inventory Transfer (NIT) - CA$/GJ_Evolving`<-cer_data$`Nova Inventory Transfer (NIT) - US$/MMBtu_Evolving`*cer_data$`Canada-US Exchange Rate (C$/US$)_Evolving`*0.94708628903179

cer_data$`Nova Inventory Transfer (NIT) - CA$/GJ_Reference`<-cer_data$`Nova Inventory Transfer (NIT) - US$/MMBtu_Reference`*cer_data$`Canada-US Exchange Rate (C$/US$)_Reference`*0.94708628903179

#Calculate differential
cer_data$`WCS_diff - CA$/bbl_Reference`<-(cer_data$`West Texas Intermediate (WTI) - US$/bbl_Reference`-cer_data$`Western Canadian Select (WCS) - US$/bbl_Reference`)*cer_data$`Canada-US Exchange Rate (C$/US$)_Reference`

cer_data$`WCS_diff - CA$/bbl_Evolving`<-(cer_data$`West Texas Intermediate (WTI) - US$/bbl_Evolving`-cer_data$`Western Canadian Select (WCS) - US$/bbl_Evolving`)*cer_data$`Canada-US Exchange Rate (C$/US$)_Evolving`

#invert fx
cer_data$`USD_CAD_Reference`=1/cer_data$`Canada-US Exchange Rate (C$/US$)_Reference`
cer_data$`USD_CAD_Evolving`=1/cer_data$`Canada-US Exchange Rate (C$/US$)_Evolving`

cer_data$year<-year(cer_data$date)

cer_data<-cer_data%>% select(c("year","West Texas Intermediate (WTI) - US$/bbl_Reference","West Texas Intermediate (WTI) - US$/bbl_Evolving",    
                               "Nova Inventory Transfer (NIT) - CA$/GJ_Reference","Nova Inventory Transfer (NIT) - CA$/GJ_Evolving",
                               "WCS_diff - CA$/bbl_Reference", "WCS_diff - CA$/bbl_Evolving",
                               "USD_CAD_Reference","USD_CAD_Evolving","Consumer Price Index (2002=100)_Reference","Consumer Price Index (2002=100)_Evolving"))%>%filter(year>=2021)

write_csv(cer_data,"cer_to_os_model.csv")


graph_palette<-colors_tableau10()

graph_palette<-viridis_pal(option = "H")(5)

graph_palette<-viridis(4,option="A",direction = -1)[2:4]

ggplot(filter(wti_wide,year(date)%%1==0,year!=2017))+
  geom_line(aes(date,Reference,group=set,color=labels),size=1.25)+
  
  geom_ribbon(aes(date,ymin=low,ymax=high,group=set,color=labels,fill=labels),alpha=.2,linetype="blank",size=.25)+
  
  geom_line(data=nymex_wti,aes(Date,value,group=version,linetype=version),size=1.25,color="black")+
  geom_line(data=wti_hist,aes(date,value,group=version,linetype=version),size=1.25,color="black")+
  
  geom_ribbon(data=cer_2020,aes(date,ymax=cer_reference,ymin=cer_evolving,fill=labels,color=labels),alpha=.2,linetype="blank",size=.25)+
  #geom_errorbar(data=filter(cer_2020,year(date)%%1==0),aes(date,ymax=cer_reference,ymin=cer_evolving,fill=labels,color=labels),linetype="solid",width=200)+
  #geom_point(data=nymex_test,aes(Date,value,group=version,color=version,fill=version,shape=version),size=2)+
  
  
  scale_fill_manual("",values = graph_palette)+
  scale_color_manual("",values = graph_palette)+
  
  #scale_fill_grey("",start = 0,end = 0.5)+
  #scale_color_grey("",start = 0,end = 0.5)+
  
  
  #scale_fill_manual("",values = c(viridis_cols[c(1,1,2,2,3,3)],"black","black"))+
  #scale_color_manual("",values = c(viridis_cols[c(1,1,2,2,3,3)],"black","black"))+
  scale_linetype_manual("",values=c("solid","11"))+
  #scale_shape_manual("",values=c(15,16,15,16,15,16,32,32))+
  scale_y_continuous(breaks=pretty_breaks())+
  expand_limits(y=0)+
  scale_x_date(breaks = "5 years",date_labels = "%b\n%Y",expand=c(0,0))+
  
  weekly_graphs()+
  work_theme()+
  theme(plot.caption = element_blank())+#no caption
  theme(plot.subtitle = element_blank(),plot.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))+
  guides(col = guide_legend(override.aes = list(linetype = c(0,1,1)),keywidth = unit(2.1,"cm"),nrow = 4,reverse = T),
         fill = guide_legend(keywidth = unit(2.1,"cm"),nrow = 4,reverse = T),
         linetype = guide_legend(keywidth = unit(2.1,"cm"),nrow = 2)
         
         #shape = guide_legend(keywidth = unit(2.4,"cm"),nrow = 2)
  )+
  labs(y="WTI Price ($/bbl)",x="",
       title=paste("EIA and CER WTI Outlook"),
       subtitle=paste("Historic and forward market settlement prices, CER Energy Futures 2020 and EIA Annual Energy Outlook (AEO) forecasts"),
       caption="Data via CME Group, Canadian Energy Regulator (CER), and the US Energy Information Administration (EIA). Graph by Andrew Leach.")
ggsave("wti_ribbon_nymex.png",dpi=300,width = 16)




#code used to build the NYMEX Data

dates<-seq.Date(ymd("2020-12-20"),ymd("2020-12-23"),by="1 day")
data_store <- list()
date_count<-1
for(date_sent in seq.Date(ymd("2020-12-20"),ymd("2020-12-23"),by="1 day")){
  date_sent<-as.Date(date_sent)
  #  nymex.settle.20200501.s
file<-paste("nymex.settle.",year(date_sent),formatC(month(date_sent),width=2, flag="0"),
            formatC(day(date_sent),width=2, flag="0"),".s.csv",sep = "")
print(file)
nymex_settle<-read_csv(file) %>% clean_names() %>%filter(sym=="CL")   
data_store[[date_count]]<-nymex_settle
date_count<-date_count+1
}
nymex_rec<-data.frame(do.call(rbind,data_store))
write_csv(nymex_rec,"nymex_wti_rec.csv")

nymex_grp<-nymex_rec %>% mutate(year=as.numeric(substr(mmy,1,4)))%>% group_by(biz_dt,year) %>%
  summarize(settle=mean(settle_price))
write_csv(nymex_rec,"nymex_wti_years.csv")


#wti_melt<- bind_rows(wti_melt,nymex_test)




#read from the web
nymex_file<-"ftp://ftp.cmegroup.com/pub/settle/nymex_future.csv"

nymex_file<-"nymex_futures_dec28.csv"
  
nymex<-read_csv(nymex_file)%>%clean_names()



#download.file("ftp://ftp.cmegroup.com/pub/settle/nymex_future.csv",destfile = "nymex_futures_dec28.csv")


nymex_wti<-read_csv(nymex_file)%>%clean_names()%>%
  filter(product_description=="Crude Oil Last Day Financial Futures")%>%
  mutate(Date=ymd(paste(contract_year,contract_month,15)),
         version=paste("NYMEX WTI Futures",format(max(mdy(tradedate)),"%b %d, %Y")))%>%
  rename("value"="settle")%>%
  select(Date,value,version)  



nymex_test<-nymex %>%
  filter(product_description=="Crude Oil Last Day Financial Futures")%>%
  mutate(Date=ymd(paste(contract_year,contract_month,15)),
         version=paste("NYMEX WTI Futures",format(max(mdy(tradedate)),"%b %d, %Y")))%>%
  rename("value"="settle")%>%
  select(Date,value,version)%>%
  filter(year(Date)<2023)

nymex_annual<-nymex %>%
  filter(product_description=="Crude Oil Last Day Financial Futures")%>%
  mutate(Date=ymd(paste(contract_year,contract_month,15)),
         version=paste("NYMEX WTI Futures",format(max(mdy(tradedate)),"%b %d, %Y")))%>%
  rename("value"="settle")%>%
  select(Date,value,version)%>%
  mutate(year=year(Date)) %>% group_by(year)%>% summarize(value=mean(value))
  write_csv(nymex_annual,"nymex_wti_years.csv")
  
#END WTI PRICE


#get ICE nat gas forwards


library(httr)
library(tidyverse)
library(lubridate)
library(rvest)
library(janitor)


ngx_read<-function(group,series_id,settlement)
{
query = list(
  htmlReport="",
  reportId=254,
  criteria.group=group,
  criteria.selectedMarket=series_id,
  criteria.selectedTimePeriod=settlement,
  rcMode=2
  )
#Settlement can be:
  #"Current Settlement"
  #Previous Day Settlement
  #Day-2 Settlement
  #Day-3 Settlement
  #Day-4 Settlement

#contracts:
#"NG Firm Phys, BS, LD1, (US/MM), ANR-SE-T">NG Firm Phys, BS, LD1, (US/MM), ANR-SE-T</option><option value="NG Firm Phys, BS, LD1, (US/MM), ANR-SW">NG Firm Phys, BS, LD1, (US/MM), ANR-SW</option><option value="NG Firm Phys, BS, LD1, (US/MM), APC-ACE">NG Firm Phys, BS, LD1, (US/MM), APC-ACE</option><option value="NG Firm Phys, BS, LD1, (US/MM), CG-Mainline">NG Firm Phys, BS, LD1, (US/MM), CG-Mainline</option><option value="NG Firm Phys, BS, LD1, (US/MM), CG-Onshore">NG Firm Phys, BS, LD1, (US/MM), CG-Onshore</option><option value="NG Firm Phys, BS, LD1, (US/MM), Cheyenne Hub">NG Firm Phys, BS, LD1, (US/MM), Cheyenne Hub</option><option value="NG Firm Phys, BS, LD1, (US/MM), Consumers">NG Firm Phys, BS, LD1, (US/MM), Consumers</option><option value="NG Firm Phys, BS, LD1, (US/MM), Cove Point">NG Firm Phys, BS, LD1, (US/MM), Cove Point</option><option value="NG Firm Phys, BS, LD1, (US/MM), Dominion-North">NG Firm Phys, BS, LD1, (US/MM), Dominion-North</option><option value="NG Firm Phys, BS, LD1, (US/MM), Dominion-South">NG Firm Phys, BS, LD1, (US/MM), Dominion-South</option><option value="NG Firm Phys, BS, LD1, (US/MM), EGT-Flex">NG Firm Phys, BS, LD1, (US/MM), EGT-Flex</option><option value="NG Firm Phys, BS, LD1, (US/MM), EP-Keystone">NG Firm Phys, BS, LD1, (US/MM), EP-Keystone</option><option value="NG Firm Phys, BS, LD1, (US/MM), EP-SJ Blanco Pool">NG Firm Phys, BS, LD1, (US/MM), EP-SJ Blanco Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), FGT-Z3">NG Firm Phys, BS, LD1, (US/MM), FGT-Z3</option><option value="NG Firm Phys, BS, LD1, (US/MM), GTN-Kingsgate">NG Firm Phys, BS, LD1, (US/MM), GTN-Kingsgate</option><option value="NG Firm Phys, BS, LD1, (US/MM), GTN-Malin">NG Firm Phys, BS, LD1, (US/MM), GTN-Malin</option><option value="NG Firm Phys, BS, LD1, (US/MM), GTN-Stanfield">NG Firm Phys, BS, LD1, (US/MM), GTN-Stanfield</option><option value="NG Firm Phys, BS, LD1, (US/MM), HSC-HPL Pool">NG Firm Phys, BS, LD1, (US/MM), HSC-HPL Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), Henry">NG Firm Phys, BS, LD1, (US/MM), Henry</option><option value="NG Firm Phys, BS, LD1, (US/MM), Iroquois (Into)">NG Firm Phys, BS, LD1, (US/MM), Iroquois (Into)</option><option value="NG Firm Phys, BS, LD1, (US/MM), KRGT-Del Pool">NG Firm Phys, BS, LD1, (US/MM), KRGT-Del Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), KRGT-Rec Pool">NG Firm Phys, BS, LD1, (US/MM), KRGT-Rec Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), Katy-Enstor Pool">NG Firm Phys, BS, LD1, (US/MM), Katy-Enstor Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), Katy-Oasis">NG Firm Phys, BS, LD1, (US/MM), Katy-Oasis</option><option value="NG Firm Phys, BS, LD1, (US/MM), Leidy-Transco">NG Firm Phys, BS, LD1, (US/MM), Leidy-Transco</option><option value="NG Firm Phys, BS, LD1, (US/MM), Michcon">NG Firm Phys, BS, LD1, (US/MM), Michcon</option><option value="NG Firm Phys, BS, LD1, (US/MM), Millennium East Pool">NG Firm Phys, BS, LD1, (US/MM), Millennium East Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), Moss Bluff Inter">NG Firm Phys, BS, LD1, (US/MM), Moss Bluff Inter</option><option value="NG Firm Phys, BS, LD1, (US/MM), NBPL-Ventura TP">NG Firm Phys, BS, LD1, (US/MM), NBPL-Ventura TP</option><option value="NG Firm Phys, BS, LD1, (US/MM), NGPL-Midcont Pool">NG Firm Phys, BS, LD1, (US/MM), NGPL-Midcont Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), NGPL-Nicor">NG Firm Phys, BS, LD1, (US/MM), NGPL-Nicor</option><option value="NG Firm Phys, BS, LD1, (US/MM), NGPL-Nipsco">NG Firm Phys, BS, LD1, (US/MM), NGPL-Nipsco</option><option value="NG Firm Phys, BS, LD1, (US/MM), NGPL-STX">NG Firm Phys, BS, LD1, (US/MM), NGPL-STX</option><option value="NG Firm Phys, BS, LD1, (US/MM), NGPL-TXOK East">NG Firm Phys, BS, LD1, (US/MM), NGPL-TXOK East</option><option value="NG Firm Phys, BS, LD1, (US/MM), NNG-Demarc">NG Firm Phys, BS, LD1, (US/MM), NNG-Demarc</option><option value="NG Firm Phys, BS, LD1, (US/MM), NNG-Ventura">NG Firm Phys, BS, LD1, (US/MM), NNG-Ventura</option><option value="NG Firm Phys, BS, LD1, (US/MM), OGT">NG Firm Phys, BS, LD1, (US/MM), OGT</option><option value="NG Firm Phys, BS, LD1, (US/MM), Oasis-Waha Pool">NG Firm Phys, BS, LD1, (US/MM), Oasis-Waha Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), Opal">NG Firm Phys, BS, LD1, (US/MM), Opal</option><option value="NG Firm Phys, BS, LD1, (US/MM), PGE-Citygate">NG Firm Phys, BS, LD1, (US/MM), PGE-Citygate</option><option value="NG Firm Phys, BS, LD1, (US/MM), Panhandle">NG Firm Phys, BS, LD1, (US/MM), Panhandle</option><option value="NG Firm Phys, BS, LD1, (US/MM), Pine Prairie">NG Firm Phys, BS, LD1, (US/MM), Pine Prairie</option><option value="NG Firm Phys, BS, LD1, (US/MM), REX E-ANR">NG Firm Phys, BS, LD1, (US/MM), REX E-ANR</option><option value="NG Firm Phys, BS, LD1, (US/MM), SoCal-Citygate">NG Firm Phys, BS, LD1, (US/MM), SoCal-Citygate</option><option value="NG Firm Phys, BS, LD1, (US/MM), Sonat-Z0 South">NG Firm Phys, BS, LD1, (US/MM), Sonat-Z0 South</option><option value="NG Firm Phys, BS, LD1, (US/MM), Stagecoach Marcellus">NG Firm Phys, BS, LD1, (US/MM), Stagecoach Marcellus</option><option value="NG Firm Phys, BS, LD1, (US/MM), TCO">NG Firm Phys, BS, LD1, (US/MM), TCO</option><option value="NG Firm Phys, BS, LD1, (US/MM), TCO-A04 Pool">NG Firm Phys, BS, LD1, (US/MM), TCO-A04 Pool</option><option value="NG Firm Phys, BS, LD1, (US/MM), TETCO-ELA">NG Firm Phys, BS, LD1, (US/MM), TETCO-ELA</option><option value="NG Firm Phys, BS, LD1, (US/MM), TETCO-M1 30">NG Firm Phys, BS, LD1, (US/MM), TETCO-M1 30</option><option value="NG Firm Phys, BS, LD1, (US/MM), TETCO-M2 (receipts)">NG Firm Phys, BS, LD1, (US/MM), TETCO-M2 (receipts)</option><option value="NG Firm Phys, BS, LD1, (US/MM), TETCO-M3">NG Firm Phys, BS, LD1, (US/MM), TETCO-M3</option><option value="NG Firm Phys, BS, LD1, (US/MM), TETCO-STX">NG Firm Phys, BS, LD1, (US/MM), TETCO-STX</option><option value="NG Firm Phys, BS, LD1, (US/MM), TETCO-WLA">NG Firm Phys, BS, LD1, (US/MM), TETCO-WLA</option><option value="NG Firm Phys, BS, LD1, (US/MM), TGP-500L">NG Firm Phys, BS, LD1, (US/MM), TGP-500L</option><option value="NG Firm Phys, BS, LD1, (US/MM), TGP-800L">NG Firm Phys, BS, LD1, (US/MM), TGP-800L</option><option value="NG Firm Phys, BS, LD1, (US/MM), TGP-Z0 North">NG Firm Phys, BS, LD1, (US/MM), TGP-Z0 North</option><option value="NG Firm Phys, BS, LD1, (US/MM), TGP-Z0 South">NG Firm Phys, BS, LD1, (US/MM), TGP-Z0 South</option><option value="NG Firm Phys, BS, LD1, (US/MM), TGT-Mainline">NG Firm Phys, BS, LD1, (US/MM), TGT-Mainline</option><option value="NG Firm Phys, BS, LD1, (US/MM), TGT-North LA">NG Firm Phys, BS, LD1, (US/MM), TGT-North LA</option><option value="NG Firm Phys, BS, LD1, (US/MM), TGT-SL">NG Firm Phys, BS, LD1, (US/MM), TGT-SL</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-30">NG Firm Phys, BS, LD1, (US/MM), Transco-30</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-65">NG Firm Phys, BS, LD1, (US/MM), Transco-65</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-85">NG Firm Phys, BS, LD1, (US/MM), Transco-85</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-River Road">NG Firm Phys, BS, LD1, (US/MM), Transco-River Road</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-Z5 South">NG Firm Phys, BS, LD1, (US/MM), Transco-Z5 South</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 (non-NY north ML)">NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 (non-NY north ML)</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 NY">NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 NY</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 Sta-210">NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 Sta-210</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 non-NY">NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 non-NY</option><option value="NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 non-NY north">NG Firm Phys, BS, LD1, (US/MM), Transco-Z6 non-NY north</option><option value="NG Firm Phys, BS, LD1, (US/MM), Trunkline-ELA">NG Firm Phys, BS, LD1, (US/MM), Trunkline-ELA</option><option value="NG Firm Phys, BS, LD1, (US/MM), Trunkline-WLA">NG Firm Phys, BS, LD1, (US/MM), Trunkline-WLA</option><option value="NG Firm Phys, BS, LD1, (US/MM), Trunkline-Z1A">NG Firm Phys, BS, LD1, (US/MM), Trunkline-Z1A</option><option value="NG Firm Phys, BS, LD1, (US/MM), Waha">NG Firm Phys, BS, LD1, (US/MM), Waha</option><option value="NG Firm Phys, FP, (US/MM), ANR-SE-T">NG Firm Phys, FP, (US/MM), ANR-SE-T</option><option value="NG Firm Phys, FP, (US/MM), ANR-SW">NG Firm Phys, FP, (US/MM), ANR-SW</option><option value="NG Firm Phys, FP, (US/MM), APC-ACE">NG Firm Phys, FP, (US/MM), APC-ACE</option><option value="NG Firm Phys, FP, (US/MM), CG-Mainline">NG Firm Phys, FP, (US/MM), CG-Mainline</option><option value="NG Firm Phys, FP, (US/MM), CG-Onshore">NG Firm Phys, FP, (US/MM), CG-Onshore</option><option value="NG Firm Phys, FP, (US/MM), Cheyenne Hub">NG Firm Phys, FP, (US/MM), Cheyenne Hub</option><option value="NG Firm Phys, FP, (US/MM), Consumers">NG Firm Phys, FP, (US/MM), Consumers</option><option value="NG Firm Phys, FP, (US/MM), Cove Point">NG Firm Phys, FP, (US/MM), Cove Point</option><option value="NG Firm Phys, FP, (US/MM), Dominion-North">NG Firm Phys, FP, (US/MM), Dominion-North</option><option value="NG Firm Phys, FP, (US/MM), Dominion-South">NG Firm Phys, FP, (US/MM), Dominion-South</option><option value="NG Firm Phys, FP, (US/MM), EGT-Flex">NG Firm Phys, FP, (US/MM), EGT-Flex</option><option value="NG Firm Phys, FP, (US/MM), EP-Keystone">NG Firm Phys, FP, (US/MM), EP-Keystone</option><option value="NG Firm Phys, FP, (US/MM), EP-SJ Blanco Pool">NG Firm Phys, FP, (US/MM), EP-SJ Blanco Pool</option><option value="NG Firm Phys, FP, (US/MM), FGT-Z3">NG Firm Phys, FP, (US/MM), FGT-Z3</option><option value="NG Firm Phys, FP, (US/MM), GTN-Kingsgate">NG Firm Phys, FP, (US/MM), GTN-Kingsgate</option><option value="NG Firm Phys, FP, (US/MM), GTN-Malin">NG Firm Phys, FP, (US/MM), GTN-Malin</option><option value="NG Firm Phys, FP, (US/MM), GTN-Stanfield">NG Firm Phys, FP, (US/MM), GTN-Stanfield</option><option value="NG Firm Phys, FP, (US/MM), HSC-HPL Pool">NG Firm Phys, FP, (US/MM), HSC-HPL Pool</option><option value="NG Firm Phys, FP, (US/MM), Henry">NG Firm Phys, FP, (US/MM), Henry</option><option value="NG Firm Phys, FP, (US/MM), Iroquois (Into)">NG Firm Phys, FP, (US/MM), Iroquois (Into)</option><option value="NG Firm Phys, FP, (US/MM), KRGT-Del Pool">NG Firm Phys, FP, (US/MM), KRGT-Del Pool</option><option value="NG Firm Phys, FP, (US/MM), KRGT-Rec Pool">NG Firm Phys, FP, (US/MM), KRGT-Rec Pool</option><option value="NG Firm Phys, FP, (US/MM), Katy-Enstor Pool">NG Firm Phys, FP, (US/MM), Katy-Enstor Pool</option><option value="NG Firm Phys, FP, (US/MM), Katy-Oasis">NG Firm Phys, FP, (US/MM), Katy-Oasis</option><option value="NG Firm Phys, FP, (US/MM), Leidy-Transco">NG Firm Phys, FP, (US/MM), Leidy-Transco</option><option value="NG Firm Phys, FP, (US/MM), Michcon">NG Firm Phys, FP, (US/MM), Michcon</option><option value="NG Firm Phys, FP, (US/MM), Millennium East Pool">NG Firm Phys, FP, (US/MM), Millennium East Pool</option><option value="NG Firm Phys, FP, (US/MM), Moss Bluff Inter">NG Firm Phys, FP, (US/MM), Moss Bluff Inter</option><option value="NG Firm Phys, FP, (US/MM), NBPL-Ventura TP">NG Firm Phys, FP, (US/MM), NBPL-Ventura TP</option><option value="NG Firm Phys, FP, (US/MM), NGPL-Midcont Pool">NG Firm Phys, FP, (US/MM), NGPL-Midcont Pool</option><option value="NG Firm Phys, FP, (US/MM), NGPL-Nicor">NG Firm Phys, FP, (US/MM), NGPL-Nicor</option><option value="NG Firm Phys, FP, (US/MM), NGPL-Nipsco">NG Firm Phys, FP, (US/MM), NGPL-Nipsco</option><option value="NG Firm Phys, FP, (US/MM), NGPL-STX">NG Firm Phys, FP, (US/MM), NGPL-STX</option><option value="NG Firm Phys, FP, (US/MM), NGPL-TXOK East">NG Firm Phys, FP, (US/MM), NGPL-TXOK East</option><option value="NG Firm Phys, FP, (US/MM), NNG-Demarc">NG Firm Phys, FP, (US/MM), NNG-Demarc</option><option value="NG Firm Phys, FP, (US/MM), NNG-Ventura">NG Firm Phys, FP, (US/MM), NNG-Ventura</option><option value="NG Firm Phys, FP, (US/MM), OGT">NG Firm Phys, FP, (US/MM), OGT</option><option value="NG Firm Phys, FP, (US/MM), Oasis-Waha Pool">NG Firm Phys, FP, (US/MM), Oasis-Waha Pool</option><option value="NG Firm Phys, FP, (US/MM), Opal">NG Firm Phys, FP, (US/MM), Opal</option><option value="NG Firm Phys, FP, (US/MM), PGE-Citygate">NG Firm Phys, FP, (US/MM), PGE-Citygate</option><option value="NG Firm Phys, FP, (US/MM), Panhandle">NG Firm Phys, FP, (US/MM), Panhandle</option><option value="NG Firm Phys, FP, (US/MM), Pine Prairie">NG Firm Phys, FP, (US/MM), Pine Prairie</option><option value="NG Firm Phys, FP, (US/MM), REX E-ANR">NG Firm Phys, FP, (US/MM), REX E-ANR</option><option value="NG Firm Phys, FP, (US/MM), SoCal-Citygate">NG Firm Phys, FP, (US/MM), SoCal-Citygate</option><option value="NG Firm Phys, FP, (US/MM), Sonat-Z0 South">NG Firm Phys, FP, (US/MM), Sonat-Z0 South</option><option value="NG Firm Phys, FP, (US/MM), Stagecoach Marcellus">NG Firm Phys, FP, (US/MM), Stagecoach Marcellus</option><option value="NG Firm Phys, FP, (US/MM), TCO">NG Firm Phys, FP, (US/MM), TCO</option><option value="NG Firm Phys, FP, (US/MM), TCO-A04 Pool">NG Firm Phys, FP, (US/MM), TCO-A04 Pool</option><option value="NG Firm Phys, FP, (US/MM), TETCO-ELA">NG Firm Phys, FP, (US/MM), TETCO-ELA</option><option value="NG Firm Phys, FP, (US/MM), TETCO-M1 30">NG Firm Phys, FP, (US/MM), TETCO-M1 30</option><option value="NG Firm Phys, FP, (US/MM), TETCO-M2 (receipts)">NG Firm Phys, FP, (US/MM), TETCO-M2 (receipts)</option><option value="NG Firm Phys, FP, (US/MM), TETCO-M3">NG Firm Phys, FP, (US/MM), TETCO-M3</option><option value="NG Firm Phys, FP, (US/MM), TETCO-STX">NG Firm Phys, FP, (US/MM), TETCO-STX</option><option value="NG Firm Phys, FP, (US/MM), TETCO-WLA">NG Firm Phys, FP, (US/MM), TETCO-WLA</option><option value="NG Firm Phys, FP, (US/MM), TGP-500L">NG Firm Phys, FP, (US/MM), TGP-500L</option><option value="NG Firm Phys, FP, (US/MM), TGP-800L">NG Firm Phys, FP, (US/MM), TGP-800L</option><option value="NG Firm Phys, FP, (US/MM), TGP-Z0 North">NG Firm Phys, FP, (US/MM), TGP-Z0 North</option><option value="NG Firm Phys, FP, (US/MM), TGP-Z0 South">NG Firm Phys, FP, (US/MM), TGP-Z0 South</option><option value="NG Firm Phys, FP, (US/MM), TGT-Mainline">NG Firm Phys, FP, (US/MM), TGT-Mainline</option><option value="NG Firm Phys, FP, (US/MM), TGT-North LA">NG Firm Phys, FP, (US/MM), TGT-North LA</option><option value="NG Firm Phys, FP, (US/MM), TGT-SL">NG Firm Phys, FP, (US/MM), TGT-SL</option><option value="NG Firm Phys, FP, (US/MM), Transco-30">NG Firm Phys, FP, (US/MM), Transco-30</option><option value="NG Firm Phys, FP, (US/MM), Transco-65">NG Firm Phys, FP, (US/MM), Transco-65</option><option value="NG Firm Phys, FP, (US/MM), Transco-85">NG Firm Phys, FP, (US/MM), Transco-85</option><option value="NG Firm Phys, FP, (US/MM), Transco-River Road">NG Firm Phys, FP, (US/MM), Transco-River Road</option><option value="NG Firm Phys, FP, (US/MM), Transco-Z5 South">NG Firm Phys, FP, (US/MM), Transco-Z5 South</option><option value="NG Firm Phys, FP, (US/MM), Transco-Z6 (non-NY north ML)">NG Firm Phys, FP, (US/MM), Transco-Z6 (non-NY north ML)</option><option value="NG Firm Phys, FP, (US/MM), Transco-Z6 NY">NG Firm Phys, FP, (US/MM), Transco-Z6 NY</option><option value="NG Firm Phys, FP, (US/MM), Transco-Z6 Sta-210">NG Firm Phys, FP, (US/MM), Transco-Z6 Sta-210</option><option value="NG Firm Phys, FP, (US/MM), Transco-Z6 non-NY">NG Firm Phys, FP, (US/MM), Transco-Z6 non-NY</option><option value="NG Firm Phys, FP, (US/MM), Transco-Z6 non-NY north">NG Firm Phys, FP, (US/MM), Transco-Z6 non-NY north</option><option value="NG Firm Phys, FP, (US/MM), Trunkline-ELA">NG Firm Phys, FP, (US/MM), Trunkline-ELA</option><option value="NG Firm Phys, FP, (US/MM), Trunkline-WLA">NG Firm Phys, FP, (US/MM), Trunkline-WLA</option><option value="NG Firm Phys, FP, (US/MM), Trunkline-Z1A">NG Firm Phys, FP, (US/MM), Trunkline-Z1A</option><option value="NG Firm Phys, FP, (US/MM), Waha">NG Firm Phys, FP, (US/MM), Waha</option><option value="NG Firm Phys, ID, GDD, (US/MM), ANR-SE-T">NG Firm Phys, ID, GDD, (US/MM), ANR-SE-T</option><option value="NG Firm Phys, ID, GDD, (US/MM), ANR-SW">NG Firm Phys, ID, GDD, (US/MM), ANR-SW</option><option value="NG Firm Phys, ID, GDD, (US/MM), APC-ACE">NG Firm Phys, ID, GDD, (US/MM), APC-ACE</option><option value="NG Firm Phys, ID, GDD, (US/MM), CG-Mainline">NG Firm Phys, ID, GDD, (US/MM), CG-Mainline</option><option value="NG Firm Phys, ID, GDD, (US/MM), CG-Onshore">NG Firm Phys, ID, GDD, (US/MM), CG-Onshore</option><option value="NG Firm Phys, ID, GDD, (US/MM), Cheyenne Hub">NG Firm Phys, ID, GDD, (US/MM), Cheyenne Hub</option><option value="NG Firm Phys, ID, GDD, (US/MM), Consumers">NG Firm Phys, ID, GDD, (US/MM), Consumers</option><option value="NG Firm Phys, ID, GDD, (US/MM), Cove Point">NG Firm Phys, ID, GDD, (US/MM), Cove Point</option><option value="NG Firm Phys, ID, GDD, (US/MM), Dominion-North">NG Firm Phys, ID, GDD, (US/MM), Dominion-North</option><option value="NG Firm Phys, ID, GDD, (US/MM), Dominion-South">NG Firm Phys, ID, GDD, (US/MM), Dominion-South</option><option value="NG Firm Phys, ID, GDD, (US/MM), EGT-Flex">NG Firm Phys, ID, GDD, (US/MM), EGT-Flex</option><option value="NG Firm Phys, ID, GDD, (US/MM), EP-Keystone (EP Perm id)">NG Firm Phys, ID, GDD, (US/MM), EP-Keystone (EP Perm id)</option><option value="NG Firm Phys, ID, GDD, (US/MM), EP-Keystone (EP WTX id)">NG Firm Phys, ID, GDD, (US/MM), EP-Keystone (EP WTX id)</option><option value="NG Firm Phys, ID, GDD, (US/MM), EP-SJ Blanco Pool">NG Firm Phys, ID, GDD, (US/MM), EP-SJ Blanco Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), FGT-Z3">NG Firm Phys, ID, GDD, (US/MM), FGT-Z3</option><option value="NG Firm Phys, ID, GDD, (US/MM), GTN-Kingsgate">NG Firm Phys, ID, GDD, (US/MM), GTN-Kingsgate</option><option value="NG Firm Phys, ID, GDD, (US/MM), GTN-Malin">NG Firm Phys, ID, GDD, (US/MM), GTN-Malin</option><option value="NG Firm Phys, ID, GDD, (US/MM), GTN-Stanfield">NG Firm Phys, ID, GDD, (US/MM), GTN-Stanfield</option><option value="NG Firm Phys, ID, GDD, (US/MM), HSC-HPL Pool">NG Firm Phys, ID, GDD, (US/MM), HSC-HPL Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), Henry">NG Firm Phys, ID, GDD, (US/MM), Henry</option><option value="NG Firm Phys, ID, GDD, (US/MM), Iroquois (Into)">NG Firm Phys, ID, GDD, (US/MM), Iroquois (Into)</option><option value="NG Firm Phys, ID, GDD, (US/MM), KRGT-Del Pool">NG Firm Phys, ID, GDD, (US/MM), KRGT-Del Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), KRGT-Rec Pool">NG Firm Phys, ID, GDD, (US/MM), KRGT-Rec Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), Katy-Enstor Pool">NG Firm Phys, ID, GDD, (US/MM), Katy-Enstor Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), Katy-Oasis">NG Firm Phys, ID, GDD, (US/MM), Katy-Oasis</option><option value="NG Firm Phys, ID, GDD, (US/MM), Leidy-Transco">NG Firm Phys, ID, GDD, (US/MM), Leidy-Transco</option><option value="NG Firm Phys, ID, GDD, (US/MM), Michcon">NG Firm Phys, ID, GDD, (US/MM), Michcon</option><option value="NG Firm Phys, ID, GDD, (US/MM), Millennium East Pool">NG Firm Phys, ID, GDD, (US/MM), Millennium East Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), NBPL-Ventura TP">NG Firm Phys, ID, GDD, (US/MM), NBPL-Ventura TP</option><option value="NG Firm Phys, ID, GDD, (US/MM), NGPL-Midcont Pool">NG Firm Phys, ID, GDD, (US/MM), NGPL-Midcont Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), NGPL-Nicor">NG Firm Phys, ID, GDD, (US/MM), NGPL-Nicor</option><option value="NG Firm Phys, ID, GDD, (US/MM), NGPL-Nipsco">NG Firm Phys, ID, GDD, (US/MM), NGPL-Nipsco</option><option value="NG Firm Phys, ID, GDD, (US/MM), NGPL-STX">NG Firm Phys, ID, GDD, (US/MM), NGPL-STX</option><option value="NG Firm Phys, ID, GDD, (US/MM), NGPL-TXOK East">NG Firm Phys, ID, GDD, (US/MM), NGPL-TXOK East</option><option value="NG Firm Phys, ID, GDD, (US/MM), NNG-Demarc">NG Firm Phys, ID, GDD, (US/MM), NNG-Demarc</option><option value="NG Firm Phys, ID, GDD, (US/MM), NNG-Ventura">NG Firm Phys, ID, GDD, (US/MM), NNG-Ventura</option><option value="NG Firm Phys, ID, GDD, (US/MM), OGT">NG Firm Phys, ID, GDD, (US/MM), OGT</option><option value="NG Firm Phys, ID, GDD, (US/MM), Oasis-Waha Pool">NG Firm Phys, ID, GDD, (US/MM), Oasis-Waha Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), Opal">NG Firm Phys, ID, GDD, (US/MM), Opal</option><option value="NG Firm Phys, ID, GDD, (US/MM), PGE-Citygate">NG Firm Phys, ID, GDD, (US/MM), PGE-Citygate</option><option value="NG Firm Phys, ID, GDD, (US/MM), Panhandle">NG Firm Phys, ID, GDD, (US/MM), Panhandle</option><option value="NG Firm Phys, ID, GDD, (US/MM), Pine Prairie">NG Firm Phys, ID, GDD, (US/MM), Pine Prairie</option><option value="NG Firm Phys, ID, GDD, (US/MM), REX E-ANR">NG Firm Phys, ID, GDD, (US/MM), REX E-ANR</option><option value="NG Firm Phys, ID, GDD, (US/MM), SoCal-Citygate">NG Firm Phys, ID, GDD, (US/MM), SoCal-Citygate</option><option value="NG Firm Phys, ID, GDD, (US/MM), Sonat-Z0 South">NG Firm Phys, ID, GDD, (US/MM), Sonat-Z0 South</option><option value="NG Firm Phys, ID, GDD, (US/MM), TCO">NG Firm Phys, ID, GDD, (US/MM), TCO</option><option value="NG Firm Phys, ID, GDD, (US/MM), TCO-A04 Pool">NG Firm Phys, ID, GDD, (US/MM), TCO-A04 Pool</option><option value="NG Firm Phys, ID, GDD, (US/MM), TETCO-ELA">NG Firm Phys, ID, GDD, (US/MM), TETCO-ELA</option><option value="NG Firm Phys, ID, GDD, (US/MM), TETCO-M1 30">NG Firm Phys, ID, GDD, (US/MM), TETCO-M1 30</option><option value="NG Firm Phys, ID, GDD, (US/MM), TETCO-M2 (receipts)">NG Firm Phys, ID, GDD, (US/MM), TETCO-M2 (receipts)</option><option value="NG Firm Phys, ID, GDD, (US/MM), TETCO-M3">NG Firm Phys, ID, GDD, (US/MM), TETCO-M3</option><option value="NG Firm Phys, ID, GDD, (US/MM), TETCO-STX">NG Firm Phys, ID, GDD, (US/MM), TETCO-STX</option><option value="NG Firm Phys, ID, GDD, (US/MM), TETCO-WLA">NG Firm Phys, ID, GDD, (US/MM), TETCO-WLA</option><option value="NG Firm Phys, ID, GDD, (US/MM), TGP-500L">NG Firm Phys, ID, GDD, (US/MM), TGP-500L</option><option value="NG Firm Phys, ID, GDD, (US/MM), TGP-800L">NG Firm Phys, ID, GDD, (US/MM), TGP-800L</option><option value="NG Firm Phys, ID, GDD, (US/MM), TGP-Z0 North">NG Firm Phys, ID, GDD, (US/MM), TGP-Z0 North</option><option value="NG Firm Phys, ID, GDD, (US/MM), TGP-Z0 South">NG Firm Phys, ID, GDD, (US/MM), TGP-Z0 South</option><option value="NG Firm Phys, ID, GDD, (US/MM), TGT-Mainline">NG Firm Phys, ID, GDD, (US/MM), TGT-Mainline</option><option value="NG Firm Phys, ID, GDD, (US/MM), TGT-North LA">NG Firm Phys, ID, GDD, (US/MM), TGT-North LA</option><option value="NG Firm Phys, ID, GDD, (US/MM), TGT-SL">NG Firm Phys, ID, GDD, (US/MM), TGT-SL</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-30">NG Firm Phys, ID, GDD, (US/MM), Transco-30</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-65">NG Firm Phys, ID, GDD, (US/MM), Transco-65</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-85">NG Firm Phys, ID, GDD, (US/MM), Transco-85</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-River Road">NG Firm Phys, ID, GDD, (US/MM), Transco-River Road</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-Z5 South">NG Firm Phys, ID, GDD, (US/MM), Transco-Z5 South</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 (non-NY north ML)">NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 (non-NY north ML)</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 NY">NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 NY</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 Sta-210">NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 Sta-210</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 non-NY">NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 non-NY</option><option value="NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 non-NY north">NG Firm Phys, ID, GDD, (US/MM), Transco-Z6 non-NY north</option><option value="NG Firm Phys, ID, GDD, (US/MM), Trunkline-ELA">NG Firm Phys, ID, GDD, (US/MM), Trunkline-ELA</option><option value="NG Firm Phys, ID, GDD, (US/MM), Trunkline-WLA">NG Firm Phys, ID, GDD, (US/MM), Trunkline-WLA</option><option value="NG Firm Phys, ID, GDD, (US/MM), Trunkline-Z1A">NG Firm Phys, ID, GDD, (US/MM), Trunkline-Z1A</option><option value="NG Firm Phys, ID, GDD, (US/MM), Waha">NG Firm Phys, ID, GDD, (US/MM), Waha</option><option value="NG Firm Phys, ID, IF, (US/MM), ANR-SE-T">NG Firm Phys, ID, IF, (US/MM), ANR-SE-T</option><option value="NG Firm Phys, ID, IF, (US/MM), ANR-SW">NG Firm Phys, ID, IF, (US/MM), ANR-SW</option><option value="NG Firm Phys, ID, IF, (US/MM), CG-Mainline">NG Firm Phys, ID, IF, (US/MM), CG-Mainline</option><option value="NG Firm Phys, ID, IF, (US/MM), CG-Onshore">NG Firm Phys, ID, IF, (US/MM), CG-Onshore</option><option value="NG Firm Phys, ID, IF, (US/MM), Cheyenne Hub">NG Firm Phys, ID, IF, (US/MM), Cheyenne Hub</option><option value="NG Firm Phys, ID, IF, (US/MM), Consumers">NG Firm Phys, ID, IF, (US/MM), Consumers</option><option value="NG Firm Phys, ID, IF, (US/MM), Dominion-North">NG Firm Phys, ID, IF, (US/MM), Dominion-North</option><option value="NG Firm Phys, ID, IF, (US/MM), Dominion-South">NG Firm Phys, ID, IF, (US/MM), Dominion-South</option><option value="NG Firm Phys, ID, IF, (US/MM), EGT-Flex">NG Firm Phys, ID, IF, (US/MM), EGT-Flex</option><option value="NG Firm Phys, ID, IF, (US/MM), EP-Keystone (EP Perm id)">NG Firm Phys, ID, IF, (US/MM), EP-Keystone (EP Perm id)</option><option value="NG Firm Phys, ID, IF, (US/MM), EP-Keystone (EP WTX id)">NG Firm Phys, ID, IF, (US/MM), EP-Keystone (EP WTX id)</option><option value="NG Firm Phys, ID, IF, (US/MM), EP-SJ Blanco Pool">NG Firm Phys, ID, IF, (US/MM), EP-SJ Blanco Pool</option><option value="NG Firm Phys, ID, IF, (US/MM), FGT-Z3">NG Firm Phys, ID, IF, (US/MM), FGT-Z3</option><option value="NG Firm Phys, ID, IF, (US/MM), HSC-HPL Pool">NG Firm Phys, ID, IF, (US/MM), HSC-HPL Pool</option><option value="NG Firm Phys, ID, IF, (US/MM), Henry">NG Firm Phys, ID, IF, (US/MM), Henry</option><option value="NG Firm Phys, ID, IF, (US/MM), Iroquois (Into)">NG Firm Phys, ID, IF, (US/MM), Iroquois (Into)</option><option value="NG Firm Phys, ID, IF, (US/MM), KRGT-Rec Pool">NG Firm Phys, ID, IF, (US/MM), KRGT-Rec Pool</option><option value="NG Firm Phys, ID, IF, (US/MM), Katy-Enstor Pool (HSC id)">NG Firm Phys, ID, IF, (US/MM), Katy-Enstor Pool (HSC id)</option><option value="NG Firm Phys, ID, IF, (US/MM), Katy-Enstor Pool (Katy id)">NG Firm Phys, ID, IF, (US/MM), Katy-Enstor Pool (Katy id)</option><option value="NG Firm Phys, ID, IF, (US/MM), Katy-Oasis (HSC id)">NG Firm Phys, ID, IF, (US/MM), Katy-Oasis (HSC id)</option><option value="NG Firm Phys, ID, IF, (US/MM), Katy-Oasis (Katy id)">NG Firm Phys, ID, IF, (US/MM), Katy-Oasis (Katy id)</option><option value="NG Firm Phys, ID, IF, (US/MM), Leidy-Transco">NG Firm Phys, ID, IF, (US/MM), Leidy-Transco</option><option value="NG Firm Phys, ID, IF, (US/MM), Michcon">NG Firm Phys, ID, IF, (US/MM), Michcon</option><option value="NG Firm Phys, ID, IF, (US/MM), Millennium East Pool">NG Firm Phys, ID, IF, (US/MM), Millennium East Pool</option><option value="NG Firm Phys, ID, IF, (US/MM), NBPL-Ventura TP">NG Firm Phys, ID, IF, (US/MM), NBPL-Ventura TP</option><option value="NG Firm Phys, ID, IF, (US/MM), NGPL-Midcont Pool">NG Firm Phys, ID, IF, (US/MM), NGPL-Midcont Pool</option><option value="NG Firm Phys, ID, IF, (US/MM), NGPL-STX">NG Firm Phys, ID, IF, (US/MM), NGPL-STX</option><option value="NG Firm Phys, ID, IF, (US/MM), NGPL-TXOK East">NG Firm Phys, ID, IF, (US/MM), NGPL-TXOK East</option><option value="NG Firm Phys, ID, IF, (US/MM), NNG-Demarc">NG Firm Phys, ID, IF, (US/MM), NNG-Demarc</option><option value="NG Firm Phys, ID, IF, (US/MM), NNG-Ventura">NG Firm Phys, ID, IF, (US/MM), NNG-Ventura</option><option value="NG Firm Phys, ID, IF, (US/MM), OGT">NG Firm Phys, ID, IF, (US/MM), OGT</option><option value="NG Firm Phys, ID, IF, (US/MM), Oasis-Waha Pool">NG Firm Phys, ID, IF, (US/MM), Oasis-Waha Pool</option><option value="NG Firm Phys, ID, IF, (US/MM), Opal">NG Firm Phys, ID, IF, (US/MM), Opal</option><option value="NG Firm Phys, ID, IF, (US/MM), Panhandle">NG Firm Phys, ID, IF, (US/MM), Panhandle</option><option value="NG Firm Phys, ID, IF, (US/MM), Pine Prairie">NG Firm Phys, ID, IF, (US/MM), Pine Prairie</option><option value="NG Firm Phys, ID, IF, (US/MM), REX E-ANR">NG Firm Phys, ID, IF, (US/MM), REX E-ANR</option><option value="NG Firm Phys, ID, IF, (US/MM), Sonat-Z0 South">NG Firm Phys, ID, IF, (US/MM), Sonat-Z0 South</option><option value="NG Firm Phys, ID, IF, (US/MM), TCO">NG Firm Phys, ID, IF, (US/MM), TCO</option><option value="NG Firm Phys, ID, IF, (US/MM), TCO-A04 Pool">NG Firm Phys, ID, IF, (US/MM), TCO-A04 Pool</option><option value="NG Firm Phys, ID, IF, (US/MM), TETCO-ELA">NG Firm Phys, ID, IF, (US/MM), TETCO-ELA</option><option value="NG Firm Phys, ID, IF, (US/MM), TETCO-M1 30">NG Firm Phys, ID, IF, (US/MM), TETCO-M1 30</option><option value="NG Firm Phys, ID, IF, (US/MM), TETCO-M2 (receipts)">NG Firm Phys, ID, IF, (US/MM), TETCO-M2 (receipts)</option><option value="NG Firm Phys, ID, IF, (US/MM), TETCO-M3">NG Firm Phys, ID, IF, (US/MM), TETCO-M3</option><option value="NG Firm Phys, ID, IF, (US/MM), TETCO-STX">NG Firm Phys, ID, IF, (US/MM), TETCO-STX</option><option value="NG Firm Phys, ID, IF, (US/MM), TETCO-WLA">NG Firm Phys, ID, IF, (US/MM), TETCO-WLA</option><option value="NG Firm Phys, ID, IF, (US/MM), TGP-500L">NG Firm Phys, ID, IF, (US/MM), TGP-500L</option><option value="NG Firm Phys, ID, IF, (US/MM), TGP-800L">NG Firm Phys, ID, IF, (US/MM), TGP-800L</option><option value="NG Firm Phys, ID, IF, (US/MM), TGP-Z0 North">NG Firm Phys, ID, IF, (US/MM), TGP-Z0 North</option><option value="NG Firm Phys, ID, IF, (US/MM), TGP-Z0 South">NG Firm Phys, ID, IF, (US/MM), TGP-Z0 South</option><option value="NG Firm Phys, ID, IF, (US/MM), TGT-Mainline">NG Firm Phys, ID, IF, (US/MM), TGT-Mainline</option><option value="NG Firm Phys, ID, IF, (US/MM), TGT-North LA">NG Firm Phys, ID, IF, (US/MM), TGT-North LA</option><option value="NG Firm Phys, ID, IF, (US/MM), TGT-SL">NG Firm Phys, ID, IF, (US/MM), TGT-SL</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-30">NG Firm Phys, ID, IF, (US/MM), Transco-30</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-65">NG Firm Phys, ID, IF, (US/MM), Transco-65</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-85">NG Firm Phys, ID, IF, (US/MM), Transco-85</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-River Road">NG Firm Phys, ID, IF, (US/MM), Transco-River Road</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-Z5 South">NG Firm Phys, ID, IF, (US/MM), Transco-Z5 South</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-Z6 (non-NY north ML)">NG Firm Phys, ID, IF, (US/MM), Transco-Z6 (non-NY north ML)</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-Z6 NY">NG Firm Phys, ID, IF, (US/MM), Transco-Z6 NY</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-Z6 Sta-210">NG Firm Phys, ID, IF, (US/MM), Transco-Z6 Sta-210</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-Z6 non-NY">NG Firm Phys, ID, IF, (US/MM), Transco-Z6 non-NY</option><option value="NG Firm Phys, ID, IF, (US/MM), Transco-Z6 non-NY north">NG Firm Phys, ID, IF, (US/MM), Transco-Z6 non-NY north</option><option value="NG Firm Phys, ID, IF, (US/MM), Trunkline-ELA">NG Firm Phys, ID, IF, (US/MM), Trunkline-ELA</option><option value="NG Firm Phys, ID, IF, (US/MM), Trunkline-WLA">NG Firm Phys, ID, IF, (US/MM), Trunkline-WLA</option><option value="NG Firm Phys, ID, IF, (US/MM), Trunkline-Z1A">NG Firm Phys, ID, IF, (US/MM), Trunkline-Z1A</option><option value="NG Firm Phys, ID, IF, (US/MM), Waha">NG Firm Phys, ID, IF, (US/MM), Waha</option><option value="NG Firm Phys, ID, NGI, (US/MM), APC-ACE">NG Firm Phys, ID, NGI, (US/MM), APC-ACE</option><option value="NG Firm Phys, ID, NGI, (US/MM), GTN-Kingsgate">NG Firm Phys, ID, NGI, (US/MM), GTN-Kingsgate</option><option value="NG Firm Phys, ID, NGI, (US/MM), GTN-Malin">NG Firm Phys, ID, NGI, (US/MM), GTN-Malin</option><option value="NG Firm Phys, ID, NGI, (US/MM), GTN-Stanfield">NG Firm Phys, ID, NGI, (US/MM), GTN-Stanfield</option><option value="NG Firm Phys, ID, NGI, (US/MM), KRGT-Del Pool">NG Firm Phys, ID, NGI, (US/MM), KRGT-Del Pool</option><option value="NG Firm Phys, ID, NGI, (US/MM), NGPL-Nicor">NG Firm Phys, ID, NGI, (US/MM), NGPL-Nicor</option><option value="NG Firm Phys, ID, NGI, (US/MM), NGPL-Nipsco">NG Firm Phys, ID, NGI, (US/MM), NGPL-Nipsco</option><option value="NG Firm Phys, ID, NGI, (US/MM), PGE-Citygate">NG Firm Phys, ID, NGI, (US/MM), PGE-Citygate</option><option value="NG Firm Phys, ID, NGI, (US/MM), SoCal-Citygate">NG Firm Phys, ID, NGI, (US/MM), SoCal-Citygate</option><option value="NGX Fin BS, LD1 for 5A, (US/MM), AB-NIT">NGX Fin BS, LD1 for 5A, (US/MM), AB-NIT</option><option value="NGX Fin BS, LD1 for 7A, (US/MM), AB-NIT">NGX Fin BS, LD1 for 7A, (US/MM), AB-NIT</option><option value="NGX Fin FF, FP for 7A, (CA/GJ), AB-NIT" selected="selected">NGX Fin FF, FP for 7A, (CA/GJ), AB-NIT</option><option value="NGX Fin FF, FP for 7A, (US/MM), AB-NIT">NGX Fin FF, FP for 7A, (US/MM), AB-NIT</option><option value="NGX Fin FF, FP for LD1, (US/MM), Henry">NGX Fin FF, FP for LD1, (US/MM), Henry</option><option value="NGX Fin SS, FP for 5A, (CA/GJ), AB-NIT">NGX Fin SS, FP for 5A, (CA/GJ), AB-NIT</option><option value="NGX Fin SS, FP for 5A, (US/MM), AB-NIT">NGX Fin SS, FP for 5A, (US/MM), AB-NIT</option><option value="NGX Phys, BS, LD1 (US/MM), AB-NIT">NGX Phys, BS, LD1 (US/MM), AB-NIT</option><option value="NGX Phys, BS, LD1 (US/MM), Spectra - Hunt">NGX Phys, BS, LD1 (US/MM), Spectra - Hunt</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - Chippawa">NGX Phys, BS, LD1 (US/MM), TCPL - Chippawa</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - East Hereford">NGX Phys, BS, LD1 (US/MM), TCPL - East Hereford</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - Emerson 1">NGX Phys, BS, LD1 (US/MM), TCPL - Emerson 1</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - Emerson 2">NGX Phys, BS, LD1 (US/MM), TCPL - Emerson 2</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - Enbridge EDA">NGX Phys, BS, LD1 (US/MM), TCPL - Enbridge EDA</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - Energir EDA">NGX Phys, BS, LD1 (US/MM), TCPL - Energir EDA</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - Iroquois">NGX Phys, BS, LD1 (US/MM), TCPL - Iroquois</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - Niagara">NGX Phys, BS, LD1 (US/MM), TCPL - Niagara</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - North Bay Junction">NGX Phys, BS, LD1 (US/MM), TCPL - North Bay Junction</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL - St. Clair">NGX Phys, BS, LD1 (US/MM), TCPL - St. Clair</option><option value="NGX Phys, BS, LD1 (US/MM), TCPL-Enbridge CDA">NGX Phys, BS, LD1 (US/MM), TCPL-Enbridge CDA</option><option value="NGX Phys, BS, LD1 (US/MM), Union-Dawn">NGX Phys, BS, LD1 (US/MM), Union-Dawn</option><option value="NGX Phys, BS, LD1 (US/MM), Union-Parkway">NGX Phys, BS, LD1 (US/MM), Union-Parkway</option><option value="NGX Phys, BS, LD1 Spr (US/MM), AB-NIT/TCPL-Emerson1">NGX Phys, BS, LD1 Spr (US/MM), AB-NIT/TCPL-Emerson1</option><option value="NGX Phys, BS, LD1 Spr (US/MM), AB-NIT/TCPL-Emerson2">NGX Phys, BS, LD1 Spr (US/MM), AB-NIT/TCPL-Emerson2</option><option value="NGX Phys, BS, LD1 Spr (US/MM), AB-NIT/Union-Dawn">NGX Phys, BS, LD1 Spr (US/MM), AB-NIT/Union-Dawn</option><option value="NGX Phys, BS, LD1 Spr (US/MM), TCPL - North Bay Junction/Union-Dawn">NGX Phys, BS, LD1 Spr (US/MM), TCPL - North Bay Junction/Union-Dawn</option><option value="NGX Phys, BS, LD1 Spr (US/MM), TCPL-St. Clair/Union-Dawn">NGX Phys, BS, LD1 Spr (US/MM), TCPL-St. Clair/Union-Dawn</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL - Enbridge EDA">NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL - Enbridge EDA</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL Energir EDA">NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL Energir EDA</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Chippawa">NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Chippawa</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Enbridge CDA">NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Enbridge CDA</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Iroquois">NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Iroquois</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Niagara">NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/TCPL-Niagara</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/Union-Parkway">NGX Phys, BS, LD1 Spr (US/MM), Union-Dawn/Union-Parkway</option><option value="NGX Phys, BS, LD1 Spr (US/MM), Union-Parkway/TCPL-Enbridge CDA">NGX Phys, BS, LD1 Spr (US/MM), Union-Parkway/TCPL-Enbridge CDA</option><option value="NGX Phys, FP (CA/GJ), AB-NIT">NGX Phys, FP (CA/GJ), AB-NIT</option><option value="NGX Phys, FP (CA/GJ), APC-ATP">NGX Phys, FP (CA/GJ), APC-ATP</option><option value="NGX Phys, FP (CA/GJ), Spectra - Stn 2">NGX Phys, FP (CA/GJ), Spectra - Stn 2</option><option value="NGX Phys, FP (CA/GJ), TCPL - Empress">NGX Phys, FP (CA/GJ), TCPL - Empress</option><option value="NGX Phys, FP (CA/GJ), TCPL-Suffield 2">NGX Phys, FP (CA/GJ), TCPL-Suffield 2</option><option value="NGX Phys, FP (CA/GJ), TEP">NGX Phys, FP (CA/GJ), TEP</option><option value="NGX Phys, FP (US/MM), AB-NIT">NGX Phys, FP (US/MM), AB-NIT</option><option value="NGX Phys, FP (US/MM), APC-ATP">NGX Phys, FP (US/MM), APC-ATP</option><option value="NGX Phys, FP (US/MM), Spectra - Hunt">NGX Phys, FP (US/MM), Spectra - Hunt</option><option value="NGX Phys, FP (US/MM), Spectra - Stn 2">NGX Phys, FP (US/MM), Spectra - Stn 2</option><option value="NGX Phys, FP (US/MM), TCPL - Chippawa">NGX Phys, FP (US/MM), TCPL - Chippawa</option><option value="NGX Phys, FP (US/MM), TCPL - East Hereford">NGX Phys, FP (US/MM), TCPL - East Hereford</option><option value="NGX Phys, FP (US/MM), TCPL - Emerson 1">NGX Phys, FP (US/MM), TCPL - Emerson 1</option><option value="NGX Phys, FP (US/MM), TCPL - Emerson 2">NGX Phys, FP (US/MM), TCPL - Emerson 2</option><option value="NGX Phys, FP (US/MM), TCPL - Enbridge EDA">NGX Phys, FP (US/MM), TCPL - Enbridge EDA</option><option value="NGX Phys, FP (US/MM), TCPL - Energir EDA">NGX Phys, FP (US/MM), TCPL - Energir EDA</option><option value="NGX Phys, FP (US/MM), TCPL - Iroquois">NGX Phys, FP (US/MM), TCPL - Iroquois</option><option value="NGX Phys, FP (US/MM), TCPL - Niagara">NGX Phys, FP (US/MM), TCPL - Niagara</option><option value="NGX Phys, FP (US/MM), TCPL - North Bay Junction">NGX Phys, FP (US/MM), TCPL - North Bay Junction</option><option value="NGX Phys, FP (US/MM), TCPL - St. Clair">NGX Phys, FP (US/MM), TCPL - St. Clair</option><option value="NGX Phys, FP (US/MM), TCPL-Enbridge CDA">NGX Phys, FP (US/MM), TCPL-Enbridge CDA</option><option value="NGX Phys, FP (US/MM), Union-Dawn">NGX Phys, FP (US/MM), Union-Dawn</option><option value="NGX Phys, FP (US/MM), Union-Parkway">NGX Phys, FP (US/MM), Union-Parkway</option><option value="NGX Phys, FP Spr (CA/GJ), AB-NIT/APC-ATP">NGX Phys, FP Spr (CA/GJ), AB-NIT/APC-ATP</option><option value="NGX Phys, FP Spr (CA/GJ), AB-NIT/Spectra-Stn2">NGX Phys, FP Spr (CA/GJ), AB-NIT/Spectra-Stn2</option><option value="NGX Phys, FP Spr (CA/GJ), AB-NIT/TCPL-Empress">NGX Phys, FP Spr (CA/GJ), AB-NIT/TCPL-Empress</option><option value="NGX Phys, FP Spr (CA/GJ), AB-NIT/TCPL-Suffield 2">NGX Phys, FP Spr (CA/GJ), AB-NIT/TCPL-Suffield 2</option><option value="NGX Phys, FP Spr (US/MM), AB-NIT/Union-Dawn">NGX Phys, FP Spr (US/MM), AB-NIT/Union-Dawn</option><option value="NGX Phys, FP Spr (US/MM), Spectra-Stn2/Spectra-Hunt">NGX Phys, FP Spr (US/MM), Spectra-Stn2/Spectra-Hunt</option><option value="NGX Phys, FP Spr (US/MM), TCPL - North Bay Junction/Union Dawn">NGX Phys, FP Spr (US/MM), TCPL - North Bay Junction/Union Dawn</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL - Enbridge EDA">NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL - Enbridge EDA</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL Energir EDA">NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL Energir EDA</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Chippawa">NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Chippawa</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Enbridge CDA">NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Enbridge CDA</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Iroquois">NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Iroquois</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Niagara">NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-Niagara</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-St. Clair">NGX Phys, FP Spr (US/MM), Union-Dawn/TCPL-St. Clair</option><option value="NGX Phys, FP Spr (US/MM), Union-Dawn/Union-Parkway">NGX Phys, FP Spr (US/MM), Union-Dawn/Union-Parkway</option><option value="NGX Phys, FP Spr (US/MM), Union-Parkway/TCPL-Enbridge CDA">NGX Phys, FP Spr (US/MM), Union-Parkway/TCPL-Enbridge CDA</option><option value="NGX Phys, ID, 2a (CA/GJ), AB-NIT">NGX Phys, ID, 2a (CA/GJ), AB-NIT</option><option value="NGX Phys, ID, 2a (CA/GJ), APC-ATP">NGX Phys, ID, 2a (CA/GJ), APC-ATP</option><option value="NGX Phys, ID, 2a (US/MM), AB-NIT">NGX Phys, ID, 2a (US/MM), AB-NIT</option><option value="NGX Phys, ID, 4a (CA/GJ), AB-NIT">NGX Phys, ID, 4a (CA/GJ), AB-NIT</option><option value="NGX Phys, ID, 5a (CA/GJ), AB-NIT">NGX Phys, ID, 5a (CA/GJ), AB-NIT</option><option value="NGX Phys, ID, 5a (CA/GJ), APC-ATP">NGX Phys, ID, 5a (CA/GJ), APC-ATP</option><option value="NGX Phys, ID, 5a (CA/GJ), TEP">NGX Phys, ID, 5a (CA/GJ), TEP</option><option value="NGX Phys, ID, 5a (US/MM), AB-NIT">NGX Phys, ID, 5a (US/MM), AB-NIT</option><option value="NGX Phys, ID, 5a (US/MM), APC-ATP">NGX Phys, ID, 5a (US/MM), APC-ATP</option><option value="NGX Phys, ID, 7a (CA/GJ), AB-NIT">NGX Phys, ID, 7a (CA/GJ), AB-NIT</option><option value="NGX Phys, ID, 7a (CA/GJ), APC-ATP">NGX Phys, ID, 7a (CA/GJ), APC-ATP</option><option value="NGX Phys, ID, 7a (CA/GJ), Spectra - Stn 2">NGX Phys, ID, 7a (CA/GJ), Spectra - Stn 2</option><option value="NGX Phys, ID, 7a (US/MM), AB-NIT">NGX Phys, ID, 7a (US/MM), AB-NIT</option><option value="NGX Phys, ID, 7a (US/MM), APC-ATP">NGX Phys, ID, 7a (US/MM), APC-ATP</option><option value="NGX Phys, ID, ATP Day Ahead (CA/GJ), APC-ATP">NGX Phys, ID, ATP Day Ahead (CA/GJ), APC-ATP</option><option value="NGX Phys, ID, ATP Same Day (CA/GJ), APC-ATP">NGX Phys, ID, ATP Same Day (CA/GJ), APC-ATP</option><option value="NGX Phys, ID, Day Ahead (CA/GJ), AB-NIT">NGX Phys, ID, Day Ahead (CA/GJ), AB-NIT</option><option value="NGX Phys, ID, Day Ahead (CA/GJ), APC-ATP">NGX Phys, ID, Day Ahead (CA/GJ), APC-ATP</option><option value="NGX Phys, ID, Day Ahead (CA/GJ), Spectra - Stn 2">NGX Phys, ID, Day Ahead (CA/GJ), Spectra - Stn 2</option><option value="NGX Phys, ID, Day Ahead (US/MM), Emerson 1">NGX Phys, ID, Day Ahead (US/MM), Emerson 1</option><option value="NGX Phys, ID, Day Ahead (US/MM), Emerson 2">NGX Phys, ID, Day Ahead (US/MM), Emerson 2</option><option value="NGX Phys, ID, Day Ahead (US/MM), Union-Dawn">NGX Phys, ID, Day Ahead (US/MM), Union-Dawn</option><option value="NGX Phys, ID, GD (US/MM), Spectra - Hunt">NGX Phys, ID, GD (US/MM), Spectra - Hunt</option><option value="NGX Phys, ID, IF (US/MM), Spectra - Hunt">NGX Phys, ID, IF (US/MM), Spectra - Hunt

html_doc<-read_html(POST("https://www.theice.com/marketdata/reports/ngx/DailySettlementPrices.shtml", body = query))

html_nodes(html_doc, "table")[[1]]%>%html_table(fill = TRUE) %>% clean_names()%>%
  mutate(begin_date=ymd(begin_date),end_date=ymd(end_date),net_oi=as.numeric(gsub(",","",net_oi)),
         settlement=settlement,date_read=Sys.Date())
}

ngx_data<-ngx_read("Natural Gas","NGX Fin FF, FP for 7A, (CA/GJ), AB-NIT","Current Settlement")%>%
  mutate(year=year(begin_date)) %>% group_by(market,begin_date,settlement,year)%>%
  summarize(settle=mean(settle),settle_date=min(begin_date))%>% write_csv("ngx_annual.csv")



ngx_us<-ngx_read("Natural Gas","NGX Fin FF, FP for 7A, (US/MM), AB-NIT","Current Settlement")

ngx_phys<-ngx_read("Natural Gas","NGX Phys, FP (CA/GJ), AB-NIT","Previous Day Settlement")


#monthly_contracts
ngx_annual<-ngx_phys %>% mutate(year=year(begin_date)) %>% filter(day(begin_date)==1,day(end_date)==days_in_month(month(end_date)))%>% group_by(market,year)%>%
  summarize(settle=mean(settle))%>% write_csv("ngx_annual.csv")




ngx_annual<-ngx_phys%>%
  mutate(year=year(begin_date),contract_days=end_date-begin_date) %>%
  #filter(contract_days>1,year(begin_date<2030)) %>% #take out the dailies trim 2030s because no open interst and a big jump in price
  group_by(market,date,settlement,year)%>%
  summarize(settle=mean(settle),settle_date=min(begin_date))%>% write_csv("ngx_annual.csv")

ngx_read("Natural Gas","NGX Fin FF, FP for 7A, (CA/GJ), AB-NIT","Current Settlement")%>%
  mutate(year=year(begin_date)) %>% group_by(market,date,settlement,year)%>%
  summarize(settle=mean(settle))



ngx_aeso<-ngx_read("Power","NGX Fin FF, FP for AESO Flat, (CA/MWh), Alberta","Previous Day Settlement")
aeso_annual<-ngx_aeso %>% mutate(year=year(begin_date)) %>% filter(day(begin_date)==1,day(end_date)==days_in_month(month(end_date)))%>% group_by(market,year)%>%
  summarize(settle=mean(settle))%>% write_csv("aeso_annual.csv")


#NGX Fin FF, FP for AESO Flat, (CA/MWh), Alberta






#CAD

download.file("ftp://ftp.cmegroup.com/pub/settle/stlcur","cad.txt",mode="wb")
cad_data <- read_delim("cad.txt",delim = " ") %>% as.data.frame()
#find where the Canadian data starts
cad_start<-grep("CANADIAN",cad_data[,2])
cad_end<-grep("TOTAL",cad_data[,1])
cad_end<-cad_end[cad_end>cad_start][1]
cad_data<-cad_data[(cad_start+1):(cad_end-1),c(1,6)]
cad_data$trade_date<-mdy(names(cad_data)[2])
names(cad_data)<-c("inst_date","settle","trade_date")
cad_data$settle<-as.numeric(cad_data$settle)
cad_data<- cad_data %>% mutate(month_abb=substr(inst_date,1,3),month_abb=gsub("JLY","JUL",month_abb),
                               inst=ymd(paste(20,substr(inst_date,4,5),"-",month_abb,"-01",sep="")),
                               month=month(inst),
                               year=year(inst))
cad_data_annual<-cad_data %>% group_by(year) %>% summarize(settle=mean(settle))




cad_data_annual %>% write_csv("cad_annual.csv")





#ST53 data 

fix_names<-function(data_sent){
  data_sent<-data_sent %>% mutate(`Scheme Name`= case_when(
    #Christina Lake
    grepl("MEG", Operator) & grepl("Christina", `Scheme Name`)  ~ "Christina Lake (MEG)",
    grepl("Meg", Operator) & grepl("Christina", `Scheme Name`)  ~ "Christina Lake (MEG)",
    grepl("Cenovus", Operator) & grepl("Christina", `Scheme Name`)  ~ "Christina Lake (FCCL)",
    #Mackay River (Suncor or Brion)
    grepl("Suncor", Operator) & grepl("Mackay", `Scheme Name`)  ~ "MacKay River (Suncor)",
    grepl("Suncor", Operator) & grepl("Mckay", `Scheme Name`)  ~ "MacKay River (Suncor)",
    grepl("Suncor", Operator) & grepl("ay", `Scheme Name`)  ~ "MacKay River (Suncor)",
    grepl("Brion", Operator) & grepl("ay", `Scheme Name`)  ~ "MacKay River (Petrochina)",
    grepl("Petrochina", Operator) & grepl("ay", `Scheme Name`)  ~ "MacKay River (Petrochina)",
    grepl("PetroChina", Operator) & grepl("ay", `Scheme Name`)  ~ "MacKay River (Petrochina)",
    grepl("Peace River", `Scheme Name`)  ~ "Peace River",
    #hangingstone 
    grepl("Japan", Operator) & grepl("Hang", `Scheme Name`)  ~ "Hangingstone (JACOS)",
    grepl("Athabasca", Operator) & grepl("Hang", `Scheme Name`)  ~ "Hangingstone (ATH)",
    grepl("Jackfish", `Scheme Name`)  ~ "Jackfish",
    grepl("Lindbergh", `Scheme Name`)  ~ "Lindbergh",
    grepl("Kirby South", `Scheme Name`)  ~ "Kirby",
    grepl("Tucker Lake", `Scheme Name`)  ~ "Tucker",
    TRUE ~ `Scheme Name`
  )
  )
  
}


st_53_online<-function(download=F){
  #every year is xls except for 2015 which is an xlsx file
  #each file contains a worksheet for bitumen, produced water, and steam usage.
  years<-seq(2010,2020)
  data_store <- list()
  for(year in years){
    #year<-2015
    #download=T
    #https://www.aer.ca/documents/sts/ST53/ST53_2009-12.xls
    #https://www.aer.ca/documents/sts/ST53/ST53_2019-12.xls
    address<-paste("https://www.aer.ca/documents/sts/ST53/ST53_",year,"-12.xls",sep="")
    filename<-paste("st53_",year,".xls",sep="")
    if(year==2015){
      filename<-paste("st53_",year,".xlsx",sep="")
      address<-paste("https://www.aer.ca/documents/sts/ST53/ST53_",year,"-12.xlsx",sep="")
    }
    if(year==2020){
      filename<-paste("st53_",year,".xls",sep="")
      address<-paste("https://www.aer.ca/documents/sts/ST53/ST53_Current.xls",sep="")
    }
    if(download==T)
      download.file(address,filename,mode = "wb")
    #read the bitumen production data. Units are Crude Bitumen Production Rates Per Calendar Day (m³)
    bitumen_data <- read_excel(filename, sheet = "BITUMEN", skip = 3)
    bitumen_data<-bitumen_data[!is.na(bitumen_data$`Approval Number`),]
    bitumen_data$Operator<-gsub(" \\(¹\\)","",bitumen_data$Operator)
    #take out notes denoted by (*)
    
    IDs<-names(bitumen_data)[1:5]
    bitumen_data$`Monthly Average`<-NULL
    bitumen_data<-melt(bitumen_data,id=IDs,measure.vars = month.abb,variable.name = "Month",value.name = "Bitumen")
    bitumen_data$Month<-match(as.character(bitumen_data$Month),month.abb)
    bitumen_data <-bitumen_data %>% filter(!is.na(Bitumen))
    #need to trim the totals from the files here
    
    #steam data - units are Steam Injection Rates Per Calendar Day (m³)											
    steam_data <- read_excel(filename, sheet = "STEAM", skip = 3)
    steam_data<-steam_data[!is.na(steam_data$`Approval Number`),]
    steam_data$Operator<-gsub(" \\(¹\\)","",steam_data$Operator) #take out notes denoted by       (*)
    IDs<-names(steam_data)[1:5]
    steam_data$`Monthly Average`<-NULL
    steam_data<-melt(steam_data,id=IDs,measure.vars = month.abb, variable.name = "Month",value.name = "Steam")
    steam_data <-steam_data %>% filter(!is.na(Steam))
    steam_data$Month<-match(as.character(steam_data$Month),month.abb)
    
    #water data: Water Production Rates Per Calendar Day (m³)											
    
    water_data <- read_excel(filename, sheet = "WATER", skip = 3)
    water_data<-water_data[!is.na(water_data$`Approval Number`),]
    water_data$Operator<-gsub(" \\(¹\\)","",water_data$Operator) #take out notes denoted by (*)
    IDs<-names(water_data)[1:5]
    water_data$`Monthly Average`<-NULL
    water_data<-melt(water_data,id=IDs,measure.vars = month.abb, variable.name = "Month",value.name = "water")
    water_data <-water_data %>% filter(!is.na(water))
    water_data$Month<-match(as.character(water_data$Month),month.abb)
    
    #merge them all  
    bitumen_data<-bitumen_data %>% left_join(steam_data %>% select(Month,`Approval Number`,Steam))%>%
      left_join(water_data %>% select(Month,`Approval Number`,water))
    #merge(bitumen_data,steam_data,by=c("Recovery Method","Area","Operator","Approval Number","Month","Date"))
    #create dates
    bitumen_data$Date<-ymd(paste(year,"-",bitumen_data$Month,"-",days_in_month(bitumen_data$Month),sep = ""))
    
    bitumen_data$year<-year
    bitumen_data$Bitumen<-as.numeric(bitumen_data$Bitumen)
    #here, we need to combine within schemes since there are a couple of Schemes with two separate approvals
    bitumen_data<-bitumen_data %>% group_by(Operator,`Scheme Name`,Area,`Recovery Method`,Month,Date,year) %>%
      summarize(Bitumen=sum(Bitumen,na.rm = T),Steam=sum(Steam,na.rm = T),water=sum(water,na.rm = T)) %>% ungroup()
    
    bitumen_data$SOR<-bitumen_data$Steam/bitumen_data$Bitumen #do SOR later
    data_store[[year]]<-bitumen_data
  }  
  ST53_data<-do.call("rbind", data_store)
  ST53_data<-fix_names(ST53_data)
  ST53_data  
}

#run the code
ST53_data  <-st_53_online(download = T)
ST53_data<-ST53_data %>% clean_names()


df_st53<- ST53_data %>% filter(recovery_method=="Commercial-SAGD") %>% 
  filter(scheme_name %in% c("BlackGold","Hangingstone (ATH)","Kirby","Sunrise" ))%>%
  mutate(capacity= case_when(
    grepl("BlackGold", scheme_name)  ~ 10000,
    grepl("Hanging", scheme_name)  ~ 12000,
    grepl("Kirby",scheme_name)  ~ 40000,
    grepl("Sunrise",scheme_name)  ~ 60000,
    TRUE ~ 10000 ))%>%
    #filter(steam>0)%>% #if they're not steaming, take them out
  arrange(scheme_name,date)%>% group_by(scheme_name)%>%
  mutate(first_obs=min(date))%>% filter(first_obs>ymd("2012-01-01"))%>% #take out projects already in the data pre-2012
  mutate(max_prod=max(bitumen),cum_prod=cumsum(bitumen),cum_steam=cumsum(steam),csor=cum_steam/cum_prod) %>% 
  filter(max_prod>5000/6.2898,cum_steam>0) %>%
  mutate(ma3=rollapply(bitumen,3,mean,align='right',fill=NA))%>%
  mutate(prod_month=cumsum((cum_prod>1)*1))%>%
  mutate(prod_year=trunc(prod_month/12,0)+1)%>%
  mutate(cap_fac=bitumen*6.2898/capacity)%>%
  ungroup()
  
annuals<-df_st53%>%
group_by(scheme_name,prod_year)%>% filter(prod_year<=4)%>%
  summarize(capacity=mean(capacity),bitumen=sum(bitumen*days_in_month(date)*6.2898)/365)%>%
  mutate(cap_fac=bitumen/capacity)




sor_ramps<-ggplot(df_st53) +
  geom_line(aes(prod_month,pmin(csor,12),linetype="Cumulative SOR")) +
  geom_line(aes(prod_month,pmin(sor,12),linetype="Monthly SOR")) +
  facet_wrap(~scheme_name,nrow = 1)+
  scale_linetype_manual("",values=c(1,2))+
  #scale_fill_viridis("Recovery Method:",discrete=TRUE)+   
  scale_x_continuous(breaks = c(0,12,24,36,48),limits=c(-1,50),expand=c(0,0)) +
  #scale_y_continuous(limits=c(0,6),breaks=seq(0,6,1))+
  scale_y_continuous(breaks=pretty_breaks(),limits=c(0,12.01))+
  ajl_line()+work_theme()+
  theme(strip.background = element_blank(),
        strip.text = element_text(colour="black", size = 14, face = "bold"),)+
  guides(col = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1))+
  labs(y="Steam:bitumen ratio*",x="Steaming Month",
       title=paste("Alberta oil sands steam:oil ratios by project",sep=""),
       subtitle=paste("Select SAGD projects with more than 5000 bbl/d production and start-up dates after 2012.",sep=""),
       caption="* Steam:bitumen ratio truncated at 12.")
sor_ramps


bit_ramps<-ggplot(df_st53) +
  geom_line(aes(prod_month,ma3*6.2898/capacity,linetype="3-month moving average production")) +
  geom_line(aes(prod_month,bitumen*6.2898/capacity,linetype="Monthly production")) +
  facet_wrap(~scheme_name,nrow = 1)+
  scale_linetype_manual("",values=c(1,2))+
  #scale_fill_viridis("Recovery Method:",discrete=TRUE)+   
  scale_x_continuous(breaks = c(0,12,24,36,48),limits=c(-1,50),expand=c(0,0)) +
  #scale_y_continuous(limits=c(0,6),breaks=seq(0,6,1))+
  scale_y_continuous(breaks=pretty_breaks(),labels=percent,limits = c(0,1.05))+
  ajl_line()+work_theme()+
  theme(strip.background = element_blank(),
        strip.text = element_text(colour="black", size = 14, face = "bold"),)+
  guides(col = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 1))+
  labs(y="Bitumen Production Capacity Factor (%)",x="Steaming Month",
       title=paste("Alberta oil sands bitumen production capacity factor by project",sep=""),
       subtitle=paste("Select SAGD projects with more than 5000 bbl/d production and start-up dates after 2012.",sep=""),
       caption="Source: AER/ERCB ST53 data, graph by Andrew Leach.")

(sor_ramps/bit_ramps)



p_grid<-plot_grid(
  sor_ramps + theme(#legend.position="none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    panel.spacing = unit(4, "lines"),
    #legend.text = element_text(colour="black", size = 14, face = "bold"),
    #plot.caption = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    plot.caption = element_blank(),
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
    )+
    NULL,
  bit_ramps +
    theme(#legend.position="none",
      panel.spacing = unit(4, "lines"),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
      strip.text =  element_blank(),
    )+
  NULL,
  align = TRUE,axis="b", ncol = 1, rel_heights = c(1,1)
)
p_grid
ggsave("bit_steam_ramps.png",dpi = 600,width=15,height = 12)



#AER price forecasts

aer_fcasts<-read_excel("ST98-Prices-and-Capital-Expenditure-Data.xlsx", sheet = "WTI Forecasts",skip=2)
names(aer_fcasts)<-c("year",as.vector(outer(c("low","base","high"),seq(2020,2013,-1), paste, sep=".")))

aer_fcasts<-aer_fcasts %>% pivot_longer(cols=-year)%>% separate(name,c("case","version")) %>% filter(!is.na(value))

#aer_fcasts<-aer_fcasts %>% mutate(v_code=paste(version,case,"case"))

aer_fcasts<-aer_fcasts %>% pivot_wider(-c(case),names_from=case,values_from=value) %>%
  mutate(label=paste(version,"AER ST-98 base case and high-low range"))

wti_annual<-wti_hist %>% mutate(year=year(date)) %>% group_by(year) %>% summarize(value=mean(value,na.rm = T))


graph_palette<-colors_tableau10()

int_years<-c(2013,2017,2018,2020)


ggplot(filter(aer_fcasts,year>=as.numeric(version),as.numeric(version)%in% int_years))+
  geom_line(aes(year,base,group=version,color=label),size=1.25)+
  geom_ribbon(aes(year,ymin=low,ymax=high,group=version,color=label,fill=label),alpha=.2,linetype="blank",size=.25)+
  
  geom_line(data=nymex_annual,aes(year,value,linetype="NYMEX WTI Futures settlement, April 1, 2021"),size=1.25,color="black")+
  geom_line(data=wti_annual,aes(year,value,linetype="EIA Historical Prices"),size=1.25,color="black")+
  
  #geom_ribbon(data=filter(cer_2020,year(date)<=2030),aes(year(date),ymax=cer_reference,ymin=cer_evolving,fill=labels,color=labels),alpha=.2,linetype="blank",size=.25)+
  #geom_errorbar(data=filter(cer_2020,year(date)%%1==0),aes(date,ymax=cer_reference,ymin=cer_evolving,fill=labels,color=labels),linetype="solid",width=200)+
  #geom_point(data=nymex_test,aes(Date,value,group=version,color=version,fill=version,shape=version),size=2)+
  
  
  scale_fill_manual("",values = graph_palette)+
  scale_color_manual("",values = graph_palette)+
  
  scale_linetype_manual("",values=c("solid","21"))+
  #scale_shape_manual("",values=c(15,16,15,16,15,16,32,32))+
  scale_y_continuous(breaks=pretty_breaks())+
  expand_limits(y=0)+
  scale_x_continuous(breaks = pretty_breaks())+
  ajl_line()+
  work_theme()+
  theme(plot.subtitle = element_text(size = 10))+
  guides(col = guide_legend(#override.aes = list(linetype = c(0,1,1)),
    keywidth = unit(2.1,"cm"),nrow = 4,reverse = F),
         fill = guide_legend(keywidth = unit(2.1,"cm"),nrow = 4,reverse = F),
         linetype = guide_legend(keywidth = unit(2.1,"cm"),nrow = 2)
         
         #shape = guide_legend(keywidth = unit(2.4,"cm"),nrow = 2)
  )+
  labs(y="WTI Price ($/bbl)",x="",
       #title=paste("Alberta Energy Regulator ST-98 WTI Forecasts"),
       #subtitle=paste("Historic and forward market settlement prices and AER annual forecasts of West Texas Intermediate crude oil price"),
       #caption="Data via CME Group, Alberta Energy Regulator (AER) ST-98 archive, and the US Energy Information Administration (EIA). Graph by Andrew Leach."
       NULL
       )
ggsave("aer_wti_ribbon_nymex.png",dpi=400,width = 16)



aer_wcs<-read_excel("ST98-Prices-and-Capital-Expenditure-Data.xlsx", sheet = "WCS Forecasts",skip=2)
names(aer_wcs)<-c("year",as.vector(outer(c("low","base","high"),seq(2020,2016,-1), paste, sep=".")))



aer_wcs<-aer_wcs %>% pivot_longer(cols=-year)%>% separate(name,c("case","version")) %>% filter(!is.na(value))

#aer_wcs<-aer_wcs %>% mutate(v_code=paste(version,case,"case"))

aer_wcs<-aer_wcs %>% pivot_wider(-c(case),names_from=case,values_from=value) %>%
  mutate(label=paste(version,"AER ST-98 base case and high-low range"))

graph_palette<-colors_tableau10()

int_years<-c(2016,2017,2018,2019,2020)
ggplot(filter(aer_wcs,year>=as.numeric(version),as.numeric(version)%in% int_years))+
  geom_line(aes(year,base,group=version,color=label),size=1.25)+
  geom_ribbon(aes(year,ymin=low,ymax=high,group=version,color=label,fill=label),alpha=.2,linetype="blank",size=.25)+
  
  #geom_line(data=nymex_annual,aes(year,value,linetype="NYMEX WTI Futures settlement, Dec 28, 2020"),size=1.25,color="black")+
  #geom_line(data=wti_annual,aes(year,value,linetype="Historical Prices"),size=1.25,color="black")+
  
  #geom_ribbon(data=filter(cer_2020,year(date)<=2030),aes(year(date),ymax=cer_reference,ymin=cer_evolving,fill=labels,color=labels),alpha=.2,linetype="blank",size=.25)+
  #geom_errorbar(data=filter(cer_2020,year(date)%%1==0),aes(date,ymax=cer_reference,ymin=cer_evolving,fill=labels,color=labels),linetype="solid",width=200)+
  #geom_point(data=nymex_test,aes(Date,value,group=version,color=version,fill=version,shape=version),size=2)+
  
  
  scale_fill_manual("",values = graph_palette)+
  scale_color_manual("",values = graph_palette)+
  
  scale_linetype_manual("",values=c("solid","21"))+
  #scale_shape_manual("",values=c(15,16,15,16,15,16,32,32))+
  scale_y_continuous(breaks=pretty_breaks())+
  expand_limits(y=0)+
  scale_x_continuous(breaks = pretty_breaks())+
  weekly_small()+
  theme(plot.subtitle = element_text(size = 10))+
  guides(col = guide_legend(#override.aes = list(linetype = c(0,1,1)),
    keywidth = unit(2.1,"cm"),nrow = 4,reverse = F),
    fill = guide_legend(keywidth = unit(2.1,"cm"),nrow = 4,reverse = F),
    linetype = guide_legend(keywidth = unit(2.1,"cm"),nrow = 1)
    
    #shape = guide_legend(keywidth = unit(2.4,"cm"),nrow = 2)
  )+
  labs(y="WTI Price ($/bbl)",x="",
       title=paste("Alberta Energy Regulator ST-98 WTI Forecasts"),
       subtitle=paste("Historic and forward market settlement prices and AER annual forecasts of West Texas Intermediate crude oil price"),
       caption="Data via Alberta Energy Regulator (AER) ST-98 archive, and the US Energy Information Administration (EIA). Graph by Andrew Leach.")
ggsave("aer_wcs_ribbon.png",dpi=300,width = 16)




#oil and gas investment



new_inv<-read_excel("budget_oil_inv.xlsx",sheet="C13 Oil Inv & Prod",range="A3:J17")%>% select(1,8,9) %>% clean_names() %>%
  rename(conventional=conventional_oil_and_gas_investment_m_8,oil_sands=oil_sands_investment_m_9)%>% filter(year>2010)%>%
  mutate(year=gsub("e","",year),year=gsub("f","",year),year=as.numeric(year),total=conventional+oil_sands)%>% pivot_longer(-year)

budget_inv<-read_excel("budget_oil_inv.xlsx",sheet="C13 Oil Inv & Prod",range="A3:J17")%>% select(1,5,6) %>% clean_names() %>%
  rename(conventional=conventional_oil_and_gas_investment_m_5,oil_sands=oil_sands_investment_m_6)%>% filter(year>2010)%>%
  mutate(year=gsub("e","",year),year=gsub("f","",year),year=as.numeric(year),total=conventional+oil_sands)%>% pivot_longer(-year)


old_inv<-read_excel("budget_oil_inv.xlsx",sheet="open_data_table") %>% select(seq(1,4))%>% clean_names() %>% 
  mutate(investment=investment_billions*1000)%>% select(-investment_billions) %>% pivot_wider(names_from = industry,values_from=investment) %>% clean_names()%>%
  rename(conventional=conventional_oil_and_gas) %>% select(-geography)%>% mutate(total=conventional+oil_sands)%>%pivot_longer(-year)



oil_inv<-old_inv %>% bind_rows(new_inv) %>% filter(year<=2020)

ggplot(oil_inv %>% filter(name=="total"))+
  geom_line(aes(year,value/1000,group=name,color="A",linetype="A"),size=2)+
  geom_line(data=new_inv %>% filter(name=="total",year>=2019),aes(year,value/1000,group=name,color="C",linetype="C"),size=2)+
  geom_line(data=budget_inv %>% filter(name=="total",year>=2019),aes(year,value/1000,group=name,color="B",linetype="B"),size=2)+
  scale_linetype_manual("",values=c("solid","21","21"))+
  #geom_point(aes(2021,27.3),size=2,colour="red")+
  scale_y_continuous(breaks=pretty_breaks())+
  expand_limits(y=0)+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_color_manual("",values=colors_tableau10(),labels=c("Historic values","Budget 2020 Estimates","Q2 Fiscal Update Revisions"))+
  work_theme()+
  theme(plot.subtitle = element_text(size = 13))+
  guides(col = guide_legend(override.aes = list(linetype=c("solid","21","21")),
    keywidth = unit(2.1,"cm"),nrow = 1,reverse = F),
    linetype =FALSE  )+
  labs(y="Annual Capital Investment ($ billions)",x="",
       title=paste("Alberta Oil and Gas Capital Invesment and Forecasts"),
       subtitle=paste("Historic values, Budget 2020 projections and November, 2020 Q2 Fiscal Update revised estimates"),
       caption="Data via Alberta Government Budget Documents. Graph by Andrew Leach.")+
  NULL
ggsave("budget_capex.png",width=16, height=9,dpi=600)

