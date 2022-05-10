


#annual energy outlooks
#get 2014 (when they bought the project), 2017 (comparability to hearings analysis), and now
series<-paste("AEO.",c(2014,2017,2020,2021,2022),".REF",c(2014,2017,2020,2021,2022),".PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep="")
series<-c(series,paste("AEO.",c(2014,2017,2020,2021,2022),".LOWPRICE.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep=""))
series<-c(series,paste("AEO.",c(2014,2017,2020,2021,2022),".HIGHPRICE.PRCE_NOMP_TEN_NA_WTI_NA_USA_NDLRPBRL.A",sep=""))
wti_data<-pdfetch_EIA(series,KEY) 
names<-c(paste("AEO ",c(2014,2017,2020,2021,2022)," Reference Case",sep=""),paste("AEO ",c(2014,2017,2020,2021,2022)," Low Price Case",sep=""),paste("AEO ",c(2014,2017,2020,2021,2022)," High Price Case",sep=""))
wti_data <- setNames(wti_data, names)
wti_data<-data.frame(date=index(wti_data), coredata(wti_data))
wti_data$date<-ymd(wti_data$date)
write_csv(wti_data,"aeo_runs.csv")
wti_melt <- wti_data %>% pivot_longer(cols=-c("date"),names_to = "version")
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


wti_wide<-wti_melt %>%filter(case!="NYMEX Historic WTI Price Data")%>%pivot_wider(names_from=case,values_from=value)%>%
  rename(low=`Low Price`,high=`High Price`)%>% mutate(labels=paste("EIA Reference case and high and low price scenario range,","Annual Energy Oulook",year))
#%>%
#  left_join(wti_hist %>% select(date,wti_history=value))


#get CER 2020 prices

cer_2020<-read_csv("https://www.cer-rec.gc.ca/open/energy/energyfutures2020/benchmark-prices-2020.csv")%>%clean_names()%>%filter(variable=="West Texas Intermediate (WTI) - US$/bbl",year>=2020)%>%
  mutate(date=ymd(paste(year,"-12-31")),
         value=value*1.02^(year-2020))%>% select(date,value,scenario)%>%
  pivot_wider(-scenario,values_from=value,names_from=scenario)%>%
  select(date,cer_reference=Reference,cer_evolving=Evolving)%>%
  mutate(labels="Reference and Evolving scenario range, CER Canada's Energy Future 2020")



cer_fx<-read_csv("https://www.cer-rec.gc.ca/open/energy/energyfutures2020/macro-indicators-2020.csv")%>%clean_names()%>%filter(region=="Canada",variable %in% c("Consumer Price Index (2002=100)","Canada-US Exchange Rate (C$/US$)"))%>%
  mutate(date=ymd(paste(year,"-12-31")))

cer_fx<-read_csv("https://www.cer-rec.gc.ca/open/energy/energyfutures2020/macro-indicators-2020.csv")%>%clean_names()%>%filter(region=="Canada",variable %in% c("Consumer Price Index (2002=100)","Canada-US Exchange Rate (C$/US$)"))%>%
  mutate(date=ymd(paste(year,"-12-31")))

v_int<-c("West Texas Intermediate (WTI) - US$/bbl", "Western Canadian Select (WCS) - US$/bbl","Nova Inventory Transfer (NIT) - US$/MMBtu", "Canada-US Exchange Rate (C$/US$)","Consumer Price Index (2002=100)")


cer_data<-read_csv("https://www.cer-rec.gc.ca/open/energy/energyfutures2020/benchmark-prices-2020.csv")%>%clean_names()%>%
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

graph_palette<-viridis(7,option="A",direction = -1)[2:5]

ggplot(filter(wti_wide,year(date)%%1==0,year!=2020))+
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
  guides(col = guide_legend(override.aes = list(linetype = c(0,1,1,1)),keywidth = unit(2.1,"cm"),nrow = 4,reverse = T),
         fill = guide_legend(keywidth = unit(2.1,"cm"),nrow = 4,reverse = T),
         linetype = guide_legend(keywidth = unit(2.1,"cm"),nrow = 2)
         
         #shape = guide_legend(keywidth = unit(2.4,"cm"),nrow = 2)
  )+
  labs(y="WTI Price ($/bbl)",x="",
       title=paste("EIA and CER WTI Outlook"),
       subtitle=paste("Historic and forward market settlement prices, CER Energy Futures 2020 and EIA Annual Energy Outlook (AEO) forecasts"),
       caption="Data via CME Group, Canadian Energy Regulator (CER), and the US Energy Information Administration (EIA). Graph by Andrew Leach.")
ggsave("images/wti_ribbon_nymex.png",dpi=res,width = 16)

