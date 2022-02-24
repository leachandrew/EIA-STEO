
paper_theme<-function(caption_align=1){
  theme_minimal()+theme(
    axis.title.x = element_text(size = 12,margin = margin(t = 2, b =2)),
    axis.text.x = element_text(size = 12,margin = margin(t = 2, b = 2)),
    axis.title.y = element_text(size = 12,margin = margin(r = 2, l = 2)),
    axis.text.y = element_text(size = 12,margin = margin(r = 2, l = 2)),
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




blake_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.7)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5)),
          legend.title = element_text(color="grey10",size=rel(.5)),
          legend.text = element_text(color="grey10",size=rel(.5)),
          axis.title = element_text(size=rel(.8)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "none",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}

insertLayer <- function(P, after=0, ...) {
  #  P     : Plot object
  # after  : Position where to insert new layers, relative to existing layers
  #  ...   : additional layers, separated by commas (,) instead of plus sign (+)
  
  if (after < 0)
    after <- after + length(P$layers)
  
  if (!length(P$layers))
    P$layers <- list(...)
  else 
    P$layers <- append(P$layers, list(...), after)
  
  return(P)
}

`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
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

weekly_graphs<-function(caption_align=1){
  theme_minimal()+theme(
    plot.margin = margin(.25, .75, .25, .75, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=caption_align),
    plot.title = element_text(size = 16,face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title.x = element_text(margin = margin(t = 15, b = 0)),
    axis.text = element_text(margin = margin(t = 10, b = 10)),
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
    #stat = select(all_of(holiday_list)) %>% rowSums()>0
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

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Get Tableau's 10-color palette
#' 
#' \code{colors_tableau10} returns a vector of RGB hex triplets representing
#' Tableau's 10-color color palette.
#' 
#' @export
#' @rdname tableau10
#' @note These values come from: \url{http://tableaufriction.blogspot.ro/2012/11/finally-you-can-use-tableau-data-colors.html}
colors_tableau10 <- function()
{
  return(c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B",
           "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"))
}

colors_tableau10_light <- function()
{
  return(c("#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94",
           "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5"))
}

colors_tableau10_medium <- function()
{
  return(c("#729ECE", "#FF9E4A", "#67BF5C", "#ED665D", "#AD8BC9", "#A8786E",
           "#ED97CA", "#A2A2A2", "#CDCC5D", "#6DCCDA"))
}

