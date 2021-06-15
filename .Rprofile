# A fun welcome message
message("Hi Andrew, welcome to R")
# Customise the R prompt that prefixes every command
# (use " " for a blank prompt)
options(prompt = "R> ",digits = 3)

#palettes

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

blakes_blue<-"#4477AA"

#u of a palette

colors_ua10 <- function()
{
  #return(c("#007C41", "#FFDB05", "#7D9AAA", "#CA7700", "#165788", "#A8B400",
  #         "#E0D760", "#404545", "#8D3C1E", "#004250"))
  c("#007C41", "#FFDB05", "#7D9AAA","#165788","#404545","#8D3C1E","#3CB6CE")
}


#Set EIA API Key

KEY <- "91b4dca0b858df64a2279d82f71af240"


#graph formats


blake_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.7)),
          plot.title = element_text(face="bold",size=rel(.8)),
          plot.caption = element_text(color="grey50",size=rel(.5),hjust=0),
          legend.title = element_text(color="grey10",size=rel(.5)),
          legend.text = element_text(color="grey10",size=rel(.5)),
          axis.title = element_text(size=rel(.8)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          #legend.position = "none",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}


ajl_line<-function(caption_align=1){
  theme_linedraw()+theme(
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
    plot.caption = element_text(size = 10, face = "italic",hjust=caption_align),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
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
    plot.caption = element_text(size = 8, color = "gray40",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
}

