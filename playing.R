library(zoo)
library(lubridate)
library(readxl)
library(scales)
library(grid)
library(gridExtra)

library(lemon)
library(ggpubr)
library(labeling)
library(timeDate)
library(cowplot)
library(patchwork)
library(timeDate)
library(broom)
library(ggthemes)
library(directlabels)
library(pdfetch)

library(gghighlight)
library(viridis)

library(ggrepel)
library(xml2)
library(rvest)

library(tidyverse)
library(janitor)
library(readxl)

#devtools::install_github("leachandrew/pdfetch")

res<-150


download.file("https://www.aeso.ca/assets/Uploads/project-reporting/May-2023-Project-List.xlsx",destfile = "aeso_proj_list.xlsx",mode="wb")
project_list<-read_excel("aeso_proj_list.xlsx",skip = 1) %>%clean_names()%>%slice(-1)%>%filter(mw_type %in% c("Solar","	
Solar + Storage"))