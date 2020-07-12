getwd()
.libPaths()
rm(list = ls())
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("readxl")
#install.packages("writexl")
#install.packages("ggpubr")
#install.packages("tidyverse")
#install.packages("ggthemes")
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

exercise <- read_xlsx("운동기록.xlsx")
str(exercise$DATETIME)

whr <- exercise %>%
  ggplot(aes(as.Date(DATETIME),WHR))+geom_line()+
  scale_x_date("DATE",date_breaks = "1 day", date_labels = "%y/%m/%d")+
  scale_y_continuous(limits=c(0.7,1),breaks=seq(0.7,1,0.1),expand=c(0,0))+
  geom_hline(yintercept = 0.80, color = "blue", linetype = 5)+
  annotate("text", x = ymd("2020/06/23"), y = 0.78, 
           label = "Target\nWHR",vjust = 1, size = 3, color = "grey40")+
  annotate("curve",x = ymd("2020/06/23"), y = 0.78, 
           xend = ymd("2020/06/23"), yend = 0.795,
           arrow = arrow(length=unit(0.15,"cm"), type = "closed"),
           color = "grey40")+
  theme(axis.text.x = element_text(angle = 45), 
        panel.grid.minor.x = element_blank())
whr

whtr <- exercise %>%
  ggplot(aes(as.Date(DATETIME),WHTR))+geom_line()+
  scale_x_date("DATE",date_breaks = "1 day", date_labels = "%y/%m/%d")+
  scale_y_continuous(limits=c(0.4,0.7),breaks=seq(0.4,0.7,0.1),
                     expand=c(0,0))+
  geom_hline(yintercept = 0.50, color = "blue", linetype = 5)+
  annotate("text", x = ymd("2020/06/23"), y = 0.48, 
           label = "Target\nWHtR",vjust = 1, size = 3, color = "grey40")+
  annotate("curve",x = ymd("2020/06/23"), y = 0.48, 
           xend = ymd("2020/06/23"), yend = 0.495,
           arrow = arrow(length=unit(0.15,"cm"), type = "closed"),
           color = "grey40")+
  theme(axis.text.x = element_text(angle = 45),
        panel.grid.minor.x = element_blank())
whtr

whr_whtr <- ggarrange(whr, whtr, ncol = 1, nrow = 2, heights = c(2,2), align = "v")
whr_whtr

운동기록 <- whr_whtr %>%
  annotate_figure(top = text_grob("운동기록",face = "bold", size = 14))
운동기록
