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
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

exercise <- read_xlsx("C:/Users/Seung Jae Han/Dropbox/Trivia/운동기록.xlsx")
str(exercise$DATETIME)

whr <- exercise %>%
  ggplot(aes(as.Date(DATETIME),WHR))+geom_line()+scale_x_date("DATE",date_breaks = "1 day", date_labels = "%y/%m/%d")+scale_y_continuous(limits=c(0.7,1),breaks=seq(0.7,1,0.1),expand=c(0,0))+theme(axis.text.x = element_text(angle = 45))
whr

bmi <- exercise %>%
  ggplot(aes(as.Date(DATETIME),BMI))+geom_line()+scale_x_date("DATE",date_breaks = "1 day", date_labels = "%y/%m/%d")+scale_y_continuous(limits=c(25,30),breaks=seq(25,30,1))+theme(axis.text.x = element_text(angle = 45))
bmi

whr_bmi <- ggarrange(whr, bmi, ncol = 1, nrow = 2, heights = c(2,2), align = "v")
whr_bmi

운동기록 <- whr_bmi %>%
  annotate_figure(top = text_grob("운동기록",face = "bold", size = 14))
운동기록
