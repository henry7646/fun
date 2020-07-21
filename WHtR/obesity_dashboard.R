# Create an R Shiny Dashboard displaying the distribution
# of the WHtR, BMI, and BFP. If you type in your gender, age cohort,
# waist circumference, weight, and height, you get your:
# 1.WHtR
# 2.WHtR percentile rank with respect to your gender and age cohort
# 3.WHtR percentile rank with respect to the whole population
# 4.BMI
# 5.BMI percentile rank with respect to your gender and age cohort
# 6.BMI percentile rank with respect to the whole population
# 7.BFP
# 8.BFP percentile rank with respect to your gender and age cohort
# 9.BFP percentile rank with respect to the whole population.

# In case of BFP, it is an estimation using the least-square
# dummy variable(LSDV) model (regress_on_WHtR_6) from
# exercise_measure.RData

getwd()
setwd("C:/R/WHtR")
#rm(list=ls())
load("exercise_measure.RData")

library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)

source("ui.R")
source("server.R")
shinyApp(ui = ui,server = server)