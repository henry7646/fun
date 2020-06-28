getwd()
rm(list=ls())

library(readr)
library(readxl)
library(writexl)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(GGally)
library(lattice)
library(mice)

#Import Korea Sports Promotion Foundation's Physical Examination Results Data (December 2019)
#Generate ID for each obeservation
exercise_measure <- read_delim("/exercise_measure_201912.csv", delim = "") %>%
  mutate(ID = row_number())

#Select only necessary columns of interest (키(height),몸무게(weight),체지방율(body fat percentage),허리둘레(waist),BMI)
#Change column names into readable and workable ones
#Eliminate an outlier (an observation being approximately 1500cm tall)
#Generate binary variables indicating whether the value is missing or not for both body fat percentage and waist: this is a must for the EDA of missing values later  
obesity_measure <- exercise_measure %>%
  select(ID, `측정항목값 : 신장 : cm`, `측정항목값 : 체중 : kg`, `측정항목값 : 체지방율 : %`, `측정항목값 : BMI : kg/㎡`, `측정항목값 : 허리둘레 : cm`) %>%
  rename(키 = `측정항목값 : 신장 : cm`, 몸무게 = `측정항목값 : 체중 : kg`, 체지방율 = `측정항목값 : 체지방율 : %`, 허리둘레 = `측정항목값 : 허리둘레 : cm`, BMI = `측정항목값 : BMI : kg/㎡`) %>%
  filter(키 <= 300) %>%
  mutate(허리둘레측정 = as.factor(ifelse(is.na(허리둘레) == 0, 1, 0)), 체지방율측정 = as.factor(ifelse(is.na(체지방율) == 0, 1, 0)))
# ---------------------------------------------------------
#Explore the pattern of the missing data
md.pattern(obesity_measure[2:6])
md.pairs(obesity_measure[2:6])

#Look at how all the variables in the whole data are correlated to each other in a single picture
correlation_matrix <- ggpairs(obesity_measure[2:6]) + ggtitle("결측치 대체 전")
correlation_matrix

#See if the distribution of the variable with missing values differs depending on whether the other variable is missing a value or not:
#Such is a sign of the data being MAR(Missing At Random) or MNAR(Missing Not At Random).
#Also, see if the distribution of the fully observed variable differs depending on whether the other variable is missing a value or not:
#Such is a sign of the data being MAR(Missing At Random) or MNAR(Missing Not At Random).

#MCAR(Missing Completely At Random): the fact that a certain value is missing has nothing to do with its hypothetical value and with the values of other variables
#MAR(Missing At Random): the propensity for a data point to be missing is not related to the missing data, but it is related to some of the observed data

fat_distribution <- ggplot(obesity_measure,aes(체지방율,허리둘레측정,fill = 허리둘레측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("체지방율(%)") + ggtitle("체지방율의 분포") + theme(plot.title = element_text(hjust = 0.5))
waist_distribution <- ggplot(obesity_measure,aes(허리둘레,체지방율측정,fill = 체지방율측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("허리둘레(cm)") + ggtitle("허리둘레의 분포") + theme(plot.title = element_text(hjust = 0.5))
BMI_distribution <- ggplot(obesity_measure,aes(BMI,허리둘레측정,fill = 허리둘레측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("BMI(몸무게(kg)/키(m)^2)") + ggtitle("BMI의 분포") + theme(plot.title = element_text(hjust = 0.5))
height_distribution <- ggplot(obesity_measure,aes(키,허리둘레측정,fill = 허리둘레측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("키(cm)") + ggtitle("키의 분포") + theme(plot.title = element_text(hjust = 0.5))
weight_distribution <- ggplot(obesity_measure,aes(몸무게,허리둘레측정,fill = 허리둘레측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("몸무게(kg)") + ggtitle("몸무게의 분포") + theme(plot.title = element_text(hjust = 0.5))

missing_values_distribution <- ggarrange(height_distribution, weight_distribution, BMI_distribution, fat_distribution, waist_distribution, ncol = 4, nrow = 2, heights = c(2,2), align = "h")
missing_values_distribution

missing_values_exploration <- missing_values_distribution %>%
  annotate_figure(top = text_grob("결측치의 분포", face = "bold", size = 14), bottom = text_grob("Data Source: \n KSPO Physical Examination Results (December 2019)", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
missing_values_exploration
# ---------------------------------------------------------
#Once checking that the missing data are MAR (waist's distribution differs largely between those who have their body fat percentages and those who do not),
#apply MICE (Multivariate Imputation by Chained Equations) using predictive mean matching in order to fill in the missing data

#m: number of cycles to repeat pmm

fill_in_missing_values <- mice(obesity_measure[2:6], m = 5, method = "pmm", seed = 23456)
fill_in_missing_values$imp$체지방율
fill_in_missing_values$imp$허리둘레

#Fill in missing values with imputed values determined by the 5th cycle of MICE
#Compare the distribution of missing values and fully observed values for each variable of interest
imputed <- complete(fill_in_missing_values,5)
imputed_vs_observed <- stripplot(fill_in_missing_values)

#Replace the original dataframe's columns with missing values
#with new columns with imputed values

#Compare the distribution of the imputed values with that of the fully observed values
#for variables with missing values: the two distributions must be similar,
#since we assumed MAR: the propensity for a data point to be missing is not related to the missing data.

#If they are not similar, it is necessary to figure out why.
obesity_measure$체지방율 = imputed$체지방율 
obesity_measure$허리둘레 = imputed$허리둘레
aft_imp_waist_comp <- ggplot(obesity_measure,aes(허리둘레,fill = 허리둘레측정)) + geom_density(alpha = 0.5) + scale_x_continuous("허리둘레(cm)") + ggtitle("허리둘레의 분포") + theme(plot.title = element_text(hjust = 0.5)) 
aft_imp_fat_comp <- ggplot(obesity_measure,aes(체지방율,fill = 체지방율측정)) + geom_density(alpha = 0.5) + scale_x_continuous("체지방율(%)") + ggtitle("체지방율의 분포") + theme(plot.title = element_text(hjust = 0.5))
aft_imp_dist_comp <- ggarrange(aft_imp_fat_comp, aft_imp_waist_comp, nrow = 1, ncol = 2, align = "h")
aft_imp_ins <- ggarrange(imputed_vs_observed, aft_imp_dist_comp, nrow = 2, ncol = 1)
after_imputation_inspection <- aft_imp_ins %>%
  annotate_figure(top = text_grob("분포: 대체값 vs. 관찰값", face = "bold", size = 14), bottom = text_grob("Caveat: Under MAR (Missing at Random) model, the density should be the same between the observed values and the imputed values", color = "blue", hjust = 1, x = 1, face = "italic", size = 10))
after_imputation_inspection
# ---------------------------------------------------------
# After imputing values into body fat percentage and waist, generate
# a new variable found out to better predict metabolic and
# cardiovascular diseases: WHtR
obesity_measure_with_WHtR <- obesity_measure %>%
  mutate(WHtR = 허리둘레/키) %>%
  select(키,몸무게,체지방율,BMI,허리둘레,WHtR)
  
# Look at how all the variables in the whole data are correlated to each other in a single picture:
# Notice that WHtR's correlation to body fat percentage is the highest among all the variables
correlation_matrix_with_WHtR <- ggpairs(obesity_measure_with_WHtR) + ggtitle("결측치 대체 후")
correlation_matrix_with_WHtR

correlation_matrices <- ggarrange(ggmatrix_gtable(correlation_matrix), ggmatrix_gtable(correlation_matrix_with_WHtR), nrow = 1, ncol = 2)
correlation_analysis <- correlation_matrices %>%
  annotate_figure(top = text_grob("상관분석: 결측치 대체 전 vs. 결측치 대체 후", face = "bold", size = 14))
correlation_analysis
# --------------------------------------------------------
# Create an R Shiny Dashboard displaying the distribution
# of the WHtR. If you type in your waist circumference and
# height, you get your WHtR and your relative WHtR placement within
# the whole probability distribution of WHtR.

ui <- fluidPage(
  titlePanel("How Fat am I?: Comparing My Waist-Height Ratio(WHtR) with Others'"),
  sidebarLayout(
    sidebarPanel(
      numericInput("waist","Waist cirumference(cm):",20,145,.1),
      numericInput("height","Height(cm):",130,200,.1),
      ),
    mainPanel(
      textOutput("q"),
      plotOutput("r")
      )
  )
)

server <- function(input,output){
  output$q <- renderText({
    paste("Your WHtR is",round(input$waist/input$height,2),".")
  })
  output$r <- renderPlot({
    ggplot(obesity_measure_with_WHtR,aes(WHtR))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = input$waist/input$height, color = "red") + scale_x_continuous("WHtR") + ggtitle("Distribution of WHtR")
  })
}

shinyApp(ui = ui,server = server)
