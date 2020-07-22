getwd()
#setwd("C:/R/WHtR")
#rm(list=ls())
#load("exercise_measure.RData")

library(readr)
library(readxl)
library(writexl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(GGally)
library(lattice)
library(mice)
library(shiny)
library(shinythemes)
library(broom)
library(stargazer)
library(arsenal)

#Import Korea Sports Promotion Foundation's Physical Examination Results Data (Januaray 2019 ~ December 2019)
#Eliminate observations who received the examination more than once, since I am going to use the pooled cross
#section model, and clearly the individuals appearing more than once across multiple cross-sections
#will make samples from each cross-section dependent to each other 
#Eliminate outliers (an observation shorter than 100cm and taller than 300cm): The 2019 world's tallest person is 251cm
for(i in c(201901:201912)){
  assign(paste0("exercise_measure_",i),
         read_delim(paste0("C:/R/WHtR/exercise_measure_",i,".csv"), delim = "") %>%
           filter(`측정 회차` == 1 & `측정항목값 : 신장 : cm` >= 100 & `측정항목값 : 신장 : cm` <= 300) %>%
           mutate(ID = row_number()))
}

#For some unknown reason, exercise_measure_201908$`측정항목값 : 체지방율 : %`
#has been converted to characters instead of numbers. It is necessary to fix this
exercise_measure_201908$`측정항목값 : 체지방율 : %` <- as.numeric(exercise_measure_201908$`측정항목값 : 체지방율 : %`)

#For each month's exercise measure,
#1.Select only necessary columns of interest (측정나이(age),
#나이구분(age cohort),측정회원성별(gender),측정항목값 : 신장 : cm(height),
#측정항목값 : 체중 : kg(weight),측정항목값 : 체지방율 : %(body fat percentage),
#측정항목값 : 허리둘레 : cm(waist circumference(WC)),측정항목값 : BMI : kg/㎡(BMI))
#2.Change column names into readable and workable ones (측정나이->나이,
#나이구분->연령대,측정회원성별->성별,측정항목값 : 신장 : cm->키,
#측정항목값 : 체중 : kg->몸무게,측정항목값 : 체지방율 : %->체지방율,
#측정항목값 : 허리둘레 : cm->허리둘레,측정항목값 : BMI : kg/㎡->BMI)
#3.Eliminate observations missing values for body fat percentage(BFP):
#For each month, the maximum number of observations missing the BFP is 30,
#and the mode is 0
#4.Eliminate outliers((WC-outside 30~200,BFP-outside 3~100,BMI-outside 10~80):
#(1)걸스데이 민아(Korean female idol singer) has a waist circumference of 35.5cm
#(2)At least 3~5% of body fat is necessary for a male to survive (8~10% for females)
#(3)According to the KCDC, the 13-year-old female's threshold for being underweight is BMI = 15.2.
#(4)gongik.info: maximum BMI among those who received physical examinations for military conscription in 2019 = 77.4
#5.Generate a binary variable indicating whether the value is missing for WC: this is a must for the exploratory data analysis(EDA) of missing values later  
for(i in c(201901:201912)){
  assign(paste0("obesity_measure_",i), get(paste0("exercise_measure_",i)) %>%
    select(측정나이, 나이구분, 측정회원성별, `측정항목값 : 신장 : cm`, `측정항목값 : 체중 : kg`, `측정항목값 : 체지방율 : %`, `측정항목값 : BMI : kg/㎡`, `측정항목값 : 허리둘레 : cm`) %>%
    rename(나이 = 측정나이, 연령대 = 나이구분, 성별 = 측정회원성별, 키 = `측정항목값 : 신장 : cm`, 몸무게 = `측정항목값 : 체중 : kg`, 체지방율 = `측정항목값 : 체지방율 : %`, 허리둘레 = `측정항목값 : 허리둘레 : cm`, BMI = `측정항목값 : BMI : kg/㎡`) %>%
    filter((체지방율 >= 3 & 체지방율 <= 100) & (is.na(허리둘레) == 1 |(허리둘레 >= 30 & 허리둘레 <= 200))) %>%
    filter(BMI >= 10 & BMI <= 80) %>%
    mutate(허리둘레측정 = as.factor(ifelse(is.na(허리둘레) == 0, 1, 0)),월 = i)
  )
}

#Below is the for loop for checking outliers(in the sense of extreme values):
#run this loop, check the outliers, and eliminate them by adding additional
#conditions to the for loop above and running it again
for(i in c(201901:201912)){
  print(paste(i,min(select(get(paste0("obesity_measure_",i)),허리둘레), na.rm = T),max(select(get(paste0("obesity_measure_",i)),허리둘레), na.rm = T)))
}
# ---------------------------------------------------------
#Explore the pattern of the missing data for each month:
#md.pattern(obesity_measure_2019**)
md.pattern(obesity_measure_201912)

#See if the distribution of each variable differ depending on whether the variable with missing values is missing a value or not:
#Such is a sign of the data being MAR(Missing At Random) or MNAR(Missing Not At Random)

#MCAR(Missing Completely At Random): the fact that a certain variable is missing values has nothing to do with its hypothetical value and with the values of other variables
#MAR(Missing At Random): the propensity for a data point to be missing is not related to the missing data, but it is related to some of the observed data
for(i in c(201901:201912)){
assign(paste0("fat_distribution_",i),ggplot(get(paste0("obesity_measure_",i)),aes(체지방율,허리둘레측정,fill = 허리둘레측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("체지방율(%)") + ggtitle("체지방율의 분포") + theme(plot.title = element_text(hjust = 0.5)))
assign(paste0("BMI_distribution_",i),ggplot(get(paste0("obesity_measure_",i)),aes(BMI,허리둘레측정,fill = 허리둘레측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("BMI(몸무게(kg)/키(m)^2)") + ggtitle("BMI의 분포") + theme(plot.title = element_text(hjust = 0.5)))
assign(paste0("age_distribution_",i),ggplot(get(paste0("obesity_measure_",i)),aes(나이,허리둘레측정,fill = 허리둘레측정)) + geom_boxplot(alpha = 0.5) + scale_x_continuous("나이") + ggtitle("나이의 분포") + theme(plot.title = element_text(hjust = 0.5)))
print(nrow(filter(get(paste0("obesity_measure_",i)),허리둘레측정 == 1 & 성별 == "M"))/nrow(filter(get(paste0("obesity_measure_",i)), 허리둘레측정 == 1)))
print(nrow(filter(get(paste0("obesity_measure_",i)),허리둘레측정 == 0 & 성별 == "M"))/nrow(filter(get(paste0("obesity_measure_",i)), 허리둘레측정 == 0)))

assign(paste0("missing_values_distribution_",i),ggarrange(get(paste0("fat_distribution_",i)),get(paste0("BMI_distribution_",i)),get(paste0("age_distribution_",i)), ncol = 1, nrow = 3))
assign(paste0("missing_values_exploration_",i),get(paste0("missing_values_distribution_", i))%>%
  annotate_figure(top = text_grob("허리둘레 결측 여부에 따른 분포", face = "bold", size = 14), bottom = text_grob(paste0("Data Source: \n KSPO Physical Examination Results (",i,")"), color = "blue", hjust = 1, x = 1, face = "italic", size = 10)))
}

missing_values_exploration_201901
missing_values_exploration_201902
missing_values_exploration_201903
missing_values_exploration_201904
missing_values_exploration_201905
missing_values_exploration_201906
missing_values_exploration_201907
missing_values_exploration_201908
missing_values_exploration_201909
missing_values_exploration_201910
missing_values_exploration_201911
missing_values_exploration_201912

#for(i in c(201901:201912)){
#  ggsave(paste0("missing_values_exploration_",i,".png"),get(paste0("missing_values_exploration_",i)),units = "in",width = 12,height = 8,dpi = 300,limitsize = FALSE)
#}
# ---------------------------------------------------------
#Once checking that the missing data are MAR (the age distribution
#differs largely between those who have their WCs measured and those who do not),
#apply linear regression model to fill in the missing values:
#since only the WC is missing values, there is not much
#point in using MICE (Multivariate Imputation by Chained Equations)

#Why are we even filling in missing values for WC?
#Because for each month, a huge percentage of it is missing. Simply
#eliminating observations missing WCs will lead to
#a large variance in the BFP prediction model's estimates due
#to small degrees of freedom.

#Backward Stepwise Regression: Start by regressing WC on
#age, gender, BMI, and body fat percentage(BFP) for each month.
#If one of the coefficient has a p-value above 0.05, drop it.
#Repeat the process until you reach the point
#where the R^2 is maximum.
for (i in c(201901:201912)){
assign(paste0("fill_in_missing_values_",i,"_1"),lm(허리둘레 ~ 나이+factor(성별)+BMI+체지방율,get(paste0("obesity_measure_",i))))
print(summary(get(paste0("fill_in_missing_values_",i,"_1"))))
assign(paste0("fill_in_missing_values_",i,"_2"),lm(허리둘레 ~ factor(성별)+BMI+체지방율,get(paste0("obesity_measure_",i))))
print(summary(get(paste0("fill_in_missing_values_",i,"_2"))))
assign(paste0("fill_in_missing_values_",i,"_3"),lm(허리둘레 ~ BMI+체지방율,get(paste0("obesity_measure_",i))))
print(summary(get(paste0("fill_in_missing_values_",i,"_3"))))
}

#Fill in missing values with imputed values determined by the linear regressions above.
#Compare the distribution of missing values and fully observed values for each variable of interest
obesity_measure_201901[is.na(obesity_measure_201901$허리둘레),8] <- predict(fill_in_missing_values_201901_2,obesity_measure_201901[is.na(obesity_measure_201901$허리둘레),])
obesity_measure_201902[is.na(obesity_measure_201902$허리둘레),8] <- predict(fill_in_missing_values_201902_1,obesity_measure_201902[is.na(obesity_measure_201902$허리둘레),])
obesity_measure_201903[is.na(obesity_measure_201903$허리둘레),8] <- predict(fill_in_missing_values_201903_1,obesity_measure_201903[is.na(obesity_measure_201903$허리둘레),])
obesity_measure_201904[is.na(obesity_measure_201904$허리둘레),8] <- predict(fill_in_missing_values_201904_1,obesity_measure_201904[is.na(obesity_measure_201904$허리둘레),])
obesity_measure_201905[is.na(obesity_measure_201905$허리둘레),8] <- predict(fill_in_missing_values_201905_1,obesity_measure_201905[is.na(obesity_measure_201905$허리둘레),])
obesity_measure_201906[is.na(obesity_measure_201906$허리둘레),8] <- predict(fill_in_missing_values_201906_1,obesity_measure_201906[is.na(obesity_measure_201906$허리둘레),])
obesity_measure_201907[is.na(obesity_measure_201907$허리둘레),8] <- predict(fill_in_missing_values_201907_1,obesity_measure_201907[is.na(obesity_measure_201907$허리둘레),])
obesity_measure_201908[is.na(obesity_measure_201908$허리둘레),8] <- predict(fill_in_missing_values_201908_1,obesity_measure_201908[is.na(obesity_measure_201908$허리둘레),])
obesity_measure_201909[is.na(obesity_measure_201909$허리둘레),8] <- predict(fill_in_missing_values_201909_1,obesity_measure_201909[is.na(obesity_measure_201909$허리둘레),])
obesity_measure_201910[is.na(obesity_measure_201910$허리둘레),8] <- predict(fill_in_missing_values_201910_1,obesity_measure_201910[is.na(obesity_measure_201910$허리둘레),])
obesity_measure_201911[is.na(obesity_measure_201911$허리둘레),8] <- predict(fill_in_missing_values_201911_1,obesity_measure_201911[is.na(obesity_measure_201911$허리둘레),])
obesity_measure_201912[is.na(obesity_measure_201912$허리둘레),8] <- predict(fill_in_missing_values_201912_2,obesity_measure_201912[is.na(obesity_measure_201912$허리둘레),])

#Compare the distribution of the imputed values with that of the fully observed values: the two distributions must be similar,
#since we assumed MAR: the propensity for a data point to be missing is not related to the missing data.

#If they are not similar, it is necessary to figure out why. Clearly, since we imputed the missing values by using age,gender,BMI,and BFP,
#looking at previous plots comparing the distributions of complete variables conditional on whether the WC is missing or not
#(missing_values_exploration_2019[month]) would help. Underneath, I have also generated the plot for comparing the distribution of gender among
#non-WC missing individuals to that among WC-missing individuals in each month.
for (i in c(201901:201912)){
assign(paste0("aft_imp_comp_",i), ggplot(get(paste0("obesity_measure_",i)),aes(허리둘레,fill = 허리둘레측정)) + geom_density(alpha = 0.5) + scale_x_continuous("허리둘레(cm)") + ggtitle("허리둘레의 분포") + theme(plot.title = element_text(hjust = 0.5)))
assign(paste0("aft_imp_ins_",i), get(paste0("aft_imp_comp_",i)) %>%
  annotate_figure(top = text_grob(paste0("분포: 대체값 vs. 관찰값 ","(",i,")"), face = "bold", size = 14), bottom = text_grob("Caveat: Under MAR (Missing at Random) model, the density should be the same between the observed values and the imputed values", color = "blue", hjust = 1, x = 1, face = "italic", size = 10)))
assign(paste0("gender_distribution_",i), ggplot(get(paste0("obesity_measure_",i)),aes(허리둘레측정,fill = 성별)) + geom_bar(position = "fill") + ggtitle("성별의 분포") + theme(plot.title = element_text(hjust = 0.5)))
}

aft_imp_ins_201901
aft_imp_ins_201902
aft_imp_ins_201903
aft_imp_ins_201904
aft_imp_ins_201905
aft_imp_ins_201906
aft_imp_ins_201907
aft_imp_ins_201908
aft_imp_ins_201909
aft_imp_ins_201910
aft_imp_ins_201911
aft_imp_ins_201912

gender_distribution_201901
gender_distribution_201902
gender_distribution_201903
gender_distribution_201904
gender_distribution_201905
gender_distribution_201906
gender_distribution_201907
gender_distribution_201908
gender_distribution_201909
gender_distribution_201910
gender_distribution_201911
gender_distribution_201912

#for(i in c(201901:201912)){
#  ggsave(paste0("aft_imp_ins_",i,".png"),get(paste0("aft_imp_ins_",i)),units = "in",width = 12,height = 8,dpi = 300,limitsize = FALSE)
#  ggsave(paste0("gender_distribution_",i,".png"),get(paste0("gender_distribution_",i)),units = "in",width = 12,height = 8,dpi = 300,limitsize = FALSE)
#}
# ---------------------------------------------------------
# After imputing values into WC, generate
# a new variable found out to better predict metabolic and
# cardiovascular diseases: WHtR
for(i in c(201901:201912)){
assign(paste0("obesity_measure_WHtR_",i), get(paste0("obesity_measure_",i)) %>%
  mutate(WHtR = 허리둘레/키))
write_xlsx(get(paste0("obesity_measure_WHtR_",i)), paste0("obesity_measure_WHtR_",i,".xlsx"))
}
#---------------------------------------------------------
#Combine monthly health statistics data into a single annual data
obesity_measure_WHtR_2019 <- rbind(obesity_measure_WHtR_201901,obesity_measure_WHtR_201902,obesity_measure_WHtR_201903,
                                   obesity_measure_WHtR_201904,obesity_measure_WHtR_201905,obesity_measure_WHtR_201906,
                                   obesity_measure_WHtR_201907,obesity_measure_WHtR_201908,obesity_measure_WHtR_201909,
                                   obesity_measure_WHtR_201910,obesity_measure_WHtR_201911,obesity_measure_WHtR_201912) %>%
  filter(체지방율 > 21 | WHtR < 1) %>%
  filter(체지방율 > 4.6 | WHtR < 0.657) %>%
  filter(체지방율 < 81.7 | WHtR > 0.41)
obesity_measure_WHtR_2019$월 <- factor(obesity_measure_WHtR_2019$월)
#write_xlsx(obesity_measure_WHtR_2019,"obesity_measure_WHtR_2019.xlsx")

#Preliminary statistics of the annual data before applying 
#the least-square dummy variable(LSDV) model:
#After getting summary statistics in the format of table with .txt
#extension, you can convert them into .md files and then into .html files via
#https://markdowntohtml.com/ .html files look much cleaner and can be
#screen-captured for later use in PPT slides or thesis.
summary_stat_by_age <- tableby(연령대~BMI+WHtR+허리둘레+체지방율,data = obesity_measure_WHtR_2019,
                                  test = T, numeric.test = "anova")
summary_stat_by_gender <- tableby(성별~BMI+WHtR+허리둘레+체지방율,data = obesity_measure_WHtR_2019,
                                  test = T, numeric.test = "anova")

#sink(file = "summary_stat_by_age.txt")
summary(summary_stat_by_age, title = "연령대별 비만지표 통계 (ANOVA 검정 결과 포함)", text = T)
#sink(file=NULL)

#sink(file = "summary_stat_by_gender.txt")
summary(summary_stat_by_gender, title = "성별 비만지표 통계 (t 검정 결과 포함)", text = T)
#sink(file=NULL)

#Before applying the LSDV (here, the dummy variable is for each month)
#model under the pooled cross-sectional data assumption, take a preliminary
#view on the distribution across months. See if there does exist differences
#in the distributions of various health and sensus stats across months.
fat_dist_across_month <- ggplot(obesity_measure_WHtR_2019,aes(체지방율,fill = 월))+geom_density(alpha = 0.3)+scale_x_continuous("체지방율(%)")+ggtitle("월별 체지방율의 분포")+theme(plot.title = element_text(hjust = 0.5))
age_dist_across_month <- ggplot(obesity_measure_WHtR_2019,aes(나이,fill = 월))+geom_density(alpha = 0.3)+scale_x_continuous("나이")+ggtitle("월별 나이의 분포")+theme(plot.title = element_text(hjust = 0.5))
gender_dist_across_month <- ggplot(obesity_measure_WHtR_2019,aes(월,fill = 성별))+geom_bar(stat = "count", alpha = 0.3)+scale_x_discrete("월")+ggtitle("월별 성별의 분포")+theme(plot.title = element_text(hjust = 0.5))
BMI_dist_across_month <- ggplot(obesity_measure_WHtR_2019,aes(BMI,fill = 월))+geom_density(alpha = 0.3)+scale_x_continuous("BMI(몸무게(kg)/키(m)^2)")+ggtitle("월별 BMI의 분포")+theme(plot.title = element_text(hjust = 0.5))
WHtR_dist_across_month <- ggplot(obesity_measure_WHtR_2019,aes(WHtR,fill = 월))+geom_density(alpha = 0.3)+scale_x_continuous("WHtR(허리둘레(cm)/키(cm))")+ggtitle("월별 WHtR의 분포")+theme(plot.title = element_text(hjust = 0.5))
waist_dist_across_month <- ggplot(obesity_measure_WHtR_2019,aes(허리둘레,fill = 월))+geom_density(alpha = 0.3)+scale_x_continuous("허리둘레(cm)")+ggtitle("월별 허리둘레의 분포")+theme(plot.title = element_text(hjust = 0.5))
dist_across_month <- ggarrange(fat_dist_across_month,age_dist_across_month,gender_dist_across_month,BMI_dist_across_month,WHtR_dist_across_month,waist_dist_across_month,nrow=2,ncol=3) %>%
  annotate_figure(top = text_grob("월별 주요 변수의 분포 (2019)", face = "bold", size = 14))

# Also, look at how all the variables in the annual data are correlated to each other in a single picture(except for gender, since it is a categorical variable):
# Notice that WHtR's correlation to BFP is the highest among all the variables
correlation_matrix_with_WHtR <- ggpairs(obesity_measure_WHtR_2019,columns = c("나이","BMI","허리둘레","WHtR","체지방율")) + ggtitle("주요 변수 간 상관관계")
correlation_matrix_with_WHtR

#Here comes the showtime. Apply the LSDV model to predict
#the BFP. The code below will carry out
#the backward stepwise regression while plugging in different
#obesity variables(BMI,WHtR,WC) to find out which model best predicts
#the BFP.

#regress_on_BMI_1 <- lm(체지방율 ~ BMI + 나이 + ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_BMI_1)

#regress_on_BMI_2 <- lm(체지방율 ~ BMI + 나이 + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_BMI_2)

regress_on_BMI_3 <- lm(체지방율 ~ BMI + ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
summary(regress_on_BMI_3)

#regress_on_BMI_4 <- lm(체지방율 ~ BMI + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_BMI_4)

#regress_on_WC_1 <- lm(체지방율 ~ 허리둘레 + 나이 + ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WC_1)

#regress_on_WC_2 <- lm(체지방율 ~ 허리둘레 + 나이 + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WC_2)

regress_on_WC_3 <- lm(체지방율 ~ 허리둘레 + ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
summary(regress_on_WC_3)

#regress_on_WC_4 <- lm(체지방율 ~ 허리둘레 + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WC_4)

#Eliminate outliers with high leverages and Cook's Distances under regress_on_WHtR_1
#(ex:one observation had BFP of 20.8% and WHtR of 1.18. How can someone who is almost
#underweight have a waist wider than one's own height?) and repeat the below regressions
#again. Combination of common sense, broom::augment() function on regress_on_WHtR_1
#(focus on .hat and .cooksd), and correlation_matrix_with_WHtR previously obtained
#would help decide which outliers to throw away.

#regress_on_WHtR_1 <- lm(체지방율 ~ WHtR + 나이 + ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WHtR_1)
#arrange(augment(regress_on_WHtR_1),desc(.hat))
#arrange(augment(regress_on_WHtR_1),desc(.cooksd))

#regress_on_WHtR_2 <- lm(체지방율 ~ WHtR + 나이 + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WHtR_2)

regress_on_WHtR_3 <- lm(체지방율 ~ WHtR + ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
summary(regress_on_WHtR_3)

#regress_on_WHtR_4 <- lm(체지방율 ~ WHtR + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WHtR_4)

#There is almost a minimal difference in the adjusted R^2
#between the regression on age,gender,health stat, and month dummy variables
#and the one missing age. Besides, the coefficient for age is significant
#but small in value. Also, the scatterplot shows no special pattern in the
#relationship between the age and the BFP. Hence, we select the regression on gender,
#health stat, and month dummy variables. Meanwhile, using WHtR as a health
#stat shows the highest adjusted R^2. Hence, for predicting the future BFP,
#I am going to use the regression on WHtR, gender, and month dummy variables
#(regress_on_WHtR_3).

#Check if the assumptions of the Classical Regression Model
#hold: (1)residuals must be independent from response
#(2)standardized residuals must follow the standard normal distribution.
#Also, check if there are any outliers distorting the slope coefficients
#based on the leverage(problem of the independent variable) and Cook's Distance.
plot(regress_on_WHtR_3)

#regress_on_WHtR_5 <- lm(체지방율 ~ WHtR + ifelse(성별 == "M",1,0) + 월 + WHtR*월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WHtR_5)
regress_on_WHtR_6 <- lm(체지방율 ~ WHtR + WHtR*ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
summary(regress_on_WHtR_6)
#regress_on_WHtR_7 <- lm(체지방율 ~ WHtR + WHtR*ifelse(성별 == "M",1,0) + 월 -1, obesity_measure_WHtR_2019)
#summary(regress_on_WHtR_7)

plot(regress_on_WHtR_6)

#The scatterplot shows that there is a slight non-linear relationship between
#residuals and fitted values of the regression of BFP on WHtR,gender,and month
#dummy variables. This means that there must be either
#(1)missing variables
#(2)missing interaction effects among variables already in the model or
#(3)missing polynomials of a single variable already in the model.
#Since
#(1)I am only interested in the effects of independent variables already in obesity_measure_WHtR_2019 on BFP, and
#(2)regressing BFP on WHtR's polynomials did not flatten out the non-linear relationship between residuals and fitted values,
#I added the interaction effect of WHtR and gender into the regression model. The residuals' non-
#linear relationship with the fitted values did flatten out on the scatterplot. Meanwhile, the
#interaction effects of each month and WHtR did not have significant effects on BFP.
#Also, the normal Q-Q plot became much closer to a 45 degree line after adding WHtR*gender
#as a variable. Hence, I go forward with the regression of BFP on WHtR, gender, WHtR*gender,
#and month dummy variables (regress_on_WHtR_6).

#stargazer(regress_on_BMI_3,regress_on_WC_3,regress_on_WHtR_3,regress_on_WHtR_6,
#          type = "html",
#          out = "LSDV_comparison.html",
#          title = "회귀분석 비교",
#          ci = F, digits= 3,
#          covariate.labels = c("BMI(kg/m<sup>2</sup>)","허리둘레(cm)","WHtR(cm/cm)",
#                               "성별","1월","2월","3월","4월","5월","6월",
#                               "7월","8월","9월","10월","11월","12월","WHtR*성별"),
#          notes = "BMI - 몸무게/키<sup>2</sup>, WHtR - 허리둘레/키",
#          model.names = T, single.row = T)
