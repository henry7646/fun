# WHtR

* R codes for a self-motivated data analytics project using **KSPO(Korea Sports Promotion Foundation)'s Physical Examination Data (2019)**. The **variables of interest** are:

|**Variable**|**Unit**|
|:----------:|:------:|
키|*cm*
몸무게|*kg*
체지방율|*%*
BMI|*몸무게(kg)/키(m)<sup>2</sup>*
허리둘레|*cm*
WHtR|*허리둘레(cm)/키(cm)*

* Each Korean column name stands for:  
  * 키: *Height*
  * 몸무게: *Weight*
  * 체지방율: *Body fat percentage (measured at one of KSPO's centers via bioeletric impedance analysis(BIA))*
  * 허리둘레: *Waist circumference (WC)*  
  
* **WHtR(waist-to-height ratio)** is the *ratio of waist circumference to height*.

* The goal of this project is to verify **if WHtR is indeed a good estimator of the body fat percentage(BFP)**.  
There are numerous methods of calculating and estimating the BFP, but I'll limit this project's scope to  
the **BFP obtained via BIA**, the one actually included in KSPO's data. Meanwhile, **other minor goals** of this  
project are:  
  * Filling in the missing values for BFP and WC  
  * Conducting exploratory data analysis of various health-related metrics (BFP, BMI, WC, WHtR) with numerous data visualizations
  * Creating an R Shiny App Dashboard to enable people to know where they are in terms of various measures of obesity (BMI, WHtR) and predict their BFP without visiting KSPO's centers or relying on biased, inconsistent formulas online.
  
\*How to open the R Shiny App Dashboard in your computer:  
Download **exercise_measure_201901.csv~exercise_measure_201912.csv, exercise_measure.R, ui.R, server.R, and obesity_dashboard.R** into the same local directory -> Run **exercise_measure.R** with **R Studio** -> Run **obesity_dashboard.R** with **R Studio**
