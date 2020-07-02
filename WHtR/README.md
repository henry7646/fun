# WHtR

* R codes for a self-motivated data analytics project using **KSPO(Korea Sports Promotion Foundation)'s Physical Examination Data (2019-12)**. The **variables of interest** are:

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
  * 허리둘레: *Waist circumference*  
  
* **WHtR(waist-to-height ratio)** is the *ratio of waist circumference to height*.

* The goal of this project is to verify **if WHtR is indeed a good estimator of the body fat percentage(BFP)**.  
There are numerous methods of calculating and estimating the BFP, but I'll limit this project's scope to  
the **BFP obtained via BIA**, the one actually included in KSPO's data. Meanwhile, **other minor goals** of this  
project are:  
  * Filling in the missing values for 체지방율 and 허리둘레 using MICE(multivariate imputation with chained equations)  
  * Conducting exploratory data analysis of various health-related metrics (키, 몸무게, 체지방율, BMI, 허리둘레, WHtR) with numerous data visualizations
  * Creating an R Shiny dashboard to enable people to know where they are in terms of various measures of obesity (BMI, 허리둘레, WHtR) and predict their BFP (체지방율) without visiting KSPO's centers or relying on biased, inconsistent formulas online.
  
\* I might update this file in the future with KSPO's Physial Examination Data from previous months.