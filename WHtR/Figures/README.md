# Figures
## 1.허리둘레 결측 여부에 따른 분포
This folder is for saving figures of how age, gender, and BMI are distributed differently between those who measured their waist circumferences(WC) and those who didn't. This process is necessary for determining which model to use for filling in missing values of WC:
* **MCAR(Missing Completely At Random)** - the fact that *a certain variable is missing values* has ***nothing** to do with its hypothetical value and the values of other variables*.
* **MAR(Missing At Random)** - *the propensity for a data point to be missing* is ***not** related to the missing data*, but it is *related to **some** of the observed data*.
* **MNAR(Missing Not At Random)** - *a certain variable is missing values* because
  * the fact that it is missing values *has to do with its hypothetical values* or
  * the propensity for a data point to be missing is *related to **all** of the observed data*.
  
In case of **MCAR**, it is the best option to *drop observations with missing values*. In case of **MAR** and **MNAR**, it is a good idea to use **multiple imputation by chained equations(MICE)**, which repeatedly fills in missing values by running multiple Bayesian regressions on and on, taking turns emptying imputed values of each variable with missing values. Yet, *when there is only a single variable that is missing values, there is no meaning in using MICE.* In R, you can install package *mice* and fill in missing values with MICE method.

## 2.대체값과_관찰값의_분포_비교
This folder is for saving figures of how similar the distribution of the imputed values of waist circumference(WC) is to that of the observed values of WC. This process is necessary for making sure that propensity for a data point to be missing is not related to the missing data (**Missing At Random(MAR)** Assumption).  

## 3.변수간상관관계분석
This folder is for saving
* The correlation matrix with scatterplots, showing Pearson correlation coefficients between variables of interest (age, BMI, waist-to-height ratio(WHtR), and body fat percentage(BFP))
* Distribution of variables of interest (age, gender, BMI, WHtR, and BFP) month by month - it is necessary to visually check how the time(month) influences major variables before applying the pooled cross-section/least-squares dummy variable(LSDV: here, the dummy variable is time dummy variable) model.  

## 4.모델사후검증
This folder is for saving plots necessary for checking **if the LSDV(least-squares dummy variable) model regressing body fat percentage(BFP) on waist-to-height ratio(WHtR), gender, and month dummy variables satisfies major OLS(ordinary least squares) assumptions and has as little outliers as possible.**  

* **The major OLS assumptions** are as follows:
  * The residuals and the fitted values should not have a relationship with each other (in other words, they must be independent (at least linearly) from each other. This is because the fitted values themselves are linear transformations of independent variables, and independent variables are uncorrelated with residuals by the construction of the OLS).
  * The residuals, when standardized, must follow the standard normal distribution (mean = 0, sd = 1).
  * The standard deviation of residuals must be the same regardless of the values of independent variables (econometricians refer to this property as *homoskedasticity*).
  
* **The numbers to consider when determining outliers** are as follows:
  * **Leverage**: the measure of how the extreme value of the independent variable distorts the regression outcome
  * **Cook's Distance**: the measure of how much influence an observation would have on the regression outcome when eliminated from the sample
  
* **The plots** in this folder are:
  * **Residuals vs. Fitted** - A scatterplot showing if there is a relationship or pattern between residuals and fitted values of the OLS regression
  * **Normal Q-Q Plot** - A scatterplot showing how closely the residuals, when standardized, follow the standard normal distribution. If they show perfect standard normal distribution, all the points on the plot should be on a 45-degree line.
  * **Scale-location** - A scatterplot showing how the spread of residuals change as the fitted values change. If there is a noticeable pattern in the plot, the homoskedasticity assumption of the OLS is broken.
  * **Residuals vs. Leverage** - A scatterplot showing the leverage of each residual. Red lines indicating the borderline Cook's Distances (.5 and 1) are also drawn on the plot.  

## 5.모델수정후검증
This file is for saving plots necessary for checking **if the revised LSDV(least-sqaures dummy variable) model *regressing body fat percentage(BFP) on waist-height ratio(WHtR), gender, WHtR\*gender, and month dummy variables* satisfies the OLS(ordinary least squares) assumptions better than the previous LSDV model *regressing BFP on WHtR, gender, and month dummy variables*.** In particular, these plots are for checking **if there is less non-linear pattern between the residuals and the fitted values on the scatterplot.**
